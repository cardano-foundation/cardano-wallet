{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: MIT
--
-- Provides the API of Coin Selection algorithm and Fee Calculation
-- This module contains the implementation of adjusting coin selection for a fee.
-- The sender pays for the fee and additional inputs are picked randomly.
-- For more information refer to:
-- https://iohk.io/blog/self-organisation-in-coin-selection/

module Cardano.Wallet.CoinSelection.Fee
    (
      -- * Fee Adjustment
      Fee(..)
    , FeeOptions (..)
    , FeeError(..)
    , TxSizeLinear (..)
    , adjustForFees
    , cardanoPolicy
    , estimateCardanoFee
    ) where

import Prelude

import Cardano.Wallet.CoinSelection
    ( CoinSelection (..) )
import Cardano.Wallet.Primitive.Types
    ( Coin (..)
    , TxIn
    , TxOut (..)
    , UTxO (..)
    , balance'
    , distance
    , invariant
    , isValidCoin
    , pickRandom
    )
import Codec.CBOR.Encoding
    ( encodeWord64 )
import Codec.CBOR.Write
    ( toLazyByteString )
import Control.Monad.Trans.Class
    ( lift )
import Control.Monad.Trans.Except
    ( ExceptT (..), throwE )
import Control.Monad.Trans.State
    ( StateT (..), evalStateT )
import Crypto.Random.Types
    ( MonadRandom )
import Data.Bifunctor
    ( bimap )
import Data.Quantity
    ( Quantity (..) )
import Data.Word
    ( Word64 )
import GHC.Generics
    ( Generic )

import qualified Data.ByteString.Lazy as BL
import qualified Data.List as L

{-------------------------------------------------------------------------------
                                Fee Adjustment
-------------------------------------------------------------------------------}

-- | A 'Fee', isomorph to 'Coin' but ease type-signatures and readability.
newtype Fee = Fee { getFee :: Word64 }

data FeeOptions = FeeOptions
    { estimate
      :: Int -> [Coin] -> Fee
      -- ^ Estimate fees based on number of inputs and values of the outputs
      -- Some pointers / order of magnitude from the current configuration:
      --     a: 155381 # absolute minimal fees per transaction
      --     b: 43.946 # additional minimal fees per byte of transaction size
    , dustThreshold
      :: Coin
      -- ^ Change addresses below the given threshold will be evicted
      -- from the created transaction. Setting 'dustThreshold' to 0
      -- removes output equal to 0
    } deriving (Generic)

newtype FeeError =
    CannotCoverFee Word64
    -- ^ UTxO exhausted during fee covering
    -- We record what amount missed to cover the fee
    deriving (Show, Eq)

-- | Given the coin selection result from a policy run, adjust the outputs
-- for fees, potentially returning additional inputs that we need to cover
-- all fees.
-- We lose the relationship between the transaction outputs and their
-- corresponding inputs/change outputs here. This is a decision we
-- may wish to revisit later. For now however note that since
--
--      (a) coin selection tries to establish a particular ratio
--          between payment outputs and change outputs (currently it
--          aims for an average of 1:1)
--
--      (b) coin selection currently only generates a single change
--          output per payment output, distributing the fee
--          proportionally across all change outputs is roughly
--          equivalent to distributing it proportionally over the
--          payment outputs (roughly, not exactly, because the 1:1
--          proportion is best effort only, and may in some cases be
--          wildly different).
--
-- Note that for (a) we don't need the ratio to be 1:1, the above
-- reasoning will remain true for any proportion 1:n. For (b) however,
-- if coin selection starts creating multiple outputs, and this number
-- may vary, then losing the connection between outputs and change
-- outputs will mean that that some outputs may pay a larger
-- percentage of the fee (depending on how many change outputs the
-- algorithm happened to choose).
adjustForFees
    :: MonadRandom m
    => FeeOptions
    -> UTxO
    -> CoinSelection
    -> ExceptT FeeError m CoinSelection
adjustForFees opt utxo coinSel = do
    coinSel'@(CoinSelection inps' outs' chgs') <- senderPaysFee opt utxo coinSel
    let estimatedFee = getFee $ feeUpperBound opt coinSel
    -- We enforce the following invariant:
    --
    --   estimatedFee < actualFee < 2 * estimatedFee
    --
    -- This coefficient (2*...) is mostly taken out of nowhere, but if anything
    -- go beyond that upper bound, we would know that our algorithm for fee
    -- reconciliation below is messed up.
    -- Similarly, the algorithm tries to take money from inputs until it reaches
    -- the goal fixed by 'estimatedFee'. So, the actualFee just can't be lower
    -- than the goal.
    let actualFee =
            invariant
            "estimatedFee =< actualFee =< 2 * estimatedFee is not satisfied"
            (computeFee coinSel')
            (\fee -> (estimatedFee <= fee && fee `div` 2 <= estimatedFee))
    let neInps = case inps' of
            []   -> error "adjustForFees: empty list of inputs"
            inps -> inps
    let neOuts = case outs' of
            []   -> error "adjustForFees: empty list of outputs"
            outs -> outs
    actualFee `seq` pure (CoinSelection neInps neOuts chgs')

-- | The sender pays fee in this scenario, so fees are removed from the change
-- outputs, and new inputs are selected if necessary.
senderPaysFee
    :: MonadRandom m
    => FeeOptions
    -> UTxO
    -> CoinSelection
    -> ExceptT FeeError m CoinSelection
senderPaysFee opt utxo sel = evalStateT (go sel) utxo where
    go
        :: MonadRandom m
        => CoinSelection
        -> StateT UTxO (ExceptT FeeError m) CoinSelection
    go coinSel@(CoinSelection inps outs chgs) = do
        -- 1/
        -- We compute fees using all inputs, outputs and changes since
        -- all of them have an influence on the fee calculation.
        let upperBound = feeUpperBound opt coinSel
        -- 2/
        -- Substract fee from change outputs, proportionally to their value.
        let (Fee remainingFee, chgs') = reduceChangeOutputs opt upperBound chgs
        -- 3.1/
        -- Should the change cover the fee, we're (almost) good. By removing
        -- change outputs, we make them smaller and may reduce the size of the
        -- transaction, and the fee. Thus, we end up paying slightly more than
        -- the upper bound. We could do some binary search and try to
        -- re-distribute excess across changes until fee becomes bigger.
        if remainingFee == 0
        then pure $ CoinSelection inps outs chgs'
        else do
            -- 3.2/
            -- Otherwise, we need an extra entries from the available utxo to
            -- cover what's left. Note that this entry may increase our change
            -- because we may not consume it entirely. So we will just split
            -- the extra change across all changes possibly increasing the
            -- number of change outputs (if there was none, or if increasing
            -- a change value causes an overflow).
            --
            -- Because selecting a new input increases the fee, we need to
            -- re-run the algorithm with this new elements and using the initial
            -- change plus the extra change brought up by this entry and see if
            -- we can now correctly cover fee.
            inps' <- coverRemainingFee (Fee remainingFee)
            let extraChange = splitChange (Coin $ balance' inps') chgs
            go $ CoinSelection (inps <> inps') outs extraChange

-- | A short / simple version of the 'random' fee policy to cover for fee in
-- case where existing change were not enough.
coverRemainingFee
    :: MonadRandom m
    => Fee
    -> StateT UTxO (ExceptT FeeError m) [(TxIn, TxOut)]
coverRemainingFee (Fee fee) = go [] where
    go acc
        | balance' acc >= fee =
            return acc
        | otherwise = do
            -- We ignore the size of the fee, and just pick randomly
            StateT (lift . pickRandom) >>= \case
                Just entry ->
                    go (entry : acc)
                Nothing -> do
                    lift $ throwE $ CannotCoverFee (fee - balance' acc)

-- | Reduce the given change outputs by the total fee, returning the remainig
-- change outputs if any are left, or the remaining fee otherwise
--
-- We divvy up the fee over all change outputs proportionally, to try and keep
-- any output:change ratio as unchanged as possible
reduceChangeOutputs :: FeeOptions -> Fee -> [Coin] -> (Fee, [Coin])
reduceChangeOutputs opt totalFee chgs =
    case removeDust (Coin 0) chgs of
        [] ->
            (totalFee, [])
        xs -> bimap (Fee . sum . map getFee) (removeDust $ dustThreshold opt)
            $ L.unzip
            $ map reduceSingleChange
            $ divvyFee totalFee xs

-- | Reduce single change output, returning remaining fee
reduceSingleChange :: (Fee, Coin) -> (Fee, Coin)
reduceSingleChange (Fee fee, Coin chng)
    | chng >= fee =
          (Fee 0, Coin $ chng - fee)
    | otherwise =
          (Fee $ fee - chng, Coin 0)

-- | Proportionally divide the fee over each output.
--
-- Pre-condition 1: The given outputs list shouldn't be empty
-- Pre-condition 2: None of the outputs should be null
--
-- It returns the a list of pairs (fee, output). Note that in case of small
-- input and because of rounding issues, outputs may end up with slight more
-- fee than expected.
divvyFee :: Fee -> [Coin] -> [(Fee, Coin)]
divvyFee _ [] =
    error "divvyFee: empty list"
divvyFee _ outs | (Coin 0) `elem` outs =
    error "divvyFee: some outputs are null"
divvyFee (Fee f) outs =
    map (\a -> (fee a, a)) outs
  where
    -- | The ratio will be between 0 and 1 so cannot overflow
    fee :: Coin -> Fee
    fee (Coin c) =
        let
            t = (sum . map getCoin) outs
            r = (fromIntegral c) / (fromIntegral t)
            c' = ceiling @Double (r * fromIntegral f)
        in
            Fee c'

-- | Compute a rough estimate of the fee bound
feeUpperBound
    :: FeeOptions
    -> CoinSelection
    -> Fee
feeUpperBound opt (CoinSelection inps outs chgs) =
    (estimate opt) (L.length inps) (map coin outs ++ chgs)

-- | Remove coins that are below a given threshold
removeDust :: Coin -> [Coin] -> [Coin]
removeDust threshold =
    L.filter (> threshold)

-- Equally split the extra change obtained when picking new inputs across all
-- other change. Note that, it may create an extra change output if:
--
--   (a) There's no change at all initially
--   (b) Adding change to an exiting one would cause an overflow
--
-- It makes no attempt to divvy the new output proportionally over the change
-- outputs. This means that if we happen to pick a very large UTxO entry, adding
-- this evenly rather than proportionally might skew the payment:change ratio a
-- lot. Could consider defining this in terms of divvy instead.
splitChange :: Coin -> [Coin] -> [Coin]
splitChange = go
  where
    go remaining as | remaining == Coin 0 = as
    go remaining [] = [remaining]
        -- we only create new change if for whatever reason there is none already
        -- or if is some overflow happens when we try to add.
    go remaining [a] =
        let
            newChange = Coin $ (getCoin remaining) + (getCoin a)
        in
            if isValidCoin newChange
            then [newChange]
            else [a, remaining]
    go rest@(Coin remaining) ls@(a : as) =
        let
            piece = remaining `div` fromIntegral (length ls)
            newRemaining = Coin (remaining - piece)
            newChange = Coin (piece + getCoin a)
        in
            if isValidCoin newChange
            then newChange : go newRemaining as
            else a : go rest as

-- Computing actual fee is a bit tricky in the generic realm because we don't
-- know what type representation is used by the underlying implementation. So,
-- we can't just sum up all the input and substract the sub of all outputs
-- (incl. change) because we'll risk an overflow with each sum. Instead, we
-- reduce the input value iteratively, coin by coin using a safe distance
-- between coins that are known to be within bounds.
-- The algorithm converge because we know that by construction, there are less
-- outputs than inputs. In essence, this computes:
--
--     fees = ∑ inputs - (∑ outputs + ∑ changes)
computeFee :: CoinSelection -> Word64
computeFee (CoinSelection inps outs chgs) =
    collapse
        (map (coin . snd) inps)
        (filter (> Coin 0) $ (map coin outs) <> chgs)
  where
    collapse :: [Coin] -> [Coin] -> Word64
    -- Some remaining inputs together. At this point, we've removed
    -- all outputs and changes, so what's left are simply the actual fees.
    collapse plus [] =
        sum $ map getCoin plus

    -- In order to safely compute fees at this level, we need make sure we don't
    -- overflow. Therefore, we remove outputs to inputs until there's no outputs
    -- left to remove.
    collapse (p:ps) (m:ms)
        | p > m =
            let p' = Coin $ distance (getCoin p) (getCoin m)
            in collapse (p':ps) ms
        | p < m =
            let m' = Coin $ distance (getCoin p) (getCoin m)
            in collapse ps (m':ms)
        | otherwise = collapse ps ms

    -- This branch can only happens if we've depleted all our inputs and there
    -- are still some outputs left to remove from them. If means the total value
    -- of outputs (incl. change) was bigger than the total input value which is
    -- by definition, impossible; unless we messed up real hard.
    collapse [] _ =
        invariant "outputs are bigger than inputs" (undefined) (const False)

-- | A linear equation on the transaction size. Represents the @\s -> a + b*s@
-- function where @s@ is the transaction size in bytes, @a@ and @b@ are
-- constant coefficients.
data TxSizeLinear =
    TxSizeLinear (Quantity "lovelace" Double) (Quantity "lovelace/byte" Double)
    deriving (Eq, Show)

cardanoPolicy :: TxSizeLinear
cardanoPolicy = TxSizeLinear (Quantity 155381) (Quantity 43.946)

-- | Estimate the fee for a transaction that has @inps@ inputs
-- and @outps@ outputs as coming from coin selection.
--
-- NOTE: The average size of @Attributes AddrAttributes@ and
--       the transaction attributes @Attributes ()@ are both hard-coded
estimateCardanoFee
    :: TxSizeLinear
    -> CoinSelection
    -> Fee
estimateCardanoFee (TxSizeLinear (Quantity a) (Quantity b)) (CoinSelection inps outs _) =
    Fee $ ceiling (a + b*totalPayload)
    where

        -- The size of TxIn is always the same as it contains the hash and index
        -- see Cardano.Wallet.Binary (encodeTxIn)
        sizeOfTxIn :: Int
        sizeOfTxIn = 42

        -- The size of TxOut depends only on the coin value
        -- see Cardano.Wallet.Binary (encodeTxOut)
        sizeOfTxOut :: Word64 -> Int
        sizeOfTxOut =
            (+77) . fromIntegral . BL.length . toLazyByteString . encodeWord64

        -- additional imprint that contains 6 one-byte elements:
        -- CBOR.encodeListLen 3
        -- CBOR.encodeListLenIndef
        -- CBOR.encodeBreak
        -- CBOR.encodeListLenIndef
        -- CBOR.encodeBreak
        -- encodeTxAttributes
        -- see Cardano.Wallet.Binary (encodeTx)
        sizeOfTxCbor :: Int
        sizeOfTxCbor = 6

        -- The size of [[TxIn], [TxWitness]]
        sizeOfTx :: Int
        sizeOfTx =
            let n = length inps
                coins = map (getCoin . coin) outs
            in sizeOfTxCbor + n*sizeOfTxIn + sum (map sizeOfTxOut coins)

        -- The size of TxWitness is always the same as it contains data constructor and hash
        -- see Cardano.Wallet.Binary (encodeTxWitness)
        sizeOfTxWitness :: Int
        sizeOfTxWitness = 139

        sizeOfListLen :: Int
        sizeOfListLen = 1

        -- The size of [TxWitness]
        sizeOfTxWitnesses :: Int
        sizeOfTxWitnesses =
            let n = length inps
            in sizeOfListLen + n*sizeOfTxWitness

        totalPayload :: Double
        totalPayload =
            fromIntegral $
               sizeOfListLen + sizeOfTx + sizeOfTxWitnesses
