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
    , adjustForFee
    , cardanoPolicy
    , estimateFee
    ) where

import Prelude

import Cardano.Environment
    ( Network (..), network )
import Cardano.Wallet.CoinSelection
    ( CoinSelection (..) )
import Cardano.Wallet.Primitive.Types
    ( Address (..)
    , Coin (..)
    , TxIn
    , TxOut (..)
    , UTxO (..)
    , balance'
    , isValidCoin
    , pickRandom
    )
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

import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Write as CBOR
import qualified Data.ByteString as BS
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
adjustForFee
    :: MonadRandom m
    => FeeOptions
    -> UTxO
    -> CoinSelection
    -> ExceptT FeeError m CoinSelection
adjustForFee opt utxo coinSel = do
    CoinSelection inps' outs' chgs' <- senderPaysFee opt utxo coinSel
    let neInps = case inps' of
            []   -> error "adjustForFees: empty list of inputs"
            inps -> inps
    let neOuts = case outs' of
            []   -> error "adjustForFees: empty list of outputs"
            outs -> outs
    return $ CoinSelection neInps neOuts chgs'

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

-- | A linear equation on the transaction size. Represents the @\s -> a + b*s@
-- function where @s@ is the transaction size in bytes, @a@ and @b@ are
-- constant coefficients.
data TxSizeLinear =
    TxSizeLinear (Quantity "lovelace" Double) (Quantity "lovelace/byte" Double)
    deriving (Eq, Show)

cardanoPolicy :: TxSizeLinear
cardanoPolicy = TxSizeLinear (Quantity 155381) (Quantity 43.946)

-- | Estimate fee for a given 'CoinSelection'. Fee follows a simple linear
-- equation:
--
-- @
--     f = a + sizeOf(tx) * b
-- @
--
-- where @a@ & @b@ are values fixed by the protocol. This operation is therefore
-- heavily coupled with the binary representation of a 'Transaction'. This
-- estimation is only a best-effort here as many of the encoding values actually
-- depends on the value of parameters at runtime.
--
-- For instance, an amount of `50` lovelace would be encoded using 2 bytes,
-- whereas an amount of `1000000` would be encoded using 4 bytes. In Byron, we
-- have only one piece of unknown from the 'CoinSelection' and it's the value of
-- the 'crc32' computed on the address payload, which can be 1,2,3 or 5 bytes
-- and we therefore always consider the worst-case scenario of a 5-byte crc.
--
-- As a consequence, our estimate may be slightly bigger than the actual
-- transaction fee (up-to 4 extra bytes per change output).
--
-- NOTE: We assume that all change outputs follow a sequential scheme and
-- therefore, have an empty address derivation payload.
estimateFee
    :: TxSizeLinear
    -> CoinSelection
    -> Fee
estimateFee policy (CoinSelection inps outs chngs) =
    Fee $ ceiling (a + b*fromIntegral totalPayload)
  where
    TxSizeLinear (Quantity a) (Quantity b) = policy

    -- With `n` the number of inputs
    -- signed tx ----------------------------------- 1 + (1|2) + n*139 + ~?
    --  | list len 2              -- 1
    --  | sizeOf(tx)              -- ~? (depends, cf 'sizeOfTx')
    --  | list len n              -- 1-2 (assuming n < 255)
    --  | n * sizeOf(witness)     -- n * 139
    totalPayload :: Int
    totalPayload =
        1
        + sizeOfTx
        + sizeOf (CBOR.encodeListLen $ fromIntegral n)
        + n*sizeOfTxWitness
      where n = length inps

    --input -------------------------------------- 41 + (1|2|3|5)
    --  | list len 2                  -- 1
    --  | word8                       -- 1
    --  | tag 24                      -- 2
    --  | bytes ------------------------ 2 + 35 + (1|2|3|5)
    --  |   | list len 2  -- 1
    --  |   | bytes       -- 2 + 32
    --  |   | word32      -- 1|2|3|5
    sizeOfTxIn :: Int -> Int
    sizeOfTxIn ix =
        41 + sizeOf (CBOR.encodeWord32 $ fromIntegral ix)

    -- SEQ + MAINNET
    -- output ------------------------------------- 41-53
    --  | list len 2                  -- 1
    --  | address ---------------------- 39|40|41|43
    --  |  | list len 2         -- 1
    --  |  | tag 24             -- 2
    --  |  | bytes --------------- 2 + 33
    --  |  |  | list len 3 -- 1
    --  |  |  | bytes      -- 2 + 28
    --  |  |  | attributes -- 1
    --  |  |  | word8      -- 1
    --  |  | word32             -- 1|2|3|5
    --  | word64                      -- 1|2|3|5|9
    --
    -- SEQ + TESTNET
    -- output ------------------------------------- 48-60
    --  | list len 2                  -- 1
    --  | address ---------------------- 46|47|48|50
    --  |  | list len 2         -- 1
    --  |  | tag 24             -- 2
    --  |  | bytes --------------- 2 + 40
    --  |  |  | list len 3 -- 1
    --  |  |  | bytes      -- 2 + 28
    --  |  |  | attributes -- 8
    --  |  |  | word8      -- 1
    --  |  | word32             -- 1|2|3|5
    --  | word64                      -- 1|2|3|5|9
    --
    -- RND + MAINNET
    -- output ------------------------------------- 74-86
    --  | list len 2                  -- 1
    --  | address ---------------------- 72|73|74|76
    --  |  | list len 2         -- 1
    --  |  | tag 24             -- 2
    --  |  | bytes --------------- 2 + 66
    --  |  |  | list len 3 -- 1
    --  |  |  | bytes      -- 2 + 28
    --  |  |  | attributes -- 34
    --  |  |  | word8      -- 1
    --  |  | word32             -- 1|2|3|5
    --  | word64                      -- 1|2|3|5|9
    --
    -- RND + MAINNET
    -- output ------------------------------------- 81-93
    --  | list len 2                  -- 1
    --  | address ---------------------- 79|80|81|83
    --  |  | list len 2         -- 1
    --  |  | tag 24             -- 2
    --  |  | bytes --------------- 2 + 73
    --  |  |  | list len 3 -- 1
    --  |  |  | bytes      -- 2 + 28
    --  |  |  | attributes -- 41
    --  |  |  | word8      -- 1
    --  |  | word32             -- 1|2|3|5
    --  | word64                      -- 1|2|3|5|9
    sizeOfTxOut :: TxOut -> Int
    sizeOfTxOut (TxOut (Address bytes) c) =
        1 + BS.length bytes + sizeOfCoin c

    -- Compute the size of a coin
    sizeOfCoin :: Coin -> Int
    sizeOfCoin = sizeOf . CBOR.encodeWord64 . getCoin

    -- Compute the size of the change, we assume that change is necessarily
    -- using a sequential scheme. For the rest, cf 'sizeOfTxOut'.
    -- Also, the size of the address depends on the size of the crc32, which can
    -- very between 1,2,3 and 5 bytes. We'll always consider the worst case for
    -- the change which makes an address payload of `43` bytes for mainnet,
    -- and `50` bytes on testnet.
    sizeOfChange :: Coin -> Int
    sizeOfChange c = case network of
        Mainnet -> 1 + 43 + sizeOfCoin c
        Staging -> 1 + 43 + sizeOfCoin c
        Testnet -> 1 + 50 + sizeOfCoin c
        Local -> 1 + 50 + sizeOfCoin c

    -- tx ------------------------------------- 6 + Σs(i) + ls(o) + Σs(c)
    --  | list len 3                  -- 1
    --  | begin                       -- 1
    --  | sizeOf(inps)                -- Σ sizeOf(inp)
    --  | break                       -- 1
    --  | begin                       -- 1
    --  | sizeOf(outs)                -- Σ sizeOf(out)
    --  | sizeOf(chngs)               -- Σ sizeOf(chng)
    --  | break                       -- 1
    --  | empty attributes            -- 1
    sizeOfTx :: Int
    sizeOfTx = 6
        + sum (map sizeOfTxIn [0..(length inps-1)])
        + sum (map sizeOfTxOut outs)
        + sum (map sizeOfChange chngs)

    -- witness ------------------------------------ 139
    --  | list len 2                  -- 1
    --  | word8                       -- 1
    --  | tag 24                      -- 2
    --  | bytes ------------------------ 2 + 133
    --  |   | list len 2 -- 1
    --  |   | bytes      -- 2+64
    --  |   | bytes      -- 2+64
    sizeOfTxWitness :: Int
    sizeOfTxWitness = 139

    -- Size of a particular CBOR encoding
    sizeOf :: CBOR.Encoding -> Int
    sizeOf = fromIntegral . BL.length . CBOR.toLazyByteString
