{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Provides the API of Coin Selection algorithm and Fee Calculation
-- This module contains the implementation of adjusting coin selection for a fee.
-- The sender pays for the fee and additional inputs are picked randomly.
-- For more information refer to:
-- https://iohk.io/blog/self-organisation-in-coin-selection/

module Cardano.Wallet.Primitive.Fee
    (
      -- * Types
      Fee (..)
    , FeePolicy (..)

      -- * Fee Calculation
    , divvyFee

      -- * Fee Adjustment
    , FeeOptions (..)
    , ErrAdjustForFee(..)
    , OnDanglingChange(..)
    , adjustForFee
    , rebalanceSelection
    , coalesceDust
    ) where

import Prelude

import Cardano.Wallet.Primitive.CoinSelection
    ( CoinSelection (..)
    , changeBalance
    , feeBalance
    , inputBalance
    , outputBalance
    )
import Cardano.Wallet.Primitive.Types
    ( FeePolicy (..), invariant )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..), isValidCoin )
import Cardano.Wallet.Primitive.Types.Tx
    ( TxIn, TxOut (..) )
import Cardano.Wallet.Primitive.Types.UTxO
    ( UTxO (..), pickRandom )
import Control.Monad
    ( when )
import Control.Monad.Trans.Class
    ( lift )
import Control.Monad.Trans.Except
    ( ExceptT (..), throwE )
import Control.Monad.Trans.State
    ( StateT (..), evalStateT )
import Data.Word
    ( Word64, Word8 )
import Fmt
    ( Buildable (..), fixedF, nameF, pretty, unlinesF, (+|) )
import GHC.Generics
    ( Generic )
import GHC.Stack
    ( HasCallStack )

import qualified Data.List as L

{-------------------------------------------------------------------------------
                                    Types
-------------------------------------------------------------------------------}

-- | A 'Fee', isomorph to 'Coin' but ease type-signatures and readability.
newtype Fee = Fee { getFee :: Word64 }
    deriving (Eq, Ord, Show)

instance Buildable Fee where
    build (Fee fee)
        | fee > oneAda = fixedF 3 (double fee / double oneAda) +| " Ada"
        | otherwise    = build fee +| " Lovelace"
      where
        oneAda = 1_000_000

        double :: Integral a => a -> Double
        double = fromIntegral

{-------------------------------------------------------------------------------
                                Fee Adjustment
-------------------------------------------------------------------------------}

data FeeOptions = FeeOptions
    { estimateFee
      :: CoinSelection -> Fee
      -- ^ Estimate fees based on number of inputs and values of the outputs
      -- Some pointers / order of magnitude from the current configuration:
      --     a: 155381 # absolute minimal fees per transaction
      --     b: 43.946 # additional minimal fees per byte of transaction size

    , dustThreshold
      :: Coin
      -- ^ Change addresses below the given threshold will be evicted
      -- from the created transaction. Setting 'dustThreshold' to 0
      -- removes output equal to 0

    , onDanglingChange
      :: OnDanglingChange
      -- ^ What do to when we encouter a dangling change output.
      -- See 'OnDanglingChange'

    , feeUpperBound
      :: Fee
      -- ^ An extra upper-bound computed from the transaction max size. This is
      -- used to construct an invariant after balancing a transaction to
      -- make sure that the resultant fee is not unexpectedly high.

    , maximumNumberOfInputs
      :: Word8
      -- ^ Maximum number of inputs allowed to be selected. This number is
      -- estimated from the maximum transaction size and an approximation of the
      -- transaction size based on how many inputs it has.
    } deriving (Generic)

-- | We call 'dangling' a change output that would be too expensive to add. This
-- can happen when a change output is small, but adding it generate more fees
-- than not having it.
--
-- In case where nodes accept slightly unbalanced transactions, we may choose to
-- save money and keep the transaction slightly unbalanced. In case where node
-- demands exactly balanced transaction, we have no choice but to add the extra
-- dangling change output and pay for the extra cost induced.
data OnDanglingChange
    = PayAndBalance
    | SaveMoney
    deriving (Generic, Show, Eq)

newtype ErrAdjustForFee
    = ErrCannotCoverFee Word64
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
    :: HasCallStack
    => FeeOptions
    -> UTxO
    -> CoinSelection
    -> ExceptT ErrAdjustForFee IO CoinSelection
adjustForFee unsafeOpt utxo coinSel = do
    let opt = invariant "fee must be non-null" unsafeOpt (not . nullFee)
    cs <- senderPaysFee opt utxo coinSel
    let actualFee = Fee (feeBalance cs)
    let maxFee = feeUpperBound opt
    when (actualFee > maxFee) $
        error $ pretty $ unlinesF
            [ "generated a coin selection with an excessively large fee."
            , nameF "actual fee" (build actualFee)
            , nameF "maximum fee" (build maxFee)
            , nameF "coin selection" (build cs)
            ]
    pure cs
  where
    nullFee opt = estimateFee opt coinSel == Fee 0

-- | The sender pays fee in this scenario, so fees are removed from the change
-- outputs, and new inputs are selected if necessary.
senderPaysFee
    :: FeeOptions
    -> UTxO
    -> CoinSelection
    -> ExceptT ErrAdjustForFee IO CoinSelection
senderPaysFee opt utxo sel = evalStateT (go sel) utxo where
    go
        :: CoinSelection
        -> StateT UTxO (ExceptT ErrAdjustForFee IO) CoinSelection
    go coinSel@(CoinSelection inps _ _ outs chgs _) = do
        -- Substract fee from change outputs, proportionally to their value.
        let (coinSel', remFee) = rebalanceSelection opt coinSel

        -- Should the change cover the fee, we're (almost) good. By removing
        -- change outputs, we make them smaller and may reduce the size of the
        -- transaction, and the fee. Thus, we end up paying slightly more than
        -- the upper bound. We could do some binary search and try to
        -- re-distribute excess across changes until fee becomes bigger.
        if remFee == Fee 0
        then pure $ coinSel'
            { change = coalesceDust (dustThreshold opt) (change coinSel')
            }
        else do
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
            let nInps = fromIntegral $ length $ inputs coinSel'
            let maxN = if nInps >= maximumNumberOfInputs opt
                    then 0
                    else maximumNumberOfInputs opt - nInps
            (inps', surplus) <- coverRemainingFee maxN remFee
            let chgs' = splitChange surplus chgs
            go $ coinSel
                { inputs  = inps <> inps'
                , outputs = outs
                , change  = chgs'
                }

-- | A short / simple version of the 'random' fee policy to cover for fee in
-- case where existing change were not enough.
coverRemainingFee
    :: Word8
    -> Fee
    -> StateT UTxO (ExceptT ErrAdjustForFee IO) ([(TxIn, TxOut)], Coin)
coverRemainingFee maxN (Fee fee) = go [] 0 where
    go additionalInputs surplus
        | surplus >= fee =
            return (additionalInputs, Coin surplus)
        | length additionalInputs >= fromIntegral maxN =
            lift $ throwE $ ErrCannotCoverFee (fee - surplus)
        | otherwise = do
            -- We ignore the size of the fee, and just pick randomly
            StateT (lift . pickRandom) >>= \case
                Just input@(_, out) ->
                    go (input : additionalInputs) (getCoin (coin out) + surplus)
                Nothing -> do
                    lift $ throwE $ ErrCannotCoverFee (fee - surplus)

-- | Reduce the given change outputs by the total fee, returning the remainig
-- change outputs if any are left, or the remaining fee otherwise
--
-- We divvy up the fee over all change outputs proportionally, to try and keep
-- any output:change ratio as unchanged as possible.
--
-- This function either consumes an existing reserve on a selection, or turn it
-- into a change output. Therefore, the resulting coin selection _will_ not have
-- any reserve. Note that the reserve will be either 'Nothing', to indicate that
-- there was no reserve at all, or 'Just 0' to indicate that there was a
-- reserve, but it has been consumed entirely.
rebalanceSelection
    :: FeeOptions
    -> CoinSelection
    -> (CoinSelection, Fee)
rebalanceSelection opts s
    -- When there are no inputs, exit right away a pick a first input.
    --
    -- A case where this could occur is when selections are balanced in the
    -- context of delegation / de-registration.
    --
    -- A transaction would have initially no inputs.
    | null (inputs s) =
        (s, Fee φ_original)

    -- selection is now balanced, nothing to do.
    | φ_original == δ_original =
        (s, Fee 0)

    -- some fee left to pay, but we've depleted all change outputs
    | φ_original > δ_original && null (change s) =
        (s, Fee (φ_original - δ_original))

    -- some fee left to pay, and we've haven't depleted all change yet
    | φ_original > δ_original && not (null (change s)) = do
        let chgs' = coalesceDust (Coin 0)
                $ map reduceSingleChange
                $ divvyFee (Fee $ φ_original - δ_original) (change s)
        rebalanceSelection opts (s { change = chgs' })

    -- we've left too much, but adding a change output would be more
    -- expensive than not having it. Here we have two choices:
    --
    -- a) If the node allows unbalanced transaction, we can stop here and do
    -- nothing. We'll leave slightly more than what's needed for fees, but
    -- having an extra change output isn't worth it anyway.
    --
    -- b) If we __must__ balance the transaction, then we can choose to pay
    -- the extra cost by adding the change output and pick a new input to
    -- pay for the fee.
    | φ_dangling >= δ_original && φ_dangling > δ_dangling =
        case onDanglingChange opts of
            SaveMoney ->
                (s, Fee 0)
            PayAndBalance ->
                (sDangling, Fee (φ_dangling - δ_dangling))

    -- So, we can simply add the change output, and iterate.
    | otherwise =
        rebalanceSelection opts sDangling
  where
    -- The original requested fee amount
    Fee φ_original = estimateFee opts s
    -- The initial amount left for fee (i.e. inputs - outputs), with a minimum
    -- of 0 in case there are more output than inputs. This is possible when
    -- there are other elements apart from normal outputs like a deposit.
    δ_original
        | inputBalance s >= (outputBalance s + changeBalance s) =
            inputBalance s - (outputBalance s + changeBalance s)
        | otherwise =
            0

    -- The new amount left after balancing (i.e. φ_original)
    Fee φ_dangling = estimateFee opts sDangling
    -- The new requested fee after adding the output.
    δ_dangling = φ_original -- by construction of the change output

    extraChng = Coin (δ_original - φ_original)
    sDangling = s { change = splitChange extraChng (change s) }

-- | Reduce single change output by a given fee amount. If fees are too big for
-- a single coin, returns a `Coin 0`.
reduceSingleChange :: (Fee, Coin) -> Coin
reduceSingleChange (Fee fee, Coin chng)
    | chng >= fee =
          Coin (chng - fee)
    | otherwise =
          Coin 0

-- | Proportionally divide the fee over each output.
--
-- Pre-condition 1: The given outputs list shouldn't be empty
-- Pre-condition 2: None of the outputs should be null
--
-- It returns the a list of pairs (fee, output).
divvyFee :: Fee -> [Coin] -> [(Fee, Coin)]
divvyFee _ outs | (Coin 0) `elem` outs =
    error "divvyFee: some outputs are null"
divvyFee (Fee f0) outs = go f0 [] outs
  where
    total = (sum . map getCoin) outs
    go _ _ [] =
        error "divvyFee: empty list"
    go fOut xs [out] =
        -- The last one pays the rounding issues
        reverse ((Fee fOut, out):xs)
    go f xs ((Coin out):q) =
        let
            r = fromIntegral out / fromIntegral total
            fOut = ceiling @Double (r * fromIntegral f)
        in
            go (f - fOut) ((Fee fOut, Coin out):xs) q

-- | Remove coins that are below a given threshold. It'll try two strategies:
--
-- 1. Try to coalesce dust coins with other non-dust coins.
--
--     ∀δ≥0. sum coins == sum (removeDust δcoins)
--
-- 2. If the result is a single coin still smaller than the threshold, it'll
-- return an empty list.
coalesceDust :: Coin -> [Coin] -> [Coin]
coalesceDust threshold coins
    | balance coins < getCoin threshold =
        []
    | otherwise =
        let
            filtered = L.filter (> threshold) coins
            diff = balance coins - balance filtered
        in
            splitChange (Coin diff) filtered

balance :: [Coin] -> Word64
balance = L.foldl' (\total (Coin c) -> c + total) 0


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
