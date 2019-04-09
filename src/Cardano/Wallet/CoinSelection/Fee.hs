{-# LANGUAGE RankNTypes #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: MIT
--
-- This module contains the recipe for adjusting coin selection for a fee.
-- The sender pays for the fee and additional inputs are picked randomly.

module Cardano.Wallet.CoinSelection.Fee (
    adjustForFees
  ) where

import Prelude

import Cardano.Wallet.CoinSelection
    ( CoinSelection (..), FeeError (..), FeeOptions (..) )
import Cardano.Wallet.CoinSelection.Random
    ( distance, pickRandom )
import Cardano.Wallet.Primitive.Types
    ( Coin (..), TxIn, TxOut (..), UTxO (..), invariant, isValidCoin )
import Control.Monad.Trans.Class
    ( lift )
import Control.Monad.Trans.Except
    ( ExceptT (..), throwE )
import Control.Monad.Trans.Maybe
    ( MaybeT (..), runMaybeT )
import Crypto.Random.Types
    ( MonadRandom )
import Data.Bifunctor
    ( bimap )
import Data.Word
    ( Word64 )

import qualified Data.List as L

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
    :: forall m. MonadRandom m
    => FeeOptions
    -> UTxO
    -> CoinSelection
    -> ExceptT FeeError m CoinSelection
adjustForFees opt utxo coinSel = do
    coinSel'@(CoinSelection inps' outs' chgs') <-
        senderPaysFee opt utxo coinSel
    let estimatedFee = feeUpperBound opt coinSel
    let actualFee = computeFee coinSel'

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
    if (   actualFee < estimatedFee
        || getCoin actualFee `div` 2 > getCoin estimatedFee) then
        throwE $ OutOfBoundFee (getCoin actualFee) (getCoin estimatedFee)
    else do
        let neInps = case inps' of
                []   -> error "adjustForFees: empty list of inputs"
                inps -> inps

        let neOuts = case outs' of
                []   -> error "adjustForFees: empty list of outputs"
                outs -> outs
        pure (CoinSelection neInps neOuts chgs')

senderPaysFee
    :: forall m. MonadRandom m
    => FeeOptions
    -> UTxO
    -> CoinSelection
    -> ExceptT FeeError m CoinSelection
senderPaysFee opt utxo  = go
    where
        removeDust
            :: [Coin]
            -> [Coin]
        removeDust = L.filter (> (dustThreshold opt))

        reduceChangeOutputs
            :: Coin
            -> [Coin]
            -> ([Coin], Coin)
        reduceChangeOutputs totalFee chgs =
            case (filter (/= Coin 0) . removeDust) chgs of
                [] ->
                    (removeDust chgs, totalFee)
                xs ->
                    bimap removeDust (Coin . sum . map getCoin)
                    $ L.unzip
                    $ map reduceSingleChange
                    $ divvyFee totalFee xs

        reduceSingleChange
            :: (Coin, Coin)
            -> (Coin, Coin)
        reduceSingleChange (fee, chng)
            | chng >= fee =
                  (Coin $ (getCoin chng) - (getCoin fee), Coin 0 )
            | otherwise =
                  (Coin 0, Coin $ (getCoin fee) - (getCoin chng) )


        go
            :: forall m. MonadRandom m
            => CoinSelection
            -> ExceptT FeeError m CoinSelection
        go coinSel@(CoinSelection inps outs chgs) = do
            -- 1/
            -- We compute fees using all inputs, outputs and changes since
            -- all of them have an influence on the fee calculation.
            let upperBound = feeUpperBound opt coinSel

            -- 2/ Substract fee from all change outputs, proportionally to their value.
            let (chgs', remainingFee) =
                    reduceChangeOutputs upperBound chgs

            -- 3.1/
            -- Should the change cover the fee, we're (almost) good. By removing
            -- change outputs, we make them smaller and may reduce the size of the
            -- transaction, and the fee. Thus, we end up paying slightly more than
            -- the upper bound. We could do some binary search and try to
            -- re-distribute excess across changes until fee becomes bigger.
            if remainingFee == Coin 0 then
                pure $ CoinSelection inps outs chgs'
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
                remFee <- lift $ runMaybeT $ coverRemainingFee remainingFee utxo
                case remFee of
                    Nothing ->
                        throwE $ CannotCoverFee (getCoin remainingFee)
                    Just inps' -> do
                        let extraChange = splitChange (currentBalance inps') chgs
                        pure $ CoinSelection (inps <> inps') outs extraChange

feeUpperBound
    :: FeeOptions
    -> CoinSelection
    -> Coin
feeUpperBound opt (CoinSelection inps outs chgs) =
    (estimate opt) (L.length inps) (map coin outs ++ chgs)

-- | Proportionally divide the fee over each output.
-- Pre-condition 1: The given outputs list shouldn't be empty
-- Pre-condition 2: None of the outputs should be null
divvyFee
    :: Coin
    -> [Coin]
    -> [(Coin, Coin)]
divvyFee _ [] =
    error "divvyFee: empty list"
divvyFee _ outs | (Coin 0) `elem` outs =
    error "divvyFee: some outputs are null"
divvyFee fee outs =
    map (\a -> (feeForOut a, a)) outs
  where
    totalOut :: Word64
    totalOut = (sum . map getCoin) outs

    -- The ratio will be between 0 and 1 so cannot overflow
    feeForOut :: Coin -> Coin
    feeForOut (Coin a) =
        let res = valueAdjust
                  (a `div` totalOut, a `quot` totalOut)
                  fee
        in if isValidCoin res then
            res
        else
            error "feeForOut : fee exceeded maximum valid value for Coin"

    valueAdjust
        :: (Word64, Word64)
        -> Coin
        -> Coin
    valueAdjust (natural, remainder) (Coin c) =
        let remainder' = c `quot` remainder
        in Coin $ natural * c + c `div` remainder + (if remainder' == 0 then 0 else 1)


coverRemainingFee
    :: forall m. MonadRandom m
    => Coin
    -> UTxO
    -> MaybeT m [(TxIn, TxOut)]
coverRemainingFee fee = go []
    where
        go
            :: forall m. MonadRandom m
            => [(TxIn, TxOut)]
            -> UTxO
            -> MaybeT m [(TxIn, TxOut)]
        go acc utxo'
            | currentBalance acc >= fee =
                  MaybeT $ return $ Just acc
            | otherwise = do
                  -- We ignore the size of the fee, and just pick randomly
                  utxoEntryPicked <-
                          lift $ runMaybeT $ pickRandom utxo'
                  case utxoEntryPicked of
                      Just (entry, utxo'') ->
                          go (entry : acc) utxo''
                      Nothing ->
                          -- cannot cover fee
                          MaybeT $ return Nothing

currentBalance
    :: [(TxIn, TxOut)]
    -> Coin
currentBalance inps =
    Coin . sum
    $ map (getCoin . coin . snd) inps


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
splitChange
    :: Coin
    -> [Coin]
    -> [Coin]
splitChange = go
  where
    go remaining as | remaining == Coin 0 =
        as
    go remaining [] = [remaining]
        -- we only create new change if for whatever reason there is none already
        -- or if is some overflow happens when we try to add.
    go remaining [a] =
        let newChange =  Coin $ (getCoin remaining) + (getCoin a)
        in
            (if isValidCoin newChange then [newChange] else [a, remaining])
    go rest@(Coin remaining) ls@(a : as) =
      let piece = fromIntegral remaining `div` (length ls)
          newRemaining = Coin $ fromIntegral $ fromIntegral remaining - piece
          newChange = Coin $ fromIntegral $ piece + ((fromIntegral . getCoin) a)
      in
          (if isValidCoin newChange then
               newChange : go newRemaining as
           else
               a : go rest as)

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
computeFee
    :: CoinSelection
    -> Coin
computeFee (CoinSelection inps outs chgs) =
    collapse
    (map (coin . snd) inps)
    (filter (> Coin 0) $ (map coin outs) <> chgs)
  where
    collapse :: [Coin] -> [Coin] -> Coin
    -- Some remaining inputs together. At this point, we've removed
    -- all outputs and changes, so what's left are simply the actual fees.
    -- It's unrealistic to imagine them being bigger than the max coin value.
    collapse plus [] =
        invariant
        "fees are bigger than max coin value"
        (Coin . sum $ map getCoin plus)
        isValidCoin

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
    collapse []  _ =
        invariant "outputs are bigger than inputs" (undefined) (const False)
