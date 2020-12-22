{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- This module contains the implementation of random
-- input selection algorithm


module Cardano.Wallet.Primitive.CoinSelection.Random
    ( random
    ) where

import Prelude

import Cardano.Wallet.Primitive.CoinSelection
    ( CoinSelection (..)
    , CoinSelectionOptions (..)
    , ErrCoinSelection (..)
    , totalBalance
    )
import Cardano.Wallet.Primitive.CoinSelection.LargestFirst
    ( largestFirst )
import Cardano.Wallet.Primitive.Types
    ( distance, invariant )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.Tx
    ( TxIn, TxOut (..), txOutCoin )
import Cardano.Wallet.Primitive.Types.UTxO
    ( UTxO (..), pickRandom )
import Control.Monad
    ( foldM )
import Control.Monad.Trans.Class
    ( lift )
import Control.Monad.Trans.Except
    ( ExceptT (..) )
import Control.Monad.Trans.Maybe
    ( MaybeT (..), runMaybeT )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Ord
    ( comparing )
import Data.Quantity
    ( Quantity (..) )
import Data.Word
    ( Word64 )

import qualified Data.List as L
import qualified Data.List.NonEmpty as NE


-- | Target range for picking inputs
data TargetRange = TargetRange
    { targetMin :: Word64
        -- ^ Minimum value to cover: only the requested amount, no change at all
    , targetAim :: Word64
        -- ^ Ideal case: change equal to requested amount
    , targetMax :: Word64
        -- ^ Maximum value: an arbitrary upper bound (e.g. @2 * targetMin@)
    }

-- | Random-Improve Algorithm
--
-- 1. Randomly select outputs from the UTxO until the payment value is covered.
--    (In the rare case that this fails because the maximum number of
--    transaction inputs has been exceeded, fall back on the largest-first
--    algorithm for this step.)
--
-- 2. The algorithm first makes a random  selection for each output from the UTxO,
--    processing the biggest output first and proceeding in a descending order.
--    If the selection is not successful largest-first fallback kicks in.
--    If the selection is successful for each output then the
--    improvement is tried for each selection, once again starting from the selection
--    made for the biggest output. The improvement is tried for the next biggest output's
--    selection. An output is considered an improvement when:
--
--    (a)  It doesn’t exceed a specified upper limit.
--    (b)  Adding the new output gets us closer to the ideal change value.
--    (c)  It doesn’t exceed a maximum number of transaction inputs.
--
-- This algorithm follows three principles:
--
-- @
-- **Self organisation principle 1**
-- Random selection has a high probability of picking dust outputs precisely
-- when there is a lot of dust in the UTxO.
-- @
--
-- @
-- **Self organisation principle 2**
-- If for each payment request for value `x` we create a change output roughly
-- of the same value `x`, then we will end up with a lot of change outputs in
-- our UTxO of size `x` precisely when we have a lot of payment requests of
-- size `x`
-- @
--
-- @
-- **Self organisation principle 3**
-- Searching the UTxO for additional entries to improve our change output is
-- only useful if the UTxO contains entries that are sufficiently small enough.
-- But precisely when the UTxO contains many small entries, it is less likely
-- that a randomly chosen UTxO entry will push the total above the upper bound
-- we set.
-- @
random
    :: CoinSelectionOptions
    -> NonEmpty TxOut
    -> Quantity "lovelace" Word64
    -> UTxO
    -> ExceptT ErrCoinSelection IO (CoinSelection, UTxO)
random opt outs (Quantity withdrawal) utxo = do
    let descending = NE.toList . NE.sortBy (flip $ comparing txOutCoin)
    let nOuts = fromIntegral $ NE.length outs
    let maxN = fromIntegral $ maximumNumberOfInputs opt nOuts
    randomMaybe <- lift $ runMaybeT $ do
        let initialState = SelectionState maxN utxo (Quantity withdrawal) []
        foldM makeSelection initialState (descending outs)
    case randomMaybe of
        Just (SelectionState maxN' utxo' _ res) -> do
            (_, sel, remUtxo) <- lift $
                foldM improveTxOut (maxN', mempty, utxo') (reverse res)
            let result = sel { withdrawal }
            pure (result, remUtxo)
        Nothing ->
            largestFirst opt outs (Quantity withdrawal) utxo

-- A little type-alias to ease signature below
data SelectionState = SelectionState
    { _maxN :: Word64
    , _utxo :: UTxO
    , _withdrawal :: Quantity "lovelace" Word64
    , _selection :: [CoinSelection]
    } deriving Show

-- | Perform a random selection on a given output, without improvement.
makeSelection
    :: SelectionState
    -> TxOut
    -> MaybeT IO SelectionState
makeSelection (SelectionState maxN utxo0 withdrawal0 selection0) txout = do
    (selection', utxo') <- coverRandomly ([], utxo0)
    return $ SelectionState
        { _maxN = maxN - fromIntegral (L.length $ inputs selection')
        , _utxo = utxo'
        , _withdrawal = (\w -> w - withdrawal selection') <$> withdrawal0
        , _selection = selection' : selection0
        }
  where
    TargetRange{targetMin} = mkTargetRange $ unCoin $ txOutCoin txout

    coverRandomly
        :: ([(TxIn, TxOut)], UTxO)
        -> MaybeT IO (CoinSelection, UTxO)
    coverRandomly (inps, utxo)
        | L.length inps > fromIntegral maxN =
            MaybeT $ return Nothing
        | currentBalance >= targetMin = do
            let remainder
                    | inputBalance >= targetMin = 0
                    | otherwise = targetMin - inputBalance
            MaybeT $ return $ Just
                ( mempty
                    { inputs = inps
                    , outputs = [txout]
                    , withdrawal = min remainder (getQuantity withdrawal0)
                    }
                , utxo
                )
        | otherwise = do
            pickRandomT utxo >>= \(io, utxo') -> coverRandomly (io:inps, utxo')
      where
        -- Withdrawal can only count towards the input balance if there's been
        -- at least one selected input.
        currentBalance
            | null inps && null selection0 = inputBalance
            | otherwise = totalBalance withdrawal0 inps

        inputBalance =
            totalBalance (Quantity 0) inps

-- | Perform an improvement to random selection on a given output.
improveTxOut
    :: (Word64, CoinSelection, UTxO)
    -> CoinSelection
    -> IO (Word64, CoinSelection, UTxO)
improveTxOut (maxN0, selection, utxo0) (CoinSelection inps0 withdraw _ outs _ _) = do
    (maxN, inps, utxo) <- improve (maxN0, inps0, utxo0)
    return
        ( maxN
        , selection <> mempty
            { inputs = inps
            , outputs = outs
            , change = mkChange (Quantity withdraw) outs inps
            , withdrawal = withdraw
            }
        , utxo
        )
  where
    target = mkTargetRange $ sum $ unCoin . txOutCoin <$> outs

    improve
        :: (Word64, [(TxIn, TxOut)], UTxO)
        -> IO (Word64, [(TxIn, TxOut)], UTxO)
    improve (maxN, inps, utxo)
        | maxN >= 1 && totalBalance (Quantity withdraw) inps < targetAim target = do
            runMaybeT (pickRandomT utxo) >>= \case
                Nothing ->
                    return (maxN, inps, utxo)
                Just (io, utxo') | isImprovement io inps -> do
                    let inps' = io : inps
                    let maxN' = maxN - 1
                    improve (maxN', inps', utxo')
                Just _ ->
                    return (maxN, inps, utxo)
        | otherwise =
            return (maxN, inps, utxo)

    isImprovement :: (TxIn, TxOut) -> [(TxIn, TxOut)] -> Bool
    isImprovement io selected =
        let
            balanceWithExtraInput =
                totalBalance (Quantity withdraw) (io : selected)

            balanceWithoutExtraInput =
                totalBalance (Quantity withdraw) selected

            condA = -- (a) It doesn’t exceed a specified upper limit.
                balanceWithExtraInput
                <
                targetMax target

            condB = -- (b) Addition gets us closer to the ideal change
                distance (targetAim target) balanceWithExtraInput
                <
                distance (targetAim target) balanceWithoutExtraInput

            -- (c) Doesn't exceed maximum number of inputs
            -- Guaranteed by the precondition on 'improve'.
        in
            condA && condB

{-------------------------------------------------------------------------------
                                 Internals
-------------------------------------------------------------------------------}

-- | Re-wrap 'pickRandom' in a 'MaybeT' monad
pickRandomT :: UTxO -> MaybeT IO ((TxIn, TxOut), UTxO)
pickRandomT =
    MaybeT . fmap (\(m,u) -> (,u) <$> m) . pickRandom

-- | Compute the target range for a given output
mkTargetRange :: Word64 -> TargetRange
mkTargetRange base = TargetRange
    { targetMin = base
    , targetAim = 2 * base
    , targetMax = 3 * base
    }

-- | Compute corresponding change outputs from a target output and a selection
-- of inputs.
--
-- > pre-condition: the output must be smaller (or eq) than the sum of inputs
mkChange :: Quantity "lovelace" Word64 -> [TxOut] -> [(TxIn, TxOut)] -> [Coin]
mkChange withdraw outs inps =
    let
        out = sum $ unCoin . txOutCoin <$> outs
        selected = invariant
            "mkChange: output is smaller than selected inputs!"
            (totalBalance withdraw inps)
            (>= out)
        Coin maxCoinValue = maxBound
    in
        case selected - out of
            c | c > maxCoinValue ->
                let h = (c `div` 2) in [Coin h, Coin (c - h)]
            c | c == 0 ->
                []
            c ->
                [ Coin c ]
