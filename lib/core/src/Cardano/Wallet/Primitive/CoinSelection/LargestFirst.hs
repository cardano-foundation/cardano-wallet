{-# LANGUAGE RankNTypes #-}


-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- This module contains the implementation of largestFirst
-- input selection algorithm


module Cardano.Wallet.Primitive.CoinSelection.LargestFirst (
    largestFirst
  ) where

import Prelude

import Cardano.Wallet.Primitive.CoinSelection
    ( CoinSelection (..), CoinSelectionOptions (..), ErrCoinSelection (..) )
import Cardano.Wallet.Primitive.Types
    ( Coin (..), TxIn, TxOut (..), UTxO (..), balance )
import Control.Arrow
    ( left )
import Control.Monad
    ( foldM, when )
import Control.Monad.Trans.Except
    ( ExceptT (..), except, throwE )
import Data.Functor
    ( ($>) )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Ord
    ( comparing )

import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map


-- | Largest-first input selection policy
largestFirst
    :: forall m e. Monad m
    => CoinSelectionOptions e
    -> NonEmpty TxOut
    -> UTxO
    -> ExceptT (ErrCoinSelection e) m (CoinSelection, UTxO)
largestFirst opt outs utxo = do
    let descending = NE.toList . NE.sortBy (flip $ comparing coin)
    let nOuts = fromIntegral $ NE.length outs
    let maxN = fromIntegral $ maximumNumberOfInputs opt (fromIntegral nOuts)
    let nLargest = take maxN
            . L.sortBy (flip $ comparing (coin . snd))
            . Map.toList
            . getUTxO
    let guard = except . left ErrInvalidSelection . validate opt

    case foldM atLeast (nLargest utxo, mempty) (descending outs) of
        Just (utxo', s) ->
            guard s $> (s, UTxO $ Map.fromList utxo')
        Nothing -> do
            let moneyRequested = sum $ (getCoin . coin) <$> (descending outs)
            let utxoBalance = fromIntegral $ balance utxo
            let nUtxo = fromIntegral $ L.length $ (Map.toList . getUTxO) utxo

            when (utxoBalance < moneyRequested)
                $ throwE $ ErrNotEnoughMoney utxoBalance moneyRequested

            when (nUtxo < nOuts)
                $ throwE $ ErrUtxoNotEnoughFragmented nUtxo nOuts

            when (fromIntegral maxN > nUtxo)
                $ throwE ErrInputsDepleted

            throwE $ ErrMaximumInputsReached (fromIntegral maxN)

-- Selecting coins to cover at least the specified value
-- The details of the algorithm are following:
-- (a) transaction outputs are processed starting from the largest one
-- (b) `maximumNumberOfInputs` biggest available UTxO inputs are taken
--      into consideration. They constitute a candidate UTxO inputs from
--      which coin selection will be tried. Each output is treated independently
--      with the heuristic described in (c).
-- (c) the biggest candidate UTxO input is tried first to cover the transaction
--     output. If the input is not enough, then the next biggest one is added
--     to check if they can cover the transaction output. This process is continued
--     until the output is covered or the candidates UTxO inputs are depleted.
--     In the latter case `MaximumInputsReached` error is triggered. If the transaction
--     output is covered the next biggest one is processed. Here, the biggest
--     UTxO input, not participating in the coverage, is taken. We are back at (b)
--     step as a result
--
-- The steps are continued until all transaction are covered.
atLeast
    :: ([(TxIn, TxOut)], CoinSelection)
    -> TxOut
    -> Maybe ([(TxIn, TxOut)], CoinSelection)
atLeast (utxo0, selection) txout =
    coverOutput (fromIntegral $ getCoin $ coin txout, mempty) utxo0
  where
    coverOutput
        :: (Integer, [(TxIn, TxOut)])
        -> [(TxIn, TxOut)]
        -> Maybe ([(TxIn, TxOut)], CoinSelection)
    coverOutput (target, ins) utxo
        | target <= 0 = Just
            ( utxo
            , selection <> CoinSelection
                { inputs = ins
                , outputs = [txout]
                , change = filter (/= (Coin 0)) [Coin (fromIntegral $ abs target)]
                }
            )
        | null utxo =
            Nothing
        | otherwise =
            let
                (inp, out):utxo' = utxo
                target' = target - (fromIntegral (getCoin (coin out)))
            in
                coverOutput (target', (inp, out):ins) utxo'
