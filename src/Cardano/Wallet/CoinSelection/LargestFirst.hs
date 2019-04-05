{-# LANGUAGE RankNTypes #-}


-- |
-- Copyright: © 2018-2019 IOHK
-- License: MIT
--
-- This module contains the implementation of largestFirst
-- input selection algorithm


module Cardano.Wallet.CoinSelection.LargestFirst (
    largestFirst
  ) where

import Prelude

import Cardano.Wallet.CoinSelection
    ( CoinSelection (..), CoinSelectionError (..), CoinSelectionOptions (..) )
import Cardano.Wallet.Primitive.Types
    ( Coin (..), TxIn, TxOut (..), UTxO (..), balance )
import Control.Monad
    ( foldM, when )
import Control.Monad.Trans.Except
    ( ExceptT (..), throwE )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Ord
    ( comparing )

import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map


-- | Largest-first input selection policy
largestFirst
    :: forall m. Monad m
    => CoinSelectionOptions
    -> UTxO
    -> NonEmpty TxOut
    -> ExceptT CoinSelectionError m CoinSelection
largestFirst opt utxo outs = do
    let descending = NE.toList . NE.sortBy (flip $ comparing coin)
    let n = fromIntegral $ maximumNumberOfInputs opt
    let nLargest = take n . L.sortBy (flip $ comparing (coin . snd)) . Map.toList . getUTxO

    case foldM atLeast (nLargest utxo, mempty) (descending outs) of
        Just (_, s) ->
            return s
        Nothing -> do
            let moneyRequested = sum $ (getCoin . coin) <$> (descending outs)
            let utxoBalance = fromIntegral $ balance utxo
            let numberOfUtxoEntries = fromIntegral $ L.length $ (Map.toList . getUTxO) utxo
            let numberOfTransactionOutputs = fromIntegral $ NE.length outs

            when (utxoBalance < moneyRequested)
                $ throwE $ NotEnoughMoney utxoBalance moneyRequested

            when (numberOfUtxoEntries < numberOfTransactionOutputs)
                $ throwE $ UtxoNotEnoughFragmented numberOfUtxoEntries numberOfTransactionOutputs

            throwE $ MaximumInputsReached (fromIntegral n)


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
        :: (Int, [(TxIn, TxOut)])
        -> [(TxIn, TxOut)]
        -> Maybe ([(TxIn, TxOut)], CoinSelection)
    coverOutput (target, ins) utxo
        | target <= 0 = Just
            ( utxo
            , selection <> CoinSelection
                { inputs = ins
                , outputs = [txout]
                , change = [Coin (fromIntegral $ abs target)]
                }
            )
        | null utxo =
            Nothing
        | otherwise =
            let
                (inp, out):utxo' = utxo
                target' =
                    target - (fromIntegral (getCoin (coin out)))
            in
                coverOutput (target', (inp, out):ins) utxo'
