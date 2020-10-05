{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

{- HLINT ignore "Unused LANGUAGE pragma" -}

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
    ( CoinSelection (..)
    , CoinSelectionOptions (..)
    , ErrCoinSelection (..)
    , totalBalance
    )
import Cardano.Wallet.Primitive.Types
    ( Coin (..), TxIn, TxOut (..), UTxO (..) )
import Control.Arrow
    ( left )
import Control.Monad
    ( when )
import Control.Monad.Trans.Except
    ( ExceptT (..), except, throwE )
import Data.Functor
    ( ($>) )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Ord
    ( Down (..) )
import Data.Quantity
    ( Quantity (..) )
import Data.Word
    ( Word64 )

import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map

-- | Largest-first input selection policy
largestFirst
    :: forall m e. Monad m
    => CoinSelectionOptions e
    -> NonEmpty TxOut
    -> Quantity "lovelace" Word64
    -> UTxO
    -> ExceptT (ErrCoinSelection e) m (CoinSelection, UTxO)
largestFirst opt outs withdrawal utxo = do
    let nOuts = fromIntegral $ NE.length outs
    let maxN = fromIntegral $ maximumNumberOfInputs opt nOuts
    let nLargest = take maxN
            . L.sortOn (Down . coin . snd)
            . Map.toList
            . getUTxO
    let guard = except . left ErrInvalidSelection . validate opt

    case atLeast (nLargest utxo) withdrawal (NE.toList outs) of
        Just (utxo', s) ->
            guard s $> (s, UTxO $ Map.fromList utxo')
        Nothing -> do
            let moneyRequested = sum $ (getCoin . coin) <$> outs
            let utxoList = Map.toList $ getUTxO utxo
            let total = totalBalance withdrawal utxoList
            let nUtxo = fromIntegral $ Map.size $ getUTxO utxo

            when (total < moneyRequested)
                $ throwE $ ErrNotEnoughMoney total moneyRequested

            when (maxN > nUtxo)
                $ throwE ErrInputsDepleted

            throwE $ ErrMaximumInputsReached (fromIntegral maxN)

-- Selecting coins to cover at least the specified value
-- The details of the algorithm are following:
--
-- (a) transaction outputs are considered as a whole (sum of all outputs).
--
-- (b) `maximumNumberOfInputs` biggest available UTxO inputs are taken
--      into consideration. They constitute a candidate UTxO inputs from
--      which coin selection will be tried.
--
-- (c) the biggest candidate UTxO input is tried first to cover the transaction
--     total output. If the input is not enough, then the next biggest one is added
--     to check if they can cover the total.
--
--     This process is continued until the total is covered or the candidates UTxO
--     inputs are depleted. In the latter case `MaximumInputsReached` error is
--     triggered.
atLeast
    :: [(TxIn, TxOut)]
    -> Quantity "lovelace" Word64
    -> [TxOut]
    -> Maybe ([(TxIn, TxOut)], CoinSelection)
atLeast utxo0 (Quantity withdrawal) outs =
    coverOutput (toInteger $ sum $ getCoin . coin <$> outs, mempty) utxo0
  where
    coverOutput
        :: (Integer, [(TxIn, TxOut)])
        -> [(TxIn, TxOut)]
        -> Maybe ([(TxIn, TxOut)], CoinSelection)
    coverOutput (target, ins) utxo
        | target <= 0 = Just
            ( utxo
            , mempty
                { inputs  = ins
                , outputs = outs
                , change  = filter (/= (Coin 0)) [Coin (fromIntegral $ abs target)]
                , withdrawal
                }
            )

        | null utxo =
            Nothing

        | otherwise =
            let
                (inp, out):utxo' = utxo
                outAmount = getCoin (coin out)
                -- NOTE: For the /first/ selected input, we also use the entire
                -- withdrawal. If it's not enough, new inputs will be selected.
                target'
                    | null ins  = target - fromIntegral (outAmount + withdrawal)
                    | otherwise = target - fromIntegral outAmount
            in
                coverOutput (target', (inp, out):ins) utxo'
