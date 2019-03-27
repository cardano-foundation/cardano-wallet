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
    ( foldM )
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
largestFirst opt utxo txOutputs = do
    let txOutputsSorted = NE.toList $ NE.sortBy (flip $ comparing coin) txOutputs
    let n = fromIntegral $ maximumNumberOfInputs opt
    let nLargest = take n . L.sortBy (flip $ comparing (coin . snd)) . Map.toList . getUTxO
    -- FIXME ? we need to check if the transaction outputs are not redeemable
    case foldM atLeast (nLargest utxo, mempty) txOutputsSorted of
        Just (_, s) -> return s
        -- If we failed to cover 'target' it might be because we
        -- depleted the Utxo or simply because our 'maxNumInputs' was
        -- to stringent and in normal conditions we @would have@ covered
        -- targetMin. To diversify the two errors, if
        -- 'utxoBalance utxo >= targetMin' it means this is a max input
        -- failure, otherwise we have genuinely exhausted the utxo.
        Nothing -> do
            let utxoBalance = fromIntegral $ balance utxo
            let target = sum $ (getCoin . coin) <$> txOutputs
            if utxoBalance < target then
                throwE $ UtxoExhausted utxoBalance target
            else
                throwE $ MaximumInputsReached (fromIntegral n)

{-------------------------------------------------------------------------------
                       Helper types and functions
-------------------------------------------------------------------------------}

-- Select coins to cover at least the specified value
-- When we fail in the random selection policy because we exceeded the maximum
-- number of inputs @n@, we fallback on the 'largestFirstFallback'. We select
-- the @n@ largest inputs from the UTxO in a single linear pass, then walk over
-- these from large to small to try and cover the value we need to cover.
-- If this fails, we have no further fallbacks and this payment request is
-- not satisfiable.
--
-- If it succeeds, we can then use this as the basis for another call to
-- the random input selection to try and construct a more useful change output
-- (provided we haven't used up all available inputs yet).
atLeast
    :: ([(TxIn, TxOut)], CoinSelection)
    -> TxOut
    -> Maybe ([(TxIn, TxOut)], CoinSelection)
atLeast (utxo0, selection) txout = go (getCoin $ coin txout, mempty) utxo0 where
    go (target, ins) utxo
        | target <= 0 = Just
            ( utxo
            , selection <> CoinSelection
                { inputs = ins
                , outputs = [txout]
                , change = [Coin (abs target)]
                }
            )
        | null utxo =
            Nothing
        | otherwise =
            let
                (inp, out):utxo' = utxo
                target' = target - getCoin (coin out)
            in
                go (target', (inp, out):ins) utxo'
