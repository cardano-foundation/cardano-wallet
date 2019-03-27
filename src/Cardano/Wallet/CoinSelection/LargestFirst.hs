{-# LANGUAGE RankNTypes #-}
--{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}


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
    ( CoinSelFinalResult
    , CoinSelOneGoResult (..)
    , CoinSelOneGoResult
    , CoinSelectionError (..)
    , CoinSelectionOptions (..)
    , SelectedUtxo (..)
    , adjustForFees
    , emptySelectedUtxo
    , select
    )
import Cardano.Wallet.Primitive.Types
    ( Coin (..), TxIn, TxOut (..), UTxO (..), balance )
import Control.Monad
    ( foldM )
import Control.Monad.Trans.Except
    ( ExceptT (..), throwE )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Map.Strict
    ( Map )
import Data.Ord
    ( comparing )
import Data.Word
    ( Word64 )

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- | Largest-first input selection policy
largestFirst
    :: forall m. Monad m
    => CoinSelectionOptions
    -> UTxO
    -> NonEmpty TxOut
    -> ExceptT CoinSelectionError m CoinSelFinalResult
largestFirst opt utxo txOutputs = do
    -- Step 1. we will cover transaction outputs starting from the largest
    let txOutputsSorted = NE.toList
            $ NE.sortBy (flip $ comparing coin) txOutputs

    -- Step 2. (TO-DO or not) we need to check if the transaction outputs are not redeemable

    -- Step 3. now for every output payment starting from the largest we will
    --         pick (n=maximumNumberOfInputs) largest outputs from UTxO that
    --         remained from last iteration of Step 3.
    (_, coinSelectionTmpResult) <-
        foldM selectCoins (utxo, []) txOutputsSorted

    -- Step 4. adjust for fee and transform to result type
    return $ adjustForFees opt pickUtxo coinSelectionTmpResult
    where
        pickUtxo :: Coin -> UTxO -> Maybe (TxIn, TxOut)
        pickUtxo v =
            let search :: Word64 -> [(TxIn, TxOut)] -> Maybe (TxIn, TxOut)
                search _ [] = Nothing
                search val ((i, o):rest)
                    | ((getCoin . coin) o) >= val = Just (i, o)
                    | otherwise = search val rest
            in search (getCoin v) . Map.toList . getUTxO

        defCoinSelResult
            :: TxOut
            -> SelectedUtxo
            -> CoinSelOneGoResult
        defCoinSelResult goal selected =
            let currentBalance = getCoin $ selectedBalance selected
                toSubstract = (getCoin . coin) goal
                change = Coin $ currentBalance - toSubstract
            in CoinSelOneGoResult
               { coinSelRequest = goal
               , coinSelOutput  = goal
               , coinSelChange  = [change]
               , coinSelInputs  = selected
               }

        selectCoins
           :: forall m. Monad m
           => (UTxO, [CoinSelOneGoResult])
           -> TxOut
           -> ExceptT CoinSelectionError m (UTxO, [CoinSelOneGoResult])
        selectCoins (currentUtxo, prev) txOutput = do
            -- select coins to cover at least specified value
            (selectedCoins, utxo') <-
                atLeast (maximumNumberOfInputs opt) ((getCoin . coin) txOutput) currentUtxo

            let coinSelectionResult = defCoinSelResult txOutput selectedCoins

            return (utxo', coinSelectionResult : prev)


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
    :: forall m. Monad m
    => Word64
    -> Word64
    -> UTxO
    -> ExceptT CoinSelectionError m (SelectedUtxo, UTxO)
atLeast maxNumInputs targetMin utxo = do
    let nLargest = map (\(f,s) -> (f, UTxO s)) $ nLargestFromMapBy coin maxNumInputs (getUTxO utxo)
    case go emptySelectedUtxo utxo nLargest of
      Nothing -> do
          -- If we failed to cover 'targetMin' it might be because we
          -- depleted the Utxo or simply because our 'maxNumInputs' was
          -- to stringent and in normal conditions we @would have@ covered
          -- targetMin. To diversify the two errors, if
          -- 'utxoBalance utxo >= targetMin' it means this is a max input
          -- failure, otherwise we have genuinely exhausted the utxo.
          let utxoBalance = fromIntegral $ balance utxo
          if utxoBalance < targetMin
             then throwE $ UtxoExhausted utxoBalance targetMin
             else throwE $ MaximumInputsReached maxNumInputs
      Just (selected, remainingUtxo) -> do
          return (selected, remainingUtxo)
    where
        go :: SelectedUtxo
           -> UTxO
           -> [((TxIn, TxOut), UTxO)]
           -> Maybe (SelectedUtxo, UTxO)
        go acc remainingUtxo sorted
            | selectedBalance acc >= (Coin targetMin) = Just (acc, remainingUtxo)
            | otherwise = case sorted of
                  [] -> Nothing
                  (io, remainingUtxo'):sorted' ->
                      go (select io acc) remainingUtxo' sorted'


----------------------------------------------------------------------------
--                       Auxiliary functions                              --
----------------------------------------------------------------------------

nLargestFromMapBy
    :: (Ord b, Ord k) => (a -> b)
    -> Word64
    -> Map k a
    -> [((k, a), Map k a)]
nLargestFromMapBy f n m =
    aux Set.empty $ nLargestFromListBy (f . snd) n (Map.toList m)
  where
    aux _ [] = []
    aux deleted ((k, a) : kas) =
        ((k, a), m `withoutKeys` deleted')
        : aux deleted' kas
      where
        deleted' = Set.insert k deleted
        theMap `withoutKeys` theSet =
            theMap `Map.difference` Map.fromSet (const ()) theSet


-- | Return the @n@ largest elements of the list, from large to small.
-- @O(n)@
nLargestFromListBy
    :: Ord b => (a -> b)
    -> Word64
    -> [a]
    -> [a]
nLargestFromListBy f n = \xs ->
    -- If the map resulting from manipulating @xs@ is empty, we need to
    -- return straight away as otherwise the call to 'Map.findMin' later
    -- would fail.
    let (firstN, rest) = splitAt (fromIntegral n) xs
        acc            = Map.fromListWith (++) $ map (\a -> (f a, [a])) firstN
    in if Map.null acc then [] else go acc rest
  where
    -- We cache the minimum element in the accumulator, since looking this up
    -- is an @O(log n)@ operation.
    --
    -- Invariants:
    -- - Map must contain exactly @n@ elements
    -- - No list in the codomain of the map can be empty
    -- NOTE: Using a PSQ here doesn't really gain us very much. Sure, we can
    -- lookup the minimum element in @O(1)@ time, but /replacing/ the minimum
    -- element still requires @O(log n)@ time. Thus, if we cache the minimum
    -- value we have the same complexity, and avoid an additional depenedency.
    go acc = go' acc (fst (Map.findMin acc))

    -- Inherits invariants from @go@
    -- Precondition: @accMin == fst (Map.findMin acc)@
    go' acc _ []    = concatMap snd $ Map.toDescList acc
    go' acc accMin (a:as)
       | b > accMin = go (replaceMin accMin b a acc) as
       | otherwise  = go' acc accMin as
       where
         b = f a

    -- Replace the minimum entry in the map
    -- Precondition: @accMin@ should be the minimum key of the map.
    replaceMin accMin b a = Map.insertWith (++) b [a] . Map.alter dropOne accMin

    -- Remove one entry from the map
    -- All of the entries in these lists have the same "size" (@b@),
    -- so we just drop the first.
    dropOne Nothing       = error "nLargest': precondition violation"
    dropOne (Just [])     = error "nLargest': invariant violation"
    dropOne (Just [_])    = Nothing
    dropOne (Just (_:as)) = Just as
