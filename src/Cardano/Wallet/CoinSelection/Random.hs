{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: MIT
--
-- This module contains the implementation of random
-- input selection algorithm


module Cardano.Wallet.CoinSelection.Random (
    random
  ) where

import Prelude

import Cardano.Wallet.CoinSelection
    ( CoinSelection (..), CoinSelectionError (..), CoinSelectionOptions (..) )
import Cardano.Wallet.Primitive.Types
    ( Coin (..), TxIn, TxOut (..), UTxO (..), balance, excluding, isValidCoin )
import Control.Monad
    ( foldM, guard, when )
import Control.Monad.Trans.Class
    ( lift )
import Control.Monad.Trans.Except
    ( ExceptT (..), throwE )
import Crypto.Number.Generate
    ( generateBetween )
import Crypto.Random.Types
    ( MonadRandom, getRandomBytes )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Map.Strict
    ( Map )
import Data.Maybe
    ( fromMaybe )
import Data.Ord
    ( comparing )
import Data.Word
    ( Word64 )

import qualified Cardano.Wallet.CoinSelection.LargestFirst as LargestFirst
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- Target range for picking inputs
data TargetRange = TargetRange
    { targetMin :: Coin
    , targetAim :: Coin
    , targetMax :: Coin
    }

-- Random input selection policy
random
    :: forall m. MonadRandom m
    => CoinSelectionOptions
    -> UTxO
    -> NonEmpty TxOut
    -> ExceptT CoinSelectionError m CoinSelection
random opt utxo txOutputs = do
    let txOutputsSorted = NE.toList
            $ NE.sortBy (flip $ comparing coin) txOutputs
    let n = maximumNumberOfInputs opt

    let moneyRequested = sum $ (getCoin . coin) <$> txOutputsSorted
    let utxoBalance = fromIntegral $ balance utxo
    let numberOfUtxoEntries = fromIntegral $ L.length $ (Map.toList . getUTxO) utxo
    let numberOfTransactionOutputs = fromIntegral $ NE.length txOutputs

    when (utxoBalance < moneyRequested)
        $ throwE $ NotEnoughMoney utxoBalance moneyRequested

    when (numberOfUtxoEntries < numberOfTransactionOutputs)
        $ throwE $ UtxoNotEnoughFragmented numberOfUtxoEntries numberOfTransactionOutputs

    (_, res) <- foldM (processTxOut n) (utxo, mempty) txOutputsSorted
    pure res


-- Selecting coins to cover at least the specified value
-- with LargestFirst fallback and subsequent iterative improvement
-- to optimize selection further
-- The details of the algorithm are following:
-- (a) transaction outputs are processed starting from the largest one
-- (b) random selection is tried. The random UTxO entry is picked and checked
--     whether it covers the transaction output (ie., `targetMin` of TargetRange.
--     If no, then additional UTxO entry is picked. If successive picking of inputs
--     gives rise to total the inputs sum covering the transaction output
--     then the optimization described in step (c) is tried.
--     If the random selection leads to the number of inputs that exceeds `maximumNumberOfInputs`
--     then, for both a given transaction output and UTxO as being at the beginning of step (b),
--     fallback LargestFirst algoritm is tried.
-- (c) candidate input selection obtained in step (b) is optimized. Both `targetAim` and `targetMax`
--     as pinpointed in TargetRange drive the optimization. Here, we pick randomly the next UTxO entry
--     from remaining UTxO and check if it is improved as depicted in `isImprovement`. If not, then
--     the optimization ends with returning its initial selection. Otherwise, the procedure tries to
--     optimize more.
processTxOut
    :: forall m. MonadRandom m
    => Word64
    -> (UTxO, CoinSelection)
    -> TxOut
    -> ExceptT CoinSelectionError m (UTxO, CoinSelection)
processTxOut maxNumInputs (utxo, selection) txout = do
    atLeast ([], getUTxO utxo) >>= improve >>= \case
        Just (inps,utxoMap) -> do
            let change =
                    ((sum . (map (getCoin . coin . snd))) inps)
                    - ((getCoin . coin) txout)
            pure (UTxO utxoMap
                 , selection <> CoinSelection
                     { inputs = inps
                     , outputs = [txout]
                     , change = [Coin change]
                     }
                 )
        Nothing ->
            throwE $ MaximumInputsReached maxNumInputs

    where
        atLeast
            :: forall m. MonadRandom m
            => ([(TxIn, TxOut)], Map TxIn TxOut)
            -> ExceptT CoinSelectionError m ([(TxIn, TxOut)], Map TxIn TxOut)
        atLeast (inps, utxoMap)
            | L.length inps > (fromIntegral maxNumInputs) = do
                  let entries = Map.toList utxoMap
                  case LargestFirst.atLeast (entries, selection) txout of
                      Just (utxo', selection') -> do
                          let oldInps =
                                  (Set.fromList . map fst) $ inputs selection
                          let diff =
                                  (UTxO . Map.fromList . inputs) selection' `excluding` oldInps
                          pure ((Map.toList . getUTxO) diff, Map.fromList utxo')
                      Nothing ->
                          throwE $ MaximumInputsReached maxNumInputs
            | sum (map (getCoin . coin . snd) inps)
              > ((getCoin . targetMin . mkTargetRange . coin) txout) =
                  pure (inps, utxoMap)
            | otherwise = do
                  let currBalance = fromIntegral $ balance utxo
                  (io, utxoMap') <- pickRandom utxoMap >>=
                      maybe (throwE $ UtxoDepleted currBalance ((getCoin . coin) txout)) return
                  atLeast (io:inps, utxoMap')

        improve
            :: forall m. MonadRandom m
            => ([(TxIn, TxOut)], Map TxIn TxOut)
            -> ExceptT CoinSelectionError m (Maybe ([(TxIn, TxOut)], Map TxIn TxOut))
        improve (inps, utxoMap) = do
            let currBalance = fromIntegral $ balance utxo
            (io, utxoMap') <- pickRandom utxoMap >>=
                maybe (throwE $ UtxoDepleted currBalance ((getCoin . coin) txout)) return
            case isImprovement io inps of
                Nothing ->
                    pure $ Just (inps, utxoMap)
                Just inps' ->
                    improve (inps', utxoMap')

        isImprovement
            :: (TxIn, TxOut)
            -> [(TxIn, TxOut)]
            -> Maybe [(TxIn, TxOut)]
        isImprovement io selected = do

            guard
                ((selectedBalance selected' <= targetMax targetRange)
                &&
                (distance (targetAim targetRange) (selectedBalance selected') <
                 distance (targetAim targetRange) (selectedBalance selected))
                &&
                (L.length selected' <= fromIntegral maxNumInputs))

            return selected'
            where
                selected' = io : selected
                selectedBalance = Coin . sum . (map (getCoin . coin . snd))
                distance (Coin val1) (Coin val2) =
                    if val1 < val2 then
                        val2 - val1
                    else
                        val1 - val2
                targetRange = (mkTargetRange . coin) txout


mkTargetRange :: Coin -> TargetRange
mkTargetRange val =
    fromMaybe (privacyOffTargetRange val) (tryCanonicalTargetRange val)
    where
        tryCanonicalTargetRange :: Coin -> Maybe TargetRange
        tryCanonicalTargetRange coin@(Coin v) = do
            let targetMin = coin
            targetAim <-
                if isValidCoin (Coin $ 2*v) then Just (Coin $ 2*v) else Nothing
            targetMax <-
                if isValidCoin (Coin $ 3*v) then Just (Coin $ 3*v) else Nothing
            return TargetRange {..}
        privacyOffTargetRange :: Coin -> TargetRange
        privacyOffTargetRange v =
            TargetRange v v v


-- Pick a random element from a map
-- Returns 'Nothing' if the map is empty
pickRandom
    :: MonadRandom m
    => Map k a
    -> m (Maybe ((k, a), Map k a))
pickRandom m
    | Map.null m = return Nothing
    | otherwise  = (withIx m) . fromIntegral <$>
                   generateBetween 0 (fromIntegral (Map.size m - 1))
    where
        withIx
            :: Map k a
            -> Int
            -> Maybe ((k, a), Map k a)
        withIx m' ix = Just (Map.elemAt ix m', Map.deleteAt ix m')


instance MonadRandom m => MonadRandom (ExceptT e m) where
    getRandomBytes = lift . getRandomBytes
