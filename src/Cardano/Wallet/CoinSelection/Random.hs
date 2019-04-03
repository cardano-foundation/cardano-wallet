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
    ( Coin (..), TxIn, TxOut (..), UTxO (..), isValidCoin )
import Control.Monad
    ( foldM, guard )
import Control.Monad.Trans.Class
    ( lift )
import Control.Monad.Trans.Except
    ( ExceptT (..) )
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

-- Target range for picking inputs
data TargetRange = TargetRange
    { targetMin :: Coin
    , targetAim :: Coin
    , targetMax :: Coin
    }

-- Random input selection policy
-- The details of the algorithm are following:
-- (a) transaction outputs are processed starting from the largest one
-- (b) random selection is tried. The random UTxO entry is picked and checked
--     whether it covers the transaction output (ie., `targetMin` of TargetRange).
--     If no, then additional UTxO entry is picked. If successive picking of inputs
--     gives rise to total the inputs sum covering the transaction output
--     then the optimization described in step (c) is tried.
--     If the random selection leads to the number of inputs that exceeds `maximumNumberOfInputs`
--     then the selection is deemed unsuccessful.
-- (c) candidate input selection obtained in step (b), if successful, is optimized.
--     Both `targetAim` and `targetMax` as pinpointed in TargetRange drive the optimization.
--     Here, we pick randomly the next UTxO entry from remaining UTxO and
--     check if it is improved as depicted in `isImprovement`. If not, then
--     the optimization ends with returning its starting selection. Otherwise, the procedure tries to
--     optimize more.
-- If the above algoritm fails to select coins then for both a given transaction outputs and initial UTxO
--     fallback LargestFirst algoritm is tried.
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

    randomMaybe <- foldM (processTxOut n) (Just (utxo, mempty)) txOutputsSorted

    case randomMaybe of
        Just (_,res) ->
            return res
        Nothing ->
            LargestFirst.largestFirst opt utxo txOutputs


processTxOut
    :: forall m. MonadRandom m
    => Word64
    -> Maybe (UTxO, CoinSelection)
    -> TxOut
    -> m (Maybe (UTxO, CoinSelection))
processTxOut maxNumInputs input txout =
    case input of
        Just (utxo, selection) -> do
            atLeast ([], getUTxO utxo) >>= improve >>= \case
                Just (inps,utxoMap) -> do
                    let change =
                            ((sum . (map (getCoin . coin . snd))) inps)
                            - ((getCoin . coin) txout)
                    pure $
                        Just (UTxO utxoMap
                             , selection <> CoinSelection
                                 { inputs = inps
                                 , outputs = [txout]
                                 , change = [Coin change]
                                 }
                             )
                Nothing ->
                    return Nothing
        Nothing ->
            return Nothing
    where
        atLeast
            :: forall m. MonadRandom m
            => ([(TxIn, TxOut)], Map TxIn TxOut)
            -> m (Maybe ([(TxIn, TxOut)], Map TxIn TxOut))
        atLeast (inps, utxoMap)
            | L.length inps > (fromIntegral maxNumInputs) =
                  return Nothing
            | sum (map (getCoin . coin . snd) inps)
              >= ((getCoin . targetMin . mkTargetRange . coin) txout) =
                  pure $ Just (inps, utxoMap)
            | otherwise = do
                  (maybe Nothing Just <$> pickRandom utxoMap) >>= \case
                      Just (io, utxoMap') ->
                          atLeast (io:inps, utxoMap')
                      Nothing -> return Nothing

        improve
            :: forall m. MonadRandom m
            => Maybe ([(TxIn, TxOut)], Map TxIn TxOut)
            -> m (Maybe ([(TxIn, TxOut)], Map TxIn TxOut))
        improve inp =
            case inp of
                Just (inps, utxoMap) -> do
                    (maybe Nothing Just <$> pickRandom utxoMap) >>= \case
                        Just (io, utxoMap') ->
                            case isImprovement io inps of
                                Nothing ->
                                    pure $ Just (inps, utxoMap)
                                Just inps' -> do
                                    let threshold = targetAim $ (mkTargetRange . coin) txout
                                    if selectedBalance inps' >= threshold then
                                        pure $ Just (inps', utxoMap')
                                    else
                                        improve $ Just (inps', utxoMap')
                        Nothing -> return $ Just (inps, utxoMap)
                Nothing ->
                    return Nothing

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
                distance (Coin val1) (Coin val2) =
                    if val1 < val2 then
                        val2 - val1
                    else
                        val1 - val2
                targetRange = (mkTargetRange . coin) txout

        selectedBalance
            :: [(TxIn, TxOut)]
            -> Coin
        selectedBalance = Coin . sum . (map (getCoin . coin . snd))

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
