{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

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
    ( MonadRandom )
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

-- | Target range for picking inputs
data TargetRange = TargetRange
    { targetMin :: Natural
        -- ^ Minimum value to cover: only the requested amount, no change at all
    , targetAim :: Natural
        -- ^ Ideal case: change equal to requested amount
    , targetMax :: Natural
        -- ^ Maximum value: an arbitrary upper bound (e.g. @2 * targetMin@)
    }

-- | Random-Improve Algorithm
--
-- 1. Randomly select outputs from the UTxO until the payment value is covered.
--    (In the rare case that this fails because the maximum number of transaction
--    inputs has been exceeded, fall-back on the largest-first algorithm for this
--    step.)
--
-- 2. Randomly select outputs from the UTxO, considering for each output if that
--    output is animprovement. If it is, add it to the transaction, and keep
--    going. An output is considered an improvement when:
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
    :: forall m. MonadRandom m
    => CoinSelectionOptions
    -> UTxO
    -> NonEmpty TxOut
    -> ExceptT CoinSelectionError m CoinSelection
random opt utxo txOutputs = do
    let txOutputsSorted = NE.toList
            $ NE.sortBy (flip $ comparing coin) txOutputs
    let n = maximumNumberOfInputs opt

    randomMaybe <- lift $ foldM (processTxOut n) (Just (utxo, mempty)) txOutputsSorted

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
