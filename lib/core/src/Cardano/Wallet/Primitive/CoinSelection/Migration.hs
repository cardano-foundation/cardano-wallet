{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: Apache-2.0
--
-- This module contains an algorithm to select coins for migration from legacy
-- wallets to newer wallets.

module Cardano.Wallet.Primitive.CoinSelection.Migration
    ( selectCoinsForMigration
    ) where

import Prelude

import Cardano.Wallet.Primitive.CoinSelection
    ( CoinSelection (..), inputBalance, outputBalance )
import Cardano.Wallet.Primitive.Fee
    ( Fee (..), FeeOptions (..) )
import Cardano.Wallet.Primitive.Types
    ( Coin (..), TxIn (..), TxOut (..), UTxO (..) )
import Control.Monad.Trans.State
    ( State, evalState, get, put )
import Data.List
    ( splitAt )
import Data.Maybe
    ( mapMaybe )
import Data.Word
    ( Word8 )

import qualified Data.Map.Strict as Map

selectCoinsForMigration
    :: FeeOptions
        -- ^ Fee computation and threshold definition
    -> Word8
        -- ^ Maximum number of inputs we can select per transaction
    -> UTxO
        -- ^ UTxO to deplete
    -> [CoinSelection]
selectCoinsForMigration feeOpts batchSize utxo =
    evalState migrate (Map.toList (getUTxO utxo))
  where
    migrate :: State [(TxIn, TxOut)] [CoinSelection]
    migrate = do
        batch <- getNextBatch
        if null batch then
            pure []
        else case adjustForFee (mkCoinSelection batch) of
            Nothing -> pure []
            Just coinSel -> do
                rest <- migrate
                pure (coinSel:rest)

    -- Construct a provisional 'CoinSelection' from the given selected inputs.
    -- Note that the selection may look a bit weird at first sight as it has
    -- not outputs (we are paying everything to ourselves!)
    mkCoinSelection :: [(TxIn, TxOut)] -> CoinSelection
    mkCoinSelection inps = CoinSelection
        { inputs = inps
        , outputs = []
        , change =
            let chgs = mapMaybe (noDust . snd) inps
            in if null chgs then [dustThreshold feeOpts] else chgs
        }
      where
        noDust :: TxOut -> Maybe Coin
        noDust (TxOut _ c)
            | c < dustThreshold feeOpts = Nothing
            | otherwise = Just c

    -- | Attempt to balance the coin selection by reducing or increasing the
    -- change values based on the computed fees.
    adjustForFee :: CoinSelection -> Maybe CoinSelection
    adjustForFee !coinSel
        -- If there's no change, nothing to adjust
        | null (change coinSel) = Nothing

        -- No difference between required and computed, we're done
        | diff == 0 = Just coinSel

        -- Otherwise, we have 2 cases:
        --
        -- 1/ diff < 0
        -- We aren't giving enough as fee, so we need to reduce one output.
        --
        -- 2/ diff > 0
        -- We have some surplus so we add it to an arbitrary output
        --
        -- If both cases we can simply modify one output by adding `diff`, the
        -- sign of `diff` making for the right modification.
        -- We then recursively call ourselves for this might reduce the number
        -- of outputs and change the fee.
        | otherwise = Just $ coinSel
            { change = modifyFirst (change coinSel) (+ diff) }
      where
        diff :: Integer
        diff = fromIntegral actualFee - fromIntegral requiredFee
          where
            (Fee requiredFee) = estimate feeOpts coinSel
            actualFee = inputBalance coinSel - outputBalance coinSel

    -- | Apply the given function to the first coin of the list. If the
    -- operation makes the 'Coin' smaller than the dust threshold, the coin is
    -- discarded.
    modifyFirst :: [Coin] -> (Integer -> Integer) -> [Coin]
    modifyFirst [] _  = error "modifyFirst: empty list of 'Coin'"
    modifyFirst ((Coin c):cs) op
        | c' < threshold = cs
        | otherwise = (Coin (fromIntegral c')):cs
      where
        c' :: Integer
        c' = op (fromIntegral c)

        threshold :: Integer
        threshold = fromIntegral (getCoin (dustThreshold feeOpts))

    getNextBatch :: State [a] [a]
    getNextBatch = do
        xs <- get
        let (batch, rest) = splitAt (fromIntegral batchSize) xs
        put rest
        pure batch
