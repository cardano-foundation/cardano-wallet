{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: Apache-2.0
--
-- This module contains an algorithm to select coins for migration from legacy
-- wallets to newer wallets.
--
-- We want users to be able to migrate their funds from a legacy random wallet
-- to a new sequential wallet. To do this, we have to move funds from a wallet
-- to another by making transactions. Funds are ultimately a sum of many coins
-- (a.k.a UTxOs). In a transaction, we can select a few coins, and send them to
-- addresses, effectively creating new coins / UTxOs doing this.
--
-- There are some limitations regarding the number of coins that can be selected
-- at once in a single transaction (theoretically 255 coins, in practice ~170)
-- because there's a transaction max size (in bytes) enforced by the network.
-- Also, there's a direct relationship between the maximum number of inputs we
-- can select, and the maximum number of outputs we can produce (increasing one
-- will decrease the other, and vice-versa).
--
-- When making a transaction, coins used as inputs for a transaction becomes
-- unavailable for a while, until the transaction is inserted into the ledger
-- and, make some new coins available as change (very much like when paying
-- with bank notes to a shop, if we give a 20 EUR note to pay for 3 EUR, we
-- can't spend the remaining 17 EUR before we have received the change!).
-- So, a wallet with a small number of UTxO will not be able to make many
-- transactions in parallel and will have to make them sequentially, waiting
-- for the previous ones to be inserted before making new ones (we also say
-- that a wallet is not "fragmented enough").

module Cardano.Wallet.Primitive.CoinSelection.Migration
    ( selectCoinsForMigration
    , idealBatchSize
    ) where

import Prelude

import Cardano.Wallet.Primitive.CoinSelection
    ( CoinSelection (..)
    , CoinSelectionOptions (..)
    , changeBalance
    , inputBalance
    )
import Cardano.Wallet.Primitive.Fee
    ( Fee (..), FeeOptions (..) )
import Cardano.Wallet.Primitive.Types
    ( Coin (..), TxIn (..), TxOut (..), UTxO (..) )
import Control.Monad.Trans.State
    ( State, evalState, get, put )
import Data.List
    ( splitAt )
import Data.List.NonEmpty
    ( NonEmpty ((:|)) )
import Data.Maybe
    ( mapMaybe )
import Data.Word
    ( Word8 )

import qualified Data.Map.Strict as Map

-- | Construct a list of coin selections / transactions to transfer the totality
-- of a user's wallet. The resulting 'CoinSelection' do not contain any
-- 'outputs', but only change coins (so there's no restriction about how
-- addresses are generated).
--
-- It tries to fit as many inputs as possible in a single transaction (fixed by
-- the 'Word8' maximum number of inputs given as argument.
--
-- The fee options are used to balance the coin selections and fix a threshold
-- for dust that is removed from the selections.
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
    -- no outputs (we are paying everything to ourselves!).
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
    adjustForFee !coinSel = case change coinSel of
        -- If there's no change, nothing to adjust
        [] -> Nothing

        -- No difference between required and computed, we're done
        (_ : _) | diff == 0 -> Just coinSel

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
        (c : cs) -> adjustForFee $ coinSel
            { change = modifyFirst (c :| cs) (+ diff) }
      where
        diff :: Integer
        diff = fromIntegral actualFee - fromIntegral requiredFee
          where
            (Fee requiredFee) = estimateFee feeOpts coinSel
            actualFee = inputBalance coinSel - changeBalance coinSel

    -- | Apply the given function to the first coin of the list. If the
    -- operation makes the 'Coin' smaller than the dust threshold, the coin is
    -- discarded.
    modifyFirst :: NonEmpty Coin -> (Integer -> Integer) -> [Coin]
    modifyFirst (Coin c :| cs) op
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

-- | Try to find a fix "ideal" number of input transactions that would generate
-- rather balanced transactions.
idealBatchSize :: CoinSelectionOptions e -> Int
idealBatchSize coinselOpts = fromIntegral (fixPoint 1)
  where
    fixPoint :: Word8 -> Word8
    fixPoint !n
        | maxN n <= n = n
        | n == maxBound = n
        | otherwise = fixPoint (n + 1)
      where
        maxN :: Word8 -> Word8
        maxN = maximumNumberOfInputs coinselOpts
