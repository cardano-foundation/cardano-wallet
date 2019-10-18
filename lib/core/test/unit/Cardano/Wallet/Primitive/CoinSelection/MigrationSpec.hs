{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Primitive.CoinSelection.MigrationSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Primitive.CoinSelection
    ( CoinSelection (..), outputBalance )
import Cardano.Wallet.Primitive.CoinSelection.Migration
    ( selectCoinsForMigration )
import Cardano.Wallet.Primitive.CoinSelectionSpec
    ()
import Cardano.Wallet.Primitive.Fee
    ( FeeOptions (..) )
import Cardano.Wallet.Primitive.FeeSpec
    ()
import Cardano.Wallet.Primitive.Types
    ( TxOut (coin), UTxO (getUTxO), balance )
import Test.Hspec
    ( Spec, describe, it, shouldSatisfy )
import Test.QuickCheck
    ( property, (===) )

import qualified Data.Map as Map
import qualified Data.Set as Set

spec :: Spec
spec = do

    describe "Coin selection for migration" $ do

        it "No coin selection has ouputs" $
            property $ \feeOpts batchSize utxo -> do
                let allOutputs = outputs =<<
                        selectCoinsForMigration feeOpts batchSize utxo
                allOutputs `shouldSatisfy` null

        it "Every coin in the selection output >= minimum threshold coin" $
            property $ \feeOpts batchSize utxo -> do
                let allCoins = fmap coin . outputs
                        =<< selectCoinsForMigration feeOpts batchSize utxo
                let undersizedCoins =
                        filter (< (dustThreshold feeOpts)) allCoins
                undersizedCoins `shouldSatisfy` null

        it "Total input UTxO value >= sum of selection output coins" $
            property $ \feeOpts batchSize utxo -> do
                let sumCoinSelectionOutput = outputBalance <$>
                        selectCoinsForMigration feeOpts batchSize utxo
                balance utxo >= fromIntegral (sum sumCoinSelectionOutput)

        it "Every selection input is unique" $
            property $ \feeOpts batchSize utxo -> do
                let selectionInputList = inputs =<<
                        selectCoinsForMigration feeOpts batchSize utxo
                let selectionInputSet =
                        Set.fromList selectionInputList
                Set.size selectionInputSet === length selectionInputSet

        it "Every selection input is a member of the UTxO" $
            property $ \feeOpts batchSize utxo -> do
                let selectionInputSet =
                        Set.fromList $ inputs =<<
                            selectCoinsForMigration feeOpts batchSize utxo
                let utxoSet =
                        Set.fromList $ Map.toList $ getUTxO utxo
                selectionInputSet `Set.isSubsetOf` utxoSet
