{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Primitive.CoinSelection.MigrationSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Primitive.CoinSelection
    ( CoinSelection (..), changeBalance, inputBalance )
import Cardano.Wallet.Primitive.CoinSelection.Migration
    ( idealBatchSize, selectCoinsForMigration )
import Cardano.Wallet.Primitive.CoinSelectionSpec
    ()
import Cardano.Wallet.Primitive.Fee
    ( Fee (..), FeeOptions (..) )
import Cardano.Wallet.Primitive.FeeSpec
    ()
import Cardano.Wallet.Primitive.Types
    ( UTxO (getUTxO), balance )
import Test.Hspec
    ( Spec, describe, it, shouldSatisfy )
import Test.QuickCheck
    ( conjoin, counterexample, label, property, withMaxSuccess, (===) )

import qualified Data.Map as Map
import qualified Data.Set as Set

spec :: Spec
spec = do

    describe "idealBatchSize" $ do
        it "Eventually converge for decreasing functions" $ do
            property $ \coinselOpts -> do
                let batchSize = idealBatchSize coinselOpts
                label (show batchSize) True

    describe "selectCoinsForMigration" $ do

        it "No coin selection has ouputs" $
            property $ withMaxSuccess 1000 $ \feeOpts batchSize utxo -> do
                let allOutputs = outputs =<<
                        selectCoinsForMigration feeOpts batchSize utxo
                allOutputs `shouldSatisfy` null

        it "Every coin in the selection change >= minimum threshold coin" $
            property $ withMaxSuccess 1000 $ \feeOpts batchSize utxo -> do
                let allChange = change
                        =<< selectCoinsForMigration feeOpts batchSize utxo
                let undersizedCoins =
                        filter (< (dustThreshold feeOpts)) allChange
                undersizedCoins `shouldSatisfy` null

        it "Total input UTxO value >= sum of selection change coins" $
            property $ withMaxSuccess 1000 $ \feeOpts batchSize utxo -> do
                let sumCoinSelectionChange = changeBalance <$>
                        selectCoinsForMigration feeOpts batchSize utxo
                balance utxo >= fromIntegral (sum sumCoinSelectionChange)

        it "Every selection input is unique" $
            property $ withMaxSuccess 1000 $ \feeOpts batchSize utxo -> do
                let selectionInputList = inputs =<<
                        selectCoinsForMigration feeOpts batchSize utxo
                let selectionInputSet =
                        Set.fromList selectionInputList
                Set.size selectionInputSet === length selectionInputSet

        it "Every selection input is a member of the UTxO" $
            property $ withMaxSuccess 1000 $ \feeOpts batchSize utxo -> do
                let selectionInputSet =
                        Set.fromList $ inputs =<<
                            selectCoinsForMigration feeOpts batchSize utxo
                let utxoSet =
                        Set.fromList $ Map.toList $ getUTxO utxo
                selectionInputSet `Set.isSubsetOf` utxoSet

        it "Every coin selection is well-balanced" $
            property $ withMaxSuccess 1000 $ \feeOpts batchSize utxo -> do
                let selections = selectCoinsForMigration feeOpts batchSize utxo
                conjoin
                    [ counterexample example (actualFee === expectedFee)
                    | s <- selections
                    , let actualFee = inputBalance s - changeBalance s
                    , let (Fee expectedFee) = estimate feeOpts s
                    , let example = unlines
                            [ "Coin Selection: " <> show s
                            , "Actual fee: " <> show actualFee
                            , "Expected fee: " <> show expectedFee
                            ]
                    ]
