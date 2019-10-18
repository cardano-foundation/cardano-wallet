{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Primitive.CoinSelection.MigrationSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Primitive.CoinSelection
    ( CoinSelection (change, inputs, outputs) )
import Cardano.Wallet.Primitive.CoinSelection.Migration
    ( selectCoinsForMigration )
import Cardano.Wallet.Primitive.CoinSelectionSpec
    ()
import Cardano.Wallet.Primitive.Fee
    ( FeeOptions (..) )
import Cardano.Wallet.Primitive.FeeSpec
    ()
import Cardano.Wallet.Primitive.Types
    ( Coin (..), TxOut (coin), UTxO (getUTxO) )
import Test.Hspec
    ( Spec, describe, it, shouldSatisfy )
import Test.QuickCheck
    ( property, (===) )

import qualified Data.Map as Map
import qualified Data.Set as Set

spec :: Spec
spec = do

    describe "Coin selection for migration" $ do

        it "No coin selection has change" $
            property $ \feeOpts batchSize utxo -> do
                let allChange = change =<<
                        selectCoinsForMigration feeOpts batchSize utxo
                allChange `shouldSatisfy` null

        it "Every coin in the selection output >= minimum threshold coin" $
            property $ \feeOpts batchSize utxo -> do
                let allCoins = fmap coin . outputs
                        =<< selectCoinsForMigration feeOpts batchSize utxo
                let undersizedCoins =
                        filter (< (dustThreshold feeOpts)) allCoins
                undersizedCoins `shouldSatisfy` null

        it "Total input UTxO value >= sum of selection output coins" $
            property $ \feeOpts batchSize utxo -> do
                let sumCoinSelectionOutput = mconcat $
                        sumCoinSelectionOutputs <$>
                            selectCoinsForMigration feeOpts batchSize utxo
                sumUTxO utxo >= sumCoinSelectionOutput

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

{-------------------------------------------------------------------------------
                                   Helpers
-------------------------------------------------------------------------------}

-- Calculate the total value of all outputs in the given coin selection.
sumCoinSelectionOutputs :: CoinSelection -> Coin
sumCoinSelectionOutputs = mconcat . fmap coin . outputs

-- Calculate the total value of all unspent outputs in the given UTxO.
sumUTxO :: UTxO -> Coin
sumUTxO = mconcat . fmap coin . Map.elems . getUTxO

{-------------------------------------------------------------------------------
                                  Instances
-------------------------------------------------------------------------------}

instance Semigroup Coin where
    Coin a <> Coin b = Coin $ a + b

instance Monoid Coin where
    mempty = minBound

