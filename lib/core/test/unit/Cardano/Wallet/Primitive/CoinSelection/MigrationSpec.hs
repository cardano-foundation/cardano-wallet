{-# LANGUAGE TypeApplications #-}

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
    ( Address (..)
    , Coin (..)
    , Hash (..)
    , TxIn (..)
    , TxOut (..)
    , UTxO (..)
    , balance
    )
import Data.ByteString
    ( ByteString )
import Data.Word
    ( Word8 )
import Numeric.Natural
    ( Natural )
import Test.Hspec
    ( Spec, SpecWith, describe, it, shouldSatisfy )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , choose
    , conjoin
    , counterexample
    , frequency
    , label
    , property
    , vectorOf
    , withMaxSuccess
    , (===)
    )
import Test.QuickCheck.Monadic
    ( monadicIO, monitor, pick )

import qualified Data.ByteString.Char8 as B8
import qualified Data.Map as Map
import qualified Data.Set as Set

spec :: Spec
spec = do
    describe "idealBatchSize" $ do
        it "Eventually converge for decreasing functions" $ do
            property $ \coinselOpts -> do
                let batchSize = idealBatchSize coinselOpts
                label (show batchSize) True

    describe "accuracy of selectCoinsForMigration" $ do
        let testAccuracy :: Double -> SpecWith ()
            testAccuracy r = it title $ withMaxSuccess 1000 $ monadicIO $ do
                let dust = Coin 100
                utxo <- pick (genUTxO r dust)
                batchSize <- pick genBatchSize
                feeOpts <- pick (genFeeOptions dust)
                let selections = selectCoinsForMigration feeOpts batchSize utxo
                monitor $ label $ accuracy dust
                    (balance utxo)
                    (fromIntegral $ sum $ inputBalance <$> selections)
              where
                title :: String
                title = "dust=" <> show (round (100 * r) :: Int) <> "%"

                accuracy :: Coin -> Natural -> Natural -> String
                accuracy (Coin dust) sup real
                    | a >= 1.0 =
                        "PERFECT  (== 100%)"
                    | a > 0.99 || (sup - real) < fromIntegral dust =
                        "OKAY     (>   99%)"
                    | otherwise =
                        "MEDIOCRE (<=  99%)"
                  where
                    a = double real / double sup
                    double = fromRational @Double . fromIntegral

        mapM_ testAccuracy [ 0.01 , 0.05 , 0.10 , 0.25 , 0.50 ]

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


{-------------------------------------------------------------------------------

-------------------------------------------------------------------------------}

genBatchSize :: Gen Word8
genBatchSize = choose (50, 150)

genFeeOptions :: Coin -> Gen FeeOptions
genFeeOptions (Coin dust) = do
    pure $ FeeOptions
        { estimate = \s ->
            let x = fromIntegral (length (inputs s) + length (outputs s))
            in Fee $ (dust `div` 100) * x + dust
        , dustThreshold = Coin dust
        }

-- | Generate a given UTxO with a particular percentage of dust
genUTxO :: Double -> Coin -> Gen UTxO
genUTxO r (Coin dust) = do
    n <- choose (10, 1000)
    inps <- genTxIn n
    outs <- genTxOut n
    pure $ UTxO $ Map.fromList $ zip inps outs
  where
    genTxIn :: Int -> Gen [TxIn]
    genTxIn n = do
        ids <- vectorOf n (Hash <$> genBytes 8)
        ixs <- vectorOf n arbitrary
        pure $ zipWith TxIn ids ixs

    genTxOut :: Int -> Gen [TxOut]
    genTxOut n = do
        coins <- vectorOf n genCoin
        addrs <- vectorOf n (Address <$> genBytes 8)
        pure $ zipWith TxOut addrs coins

    genBytes :: Int -> Gen ByteString
    genBytes n = B8.pack <$> vectorOf n arbitrary

    genCoin :: Gen Coin
    genCoin = Coin <$> frequency
        [ (round (100*r), choose (1, dust))
        , (round (100*(1-r)), choose (dust, 1000*dust))
        ]
