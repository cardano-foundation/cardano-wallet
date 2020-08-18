{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Primitive.CoinSelection.MigrationSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Primitive.CoinSelection
    ( CoinSelection (..), changeBalance, inputBalance )
import Cardano.Wallet.Primitive.CoinSelection.Migration
    ( depleteUTxO, idealBatchSize )
import Cardano.Wallet.Primitive.CoinSelectionSpec
    ()
import Cardano.Wallet.Primitive.Fee
    ( Fee (..), FeeOptions (..), OnDanglingChange (..) )
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
import Data.Function
    ( (&) )
import Data.Word
    ( Word8 )
import Numeric.Natural
    ( Natural )
import Test.Hspec
    ( Spec, SpecWith, describe, it, shouldSatisfy )
import Test.QuickCheck
    ( Gen
    , Property
    , choose
    , conjoin
    , counterexample
    , frequency
    , label
    , property
    , vector
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

    describe "accuracy of depleteUTxO" $ do
        let testAccuracy :: Double -> SpecWith ()
            testAccuracy r = it title $ withMaxSuccess 1000 $ monadicIO $ do
                let dust = Coin 100
                utxo <- pick (genUTxO r dust)
                batchSize <- pick genBatchSize
                feeOpts <- pick (genFeeOptions dust)
                let selections = depleteUTxO feeOpts batchSize utxo
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

    describe "depleteUTxO properties" $ do
        it "No coin selection has outputs" $
            property $ withMaxSuccess 1000 prop_onlyChangeOutputs

        it "Every coin in the selection change >= minimum threshold coin" $
            property $ withMaxSuccess 1000 prop_noLessThanThreshold

        it "Total input UTxO value >= sum of selection change coins" $
            property $ withMaxSuccess 1000 prop_inputsGreaterThanOutputs

        it "Every selection input is unique" $
            property $ withMaxSuccess 1000 prop_inputsAreUnique

        it "Every selection input is a member of the UTxO" $
            property $ withMaxSuccess 1000 prop_inputsStillInUTxO

        it "Every coin selection is well-balanced" $
            property $ withMaxSuccess 1000 prop_wellBalanced

    describe "depleteUTxO regressions" $ do
        it "regression #1" $ do
            let feeOpts = FeeOptions
                    { dustThreshold = Coin 9
                    , estimateFee = \s -> Fee
                        $ fromIntegral
                        $ 5 * (length (inputs s) + length (outputs s))
                    , onDanglingChange = PayAndBalance
                    }
            let batchSize = 1
            let utxo = UTxO $ Map.fromList
                    [ ( TxIn
                        { inputId = Hash "|\243^\SUBg\242\231\&1\213\203"
                        , inputIx = 2
                        }
                      , TxOut
                        { address = Address "ADDR03"
                        , coin = Coin 2
                        }
                      )
                    ]
            property (prop_inputsGreaterThanOutputs feeOpts batchSize utxo)

{-------------------------------------------------------------------------------
                                  Properties
-------------------------------------------------------------------------------}

-- | No coin selection has outputs
prop_onlyChangeOutputs
    :: FeeOptions
    -> Word8
    -> UTxO
    -> Property
prop_onlyChangeOutputs feeOpts batchSize utxo = do
    let allOutputs = outputs =<<
            depleteUTxO feeOpts batchSize utxo
    property (allOutputs `shouldSatisfy` null)

-- | Every coin in the selection change >= minimum threshold coin
prop_noLessThanThreshold
    :: FeeOptions
    -> Word8
    -> UTxO
    -> Property
prop_noLessThanThreshold feeOpts batchSize utxo = do
    let allChange = change
            =<< depleteUTxO feeOpts batchSize utxo
    let undersizedCoins =
            filter (< (dustThreshold feeOpts)) allChange
    property (undersizedCoins `shouldSatisfy` null)

-- | Total input UTxO value >= sum of selection change coins
prop_inputsGreaterThanOutputs
    :: FeeOptions
    -> Word8
    -> UTxO
    -> Property
prop_inputsGreaterThanOutputs feeOpts batchSize utxo = do
    let selections  = depleteUTxO feeOpts batchSize utxo
    let totalChange = sum (changeBalance <$> selections)
    let balanceUTxO = balance utxo
    property (balanceUTxO >= fromIntegral totalChange)
        & counterexample ("Total change balance: " <> show totalChange)
        & counterexample ("Total UTxO balance: " <> show balanceUTxO)
        & counterexample ("Selections: " <> show selections)

-- | Every selected input is unique, i.e. selected only once
prop_inputsAreUnique
    :: FeeOptions
    -> Word8
    -> UTxO
    -> Property
prop_inputsAreUnique feeOpts batchSize utxo = do
    let selectionInputList = inputs =<<
            depleteUTxO feeOpts batchSize utxo
    let selectionInputSet =
            Set.fromList selectionInputList
    Set.size selectionInputSet === length selectionInputSet

-- | Every selection input is still a member of the UTxO" $
prop_inputsStillInUTxO
    :: FeeOptions
    -> Word8
    -> UTxO
    -> Property
prop_inputsStillInUTxO feeOpts batchSize utxo = do
    let selectionInputSet =
            Set.fromList $ inputs =<<
                depleteUTxO feeOpts batchSize utxo
    let utxoSet =
            Set.fromList $ Map.toList $ getUTxO utxo
    property (selectionInputSet `Set.isSubsetOf` utxoSet)

-- | Every coin selection is well-balanced (i.e. actual fees are exactly the
-- expected fees)
prop_wellBalanced
    :: FeeOptions
    -> Word8
    -> UTxO
    -> Property
prop_wellBalanced feeOpts batchSize utxo = do
    let selections = depleteUTxO feeOpts batchSize utxo
    conjoin
        [ counterexample example (actualFee === expectedFee)
        | s <- selections
        , let actualFee = inputBalance s - changeBalance s
        , let (Fee expectedFee) = estimateFee feeOpts s
        , let example = unlines
                [ "Coin Selection: " <> show s
                , "Actual fee: " <> show actualFee
                , "Expected fee: " <> show expectedFee
                ]
        ]

{-------------------------------------------------------------------------------
                                  Generators
-------------------------------------------------------------------------------}

genBatchSize :: Gen Word8
genBatchSize = choose (50, 150)

genFeeOptions :: Coin -> Gen FeeOptions
genFeeOptions (Coin dust) = do
    pure $ FeeOptions
        { estimateFee = \s ->
            let x = fromIntegral (length (inputs s) + length (outputs s))
            in Fee $ (dust `div` 100) * x + dust
        , dustThreshold = Coin dust
        , onDanglingChange = PayAndBalance
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
        ixs <- vector n
        pure $ zipWith TxIn ids ixs

    genTxOut :: Int -> Gen [TxOut]
    genTxOut n = do
        coins <- vectorOf n genCoin
        addrs <- vectorOf n (Address <$> genBytes 8)
        pure $ zipWith TxOut addrs coins

    genBytes :: Int -> Gen ByteString
    genBytes n = B8.pack <$> vector n

    genCoin :: Gen Coin
    genCoin = Coin <$> frequency
        [ (round (100*r), choose (1, dust))
        , (round (100*(1-r)), choose (dust, 1000*dust))
        ]
