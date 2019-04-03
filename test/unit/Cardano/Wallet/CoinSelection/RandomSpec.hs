{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.CoinSelection.RandomSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.CoinSelection
    ( CoinSelection (..), CoinSelectionError (..), CoinSelectionOptions (..) )
import Cardano.Wallet.CoinSelection.Random
    ( random )
import Cardano.Wallet.Primitive.Types
    ( Address (..), Coin (..), Hash (..), TxIn (..), TxOut (..), UTxO (..) )
import Control.Monad.Trans.Except
    ( runExceptT )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Word
    ( Word64, Word8 )
import Test.Hspec
    ( Expectation, Spec, describe, it, shouldBe )
import Test.QuickCheck
    ( Arbitrary (..), Gen, generate, oneof, scale, vectorOf )


import qualified Data.ByteString as BS
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map

spec :: Spec
spec = do
    describe "Coin selection : Random algorithm unit tests" $ do
        it "one output (targetMin=2), 6 small inputs should select 4 coins" $ do
            (coinSelectionUnitTest
                Fixture
                { maxNumOfInputs = 100
                , utxoInputs = [1,1,1,1,1,1]
                , txOutputs = 2 :| []
                , expectedResult = Right [1,1,1,1]
                })
        it "two outputs (targetMin=2, targetMin=1), 6 small inputs should select 6 coins" $ do
            (coinSelectionUnitTest
                Fixture
                { maxNumOfInputs = 100
                , utxoInputs = [1,1,1,1,1,1]
                , txOutputs = 2 :| [1]
                , expectedResult = Right [1,1,1,1,1,1]
                })
        it "two outputs (targetMin=2, targetMin=1), 5 small inputs should select 5 coins" $ do
            (coinSelectionUnitTest
                Fixture
                { maxNumOfInputs = 100
                , utxoInputs = [1,1,1,1,1]
                , txOutputs = 2 :| [1]
                , expectedResult = Right [1,1,1,1,1]
                })
        it "two outputs (targetMin=2, targetMin=1), 4 small inputs should select 3 coins - fallback" $ do
            (coinSelectionUnitTest
                Fixture
                { maxNumOfInputs = 100
                , utxoInputs = [1,1,1,1]
                , txOutputs = 2 :| [1]
                , expectedResult = Right [1,1,1]
                })
        it "one output (targetMin=2), 3 medium inputs should select 1 coin" $ do
            (coinSelectionUnitTest
                Fixture
                { maxNumOfInputs = 100
                , utxoInputs = [5,5,5]
                , txOutputs = 2 :| []
                , expectedResult = Right [5]
                })
        it "two outputs (targetMin=2, targetMin=2), 3 big inputs should select 2 coins" $ do
            (coinSelectionUnitTest
                Fixture
                { maxNumOfInputs = 100
                , utxoInputs = [10,10,10]
                , txOutputs = 2 :| [2]
                , expectedResult = Right [10,10]
                })
        it "one output (targetMin=3), 6 small inputs should select 4 coins - maxNumOfInputs can cover targetMin but not targetAim" $ do
            (coinSelectionUnitTest
                Fixture
                { maxNumOfInputs = 4
                , utxoInputs = [1,1,1,1,1,1]
                , txOutputs = 3 :| []
                , expectedResult = Right [1,1,1,1]
                })
        it "one output (targetMin=3), 6 small inputs result in  - maxNumOfInputs cannot cover targetMin - fallback" $ do
            (coinSelectionUnitTest
                Fixture
                { maxNumOfInputs = 2
                , utxoInputs = [1,1,1,1,1,1]
                , txOutputs = 3 :| []
                , expectedResult = Left $ MaximumInputsReached 2
                })
        it "NotEnoughMoney error expected when not enough coins - fallback" $ do
            (coinSelectionUnitTest
                Fixture
                { maxNumOfInputs = 100
                , utxoInputs = [12,10,17]
                , txOutputs = 40 :| []
                , expectedResult = Left $ NotEnoughMoney 39 40
                })
        it "NotEnoughMoney error expected when not enough coins and utxo not fragmented enough - fallback" $ do
            (coinSelectionUnitTest
                Fixture
                { maxNumOfInputs = 100
                , utxoInputs = [12,10,17]
                , txOutputs = 40 :| [1,1,1]
                , expectedResult = Left $ NotEnoughMoney 39 43
                })
        it "UtxoNotEnoughFragmented error expected when enough coins and utxo not fragmented enough - fallback" $ do
            (coinSelectionUnitTest
                Fixture
                { maxNumOfInputs = 100
                , utxoInputs = [12,20,17]
                , txOutputs = 40 :| [1,1,1]
                , expectedResult = Left $ UtxoNotEnoughFragmented 3 4
                })


data Fixture = Fixture
    { maxNumOfInputs :: Word64
    , utxoInputs :: [Word64]
    , txOutputs :: NonEmpty Word64
    , expectedResult :: Either CoinSelectionError [Word64]
    } deriving Show

defaultCoinSelectionOptions
    :: Word64
    -> CoinSelectionOptions
defaultCoinSelectionOptions n = CoinSelectionOptions
    { estimateFee = \_ _ -> Coin 0
    , dustThreshold = Coin 0
    , maximumNumberOfInputs = n
    }

coinSelectionUnitTest
    :: Fixture
    -> Expectation
coinSelectionUnitTest (Fixture n utxoCoins txOutsCoins expected) = do
    (utxo,txOuts) <- setup

    result <- runExceptT $ do
        CoinSelection inps _ _ <-
            random (defaultCoinSelectionOptions n) utxo txOuts
        return $ map (getCoin . coin . snd) inps

    result `shouldBe` expected
    where
        setup :: IO (UTxO, NonEmpty TxOut)
        setup = do
            ins <- generate $ vectorOf (L.length utxoCoins) arbitrary
            addrs <- generate $ vectorOf (L.length utxoCoins) arbitrary
            let utxo = UTxO $ Map.fromList
                    $ L.zip ins
                    $ L.zipWith TxOut addrs
                    $ map Coin utxoCoins
            txOutsAddrs <- generate $ vectorOf (L.length txOutsCoins) arbitrary
            let txOuts = NE.zipWith TxOut (NE.fromList txOutsAddrs)
                    $ NE.map Coin txOutsCoins
            pure (utxo, txOuts)


instance Arbitrary Address where
    -- No Shrinking
    arbitrary = oneof
        [ pure $ Address "ADDR01"
        , pure $ Address "ADDR02"
        , pure $ Address "ADDR03"
        ]

instance Arbitrary TxIn where
    -- No Shrinking
    arbitrary = TxIn
        <$> arbitrary
        <*> scale (`mod` 3) arbitrary -- No need for a high indexes

instance Arbitrary (Hash "Tx") where
    -- No Shrinking
    arbitrary = do
        wds <- vectorOf 10 arbitrary :: Gen [Word8]
        let bs = BS.pack wds
        pure $ Hash bs
