{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.CoinSelection.LargestFirstSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.CoinSelection
    ( CoinSelection (..), CoinSelectionError (..), CoinSelectionOptions (..) )
import Cardano.Wallet.CoinSelection.LargestFirst
    ( largestFirst )
import Cardano.Wallet.Primitive.Types
    ( Address (..)
    , Coin (..)
    , Hash (..)
    , TxIn (..)
    , TxOut (..)
    , UTxO (..)
    , excluding
    )
import Control.Monad
    ( unless )
import Control.Monad.Trans.Except
    ( runExceptT )
import Data.Either
    ( isRight )
import Data.Functor.Identity
    ( Identity (runIdentity) )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Word
    ( Word64, Word8 )
import Test.Hspec
    ( Expectation, Spec, describe, it, shouldBe, shouldSatisfy )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , Property
    , choose
    , generate
    , oneof
    , property
    , scale
    , vectorOf
    , (===)
    , (==>)
    )

import qualified Data.ByteString as BS
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

spec :: Spec
spec = do
    describe "Coin selection : LargestFirst algorithm unit tests" $ do
        it "one input per small output" $ do
            (coinSelectionUnitTest
                Fixture
                { maxNumOfInputs = 100
                , utxoInputs = [10,10,17]
                , txOutputs = 17 :| []
                , expectedResult = Right [17]
                })
        it "one input per big output" $ do
            (coinSelectionUnitTest
                Fixture
                { maxNumOfInputs = 100
                , utxoInputs = [12,10,17]
                , txOutputs = 1 :| []
                , expectedResult = Right [17]
                })
        it "two inputs per output" $ do
            (coinSelectionUnitTest
                Fixture
                { maxNumOfInputs = 100
                , utxoInputs = [12,10,17]
                , txOutputs = 18 :| []
                , expectedResult = Right [17, 12]
                })
        it "three inputs per output" $ do
            (coinSelectionUnitTest
                Fixture
                { maxNumOfInputs = 100
                , utxoInputs = [12,10,17]
                , txOutputs = 30 :| []
                , expectedResult = Right [17, 12, 10]
                })
        it "NotEnoughMoney error expected when not enough coins" $ do
            (coinSelectionUnitTest
                Fixture
                { maxNumOfInputs = 100
                , utxoInputs = [12,10,17]
                , txOutputs = 40 :| []
                , expectedResult = Left $ NotEnoughMoney 39 40
                })
        it "NotEnoughMoney error expected when not enough coins and utxo not fragmented enough" $ do
            (coinSelectionUnitTest
                Fixture
                { maxNumOfInputs = 100
                , utxoInputs = [12,10,17]
                , txOutputs = 40 :| [1,1,1]
                , expectedResult = Left $ NotEnoughMoney 39 43
                })
        it "UtxoNotEnoughFragmented error expected when enough coins and utxo not fragmented enough" $ do
            (coinSelectionUnitTest
                Fixture
                { maxNumOfInputs = 100
                , utxoInputs = [12,20,17]
                , txOutputs = 40 :| [1,1,1]
                , expectedResult = Left $ UtxoNotEnoughFragmented 3 4
                })
        it "happy path with correct maximumNumberOfInputs - 3 inputs for 2 outputs" $ do
            (coinSelectionUnitTest
                Fixture
                { maxNumOfInputs = 3
                , utxoInputs = [1,2,10,6,5]
                , txOutputs = 11 :| [1]
                , expectedResult = Right [10,6,5]
                })
        it "happy path with too strict maximumNumberOfInputs result in error - 3 inputs for 2 outputs" $ do
            (coinSelectionUnitTest
                Fixture
                { maxNumOfInputs = 2
                , utxoInputs = [1,2,10,6,5]
                , txOutputs = 11 :| [1]
                , expectedResult = Left $ MaximumInputsReached 2
                })

    describe "Coin selection properties : LargestFirst algorithm" $ do
        it "forall (UTxO, NonEmpty TxOut), \
           \ running algorithm twice yields exactly the same result"
            (property propDeterministic)
        it "forall (UTxO, NonEmpty TxOut), \
           \ there's at least as many selected inputs as there are requested outputs"
            (property propAtLeast)
        it "forall (UTxO, NonEmpty TxOut), for all selected input, \
           \ there's no bigger input in the UTxO that is not already in the selected inputs."
            (property propInputDecreasingOrder)

{-------------------------------------------------------------------------------
                 Properties and unit test generic scenario
-------------------------------------------------------------------------------}

data Fixture = Fixture
    { maxNumOfInputs :: Word64
    , utxoInputs :: [Word64]
    , txOutputs :: NonEmpty Word64
    , expectedResult :: Either CoinSelectionError [Word64]
    } deriving Show


coinSelectionUnitTest
    :: Fixture
    -> Expectation
coinSelectionUnitTest (Fixture n utxoCoins txOutsCoins expected) = do
    (utxo,txOuts) <- setup

    result <- runExceptT $ do
        CoinSelection inps _ _ <-
            largestFirst (defaultCoinSelectionOptions n) utxo txOuts
        return $ map (getCoin . coin . snd) inps

    case expected of
        Left err ->
            result `shouldBe` (Left err)
        Right expectedCoinsSel ->
            result `shouldBe` (return expectedCoinsSel)
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


propDeterministic
    :: CoveringCase
    -> Property
propDeterministic (CoveringCase (utxo, txOuts)) = do
    let opts = defaultCoinSelectionOptions 100
    let resultOne = runIdentity $ runExceptT $ largestFirst opts utxo txOuts
    let resultTwo = runIdentity $ runExceptT $ largestFirst opts utxo txOuts
    resultOne === resultTwo

propAtLeast
    :: CoveringCase
    -> Property
propAtLeast (CoveringCase (utxo, txOuts)) =
    isRight selection ==> let Right s = selection in prop s
  where
    prop (CoinSelection inps _ _) =
        L.length inps `shouldSatisfy` (>= NE.length txOuts)
    selection = runIdentity $ runExceptT $
        largestFirst (defaultCoinSelectionOptions 100) utxo txOuts

propInputDecreasingOrder
    :: CoveringCase
    -> Property
propInputDecreasingOrder (CoveringCase (utxo, txOuts)) =
    isRight selection ==> let Right s = selection in prop s
  where
    prop (CoinSelection inps _ _) =
        let
            utxo' = (Map.toList . getUTxO) $
                utxo `excluding` (Set.fromList . map fst $ inps)
        in unless (L.null utxo') $
            (getExtremumValue L.minimum inps)
            `shouldSatisfy`
            (>= (getExtremumValue L.maximum utxo'))
    getExtremumValue f = f . map (getCoin . coin . snd)
    selection = runIdentity $ runExceptT $
        largestFirst (defaultCoinSelectionOptions 100) utxo txOuts


{-------------------------------------------------------------------------------
                                  Test Data
-------------------------------------------------------------------------------}

defaultCoinSelectionOptions
    :: Word64
    -> CoinSelectionOptions
defaultCoinSelectionOptions n = CoinSelectionOptions
    { estimateFee = \_ _ -> Coin 0
    , dustThreshold = Coin 0
    , maximumNumberOfInputs = n
    }

newtype CoveringCase = CoveringCase { getCoveringCase :: (UTxO, NonEmpty TxOut)}
    deriving Show

instance Arbitrary CoveringCase where
    arbitrary = do
        n <- choose (1, 10)
        txOutsNonEmpty <- NE.fromList <$> vectorOf n arbitrary
        utxo <- arbitrary
        return $ CoveringCase (utxo, txOutsNonEmpty)

instance Arbitrary (Hash "Tx") where
    -- No Shrinking
    arbitrary = do
        wds <- vectorOf 10 arbitrary :: Gen [Word8]
        let bs = BS.pack wds
        pure $ Hash bs

instance Arbitrary Address where
    -- No Shrinking
    arbitrary = oneof
        [ pure $ Address "ADDR01"
        , pure $ Address "ADDR02"
        , pure $ Address "ADDR03"
        ]

instance Arbitrary Coin where
    -- No Shrinking
    arbitrary = Coin <$> choose (1, 100000)

instance Arbitrary TxOut where
    -- No Shrinking
    arbitrary = TxOut
        <$> arbitrary
        <*> arbitrary

instance Arbitrary TxIn where
    -- No Shrinking
    arbitrary = TxIn
        <$> arbitrary
        <*> scale (`mod` 3) arbitrary -- No need for a high indexes

instance Arbitrary UTxO where
    shrink (UTxO utxo) = UTxO <$> shrink utxo
    arbitrary = do
        n <- choose (1, 100)
        utxo <- zip
            <$> vectorOf n arbitrary
            <*> vectorOf n arbitrary
        return $ UTxO $ Map.fromList utxo
