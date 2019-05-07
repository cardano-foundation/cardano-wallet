{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Primitive.CoinSelectionSpec
    ( spec

    -- * Export used to test various coin selection implementations
    , CoinSelectionFixture(..)
    , CoinSelectionResult(..)
    , CoinSelProp(..)
    , coinSelectionUnitTest
    ) where

-- | This module contains shared logic between the coin selection tests. They
-- ought to share the same interface, and therefore, it makes sense for them to
-- also require the same arbitrary instances and instrument testing in a similar
-- way for both.

import Prelude

import Cardano.Wallet.Primitive.CoinSelection
    ( CoinSelection (..)
    , CoinSelectionError (..)
    , CoinSelectionOptions (..)
    , shuffle
    )
import Cardano.Wallet.Primitive.Types
    ( Address (..)
    , Coin (..)
    , Hash (..)
    , ShowFmt (..)
    , TxIn (..)
    , TxOut (..)
    , UTxO (..)
    )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Monad.Trans.Except
    ( ExceptT, runExceptT )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Maybe
    ( catMaybes )
import Data.Word
    ( Word64, Word8 )
import Fmt
    ( Buildable (..), blockListF, nameF )
import Test.Hspec
    ( Spec, SpecWith, describe, it, shouldBe )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , Property
    , checkCoverage
    , choose
    , cover
    , generate
    , oneof
    , scale
    , vectorOf
    , (==>)
    )
import Test.QuickCheck.Monadic
    ( monadicIO )

import qualified Data.ByteString as BS
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map

spec :: Spec
spec = do
    describe "shuffle" $ do
        it "every non-empty list can be shuffled, ultimately"
            (checkCoverage prop_shuffleCanShuffle)
        it "shuffle is non-deterministic"
            (checkCoverage prop_shuffleNotDeterministic)
        it "sort (shuffled xs) == sort xs"
            (checkCoverage prop_shufflePreserveElements)

{-------------------------------------------------------------------------------
                                 Properties
-------------------------------------------------------------------------------}

prop_shuffleCanShuffle
    :: [Int]
    -> Property
prop_shuffleCanShuffle xs =
    length xs > 1 ==> monadicIO $ liftIO $ do
        xs' <- shuffle xs
        return $ cover 90 (xs /= xs') "shuffled" ()

prop_shuffleNotDeterministic
    :: [Int]
    -> Property
prop_shuffleNotDeterministic xs =
    length xs > 1 ==> monadicIO $ liftIO $ do
        xs1 <- shuffle xs
        xs2 <- shuffle xs
        return $ cover 90 (xs1 /= xs2) "not deterministic" ()

prop_shufflePreserveElements
    :: [Int]
    -> Property
prop_shufflePreserveElements xs = monadicIO $ liftIO $ do
    xs' <- shuffle xs
    return $ cover 90 (not $ null xs) "non-empty" (L.sort xs == L.sort xs')

{-------------------------------------------------------------------------------
                         Coin Selection - Unit Tests
-------------------------------------------------------------------------------}

-- | Data for running
data CoinSelProp = CoinSelProp
    { csUtxO :: UTxO
        -- ^ Available UTxO for the selection
    , csOuts :: NonEmpty TxOut
        -- ^ Requested outputs for the payment
    } deriving  Show

instance Buildable CoinSelProp where
    build (CoinSelProp utxo outs) = mempty
        <> build utxo
        <> nameF "outs" (blockListF outs)

-- | A fixture for testing the coin selection
data CoinSelectionFixture = CoinSelectionFixture
    { maxNumOfInputs :: Word64
        -- ^ Maximum number of inputs that can be selected
    , utxoInputs :: [Word64]
        -- ^ Value (in Lovelace) & number of available coins in the UTxO
    , txOutputs :: NonEmpty Word64
        -- ^ Value (in Lovelace) & number of requested outputs
    } deriving Show

-- | Testing-friendly format for 'CoinSelection' results of unit tests
data CoinSelectionResult = CoinSelectionResult
    { rsInputs :: [Word64]
    , rsChange :: [Word64]
    , rsOutputs :: [Word64]
    } deriving (Eq, Show)

-- | Generate a 'UTxO' and 'TxOut' matching the given 'Fixture', and perform
-- given coin selection on it.
coinSelectionUnitTest
    :: ( CoinSelectionOptions
         -> NonEmpty TxOut
         -> UTxO
         -> ExceptT CoinSelectionError IO (CoinSelection, UTxO)
       )
    -> String
    -> Either CoinSelectionError CoinSelectionResult
    -> CoinSelectionFixture
    -> SpecWith ()
coinSelectionUnitTest run lbl expected (CoinSelectionFixture n utxoF outsF) =
    it title $ do
        (utxo,txOuts) <- setup
        result <- runExceptT $ do
            (CoinSelection inps outs chngs, _) <-
                run (CoinSelectionOptions n) txOuts utxo
            return $ CoinSelectionResult
                { rsInputs = map (getCoin . coin . snd) inps
                , rsChange = map getCoin chngs
                , rsOutputs = map (getCoin . coin) outs
                }
        result `shouldBe` expected
  where
    title :: String
    title = mempty
        <> "max=" <> show n
        <> ", UTxO=" <> show utxoF
        <> ", Output=" <> show (NE.toList outsF)
        <> " --> " <> show (rsInputs <$> expected)
        <> if null lbl then "" else " (" <> lbl <> ")"

    setup :: IO (UTxO, NonEmpty TxOut)
    setup = do
        utxo <- generate (genUTxO utxoF)
        outs <- generate (genTxOut $ NE.toList outsF)
        pure (utxo, NE.fromList outs)

{-------------------------------------------------------------------------------
                            Arbitrary Instances
-------------------------------------------------------------------------------}

deriving instance Arbitrary a => Arbitrary (ShowFmt a)

instance Arbitrary a => Arbitrary (NonEmpty a) where
    shrink xs = catMaybes (NE.nonEmpty <$> shrink (NE.toList xs))
    arbitrary = do
        n <- choose (1, 10)
        NE.fromList <$> vectorOf n arbitrary

instance Arbitrary CoinSelProp where
    shrink (CoinSelProp utxo outs) = uncurry CoinSelProp
        <$> zip (shrink utxo) (shrink outs)
    arbitrary = CoinSelProp
        <$> arbitrary
        <*> arbitrary

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

instance Arbitrary TxOut where
    -- No Shrinking
    arbitrary = TxOut
        <$> arbitrary
        <*> arbitrary

instance Arbitrary UTxO where
    shrink (UTxO utxo) = UTxO <$> shrink utxo
    arbitrary = do
        n <- choose (1, 100)
        utxo <- zip
            <$> vectorOf n arbitrary
            <*> vectorOf n arbitrary
        return $ UTxO $ Map.fromList utxo

genUTxO :: [Word64] -> Gen UTxO
genUTxO coins = do
    let n = length coins
    inps <- vectorOf n arbitrary
    outs <- genTxOut coins
    return $ UTxO $ Map.fromList $ zip inps outs

genTxOut :: [Word64] -> Gen [TxOut]
genTxOut coins = do
    let n = length coins
    outs <- vectorOf n arbitrary
    return $ zipWith TxOut outs (map Coin coins)
