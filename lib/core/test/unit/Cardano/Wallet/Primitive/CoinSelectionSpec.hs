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
    , ErrValidation(..)
    , coinSelectionUnitTest
    , noValidation
    , alwaysFail
    ) where

-- | This module contains shared logic between the coin selection tests. They
-- ought to share the same interface, and therefore, it makes sense for them to
-- also require the same arbitrary instances and instrument testing in a similar
-- way for both.

import Prelude

import Cardano.Wallet.Primitive.CoinSelection
    ( CoinSelection (..)
    , CoinSelectionOptions (..)
    , ErrCoinSelection (..)
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
    , Confidence (..)
    , Gen
    , NonEmptyList (..)
    , Property
    , checkCoverageWith
    , choose
    , cover
    , elements
    , generate
    , oneof
    , scale
    , vectorOf
    )
import Test.QuickCheck.Monadic
    ( monadicIO )

import qualified Data.ByteString as BS
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Test.QuickCheck.Monadic as QC

spec :: Spec
spec = do
    describe "Coin selection properties : shuffle" $ do
        it "every non-empty list can be shuffled, ultimately"
            (checkCoverageWith lowerConfidence prop_shuffleCanShuffle)
        it "shuffle is non-deterministic"
            (checkCoverageWith lowerConfidence prop_shuffleNotDeterministic)
        it "sort (shuffled xs) == sort xs"
            (checkCoverageWith lowerConfidence prop_shufflePreserveElements)
        it "UTxO toList order deterministic" $
            checkCoverageWith
                lowerConfidence
                prop_utxoToListOrderDeterministic
  where
    lowerConfidence :: Confidence
    lowerConfidence = Confidence (10^(6 :: Integer)) 0.75

{-------------------------------------------------------------------------------
                                 Properties
-------------------------------------------------------------------------------}

prop_shuffleCanShuffle
    :: NonEmptyList Int
    -> Property
prop_shuffleCanShuffle (NonEmpty xs) = monadicIO $ QC.run $ do
    xs' <- shuffle xs
    return $ cover 90 (xs /= xs') "shuffled" ()

prop_shuffleNotDeterministic
    :: NonEmptyList Int
    -> Property
prop_shuffleNotDeterministic (NonEmpty xs) = monadicIO $ QC.run $ do
    xs1 <- shuffle xs
    xs2 <- shuffle xs
    return $ cover 90 (xs1 /= xs2) "not deterministic" ()

prop_shufflePreserveElements
    :: [Int]
    -> Property
prop_shufflePreserveElements xs = monadicIO $ QC.run $ do
    xs' <- shuffle xs
    return $ cover 90 (not $ null xs) "non-empty" (L.sort xs == L.sort xs')

prop_utxoToListOrderDeterministic
    :: UTxO
    -> Property
prop_utxoToListOrderDeterministic u = monadicIO $ QC.run $ do
    let list0 = Map.toList $ getUTxO u
    list1 <- shuffle list0
    return $
        cover 90 (list0 /= list1) "shuffled" $
        list0 == Map.toList (Map.fromList list1)

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
    { maxNumOfInputs :: Word8
        -- ^ Maximum number of inputs that can be selected
    , validateSelection :: CoinSelection -> Either ErrValidation ()
        -- ^ A extra validation function on the resulting selection
    , utxoInputs :: [Word64]
        -- ^ Value (in Lovelace) & number of available coins in the UTxO
    , txOutputs :: NonEmpty Word64
        -- ^ Value (in Lovelace) & number of requested outputs
    }

-- | A dummy error for testing extra validation
data ErrValidation = ErrValidation deriving (Eq, Show)

-- | Smart constructor for the validation function that always succeed
noValidation :: CoinSelection -> Either ErrValidation ()
noValidation = const (Right ())

-- | Smart constructor for the validation function that always fail
alwaysFail :: CoinSelection -> Either ErrValidation ()
alwaysFail = const (Left ErrValidation)

-- | Testing-friendly format for 'CoinSelection' results of unit tests
data CoinSelectionResult = CoinSelectionResult
    { rsInputs :: [Word64]
    , rsChange :: [Word64]
    , rsOutputs :: [Word64]
    } deriving (Eq, Show)

-- | Generate a 'UTxO' and 'TxOut' matching the given 'Fixture', and perform
-- given coin selection on it.
coinSelectionUnitTest
    :: ( CoinSelectionOptions ErrValidation
         -> NonEmpty TxOut
         -> UTxO
         -> ExceptT (ErrCoinSelection ErrValidation) IO (CoinSelection, UTxO)
       )
    -> String
    -> Either (ErrCoinSelection ErrValidation) CoinSelectionResult
    -> CoinSelectionFixture
    -> SpecWith ()
coinSelectionUnitTest run lbl expected (CoinSelectionFixture n fn utxoF outsF) =
    it title $ do
        (utxo,txOuts) <- setup
        result <- runExceptT $ do
            (CoinSelection inps outs chngs, _) <-
                run (CoinSelectionOptions (const n) fn) txOuts utxo
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

instance Arbitrary (CoinSelectionOptions e) where
    arbitrary = do
        -- NOTE Functions have to be decreasing functions
        fn <- elements
            [ (maxBound -)
            , \x ->
                if x > maxBound `div` 2
                    then maxBound
                    else maxBound - (2 * x)
            , const 42
            ]
        pure $ CoinSelectionOptions fn (const (pure ()))

instance Show (CoinSelectionOptions e) where
    show _ = "CoinSelectionOptions"

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
