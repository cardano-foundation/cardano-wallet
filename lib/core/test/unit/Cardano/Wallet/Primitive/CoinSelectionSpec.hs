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

import Cardano.Wallet.Api.Server
    ( assignMigrationAddresses )
import Cardano.Wallet.Primitive.CoinSelection
    ( CoinSelection (..), CoinSelectionOptions (..), ErrCoinSelection (..) )
import Cardano.Wallet.Primitive.Types
    ( Address (..)
    , Coin (..)
    , Hash (..)
    , ShowFmt (..)
    , TxIn (..)
    , TxOut (..)
    , UTxO (..)
    , UnsignedTx (..)
    )
import Control.Monad.Trans.Except
    ( ExceptT, runExceptT )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Maybe
    ( catMaybes )
import Data.Vector.Shuffle
    ( shuffle )
import Data.Word
    ( Word64, Word8 )
import Fmt
    ( Buildable (..), blockListF, nameF )
import Test.Hspec
    ( Spec, SpecWith, describe, it, shouldBe )
import Test.Hspec.QuickCheck
    ( prop )
import Test.QuickCheck
    ( Arbitrary (..)
    , Confidence (..)
    , Gen
    , Property
    , applyArbitrary2
    , checkCoverageWith
    , choose
    , cover
    , elements
    , generate
    , scale
    , vector
    , (===)
    )
import Test.QuickCheck.Monadic
    ( monadicIO )

import qualified Data.ByteString as BS
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Test.QuickCheck.Monadic as QC

spec :: Spec
spec = do
    describe "Coin selection properties" $ do
        it "UTxO toList order deterministic" $
            checkCoverageWith
                lowerConfidence
                prop_utxoToListOrderDeterministic

    describe "assignMigrationAddresses properties" $ do
        prop "Selection count is preserved" prop_selectionCountPreserved
        prop "Coin values are preserved" prop_coinValuesPreserved
        prop "All inputs are used" prop_allInputsAreUsed

  where
    lowerConfidence :: Confidence
    lowerConfidence = Confidence (10^(6 :: Integer)) 0.75

{-------------------------------------------------------------------------------
                                 Properties
-------------------------------------------------------------------------------}

prop_utxoToListOrderDeterministic
    :: UTxO
    -> Property
prop_utxoToListOrderDeterministic u = monadicIO $ QC.run $ do
    let list0 = Map.toList $ getUTxO u
    list1 <- shuffle list0
    return $
        cover 90 (list0 /= list1) "shuffled" $
        list0 == Map.toList (Map.fromList list1)

prop_selectionCountPreserved
    :: CoinSelectionsSetup
    -> Property
prop_selectionCountPreserved (CoinSelectionsSetup cs addrs) = do
    let sels = getCS <$> cs
    length (assignMigrationAddresses addrs sels) === length sels

prop_coinValuesPreserved
    :: CoinSelectionsSetup
    -> Property
prop_coinValuesPreserved (CoinSelectionsSetup cs addrs) = do
    let sels = getCS <$> cs
    let getCoinValueFromInp =
            sum . map (\(_, TxOut _ (Coin c)) -> c)
    let selsCoinValue =
            sum $
            (\(CoinSelection inps _ _) -> getCoinValueFromInp inps) . getCS
            <$> cs
    let getCoinValueFromTxOut (UnsignedTx _ txouts) =
            sum $ map (\(TxOut _ (Coin c)) -> c) $ NE.toList txouts
    let txsCoinValue =
            sum . map getCoinValueFromTxOut
    txsCoinValue (assignMigrationAddresses addrs sels) === selsCoinValue

prop_allInputsAreUsed
    :: CoinSelectionsSetup
    -> Property
prop_allInputsAreUsed (CoinSelectionsSetup cs addrs) = do
    let sels = getCS <$> cs
    let csInps =
            Set.fromList $ concatMap (\(CoinSelection inp _ _) -> inp) sels
    let getInpsFromTx (UnsignedTx inp _) = NE.toList inp
    let txsCoinValue = Set.fromList . concatMap getInpsFromTx
    txsCoinValue (assignMigrationAddresses addrs sels) === csInps

{-------------------------------------------------------------------------------
                         Coin Selection - Unit Tests
-------------------------------------------------------------------------------}

newtype CoinSelectionForMigration = CoinSelectionForMigration
    { getCS :: CoinSelection } deriving Show

data CoinSelectionsSetup = CoinSelectionsSetup
    { coinSelections :: [CoinSelectionForMigration]
    , addresses :: [Address]
    } deriving Show

-- | Data for running
data CoinSelProp = CoinSelProp
    { csUtxO :: UTxO
        -- ^ Available UTxO for the selection
    , csOuts :: NonEmpty TxOut
        -- ^ Requested outputs for the payment
    } deriving Show

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
        NE.fromList <$> vector n

instance Arbitrary CoinSelProp where
    shrink (CoinSelProp utxo outs) = uncurry CoinSelProp
        <$> zip (shrink utxo) (shrink outs)
    arbitrary = applyArbitrary2 CoinSelProp

instance Arbitrary CoinSelectionForMigration where
    arbitrary = do
        txIntxOuts <- Map.toList . getUTxO <$> arbitrary
        let chgs = map (\(_, TxOut _ c) -> c) txIntxOuts
        pure $ CoinSelectionForMigration
            $ CoinSelection txIntxOuts [] chgs

instance Arbitrary CoinSelectionsSetup where
    arbitrary = do
        csNum <- choose (1,10)
        addrNum <- choose (1,10)
        addrs <- L.nub <$> vector addrNum
        cs <- vector csNum
        pure $ CoinSelectionsSetup cs addrs

instance Arbitrary Address where
    -- No Shrinking
    arbitrary = elements
        [ Address "ADDR01"
        , Address "ADDR02"
        , Address "ADDR03"
        , Address "ADDR04"
        , Address "ADDR05"
        , Address "ADDR06"
        , Address "ADDR07"
        , Address "ADDR08"
        , Address "ADDR09"
        , Address "ADDR10"
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
        wds <- vector 10 :: Gen [Word8]
        let bs = BS.pack wds
        pure $ Hash bs

instance Arbitrary TxOut where
    -- No Shrinking
    arbitrary = applyArbitrary2 TxOut

instance Arbitrary UTxO where
    shrink (UTxO utxo) = UTxO <$> shrink utxo
    arbitrary = do
        n <- choose (1, 100)
        utxo <- zip
            <$> vector n
            <*> vector n
        return $ UTxO $ Map.fromList utxo

genUTxO :: [Word64] -> Gen UTxO
genUTxO coins = do
    let n = length coins
    inps <- vector n
    outs <- genTxOut coins
    return $ UTxO $ Map.fromList $ zip inps outs

genTxOut :: [Word64] -> Gen [TxOut]
genTxOut coins = do
    let n = length coins
    outs <- vector n
    return $ zipWith TxOut outs (map Coin coins)
