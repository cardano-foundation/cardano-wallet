{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
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
    ( ShowFmt (..) )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..) )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Cardano.Wallet.Primitive.Types.Tx
    ( TxIn (..), TxOut (..), UnsignedTx (..) )
import Cardano.Wallet.Primitive.Types.UTxO
    ( UTxO (..) )
import Control.Monad.Trans.Except
    ( ExceptT, runExceptT )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Maybe
    ( catMaybes )
import Data.Quantity
    ( Quantity (..) )
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
    , counterexample
    , cover
    , elements
    , frequency
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

{- HLINT ignore "Use <$>" -}

spec :: Spec
spec = do
    describe "Coin selection properties" $ do
        it "UTxO toList order deterministic" $
            checkCoverageWith
                lowerConfidence
                prop_utxoToListOrderDeterministic

    describe "assignMigrationAddresses properties" $ do
        prop "Selection count is preserved" prop_selectionCountPreserved
        prop "Overall coin values are preserved" prop_coinValuesPreserved
        prop "Coin values (sum) are preserved per transaction"
            (prop_coinValuesPreservedPerTx sum)
        prop "Coin values (length) are preserved per transaction"
            (prop_coinValuesPreservedPerTx length)
        prop "All inputs are used" prop_allInputsAreUsed
        prop "All inputs are used per transaction" prop_allInputsAreUsedPerTx
        prop "Addresses are recycled fairly" prop_fairAddressesRecycled

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

-- The number of created transactions should be the same as
-- the number of selections.
prop_selectionCountPreserved
    :: CoinSelectionsSetup
    -> Property
prop_selectionCountPreserved (CoinSelectionsSetup cs addrs) = do
    let sels = getCS <$> cs
    length (assignMigrationAddresses addrs sels) === length sels

-- For all transactions created from selections, the coin values
-- in transactions should be identical to the sum of change of coin values
-- of selections.
prop_coinValuesPreserved
    :: CoinSelectionsSetup
    -> Property
prop_coinValuesPreserved (CoinSelectionsSetup cs addrs) = do
    let sels = getCS <$> cs
    let getCoinValueFromInp =
            sum . map (\(_, TxOut _ (Coin c)) -> c)
    let selsCoinValue =
            sum $ getCoinValueFromInp . inputs . getCS <$> cs
    let getCoinValueFromTxOut (UnsignedTx _ txouts _) =
            sum $ map (\(TxOut _ (Coin c)) -> c) txouts
    let txsCoinValue =
            sum . map getCoinValueFromTxOut
    txsCoinValue (assignMigrationAddresses addrs sels) === selsCoinValue

-- For each transaction t created from a selection s, the coin values within
-- t should be identical to the change coin values within s.
-- (The counts and values of coins should both be identical.)
prop_coinValuesPreservedPerTx
    :: (Show a, Eq a)
    => ([Word64] -> a)
    -> CoinSelectionsSetup
    -> Property
prop_coinValuesPreservedPerTx f (CoinSelectionsSetup cs addrs) = do
    let sels = getCS <$> cs
    let getCoinValueFromInp =
            f . map (\(_, TxOut _ (Coin c)) -> c)
    let selsCoinValue = getCoinValueFromInp . inputs . getCS <$> cs
    let getCoinValueFromTxOut (UnsignedTx _ txouts _) =
            f $ map (\(TxOut _ (Coin c)) -> c) txouts
    let txsCoinValue = map getCoinValueFromTxOut
    txsCoinValue (assignMigrationAddresses addrs sels) === selsCoinValue

-- For all transactions created from selections, the inputs within
-- transactions should be identical to the inputs within selections.
prop_allInputsAreUsed
    :: CoinSelectionsSetup
    -> Property
prop_allInputsAreUsed (CoinSelectionsSetup cs addrs) = do
    let sels = getCS <$> cs
    let csInps = Set.fromList $ concatMap inputs sels
    let getInpsFromTx (UnsignedTx inp _ _) = NE.toList inp
    let txsCoinValue = Set.fromList . concatMap getInpsFromTx
    txsCoinValue (assignMigrationAddresses addrs sels) === csInps

-- For each transaction t created from a selection s, the inputs within
-- t should be identical to the inputs within s.
-- (The counts and values of coins should both be identical.)
prop_allInputsAreUsedPerTx
    :: CoinSelectionsSetup
    -> Property
prop_allInputsAreUsedPerTx (CoinSelectionsSetup cs addrs) = do
    let sels = getCS <$> cs
    let csInps = Set.fromList . inputs <$> sels
    let getInpsFromTx (UnsignedTx inp _ _) = NE.toList inp
    let txsCoinValue = map (Set.fromList . getInpsFromTx)
    txsCoinValue (assignMigrationAddresses addrs sels) === csInps

-- For any given pair of addresses a1 and a2 in the given address list,
-- if a1 is used n times, then a2 should be used either n or n − 1 times.
-- (Assuming a1 and a2 appear in order.)
prop_fairAddressesRecycled
    :: CoinSelectionsSetup
    -> Property
prop_fairAddressesRecycled (CoinSelectionsSetup cs addrs) = do
    let sels = getCS <$> cs
    let getAllAddrPerTx (UnsignedTx _ txouts _) =
            map (\(TxOut addr _) -> addr) txouts
    let getAllAddrCounts =
            Map.elems .
            foldr (\x -> Map.insertWith (+) x (1::Int)) Map.empty .
            concatMap getAllAddrPerTx
    let addrsCounts = getAllAddrCounts $ assignMigrationAddresses addrs sels
    let maxAddressCountDiff :: [Int] -> Bool
        maxAddressCountDiff xs = L.maximum xs - L.minimum xs <= 1
    counterexample (show addrsCounts) $
        maxAddressCountDiff addrsCounts === True

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
    , csWithdrawal :: Quantity "lovelace" Word64
    -- ^ Available Withdrawal
    , csOuts :: NonEmpty TxOut
        -- ^ Requested outputs for the payment
    } deriving Show

instance Buildable CoinSelProp where
    build (CoinSelProp utxo wdrl outs) = mempty
        <> build utxo
        <> nameF "outs" (blockListF outs)
        <> nameF "withdrawal" (build wdrl)

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
    , totalWithdrawal :: Word64
        -- ^ Total withdrawal available for the selection. May be split across
        -- outputs.
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
         -> Quantity "lovelace" Word64
         -> UTxO
         -> ExceptT (ErrCoinSelection ErrValidation) IO (CoinSelection, UTxO)
       )
    -> String
    -> Either (ErrCoinSelection ErrValidation) CoinSelectionResult
    -> CoinSelectionFixture
    -> SpecWith ()
coinSelectionUnitTest run lbl expected (CoinSelectionFixture n fn utxoF outsF w) =
    it title $ do
        (utxo,txOuts) <- setup
        result <- runExceptT $ do
            cs <- fst <$> run
                (CoinSelectionOptions (const n) fn) txOuts (Quantity w) utxo
            return $ CoinSelectionResult
                { rsInputs  = map (getCoin . coin . snd) (inputs cs)
                , rsChange  = map getCoin (change cs)
                , rsOutputs = map (getCoin . coin) (outputs cs)
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
    shrink (CoinSelProp utxo wdrl outs) =
        [ CoinSelProp utxo' wdrl outs | utxo' <- shrink utxo ]
        ++ [ CoinSelProp utxo wdrl' outs | wdrl' <- shrinkWdrl wdrl ]
        ++ [ CoinSelProp utxo wdrl outs' | outs' <- shrink outs ]
      where
        shrinkWdrl = map Quantity . shrink . getQuantity
    arbitrary = do
        utxo <- arbitrary
        wdrl <- Quantity <$> frequency [(65, return 0), (35, arbitrary)]
        outs <- arbitrary
        return $ CoinSelProp utxo wdrl outs

instance Arbitrary CoinSelectionForMigration where
    arbitrary = do
        txIntxOuts <- Map.toList . getUTxO <$> arbitrary
        let chgs = map (\(_, TxOut _ c) -> c) txIntxOuts
        pure $ CoinSelectionForMigration $ mempty
            { inputs = txIntxOuts
            , change = chgs
            }

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
