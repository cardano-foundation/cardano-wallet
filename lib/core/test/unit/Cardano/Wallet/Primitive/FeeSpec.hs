{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Primitive.FeeSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Primitive.CoinSelection
    ( CoinSelection (..) )
import Cardano.Wallet.Primitive.CoinSelection.LargestFirst
    ( largestFirst )
import Cardano.Wallet.Primitive.Fee
    ( ErrAdjustForFee (..), Fee (..), FeeOptions (..), adjustForFee, divvyFee )
import Cardano.Wallet.Primitive.Types
    ( Address (..)
    , Coin (..)
    , Hash (..)
    , ShowFmt (..)
    , TxIn (..)
    , TxOut (..)
    , UTxO (..)
    )
import Control.Arrow
    ( left )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Monad.Trans.Except
    ( runExceptT )
import Crypto.Random
    ( SystemDRG, getSystemDRG )
import Crypto.Random.Types
    ( withDRG )
import Data.Either
    ( isRight )
import Data.Functor.Identity
    ( Identity (runIdentity) )
import Data.List.NonEmpty
    ( NonEmpty )
import Data.Word
    ( Word64 )
import Fmt
    ( Buildable (..), nameF, tupleF )
import Test.Hspec
    ( Spec, SpecWith, before, describe, it, shouldBe, shouldSatisfy )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , NonEmptyList (..)
    , Property
    , checkCoverage
    , choose
    , coverTable
    , disjoin
    , elements
    , expectFailure
    , generate
    , property
    , scale
    , tabulate
    , vectorOf
    , withMaxSuccess
    , (===)
    , (==>)
    )
import Test.QuickCheck.Monadic
    ( monadicIO )

import qualified Cardano.Wallet.Primitive.CoinSelection as CS
import qualified Data.ByteString as BS
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map

spec :: Spec
spec = do
    describe "Fee calculation : unit tests" $ do
        -- Change covers fee exactly, single change output
        feeUnitTest (FeeFixture
            { fInps = [20]
            , fOuts = [17]
            , fChngs = [3]
            , fUtxo = []
            , fFee = 3
            , fDust = 0
            }) (Right $ FeeOutput
            { csInps = [20]
            , csOuts = [17]
            , csChngs = []
            })

        -- Total change covers fee, multiple change outputs
        feeUnitTest (FeeFixture
            { fInps = [20,20]
            , fOuts = [16,18]
            , fChngs = [4,2]
            , fUtxo = []
            , fFee = 6
            , fDust = 0
            }) (Right $ FeeOutput
            { csInps = [20,20]
            , csOuts = [16,18]
            , csChngs = []
            })

        -- Fee split evenly across change outputs
        feeUnitTest (FeeFixture
            { fInps = [20,20]
            , fOuts = [18,18]
            , fChngs = [2,2]
            , fUtxo = []
            , fFee = 2
            , fDust = 0
            }) (Right $ FeeOutput
            { csInps = [20,20]
            , csOuts = [18,18]
            , csChngs = [1,1]
            })

        -- Fee split evenly across change outputs
        feeUnitTest (FeeFixture
            { fInps = [20,20]
            , fOuts = [17,18]
            , fChngs = [3,2]
            , fUtxo = []
            , fFee = 2
            , fDust = 0
            }) (Right $ FeeOutput
            { csInps = [20,20]
            , csOuts = [17,18]
            , csChngs = [1,2]
            })

        -- Fee divvied, dust removed (dust = 0)
        feeUnitTest (FeeFixture
            { fInps = [20,20,20]
            , fOuts = [14,18,19]
            , fChngs = [6,2,1]
            , fUtxo = []
            , fFee = 3
            , fDust = 0
            }) (Right $ FeeOutput
            { csInps = [20,20,20]
            , csOuts = [14,18,19]
            , csChngs = [4,1,1]
            })

        -- Fee divvied, dust removed (dust = 1)
        feeUnitTest (FeeFixture
            { fInps = [20,20,20]
            , fOuts = [14,18,19]
            , fChngs = [6,2,1]
            , fUtxo = []
            , fFee = 3
            , fDust = 1
            }) (Right $ FeeOutput
            { csInps = [20,20,20]
            , csOuts = [14,18,19]
            , csChngs = [4]
            })

        -- Cannot cover fee, no extra inputs
        feeUnitTest (FeeFixture
            { fInps = [20]
            , fOuts = [17]
            , fChngs = [3]
            , fUtxo = []
            , fFee = 4
            , fDust = 0
            }) (Left $ ErrCannotCoverFee 1)

        -- Cannot cover fee even with an extra (too small) inputs
        feeUnitTest (FeeFixture
            { fInps = [10]
            , fOuts = [7]
            , fChngs = [3]
            , fUtxo = [1]
            , fFee = 5
            , fDust = 0
            }) (Left $ ErrCannotCoverFee 1)

        -- Can select extra inputs to exactly cover fee, no change back
        feeUnitTest (FeeFixture
            { fInps = [10]
            , fOuts = [7]
            , fChngs = [3]
            , fUtxo = [1,1]
            , fFee = 5
            , fDust = 0
            }) (Right $ FeeOutput
            { csInps = [10,1,1]
            , csOuts = [7]
            , csChngs = []
            })

        -- Can select extra inputs to cover for fee, and leave a change back
        feeUnitTest (FeeFixture
            { fInps = [10]
            , fOuts = [7]
            , fChngs = [3]
            , fUtxo = [3]
            , fFee = 5
            , fDust = 0
            }) (Right $ FeeOutput
            { csInps = [10,3]
            , csOuts = [7]
            , csChngs = [1]
            })

        -- Multiple change output, can select extra inputs to cover fee, no change
        feeUnitTest (FeeFixture
            { fInps = [10,10]
            , fOuts = [7,7]
            , fChngs = [3,3]
            , fUtxo = [2,2]
            , fFee = 10
            , fDust = 0
            }) (Right $ FeeOutput
            { csInps = [10,10,2,2]
            , csOuts = [7,7]
            , csChngs = []
            })

        -- Multiple outputs, extra inputs selected, resulting change
        feeUnitTest (FeeFixture
            { fInps = [10,10]
            , fOuts = [7,7]
            , fChngs = [3,3]
            , fUtxo = [3,3]
            , fFee = 10
            , fDust = 0
            }) (Right $ FeeOutput
            { csInps = [10,10,3,3]
            , csOuts = [7,7]
            , csChngs = [1,1]
            })

        -- Multiple change outputs, some bigger than actual Dust
        feeUnitTest (FeeFixture
            { fInps = [20,20]
            , fOuts = [16,18]
            , fChngs = [4,2]
            , fUtxo = []
            , fFee = 6
            , fDust = 2
            }) (Right $ FeeOutput
            { csInps = [20,20]
            , csOuts = [16,18]
            , csChngs = []
            })

        -- Change created when there was no change before
        feeUnitTest (FeeFixture
            { fInps = [1]
            , fOuts = [1]
            , fChngs = []
            , fUtxo = [2]
            , fFee = 1
            , fDust = 0
            }) (Right $ FeeOutput
            { csInps = [1,2]
            , csOuts = [1]
            , csChngs = [1]
            })

        let c = getCoin maxBound

        -- New BIG inputs selected causes change to overflow
        feeUnitTest (FeeFixture
            { fInps = [c-1, c-1]
            , fOuts = [c-1]
            , fChngs = [c-1]
            , fUtxo = [c]
            , fFee = c
            , fDust = 0
            }) (Right $ FeeOutput
            { csInps = [c-1, c-1, c]
            , csOuts = [c-1]
            , csChngs = [c `div` 2 - 1, c `div` 2]
            })

        feeUnitTest (FeeFixture
            { fInps = []
            , fOuts = []
            , fChngs = []
            , fUtxo = [3]
            , fFee = 3
            , fDust = 0
            }) (Right $ FeeOutput
            { csInps = [3]
            , csOuts = []
            , csChngs = []
            })

        feeUnitTest (FeeFixture
            { fInps = []
            , fOuts = []
            , fChngs = []
            , fUtxo = [2,2]
            , fFee = 3
            , fDust = 0
            }) (Right $ FeeOutput
            { csInps = [2,2]
            , csOuts = []
            , csChngs = [1]
            })

        feeUnitTest (FeeFixture
            { fInps = []
            , fOuts = []
            , fChngs = []
            , fUtxo = [2,2]
            , fFee = 3
            , fDust = 1
            }) (Right $ FeeOutput
            { csInps = [2,2]
            , csOuts = []
            , csChngs = []
            })

    describe "Fee Calculation: Generators" $ do
        it "Arbitrary CoinSelection" $ property $ \(ShowFmt cs) ->
            property $ isValidSelection cs

    before getSystemDRG $ describe "Fee Adjustment properties" $ do
        it "Fee adjustment is deterministic when there's no extra inputs"
            (\_ -> property propDeterministic)
        it "Adjusting for fee (/= 0) reduces the change outputs or increase inputs"
            (property . propReducedChanges)

    describe "divvyFee" $ do
        it "Σ fst (divvyFee fee outs) == fee"
            (checkCoverage propDivvyFeeSame)
        it "snd (divvyFee fee outs) == outs"
            (checkCoverage propDivvyFeeOuts)
        it "expectFailure: not (any null (fst <$> divvyFee fee outs))"
            (expectFailure propDivvyFeeNoNullFee)
        it "expectFailure: empty list"
            (expectFailure propDivvyFeeInvariantEmptyList)

{-------------------------------------------------------------------------------
                         Fee Adjustment - Properties
-------------------------------------------------------------------------------}

-- Check whether a selection is valid
isValidSelection :: CoinSelection -> Bool
isValidSelection (CoinSelection i o c) =
    let
        oAmt = sum $ map (fromIntegral . getCoin . coin) o
        cAmt = sum $ map (fromIntegral . getCoin) c
        iAmt = sum $ map (fromIntegral . getCoin . coin . snd) i
    in
        (iAmt :: Integer) >= (oAmt + cAmt)

-- | Data for running fee calculation properties
data FeeProp = FeeProp
    { selection :: CoinSelection
     -- ^ inputs from wich largestFirst can be calculated
    , availableUtxo :: UTxO
     -- ^ additional UTxO from which fee calculation will pick needed coins
    , feeDust :: (Word64, Word64)
     -- ^ constant fee and dust threshold
    } deriving Show

instance Buildable FeeProp where
    build (FeeProp cc utxo opt) = mempty
        <> nameF "selection" (build cc)
        <> build utxo
        <> nameF "options" (tupleF opt)

propDeterministic
    :: ShowFmt FeeProp
    -> Property
propDeterministic (ShowFmt (FeeProp coinSel _ (fee, dust))) = monadicIO $ liftIO $ do
    let feeOpt = feeOptions fee dust
    let utxo = mempty
    resultOne <- runExceptT $ adjustForFee feeOpt utxo coinSel
    resultTwo <- runExceptT $ adjustForFee feeOpt utxo coinSel
    resultOne `shouldBe` resultTwo

propReducedChanges
    :: SystemDRG
    -> ShowFmt FeeProp
    -> Property
propReducedChanges drg (ShowFmt (FeeProp coinSel utxo (fee, dust))) = do
    isRight coinSel' ==> let Right s = coinSel' in prop s
  where
    prop s = do
        let chgs' = sum $ map getCoin $ change s
        let chgs = sum $ map getCoin $ change coinSel
        let inps' = CS.inputs s
        let inps = CS.inputs coinSel
        disjoin
            [ chgs' `shouldSatisfy` (<= chgs)
            , length inps' `shouldSatisfy` (>= length inps)
            ]
    feeOpt = feeOptions fee dust
    coinSel' = left show $ fst $ withDRG drg $ runExceptT $
        adjustForFee feeOpt utxo coinSel

{-------------------------------------------------------------------------------
                             divvyFee - Properties
-------------------------------------------------------------------------------}

-- | Helper to re-apply the pre-conditions for divvyFee
propDivvyFee
    :: ((Fee, [Coin]) -> Property)
    -> (Fee, NonEmptyList Coin)
    -> Property
propDivvyFee prop (fee, NonEmpty outs) =
    coverTable "properties"
        [ ("fee > 0", 50)
        , ("nOuts=1", 1)
        , ("nOuts=2", 1)
        , ("nOuts=2+", 10)
        ]
        $ tabulate "properties"
            [ if fee > Fee 0 then "fee > 0" else "fee == 0"
            , "nOuts=" <> case length outs of
                n | n <= 2 -> show n
                _ -> "2+"
            ]
        $ prop (fee, outs)

-- | Sum of the fees divvied over each output is the same as the initial total
-- fee.
propDivvyFeeSame
    :: (Fee, NonEmptyList Coin)
    -> Property
propDivvyFeeSame = propDivvyFee $ \(fee, outs) ->
    sum (getFee . fst <$> divvyFee fee outs) === getFee fee

-- | divvyFee doesn't change any of the outputs
propDivvyFeeOuts
    :: (Fee, NonEmptyList Coin)
    -> Property
propDivvyFeeOuts = propDivvyFee $ \(fee, outs) ->
    (snd <$> divvyFee fee outs) === outs

-- | divvyFee never generates null fees for a given output.
--
-- This is NOT a property. It is here to illustrate that this can happen in
-- practice, and is known as a possible outcome for the divvyFee function
-- (it is fine for one of the output to be assigned no fee). The only reason
-- this would happen is because there would be less outputs than the fee amount
-- which is probably never going to happen in practice...
propDivvyFeeNoNullFee
    :: (Fee, [Coin])
    -> Property
propDivvyFeeNoNullFee (fee, outs) =
    not (null outs) ==> withMaxSuccess 100000 prop
  where
    prop = property $ Fee 0 `notElem` (fst <$> divvyFee fee outs)

-- | Illustrate the invariant: 'outs' should be an non-empty list
propDivvyFeeInvariantEmptyList
    :: (Fee, [Coin])
    -> Property
propDivvyFeeInvariantEmptyList (fee, outs) =
    withMaxSuccess 100000 prop
  where
    prop = divvyFee fee outs `seq` True

{-------------------------------------------------------------------------------
                         Fee Adjustment - Unit Tests
-------------------------------------------------------------------------------}

feeOptions
    :: Word64
    -> Word64
    -> FeeOptions
feeOptions fee dust = FeeOptions
    { estimateFee =
        \_ -> Fee fee
    , dustThreshold =
        Coin dust
    }

feeUnitTest
    :: FeeFixture
    -> Either ErrAdjustForFee FeeOutput
    -> SpecWith ()
feeUnitTest (FeeFixture inpsF outsF chngsF utxoF feeF dustF) expected = it title $ do
    (utxo, sel) <- setup
    result <- runExceptT $ do
        (CoinSelection inps outs chngs) <-
            adjustForFee (feeOptions feeF dustF) utxo sel
        return $ FeeOutput
            { csInps = map (getCoin . coin . snd) inps
            , csOuts = map (getCoin . coin) outs
            , csChngs = map getCoin chngs
            }
    result `shouldBe` expected
  where
    setup :: IO (UTxO, CoinSelection)
    setup = do
        utxo <- generate (genUTxO $ Coin <$> utxoF)
        inps <- (Map.toList . getUTxO) <$> generate (genUTxO $ Coin <$> inpsF)
        outs <- generate (genTxOut $ Coin <$> outsF)
        let chngs = map Coin chngsF
        pure (utxo, CoinSelection inps outs chngs)

    title :: String
    title = mempty
        <> "CoinSelection (inps=" <> show inpsF
        <> "outs=" <> show outsF
        <> "chngs=" <> show chngsF
        <> "), UTxO=" <> show utxoF
        <> "), fee=" <> show feeF
        <> " --> " <> show expected

-- | A fixture for testing the fee calculation
data FeeFixture = FeeFixture
    { fInps :: [Word64]
        -- ^ Value (in Lovelace) & number of coins in inputs
    , fOuts :: [Word64]
        -- ^ Value (in Lovelace) & number of requested outputs
    , fChngs :: [Word64]
        -- ^ Value (in Lovelace) & number of changes
    , fUtxo :: [Word64]
        -- ^ Value (in Lovelace) & number of available coins in the UTxO
    , fFee :: Word64
        -- ^ Value (in Lovelace) of rigid fee
    , fDust :: Word64
        -- ^ Value (in Lovelace) of dust
    } deriving Show

-- | A fee calculation output
data FeeOutput = FeeOutput
    { csInps :: [Word64]
        -- ^ Value (in Lovelace) & number of available coins in the UTxO
    , csOuts :: [Word64]
        -- ^ Value (in Lovelace) & number of requested outputs
    , csChngs :: [Word64]
        -- ^ Value (in Lovelace) & number of changes
    } deriving (Show, Eq)

{-------------------------------------------------------------------------------
                            Arbitrary Instances
-------------------------------------------------------------------------------}

deriving newtype instance Arbitrary a => Arbitrary (ShowFmt a)

genUTxO :: [Coin] -> Gen UTxO
genUTxO coins = do
    let n = length coins
    inps <- vectorOf n arbitrary
    outs <- genTxOut coins
    return $ UTxO $ Map.fromList $ zip inps outs

genTxOut :: [Coin] -> Gen [TxOut]
genTxOut coins = do
    let n = length coins
    outs <- vectorOf n arbitrary
    return $ zipWith TxOut outs coins

genSelection :: NonEmpty TxOut -> Gen CoinSelection
genSelection outs = do
    let opts = CS.CoinSelectionOptions (const 100) (const $ pure ())
    utxo <- vectorOf (NE.length outs * 3) arbitrary >>= genUTxO
    case runIdentity $ runExceptT $ largestFirst opts outs utxo of
        Left _ -> genSelection outs
        Right (s,_) -> return s

instance Arbitrary TxIn where
    shrink _ = []
    arbitrary = TxIn
        <$> arbitrary
        <*> scale (`mod` 3) arbitrary -- No need for a high indexes

instance Arbitrary Coin where
    shrink (Coin c) = Coin <$> filter (> 0) (shrink $ fromIntegral c)
    arbitrary = Coin <$> choose (1, 200000)

instance Arbitrary Fee where
    shrink (Fee c) = Fee <$> filter (> 0) (shrink $ fromIntegral c)
    arbitrary = Fee . getCoin <$> arbitrary

instance Arbitrary FeeProp where
    shrink (FeeProp cs utxo opts) =
        case Map.toList $ getUTxO utxo  of
            [] ->
                map (\cs' -> FeeProp cs' utxo opts) (shrink cs)
            us ->
                concatMap (\cs' ->
                    [ FeeProp cs' mempty opts
                    , FeeProp cs' (UTxO $ Map.fromList (drop 1 us)) opts
                    ]
                ) (shrink cs)
    arbitrary = do
        cs <- arbitrary
        utxo <- choose (0, 50)
            >>= \n -> vectorOf n arbitrary
            >>= genUTxO
        fee <- choose (100000, 500000)
        dust <- choose (0, 10000)
        return $ FeeProp cs utxo (fee, dust)

instance Arbitrary (Hash "Tx") where
    shrink _ = []
    arbitrary = do
        bytes <- BS.pack <$> vectorOf 32 arbitrary
        pure $ Hash bytes

instance Arbitrary Address where
    shrink _ = []
    arbitrary = elements
        [ Address "addr-0"
        , Address "addr-1"
        , Address "addr-2"
        , Address "addr-3"
        ]

instance Arbitrary CoinSelection where
    shrink sel@(CoinSelection inps outs chgs) = case (inps, outs, chgs) of
        ([_], [_], []) ->
            []
        _ ->
            let
                inps' = if length inps > 1 then drop 1 inps else inps
                outs' = if length outs > 1 then drop 1 outs else outs
                chgs' = drop 1 chgs
            in
                filter (\s -> s /= sel && isValidSelection s)
                    [ CoinSelection inps' outs' chgs'
                    , CoinSelection inps' outs chgs
                    , CoinSelection inps outs' chgs
                    , CoinSelection inps outs chgs'
                    ]
    arbitrary = do
        outs <- choose (1, 10)
            >>= \n -> vectorOf n arbitrary
            >>= genTxOut
        genSelection (NE.fromList outs)

instance Arbitrary FeeOptions where
    arbitrary = do
        t <- choose (0, 10) -- dust threshold
        c <- choose (0, 10) -- price per transaction
        a <- choose (0, 10) -- price per input/output
        return $ FeeOptions
            { estimateFee =
                \s -> Fee
                    $ fromIntegral
                    $ c + a * (length (inputs s) + length (outputs s))
            , dustThreshold = Coin t
            }

instance Show FeeOptions where
    show (FeeOptions _ dust) = show dust
