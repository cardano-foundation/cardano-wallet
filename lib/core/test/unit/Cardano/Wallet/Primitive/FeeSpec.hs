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
    ( CoinSelection (..)
    , changeBalance
    , feeBalance
    , inputBalance
    , outputBalance
    )
import Cardano.Wallet.Primitive.CoinSelection.LargestFirst
    ( largestFirst )
import Cardano.Wallet.Primitive.Fee
    ( ErrAdjustForFee (..)
    , Fee (..)
    , FeeOptions (..)
    , OnDanglingChange (..)
    , adjustForFee
    , coalesceDust
    , divvyFee
    , rebalanceSelection
    )
import Cardano.Wallet.Primitive.Types
    ( ShowFmt (..), TxIn (..), TxOut (..), UTxO (..) )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..) )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Control.Arrow
    ( first )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Monad.Trans.Except
    ( runExceptT )
import Data.Either
    ( isRight )
import Data.Function
    ( (&) )
import Data.Functor.Identity
    ( Identity (runIdentity) )
import Data.List.NonEmpty
    ( NonEmpty )
import Data.Quantity
    ( Quantity (..) )
import Data.Word
    ( Word64 )
import Fmt
    ( Buildable (..), nameF, pretty, tupleF )
import Test.Hspec
    ( Spec, SpecWith, describe, it, shouldBe )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , NonEmptyList (..)
    , Property
    , checkCoverage
    , choose
    , classify
    , conjoin
    , counterexample
    , coverTable
    , elements
    , expectFailure
    , forAllBlind
    , frequency
    , generate
    , oneof
    , property
    , scale
    , tabulate
    , vector
    , withMaxSuccess
    , (===)
    , (==>)
    )
import Test.QuickCheck.Monadic
    ( assert, monadicIO, pre, run )

import qualified Cardano.Wallet.Primitive.CoinSelection as CS
import qualified Data.ByteString as BS
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map

spec :: Spec
spec = do
    describe "Fee calculation : unit tests" $ do
        -- Change covers fee exactly, single change output
        feeUnitTest id (FeeFixture
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
        feeUnitTest id (FeeFixture
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
        feeUnitTest id (FeeFixture
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
        feeUnitTest id (FeeFixture
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
        feeUnitTest id (FeeFixture
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
        feeUnitTest id (FeeFixture
            { fInps = [20,20,20]
            , fOuts = [14,18,19]
            , fChngs = [6,2,1]
            , fUtxo = []
            , fFee = 3
            , fDust = 1
            }) (Right $ FeeOutput
            { csInps = [20,20,20]
            , csOuts = [14,18,19]
            , csChngs = [6]
            })

        -- Cannot cover fee, no extra inputs
        feeUnitTest id (FeeFixture
            { fInps = [20]
            , fOuts = [17]
            , fChngs = [3]
            , fUtxo = []
            , fFee = 4
            , fDust = 0
            }) (Left $ ErrCannotCoverFee 1)

        -- Cannot cover fee even with an extra (too small) inputs
        feeUnitTest id (FeeFixture
            { fInps = [10]
            , fOuts = [7]
            , fChngs = [3]
            , fUtxo = [1]
            , fFee = 5
            , fDust = 0
            }) (Left $ ErrCannotCoverFee 1)

        -- Can select extra inputs to exactly cover fee, no change back
        feeUnitTest id (FeeFixture
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

        -- Cannot select more inputs than allowed to cover fee.
        feeUnitTest (\opts -> opts { maximumNumberOfInputs = 2 }) (FeeFixture
            { fInps = [10]
            , fOuts = [7]
            , fChngs = [3]
            , fUtxo = [1,1]
            , fFee = 5
            , fDust = 0
            }) (Left $ ErrCannotCoverFee 1)

        -- Can select extra inputs to cover for fee, and leave a change back
        feeUnitTest id (FeeFixture
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
        feeUnitTest id (FeeFixture
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
        feeUnitTest id (FeeFixture
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
        feeUnitTest id (FeeFixture
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
        feeUnitTest id (FeeFixture
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
        feeUnitTest id (FeeFixture
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

        feeUnitTest id (FeeFixture
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

        feeUnitTest id (FeeFixture
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

        feeUnitTest id (FeeFixture
            { fInps = []
            , fOuts = []
            , fChngs = []
            , fUtxo = [2,2]
            , fFee = 3
            , fDust = 1
            }) (Right $ FeeOutput
            { csInps = [2,2]
            , csOuts = []
            , csChngs = [1]
            })

    describe "Fee Calculation: Generators" $ do
        it "Arbitrary CoinSelection" $ property $ \(ShowFmt cs) ->
            property $ isValidSelection cs
                & counterexample ("output balance: "
                    <> show (outputBalance cs + changeBalance cs))
                & counterexample ("input balance:  "
                    <> show (inputBalance cs))

    describe "Fee Adjustment properties" $ do
        it "Fee adjustment is deterministic when there's no extra inputs"
            (property propDeterministic)
        it "Adjusting for fee (/= 0) reduces the change outputs or increase inputs"
            (property propReducedChanges)

    describe "divvyFee" $ do
        it "Σ fst (divvyFee fee outs) == fee"
            (checkCoverage propDivvyFeeSame)
        it "snd (divvyFee fee outs) == outs"
            (checkCoverage propDivvyFeeOuts)
        it "expectFailure: not (any null (fst <$> divvyFee fee outs))"
            (expectFailure propDivvyFeeNoNullFee)
        it "expectFailure: empty list"
            (expectFailure propDivvyFeeInvariantEmptyList)

    describe "prop_rebalanceSelection" $ do
        it "The fee balancing algorithm converges for any coin selection."
            $ property
            $ withMaxSuccess 10000
            $ forAllBlind genSelection' prop_rebalanceSelection


{-------------------------------------------------------------------------------
                         Fee Adjustment - Properties
-------------------------------------------------------------------------------}

-- Check whether a selection is valid
isValidSelection :: CoinSelection -> Bool
isValidSelection cs =
    let
        oAmt = sum $ map (fromIntegral . getCoin . coin) (outputs cs)
        cAmt = sum $ map (fromIntegral . getCoin) (change cs)
        iAmt = sum $ map (fromIntegral . getCoin . coin . snd) (inputs cs)
    in
        iAmt + (withdrawal cs) + (reclaim cs) >= oAmt + cAmt + (deposit cs)

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
    :: ShowFmt FeeProp
    -> Property
propReducedChanges (ShowFmt (FeeProp coinSel utxo (fee, dust)))
    = withMaxSuccess 1000
    $ classify (reserve coinSel > fee) "reserve > fee"
    $ monadicIO
    $ do
        coinSel' <- run $ runExceptT $ adjustForFee feeOpt utxo coinSel
        pre (isRight coinSel')
        let Right s = coinSel'
        let chgs' = sum $ map getCoin $ change s
        let chgs = sum $ map getCoin $ change coinSel
        let inps' = CS.inputs s
        let inps = CS.inputs coinSel
        assert (chgs' < chgs || length inps' >= length inps)
  where
    reserve cs = withdrawal cs + reclaim cs
    feeOpt = feeOptions fee dust

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
                         Fee Adjustment - properties
-------------------------------------------------------------------------------}

prop_rebalanceSelection
    :: CoinSelection
    -> OnDanglingChange
    -> Coin
    -> Property
prop_rebalanceSelection sel onDangling threshold = do
    let (sel', fee') = first withCoalescedDust $ rebalanceSelection opts sel

    let selectionIsBalanced = case onDangling of
            PayAndBalance ->
                delta sel' == fromIntegral (getFee $ estimateFee opts sel')
            SaveMoney ->
                delta sel' >= fromIntegral (getFee $ estimateFee opts sel')

    let equalityModuloChange =
            sel { change = [] } == sel' { change = [] }

    let noDust =
            all (> threshold') (change sel')

    conjoin
        [ fee' == Fee 0 ==> selectionIsBalanced
        , selectionIsBalanced ==> not (null (inputs sel'))
        , selectionIsBalanced ==> isValidSelection sel'
        , property noDust
        , property equalityModuloChange
        ]
        & counterexample (unlines
            [ "selection (before):", pretty sel
            , "selection (after):", pretty sel'
            , "delta (before): " <> show (delta sel)
            , "delta (after):  " <> show (delta sel')
            , "total fee:      " <> show (getFee $ estimateFee opts sel')
            , "remaining fee:  " <> show (getFee fee')
            ])
        & classify (reserveNonNull && feeLargerThanDelta)
            "reserve > 0 && fee > delta"
        & classify (reserveLargerThanFee && feeLargerThanDelta)
            "reserve > fee && fee > delta"
        & classify reserveLargerThanFee
            "reserve > fee"
        & classify feeLargerThanDelta
            "fee > delta"
        & classify (null (inputs sel))
            "no inputs"
  where
    delta :: CoinSelection -> Integer
    delta s =
        fromIntegral (inputBalance s)
        -
        fromIntegral (outputBalance s + changeBalance s)

    withCoalescedDust :: CoinSelection -> CoinSelection
    withCoalescedDust cs =
        cs { change = coalesceDust threshold' (change cs) }

    -- NOTE: We only allow non-null dust threshold if 'onDangling' is
    -- set to 'SaveMoney'.
    threshold' =
        case onDangling of
            SaveMoney ->
                threshold

            PayAndBalance ->
                Coin 0

    opts = FeeOptions
        { estimateFee = \cs ->
            case onDangling of
                -- NOTE
                -- Dummy fee policy but, following a similar rule as the fee
                -- policy on Byron / Shelley (bigger transaction cost more) with
                -- sensible values.
                SaveMoney ->
                    let
                        size = fromIntegral $ length $ show cs
                    in
                        Fee (100000 + 100 * size)

                -- NOTE
                -- Dummy fee policy but, following a similar rule as the fee
                -- policy on Jormungandr. More inputs/outputs cost more.
                PayAndBalance ->
                    let
                        ios = fromIntegral
                            $ length (inputs cs)
                            + length (outputs cs)
                            + length (change cs)
                    in
                        Fee (100000 + 100 * ios)

        , dustThreshold = threshold'
        , onDanglingChange = onDangling
        , feeUpperBound = Fee maxBound
        , maximumNumberOfInputs = maxBound
        }

    reserveNonNull =
        withdrawal sel + reclaim sel > 0
    reserveLargerThanFee =
        withdrawal sel + reclaim sel > getFee (estimateFee opts sel)
    feeLargerThanDelta =
        fromIntegral (getFee $ estimateFee opts sel) > delta sel

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
    , onDanglingChange =
        PayAndBalance
    , feeUpperBound =
        Fee maxBound
    , maximumNumberOfInputs =
        maxBound
    }

feeUnitTest
    :: (FeeOptions -> FeeOptions)
    -> FeeFixture
    -> Either ErrAdjustForFee FeeOutput
    -> SpecWith ()
feeUnitTest adjustOpts fixture expected = it title $ do
    (utxo, cs) <- setup
    result <- runExceptT $ do
        cs' <- adjustForFee (adjustOpts $ feeOptions feeF dustF) utxo cs
        return $ FeeOutput
            { csInps  = map (getCoin . coin . snd) (inputs cs')
            , csOuts  = map (getCoin . coin) (outputs cs')
            , csChngs = map getCoin (change cs')
            }
    result `shouldBe` expected
  where
    FeeFixture inpsF outsF chngsF utxoF feeF dustF = fixture
    setup :: IO (UTxO, CoinSelection)
    setup = do
        utxo <- generate (genUTxO $ Coin <$> utxoF)
        inps <- (Map.toList . getUTxO) <$> generate (genUTxO $ Coin <$> inpsF)
        outs <- generate (genTxOut $ Coin <$> outsF)
        let chngs = map Coin chngsF
        pure (utxo, mempty { inputs = inps, outputs = outs, change = chngs })

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
    inps <- vector n
    outs <- genTxOut coins
    return $ UTxO $ Map.fromList $ zip inps outs

genTxOut :: [Coin] -> Gen [TxOut]
genTxOut coins = do
    let n = length coins
    outs <- vector n
    return $ zipWith TxOut outs coins

genSelection :: Gen CoinSelection
genSelection = do
    outs <- choose (1, 10) >>= vector >>= genTxOut
    genSelectionFor (NE.fromList outs)

-- Like 'genSelection', but allows for having empty input and output.
genSelection' :: Gen CoinSelection
genSelection' = frequency
    [ (4, genSelection)
    , (1, do
        reclaim_ <- genReclaim
        pure $ mempty { reclaim = reclaim_ }
      )
    , (1, do
        deposit_ <- genDeposit 100000
        pure $ mempty { deposit = deposit_ }
      )
    ]

genWithdrawal :: Gen Word64
genWithdrawal = frequency
    [ (3, pure 0)
    , (1, oneof
        [ choose (1, 10000)
        , choose (500000, 1000000)
        ]
      )
    ]

genReclaim :: Gen Word64
genReclaim = genWithdrawal

genDeposit :: Word64 -> Gen Word64
genDeposit sup
    | sup == 0  = pure 0
    | otherwise = frequency
        [ (3, pure 0)
        , (1, choose (1, sup))
        ]

genSelectionFor :: NonEmpty TxOut -> Gen CoinSelection
genSelectionFor outs = do
    let opts = CS.CoinSelectionOptions (const 100) (const $ pure ())
    utxo <- vector (NE.length outs * 3) >>= genUTxO
    withdrawal_ <- genWithdrawal
    case runIdentity $ runExceptT $ largestFirst opts outs (Quantity withdrawal_) utxo of
        Left _ -> genSelectionFor outs
        Right (s,_) -> do
            reclaim_ <- genReclaim
            let s' = s { withdrawal = withdrawal_, reclaim = reclaim_ }
            deposit_ <- genDeposit (feeBalance s')
            pure $ s' { deposit = deposit_ }

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
            >>= vector
            >>= genUTxO
        fee <- choose (100000, 500000)
        dust <- choose (0, 10000)
        return $ FeeProp cs utxo (fee, dust)

instance Arbitrary (Hash "Tx") where
    shrink _ = []
    arbitrary = do
        bytes <- BS.pack <$> vector 32
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
    shrink cs = case (inputs cs, outputs cs, change cs) of
        ([_], [_], []) ->
            []
        _ ->
            let
                shrinkList xs
                    | length xs > 1 = drop 1 xs
                    | otherwise     = xs
                inps  = inputs cs
                inps' = shrinkList inps
                outs  = outputs cs
                outs' = shrinkList outs
                chgs  = change cs
                chgs' = drop 1 chgs
            in
                filter (\s -> s /= cs && isValidSelection s)
                    [ cs { inputs = inps', outputs = outs', change = chgs' }
                    , cs { inputs = inps', outputs = outs , change = chgs  }
                    , cs { inputs = inps , outputs = outs', change = chgs  }
                    , cs { inputs = inps , outputs = outs , change = chgs' }
                    ]
    arbitrary = do
        outs <- choose (1, 10)
            >>= vector
            >>= genTxOut
        genSelectionFor (NE.fromList outs)

instance Arbitrary OnDanglingChange
  where
    arbitrary = elements [ PayAndBalance, SaveMoney ]

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
            , onDanglingChange = PayAndBalance
            , feeUpperBound = Fee maxBound
            , maximumNumberOfInputs = maxBound
            }

instance Show FeeOptions where
    show (FeeOptions _ dust onDangling maxFee maxN) =
        show (dust, onDangling, maxFee, maxN)
