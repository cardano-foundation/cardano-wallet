{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Primitive.TypesSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..), WalletKey (..), XPrv, digest, publicKey )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( ShelleyKey (..), generateKeyFromSeed )
import Cardano.Wallet.Primitive.Types
    ( ActiveSlotCoefficient (..)
    , Address (..)
    , AddressState (..)
    , Block (..)
    , BlockHeader (..)
    , BoundType
    , Coin (..)
    , Direction (..)
    , Dom (..)
    , EpochLength (..)
    , EpochNo (..)
    , FeePolicy (..)
    , Hash (..)
    , HistogramBar (..)
    , PoolOwner (..)
    , Range (..)
    , RangeBound (..)
    , ShowFmt (..)
    , SlotId (..)
    , SlotLength (..)
    , SlotNo (..)
    , SlotParameters (..)
    , StartTime (..)
    , SyncProgress (..)
    , SyncTolerance (..)
    , Tx (..)
    , TxIn (..)
    , TxMeta (..)
    , TxOut (..)
    , TxStatus (..)
    , UTxO (..)
    , UTxOStatistics (..)
    , WalletId (..)
    , WalletName (..)
    , balance
    , computeUtxoStatistics
    , epochPred
    , epochStartTime
    , epochSucc
    , excluding
    , flatSlot
    , fromFlatSlot
    , isAfterRange
    , isBeforeRange
    , isSubrangeOf
    , isSubsetOf
    , isValidCoin
    , isWithinRange
    , mapRangeLowerBound
    , mapRangeUpperBound
    , mkSyncTolerance
    , rangeHasLowerBound
    , rangeHasUpperBound
    , rangeIsFinite
    , rangeIsSingleton
    , rangeIsValid
    , rangeLowerBound
    , rangeUpperBound
    , restrictedBy
    , restrictedTo
    , slotAt
    , slotCeiling
    , slotDifference
    , slotFloor
    , slotMinBound
    , slotPred
    , slotRangeFromTimeRange
    , slotStartTime
    , slotSucc
    , syncProgress
    , unsafeEpochNo
    , walletNameMaxLength
    , walletNameMinLength
    , wholeRange
    )
import Cardano.Wallet.Unsafe
    ( unsafeFromHex, unsafeMkSomeMnemonicFromEntropy )
import Control.DeepSeq
    ( deepseq )
import Control.Exception
    ( evaluate )
import Control.Monad
    ( forM_, replicateM )
import Crypto.Hash
    ( hash )
import Data.Either
    ( isRight )
import Data.Function
    ( (&) )
import Data.Function.Utils
    ( applyN )
import Data.Maybe
    ( catMaybes, fromMaybe, isJust, isNothing )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Quantity (..) )
import Data.Set
    ( Set, (\\) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( TextDecodingError (..), fromText, toText )
import Data.Time
    ( Day (ModifiedJulianDay), UTCTime, toModifiedJulianDay, utctDay )
import Data.Time.Utils
    ( utcTimePred, utcTimeSucc )
import Data.Word
    ( Word32 )
import Data.Word.Odd
    ( Word31 )
import Fmt
    ( pretty )
import Test.Hspec
    ( Spec
    , anyErrorCall
    , describe
    , it
    , shouldBe
    , shouldNotSatisfy
    , shouldSatisfy
    , shouldThrow
    )
import Test.QuickCheck
    ( Arbitrary (..)
    , NonNegative (..)
    , NonZero (..)
    , Property
    , Small (..)
    , arbitraryBoundedEnum
    , arbitraryBoundedIntegral
    , arbitraryPrintableChar
    , arbitrarySizedBoundedIntegral
    , checkCoverage
    , choose
    , counterexample
    , cover
    , infiniteList
    , oneof
    , property
    , scale
    , shrinkIntegral
    , vectorOf
    , withMaxSuccess
    , (.&&.)
    , (=/=)
    , (===)
    , (==>)
    )
import Test.QuickCheck.Arbitrary.Generic
    ( genericArbitrary, genericShrink )
import Test.QuickCheck.Monadic
    ( monadicIO, run )
import Test.Text.Roundtrip
    ( textRoundtrip )
import Test.Utils.Time
    ( genUniformTime, genUniformTimeWithinRange, getUniformTime )

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T

spec :: Spec
spec = do
    describe "Generators are valid" $ do
        it "Arbitrary Coin" $ property isValidCoin

    describe "Can perform roundtrip textual encoding & decoding" $ do
        textRoundtrip $ Proxy @Address
        textRoundtrip $ Proxy @AddressState
        textRoundtrip $ Proxy @Direction
        textRoundtrip $ Proxy @FeePolicy
        textRoundtrip $ Proxy @TxStatus
        textRoundtrip $ Proxy @WalletName
        textRoundtrip $ Proxy @WalletId
        textRoundtrip $ Proxy @(Hash "Genesis")
        textRoundtrip $ Proxy @(Hash "Tx")
        textRoundtrip $ Proxy @(Hash "Account")
        textRoundtrip $ Proxy @(Hash "Block")
        textRoundtrip $ Proxy @(Hash "BlockHeader")
        textRoundtrip $ Proxy @SyncTolerance
        textRoundtrip $ Proxy @PoolOwner

        -- Extra hand-crafted tests
        it "Valid account IDs are properly decoded from text" $ do
            forM_ testAccountIdTexts $ \text ->
                toText <$> fromText @(Hash "Account") text
                    `shouldBe` Right text

    describe "Buildable" $ do
        it "WalletId" $ do
            let mw = unsafeMkSomeMnemonicFromEntropy (Proxy @12)
                    "0000000000000000"
            let xprv = generateKeyFromSeed
                    (mw, Nothing) mempty :: ShelleyKey 'RootK XPrv
            let wid = WalletId $ digest $ publicKey xprv
            "336c96f1...b8cac9ce" === pretty @_ @Text wid
        it "TxMeta (1)" $ do
            let txMeta = TxMeta
                    { status = Pending
                    , direction = Outgoing
                    , slotId = SlotId 14 42
                    , blockHeight = Quantity 37
                    , amount = Quantity 1337
                    }
            "-0.001337 pending since 14.42#37" === pretty @_ @Text txMeta
        it "TxMeta (2)" $ do
            let txMeta = TxMeta
                    { status = InLedger
                    , direction = Incoming
                    , slotId = SlotId 14 0
                    , blockHeight = Quantity 1
                    , amount = Quantity 13371442
                    }
            "+13.371442 in ledger since 14.0#1" === pretty @_ @Text txMeta

    let sp = SlotParameters
            { getEpochLength = EpochLength 21600
            , getSlotLength  = SlotLength 10
            , getGenesisBlockDate = StartTime (read "2019-11-09 16:43:02 UTC")
            , getActiveSlotCoefficient = 1
            }
    let st = SyncTolerance 10
    let slotsPerEpoch = getEpochLength sp

    describe "syncProgress" $ do
        it "works for any two slots" $ property $ \sl0 sl1 ->
            syncProgress st sp sl0 sl1 `deepseq` ()

        let mockBlockHeader sl h = BlockHeader
                { slotId = sl
                , blockHeight = Quantity h
                , headerHash = Hash "-"
                , parentHeaderHash = Hash "-"
                }

        let mockSlotParams f = SlotParameters
                { getEpochLength = EpochLength 10
                , getSlotLength = SlotLength 10
                , getGenesisBlockDate = StartTime $
                    read "2019-01-01 00:00:00 UTC"
                , getActiveSlotCoefficient = f
                }

        let syncTolerance = SyncTolerance 0

        it "unit test #1 - 0/10   0%" $ do
            let slotParams = mockSlotParams 1
            let nodeTip = mockBlockHeader (SlotId 0 0) 0
            let ntwkTip = SlotId 1 0
            syncProgress syncTolerance slotParams nodeTip ntwkTip
                `shouldBe` Syncing (Quantity $ toEnum 0)

        it "unit test #2 - 10/20 50%" $ do
            let slotParams = mockSlotParams 1
            let nodeTip = mockBlockHeader (SlotId 1 0) 10
            let ntwkTip = SlotId 2 0
            syncProgress syncTolerance slotParams nodeTip ntwkTip
                `shouldBe` Syncing (Quantity $ toEnum 50)

        it "unit test #3 - 12/15 80% " $ do
            let slotParams = mockSlotParams 0.5
            let nodeTip = mockBlockHeader (SlotId 2 2) 12
            let ntwkTip = SlotId 2 8
            syncProgress syncTolerance slotParams nodeTip ntwkTip
                `shouldBe` Syncing (Quantity $ toEnum 80)

        it "unit test #4 - 10/10 100%" $ do
            let slotParams = mockSlotParams 1
            let nodeTip = mockBlockHeader (SlotId 1 0) 10
            let ntwkTip = SlotId 1 0
            syncProgress syncTolerance slotParams nodeTip ntwkTip
                `shouldBe` Ready

        it "unit test #5 - 11/10 100%" $ do
            let slotParams = mockSlotParams 1
            let nodeTip = mockBlockHeader (SlotId 1 1) 11
            let ntwkTip = SlotId 1 0
            syncProgress syncTolerance slotParams nodeTip ntwkTip
                `shouldBe` Ready

        --   100 |                          +  +
        --       |
        --       |                       +
        --       |                    +
        --       |              +  +
        --       |
        --       |           +
        --       |     +  +
        --       |  +
        --       |
        --       |--|--|--|--|--|--|--|--|--|--|
        --       0  1  2  3  4  5  6  7  8  9  10
        --
        it "unit test #5 - distribution over several slots" $ do
            let slotParams = mockSlotParams 0.5
            let ntwkTip = SlotId 1 0
            let plots =
                    [ (mockBlockHeader (SlotId 0 0) 0, 0)
                    , (mockBlockHeader (SlotId 0 1) 1, 20)
                    , (mockBlockHeader (SlotId 0 2) 2, 33)
                    , (mockBlockHeader (SlotId 0 3) 2, 33)
                    , (mockBlockHeader (SlotId 0 4) 2, 40)
                    , (mockBlockHeader (SlotId 0 5) 3, 60)
                    , (mockBlockHeader (SlotId 0 6) 3, 60)
                    , (mockBlockHeader (SlotId 0 7) 4, 66)
                    , (mockBlockHeader (SlotId 0 8) 5, 83)
                    , (mockBlockHeader (SlotId 0 9) 5, 100)
                    , (mockBlockHeader (SlotId 1 0) 5, 100)
                    ]
            forM_ plots $ \(nodeTip, p) -> do
                let progress = if p == 100
                        then Ready
                        else Syncing (Quantity $ toEnum p)
                syncProgress syncTolerance slotParams nodeTip ntwkTip
                    `shouldBe` progress


    describe "flatSlot" $ do

        it "fromFlatSlot . flatSlot == id" $ property $ \sl ->
            fromFlatSlot slotsPerEpoch (flatSlot slotsPerEpoch sl) === sl

        it "flatSlot . fromFlatSlot == id" $ property $ \n -> do
            -- The value of 'fromFlatSlot' is undefined when applied to a value
            -- that is higher than the maximum value of 'flatSlot' for that
            -- epoch length.
            --
            -- Therefore, the roundtrip property only holds when the flat slot
            -- value is less than or equal to this maximum.
            --
            -- For flat slot values that are higher than this maximum, we expect
            -- the 'fromFlatSlot' function to fail with an error.
            let maxSlotNo = SlotNo $ unEpochLength slotsPerEpoch - 1
            let maxSlotId = SlotId (EpochNo maxBound) maxSlotNo
            let maxFlatSlot = flatSlot slotsPerEpoch maxSlotId
            let result = flatSlot slotsPerEpoch $ fromFlatSlot slotsPerEpoch n
            checkCoverage $
                cover 20 (n <= maxFlatSlot) "<= maxFlatSlot" $
                cover 20 (n >  maxFlatSlot) ">  maxFlatSlot" $
                if n <= maxFlatSlot then
                    result `shouldBe` n
                else
                    evaluate result `shouldThrow` anyErrorCall

    describe "Ranges" $ do

        it "arbitrary ranges are valid" $
            withMaxSuccess 1000 $ property $ \(r :: Range Integer) ->
                checkCoverage $
                cover 10 (rangeIsFinite r) "finite range" $
                rangeIsValid r .&&.
                    all rangeIsValid (shrink r)

        it "arbitrary non-singleton ranges are valid" $
            withMaxSuccess 1000 $ property $ \(nsr :: NonSingletonRange Int) ->
                let isValidNonSingleton (NonSingletonRange r) =
                        rangeIsValid r && not (rangeIsSingleton r) in
                checkCoverage $
                cover 10 (rangeIsFinite (getNonSingletonRange nsr))
                    "finite range" $
                isValidNonSingleton nsr .&&.
                    all isValidNonSingleton (shrink nsr)

        it "functions is{Before,Within,After}Range are mutually exclusive" $
            withMaxSuccess 1000 $ property $ \(a :: Integer) r ->
                let options =
                        [ (isBeforeRange)
                        , (isWithinRange)
                        , (isAfterRange) ] in
                checkCoverage $
                cover 10 (a `isBeforeRange` r) "isBeforeRange" $
                cover 10 (a `isWithinRange` r) "isWithinRange" $
                cover 10 (a `isAfterRange`  r) "isAfterRange"  $
                1 === length (filter (\f -> f a r) options)

        it "pred (inclusiveLowerBound r) `isBeforeRange` r" $
            withMaxSuccess 1000 $ property $ \(r :: Range Integer) ->
                checkCoverage $
                cover 10 (rangeHasLowerBound r) "has lower bound" $
                ((`isBeforeRange` r) . pred <$> inclusiveLowerBound r)
                    =/= Just False

        it "inclusiveLowerBound r `isWithinRange` r" $
            withMaxSuccess 1000 $ property $ \(r :: Range Integer) ->
                checkCoverage $
                cover 10 (rangeHasLowerBound r) "has lower bound" $
                ((`isWithinRange` r) <$> inclusiveLowerBound r)
                    =/= Just False

        it "inclusiveUpperBound r `isWithinRange` r" $
            withMaxSuccess 1000 $ property $ \(r :: Range Integer) ->
                checkCoverage $
                cover 10 (rangeHasUpperBound r) "has upper bound" $
                ((`isWithinRange` r) <$> inclusiveUpperBound r)
                    =/= Just False

        it "succ (inclusiveUpperBound r) `isAfterRange` r" $
            withMaxSuccess 1000 $ property $ \(r :: Range Integer) ->
                checkCoverage $
                cover 10 (rangeHasUpperBound r) "has upper bound" $
                ((`isAfterRange` r) . succ <$> inclusiveUpperBound r)
                    =/= Just False

        it "a `isWithinRange` wholeRange == True" $
            property $ \(a :: Integer) ->
                a `isWithinRange` wholeRange === True

        it "rangeIsSingleton (Range a a)" $
            property $ \(a :: Int) ->
                Range (Just a) (Just a) `shouldSatisfy` rangeIsSingleton

        it "not (rangeIsSingleton (Range (pred a) a))" $
            property $ \(a :: Int) ->
                Range (Just (pred a)) (Just a)
                    `shouldNotSatisfy` rangeIsSingleton

        it "not (rangeIsSingleton (Range a (succ a)))" $
            property $ \(a :: Int) ->
                Range (Just a) (Just (succ a))
                    `shouldNotSatisfy` rangeIsSingleton

        it "rangeLowerBound r = rangeUpperBound r <=> rangeIsSingleton r" $
            property $ \(r :: Range Bool) ->
                checkCoverage $
                cover 10 (rangeIsFinite r) "is finite" $
                (rangeLowerBound r == rangeUpperBound r) === rangeIsSingleton r

        it "r `isSubrangeOf` r" $
            property $ \(r :: Range Int) ->
                r `isSubrangeOf` r

        it "r `isSubrangeOf` wholeRange" $
            property $ \(r :: Range Int) ->
                checkCoverage $
                cover 10 (rangeHasLowerBound r) "has lower bound" $
                cover 10 (rangeHasUpperBound r) "has upper bound" $
                cover 10 (rangeIsFinite      r) "is finite" $
                r `isSubrangeOf` wholeRange

        it "Range (succ a) b `isSubrangeOf` Range a b" $
            withMaxSuccess 1000 $ property $ \nsr ->
                let r@(Range a b :: Range Int) = getNonSingletonRange nsr in
                checkCoverage $
                cover 10 (rangeHasLowerBound r) "has lower bound" $
                cover 10 (rangeHasUpperBound r) "has upper bound" $
                cover 10 (rangeIsFinite      r) "is finite" $
                Range (succ <$> a) b `isSubrangeOf` Range a b

        it "Range a (pred b) `isSubrangeOf` Range a b" $
            withMaxSuccess 1000 $ property $ \nsr ->
                let r@(Range a b :: Range Int) = getNonSingletonRange nsr in
                checkCoverage $
                cover 10 (rangeHasLowerBound r) "has lower bound" $
                cover 10 (rangeHasUpperBound r) "has upper bound" $
                cover 10 (rangeIsFinite      r) "is finite" $
                Range a (pred <$> b) `isSubrangeOf` Range a b

        it "Range a b `isSubrangeOf` Range (pred a) b" $
            property $ \r@(Range a b :: Range Int) ->
                checkCoverage $
                cover 10 (rangeHasLowerBound r) "has lower bound" $
                cover 10 (rangeHasUpperBound r) "has upper bound" $
                cover 10 (rangeIsFinite      r) "is finite" $
                Range a b `isSubrangeOf` Range (pred <$> a) b

        it "Range a b `isSubrangeOf` Range a (succ b)" $
            property $ \r@(Range a b :: Range Int) ->
                checkCoverage $
                cover 10 (rangeHasLowerBound r) "has lower bound" $
                cover 10 (rangeHasUpperBound r) "has upper bound" $
                cover 10 (rangeIsFinite      r) "is finite" $
                Range a b `isSubrangeOf` Range a (succ <$> b)

    describe "Range bounds" $ do

        it "NegativeInfinity < InclusiveBound a" $
            property $ \(a :: Int) ->
                NegativeInfinity < InclusiveBound a

        it "InclusiveBound a < PositiveInfinity" $
            property $ \(a :: Int) ->
                InclusiveBound a < PositiveInfinity

        it "compare (InclusiveBound a) (InclusiveBound b) = compare a b" $
            property $ \(a :: Int) (b :: Int) ->
                compare (InclusiveBound a) (InclusiveBound b) === compare a b

    describe "Epoch arithmetic: arbitrary value generation" $ do

        it "EpochNo generation covers interesting cases" $
            withMaxSuccess 10000 $ property $ \(epoch :: EpochNo) ->
                checkCoverage
                    $ cover 10 (epoch == minBound)
                        "minBound"
                    $ cover 10 (epoch == maxBound)
                        "maxBound"
                    $ cover 10 (epoch == minBound + 1)
                        "minBound + 1"
                    $ cover 10 (epoch == maxBound - 1)
                        "maxBound - 1"
                    $ cover 10 (epoch > minBound && epoch < maxBound)
                        "intermediate value"
                True

        it "SlotParametersAndTimePoint generation covers interesting cases" $
            withMaxSuccess 10000 $ property $
                \(SlotParametersAndTimePoint sps t) ->
                    let belowMin = t < epochStartTime sps minBound
                        aboveMax = t > epochStartTime sps maxBound
                    in
                    checkCoverage
                        $ cover 10 belowMin
                            "time point before the earliest representable slot"
                        $ cover 10 aboveMax
                            "time point after the latest representable slot"
                        $ cover 10 (not belowMin && not aboveMax)
                            "time point during the lifetime of the blockchain"
                    True

    describe "Epoch arithmetic: predecessors and successors" $ do

        let succN n = applyN n (epochSucc =<<)
        let predN n = applyN n (epochPred =<<)

        it "epochPred minBound == Nothing" $
            epochPred minBound === Nothing

        it "epochSucc maxBound == Nothing" $
            epochSucc maxBound === Nothing

        it "(applyN n epochSucc) . (applyN n epochPred) == id" $
            withMaxSuccess 1000 $ property $
                \(Small epochWord) (Small n) ->
                    let epoch = EpochNo epochWord
                        withinBounds = minBound + n <= unEpochNo epoch
                        expectedResult =
                            if withinBounds then Just epoch else Nothing
                    in
                    checkCoverage $
                    cover 10      withinBounds  "within bounds" $
                    cover 10 (not withinBounds) "out of bounds" $
                    expectedResult === succN n (predN n $ Just epoch)

        it "(applyN n epochPred) . (applyN n epochSucc) == id" $
            withMaxSuccess 1000 $ property $
                \(Small epochWord) (Small n) ->
                    let epoch = EpochNo $ maxBound - epochWord
                        withinBounds = maxBound - n >= unEpochNo epoch
                        expectedResult =
                            if withinBounds then Just epoch else Nothing
                    in
                    checkCoverage $
                    cover 10      withinBounds  "within bounds" $
                    cover 10 (not withinBounds) "out of bounds" $
                    expectedResult === predN n (succN n $ Just epoch)

    describe "Slot arithmetic" $ do

        it "slotFloor (slotStartTime slotMinBound) == Just slotMinBound" $
            withMaxSuccess 1000 $ property $ \sps ->
                slotFloor sps (slotStartTime sps slotMinBound)
                    === Just slotMinBound

        it "slotFloor (utcTimePred (slotStartTime slotMinBound)) == Nothing" $
            withMaxSuccess 1000 $ property $ \sps ->
                slotFloor sps (utcTimePred (slotStartTime sps slotMinBound))
                    === Nothing

        it "t < slotStartTime slotMinBound => slotFloor t == Nothing" $
            withMaxSuccess 1000 $ property $ \sps t ->
                StartTime (getUniformTime t) < getGenesisBlockDate sps ==>
                    slotFloor sps (getUniformTime t) === Nothing

        it "t < slotStartTime slotMinBound => slotCeiling t == slotMinBound" $
            withMaxSuccess 1000 $ property $ \sps t ->
                StartTime (getUniformTime t) < getGenesisBlockDate sps ==>
                    slotCeiling sps (getUniformTime t) === slotMinBound

        it "slotStartTime slotMinBound `isAfterRange` r => \
            \isNothing (slotRangeFromTimeRange r)" $
            withMaxSuccess 1000 $ property $ \sps r -> do
                let r' = getUniformTime <$> r
                slotStartTime sps slotMinBound `isAfterRange` r' ==>
                    isNothing (slotRangeFromTimeRange sps r')

        it "applyN (flatSlot slot) slotPred slot == Just slotMinBound" $
            withMaxSuccess 10 $ property $
                \(sps, slot) -> do
                    let n = flatSlot (getEpochLength sps) slot
                    Just slotMinBound ===
                        applyN n (slotPred sps =<<) (Just slot)

        it "applyN (flatSlot slot + 1) slotPred slot == Nothing" $
            withMaxSuccess 10 $ property $
                \(sps, slot) -> do
                    let n = flatSlot (getEpochLength sps) slot + 1
                    Nothing === applyN n (slotPred sps =<<) (Just slot)

        it "(applyN n slotSucc) . (applyN n slotPred) == id" $
            withMaxSuccess 1000 $ property $
                \(sps, slot) (NonNegative (n :: Int)) ->
                    flatSlot (getEpochLength sps) slot >= fromIntegral n ==> do
                    let s = applyN n (slotSucc sps <$>)
                    let p = applyN n (slotPred sps =<<)
                    Just slot === (s . p) (Just slot)

        it "(applyN n slotPred) . (applyN n slotSucc) == id" $
            withMaxSuccess 1000 $ property $
                \(sps, slot) (NonNegative (n :: Int)) -> do
                    let s = applyN n (slotSucc sps <$>)
                    let p = applyN n (slotPred sps =<<)
                    Just slot === (p . s) (Just slot)

        it "slotDifference (applyN n slotSucc slot) slot == \
            \n (valid difference)" $
            withMaxSuccess 1000 $ property $
                \(sps, slot) (NonNegative (n :: Int)) ->
                    Quantity (fromIntegral n) ===
                        slotDifference sps (applyN n (slotSucc sps) slot) slot

        it "slotDifference slot (applyN n slotPred slot) == \
            \n (valid difference)" $
            withMaxSuccess 1000 $ property $
                \(sps, slot) (NonNegative (n :: Int)) ->
                    flatSlot (getEpochLength sps) slot >= fromIntegral n ==>
                    Just (Quantity (fromIntegral n)) ===
                        (slotDifference sps slot <$>
                            applyN n (slotPred sps =<<) (Just slot))

        it "slotDifference (applyN n slotPred slot) slot == \
            \0 (invalid difference)" $
            withMaxSuccess 1000 $ property $
                \(sps, slot) (NonNegative (n :: Int)) ->
                    flatSlot (getEpochLength sps) slot >= fromIntegral n ==>
                    Just (Quantity 0) ===
                        (flip (slotDifference sps) slot <$>
                            applyN n (slotPred sps =<<) (Just slot))

        it "slotDifference slot (applyN n slotSucc slot) == \
            \0 (invalid difference)" $
            withMaxSuccess 1000 $ property $
                \(sps, slot) (NonNegative (n :: Int)) ->
                    Quantity 0 ===
                        slotDifference sps slot (applyN n (slotSucc sps) slot)

        it "slotAt . slotStartTime == id" $
            withMaxSuccess 1000 $ property $
                \(sps, slot) -> do
                    let f = slotAt sps . slotStartTime sps
                    Just slot === f slot

        it "slotCeiling . slotStartTime == id" $
            withMaxSuccess 1000 $ property $
                \(sps, slot) -> do
                    let f = slotCeiling sps . slotStartTime sps
                    slot === f slot

        it "slotFloor . slotStartTime == id" $
            withMaxSuccess 1000 $ property $
                \(sps, slot) -> do
                    let f = slotFloor sps . slotStartTime sps
                    Just slot === f slot

        it "slot > slotMinBound => \
            \slotSucc . slotFloor . utcTimePred . slotStartTime == id" $
            withMaxSuccess 1000 $ property $
                \(sps, slot) -> slot > slotMinBound ==> do
                    let f = fmap (slotSucc sps)
                            . slotFloor sps
                            . utcTimePred
                            . slotStartTime sps
                    Just slot === f slot

        it "slotPred . slotCeiling . utcTimeSucc . slotStartTime == id" $
            withMaxSuccess 1000 $ property $
                \(sps, slot) -> do
                    let f = slotPred sps
                            . slotCeiling sps
                            . utcTimeSucc
                            . slotStartTime sps
                    Just slot === f slot

        it "slotRangeFromTimeRange is maximal" $
            property $ \(sps, uniformTimeRange) ->

                let timeRange :: Range UTCTime
                    timeRange = getUniformTime <$> uniformTimeRange

                    maybeSlotRange :: Maybe (Range SlotId)
                    maybeSlotRange = slotRangeFromTimeRange sps timeRange

                    startsWithin :: Range SlotId -> Range UTCTime -> Bool
                    startsWithin sr tr =
                        (`isSubrangeOf` tr) $ fmap (slotStartTime sps) sr

                    lowerBoundPred = mapRangeLowerBound slotPred'
                    upperBoundSucc = mapRangeUpperBound slotSucc'

                    slotPred' :: SlotId -> SlotId
                    slotPred' s = fromMaybe slotMinBound $ slotPred sps s

                    slotSucc' :: SlotId -> SlotId
                    slotSucc' = slotSucc sps
                in
                checkCoverage $
                cover 20 (isNothing maybeSlotRange)
                    "have no slot range" $
                cover 50 (isJust maybeSlotRange)
                    "have slot range" $
                cover 20 (fmap rangeHasLowerBound maybeSlotRange == Just True)
                    "slot range has lower bound" $
                cover 20 (fmap rangeHasUpperBound maybeSlotRange == Just True)
                    "slot range has upper bound" $
                cover 20 (fmap rangeIsFinite maybeSlotRange == Just True)
                    "slot range is finite" $

                case maybeSlotRange of
                    Nothing ->
                        (Just True ===
                            ((< getGenesisBlockDate sps) . StartTime
                                <$> inclusiveUpperBound timeRange))
                    Just slotRange ->
                        -- Rule 1: Slot range is within specified time range:
                        (slotRange `startsWithin` timeRange)
                        .&&.
                        -- Rule 2: Slot range lower bound is minimal:
                        (not (lowerBoundPred slotRange `startsWithin` timeRange)
                            -- Exceptions to the rule:
                            || not (rangeHasLowerBound slotRange)
                            || lowerBoundPred slotRange == slotRange)
                        .&&.
                        -- Rule 3: Slot range upper bound is maximal:
                        (not (upperBoundSucc slotRange `startsWithin` timeRange)
                            -- Exceptions to the rule:
                            || not (rangeHasUpperBound slotRange)
                            || upperBoundSucc slotRange == slotRange)

        it ("`slotStartTime . slotRangeFromTimeRange` is idempotent") $
            withMaxSuccess 1000 $ property $
                \sps timeRange -> do
                    let f = fmap (fmap (slotStartTime sps))
                            . slotRangeFromTimeRange sps
                    let r = getUniformTime <$> timeRange
                    checkCoverage $
                        cover 20 (isJust $ f r)
                            "`slotRangeFromTimeRange` yielded a slot range" $
                        cover 20 (isNothing $ f r)
                            "`slotRangeFromTimeRange` yielded nothing" $
                        (f =<< f r) === f r

    describe "Negative cases for types decoding" $ do
        it "fail fromText @SyncTolerance \"patate\"" $ do
            let err = "Cannot parse given time duration. Here are a few \
                    \examples of valid text representing a sync tolerance: \
                    \'3s', '3600s', '42s'."
            fromText @SyncTolerance "patate" === Left (TextDecodingError err)
        it "fail fromText @AddressState \"unusedused\"" $ do
            let err = "Unable to decode the given value: \"unusedused\".\
                    \ Please specify one of the following values: used, unused."
            fromText @AddressState "unusedused" === Left (TextDecodingError err)
        it "fail fromText @WalletName \"\"" $ do
            let err = "name is too short: expected at least "
                      <> show walletNameMinLength <> " character"
            fromText @WalletName "" === Left (TextDecodingError err)
        it "fail fromText @WalletName > walletNameMaxLength" $ do
            let err = "name is too long: expected at most "
                      <> show walletNameMaxLength <> " characters"
            let walName = T.pack (replicate (walletNameMaxLength + 1) 'x')
            fromText @WalletName walName === Left (TextDecodingError err)
        it "fail fromText @WalletId \"101\"" $ do
            let err = "wallet id should be a hex-encoded string \
                      \of 40 characters"
            fromText @WalletId "101" === Left (TextDecodingError err)
        it "fail fromText (@Hash \"Tx\")" $ do
            let err =
                    "Invalid tx hash: \
                    \expecting a hex-encoded value that is 32 bytes in length."
            fromText @(Hash "Tx") "----" === Left (TextDecodingError err)
        it "fail fromText (@Hash \"Genesis\")" $ do
            let err = "Invalid genesis hash: \
                    \expecting a hex-encoded value that is 32 bytes in length."
            fromText @(Hash "Genesis") "----" === Left (TextDecodingError err)
        it "fail fromText (@Hash \"Block\")" $ do
            let err = "Invalid block hash: \
                    \expecting a hex-encoded value that is 32 bytes in length."
            fromText @(Hash "Block") "----" === Left (TextDecodingError err)
        it "fail fromText (@Hash \"BlockHeader\")" $ do
            let err = "Invalid blockHeader hash: \
                    \expecting a hex-encoded value that is 32 bytes in length."
            fromText @(Hash "BlockHeader") "----" === Left (TextDecodingError err)
        it "Invalid account IDs cannot be decoded from text" $ do
            let expectedErrorMessage =
                    "Invalid account hash: \
                    \expecting a hex-encoded value that is 32 bytes in length."
            forM_ invalidAccountIdTexts $ \text ->
                toText <$> fromText @(Hash "Account") text
                    `shouldBe` Left (TextDecodingError expectedErrorMessage)

        let invalidFeePolicyTexts =
                [ ""
                , " "
                , "1"
                , "1x"
                , "1 + 1"
                , "1x + 1"
                , "1+1x"
                , "1 +1x"
                , "1+ 1x"
                , "1+ 1x + 1y"
                , "1 +1x + 1y"
                , "1 + 1x+ 1y"
                , "1 + 1x +1y"
                , "xxxx"
                , "1 + 6667y + 12x"
                , "a + bx + cy"
                , "dasd + asdax + dadsy"
                ]
        forM_ invalidFeePolicyTexts $ \policyText ->
            it ("fail fromText @FeePolicy " <> show policyText) $ do
                let err =
                        "Unable to decode FeePolicy: \
                        \Linear equation not in expected format: a + bx + cy \
                        \where 'a', 'b', and 'c' are numbers"
                fromText @FeePolicy policyText === Left (TextDecodingError err)

        let correctPolicyTexts =
                [ "1 + 6667x + 12y"
                , "1.12 + 1.4324x + 3.14159265359y"
                , "1 + 0x + 0y"
                , "-13 + 3.14159265359x + 1y"
                , "-3.14159265359 + -  1 x + - 1 y"
                , "1     +      11    x +  1  y"
                ]
        forM_ correctPolicyTexts $ \policyText ->
            it ("correct fromText @FeePolicy " <> show policyText) $ do
                fromText @FeePolicy policyText `shouldSatisfy` isRight

        let poolOwnerTests =
                [ ( "Invalid bech32"
                  , "hello"
                  , "Stake pool owner is not a valid bech32 string: "
                    <> "StringToDecodeTooShort" )
                , ( "Wrong HRP"
                  , "split1checkupstagehandshakeupstreamerranterredcaperred2y9e3w"
                  , "Stake pool owner has wrong prefix: expected ed25519_pk "
                    <> "but got HumanReadablePart \"split\"" )
                ]

        forM_ poolOwnerTests $ \(title, str, msg) ->
            it ("fail fromText @PoolOwner " ++ title) $
                fromText @PoolOwner str `shouldBe` (Left $ TextDecodingError msg)

    describe "unsafeEpochNo" $ do
        it "returns a successful result for any Word31" $
            property prop_unsafeEpochNoValid
        it "throws an exception for any value bigger than a Word31" $
            property prop_unsafeEpochNoThrows

    describe "Lemma 2.1 - Properties of UTxO operations" $ do
        it "2.1.1) ins⊲ u ⊆ u"
            (checkCoverage prop_2_1_1)
        it "2.1.2) ins⋪ u ⊆ u"
            (checkCoverage prop_2_1_2)
        it "2.1.3) u ⊳ outs ⊆ u"
            (checkCoverage prop_2_1_3)
        it "2.1.4) ins⊲ (u ⋃ v) = (ins⊲ u) ⋃ (ins⊲ v)"
            (checkCoverage prop_2_1_4)
        it "2.1.5) ins⋪ (u ⋃ v) = (ins⋪ u) ⋃ (ins⋪ v)"
            (checkCoverage prop_2_1_5)
        it "2.1.6) (dom u ⋂ ins) ⊲ u = ins⊲ u"
            (checkCoverage prop_2_1_6)
        it "2.1.7) (dom u ⋂ ins) ⋪ u = ins⋪ u"
            (checkCoverage prop_2_1_7)
        it "2.1.8) (dom u ⋃ ins) ⋪ (u ⋃ v) = (ins ⋃ dom u) ⋪ v"
            (checkCoverage prop_2_1_8)
        it "2.1.9) ins⋪ u = (dom u \\ ins)⊲ u"
            (checkCoverage prop_2_1_9)

    describe "Lemma 2.6 - Properties of balance" $ do
        it "2.6.1) dom u ⋂ dom v ==> balance (u ⋃ v) = balance u + balance v"
            (checkCoverage prop_2_6_1)
        it "2.6.2) balance (ins⋪ u) = balance u - balance (ins⊲ u)"
            (checkCoverage prop_2_6_2)

    describe "Slotting ordering" $ do
        it "Any Slot >= slotMinBound"
            (property (>= slotMinBound))
        it "SlotId 1 2 < SlotId 2 1"
            (property $ SlotId { epochNumber = 1, slotNumber = 2 } < SlotId 2 1)
        it "SlotId 1 1 < SlotId 1 2"
            (property $ SlotId { epochNumber = 1, slotNumber = 1 } < SlotId 1 2)
        it "SlotId 1 2 < SlotId 2 2"
            (property $ SlotId { epochNumber = 1, slotNumber = 2 } < SlotId 2 2)

    describe "UtxoStatistics" $ do
        it "total statistics == balance utxo"
            (checkCoverage propUtxoTotalIsBalance)
        it "sum of weighted distribution >= total balance"
            (checkCoverage propUtxoSumDistribution)
        it "distribution == empty <=> UTxO == empty"
            (checkCoverage propUtxoEmptyIsEmpty)
        it "sum of the distribution coeffs == sizeOf UTxO"
            (checkCoverage propUtxoWeightsEqualSize)

{-------------------------------------------------------------------------------
       Wallet Specification - Lemma 2.1 - Properties of UTxO operations
-------------------------------------------------------------------------------}

prop_2_1_1 :: (Set TxIn, UTxO) -> Property
prop_2_1_1 (ins, u) =
    cover 50 cond "dom u ⋂ ins ≠ ∅" (property prop)
  where
    cond = not $ Set.null $ dom u `Set.intersection` ins
    prop = (u `restrictedBy` ins) `isSubsetOf` u

prop_2_1_2 :: (Set TxIn, UTxO) -> Property
prop_2_1_2 (ins, u) =
    cover 50 cond "dom u ⋂ ins ≠ ∅" (property prop)
  where
    cond = not $ Set.null $ dom u `Set.intersection` ins
    prop = (u `excluding` ins) `isSubsetOf` u

prop_2_1_3 :: (Set TxOut, UTxO) -> Property
prop_2_1_3 (outs, u) =
    cover 50 cond "u ⋂ outs ≠ ∅" (property prop)
  where
    cond = not $ Set.null $
        Set.fromList (Map.elems (getUTxO u)) `Set.intersection` outs
    prop = (u `restrictedTo` outs) `isSubsetOf` u

prop_2_1_4 :: (Set TxIn, UTxO, UTxO) -> Property
prop_2_1_4 (ins, u, v) =
    cover 50 cond "(dom u ⋃ dom v) ⋂ ins ≠ ∅" (property prop)
  where
    cond = not $ Set.null $ Set.union (dom u) (dom v) `Set.intersection` ins
    prop =
        ((u <> v) `restrictedBy` ins)
            ===
        (u `restrictedBy` ins) <> (v `restrictedBy` ins)

prop_2_1_5 :: (Set TxIn, UTxO, UTxO) -> Property
prop_2_1_5 (ins, u, v) =
    cover 50 cond "(dom u ⋃ dom v) ⋂ ins ≠ ∅" (property prop)
  where
    cond = not $ Set.null $ Set.union (dom u) (dom v) `Set.intersection` ins
    prop =
        ((u <> v) `excluding` ins)
            ===
        (u `excluding` ins) <> (v `excluding` ins)

prop_2_1_6 :: (Set TxIn, UTxO) -> Property
prop_2_1_6 (ins, u) =
    cover 50 cond "dom u ⋂ ins ≠ ∅" (property prop)
  where
    cond = not $ Set.null $ dom u `Set.intersection` ins
    prop =
        (u `restrictedBy` (dom u `Set.intersection` ins))
            ===
        (u `restrictedBy` ins)

prop_2_1_7 :: (Set TxIn, UTxO) -> Property
prop_2_1_7 (ins, u) =
    cover 50 cond "dom u ⋂ ins ≠ ∅" (property prop)
  where
    cond = not $ Set.null $ dom u `Set.intersection` ins
    prop =
        (u `excluding` (dom u `Set.intersection` ins))
            ===
        (u `excluding` ins)

prop_2_1_8 :: (Set TxIn, UTxO, UTxO) -> Property
prop_2_1_8 (ins, u, v) =
    cover 50 cond "dom u ⋂ ins ≠ ∅" (property prop)
  where
    cond = not $ Set.null $ dom u `Set.intersection` ins
    prop =
        ((u <> v) `excluding` (dom u <> ins))
            ===
        v `excluding` (ins <> dom u)

prop_2_1_9 :: (Set TxIn, UTxO) -> Property
prop_2_1_9 (ins, u) =
    cover 50 cond "dom u ⋂ ins ≠ ∅" (property prop)
  where
    cond = not $ Set.null $ dom u `Set.intersection` ins
    prop = (u `excluding` ins) === u `restrictedBy` (dom u \\ ins)


{-------------------------------------------------------------------------------
       Wallet Specification - Lemma 2.6 - Properties of Balance
-------------------------------------------------------------------------------}

prop_2_6_1 :: (UTxO, UTxO) -> Property
prop_2_6_1 (u, v) =
    cover 50 cond "u ≠ ∅ , v ≠ ∅" (property prop)
  where
    -- NOTE:
    -- A precondition (u ⋂ v = ∅ ) is hard to satisfy because our generators
    -- are built in order to not be 'too entropic'. So, we better just create
    -- a v' that has no overlap with u.
    v' = v `excluding` dom u
    cond = not (u `isSubsetOf` mempty || v' `isSubsetOf` mempty)
    prop = balance (u <> v') === balance u + balance v'

prop_2_6_2 :: (Set TxIn, UTxO) -> Property
prop_2_6_2 (ins, u) =
    cover 50 cond "dom u ⋂ ins ≠ ∅" (property prop)
  where
    cond = not $ Set.null $ dom u `Set.intersection` ins
    prop =
        balance (u `excluding` ins)
            ===
        balance u - balance (u `restrictedBy` ins)

{-------------------------------------------------------------------------------
                        UTxO statistics Properties
-------------------------------------------------------------------------------}

-- | The 'total' stake in the statistics is the UTxO's balance
propUtxoTotalIsBalance
    :: BoundType
    -> ShowFmt UTxO
    -> Property
propUtxoTotalIsBalance bType (ShowFmt utxo) =
    fromIntegral totalStake == balance utxo
    & cover 75 (utxo /= mempty) "UTxO /= empty"
  where
    UTxOStatistics _ totalStake _ = computeUtxoStatistics bType utxo

-- | The sum of the weighted distribution is greater than the sum of UTxOs
-- outputs (we distribute the UtxO over buckets with upper-bounds, so everything
-- in a bucket is lower than its upper-bound).
propUtxoSumDistribution
    :: BoundType
    -> ShowFmt UTxO
    -> Property
propUtxoSumDistribution bType (ShowFmt utxo) =
    sum (upperVal <$> bars) >= fromIntegral (balance utxo)
    & cover 75 (utxo /= mempty) "UTxO /= empty"
    & counterexample ("Histogram: " <> pretty bars)
  where
    UTxOStatistics bars _ _ = computeUtxoStatistics bType utxo
    upperVal (HistogramBar k v) = k * v

-- | The distribution is empty if and only if the UTxO is empty
propUtxoEmptyIsEmpty
    :: BoundType
    -> ShowFmt UTxO
    -> Property
propUtxoEmptyIsEmpty bType (ShowFmt utxo) =
    if all isEmpty bars then utxo === mempty else utxo =/= mempty
    & cover 75 (utxo /= mempty) "UTxO /= empty"
    & counterexample ("Histogram: " <> pretty bars)
  where
    UTxOStatistics bars _ _ = computeUtxoStatistics bType utxo
    isEmpty (HistogramBar _ v) = v == 0

-- | The sum of the distribution coefficients should is equal to the number of
-- UTxO entries
propUtxoWeightsEqualSize
    :: BoundType
    -> ShowFmt UTxO
    -> Property
propUtxoWeightsEqualSize bType (ShowFmt utxo) =
    sum (histElems bars) === fromIntegral (Map.size $ getUTxO utxo)
    & cover 75 (utxo /= mempty) "UTxO /= empty"
    & counterexample ("Coefficients: " <> pretty (histElems bars))
  where
    UTxOStatistics bars _ _ = computeUtxoStatistics bType utxo
    histElems = fmap $ \(HistogramBar _ v) -> v

{-------------------------------------------------------------------------------
                            unsafeEpochNo Properties
-------------------------------------------------------------------------------}

prop_unsafeEpochNoValid :: Word31 -> Property
prop_unsafeEpochNoValid ep =
    unsafeEpochNo (fromIntegral ep) === EpochNo ep

prop_unsafeEpochNoThrows :: Word32 -> Property
prop_unsafeEpochNoThrows ep
    | ep > maxEpochNo = prop ep
    | otherwise = prop (ep + maxEpochNo + 1)
  where
    maxEpochNo :: Word32
    maxEpochNo = fromIntegral $ unEpochNo maxBound

    prop :: Word32 -> Property
    prop ep' = monadicIO $ run $
        evaluate (unsafeEpochNo ep') `shouldThrow` anyErrorCall

{-------------------------------------------------------------------------------
                            Arbitrary Instances

    Arbitrary instances define here aren't necessarily reflecting on real-life
    scenario, but they help test the property above by constructing data
    structures that don't have much entropy and therefore, allow us to even test
    something when checking for intersections and set restrictions!
-------------------------------------------------------------------------------}

deriving instance Arbitrary a => Arbitrary (ShowFmt a)

instance Arbitrary Direction where
    arbitrary = arbitraryBoundedEnum
    shrink = genericShrink

instance Arbitrary FeePolicy where
    arbitrary = do
        NonNegative a <- arbitrary
        NonNegative b <- arbitrary
        NonNegative c <- arbitrary
        return $ LinearFee (Quantity a) (Quantity b) (Quantity c)
    shrink (LinearFee (Quantity a) (Quantity b) (Quantity c)) =
        f <$> shrink (a, b, c)
      where
        f (x, y, z) = LinearFee (Quantity x) (Quantity y) (Quantity z)

instance Arbitrary (Hash "Genesis") where
    arbitrary = Hash . BS.pack <$> vectorOf 32 arbitrary

instance Arbitrary (Hash "Block") where
    arbitrary = Hash . BS.pack <$> vectorOf 32 arbitrary

instance Arbitrary (Hash "Account") where
    arbitrary = Hash . BS.pack <$> vectorOf 32 arbitrary

instance Arbitrary (Hash "BlockHeader") where
    arbitrary = Hash . BS.pack <$> vectorOf 32 arbitrary

instance Arbitrary (Hash "Tx") where
    -- No Shrinking
    arbitrary = oneof
        [ pure $ Hash $ unsafeFromHex
            "0000000000000000000000000000000000000000000000000000000000000001"
        , pure $ Hash $ unsafeFromHex
            "0000000000000000000000000000000000000000000000000000000000000002"
        , pure $ Hash $ unsafeFromHex
            "0000000000000000000000000000000000000000000000000000000000000003"
        ]

-- Same for addresses
instance Arbitrary Address where
    -- No Shrinking
    arbitrary = oneof
        [ pure $ Address "ADDR01"
        , pure $ Address "ADDR02"
        , pure $ Address "ADDR03"
        ]

instance Arbitrary AddressState where
    shrink = genericShrink
    arbitrary = genericArbitrary

instance Arbitrary Coin where
    -- No Shrinking
    arbitrary = Coin <$> choose (0, 3)

instance (Arbitrary a, Ord a) => Arbitrary (Range a) where
    arbitrary =
        makeRangeValid . uncurry Range <$> arbitrary
    shrink (Range p q) =
        makeRangeValid . uncurry Range <$> shrink (p, q)

-- Ensures that the start of a range is not greater than its end.
makeRangeValid :: Ord a => Range a -> Range a
makeRangeValid = \case
    Range (Just p) (Just q) -> Range (Just $ min p q) (Just $ max p q)
    r -> r

-- A range that contains more than a single element.
newtype NonSingletonRange a = NonSingletonRange
    { getNonSingletonRange :: Range a
    } deriving Show

instance (Arbitrary a, Ord a) => Arbitrary (NonSingletonRange a) where
    arbitrary = do
        -- Iterate through the infinite list of arbitrary ranges and return
        -- the first range that is not a singleton range:
        ranges <- infiniteList
        pure $ head $ catMaybes $
            makeNonSingletonRangeValid . NonSingletonRange <$> ranges
    shrink (NonSingletonRange r) = catMaybes $
        makeNonSingletonRangeValid . NonSingletonRange <$> shrink r

-- Ensures that a range is not a singleton range.
makeNonSingletonRangeValid
    :: Ord a => NonSingletonRange a -> Maybe (NonSingletonRange a)
makeNonSingletonRangeValid (NonSingletonRange r)
    | rangeIsSingleton r = Nothing
    | otherwise = Just $ NonSingletonRange $ makeRangeValid r

instance Arbitrary TxOut where
    -- No Shrinking
    arbitrary = TxOut
        <$> arbitrary
        <*> arbitrary

instance Arbitrary TxIn where
    -- No Shrinking
    arbitrary = TxIn
        <$> arbitrary
        <*> scale (`mod` 3) arbitrary -- No need for a crazy high indexes

instance Arbitrary TxStatus where
    arbitrary = arbitraryBoundedEnum
    shrink = genericShrink

instance Arbitrary UTxO where
    shrink (UTxO utxo) = UTxO <$> shrink utxo
    arbitrary = do
        n <- choose (0, 10)
        utxo <- zip
            <$> vectorOf n arbitrary
            <*> vectorOf n arbitrary
        return $ UTxO $ Map.fromList utxo

instance Arbitrary Tx where
    shrink (Tx tid ins outs) =
        (flip (Tx tid) outs <$> shrink ins) <> ((Tx tid) ins <$> shrink outs)
    arbitrary = do
        ins <- choose (1, 3) >>= flip vectorOf arbitrary
        outs <- choose (1, 3) >>= flip vectorOf arbitrary
        tid <- genHash
        return $ Tx tid ins outs
      where
        genHash = oneof
          [ pure $ Hash "Tx1"
          , pure $ Hash "Tx2"
          , pure $ Hash "Tx3"
          ]

instance Arbitrary BlockHeader where
    -- No Shrinking
    arbitrary = do
        sl <- arbitrary
        BlockHeader sl (mockBlockHeight sl) <$> genHash <*> genHash
      where
        mockBlockHeight :: SlotId -> Quantity "block" Word32
        mockBlockHeight = Quantity . fromIntegral . flatSlot (EpochLength 200)

        genHash = oneof
            [ pure $ Hash "BLOCK01"
            , pure $ Hash "BLOCK02"
            , pure $ Hash "BLOCK03"
            ]

instance Arbitrary EpochNo where
    arbitrary = EpochNo <$> oneof
        [ pure minBound
        , pure maxBound
        , pure $ succ minBound
        , pure $ pred maxBound
        , closeToMinBound
        , closeToMaxBound
        , arbitraryBoundedIntegral
        ]
      where
        closeToMinBound =                getSmall <$> arbitrary
        closeToMaxBound = (maxBound -) . getSmall <$> arbitrary
    shrink = genericShrink

instance Arbitrary SlotId where
    shrink _ = []
    arbitrary = do
        ep <- choose (0, 10)
        sl <- choose (0, 100)
        return (SlotId (unsafeEpochNo ep) (SlotNo sl))

instance Arbitrary Block where
    shrink (Block h txs _) = Block h <$> shrink txs <*> pure []
    arbitrary = do
        txs <- choose (0, 500) >>= flip vectorOf arbitrary
        Block <$> arbitrary <*> pure txs <*> pure []

instance Arbitrary WalletId where
    shrink _ = []
    arbitrary = do
        bytes <- BS.pack <$> replicateM 16 arbitrary
        return $ WalletId (hash bytes)

instance Arbitrary WalletName where
    arbitrary = do
        nameLength <- choose (walletNameMinLength, walletNameMaxLength)
        WalletName . T.pack <$> replicateM nameLength arbitraryPrintableChar
    shrink (WalletName t)
        | T.length t <= walletNameMinLength = []
        | otherwise = [WalletName $ T.take walletNameMinLength t]

instance Arbitrary BoundType where
    shrink = genericShrink
    arbitrary = genericArbitrary

instance Arbitrary SlotLength where
    shrink (SlotLength t) =
        map (SlotLength . fromIntegral)
        $ filter (> 0)
        $ shrink (floor t :: Int)
    arbitrary =
        SlotLength . fromIntegral <$> choose (1 :: Int, 100)

instance Arbitrary StartTime where
    arbitrary = StartTime <$> genUniformTime

instance Arbitrary EpochLength where
    arbitrary = EpochLength . getNonZero <$> arbitrary

instance Arbitrary ActiveSlotCoefficient where
    shrink (ActiveSlotCoefficient f)
        | f < 1 = [1]
        | otherwise = []
    arbitrary =
        ActiveSlotCoefficient <$> choose (0.001, 1.0)

instance Arbitrary SlotParameters where
    arbitrary = SlotParameters
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
    shrink = genericShrink

instance Arbitrary SyncTolerance where
    arbitrary = mkSyncTolerance <$> choose (1, 10000)

instance {-# OVERLAPS #-} Arbitrary (SlotParameters, SlotId) where
    arbitrary = do
        (el, slot) <- arbitrary
        sl <- arbitrary
        st <- arbitrary
        f <- arbitrary
        pure (SlotParameters el sl st f, slot)
    shrink (SlotParameters el sl st f, slot) = do
        (el', slot') <- shrink (el, slot)
        pure (SlotParameters el' sl st f, slot')

-- | Combines a 'SlotParameters' object and a single point in time.
--
-- The point in time falls into one of the following categories:
--
-- 1. occurs during the lifetime of the blockchain;
-- 2. occurs before the earliest representable slot;
-- 3. occurs after the latest representable slot.
--
data SlotParametersAndTimePoint = SlotParametersAndTimePoint
    { getSlotParameters :: SlotParameters
    , getTimePoint :: UTCTime
    } deriving (Eq, Show)

instance Arbitrary SlotParametersAndTimePoint where
    arbitrary = do
        sps <- arbitrary
        let timeA = 0
        let timeB = toModifiedJulianDay $ utctDay $ epochStartTime sps minBound
        let timeC = toModifiedJulianDay $ utctDay $ epochStartTime sps maxBound
        let timeD = timeC * 2
        (lowerBound, upperBound) <- oneof $ fmap pure
            [ (timeA, timeB)
            , (timeB, timeC)
            , (timeC, timeD)
            ]
        time <- genUniformTimeWithinRange
            (ModifiedJulianDay lowerBound)
            (ModifiedJulianDay upperBound)
        pure $ SlotParametersAndTimePoint sps time

-- | Note, for functions which works with both an epoch length and a slot id,
-- we need to make sure that the 'slotNumber' doesn't exceed the epoch length,
-- otherwise, all computations get mixed up.
instance {-# OVERLAPS #-} Arbitrary (EpochLength, SlotId) where
    shrink (a,b) =
        filter validSlotConfig $ zip (shrink a) (shrink b)
      where
        validSlotConfig (EpochLength ep, SlotId _ (SlotNo sl)) = sl < ep

    arbitrary = do
        (EpochLength epochLength) <- arbitrary
        ep <- unsafeEpochNo <$> choose (0, 1000)
        sl <- SlotNo <$> choose (0, fromIntegral epochLength - 1)
        return (EpochLength epochLength, SlotId ep sl)

instance Arbitrary Word31 where
    arbitrary = arbitrarySizedBoundedIntegral
    shrink = shrinkIntegral

instance Arbitrary PoolOwner where
    arbitrary = PoolOwner . BS.pack <$> vectorOf 32 arbitrary

{-------------------------------------------------------------------------------
                                  Test data
-------------------------------------------------------------------------------}

testAccountIdTexts :: [Text]
testAccountIdTexts =
    [ testAccountIdText1
    , testAccountIdText2
    , testAccountIdText3
    ]

invalidAccountIdTexts :: [Text]
invalidAccountIdTexts =
    [ ""
    , "a"
    , "0123456789abcdef"
    ]

testAccountIdText1 :: Text
testAccountIdText1 =
    "addf8bd48558b72b257408a0164c8722058b4d5337134ab9a02bc4e64194933a"

testAccountIdText2 :: Text
testAccountIdText2 =
    "c0bd85194eeff70ddfdd4f6302b1b86c69b0474e48a97f78cd3f9ec7669c2c90"

testAccountIdText3 :: Text
testAccountIdText3 =
    "853296463f54371de809799ed7cbde26d6791b51d842f61aedb2c2454a7d7a07"
