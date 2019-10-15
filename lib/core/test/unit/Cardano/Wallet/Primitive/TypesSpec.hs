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

import Cardano.Wallet.DummyTarget.Primitive.Types
    ( Tx (..) )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..), Passphrase (..), WalletKey (..), XPrv, digest, publicKey )
import Cardano.Wallet.Primitive.AddressDerivation.Sequential
    ( SeqKey (..), generateKeyFromSeed )
import Cardano.Wallet.Primitive.Types
    ( Address (..)
    , AddressState (..)
    , Block (..)
    , BlockHeader (..)
    , BoundType
    , Coin (..)
    , Direction (..)
    , Dom (..)
    , EpochLength (..)
    , EpochNo (..)
    , Hash (..)
    , HistogramBar (..)
    , Range (..)
    , RangeBound (..)
    , ShowFmt (..)
    , SlotId (..)
    , SlotLength (..)
    , SlotNo (..)
    , SlotParameters (..)
    , StartTime (..)
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
    , walletNameMaxLength
    , walletNameMinLength
    , wholeRange
    )
import Control.DeepSeq
    ( deepseq )
import Control.Monad
    ( replicateM )
import Crypto.Hash
    ( hash )
import Data.ByteString
    ( ByteString )
import Data.Function
    ( (&) )
import Data.Function.Utils
    ( applyN )
import Data.Maybe
    ( fromMaybe, isJust, isNothing )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Quantity (..) )
import Data.Set
    ( Set, (\\) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( TextDecodingError (..), fromText )
import Data.Time
    ( UTCTime )
import Data.Time.Utils
    ( utcTimePred, utcTimeSucc )
import Data.Word
    ( Word32 )
import Fmt
    ( pretty )
import Test.Hspec
    ( Spec, describe, it, shouldNotSatisfy, shouldSatisfy )
import Test.QuickCheck
    ( Arbitrary (..)
    , NonNegative (..)
    , NonZero (..)
    , Property
    , arbitraryBoundedEnum
    , arbitraryPrintableChar
    , checkCoverage
    , choose
    , counterexample
    , cover
    , oneof
    , property
    , scale
    , vectorOf
    , withMaxSuccess
    , (.&&.)
    , (=/=)
    , (===)
    , (==>)
    )
import Test.QuickCheck.Arbitrary.Generic
    ( genericArbitrary, genericShrink )
import Test.Text.Roundtrip
    ( textRoundtrip )
import Test.Utils.Time
    ( genUniformTime, getUniformTime )

import qualified Data.ByteArray as BA
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
        textRoundtrip $ Proxy @TxStatus
        textRoundtrip $ Proxy @WalletName
        textRoundtrip $ Proxy @WalletId
        textRoundtrip $ Proxy @(Hash "Genesis")
        textRoundtrip $ Proxy @(Hash "Tx")

    describe "Buildable" $ do
        it "WalletId" $ do
            let seed = Passphrase (BA.convert @ByteString "0000000000000000")
            let xprv = generateKeyFromSeed (seed, mempty) mempty :: SeqKey 'RootK XPrv
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
        it "TxMeta (3)" $ do
            let txMeta = TxMeta
                    { status = Invalidated
                    , direction = Incoming
                    , slotId = SlotId 0 42
                    , blockHeight = Quantity 8
                    , amount = Quantity 0
                    }
            "+0.000000 invalidated since 0.42#8" === pretty @_ @Text txMeta

    let slotsPerEpoch = EpochLength 21600

    describe "syncProgress" $ do
        it "works for any two slots" $ property $ \sl0 sl1 ->
            syncProgress slotsPerEpoch sl0 sl1 `deepseq` ()
    describe "flatSlot" $ do
        it "flatSlot . fromFlatSlot == id" $ property $ \sl ->
            fromFlatSlot slotsPerEpoch (flatSlot slotsPerEpoch sl) === sl
        it "fromFlatSlot . flatSlot == id" $ property $ \n ->
            flatSlot slotsPerEpoch (fromFlatSlot slotsPerEpoch n) === n

    describe "Ranges" $ do

        it "arbitrary ranges are valid" $
            withMaxSuccess 1000 $ property $ \(r :: Range Integer) ->
                checkCoverage $
                cover 10 (rangeIsFinite r) "finite range" $
                rangeIsValid r .&&.
                    all rangeIsValid (shrink r)

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
            withMaxSuccess 1000 $ property $ \r@(Range a b :: Range Int) ->
                not (rangeIsSingleton r) ==>
                checkCoverage $
                cover 10 (rangeHasLowerBound r) "has lower bound" $
                cover 10 (rangeHasUpperBound r) "has upper bound" $
                cover 10 (rangeIsFinite      r) "is finite" $
                Range (succ <$> a) b `isSubrangeOf` Range a b

        it "Range a (pred b) `isSubrangeOf` Range a b" $
            withMaxSuccess 1000 $ property $ \r@(Range a b :: Range Int) ->
                not (rangeIsSingleton r) ==>
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
            let err = "wallet id should be an hex-encoded string \
                      \of 40 characters"
            fromText @WalletId "101" === Left (TextDecodingError err)
        it "fail fromText (@Hash \"Genesis\")" $ do
            let err = "Unable to decode (Hash \"Genesis\"): \
                      \expected Base16 encoding"
            fromText @(Hash "Genesis") "----" === Left (TextDecodingError err)

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

instance Arbitrary (Hash "Genesis") where
    arbitrary = Hash . BS.pack <$> arbitrary
    shrink (Hash v) = Hash . BS.pack <$> shrink (BS.unpack v)

instance Arbitrary (Hash "Tx") where
    -- No Shrinking
    arbitrary = oneof
        [ pure $ Hash "TXID01"
        , pure $ Hash "TXID02"
        , pure $ Hash "TXID03"
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
    arbitrary = fmap makeRangeValid $
        Range
            <$> arbitrary
            <*> arbitrary
    shrink (Range p q) = makeRangeValid <$>
        [ Range u v
        | u <- shrink p
        , v <- shrink q
        ]

-- Ensures that the start of a range is not greater than its end.
makeRangeValid :: Ord a => Range a -> Range a
makeRangeValid = \case
    Range (Just p) (Just q) -> Range (Just $ min p q) (Just $ max p q)
    r -> r

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
    shrink (Tx ins outs) =
        (flip Tx outs <$> shrink ins) <> (Tx ins <$> shrink outs)
    arbitrary = do
        ins <- choose (1, 3) >>= flip vectorOf arbitrary
        outs <- choose (1, 3) >>= flip vectorOf arbitrary
        return $ Tx ins outs

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

instance Arbitrary SlotId where
    shrink _ = []
    arbitrary = do
        ep <- choose (0, 10)
        sl <- choose (0, 100)
        return (SlotId (EpochNo ep) (SlotNo sl))

instance Arbitrary (Block Tx) where
    shrink (Block h txs) = Block h <$> shrink txs
    arbitrary = do
        txs <- choose (0, 500) >>= flip vectorOf arbitrary
        Block <$> arbitrary <*> pure txs

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

instance Arbitrary SlotParameters where
    arbitrary = SlotParameters <$> arbitrary <*> arbitrary <*> arbitrary
    shrink = genericShrink

instance {-# OVERLAPS #-} Arbitrary (SlotParameters, SlotId) where
    arbitrary = do
        (el, slot) <- arbitrary
        sl <- arbitrary
        st <- arbitrary
        pure (SlotParameters el sl st, slot)
    shrink (SlotParameters el sl st, slot) = do
        (el', slot') <- shrink (el, slot)
        pure (SlotParameters el' sl st, slot')

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
        ep <- EpochNo <$> choose (0, 1000)
        sl <- SlotNo <$> choose (0, fromIntegral epochLength - 1)
        return (EpochLength epochLength, SlotId ep sl)
