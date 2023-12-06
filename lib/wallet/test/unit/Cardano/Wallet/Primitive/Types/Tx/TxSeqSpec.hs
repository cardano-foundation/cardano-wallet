{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{- HLINT ignore "Hoist not" -}

module Cardano.Wallet.Primitive.Types.Tx.TxSeqSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.Address.Gen
    ( genAddress
    )
import Cardano.Wallet.Primitive.Types.AssetId
    ( AssetId
    )
import Cardano.Wallet.Primitive.Types.AssetId.Gen
    ( genAssetId
    , shrinkAssetId
    )
import Cardano.Wallet.Primitive.Types.AssetName
    ( TokenName (..)
    )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..)
    )
import Cardano.Wallet.Primitive.Types.TokenPolicyId
    ( TokenPolicyId (..)
    )
import Cardano.Wallet.Primitive.Types.Tx.TxSeq.Gen
    ( ShrinkableTxSeq
    , genTxSeq
    , getShrinkPhase
    , getTxSeq
    , shrinkTxSeq
    )
import Cardano.Wallet.Primitive.Types.UTxO.Gen
    ( genUTxO
    )
import Data.Function
    ( (&)
    )
import Data.Function.Utils
    ( isInjectiveOver
    )
import Data.Maybe
    ( fromMaybe
    , mapMaybe
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    )
import Test.QuickCheck
    ( Arbitrary (..)
    , CoArbitrary (..)
    , Fun (..)
    , Function (..)
    , Property
    , Testable (..)
    , applyFun
    , checkCoverage
    , chooseInt
    , cover
    , forAll
    , property
    , withMaxSuccess
    , (===)
    , (==>)
    )
import Test.QuickCheck.Extra
    ( ScaleDiv (..)
    , genShrinkSequence
    , labelInterval
    , shrinkWhile
    )
import Test.QuickCheck.Instances.ByteString
    ()

import qualified Cardano.Wallet.Primitive.Types.Tx.TxSeq as TxSeq
import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set

spec :: Spec
spec = do

    describe "Generation" $ do

        describe "genTxSeq" $ do
            it "prop_genTxSeq_isValid" $
                prop_genTxSeq_isValid
                    & property
            it "prop_genTxSeq_toTxGroupList_length" $
                prop_genTxSeq_toTxGroupList_length
                    & property
            it "prop_genTxSeq_toTxGroupList_lengths" $
                prop_genTxSeq_toTxGroupList_lengths
                    & property
            it "prop_genTxSeq_assetIds_size" $
                prop_genTxSeq_assetIds_size
                    & property
            it "prop_genTxSeq_txCount" $
                prop_genTxSeq_txCount
                    & property

    describe "Shrinking" $ do

        describe "dropGroupBoundary" $ do
            it "prop_dropGroupBoundary_isValid" $
                prop_dropGroupBoundary_isValid
                    & property
            it "prop_dropGroupBoundary_toTxs" $
                prop_dropGroupBoundary_toTxs
                    & property
            it "prop_dropGroupBoundary_txGroupCount_length" $
                prop_dropGroupBoundary_txGroupCount_length
                    & property
            it "prop_dropGroupBoundary_txGroupCount_pred" $
                prop_dropGroupBoundary_txGroupCount_pred
                    & property

        describe "prefixes" $ do
            it "prop_prefixes_isValid" $
                prop_prefixes_isValid
                    & property

        describe "suffixes" $ do
            it "prop_suffixes_isValid" $
                prop_suffixes_isValid
                    & property

        describe "shrinkAssetIds" $ do
            it "prop_shrinkAssetIds_idempotent" $
                prop_shrinkAssetIds_idempotent
                    & property
            it "prop_shrinkAssetIds_length" $
                prop_shrinkAssetIds_length
                    & property
            it "prop_shrinkAssetIds_isValid" $
                prop_shrinkAssetIds_isValid
                    & property

        describe "shrinkTxIds" $ do
            it "prop_shrinkTxIds_idempotent" $
                prop_shrinkTxIds_idempotent
                    & property
            it "prop_shrinkTxIds_length" $
                prop_shrinkTxIds_length
                    & property
            it "prop_shrinkTxIds_isValid" $
                prop_shrinkTxIds_isValid
                    & property

        describe "shrinkTxSeq" $ do
            it "prop_shrinkTxSeq_canShrinkToEmptySequence" $
                prop_shrinkTxSeq_canShrinkToEmptySequence
                    & property
            it "prop_shrinkTxSeq_canShrinkToTargetLength" $
                prop_shrinkTxSeq_canShrinkToTargetLength
                    & property
            it "prop_shrinkTxSeq_genShrinkSequence_allShrinkPhasesCovered" $
                prop_shrinkTxSeq_genShrinkSequence_allShrinkPhasesCovered
                    & property
            it "prop_shrinkTxSeq_genShrinkSequence_isValid" $
                prop_shrinkTxSeq_genShrinkSequence_isValid
                    & property
            it "prop_shrinkTxSeq_genShrinkSequence_length" $
                prop_shrinkTxSeq_genShrinkSequence_length
                    & property

    describe "Mapping" $ do

        describe "mapAssetIds" $ do
            it "prop_mapAssetIds_assetIds" $
                prop_mapAssetIds_assetIds
                    & withMaxSuccess 20
                    & property
            it "prop_mapAssetIds_composition" $
                prop_mapAssetIds_composition
                    & withMaxSuccess 20
                    & property
            it "prop_mapAssetIds_identity" $
                prop_mapAssetIds_identity
                    & withMaxSuccess 20
                    & property
            it "prop_mapAssetIds_isValid" $
                prop_mapAssetIds_isValid
                    & withMaxSuccess 20
                    & property

        describe "mapTxIds" $ do
            it "prop_mapTxIds_txIds" $
                prop_mapTxIds_txIds
                    & withMaxSuccess 20
                    & property
            it "prop_mapTxIds_composition" $
                prop_mapTxIds_composition
                    & withMaxSuccess 20
                    & property
            it "prop_mapTxIds_identity" $
                prop_mapTxIds_identity
                    & withMaxSuccess 20
                    & property
            it "prop_mapTxIds_isValid" $
                prop_mapTxIds_isValid
                    & withMaxSuccess 20
                    & property

    describe "Conversion" $ do

        describe "toTxs" $ do
            it "prop_toTxList_txCount" $
                prop_toTxList_txCount
                    & property

        describe "toTxGroups" $ do
            it "prop_toTxGroupList_txGroupCount" $
                prop_toTxGroupList_txGroupCount
                    & property
            it "prop_toTxGroupList_toTxs" $
                prop_toTxGroupList_toTxs
                    & property

--------------------------------------------------------------------------------
-- Generation
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- genTxSeq
--------------------------------------------------------------------------------

prop_genTxSeq_isValid :: Property
prop_genTxSeq_isValid =
    forAll (genTxSeq genUTxO genAddress) $ \(getTxSeq -> txSeq) ->
        TxSeq.isValid txSeq

prop_genTxSeq_toTxGroupList_length :: Property
prop_genTxSeq_toTxGroupList_length =
    forAll (genTxSeq genUTxO genAddress) $ \(getTxSeq -> txSeq) ->
        let txGroups = TxSeq.toTxGroupList txSeq in
        checkCoverage
            $ cover 1 (length txGroups == 1)
                "number of groups = 1"
            $ cover 10 (length txGroups > 1)
                "number of groups > 1"
            $ property True

prop_genTxSeq_toTxGroupList_lengths :: Property
prop_genTxSeq_toTxGroupList_lengths =
    forAll (genTxSeq genUTxO genAddress) $ \(getTxSeq -> txSeq) ->
        let txGroups = TxSeq.toTxGroupList txSeq in
        checkCoverage
            $ cover 5 (null (NE.head txGroups))
                "number of elements in head group = 0"
            $ cover 5 (length (NE.head txGroups) == 1)
                "number of elements in head group = 1"
            $ cover 5 (length (NE.head txGroups) > 1)
                "number of elements in head group > 1"
            $ cover 5 (null (NE.last txGroups))
                "number of elements in last group = 0"
            $ cover 5 (length (NE.last txGroups) == 1)
                "number of elements in last group = 1"
            $ cover 5 (length (NE.last txGroups) > 1)
                "number of elements in last group > 1"
            $ property True

prop_genTxSeq_assetIds_size :: Property
prop_genTxSeq_assetIds_size =
    forAll (genTxSeq genUTxO genAddress) $ \(getTxSeq -> txSeq) ->
        let assetIdCount = Set.size (TxSeq.assetIds txSeq) in
        labelInterval 10 "number of unique asset ids" assetIdCount
        True

prop_genTxSeq_txCount :: Property
prop_genTxSeq_txCount =
    forAll (genTxSeq genUTxO genAddress) $ \(getTxSeq -> txSeq) ->
        labelInterval 10 "number of transactions" (TxSeq.txCount txSeq)
        True

--------------------------------------------------------------------------------
-- Shrinking
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- dropGroupBoundary
--------------------------------------------------------------------------------

prop_dropGroupBoundary_isValid :: ShrinkableTxSeq -> Property
prop_dropGroupBoundary_isValid (getTxSeq -> txSeq) =
    all TxSeq.isValid (TxSeq.dropGroupBoundary txSeq) === True

prop_dropGroupBoundary_toTxs :: ShrinkableTxSeq -> Property
prop_dropGroupBoundary_toTxs (getTxSeq -> txSeq) =
    all
        ((== TxSeq.toTxList txSeq) . TxSeq.toTxList)
        (TxSeq.dropGroupBoundary txSeq)
    === True

prop_dropGroupBoundary_txGroupCount_length
    :: ShrinkableTxSeq -> Property
prop_dropGroupBoundary_txGroupCount_length (getTxSeq -> txSeq) =
    length (TxSeq.dropGroupBoundary txSeq)
    ===
    pred (TxSeq.txGroupCount txSeq)

prop_dropGroupBoundary_txGroupCount_pred
    :: ShrinkableTxSeq -> Property
prop_dropGroupBoundary_txGroupCount_pred (getTxSeq -> txSeq)
    | txGroupCount == 0 =
        TxSeq.dropGroupBoundary txSeq === []
    | otherwise =
        all ((== pred txGroupCount) . TxSeq.txGroupCount)
            (TxSeq.dropGroupBoundary txSeq)
        === True
  where
    txGroupCount = TxSeq.txGroupCount txSeq

--------------------------------------------------------------------------------
-- prefixes
--------------------------------------------------------------------------------

prop_prefixes_isValid :: ShrinkableTxSeq -> Property
prop_prefixes_isValid (getTxSeq -> txSeq) =
    all TxSeq.isValid (TxSeq.prefixes txSeq) === True

--------------------------------------------------------------------------------
-- suffixes
--------------------------------------------------------------------------------

prop_suffixes_isValid :: ShrinkableTxSeq -> Property
prop_suffixes_isValid (getTxSeq -> txSeq) =
    all TxSeq.isValid (TxSeq.suffixes txSeq) === True

--------------------------------------------------------------------------------
-- shrinkAssetIds
--------------------------------------------------------------------------------

prop_shrinkAssetIds_idempotent :: ShrinkableTxSeq -> Property
prop_shrinkAssetIds_idempotent (getTxSeq -> txSeq) =
    TxSeq.assetIds (f (f txSeq)) === TxSeq.assetIds (f txSeq)
  where
    f = TxSeq.shrinkAssetIds

prop_shrinkAssetIds_length :: ShrinkableTxSeq -> Property
prop_shrinkAssetIds_length (getTxSeq -> txSeq) =
    length (TxSeq.assetIds (f txSeq)) === length (TxSeq.assetIds txSeq)
  where
    f = TxSeq.shrinkAssetIds

prop_shrinkAssetIds_isValid :: ShrinkableTxSeq -> Property
prop_shrinkAssetIds_isValid (getTxSeq -> txSeq) =
    TxSeq.isValid (TxSeq.shrinkAssetIds txSeq) === True

--------------------------------------------------------------------------------
-- shrinkTxIds
--------------------------------------------------------------------------------

prop_shrinkTxIds_idempotent :: ShrinkableTxSeq -> Property
prop_shrinkTxIds_idempotent (getTxSeq -> txSeq) =
    TxSeq.txIds (f (f txSeq)) === TxSeq.txIds (f txSeq)
  where
    f = TxSeq.shrinkTxIds

prop_shrinkTxIds_length :: ShrinkableTxSeq -> Property
prop_shrinkTxIds_length (getTxSeq -> txSeq) =
    length (TxSeq.txIds (TxSeq.shrinkTxIds txSeq))
    ===
    length (TxSeq.txIds txSeq)

prop_shrinkTxIds_isValid :: ShrinkableTxSeq -> Property
prop_shrinkTxIds_isValid (getTxSeq -> txSeq) =
    TxSeq.isValid (TxSeq.shrinkTxIds txSeq) === True

--------------------------------------------------------------------------------
-- shrinkTxSeq
--------------------------------------------------------------------------------

prop_shrinkTxSeq_canShrinkToEmptySequence :: Property
prop_shrinkTxSeq_canShrinkToEmptySequence =
    forAll (genTxSeq genUTxO genAddress) $ \s0 ->
        case shrinkSpaceMinimum shrinkTxSeq s0 of
            Nothing -> TxSeq.length (getTxSeq s0) === 0
            Just s1 -> TxSeq.length (getTxSeq s1) === 0
  where
    shrinkSpaceMinimum :: (a -> [a]) -> a -> Maybe a
    shrinkSpaceMinimum = shrinkWhile (const True)

prop_shrinkTxSeq_canShrinkToTargetLength :: Property
prop_shrinkTxSeq_canShrinkToTargetLength =
    forAll (genTxSeq genUTxO genAddress) $ \txSeq ->
    forAll (chooseInt (0, TxSeq.length (getTxSeq txSeq))) $ \targetLength ->
    prop_inner txSeq targetLength
  where
    prop_inner :: ShrinkableTxSeq -> Int -> Property
    prop_inner txSeq targetLength =
        TxSeq.length (getTxSeq $ shrinkTxSeqToLength targetLength txSeq)
            === targetLength

    shrinkTxSeqToLength :: Int -> ShrinkableTxSeq -> ShrinkableTxSeq
    shrinkTxSeqToLength targetLength txSeq = fromMaybe txSeq $
        shrinkWhile
            ((>= targetLength) . TxSeq.length . getTxSeq)
            shrinkTxSeq
            txSeq

prop_shrinkTxSeq_genShrinkSequence_allShrinkPhasesCovered :: Property
prop_shrinkTxSeq_genShrinkSequence_allShrinkPhasesCovered =
    forAll (genShrinkSequence shrinkTxSeq =<< genTxSeq genUTxO genAddress) $
        \txSeqShrinks ->
            Set.fromList (mapMaybe getShrinkPhase txSeqShrinks) ===
            Set.fromList [minBound .. maxBound]

prop_shrinkTxSeq_genShrinkSequence_isValid :: Property
prop_shrinkTxSeq_genShrinkSequence_isValid =
    forAll (genShrinkSequence shrinkTxSeq =<< genTxSeq genUTxO genAddress) $
        \txSeqShrinks ->
            all (TxSeq.isValid . getTxSeq) txSeqShrinks

prop_shrinkTxSeq_genShrinkSequence_length :: Property
prop_shrinkTxSeq_genShrinkSequence_length =
    forAll (genShrinkSequence shrinkTxSeq =<< genTxSeq genUTxO genAddress) $
        \txSeqShrinks ->
            let shrinkCount = length txSeqShrinks in
            labelInterval 10 "shrink count" shrinkCount
            True

--------------------------------------------------------------------------------
-- Mapping
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- mapAssetIds
--------------------------------------------------------------------------------

prop_mapAssetIds_assetIds :: ShrinkableTxSeq -> Fun AssetId AssetId -> Property
prop_mapAssetIds_assetIds (getTxSeq -> txSeq) (applyFun -> f) =
    TxSeq.assetIds (TxSeq.mapAssetIds f txSeq)
    ===
    Set.map f (TxSeq.assetIds txSeq)

prop_mapAssetIds_composition
    :: ShrinkableTxSeq
    -> Fun AssetId AssetId
    -> Fun AssetId AssetId
    -> Property
prop_mapAssetIds_composition
    (getTxSeq -> txSeq) (applyFun -> f) (applyFun -> g) =
        TxSeq.mapAssetIds f (TxSeq.mapAssetIds g txSeq) ===
        TxSeq.mapAssetIds (f . g) txSeq

prop_mapAssetIds_identity :: ShrinkableTxSeq -> Property
prop_mapAssetIds_identity (getTxSeq -> txSeq) =
    TxSeq.mapAssetIds id txSeq === txSeq

prop_mapAssetIds_isValid
    :: ScaleDiv 2 ShrinkableTxSeq
    -> Fun AssetId AssetId
    -> Property
prop_mapAssetIds_isValid (getTxSeq . unScaleDiv -> txSeq) (applyFun -> f) =
    -- Validity is maintained regardless of whether the specified mapping
    -- function is injective w.r.t. the set of asset identifiers in the
    -- given sequence.
    checkCoverage $
    cover 10 injective "injective" $
    cover 10 (not injective) "not injective" $
    TxSeq.isValid (TxSeq.mapAssetIds f txSeq) === True
  where
    injective = f `isInjectiveOver` TxSeq.assetIds txSeq

--------------------------------------------------------------------------------
-- mapTxIds
--------------------------------------------------------------------------------

prop_mapTxIds_txIds
    :: ShrinkableTxSeq -> Fun (Hash "Tx") (Hash "Tx") -> Property
prop_mapTxIds_txIds (getTxSeq -> txSeq) (applyFun -> f) =
    TxSeq.txIds (TxSeq.mapTxIds f txSeq) === Set.map f (TxSeq.txIds txSeq)

prop_mapTxIds_composition
    :: ShrinkableTxSeq
    -> Fun (Hash "Tx") (Hash "Tx")
    -> Fun (Hash "Tx") (Hash "Tx")
    -> Property
prop_mapTxIds_composition (getTxSeq -> txSeq) (applyFun -> f) (applyFun -> g) =
    TxSeq.mapTxIds f (TxSeq.mapTxIds g txSeq) ===
    TxSeq.mapTxIds (f . g) txSeq

prop_mapTxIds_identity :: ShrinkableTxSeq -> Property
prop_mapTxIds_identity (getTxSeq -> txSeq) =
    TxSeq.mapTxIds id txSeq === txSeq

prop_mapTxIds_isValid
    :: ShrinkableTxSeq -> Fun (Hash "Tx") (Hash "Tx") -> Property
prop_mapTxIds_isValid (getTxSeq -> txSeq) (applyFun -> f) =
    -- Validity is only guaranteed if the specified mapping function is
    -- injective w.r.t. the set of transaction identifiers in the given
    -- sequence.
    injective ==> TxSeq.isValid (TxSeq.mapTxIds f txSeq)
  where
    injective = f `isInjectiveOver` TxSeq.txIds txSeq

--------------------------------------------------------------------------------
-- Conversion
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- toTxList
--------------------------------------------------------------------------------

prop_toTxList_txCount :: ShrinkableTxSeq -> Property
prop_toTxList_txCount (getTxSeq -> txSeq) =
    length (TxSeq.toTxList txSeq) === TxSeq.txCount txSeq

--------------------------------------------------------------------------------
-- toTxGroupList
--------------------------------------------------------------------------------

prop_toTxGroupList_txGroupCount :: ShrinkableTxSeq -> Property
prop_toTxGroupList_txGroupCount (getTxSeq -> txSeq) =
    length (TxSeq.toTxGroupList txSeq) === TxSeq.txGroupCount txSeq

prop_toTxGroupList_toTxs :: ShrinkableTxSeq -> Property
prop_toTxGroupList_toTxs (getTxSeq -> txSeq) =
    F.fold (TxSeq.toTxGroupList txSeq) === TxSeq.toTxList txSeq

--------------------------------------------------------------------------------
-- Arbitrary instances
--------------------------------------------------------------------------------

instance Arbitrary AssetId where
    arbitrary = genAssetId
    shrink = shrinkAssetId

deriving newtype instance Arbitrary (Hash "Tx")

instance Arbitrary ShrinkableTxSeq where
    arbitrary = genTxSeq genUTxO genAddress
    shrink = shrinkTxSeq

deriving anyclass instance CoArbitrary (Hash "TokenPolicy")
deriving anyclass instance CoArbitrary (Hash "Tx")
deriving anyclass instance CoArbitrary AssetId
deriving anyclass instance CoArbitrary TokenName
deriving anyclass instance CoArbitrary TokenPolicyId

deriving anyclass instance Function (Hash "TokenPolicy")
deriving anyclass instance Function (Hash "Tx")
deriving anyclass instance Function AssetId
deriving anyclass instance Function TokenName
deriving anyclass instance Function TokenPolicyId
