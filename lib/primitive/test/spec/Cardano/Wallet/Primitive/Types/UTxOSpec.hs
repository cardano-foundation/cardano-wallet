{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{- HLINT ignore "Use camelCase" -}

module Cardano.Wallet.Primitive.Types.UTxOSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.Address
    ( Address (..)
    )
import Cardano.Wallet.Primitive.Types.Address.Gen
    ( Parity (..)
    , addressParity
    )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..)
    , mockHash
    )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( AssetId (..)
    )
import Cardano.Wallet.Primitive.Types.TokenMap.Gen
    ( genAssetId
    , shrinkAssetId
    )
import Cardano.Wallet.Primitive.Types.TokenPolicy
    ( TokenName (..)
    , TokenPolicyId (..)
    )
import Cardano.Wallet.Primitive.Types.Tx.TxIn
    ( TxIn (..)
    )
import Cardano.Wallet.Primitive.Types.Tx.TxIn.Gen
    ( coarbitraryTxIn
    )
import Cardano.Wallet.Primitive.Types.Tx.TxOut
    ( TxOut (..)
    )
import Cardano.Wallet.Primitive.Types.UTxO
    ( UTxO (..)
    , dom
    , excludingD
    , filterByAddress
    , filterByAddressM
    , isSubsetOf
    , receiveD
    )
import Cardano.Wallet.Primitive.Types.UTxO.Gen
    ( genUTxO
    , shrinkUTxO
    )
import Data.Delta
    ( apply
    )
import Data.Function
    ( (&)
    )
import Data.Functor.Identity
    ( runIdentity
    )
import Data.Generics.Internal.VL.Lens
    ( view
    )
import Data.Maybe
    ( listToMaybe
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
    , Testable
    , applyFun
    , checkCoverage
    , conjoin
    , cover
    , property
    , (===)
    )
import Test.QuickCheck.Instances.ByteString
    (
    )

import qualified Cardano.Wallet.Primitive.Types.UTxO as UTxO
import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

spec :: Spec
spec =
    describe "Cardano.Wallet.Primitive.Types.UTxOSpec" $ do
        describe "delta encoding" $ do
            it "DeltaUTXO is a semigroup compatible with `apply`"
                $ property prop_deltaUTxO_semigroup_apply

        describe "filtering and partitioning" $ do
            it "prop_filter_disjoint"
                $ property prop_filter_disjoint
            it "prop_filter_partition"
                $ property prop_filter_partition
            it "prop_filter_toList"
                $ property prop_filter_toList
            it "prop_partition_disjoint"
                $ property prop_partition_disjoint
            it "prop_partition_mappend"
                $ property prop_partition_mappend

        describe "filtering by address" $ do
            it "matching everything gives us everything"
                $ property prop_filterByAddress_matchAll
            it "matching nothing gives us nothing"
                $ property prop_filterByAddress_matchNone
            it "matching some addresses gives us the appropriate subset"
                $ property prop_filterByAddress_matchSome
            it "if there are no utxos, the result utxo should be empty"
                $ property prop_filterByAddress_empty
            it "filterByAddress/filterByAddressM"
                $ property prop_filterByAddress_filterByAddressM
            it "filterByAddress is always subset"
                $ property prop_filterByAddress_isSubset

        describe "Transformations" $ do
            describe "mapAssetIds" $ do
                it "prop_mapAssetIds_identity"
                    $ prop_mapAssetIds_identity
                    & property
                it "prop_mapAssetIds_composition"
                    $ prop_mapAssetIds_composition
                    & property

            describe "mapTxIds" $ do
                it "prop_mapTxIds_identity"
                    $ prop_mapTxIds_identity
                    & property
                it "prop_mapTxIds_composition"
                    $ prop_mapTxIds_composition
                    & property

            describe "removeAssetId" $ do
                it "prop_removeAssetId_assetIds"
                    $ prop_removeAssetId_assetIds
                    & property

prop_deltaUTxO_semigroup_apply :: Property
prop_deltaUTxO_semigroup_apply =
    delta2 `apply` (delta1 `apply` utxo0)
        === (delta2 <> delta1) `apply` utxo0
  where
    {- Note [Property Testing of Boolean Algebras]

    It turns out that the validity of simple properties of finite sets
    (or, more generally, boolean algebras) can be decided
    by considering a single, universal example.
    Essentially, this example corresponds to a truth table.
    These examples are typically visualized as Venn diagrams.

    For example, in order to show that the equality

        (A ∩ B) ∪ C = (A ∪ C) ∩ (B ∪ C)

    holds for all finite sets A,B,C, it is sufficient to show that it
    holds for the specific case

        A = { "100", "110", "101", "111"}
        B = { "010", "110", "011", "111"}
        C = { "001", "101", "011", "111"}

    Even though the elements like "010" seem very specific, they stand
    for an entire subset of elements; here, "010" stands for all elements
    that are contained in B, but not in A or C.
    -}
    {- Note [Property Testing of DeltaUTxO]

    In order to test properties of `DeltaUTxO`, we can apply
    Note [Property Testing of Boolean Algebras] above, as most operations
    on this data type are essentially set-theoretic operations.

    In particular, the associativity of `(<>)` on `DeltaUTxO` corresponds
    to a simple statement about finite sets, and it is sufficient to
    test a single, universal case — the test case below.

    The outputs named "a0" etc correspond to entire subsets of outputs.
    For example, "a2" represents the subset of outputs contained in utxo0,
    not spent by delta1, but spent by delta2.
    Due to the "no double-spending" constraint, not all
    subsets are relevant; the relevant ones are included in this test case.
    -}
    utxo0 = mkUTxOs ["a0", "a1", "a2"]
    delta1 = mkDelta ["a1"] ["b1", "b2"]
    delta2 = mkDelta ["a2", "b2"] ["c2"]

    names = Map.fromList $ zip ["a0", "a1", "a2", "b1", "b2", "c2" :: String] [0 ..]
    mkTxIn name = TxIn (mockHash name) (names Map.! name)
    mkUTxO ix =
        UTxO
            $ Map.fromList
                [(mkTxIn ix, TxOut (Address "TEST") mempty)]
    mkUTxOs = mconcat . map mkUTxO
    mkDelta ex re =
        fst (excludingD (mkUTxOs ex) $ Set.fromList (map mkTxIn ex))
            <> fst (receiveD mempty $ mkUTxOs re)

--------------------------------------------------------------------------------
-- Filtering and partitioning
--------------------------------------------------------------------------------

prop_filter_disjoint :: (TxIn -> Bool) -> UTxO -> Property
prop_filter_disjoint f u =
    checkCoverage_filter_partition f u
        $ UTxO.filter f u `UTxO.disjoint` UTxO.filter (not . f) u === True

prop_filter_partition :: (TxIn -> Bool) -> UTxO -> Property
prop_filter_partition f u =
    checkCoverage_filter_partition f u
        $ (UTxO.filter f u, UTxO.filter (not . f) u) === UTxO.partition f u

prop_filter_toList :: (TxIn -> Bool) -> UTxO -> Property
prop_filter_toList f u =
    checkCoverage_filter_partition f u
        $ UTxO.toList (UTxO.filter f u) === L.filter (f . fst) (UTxO.toList u)

prop_partition_disjoint :: (TxIn -> Bool) -> UTxO -> Property
prop_partition_disjoint f u =
    checkCoverage_filter_partition f u
        $ uncurry UTxO.disjoint (UTxO.partition f u) === True

prop_partition_mappend :: (TxIn -> Bool) -> UTxO -> Property
prop_partition_mappend f u =
    checkCoverage_filter_partition f u
        $ uncurry (<>) (UTxO.partition f u) === u

checkCoverage_filter_partition
    :: (Testable prop) => (TxIn -> Bool) -> UTxO -> (prop -> Property)
checkCoverage_filter_partition f u =
    checkCoverage
        . cover
            10
            (UTxO.filter f u `isNonEmptyProperSubsetOf` u)
            "UTxO.filter f u `isNonEmptyProperSubsetOf` u"
        . cover
            10
            (UTxO.filter (not . f) u `isNonEmptyProperSubsetOf` u)
            "UTxO.filter (not . f) u `isNonEmptyProperSubsetOf` u"
        . cover
            10
            (UTxO.size (UTxO.filter f u) > UTxO.size (UTxO.filter (not . f) u))
            "UTxO.size (UTxO.filter f u) > UTxO.size (UTxO.filter (not . f) u)"
        . cover
            10
            (UTxO.size (UTxO.filter f u) < UTxO.size (UTxO.filter (not . f) u))
            "UTxO.size (UTxO.filter f u) < UTxO.size (UTxO.filter (not . f) u)"
  where
    u1 `isNonEmptyProperSubsetOf` u2 =
        not (UTxO.null u1)
            && u1 `UTxO.isSubsetOf` u2
            && u1 /= u2

--------------------------------------------------------------------------------
-- Filtering by address
--------------------------------------------------------------------------------

prop_filterByAddress_matchAll :: UTxO -> Property
prop_filterByAddress_matchAll u =
    checkCoverage
        $ cover 2 (u == mempty) "empty"
        $ cover 8 (u /= mempty) "non-empty"
        $ filterByAddress (const True) u === u

prop_filterByAddress_matchNone :: UTxO -> Property
prop_filterByAddress_matchNone u =
    checkCoverage
        $ cover 2 (u == mempty) "empty"
        $ cover 8 (u /= mempty) "non-empty"
        $ filterByAddress (const False) u === mempty

prop_filterByAddress_matchSome :: UTxO -> Property
prop_filterByAddress_matchSome utxo =
    checkCoverage
        $ cover
            10
            (domEven /= mempty && domEven `Set.isProperSubsetOf` dom utxo)
            "domEven /= mempty && domEven `Set.isProperSubsetOf` dom utxo"
        $ cover
            10
            (domOdd /= mempty && domOdd `Set.isProperSubsetOf` dom utxo)
            "domOdd /= mempty && domOdd `Set.isProperSubsetOf` dom utxo"
        $ conjoin
            [ utxoEven <> utxoOdd == utxo
            , unUTxO utxoEven `Map.isSubmapOf` unUTxO utxo
            , unUTxO utxoOdd `Map.isSubmapOf` unUTxO utxo
            , all ((== Even) . addressParity . view #address) (unUTxO utxoEven)
            , all ((== Odd) . addressParity . view #address) (unUTxO utxoOdd)
            ]
  where
    domEven = dom utxoEven
    domOdd = dom utxoOdd

    utxoEven = filterByAddress ((== Even) . addressParity) utxo
    utxoOdd = filterByAddress ((== Odd) . addressParity) utxo

prop_filterByAddress_empty :: (Address -> Bool) -> Property
prop_filterByAddress_empty f =
    filterByAddress f mempty === mempty

prop_filterByAddress_filterByAddressM :: UTxO -> (Address -> Bool) -> Property
prop_filterByAddress_filterByAddressM u f =
    checkCoverage
        $ cover 10 isNonEmptyProperSubset "is non-empty proper subset"
        $ filterByAddress f u === runIdentity (filterByAddressM (pure . f) u)
  where
    isNonEmptyProperSubset =
        (&&)
            (filterByAddress f u /= mempty)
            (dom (filterByAddress f u) `Set.isProperSubsetOf` dom u)

prop_filterByAddress_isSubset :: UTxO -> (Address -> Bool) -> Property
prop_filterByAddress_isSubset u f =
    checkCoverage
        $ cover 10 isNonEmptyProperSubset "is non-empty proper subset"
        $ property
        $ filterByAddress f u `isSubsetOf` u
  where
    isNonEmptyProperSubset =
        (&&)
            (filterByAddress f u /= mempty)
            (dom (filterByAddress f u) `Set.isProperSubsetOf` dom u)

--------------------------------------------------------------------------------
-- Transformations
--------------------------------------------------------------------------------

prop_mapAssetIds_identity :: UTxO -> Property
prop_mapAssetIds_identity m =
    UTxO.mapAssetIds id m === m

prop_mapAssetIds_composition
    :: UTxO
    -> Fun AssetId AssetId
    -> Fun AssetId AssetId
    -> Property
prop_mapAssetIds_composition m (applyFun -> f) (applyFun -> g) =
    UTxO.mapAssetIds f (UTxO.mapAssetIds g m)
        === UTxO.mapAssetIds (f . g) m

prop_mapTxIds_identity :: UTxO -> Property
prop_mapTxIds_identity m =
    UTxO.mapTxIds id m === m

prop_mapTxIds_composition
    :: UTxO
    -> Fun (Hash "Tx") (Hash "Tx")
    -> Fun (Hash "Tx") (Hash "Tx")
    -> Property
prop_mapTxIds_composition m (applyFun -> f) (applyFun -> g) =
    UTxO.mapTxIds f (UTxO.mapTxIds g m)
        === UTxO.mapTxIds (f . g) m

prop_removeAssetId_assetIds :: UTxO -> Property
prop_removeAssetId_assetIds u =
    case assetIdM of
        Nothing ->
            assetIds === mempty
        Just assetId ->
            Set.notMember
                assetId
                (UTxO.assetIds (u `UTxO.removeAssetId` assetId))
                === True
  where
    assetIdM = listToMaybe $ F.toList assetIds
    assetIds = UTxO.assetIds u

--------------------------------------------------------------------------------
-- Arbitrary instances
--------------------------------------------------------------------------------

deriving anyclass instance CoArbitrary Address
deriving anyclass instance Function Address

instance Arbitrary AssetId where
    arbitrary = genAssetId
    shrink = shrinkAssetId

deriving anyclass instance CoArbitrary AssetId
deriving anyclass instance Function AssetId

deriving anyclass instance CoArbitrary (Hash "TokenPolicy")
deriving anyclass instance Function (Hash "TokenPolicy")

deriving newtype instance Arbitrary (Hash "Tx")
deriving anyclass instance CoArbitrary (Hash "Tx")
deriving anyclass instance Function (Hash "Tx")

deriving anyclass instance CoArbitrary TokenName
deriving anyclass instance Function TokenName

deriving anyclass instance CoArbitrary TokenPolicyId
deriving anyclass instance Function TokenPolicyId

instance CoArbitrary TxIn where
    coarbitrary = coarbitraryTxIn

instance Arbitrary UTxO where
    arbitrary = genUTxO
    shrink = shrinkUTxO

--------------------------------------------------------------------------------
-- Show instances
--------------------------------------------------------------------------------

instance Show (Address -> Bool) where
    show = const "(Address -> Bool)"

instance Show (TxIn -> Bool) where
    show = const "(TxIn -> Bool)"
