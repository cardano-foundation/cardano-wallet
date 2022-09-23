{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Copyright: © 2021 IOHK
-- License: Apache-2.0
--
module Cardano.Wallet.Primitive.Types.TxSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( AssetId (..) )
import Cardano.Wallet.Primitive.Types.TokenMap.Gen
    ( genAssetId, shrinkAssetId )
import Cardano.Wallet.Primitive.Types.TokenPolicy
    ( TokenName (..), TokenPolicyId (..) )
import Cardano.Wallet.Primitive.Types.Tx
    ( SealedTx (..)
    , Tx (..)
    , TxOut (..)
    , mockSealedTx
    , sealedTxFromBytes
    , txAssetIds
    , txMapAssetIds
    , txMapTxIds
    , txOutAssetIds
    , txOutMapAssetIds
    , txOutRemoveAssetId
    , txRemoveAssetId
    )
import Cardano.Wallet.Primitive.Types.Tx.Gen
    ( genTx, genTxOut, shrinkTx, shrinkTxOut )
import Data.ByteString
    ( ByteString, pack )
import Data.Either
    ( isLeft )
import Data.Function
    ( (&) )
import Data.Maybe
    ( listToMaybe )
import Test.Hspec
    ( Spec, describe, it )
import Test.Hspec.Extra
    ( parallel )
import Test.Hspec.QuickCheck
    ( prop )
import Test.QuickCheck
    ( Arbitrary (..)
    , CoArbitrary (..)
    , Fun (..)
    , Function (..)
    , Property
    , property
    , (===)
    )
import Test.QuickCheck.Extra
    ( pattern ViewFun )
import Test.QuickCheck.Instances.ByteString
    ()

import qualified Data.Foldable as F
import qualified Data.Set as Set

spec :: Spec
spec = do

    parallel $ describe "SealedTx" $ do
        prop "sealedTxFromBytes - won't accept gibberish"
            prop_sealedTxGibberish
        prop "mockSealedTx - passes through mock values"
            prop_mockSealedTx

    parallel $ describe "Transformations" $ do

        describe "txMapAssetIds" $ do
            it "prop_txMapAssetIds_identity" $
                prop_txMapAssetIds_identity & property
            it "prop_txMapAssetIds_composition" $
                prop_txMapAssetIds_composition & property

        describe "txMapTxIds" $ do
            it "prop_txMapTxIds_identity" $
                prop_txMapTxIds_identity & property
            it "prop_txMapTxIds_composition" $
                prop_txMapTxIds_composition & property

        describe "txRemoveAssetId" $ do
            it "prop_txRemoveAssetId_txAssetIds" $
                prop_txRemoveAssetId_txAssetIds & property

        describe "txOutMapAssetIds" $ do
            it "prop_txOutMapAssetIds_identity" $
                prop_txOutMapAssetIds_identity & property
            it "prop_txOutMapAssetIds_composition" $
                prop_txOutMapAssetIds_composition & property

        describe "txOutRemoveAssetId" $ do
            it "prop_txOutRemoveAssetId_txOutAssetIds" $
                prop_txOutRemoveAssetId_txOutAssetIds & property

{-------------------------------------------------------------------------------
                         Evaluation of SealedTx fields
-------------------------------------------------------------------------------}

prop_sealedTxGibberish :: Gibberish -> Property
prop_sealedTxGibberish (Gibberish bs) = property $
    isLeft (sealedTxFromBytes bs)

prop_mockSealedTx :: Gibberish -> Property
prop_mockSealedTx (Gibberish bs) =
    serialisedTx (mockSealedTx bs) === bs

newtype Gibberish = Gibberish ByteString deriving (Show, Read, Eq)

instance Arbitrary Gibberish where
    arbitrary = Gibberish . pack <$> arbitrary

--------------------------------------------------------------------------------
-- Transformations
--------------------------------------------------------------------------------

prop_txMapAssetIds_identity :: Tx -> Property
prop_txMapAssetIds_identity m =
    txMapAssetIds id m === m

prop_txMapAssetIds_composition
    :: Tx -> Fun AssetId AssetId -> Fun AssetId AssetId -> Property
prop_txMapAssetIds_composition m (ViewFun f) (ViewFun g) =
    txMapAssetIds f (txMapAssetIds g m) ===
    txMapAssetIds (f . g) m

prop_txMapTxIds_identity :: Tx -> Property
prop_txMapTxIds_identity m =
    txMapTxIds id m === m

prop_txMapTxIds_composition
    :: Tx
    -> Fun (Hash "Tx") (Hash "Tx")
    -> Fun (Hash "Tx") (Hash "Tx")
    -> Property
prop_txMapTxIds_composition m (ViewFun f) (ViewFun g) =
    txMapTxIds f (txMapTxIds g m) ===
    txMapTxIds (f . g) m

prop_txRemoveAssetId_txAssetIds :: Tx -> Property
prop_txRemoveAssetId_txAssetIds tx =
    case assetIdM of
        Nothing ->
            assetIds === mempty
        Just assetId ->
            Set.notMember assetId
                (txAssetIds (tx `txRemoveAssetId` assetId))
            === True
  where
    assetIdM = listToMaybe $ F.toList assetIds
    assetIds = txAssetIds tx

prop_txOutMapAssetIds_identity :: TxOut -> Property
prop_txOutMapAssetIds_identity m =
    txOutMapAssetIds id m === m

prop_txOutMapAssetIds_composition
    :: TxOut -> Fun AssetId AssetId -> Fun AssetId AssetId -> Property
prop_txOutMapAssetIds_composition m (ViewFun f) (ViewFun g) =
    txOutMapAssetIds f (txOutMapAssetIds g m) ===
    txOutMapAssetIds (f . g) m

prop_txOutRemoveAssetId_txOutAssetIds :: TxOut -> Property
prop_txOutRemoveAssetId_txOutAssetIds txOut =
    case assetIdM of
        Nothing ->
            assetIds === mempty
        Just assetId ->
            Set.notMember assetId
                (txOutAssetIds (txOut `txOutRemoveAssetId` assetId))
            === True
  where
    assetIdM = listToMaybe $ F.toList assetIds
    assetIds = txOutAssetIds txOut

--------------------------------------------------------------------------------
-- Arbitrary instances
--------------------------------------------------------------------------------

instance Arbitrary AssetId where
    arbitrary = genAssetId
    shrink = shrinkAssetId

deriving anyclass instance CoArbitrary AssetId
deriving anyclass instance Function AssetId

deriving newtype instance Arbitrary (Hash "Tx")
deriving anyclass instance CoArbitrary (Hash "TokenPolicy")
deriving anyclass instance Function (Hash "TokenPolicy")

deriving anyclass instance CoArbitrary (Hash "Tx")
deriving anyclass instance Function (Hash "Tx")

instance Arbitrary Tx where
    arbitrary = genTx
    shrink = shrinkTx

instance Arbitrary TxOut where
    arbitrary = genTxOut
    shrink = shrinkTxOut

deriving anyclass instance CoArbitrary TokenName
deriving anyclass instance Function TokenName

deriving anyclass instance CoArbitrary TokenPolicyId
deriving anyclass instance Function TokenPolicyId
