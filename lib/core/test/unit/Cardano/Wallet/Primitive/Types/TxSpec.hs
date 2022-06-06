{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Copyright: © 2021 IOHK
-- License: Apache-2.0
--
module Cardano.Wallet.Primitive.Types.TxSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.Tx
    ( SealedTx (..)
    , TxOut (..)
    , mockSealedTx
    , sealedTxFromBytes
    , txOutAssetIds
    , txOutRemoveAssetId
    )
import Cardano.Wallet.Primitive.Types.Tx.Gen
    ( genTxOut, shrinkTxOut )
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
    ( Arbitrary (..), Property, property, (.&&.), (===) )

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
        it "prop_txOutRemoveAssetId_txOutAssetIds" $
            prop_txOutRemoveAssetId_txOutAssetIds & property

{-------------------------------------------------------------------------------
                         Evaluation of SealedTx fields
-------------------------------------------------------------------------------}

prop_sealedTxGibberish :: Gibberish -> Property
prop_sealedTxGibberish (Gibberish bs) =
    isLeft (serialisedTx <$> sealedTxFromBytes bs) .&&.
    isLeft (cardanoTx <$> sealedTxFromBytes bs)

prop_mockSealedTx :: Gibberish -> Property
prop_mockSealedTx (Gibberish bs) =
    serialisedTx (mockSealedTx bs) === bs

newtype Gibberish = Gibberish ByteString deriving (Show, Read, Eq)

instance Arbitrary Gibberish where
    arbitrary = Gibberish . pack <$> arbitrary

--------------------------------------------------------------------------------
-- Transformations
--------------------------------------------------------------------------------

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

instance Arbitrary TxOut where
    arbitrary = genTxOut
    shrink = shrinkTxOut
