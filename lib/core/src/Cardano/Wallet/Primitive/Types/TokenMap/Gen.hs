{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Wallet.Primitive.Types.TokenMap.Gen
    ( genAssetId
    , genAssetIdLargeRange
    , genTokenMap
    , genTokenMapSmallRange
    , shrinkAssetId
    , shrinkTokenMap
    , AssetIdF (..)
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.TokenMap
    ( AssetId (..), TokenMap )
import Cardano.Wallet.Primitive.Types.TokenPolicy.Gen
    ( genTokenName
    , genTokenNameLargeRange
    , genTokenPolicyId
    , genTokenPolicyIdLargeRange
    , shrinkTokenName
    , shrinkTokenPolicyId
    , testTokenNames
    , testTokenPolicyIds
    )
import Cardano.Wallet.Primitive.Types.TokenQuantity.Gen
    ( genTokenQuantity, shrinkTokenQuantity )
import Control.Monad
    ( replicateM )
import Data.List
    ( elemIndex )
import Data.Maybe
    ( fromMaybe )
import GHC.Generics
    ( Generic )
import Test.QuickCheck
    ( CoArbitrary (..)
    , Function (..)
    , Gen
    , choose
    , functionMap
    , oneof
    , shrinkList
    , sized
    , variant
    )
import Test.QuickCheck.Extra
    ( genSized2With, shrinkInterleaved )

import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap

--------------------------------------------------------------------------------
-- Asset identifiers chosen from a range that depends on the size parameter
--------------------------------------------------------------------------------

genAssetId :: Gen AssetId
genAssetId = genSized2With AssetId genTokenPolicyId genTokenName

shrinkAssetId :: AssetId -> [AssetId]
shrinkAssetId (AssetId p t) = uncurry AssetId <$> shrinkInterleaved
    (p, shrinkTokenPolicyId)
    (t, shrinkTokenName)

--------------------------------------------------------------------------------
-- Asset identifiers chosen from a large range (to minimize collisions)
--------------------------------------------------------------------------------

genAssetIdLargeRange :: Gen AssetId
genAssetIdLargeRange = AssetId
    <$> genTokenPolicyIdLargeRange
    <*> genTokenNameLargeRange

--------------------------------------------------------------------------------
-- Token maps with assets and quantities chosen from ranges that depend on the
-- size parameter
--------------------------------------------------------------------------------

genTokenMap :: Gen TokenMap
genTokenMap = sized $ \size -> do
    assetCount <- choose (0, size)
    TokenMap.fromFlatList <$> replicateM assetCount genAssetQuantity
  where
    genAssetQuantity = (,)
        <$> genAssetId
        <*> genTokenQuantity

--------------------------------------------------------------------------------
-- Token maps with assets and quantities chosen from small ranges
--------------------------------------------------------------------------------

genTokenMapSmallRange :: Gen TokenMap
genTokenMapSmallRange = do
    assetCount <- oneof
        [ pure 0
        , pure 1
        , choose (2, 16)
        ]
    TokenMap.fromFlatList <$> replicateM assetCount genAssetQuantity
  where
    genAssetQuantity = (,)
        <$> genAssetId
        <*> genTokenQuantity

shrinkTokenMap :: TokenMap -> [TokenMap]
shrinkTokenMap
    = fmap TokenMap.fromFlatList
    . shrinkList shrinkAssetQuantity
    . TokenMap.toFlatList
  where
    shrinkAssetQuantity (a, q) = shrinkInterleaved
        (a, shrinkAssetId)
        (q, shrinkTokenQuantity)

--------------------------------------------------------------------------------
-- Filtering functions
--------------------------------------------------------------------------------

newtype AssetIdF = AssetIdF AssetId
    deriving (Generic, Eq, Show, Read)

instance Function AssetIdF where
    function = functionMap show read

instance CoArbitrary AssetIdF where
    coarbitrary (AssetIdF AssetId{tokenName, tokenPolicyId}) genB = do
        let n = fromMaybe 0 (elemIndex tokenName testTokenNames)
        let m = fromMaybe 0 (elemIndex tokenPolicyId testTokenPolicyIds)
        variant (n+m) genB
