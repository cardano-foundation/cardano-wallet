{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Wallet.Primitive.Types.TokenMap.Gen
    ( genAssetIdSmallRange
    , genTokenMapSmallRange
    , shrinkAssetIdSmallRange
    , shrinkTokenMapSmallRange
    , AssetIdF (..)
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.TokenMap
    ( AssetId (..), TokenMap )
import Cardano.Wallet.Primitive.Types.TokenPolicy.Gen
    ( genTokenNameSmallRange
    , genTokenPolicyIdSmallRange
    , shrinkTokenNameSmallRange
    , shrinkTokenPolicyIdSmallRange
    , tokenNamesMediumRange
    , tokenPolicies
    )
import Cardano.Wallet.Primitive.Types.TokenQuantity.Gen
    ( genTokenQuantitySmall, shrinkTokenQuantitySmall )
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
    , variant
    )
import Test.QuickCheck.Extra
    ( shrinkInterleaved )

import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap

--------------------------------------------------------------------------------
-- Asset identifiers chosen from a small range (to allow collisions)
--------------------------------------------------------------------------------

genAssetIdSmallRange :: Gen AssetId
genAssetIdSmallRange = AssetId
    <$> genTokenPolicyIdSmallRange
    <*> genTokenNameSmallRange

shrinkAssetIdSmallRange :: AssetId -> [AssetId]
shrinkAssetIdSmallRange (AssetId p t) = uncurry AssetId <$> shrinkInterleaved
    (p, shrinkTokenPolicyIdSmallRange)
    (t, shrinkTokenNameSmallRange)

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
        <$> genAssetIdSmallRange
        <*> genTokenQuantitySmall

shrinkTokenMapSmallRange :: TokenMap -> [TokenMap]
shrinkTokenMapSmallRange
    = fmap TokenMap.fromFlatList
    . shrinkList shrinkAssetQuantity
    . TokenMap.toFlatList
  where
    shrinkAssetQuantity (a, q) = shrinkInterleaved
        (a, shrinkAssetIdSmallRange)
        (q, shrinkTokenQuantitySmall)

--------------------------------------------------------------------------------
-- Filtering functions
--------------------------------------------------------------------------------

newtype AssetIdF = AssetIdF AssetId
    deriving (Generic, Eq, Show, Read)

instance Function AssetIdF where
    function = functionMap show read

instance CoArbitrary AssetIdF where
    coarbitrary (AssetIdF AssetId{tokenName, tokenPolicyId}) genB = do
        let n = fromMaybe 0 (elemIndex tokenName tokenNamesMediumRange)
        let m = fromMaybe 0 (elemIndex tokenPolicyId tokenPolicies)
        variant (n+m) genB
