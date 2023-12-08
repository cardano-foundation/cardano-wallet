module Cardano.Wallet.Primitive.Types.AssetId.Gen
    ( genAssetId
    , genAssetIdLargeRange
    , shrinkAssetId
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.AssetId
    ( AssetId (..)
    )
import Cardano.Wallet.Primitive.Types.AssetName.Gen
    ( genAssetName
    , genAssetNameLargeRange
    , shrinkAssetName
    )
import Cardano.Wallet.Primitive.Types.TokenPolicyId.Gen
    ( genTokenPolicyId
    , genTokenPolicyIdLargeRange
    , shrinkTokenPolicyId
    )
import Test.QuickCheck
    ( Gen
    )
import Test.QuickCheck.Extra
    ( genSized2With
    , shrinkInterleaved
    )

--------------------------------------------------------------------------------
-- Asset identifiers chosen from a range that depends on the size parameter
--------------------------------------------------------------------------------

genAssetId :: Gen AssetId
genAssetId = genSized2With AssetId genTokenPolicyId genAssetName

shrinkAssetId :: AssetId -> [AssetId]
shrinkAssetId (AssetId p t) = uncurry AssetId <$> shrinkInterleaved
    (p, shrinkTokenPolicyId)
    (t, shrinkAssetName)

--------------------------------------------------------------------------------
-- Asset identifiers chosen from a large range (to minimize collisions)
--------------------------------------------------------------------------------

genAssetIdLargeRange :: Gen AssetId
genAssetIdLargeRange = AssetId
    <$> genTokenPolicyIdLargeRange
    <*> genAssetNameLargeRange
