module Cardano.Wallet.Primitive.Types.TokenBundle.Gen
    ( genAssetId
    , genTokenBundle
    , genTokenName
    , genTokenPolicyId
    , genTokenQuantity
    , shrinkTokenBundle
    , shrinkTokenPolicyId
    , shrinkTokenQuantity
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.TokenBundle
    ( AssetId (..), TokenBundle )
import Cardano.Wallet.Primitive.Types.TokenPolicy
    ( TokenName, TokenPolicyId )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity (..) )
import Cardano.Wallet.Unsafe
    ( unsafeFromHex )
import Test.QuickCheck
    ( Gen )

import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TB
import qualified Cardano.Wallet.Primitive.Types.TokenPolicy as TP
import qualified Cardano.Wallet.Primitive.Types.TokenQuantity as TQ
import qualified Data.Text as T

genAssetId :: Gen AssetId
genAssetId = AssetId <$> genTokenPolicyId <*> genTokenName

genTokenName :: Gen TokenName
genTokenName = mkTokenName <$> elements ['A' .. 'D']
  where
    mkTokenName = TP.mkTokenName . ("Token" `T.snoc`)

genTokenPolicyId = dummyTokenPolicyId <$> elements ['A' .. 'D']
