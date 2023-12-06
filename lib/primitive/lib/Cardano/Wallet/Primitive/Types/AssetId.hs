{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Cardano.Wallet.Primitive.Types.AssetId
    ( AssetId (..)
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.AssetName
    ( AssetName
    )
import Cardano.Wallet.Primitive.Types.TokenPolicyId
    ( TokenPolicyId
    )
import Control.DeepSeq
    ( NFData
    )
import GHC.Generics
    ( Generic
    )

-- | A combination of a token policy identifier and a token name that can be
--   used as a compound identifier.
--
data AssetId = AssetId
    { tokenPolicyId
        :: !TokenPolicyId
    , tokenName
        :: !AssetName
    }
    deriving stock (Eq, Generic, Ord, Read, Show)

instance NFData AssetId
