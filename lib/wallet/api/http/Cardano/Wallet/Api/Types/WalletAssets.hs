{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright: © 2023 Cardano Foundation
-- License: Apache-2.0
--
-- Representation of the API specification `walletAssets` type.
--
module Cardano.Wallet.Api.Types.WalletAssets
    ( ApiWalletAssets (..)
    , fromTokenMap
    , toTokenMap
    )
    where

import Prelude

import Cardano.Wallet.Api.Lib.ApiT
    ( ApiT (ApiT)
    )
import Cardano.Wallet.Api.Types.WalletAsset
    ( ApiWalletAsset (..)
    )
import Control.DeepSeq
    ( NFData (..)
    )
import Data.Aeson.Types
    ( FromJSON (..)
    , ToJSON (..)
    )
import Data.Data
    ( Data
    )
import Data.Hashable
    ( Hashable
    )
import Data.Typeable
    ( Typeable
    )
import GHC.Exts
    ( IsList (fromList, toList)
    )
import GHC.Generics
    ( Generic
    )

import qualified Cardano.Wallet.Primitive.Types.AssetId as W
    ( AssetId (AssetId)
    )
import qualified Cardano.Wallet.Primitive.Types.TokenMap as W
    ( TokenMap
    )
import qualified Cardano.Wallet.Primitive.Types.TokenMap as W.TokenMap
import qualified Cardano.Wallet.Primitive.Types.TokenQuantity as W
    ( TokenQuantity (TokenQuantity)
    )

newtype ApiWalletAssets = ApiWalletAssets [ApiWalletAsset]
    deriving (Data, Eq, Generic, Ord, Show, Typeable)
    deriving newtype (Hashable, IsList, Semigroup, Monoid, FromJSON, ToJSON)
    deriving anyclass NFData

fromTokenMap :: W.TokenMap -> ApiWalletAssets
fromTokenMap = fromList . fmap f . W.TokenMap.toFlatList
  where
    f (W.AssetId p a, W.TokenQuantity q) = ApiWalletAsset (ApiT p) (ApiT a) q

toTokenMap :: ApiWalletAssets -> W.TokenMap
toTokenMap = W.TokenMap.fromFlatList . fmap f . toList
  where
    f (ApiWalletAsset (ApiT p) (ApiT a) q) = (W.AssetId p a, W.TokenQuantity q)
