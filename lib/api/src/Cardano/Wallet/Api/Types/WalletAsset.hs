{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Copyright: © 2023 Cardano Foundation
-- License: Apache-2.0
--
-- Representation of the API specification `walletAsset` type.
--
module Cardano.Wallet.Api.Types.WalletAsset
    ( ApiWalletAsset (..)
    )
    where

import Prelude

import Cardano.Wallet.Api.Lib.ApiT
    ( ApiT
    )
import Cardano.Wallet.Api.Lib.Options
    ( DefaultRecord (..)
    )
import Cardano.Wallet.Api.Types.Primitive
    ()
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
import GHC.Generics
    ( Generic
    )
import Numeric.Natural
    ( Natural
    )

import qualified Cardano.Wallet.Primitive.Types.AssetName as W
import qualified Cardano.Wallet.Primitive.Types.TokenPolicyId as W

data ApiWalletAsset = ApiWalletAsset
    { policyId :: !(ApiT W.TokenPolicyId)
    , assetName :: !(ApiT W.AssetName)
    , quantity :: !Natural
    }
    deriving (Data, Eq, Generic, Hashable, Ord, Show, Typeable)
    deriving (FromJSON, ToJSON) via DefaultRecord ApiWalletAsset
    deriving anyclass NFData
