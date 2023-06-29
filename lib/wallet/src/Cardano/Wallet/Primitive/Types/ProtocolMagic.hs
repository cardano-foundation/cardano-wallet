{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2018-2022 IOHK
-- License: Apache-2.0
--
-- Provides the 'ProtocolMagic' type and related constants.
module Cardano.Wallet.Primitive.Types.ProtocolMagic
    ( ProtocolMagic (..)
    , mainnetMagic
    , testnetMagic
    , magicSNetworkId
    ) where

import Prelude

import Cardano.Wallet.Read.NetworkId
    ( SNetworkId (..)
    , fromSNat
    )
import Control.DeepSeq
    ( NFData (..)
    )
import Data.Aeson
    ( FromJSON (..)
    , ToJSON (..)
    )
import Data.Int
    ( Int32
    )
import Data.Proxy
    ( Proxy (..)
    )
import Data.Text.Class
    ( FromText (..)
    , ToText (..)
    )
import GHC.Generics
    ( Generic
    )
import GHC.TypeLits
    ( KnownNat
    , natVal
    )
import Numeric.Natural
    ( Natural
    )

import qualified Data.Text as T

-- | Magic constant associated with a given network.
newtype ProtocolMagic = ProtocolMagic {getProtocolMagic :: Int32}
    deriving (Generic, Show, Eq, NFData, FromJSON, ToJSON)

instance ToText ProtocolMagic where
    toText (ProtocolMagic pm) = T.pack (show pm)

instance FromText ProtocolMagic where
    fromText = fmap (ProtocolMagic . fromIntegral @Natural) . fromText

-- | Hard-coded protocol magic for the Byron MainNet
mainnetMagic :: ProtocolMagic
mainnetMagic = ProtocolMagic 764824073

-- | Derive testnet magic from a type-level Nat
testnetMagic :: forall pm. KnownNat pm => ProtocolMagic
testnetMagic = ProtocolMagic $ fromIntegral $ natVal $ Proxy @pm

magicSNetworkId :: SNetworkId n -> ProtocolMagic
magicSNetworkId SMainnet = ProtocolMagic 764824073
magicSNetworkId (STestnet pm) = ProtocolMagic $ fromIntegral $ fromSNat pm
