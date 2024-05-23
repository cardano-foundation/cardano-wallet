{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Copyright:
--    © 2018-2023 IOHK
--    © 2023-2024 Cardano Foundation
-- License:
--    Apache-2.0
--
-- This module provides API types and functions relating to eras.
--
module Cardano.Wallet.Api.Types.Era
    ( ApiEra (..)
    , toApiEra
    , fromApiEra
    , allRecentEras
    )
    where

import Cardano.Api
    ( AnyCardanoEra (AnyCardanoEra)
    , CardanoEra (..)
    )
import Control.DeepSeq
    ( NFData
    )
import Data.Aeson
    ( FromJSON (parseJSON)
    , Options (constructorTagModifier)
    , ToJSON (toJSON)
    , camelTo2
    , genericParseJSON
    , genericToJSON
    )
import Data.Data
    ( Data
    )
import Data.Eq
    ( Eq
    )
import Data.Function
    ( ($)
    , (.)
    )
import Data.List
    ( drop
    )
import Data.Ord
    ( Ord
    )
import Data.Set
    ( Set
    )
import GHC.Generics
    ( Generic
    )
import Prelude
    ( Bounded
    , Enum
    )
import Text.Show
    ( Show
    )

import qualified Data.Aeson as Aeson
import qualified Data.Set as Set
import qualified Internal.Cardano.Write.Tx as Write
    ( allRecentEras
    , toAnyCardanoEra
    )

data ApiEra
    = ApiByron
    | ApiShelley
    | ApiAllegra
    | ApiMary
    | ApiAlonzo
    | ApiBabbage
    | ApiConway
    deriving (Data, Show, Eq, Generic, Enum, Ord, Bounded)
    deriving anyclass NFData

instance FromJSON ApiEra where
    parseJSON = genericParseJSON $ Aeson.defaultOptions
        { constructorTagModifier = drop 4 . camelTo2 '_' }

instance ToJSON ApiEra where
    toJSON = genericToJSON $ Aeson.defaultOptions
        { constructorTagModifier = drop 4 . camelTo2 '_' }

toApiEra :: AnyCardanoEra -> ApiEra
toApiEra = \case
    AnyCardanoEra ByronEra -> ApiByron
    AnyCardanoEra ShelleyEra -> ApiShelley
    AnyCardanoEra AllegraEra -> ApiAllegra
    AnyCardanoEra MaryEra -> ApiMary
    AnyCardanoEra AlonzoEra -> ApiAlonzo
    AnyCardanoEra BabbageEra -> ApiBabbage
    AnyCardanoEra ConwayEra -> ApiConway

fromApiEra :: ApiEra -> AnyCardanoEra
fromApiEra = \case
    ApiByron -> AnyCardanoEra ByronEra
    ApiShelley -> AnyCardanoEra ShelleyEra
    ApiAllegra -> AnyCardanoEra AllegraEra
    ApiMary -> AnyCardanoEra MaryEra
    ApiAlonzo -> AnyCardanoEra AlonzoEra
    ApiBabbage -> AnyCardanoEra BabbageEra
    ApiConway -> AnyCardanoEra ConwayEra

-- | The complete set of recent eras.
--
allRecentEras :: Set ApiEra
allRecentEras = Set.map (toApiEra . Write.toAnyCardanoEra) Write.allRecentEras
