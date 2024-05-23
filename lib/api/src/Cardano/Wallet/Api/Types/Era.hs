{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

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
    )
    where

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
