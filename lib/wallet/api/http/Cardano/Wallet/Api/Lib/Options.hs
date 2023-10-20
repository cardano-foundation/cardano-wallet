{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright: © 2018-2022 IOHK
-- License: Apache-2.0

module Cardano.Wallet.Api.Lib.Options
    ( TaggedObjectOptions (..)
    , defaultRecordTypeOptions
    , defaultSumTypeOptions
    , explicitNothingRecordTypeOptions
    , strictRecordTypeOptions
    , taggedSumTypeOptions

    -- * Support for deriving default JSON encodings
    , DefaultRecord (..)
    , DefaultSum (..)
    )
    where

import Prelude

import Data.Aeson
    ( FromJSON (..)
    , GFromJSON
    , GToJSON'
    , Options (..)
    , SumEncoding (..)
    , ToJSON (..)
    , Value
    , Zero
    , camelTo2
    , genericParseJSON
    , genericToJSON
    )
import GHC.Generics
    ( Generic
    , Rep
    )

import qualified Data.Aeson as Aeson

data TaggedObjectOptions = TaggedObjectOptions
    { _tagFieldName :: String
    , _contentsFieldName :: String
    }

defaultSumTypeOptions :: Aeson.Options
defaultSumTypeOptions = Aeson.defaultOptions
    { constructorTagModifier = camelTo2 '_'
    , tagSingleConstructors = True
    }

defaultRecordTypeOptions :: Aeson.Options
defaultRecordTypeOptions = Aeson.defaultOptions
    { fieldLabelModifier = camelTo2 '_' . dropWhile (== '_')
    , omitNothingFields = True
    }

strictRecordTypeOptions :: Aeson.Options
strictRecordTypeOptions = defaultRecordTypeOptions
    { rejectUnknownFields = True
    }

taggedSumTypeOptions :: Aeson.Options -> TaggedObjectOptions -> Aeson.Options
taggedSumTypeOptions base opts = base
    { sumEncoding = TaggedObject (_tagFieldName opts) (_contentsFieldName opts)
    }

explicitNothingRecordTypeOptions :: Aeson.Options
explicitNothingRecordTypeOptions = defaultRecordTypeOptions
    { omitNothingFields = False
    }

-- | Enables deriving of the default API JSON encoding for record types.
--
-- Usage:
--
-- @
-- data ApiMyRecord = ApiMyRecord
--     { foo :: Foo
--     , bar :: Bar
--     , baz :: Baz
--     }
--     deriving (FromJSON, ToJSON) via DefaultRecord ApiMyRecord
-- @
--
newtype DefaultRecord a = DefaultRecord {unDefaultRecord :: a}

instance (Generic a, GFromJSON Zero (Rep a)) => FromJSON (DefaultRecord a)
  where
    parseJSON = fmap DefaultRecord . genericParseJSON defaultRecordTypeOptions

instance (Generic a, GToJSON' Value Zero (Rep a)) => ToJSON (DefaultRecord a)
  where
    toJSON = genericToJSON defaultRecordTypeOptions . unDefaultRecord

-- | Enables deriving of the default API JSON encoding for sum types.
--
-- Usage:
--
-- @
-- data ApiMySum
--     = Foo
--     | Bar
--     | Baz
--     deriving (FromJSON, ToJSON) via DefaultSum ApiMySum
-- @
--
newtype DefaultSum a = DefaultSum {unDefaultSum :: a}

instance (Generic a, GFromJSON Zero (Rep a)) => FromJSON (DefaultSum a)
  where
    parseJSON = fmap DefaultSum . genericParseJSON defaultSumTypeOptions

instance (Generic a, GToJSON' Value Zero (Rep a)) => ToJSON (DefaultSum a)
  where
    toJSON = genericToJSON defaultSumTypeOptions . unDefaultSum
