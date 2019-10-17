{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: Apache-2.0
--
-- This module contains instances and types necessary for storing wallets in a
-- SQL database with Persistent.
--
-- It's in a separate module due to the GHC stage restriction.
--
-- The ToJSON/FromJSON and Read instance orphans exist due to class constraints
-- on Persistent functions.

module Cardano.Pool.DB.Sqlite.Types
    ( sqlSettings'
    , BlockId (..)
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types
    ( EpochLength (..)
    , EpochNo
    , Hash (..)
    , PoolId
    , SlotId (..)
    , SlotNo
    , flatSlot
    , fromFlatSlot
    )
import Control.Monad
    ( (>=>) )
import Data.Aeson
    ( FromJSON, ToJSON, genericParseJSON, genericToJSON )
import Data.Bifunctor
    ( first )
import Data.Proxy
    ( Proxy (..) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( FromText (..)
    , TextDecodingError (..)
    , ToText (..)
    , fromTextMaybe
    , getTextDecodingError
    )
import Data.Word
    ( Word16, Word64 )
import Database.Persist.Sqlite
    ( PersistField (..), PersistFieldSql (..), PersistValue )
import Database.Persist.TH
    ( MkPersistSettings (..), sqlSettings )
import GHC.Generics
    ( Generic )
import Web.HttpApiData
    ( FromHttpApiData (..), ToHttpApiData (..) )
import Web.PathPieces
    ( PathPiece (..) )

import qualified Data.Aeson as Aeson
import qualified Data.Text as T

----------------------------------------------------------------------------

-- | Settings for generating the Persistent types.
sqlSettings' :: MkPersistSettings
sqlSettings' = sqlSettings { mpsPrefixFields = False }

----------------------------------------------------------------------------
-- Helper functions

-- | 'fromText' but with a simpler error type.
fromText' :: FromText a => Text -> Either Text a
fromText' = first (T.pack . getTextDecodingError) . fromText

-- | 'fromPersistValue' defined in terms of 'fromText'
fromPersistValueFromText :: FromText a => PersistValue -> Either Text a
fromPersistValueFromText = fromPersistValue >=> fromTextWithErr
    where fromTextWithErr = first ("not a valid value: " <>) . fromText'

----------------------------------------------------------------------------
-- PoolId

instance PersistField PoolId where
    toPersistValue = toPersistValue . toText
    fromPersistValue = fromPersistValueFromText

instance PersistFieldSql PoolId where
    sqlType _ = sqlType (Proxy @Text)

instance Read PoolId where
    readsPrec _ = error "readsPrec stub needed for persistent"

instance PathPiece PoolId where
    fromPathPiece = fromTextMaybe
    toPathPiece = toText

----------------------------------------------------------------------------
-- BlockId

-- Wraps Hash "BlockHeader" because the persistent dsl doesn't like it raw.
newtype BlockId = BlockId { getBlockId :: Hash "BlockHeader" }
    deriving (Show, Eq, Ord, Generic)

instance PersistField BlockId where
    toPersistValue = toPersistValue . toText . getBlockId
    fromPersistValue = fmap BlockId <$> fromPersistValueFromText

instance PersistFieldSql BlockId where
    sqlType _ = sqlType (Proxy @Text)

instance Read BlockId where
    readsPrec _ = error "readsPrec stub needed for persistent"

----------------------------------------------------------------------------
-- SlotId

-- | As a short-to-medium term solution of persisting 'SlotId', we use
-- 'flatSlot' with an artificial epochLength. I.e. /not the same epochLength as
-- the blockchain/. This is just for the sake of storing the 64 bit epoch and
-- the 16 bit slot inside a single 64-bit field.
artificialEpochLength :: EpochLength
artificialEpochLength = EpochLength $ fromIntegral (maxBound :: Word16)

instance PersistFieldSql SlotId where
    sqlType _ = sqlType (Proxy @Word64)

instance PersistField SlotId where
    toPersistValue = toPersistValue . flatSlot artificialEpochLength
    fromPersistValue = fmap (fromFlatSlot artificialEpochLength) . fromPersistValue

instance ToJSON SlotId where toJSON = genericToJSON Aeson.defaultOptions
instance FromJSON SlotId where parseJSON = genericParseJSON Aeson.defaultOptions
instance ToJSON SlotNo where toJSON = genericToJSON Aeson.defaultOptions
instance FromJSON SlotNo where parseJSON = genericParseJSON Aeson.defaultOptions
instance ToJSON EpochNo where toJSON = genericToJSON Aeson.defaultOptions
instance FromJSON EpochNo where parseJSON = genericParseJSON Aeson.defaultOptions
instance ToHttpApiData SlotId where
    toUrlPiece = error "toUrlPiece stub needed for persistent"
instance FromHttpApiData SlotId where
    parseUrlPiece = error "parseUrlPiece stub needed for persistent"
instance PathPiece SlotId where
    toPathPiece = error "toPathPiece stub needed for persistent"
    fromPathPiece = error "fromPathPiece stub needed for persistent"
