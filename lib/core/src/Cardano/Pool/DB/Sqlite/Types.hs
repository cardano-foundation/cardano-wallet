{-# LANGUAGE DataKinds #-}
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

module Cardano.Pool.DB.Sqlite.Types where

import Prelude

import Cardano.Wallet.Primitive.Types
    ( EpochLength (..), PoolId, SlotId (..), flatSlot, fromFlatSlot )
import Control.Monad
    ( (>=>) )
import Data.Aeson
    ( FromJSON (..)
    , ToJSON (..)
    , defaultOptions
    , genericParseJSON
    , genericToJSON
    )
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
import Web.PathPieces
    ( PathPiece (..) )

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
-- SlotId

instance PersistFieldSql SlotId where
    sqlType _ = sqlType (Proxy @Word64)

-- | As a short-to-medium term solution of persisting 'SlotId', we use
-- 'flatSlot' with an artificial epochLength. I.e. /not the same epochLength as
-- the blockchain/. This is just for the sake of storing the 64 bit epoch and
-- the 16 bit slot inside a single 64-bit field.
artificialEpochLength :: EpochLength
artificialEpochLength = EpochLength $ fromIntegral (maxBound :: Word16)

instance PersistField SlotId where
    toPersistValue = toPersistValue . flatSlot artificialEpochLength
    fromPersistValue = fmap (fromFlatSlot artificialEpochLength) . fromPersistValue

instance ToJSON SlotId where
    toJSON = genericToJSON defaultOptions

instance FromJSON SlotId where
    parseJSON = genericParseJSON defaultOptions

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
