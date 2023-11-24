{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Pool.Metadata.Types
    ( StakePoolMetadata (..)
    , StakePoolMetadataHash (..)
    , StakePoolMetadataUrl (..)
    , PoolMetadataGCStatus (..)
    , UrlBuilder
    )
where

import Prelude

import Cardano.Pool.Types
    ( PoolId
    , StakePoolTicker (unStakePoolTicker)
    )
import Cardano.Wallet.Primitive.Types.StakePoolMetadata
    ( StakePoolMetadataHash (..)
    , StakePoolMetadataUrl (..)
    )
import Control.Monad
    ( when
    )
import Data.Aeson
    ( FromJSON (parseJSON)
    , withObject
    , (.:)
    , (.:?)
    )
import Data.Aeson.Extra
    ( aesonFromText
    )
import Data.Aeson.Types
    ( ToJSON (toJSON)
    , Value (String)
    )
import Data.Proxy
    ( Proxy (Proxy)
    )
import Data.Text
    ( Text
    )
import Data.Text.Class.Extended
    ( ToText (toText)
    , fromText'
    , fromTextMaybe
    )
import Data.Time.Clock.POSIX
    ( POSIXTime
    )
import Database.Persist.PersistValue.Extended
    ( fromPersistValueFromText
    )
import Database.Persist.Sql
    ( PersistField (..)
    , PersistFieldSql (..)
    )
import GHC.Generics
    ( Generic
    )
import Network.HTTP.Client
    ( HttpException
    )
import Network.URI
    ( URI
    )
import Web.HttpApiData
    ( FromHttpApiData (parseUrlPiece)
    , ToHttpApiData (toUrlPiece)
    )
import Web.PathPieces
    ( PathPiece (..)
    )

import qualified Data.Text as T

-- Status encoding of the metadata GC thread, which queries
-- the SMASH server for delisted pools.
data PoolMetadataGCStatus
    = NotApplicable
    | NotStarted
    | Restarting POSIXTime -- shows last GC before restart occurred
    | HasRun POSIXTime     -- shows last GC
    deriving (Eq, Show, Generic)

instance PersistField StakePoolMetadataHash where
    toPersistValue = toPersistValue . toText
    fromPersistValue = fromPersistValueFromText

instance PersistFieldSql StakePoolMetadataHash where
    sqlType _ = sqlType (Proxy @Text)

instance Read StakePoolMetadataHash where
    readsPrec _ = error "readsPrec stub needed for persistent"

instance ToHttpApiData StakePoolMetadataHash where
    toUrlPiece = toText

instance FromHttpApiData StakePoolMetadataHash where
    parseUrlPiece = fromText'

instance ToJSON StakePoolMetadataHash where
    toJSON = String . toText

instance FromJSON StakePoolMetadataHash where
    parseJSON = aesonFromText "StakePoolMetadataHash"

instance PathPiece StakePoolMetadataHash where
    fromPathPiece = fromTextMaybe
    toPathPiece = toText

instance PersistField StakePoolMetadataUrl where
    toPersistValue = toPersistValue . toText
    fromPersistValue = fromPersistValueFromText

instance PersistFieldSql StakePoolMetadataUrl where
    sqlType _ = sqlType (Proxy @Text)

instance Read StakePoolMetadataUrl where
    readsPrec _ = error "readsPrec stub needed for persistent"

instance ToHttpApiData StakePoolMetadataUrl where
    toUrlPiece = toText

instance FromHttpApiData StakePoolMetadataUrl where
    parseUrlPiece = fromText'

instance ToJSON StakePoolMetadataUrl where
    toJSON = String . toText

instance FromJSON StakePoolMetadataUrl where
    parseJSON = aesonFromText "StakePoolMetadataUrl"

instance PathPiece StakePoolMetadataUrl where
    fromPathPiece = fromTextMaybe
    toPathPiece = toText

-- | A type-alias to ease signatures
type UrlBuilder
    =  PoolId
    -> StakePoolMetadataUrl
    -> StakePoolMetadataHash
    -> Either HttpException URI

-- | Information about a stake pool.
--
-- The metadata information is not used directly by cardano-wallet, but rather
-- passed straight through to API consumers.
data StakePoolMetadata = StakePoolMetadata
    { ticker :: StakePoolTicker
    -- ^ Very short human-readable ID for the stake pool.
    , name :: Text
    -- ^ Name of the stake pool.
    , description :: Maybe Text
    -- ^ Short description of the stake pool.
    , homepage :: Text
    -- ^ Absolute URL for the stake pool's homepage link.
    } deriving (Eq, Ord, Show, Generic)

instance FromJSON StakePoolMetadata where
    parseJSON = withObject "StakePoolMetadta" $ \obj -> do
        ticker <- obj .: "ticker"
        let tickerLen = T.length . unStakePoolTicker $ ticker
        when (tickerLen > 5 || tickerLen < 3)
            $ fail "ticker length must be between 3 and 5 characters"
        name <- obj .: "name"
        when (T.length name > 50)
            $ fail "name exceeds max length of 50 chars"
        description <- obj .:? "description"
        when ((T.length <$> description) > Just 255)
            $ fail "description exceeds max length of 255 characters"
        homepage <- obj .: "homepage"
        pure StakePoolMetadata{ticker,name,description,homepage}
