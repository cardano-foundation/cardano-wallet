{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: Apache-2.0
--
-- Definitions of types used in the Jörmungandr REST API.
--
module Cardano.Wallet.Jormungandr.Api.Types
    (
    -- * Polymorphic Types
      ApiT (..)

    -- * API types
    , AccountId (..)
    , AccountState (..)
    , ApiStakeDistribution (..)
    , BlockId (..)
    , StakeApiResponse (..)

    -- * Content types
    , Hex
    , JormungandrBinary

    ) where

import Prelude

import Cardano.Wallet.Jormungandr.Binary
    ( Block, getBlock, runGet )
import Cardano.Wallet.Primitive.Types
    ( EpochNo (..)
    , Hash (..)
    , PoolId (..)
    , SealedTx (..)
    , ShowFmt (..)
    , unsafeEpochNo
    )
import Control.Applicative
    ( many )
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
    ( bimap )
import Data.Binary.Get
    ( getByteString )
import Data.ByteArray.Encoding
    ( Base (Base16), convertFromBase )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Quantity (..) )
import Data.Text.Class
    ( FromText (..), ToText (..) )
import Data.Word
    ( Word64 )
import GHC.Generics
    ( Generic )
import Servant.API
    ( Accept (..), MimeRender (..), MimeUnrender (..), ToHttpApiData (..) )

import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Servant.API.ContentTypes as Servant

newtype AccountId = AccountId { getAccountId :: Hash "Account" }
    deriving (Eq, Show)

data AccountState = AccountState
    { currentBalance
        :: !(Quantity "lovelace" Word64)
    , stakePools
        :: [(PoolId, Quantity "stake-pool-ratio" Word64)]
    , totalTransactionCount
        :: !(Quantity "transaction-count" Word64)
    } deriving (Eq, Show)

newtype BlockId = BlockId { getBlockId :: Hash "BlockHeader" }

data StakeApiResponse = StakeApiResponse
    { epoch :: ApiT EpochNo
    , stake :: ApiStakeDistribution
    } deriving (Show, Eq, Generic)

data ApiStakeDistribution = ApiStakeDistribution
    { dangling :: ApiT (Quantity "lovelace" Word64)
    , pools :: [(ApiT PoolId, ApiT (Quantity "lovelace" Word64))]
    , unassigned :: ApiT (Quantity "lovelace" Word64)
    } deriving (Eq, Show, Generic)

instance ToHttpApiData BlockId where
    toUrlPiece = toText . getBlockId

instance ToHttpApiData AccountId where
    toUrlPiece = toText . getAccountId

instance MimeUnrender JormungandrBinary [BlockId] where
    mimeUnrender _ =
        pure . fmap (BlockId . Hash) . runGet (many $ getByteString 32)

instance MimeUnrender Hex BlockId where
    mimeUnrender _ bs =
        BlockId . Hash <$> convertFromBase Base16 (BL.toStrict bs)

-- | Polymorphic wrapper type to put around primitive types and, 3rd party lib
-- types to avoid defining orphan instances and/or, undesirable instances on
-- primitive types. It helps to keep a nice separation of concerns between the
-- API layer and other modules.
newtype ApiT a =
    ApiT { getApiT :: a }
    deriving (Generic, Show, Eq)

{-------------------------------------------------------------------------------
                                Content Types
-------------------------------------------------------------------------------}

-- | Represents the binary format of Jörmungandr.
data JormungandrBinary

instance Accept JormungandrBinary where
    contentType _ = contentType $ Proxy @Servant.OctetStream

instance MimeUnrender JormungandrBinary Block where
    mimeUnrender _ = pure . runGet getBlock

instance MimeRender JormungandrBinary SealedTx where
    mimeRender _ (SealedTx bytes) = BL.fromStrict bytes

data Hex

-- | Represents data rendered to hexadecimal text.
instance Accept Hex where
    contentType _ = contentType $ Proxy @Servant.PlainText

{-------------------------------------------------------------------------------
                              FromJson instances
-------------------------------------------------------------------------------}

instance FromJSON StakeApiResponse where
    parseJSON = genericParseJSON defaultOptions

instance FromJSON AccountState where
    parseJSON = fmap fromApiAccountState . parseJSON
instance ToJSON AccountState where
    toJSON = toJSON . toApiAccountState

data ApiAccountState = ApiAccountState
    { counter
        :: !(ApiT (Quantity "transaction-count" Word64))
    , delegation
        :: !ApiDelegationState
    , value
        :: !(ApiT (Quantity "lovelace" Word64))
    } deriving (Eq, Show, Generic)

newtype ApiDelegationState = ApiDelegationState
    { pools
        :: [(ApiT PoolId, ApiT (Quantity "stake-pool-ratio" Word64))]
    } deriving (Eq, Show, Generic)

instance FromJSON ApiAccountState where
    parseJSON = genericParseJSON defaultOptions
instance ToJSON ApiAccountState where
    toJSON = genericToJSON defaultOptions

instance FromJSON ApiDelegationState where
    parseJSON = genericParseJSON defaultOptions
instance ToJSON ApiDelegationState where
    toJSON = genericToJSON defaultOptions

fromApiAccountState :: ApiAccountState -> AccountState
fromApiAccountState a = AccountState
    { currentBalance
        = getApiT $ value a
    , stakePools
        = bimap getApiT getApiT <$> pools (delegation a :: ApiDelegationState)
    , totalTransactionCount
        = getApiT $ counter a
    }

toApiAccountState :: AccountState -> ApiAccountState
toApiAccountState a = ApiAccountState
    { value
        = ApiT $ currentBalance a
    , delegation
        = ApiDelegationState $ bimap ApiT ApiT <$> stakePools a
    , counter
        = ApiT $ totalTransactionCount a
    }

instance FromJSON (ApiStakeDistribution) where
    parseJSON = genericParseJSON defaultOptions

instance FromJSON (ApiT (Quantity "lovelace" Word64)) where
    parseJSON = fmap (ApiT . Quantity) . parseJSON
instance ToJSON (ApiT (Quantity "lovelace" Word64)) where
    toJSON (ApiT (Quantity q)) = toJSON q

instance FromJSON (ApiT (Quantity "stake-pool-ratio" Word64)) where
    parseJSON = fmap (ApiT . Quantity) . parseJSON
instance ToJSON (ApiT (Quantity "stake-pool-ratio" Word64)) where
    toJSON (ApiT (Quantity q)) = toJSON q

instance FromJSON (ApiT (Quantity "transaction-count" Word64)) where
    parseJSON = fmap (ApiT . Quantity) . parseJSON
instance ToJSON (ApiT (Quantity "transaction-count" Word64)) where
    toJSON (ApiT (Quantity q)) = toJSON q

instance FromJSON (ApiT EpochNo) where
    parseJSON = fmap (ApiT . unsafeEpochNo) . parseJSON

instance FromJSON (ApiT PoolId) where
    parseJSON = parseJSON >=> eitherToParser . bimap ShowFmt ApiT . fromText
instance ToJSON (ApiT PoolId) where
    toJSON = toJSON . toText . getApiT

eitherToParser :: Show s => Either s a -> Aeson.Parser a
eitherToParser = either (fail . show) pure
