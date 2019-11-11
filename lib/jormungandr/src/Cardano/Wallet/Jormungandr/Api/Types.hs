{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Jormungandr.Api.Types
    ( ApiStakeDistribution (..)
    , ApiT (..)
    , BlockId (..)
    , Hex
    , JormungandrBinary
    , StakeApiResponse (..)
    ) where

import Prelude

import Cardano.Wallet.Jormungandr.Binary
    ( Block
    , FragmentSpec (..)
    , getBlock
    , putSignedTx
    , runGet
    , runPut
    , withHeader
    )
import Cardano.Wallet.Primitive.Types
    ( EpochNo (..), Hash (..), PoolId (..), ShowFmt (..), Tx (..), TxWitness )
import Control.Applicative
    ( many )
import Control.Arrow
    ( left )
import Data.Aeson
    ( FromJSON (..), defaultOptions, genericParseJSON )
import Data.Binary.Get
    ( getByteString )
import Data.ByteArray.Encoding
    ( Base (Base16), convertFromBase, convertToBase )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Quantity (..) )
import Data.Text.Class
    ( FromText (..) )
import Data.Text.Encoding
    ( decodeUtf8 )
import Data.Word
    ( Word64 )
import GHC.Generics
    ( Generic )
import Servant.API
    ( Accept (..), MimeRender (..), MimeUnrender (..), ToHttpApiData (..) )

import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Servant.API.ContentTypes as Servant

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
    toUrlPiece (BlockId (Hash bytes)) = decodeUtf8 $ convertToBase Base16 bytes

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

instance MimeRender JormungandrBinary (Tx, [TxWitness]) where
    mimeRender _ (Tx _ ins outs, wits) =
        runPut $ withHeader FragmentTransaction $ putSignedTx ins outs wits

data Hex

-- | Represents data rendered to hexadecimal text.
instance Accept Hex where
    contentType _ = contentType $ Proxy @Servant.PlainText

{-------------------------------------------------------------------------------
                              FromJson instances
-------------------------------------------------------------------------------}

instance FromJSON StakeApiResponse where
    parseJSON = genericParseJSON defaultOptions

instance FromJSON (ApiStakeDistribution) where
    parseJSON = genericParseJSON defaultOptions

instance FromJSON (ApiT (Quantity "lovelace" Word64)) where
    parseJSON = fmap (ApiT . Quantity) . parseJSON

instance FromJSON (ApiT EpochNo) where
    parseJSON = fmap (ApiT . EpochNo) . parseJSON

instance FromJSON (ApiT PoolId) where
    parseJSON val = do
        txt <- parseJSON val
        m <- eitherToParser $ left ShowFmt $ fromText txt
        return $ ApiT m

eitherToParser :: Show s => Either s a -> Aeson.Parser a
eitherToParser = either (fail . show) pure
