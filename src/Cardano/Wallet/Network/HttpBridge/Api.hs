{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: MIT
--
-- An API specification for the Cardano HTTP Bridge.
module Cardano.Wallet.Network.HttpBridge.Api
    ( Api
    , api
    , ApiT(..)
    , EpochIndex (..)
    , NetworkName (..)
    ) where

import Prelude

import Cardano.Wallet.Binary
    ( decodeBlock, decodeBlockHeader )
import Cardano.Wallet.Primitive.Types
    ( Block, BlockHeader )
import Crypto.Hash.Algorithms
    ( Blake2b_256 )
import Data.Proxy
    ( Proxy (..) )
import Data.Text
    ( Text )
import Data.Word
    ( Word64 )
import Servant.API
    ( (:<|>), (:>), Capture, Get, ToHttpApiData (..) )
import Servant.Extra.ContentTypes
    ( CBOR, ComputeHash, FromCBOR (..), Hash, Packed, WithHash )


api :: Proxy Api
api = Proxy

type Api
    =    GetBlockByHash
    :<|> GetEpochById
    :<|> GetTipBlockHeader

-- | Retrieve a block identified by the unique hash of its header.
type GetBlockByHash
    =  Capture "networkName" NetworkName
    :> "block"
    :> Capture "blockHeaderHash" (Hash Blake2b_256 (ApiT BlockHeader))
    :> Get '[CBOR] (ApiT Block)

-- | Retrieve all the blocks for the epoch identified by the given integer ID.
type GetEpochById
    =  Capture "networkName" NetworkName
    :> "epoch"
    :> Capture "epochId" EpochIndex
    :> Get '[Packed CBOR] [ApiT Block]

-- | Retrieve the header of the latest known block.
type GetTipBlockHeader
    = Capture "networkName" NetworkName
    :> "tip"
    :> Get '[ComputeHash Blake2b_256 CBOR] (WithHash Blake2b_256 (ApiT BlockHeader))

newtype ApiT a = ApiT { getApiT :: a } deriving (Show)

instance FromCBOR (ApiT Block) where
    fromCBOR = ApiT <$> decodeBlock

instance FromCBOR (ApiT BlockHeader) where
    fromCBOR = ApiT <$> decodeBlockHeader

-- | Represents a unique epoch.
newtype EpochIndex = EpochIndex
    { getEpochIndex :: Word64
    } deriving (Eq, Show)

instance ToHttpApiData (EpochIndex) where
    toUrlPiece = toUrlPiece . getEpochIndex

-- | Represents the name of a Cardano network.
newtype NetworkName = NetworkName
    { getNetworkName :: Text
    } deriving (Eq, Show)

instance ToHttpApiData NetworkName where
    toUrlPiece = getNetworkName
