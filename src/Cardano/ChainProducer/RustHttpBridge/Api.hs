{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: MIT
--
-- An API specification for the Cardano HTTP Bridge.
module Cardano.ChainProducer.RustHttpBridge.Api
    ( Api
    , api
    , Block (..)
    , BlockHeader (..)
    , EpochIndex (..)
    , NetworkName (..)
    ) where

import Cardano.Wallet.Binary
    ( decodeBlock, decodeBlockHeader )
import Crypto.Hash.Algorithms
    ( Blake2b_256 )
import Data.Proxy
    ( Proxy (..) )
import Data.Text
    ( Text )
import Data.Word
    ( Word64 )
import Prelude
import Servant.API
    ( (:<|>), (:>), Capture, Get, ToHttpApiData (..) )
import Servant.Extra.ContentTypes
    ( CBOR, ComputeHash, FromCBOR (..), Hash, Packed, WithHash )

import qualified Cardano.Wallet.Primitive as Primitive

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
    :> Capture "blockHeaderHash" (Hash Blake2b_256 BlockHeader)
    :> Get '[CBOR] Block

-- | Retrieve all the blocks for the epoch identified by the given integer ID.
type GetEpochById
    =  Capture "networkName" NetworkName
    :> "epoch"
    :> Capture "epochId" EpochIndex
    :> Get '[Packed CBOR] [Block]

-- | Retrieve the header of the latest known block.
type GetTipBlockHeader
    = Capture "networkName" NetworkName
    :> "tip"
    :> Get '[ComputeHash Blake2b_256 CBOR] (WithHash Blake2b_256 BlockHeader)

-- | Represents a block.
newtype Block = Block
    { getBlock :: Primitive.Block
    } deriving Eq

instance FromCBOR Block where
    fromCBOR = Block <$> decodeBlock

-- | Represents a block header.
newtype BlockHeader = BlockHeader
    { getBlockHeader :: Primitive.BlockHeader
    } deriving Eq

instance FromCBOR BlockHeader where
    fromCBOR = BlockHeader <$> decodeBlockHeader

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
