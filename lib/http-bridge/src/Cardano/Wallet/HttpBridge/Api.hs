{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: Apache-2.0
--
-- An API specification for the Cardano HTTP Bridge.
module Cardano.Wallet.HttpBridge.Api
    ( Api
    , GetBlockByHash
    , GetTipBlockHeader
    , GetEpochById
    , PostSignedTx
    , api
    , ApiT(..)
    , EpochIndex (..)
    , NetworkName (..)
    ) where

import Prelude

import Cardano.Byron.Codec.Cbor
    ( decodeBlock, decodeBlockHeader, decodeSignedTx, encodeSignedTx )
import Cardano.Wallet.HttpBridge.Compatibility
    ( byronEpochLength )
import Cardano.Wallet.HttpBridge.Primitive.Types
    ( Tx (..) )
import Cardano.Wallet.Primitive.Types
    ( Block (..), BlockHeader, TxWitness )
import Crypto.Hash.Algorithms
    ( Blake2b_256 )
import Data.Aeson
    ( FromJSON (parseJSON), ToJSON (toJSON), object, withObject, (.:), (.=) )
import Data.ByteArray.Encoding
    ( Base (Base64), convertFromBase, convertToBase )
import Data.ByteString
    ( ByteString )
import Data.Proxy
    ( Proxy (..) )
import Data.Text
    ( Text )
import Data.Word
    ( Word64 )
import Servant.API
    ( (:<|>)
    , (:>)
    , Capture
    , Get
    , JSON
    , NoContent
    , Post
    , ReqBody
    , ToHttpApiData (..)
    )
import Servant.Extra.ContentTypes
    ( CBOR, ComputeHash, FromCBOR (..), Hash, Packed, WithHash )

import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Write as CBOR
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL


api :: Proxy Api
api = Proxy

type Api
    =    GetBlockByHash
    :<|> GetEpochById
    :<|> GetTipBlockHeader
    :<|> PostSignedTx

-- | Retrieve a block identified by the unique hash of its header.
type GetBlockByHash
    =  Capture "networkName" NetworkName
    :> "block"
    :> Capture "blockHeaderHash" (Hash Blake2b_256 (ApiT BlockHeader))
    :> Get '[CBOR] (ApiT (Block Tx))

-- | Retrieve all the blocks for the epoch identified by the given integer ID.
type GetEpochById
    =  Capture "networkName" NetworkName
    :> "epoch"
    :> Capture "epochId" EpochIndex
    :> Get '[Packed CBOR] [ApiT (Block Tx)]

-- | Retrieve the header of the latest known block.
type GetTipBlockHeader
    = Capture "networkName" NetworkName
    :> "tip"
    :> Get '[ComputeHash Blake2b_256 CBOR]
            (WithHash Blake2b_256 (ApiT BlockHeader))

type PostSignedTx
    =  Capture "networkName" NetworkName
    :> "txs"
    :> "signed"
    :> ReqBody '[JSON] (ApiT (Tx, [TxWitness]))
    :> Post '[NoContent] NoContent

newtype ApiT a = ApiT { getApiT :: a } deriving (Show)

instance FromCBOR (ApiT (Block Tx)) where
    fromCBOR = ApiT . hoist <$> decodeBlock byronEpochLength
      where
        hoist (Block h txs) = Block h (uncurry Tx <$> txs)

instance FromCBOR (ApiT BlockHeader) where
    fromCBOR = ApiT <$> decodeBlockHeader byronEpochLength

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

instance ToJSON (ApiT (Tx, [TxWitness])) where
    toJSON (ApiT ((Tx inps outs), wit))= object ["signedTx" .= base64 bytes]
      where
        bytes :: ByteString
        bytes = CBOR.toStrictByteString $ encodeSignedTx ((inps, outs), wit)
        base64 :: ByteString -> String
        base64 = B8.unpack . convertToBase Base64

instance FromJSON (ApiT (Tx, [TxWitness])) where
    parseJSON = withObject "SignedTx" $ \p -> do
        bs <- (base64 <$> (p .: "signedTx"))
            >>= either fail return
        ((inps, outs), wits) <-
            pure (CBOR.deserialiseFromBytes decodeSignedTx (BL.fromStrict bs))
            >>= either (fail . show) (return . snd)
        return $ ApiT ((Tx inps outs), wits)
      where
        base64 :: String -> Either String ByteString
        base64 bs = convertFromBase Base64 (B8.pack bs)
