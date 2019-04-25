{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Extra content types for Servant.
--
module Servant.Extra.ContentTypes
    ( ComputeHash
    , CBOR
    , FromCBOR (..)
    , Hash (..)
    , Packed
    , WithHash (..)
    , Any
    ) where

import Prelude

import Cardano.Wallet.Binary.Packfile
    ( decodePackfile )
import Crypto.Hash
    ( Digest, hashWith )
import Crypto.Hash.IO
    ( HashAlgorithm (..) )
import Data.ByteArray.Encoding
    ( Base (Base16), convertToBase )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Proxy
    ( Proxy (..) )
import Data.Text.Encoding
    ( decodeUtf8 )
import Network.HTTP.Media
    ( (//), (/:) )
import Servant.API
    ( Accept (..), MimeUnrender (..), ToHttpApiData (..) )

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import qualified Data.ByteString.Lazy as BL

-- | Represents a CBOR (Concise Binary Object Representation) object.
--
-- See RFC 7049 (http://cbor.io/) for further details.
--
data CBOR

-- | The class of types that can be converted to from CBOR.
--
class FromCBOR a where
    fromCBOR :: CBOR.Decoder s a

instance Accept CBOR where
    contentType _ = "text" // "plain"

instance FromCBOR a => MimeUnrender CBOR a where
    mimeUnrender _ bl = either
        (Left . show)
        (Right . snd)
        (CBOR.deserialiseFromBytes fromCBOR bl)

-- | Represents a piece of binary data for which a hash value should be
--   calculated before performing any further deserialization.
--
data ComputeHash algorithm a

-- | Represents the result of hashing a piece of data.
--
newtype Hash algorithm a = Hash (Digest algorithm)

instance ToHttpApiData (Hash algorithm a) where
    toUrlPiece (Hash digest) = decodeUtf8 $ convertToBase Base16 digest

-- | Represents a piece of data with an accompanying hash value.
data WithHash algorithm a = WithHash
    { getHash  :: Digest algorithm
    , getValue :: a
    } deriving Show

instance Accept a => Accept (ComputeHash algorithm a) where
    contentType _ = contentType (Proxy :: Proxy a)

instance forall a b alg . (MimeUnrender a b, HashAlgorithm alg) =>
    MimeUnrender (ComputeHash alg a) (WithHash alg b) where
        mimeUnrender _ bl =
            WithHash (hashWith (undefined :: alg) $ BL.toStrict bl)
                <$> mimeUnrender (Proxy :: Proxy a) bl

-- | Represents something that has been packed with the Cardano packfile format.
--
data Packed a

instance Accept a => Accept (Packed a) where
    contentType _ = "text" // "plain"

instance forall a b . MimeUnrender a b => MimeUnrender (Packed a) [b] where
    mimeUnrender _ bs = either
        (Left . show)
        (traverse $ mimeUnrender (Proxy :: Proxy a) . BL.fromStrict)
        (decodePackfile bs)

-- | Any media type
data Any

instance Accept Any where
    contentTypes _ = ("*" // "*") :|
        -- We also 'conveniently' accept JSON format
        [ "application" // "json"
        , "application" // "json" /: ("charset", "utf-8")
        ]
