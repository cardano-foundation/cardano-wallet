{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}

module Cardano.Wallet.Primitive.Types.PoolId
    ( PoolId (..)
    , poolIdBytesLength
    , decodePoolIdBech32
    , encodePoolIdBech32
    )
where

import Prelude

import Control.DeepSeq
    ( NFData
    )
import Data.ByteArray.Encoding
    ( Base (Base16)
    , convertFromBase
    , convertToBase
    )
import Data.ByteString
    ( ByteString
    )
import Data.List
    ( intercalate
    )
import Data.Text.Class
    ( FromText (..)
    , TextDecodingError (TextDecodingError)
    , ToText (..)
    )
import Data.Text.Encoding
    ( decodeUtf8
    , encodeUtf8
    )
import Fmt
    ( Buildable (..)
    , prefixF
    )
import GHC.Generics
    ( Generic
    )

import qualified Codec.Binary.Bech32 as Bech32
import qualified Codec.Binary.Bech32.TH as Bech32
import qualified Data.ByteString as BS
import qualified Data.Text as T

-- | Identifies a stake pool.
-- For Jörmungandr a 'PoolId' is the blake2b-256 hash of the stake pool
-- registration certificate.
newtype PoolId = PoolId { getPoolId :: ByteString }
    deriving (Generic, Eq, Ord)

instance Show PoolId where
    show p = "(PoolId " <> show (encodePoolIdBech32 p) <> ")"

poolIdBytesLength :: [Int]
poolIdBytesLength = [28, 32]

instance NFData PoolId

instance Buildable PoolId where
    build poolId = mempty
        <> prefixF 8 poolIdF
      where
        poolIdF = build (toText poolId)

instance ToText PoolId where
    toText = decodeUtf8
        . convertToBase Base16
        . getPoolId

instance FromText PoolId where
    fromText t = case convertFromBase Base16 $ encodeUtf8 t of
        Left _ ->
            textDecodingError
        Right bytes | BS.length bytes `elem` poolIdBytesLength ->
            Right $ PoolId bytes
        Right _ ->
            textDecodingError
      where
        textDecodingError = Left $ TextDecodingError $ unwords
            [ "Invalid stake pool id: expecting a hex-encoded value that is"
            , intercalate " or " (show <$> poolIdBytesLength)
            , "bytes in length."
            ]

-- | Encode 'PoolId' as Bech32 with "pool" hrp.
encodePoolIdBech32 :: PoolId -> T.Text
encodePoolIdBech32 =
    Bech32.encodeLenient hrp
        . Bech32.dataPartFromBytes
        . getPoolId
  where
    hrp = [Bech32.humanReadablePart|pool|]

-- | Decode a Bech32 encoded 'PoolId'.
decodePoolIdBech32 :: T.Text -> Either TextDecodingError PoolId
decodePoolIdBech32 t =
    case fmap Bech32.dataPartToBytes <$> Bech32.decodeLenient t of
        Left _ -> Left textDecodingError
        Right (_, Just bytes) ->
            Right $ PoolId bytes
        Right _ -> Left textDecodingError
      where
        textDecodingError = TextDecodingError $ unwords
            [ "Invalid stake pool id: expecting a Bech32 encoded value"
            , "with human readable part of 'pool'."
            ]
