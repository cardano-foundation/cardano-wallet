{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Pool.Types
    ( StakePoolsSummary (..)
    , PoolId(..)
    , PoolOwner(..)
    , poolIdBytesLength
    , decodePoolIdBech32
    , encodePoolIdBech32
    , StakePoolTicker (..)
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Util
    ( ShowFmt (..) )
import Control.DeepSeq
    ( NFData )
import Control.Monad
    ( (>=>) )
import Data.Aeson
    ( FromJSON (parseJSON), ToJSON (toJSON) )
import Data.ByteArray.Encoding
    ( Base (Base16), convertFromBase, convertToBase )
import Data.ByteString
    ( ByteString )
import Data.List
    ( intercalate )
import Data.Map
    ( Map )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Percentage )
import Data.Text
    ( Text )
import Data.Text.Class
    ( FromText (..), TextDecodingError (TextDecodingError), ToText (..) )
import Data.Text.Encoding
    ( decodeUtf8, encodeUtf8 )
import Database.Persist.Class.PersistField
    ( PersistField (..) )
import Database.Persist.PersistValue.Extended
    ( fromPersistValueFromText )
import Database.Persist.Sqlite
    ( PersistFieldSql (..) )
import Fmt
    ( Buildable (..), listF', mapF, prefixF, pretty )
import GHC.Generics
    ( Generic )

import qualified Codec.Binary.Bech32 as Bech32
import qualified Codec.Binary.Bech32.TH as Bech32
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Data.Text as T

-- | Very short name for a stake pool.
newtype StakePoolTicker = StakePoolTicker { unStakePoolTicker :: Text }
    deriving stock (Generic, Show, Eq, Ord)
    deriving newtype (ToText)

instance FromText StakePoolTicker where
    fromText t
        | T.length t >= 3 && T.length t <= 5
            = Right $ StakePoolTicker t
        | otherwise
            = Left . TextDecodingError $
                "stake pool ticker length must be 3-5 characters"

-- Here to avoid needless orphan instances in the API types.
instance FromJSON StakePoolTicker where
    parseJSON = parseJSON >=> either (fail . show . ShowFmt) pure . fromText

instance ToJSON StakePoolTicker where
    toJSON = toJSON . toText

instance PersistField StakePoolTicker where
    toPersistValue = toPersistValue . toText
    fromPersistValue = fromPersistValueFromText

instance PersistFieldSql StakePoolTicker where
    sqlType _ = sqlType (Proxy @Text)

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

-- | A stake pool owner, which is a public key encoded in bech32 with prefix
-- ed25519_pk.
newtype PoolOwner = PoolOwner { getPoolOwner :: ByteString }
    deriving (Generic, Eq, Show, Ord)

poolOwnerPrefix :: Bech32.HumanReadablePart
poolOwnerPrefix = [Bech32.humanReadablePart|ed25519_pk|]

instance NFData PoolOwner

instance Buildable PoolOwner where
    build poolId = build (toText poolId)

instance ToText PoolOwner where
    toText = Bech32.encodeLenient poolOwnerPrefix
        . Bech32.dataPartFromBytes
        . getPoolOwner

instance FromText PoolOwner where
    fromText t = case fmap Bech32.dataPartToBytes <$> Bech32.decode t of
        Left err ->
            Left $ TextDecodingError $
            "Stake pool owner is not a valid bech32 string: "
            <> show err
        Right (hrp, Just bytes)
            | hrp == poolOwnerPrefix ->
                Right $ PoolOwner bytes
            | otherwise ->
                Left $ TextDecodingError $
                "Stake pool owner has wrong prefix:"
                <> " expected "
                <> T.unpack (Bech32.humanReadablePartToText poolOwnerPrefix)
                <> " but got "
                <> show hrp
        Right (_, Nothing) ->
                Left $ TextDecodingError "Stake pool owner is invalid"

instance FromJSON PoolOwner where
    parseJSON = parseJSON >=> either (fail . show . ShowFmt) pure . fromText

instance ToJSON PoolOwner where
    toJSON = toJSON . toText

instance PersistField PoolOwner where
    toPersistValue = toPersistValue . toText
    fromPersistValue = fromPersistValueFromText

instance PersistFieldSql PoolOwner where
    sqlType _ = sqlType (Proxy @Text)

instance Read PoolOwner where
    readsPrec _ = error "readsPrec stub needed for persistent"

instance FromText [PoolOwner] where
    fromText t = mapM fromText $ T.words t

instance PersistField [PoolOwner] where
    toPersistValue v = toPersistValue $ T.unwords $ toText <$> v
    fromPersistValue = fromPersistValueFromText

instance PersistFieldSql [PoolOwner] where
    sqlType _ = sqlType (Proxy @Text)

data StakePoolsSummary = StakePoolsSummary
    { nOpt :: Int
    , rewards :: Map PoolId Coin
    , stake :: Map PoolId Percentage
    } deriving (Show, Eq)

instance Buildable StakePoolsSummary where
    build StakePoolsSummary{nOpt,rewards,stake} = listF' id
        [ "Stake: " <> mapF (Map.toList stake)
        , "Non-myopic member rewards: " <> mapF (Map.toList rewards)
        , "Optimum number of pools: " <> pretty nOpt
        ]
