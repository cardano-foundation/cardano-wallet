{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Cardano.Wallet.Primitive.Types.DRep
    ( DRepID (..)
    , DRepKeyHash (..)
    , DRepScriptHash (..)
    , DRep (..)
    , encodeDRepKeyHashBech32
    , decodeDRepKeyHashBech32
    , encodeDRepScriptHashBech32
    , decodeDRepScriptHashBech32
    )
where

import Prelude

import Control.DeepSeq
    ( NFData
    )
import Data.ByteString
    ( ByteString
    )
import Data.Text
    ( Text
    )
import Data.Text.Class
    ( FromText (..)
    , TextDecodingError (TextDecodingError)
    , ToText (..)
    )
import Fmt
    ( Buildable (..)
    )
import GHC.Generics
    ( Generic
    )

import qualified Codec.Binary.Bech32 as Bech32
import qualified Codec.Binary.Bech32.TH as Bech32

newtype DRepKeyHash = DRepKeyHash { getDRepKeyHash :: ByteString }
    deriving (Generic, Eq, Ord, Show)

instance NFData DRepKeyHash

newtype DRepScriptHash = DRepScriptHash { getDRepScriptHash :: ByteString }
    deriving (Generic, Eq, Ord, Show)

instance NFData DRepScriptHash

data DRepID =
    DRepFromKeyHash DRepKeyHash | DRepFromScriptHash DRepScriptHash
    deriving (Eq, Generic, Show, Ord)
    deriving anyclass NFData

-- | Encode 'DRepKeyHash' as Bech32 with "drep" hrp.
encodeDRepKeyHashBech32 :: DRepKeyHash -> Text
encodeDRepKeyHashBech32 =
    Bech32.encodeLenient hrp
        . Bech32.dataPartFromBytes
        . getDRepKeyHash
  where
    hrp = [Bech32.humanReadablePart|drep|]

-- | Decode a Bech32 encoded 'DRepKeyHash'.
decodeDRepKeyHashBech32 :: Text -> Either TextDecodingError DRepKeyHash
decodeDRepKeyHashBech32 t =
    case fmap Bech32.dataPartToBytes <$> Bech32.decodeLenient t of
        Left _ -> Left textDecodingError
        Right (hrp', Just bytes)
            | hrp' == hrp -> Right $ DRepKeyHash bytes
        Right _ -> Left textDecodingError
      where
        textDecodingError = TextDecodingError $ unwords
            [ "Invalid DRep key hash: expecting a Bech32 encoded value"
            , "with human readable part of 'drep'."
            ]
        hrp = [Bech32.humanReadablePart|drep|]

-- | Encode 'DRepScriptHash' as Bech32 with "drep_script" hrp.
encodeDRepScriptHashBech32 :: DRepScriptHash -> Text
encodeDRepScriptHashBech32 =
    Bech32.encodeLenient hrp
        . Bech32.dataPartFromBytes
        . getDRepScriptHash
  where
    hrp = [Bech32.humanReadablePart|drep_script|]

-- | Decode a Bech32 encoded 'DRepScriptHash'.
decodeDRepScriptHashBech32 :: Text -> Either TextDecodingError DRepScriptHash
decodeDRepScriptHashBech32 t =
    case fmap Bech32.dataPartToBytes <$> Bech32.decodeLenient t of
        Left _ -> Left textDecodingError
        Right (hrp', Just bytes)
            | hrp' == hrp -> Right $ DRepScriptHash bytes
        Right _ -> Left textDecodingError
      where
        textDecodingError = TextDecodingError $ unwords
            [ "Invalid DRep Script hash: expecting a Bech32 encoded value"
            , "with human readable part of 'drep_script'."
            ]
        hrp = [Bech32.humanReadablePart|drep_script|]

instance Buildable DRepID where
    build = \case
        DRepFromKeyHash keyhash -> build $ encodeDRepKeyHashBech32 keyhash
        DRepFromScriptHash scripthash -> build $ encodeDRepScriptHashBech32 scripthash

-- | A decentralized representation ('DRep') will
-- vote on behalf of the stake delegated to it.
data DRep
    = Abstain
    | NoConfidence
    | FromDRepID DRepID
    deriving (Eq, Generic, Show, Ord)
    deriving anyclass NFData

instance ToText DRep where
    toText Abstain = "abstain"
    toText NoConfidence = "no_confidence"
    toText (FromDRepID (DRepFromKeyHash keyhash)) =
        encodeDRepKeyHashBech32 keyhash
    toText (FromDRepID (DRepFromScriptHash scripthash)) =
        encodeDRepScriptHashBech32 scripthash

instance FromText DRep where
    fromText txt = case txt of
        "abstain" -> Right Abstain
        "no_confidence" -> Right NoConfidence
        _ -> case decodeDRepKeyHashBech32 txt of
                Right keyhash ->
                     Right $ FromDRepID $ DRepFromKeyHash keyhash
                Left _ -> case decodeDRepScriptHashBech32 txt of
                    Right scripthash ->
                        Right $ FromDRepID $ DRepFromScriptHash scripthash
                    Left _ -> Left $ TextDecodingError $ unwords
                        [ "I couldn't parse the given decentralized representative (DRep)."
                        , "I am expecting either 'abstain', 'no confidence'"
                        , "or bech32 encoded drep having prefixes: 'drep'"
                        , "or 'drep_script'."]

instance Buildable DRep where
    build = \case
        Abstain -> "abstain"
        NoConfidence -> "casting no confidence"
        FromDRepID drep -> "delegating voting to " <> build drep
