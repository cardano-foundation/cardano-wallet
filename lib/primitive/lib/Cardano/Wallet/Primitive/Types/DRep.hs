{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Cardano.Wallet.Primitive.Types.DRep
    ( DRep (..)
    , DRepKeyHash (..)
    , DRepScriptHash (..)
    , VoteAction (..)
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

data DRep =
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

instance Buildable DRep where
    build = \case
        DRepFromKeyHash keyhash -> build $ encodeDRepKeyHashBech32 keyhash
        DRepFromScriptHash scripthash -> build $ encodeDRepScriptHashBech32 scripthash

-- | Vote action.
data VoteAction
    = Abstain
    | NoConfidence
    | VoteTo !DRep
    deriving (Eq, Generic, Show, Ord)
    deriving anyclass NFData

instance ToText VoteAction where
    toText Abstain = "abstain"
    toText NoConfidence = "no confidence"
    toText (VoteTo (DRepFromKeyHash keyhash)) =
        encodeDRepKeyHashBech32 keyhash
    toText (VoteTo (DRepFromScriptHash scripthash)) =
        encodeDRepScriptHashBech32 scripthash

instance FromText VoteAction where
    fromText txt = case txt of
        "abstain" -> Right Abstain
        "no confidence" -> Right NoConfidence
        _ -> case decodeDRepKeyHashBech32 txt of
                Right keyhash ->
                     Right $ VoteTo $ DRepFromKeyHash keyhash
                Left _ -> case decodeDRepScriptHashBech32 txt of
                    Right scripthash ->
                        Right $ VoteTo $ DRepFromScriptHash scripthash
                    Left _ -> Left $ TextDecodingError $ unwords
                        [ "I couldn't parse the given vote action."
                        , "I am expecting either 'abstain', 'no confidence'"
                        , "or bech32 encoded drep having prefixes: 'drep_script'"
                        , "or 'drep_script'."]

instance Buildable VoteAction where
    build = \case
        Abstain -> "abstaining"
        NoConfidence -> "casting no confidence"
        VoteTo drep -> "voting to " <> build drep
