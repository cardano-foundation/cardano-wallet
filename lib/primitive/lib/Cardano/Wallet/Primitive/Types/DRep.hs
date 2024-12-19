{-# LANGUAGE BinaryLiterals #-}
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
    , encodeDRepIDBech32
    , decodeDRepIDBech32
    , fstByteDRepKeyHash
    , fstByteDRepScriptHash
    )
where

import Prelude

import Control.DeepSeq
    ( NFData
    )
import Data.Bifunctor
    ( first
    , second
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
import Data.Word
    ( Word8
    )
import Fmt
    ( Buildable (..)
    )
import GHC.Generics
    ( Generic
    )

import qualified Codec.Binary.Bech32 as Bech32
import qualified Codec.Binary.Bech32.TH as Bech32
import qualified Data.ByteString as BS

-- | Raw key hash credential
newtype DRepKeyHash = DRepKeyHash { getDRepKeyHash :: ByteString }
    deriving (Generic, Eq, Ord, Show)

-- | In accordance to CIP-0129 (https://github.com/cardano-foundation/CIPs/tree/master/CIP-0129)
--   drep    0010....
--   keyhash ....0010
fstByteDRepKeyHash :: Word8
fstByteDRepKeyHash = 0b00100010

instance NFData DRepKeyHash

-- | Raw script hash credential
newtype DRepScriptHash = DRepScriptHash { getDRepScriptHash :: ByteString }
    deriving (Generic, Eq, Ord, Show)

-- | In accordance to CIP-0129 (https://github.com/cardano-foundation/CIPs/tree/master/CIP-0129)
--   drep       0010....
--   scripthash ....0011
fstByteDRepScriptHash :: Word8
fstByteDRepScriptHash = 0b00100011

instance NFData DRepScriptHash

data DRepID =
    DRepFromKeyHash DRepKeyHash | DRepFromScriptHash DRepScriptHash
    deriving (Eq, Generic, Show, Ord)
    deriving anyclass NFData

-- | Encode 'DRepID' as Bech32 with "drep" human readable prefix.
encodeDRepIDBech32 :: DRepID -> Text
encodeDRepIDBech32 drepid =
    Bech32.encodeLenient hrp
        . Bech32.dataPartFromBytes
        $ appendCip0129BytePrefix
  where
    hrp = [Bech32.humanReadablePart|drep|]
    appendCip0129BytePrefix = case drepid of
        DRepFromKeyHash (DRepKeyHash payload) ->
            BS.cons fstByteDRepKeyHash payload
        DRepFromScriptHash (DRepScriptHash payload) ->
            BS.cons fstByteDRepScriptHash payload

-- | Decode a Bech32 encoded 'DRepID'.
decodeDRepIDBech32 :: Text -> Either TextDecodingError DRepID
decodeDRepIDBech32 t =
    case fmap Bech32.dataPartToBytes <$> Bech32.decodeLenient t of
        Left _ -> Left textDecodingError
        Right (hrp', Just bytes)
            | hrp' == hrpDrep && BS.length bytes == 29 ->
              let (fstByte, payload) = first BS.head $ BS.splitAt 1 bytes
              in case fstByte of
                  0b00100010 ->
                      Right $ DRepFromKeyHash (DRepKeyHash payload)
                  0b00100011 ->
                      Right $ DRepFromScriptHash (DRepScriptHash payload)
                  _ ->
                      Left textFirstByteError
            | hrp' == hrpDrep && BS.length bytes == 28 ->
                  Right $ DRepFromKeyHash (DRepKeyHash bytes)
            | hrp' == hrpDrepVKH && BS.length bytes == 28 ->
                  Right $ DRepFromKeyHash (DRepKeyHash bytes)
            | hrp' == hrpDrepScript && BS.length bytes == 28 ->
                  Right $ DRepFromScriptHash (DRepScriptHash bytes)
        Right _ -> Left textDecodingError
      where
        textDecodingError = TextDecodingError $ unwords
            [ "Invalid DRep key/script hash: expecting a Bech32 encoded value"
            , "with human readable part of 'drep', 'drep_vkh' or 'drep_script'."
            ]
        textFirstByteError = TextDecodingError $ unwords
            [ "Invalid DRep credential: expecting a first byte '0b00100010' value for key hash or"
            , "a first byte '0b00100011' value for script hash."
            ]
        hrpDrep = [Bech32.humanReadablePart|drep|]
        hrpDrepVKH = [Bech32.humanReadablePart|drep_vkh|]
        hrpDrepScript = [Bech32.humanReadablePart|drep_script|]

instance Buildable DRepID where
    build = build . encodeDRepIDBech32

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
    toText (FromDRepID drepid) = encodeDRepIDBech32 drepid

instance FromText DRep where
    fromText txt = case txt of
        "abstain" -> Right Abstain
        "no_confidence" -> Right NoConfidence
        _ -> second FromDRepID (decodeDRepIDBech32 txt)

instance Buildable DRep where
    build = \case
        Abstain -> "abstain"
        NoConfidence -> "no_confidence"
        FromDRepID drep -> "delegating voting to " <> build drep
