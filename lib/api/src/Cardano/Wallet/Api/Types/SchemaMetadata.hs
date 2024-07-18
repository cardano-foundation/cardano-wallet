{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE StrictData #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Copyright: © 2018-2022 IOHK, 2023 Cardano Foundation
-- License: Apache-2.0
--
-- A wrapper around TxMetadata to allow different JSON codecs. (ADP-1596)
-- see https://github.com/IntersectMBO/cardano-node/blob/master/cardano-api/src/Cardano/Api/TxMetadata.hs
module Cardano.Wallet.Api.Types.SchemaMetadata where

import Cardano.Api
    ( TxMetadataJsonSchema (..)
    , TxMetadataJsonSchemaError (..)
    , TxMetadataValue (..)
    , metadataFromJson
    , metadataToJson
    , metadataValueToJsonNoSchema
    )
import Cardano.Api.Error
    ( displayError
    )
import Cardano.Wallet
    ( ErrConstructTx (..)
    , ErrDecodeTx (..)
    )
import Cardano.Wallet.Primitive.Types.Tx
    ( TxMetadata (..)
    )
import Control.Applicative
    ( (<|>)
    )
import Control.DeepSeq
    ( NFData
    )
import Control.Monad
    ( guard
    , when
    )
import Cryptography.Cipher.AES256CBC
    ( CipherError
    , CipherMode (..)
    )
import Cryptography.Hash.Core
    ( SHA256 (..)
    )
import Cryptography.KDF.PBKDF2
    ( PBKDF2Config (..)
    )
import Data.Aeson
    ( FromJSON (parseJSON)
    , ToJSON (toJSON)
    )
import Data.Bifunctor
    ( bimap
    , first
    )
import Data.ByteArray.Encoding
    ( Base (..)
    , convertFromBase
    , convertToBase
    )
import Data.ByteString
    ( ByteString
    )
import Data.Generics.Internal.VL.Lens
    ( (^.)
    )
import Data.Generics.Labels
    ()
import Data.Maybe
    ( isJust
    , fromMaybe
    , mapMaybe
    )
import Data.Text
    ( Text
    )
import Data.Word
    ( Word64
    )
import GHC.Generics
    ( Generic
    )
import Prelude

import qualified Cardano.Ledger.Binary as CBOR
import qualified Cardano.Ledger.Shelley.TxAuxData as Ledger
import qualified Codec.CBOR.Magic as CBOR
import qualified Cryptography.Cipher.AES256CBC as AES256CBC
import qualified Cryptography.KDF.PBKDF2 as PBKDF2
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Aeson
import qualified Data.Aeson.KeyMap as Aeson
import qualified Data.Attoparsec.ByteString.Char8 as Atto
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Scientific as Scientific
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V

-- | A tag to select the json codec
data TxMetadataSchema = TxMetadataNoSchema | TxMetadataDetailedSchema
    deriving (Show, Eq, Generic, NFData)

-- | A wrapper to drive the json codec of metadata
data TxMetadataWithSchema = TxMetadataWithSchema
    { -- | How to codec the metadata into json
        txMetadataWithSchema_schema :: TxMetadataSchema
    , -- | The metadata
        txMetadataWithSchema_metadata :: TxMetadata
    }
    deriving (Show, Eq, Generic, NFData)

-- | Parses a Boolean "simple-metadata" API flag.
--
-- prop> toSimpleMetadataFlag . parseSimpleMetadataFlag == id
-- prop> parseSimpleMetadataFlag . toSimpleMetadataFlag == id
--
parseSimpleMetadataFlag :: Bool -> TxMetadataSchema
parseSimpleMetadataFlag flag =
    if flag
    then TxMetadataNoSchema
    else TxMetadataDetailedSchema

-- | Produces a Boolean "simple-metadata" API flag.
--
-- prop> toSimpleMetadataFlag . parseSimpleMetadataFlag == id
-- prop> parseSimpleMetadataFlag . toSimpleMetadataFlag == id
--
toSimpleMetadataFlag :: TxMetadataSchema -> Bool
toSimpleMetadataFlag = \case
    TxMetadataNoSchema -> True
    TxMetadataDetailedSchema -> False

instance ToJSON TxMetadataWithSchema where
    toJSON (TxMetadataWithSchema TxMetadataDetailedSchema x) =
        metadataToJson TxMetadataJsonDetailedSchema x
    toJSON (TxMetadataWithSchema TxMetadataNoSchema x) =
        metadataToJson TxMetadataJsonNoSchema x

detailedMetadata :: TxMetadata -> TxMetadataWithSchema
detailedMetadata = TxMetadataWithSchema TxMetadataDetailedSchema

noSchemaMetadata :: TxMetadata -> TxMetadataWithSchema
noSchemaMetadata = TxMetadataWithSchema TxMetadataNoSchema

instance FromJSON TxMetadataWithSchema where
    parseJSON = liftA2
        (<|>)
        (fmap detailedMetadata
            . either (fail . displayError) pure
            . metadataFromJson TxMetadataJsonDetailedSchema
        )
        (fmap noSchemaMetadata
            . either (fail . displayError) pure
            . metadataFromJson TxMetadataJsonNoSchema
        )

instance ToJSON TxMetadataValue where
    toJSON = metadataValueToJsonNoSchema

instance FromJSON TxMetadataValue where
    parseJSON = either (fail . displayError) pure . metadataValueFromJsonNoSchema

-- when cardano-api exports metadataValueFromJsonNoSchema the below could be removed (together with cabal dependencies)
metadataValueFromJsonNoSchema
    :: Aeson.Value
    -> Either TxMetadataJsonSchemaError TxMetadataValue
metadataValueFromJsonNoSchema = conv
  where
    conv :: Aeson.Value -> Either TxMetadataJsonSchemaError TxMetadataValue
    conv Aeson.Null   = Left TxMetadataJsonNullNotAllowed
    conv Aeson.Bool{} = Left TxMetadataJsonBoolNotAllowed

    conv (Aeson.Number d) =
      case Scientific.floatingOrInteger d :: Either Double Integer of
        Left  n -> Left (TxMetadataJsonNumberNotInteger n)
        Right n -> Right (TxMetaNumber n)

    conv (Aeson.String s)
      | Just s' <- T.stripPrefix bytesPrefix s
      , let bs' = T.encodeUtf8 s'
      , Right bs <- B16.decode bs'
      , not (B8.any (\c -> c >= 'A' && c <= 'F') bs')
      = Right (TxMetaBytes bs)

    conv (Aeson.String s) = Right (TxMetaText s)

    conv (Aeson.Array vs) =
        fmap TxMetaList
      . traverse conv
      $ V.toList vs

    conv (Aeson.Object kvs) =
        fmap
        ( TxMetaMap
        . sortCanonicalForCbor
        )
      . traverse (\(k,v) -> (,) (convKey k) <$> conv v)
      . fmap (first Aeson.toText)
      $ Aeson.toList kvs

    convKey :: Text -> TxMetadataValue
    convKey s =
      fromMaybe (TxMetaText s) $
      parseAll ((fmap TxMetaNumber pSigned <* Atto.endOfInput)
            <|> (fmap TxMetaBytes  pBytes  <* Atto.endOfInput)) s

bytesPrefix :: Text
bytesPrefix = "0x"

parseAll :: Atto.Parser a -> Text -> Maybe a
parseAll p =
    either (const Nothing) Just
    . Atto.parseOnly p
    . T.encodeUtf8

pUnsigned :: Atto.Parser Integer
pUnsigned = do
    bs <- Atto.takeWhile1 Atto.isDigit
    -- no redundant leading 0s allowed, or we cannot round-trip properly
    guard (not (BS.length bs > 1 && B8.head bs == '0'))
    return $! BS.foldl' step 0 bs
  where
    step a w = a * 10 + fromIntegral (w - 48)

pSigned :: Atto.Parser Integer
pSigned = Atto.signed pUnsigned

pBytes :: Atto.Parser ByteString
pBytes = do
  _ <- Atto.string "0x"
  remaining <- Atto.takeByteString
  when (B8.any hexUpper remaining) $ fail ("Unexpected uppercase hex characters in " <> show remaining)
  case B16.decode remaining of
    Right bs -> return bs
    _ -> fail ("Expecting base16 encoded string, found: " <> show remaining)
  where
    hexUpper c = c >= 'A' && c <= 'F'

sortCanonicalForCbor
    :: [(TxMetadataValue, TxMetadataValue)] -> [(TxMetadataValue, TxMetadataValue)]
sortCanonicalForCbor =
  map snd
  . L.sortOn fst
  . map (\e@(k, _) -> (CBOR.uintegerFromBytes $ serialiseKey k, e))
    where
      serialiseKey = CBOR.serialize' CBOR.shelleyProtVer . toShelleyMetadatum

toShelleyMetadatum :: TxMetadataValue -> Ledger.Metadatum
toShelleyMetadatum (TxMetaNumber x) = Ledger.I x
toShelleyMetadatum (TxMetaBytes  x) = Ledger.B x
toShelleyMetadatum (TxMetaText   x) = Ledger.S x
toShelleyMetadatum (TxMetaList  xs) =
    Ledger.List [ toShelleyMetadatum x | x <- xs ]
toShelleyMetadatum (TxMetaMap   xs) =
    Ledger.Map [ (toShelleyMetadatum k,
                      toShelleyMetadatum v)
                | (k,v) <- xs ]

-- Metadata encryption/decryption
metadataPBKDF2Config :: PBKDF2Config SHA256
metadataPBKDF2Config = PBKDF2Config
    { hash = SHA256
    , iterations = 10000
    , keyLength = 32
    , ivLength = 16
    }

-- A key that identifies transaction metadata, defined in CIP-20 and used by
-- CIP-83.
--
-- See:
-- https://github.com/cardano-foundation/CIPs/tree/master/CIP-0020
-- https://github.com/cardano-foundation/CIPs/tree/master/CIP-0083
--
cip20MetadataKey :: Word64
cip20MetadataKey = 674

cip83EncryptMethodKey :: Text
cip83EncryptMethodKey = "enc"

cip83EncryptPayloadKey :: Text
cip83EncryptPayloadKey = "msg"

cip83EncryptPayloadValue :: Text
cip83EncryptPayloadValue = "basic"

-- When encryption is enabled we do the following:
-- (a) find field `msg` in the object of "674" label
-- (b) encrypt the 'msg' value if present, if there is neither "674" label
--     nor 'msg' value inside object of it emit error
-- (c) update value of `msg` with the encrypted initial value(s) encoded in
--     base64:
--     [TxMetaText base64_1, TxMetaText base64_2, ..., TxMetaText base64_n]
-- (d) add `enc` field with encryption method value 'basic'
toMetadataEncrypted
    :: ByteString
    -> TxMetadataWithSchema
    -> Maybe ByteString
    -> Either ErrConstructTx TxMetadata
toMetadataEncrypted pwd payload saltM =
    fmap updateTxMetadata . encryptMessage =<< extractMessage
  where
    secretKey, iv :: ByteString
    (secretKey, iv) = PBKDF2.generateKey metadataPBKDF2Config pwd saltM

    -- `msg` is embedded at the first level
    parseMessage :: TxMetadataValue -> Maybe [TxMetadataValue]
    parseMessage = \case
        TxMetaMap kvs ->
            case mapMaybe getValue kvs of
                [ ] -> Nothing
                vs -> Just vs
        _ ->
            Nothing
      where
        getValue :: (TxMetadataValue, TxMetadataValue) -> Maybe TxMetadataValue
        getValue (TxMetaText k, v) =
            if k == cip83EncryptPayloadKey then
                Just v
            else
                Nothing
        getValue _ = Nothing

    validKeyAndMessage :: Word64 -> TxMetadataValue -> Bool
    validKeyAndMessage k v = k == cip20MetadataKey && isJust (parseMessage v)

    extractMessage :: Either ErrConstructTx TxMetadataValue
    extractMessage
        | [v] <- F.toList filteredMap =
            Right v
        | otherwise =
            Left ErrConstructTxIncorrectRawMetadata
      where
        TxMetadata themap = payload ^. #txMetadataWithSchema_metadata
        filteredMap = Map.filterWithKey validKeyAndMessage themap

    encryptMessage :: TxMetadataValue -> Either ErrConstructTx TxMetadataValue
    encryptMessage = \case
        TxMetaMap pairs ->
            TxMetaMap . reverse . L.nub . reverse . concat <$>
            mapM encryptPairIfQualifies pairs
        _ ->
            error "encryptMessage should have TxMetaMap value"
      where
        encryptPairIfQualifies
            :: (TxMetadataValue, TxMetadataValue)
            -> Either ErrConstructTx [(TxMetadataValue, TxMetadataValue)]
        encryptPairIfQualifies = \case
            (TxMetaText "msg", m) -> do
                bimap ErrConstructTxEncryptMetadata toPair (encryptValue m)
            pair ->
                Right [pair]

        encryptValue :: TxMetadataValue -> Either CipherError ByteString
        encryptValue
            = AES256CBC.encrypt WithPadding secretKey iv saltM
            . BL.toStrict
            . Aeson.encode
            . metadataValueToJsonNoSchema

        toPair :: ByteString -> [(TxMetadataValue, TxMetadataValue)]
        toPair encryptedMessage =
            [ (TxMetaText cip83EncryptPayloadKey, TxMetaList (toChunks encryptedMessage))
            , (TxMetaText cip83EncryptMethodKey, TxMetaText cip83EncryptPayloadValue)
            ]

        toChunks :: ByteString -> [TxMetadataValue]
        toChunks
            = fmap TxMetaText
            . T.chunksOf 64
            . T.decodeUtf8
            . convertToBase Base64

    updateTxMetadata :: TxMetadataValue -> TxMetadata
    updateTxMetadata v = TxMetadata (Map.insert cip20MetadataKey v themap)
      where
        TxMetadata themap = payload ^. #txMetadataWithSchema_metadata

-- When decryption is enabled we do the following:
-- (a) retrieve list of TxMetaBytes under proper key, ie.674,
--     cip20MetadataKey
-- (b) recreate each encrypted payload from chunks
--     (0, TxMetaBytes chunk1)
--     (1, TxMetaBytes chunk2)
--     ....
--     (N, TxMetaBytes chunkN)
--     ie., payload=chunk1+chunk2+...+chunkN
-- (c) decrypt each payload
-- (d) update structure
-- (e) decode metadata
fromMetadataEncrypted
    :: ByteString
    -> TxMetadata
    -> Either ErrDecodeTx TxMetadata
fromMetadataEncrypted pwd metadata =
   composePayload metadata >>=
   mapM decrypt >>=
   adjust metadata
  where
    checkPresenceOfMethod value =
        let presentPair (TxMetaText k, TxMetaText v) =
                k == cip83EncryptMethodKey && v == cip83EncryptPayloadValue
            presentPair _ = False
        in case value of
            TxMetaMap list -> not (any presentPair list)
            _ -> True
    getEncryptedPayload value =
        let presentPair (TxMetaText k, TxMetaList _) =
                k == cip83EncryptPayloadKey
            presentPair _ = False
        in case value of
            TxMetaMap list -> snd <$> filter presentPair list
            _ -> []
    extractTxt (TxMetaText txt) = txt
    extractTxt _ =
        error "TxMetaText is expected"
    extractPayload (TxMetaList chunks)=
        foldl T.append T.empty $ extractTxt <$> chunks
    extractPayload _ = T.empty
    composePayload (TxMetadata themap) = do
        validValue <- case Map.lookup cip20MetadataKey themap of
            Nothing -> Left ErrDecodeTxMissingMetadataKey
            Just v -> pure v
        when (checkPresenceOfMethod validValue) $
            Left ErrDecodeTxMissingEncryptionMethod
        let payloads = getEncryptedPayload validValue
        if null payloads then
            Left ErrDecodeTxMissingValidEncryptionPayload
        else do
            let extracted = extractPayload <$> payloads
            when (T.empty `elem` extracted) $
                Left ErrDecodeTxMissingValidEncryptionPayload
            Right extracted

    decodeFromJSON = ---use metadataValueFromJsonNoSchema when available from cardano-api
        first (ErrDecodeTxDecryptedPayload . T.pack) .
        Aeson.eitherDecode . BL.fromStrict
    decrypt payload = case convertFromBase Base64 (T.encodeUtf8 payload) of
        Right payloadBS ->
            case AES256CBC.getSaltFromEncrypted payloadBS of
                     Nothing -> Left ErrDecodeTxMissingSalt
                     Just salt -> do
                         let (secretKey, iv) =
                                 PBKDF2.generateKey metadataPBKDF2Config pwd (Just salt)
                         decrypted <- bimap ErrDecodeTxDecryptPayload fst
                             (AES256CBC.decrypt WithPadding secretKey iv payloadBS)
                         decodeFromJSON decrypted
        Left _ ->
            Left ErrDecodeTxEncryptedPayloadWrongBase

    adjust (TxMetadata metadata') decodedElems =
        pure $ TxMetadata $
        Map.adjust updateMetaMap cip20MetadataKey metadata'
      where
        updateElem acc@(decryptedList, list) elem' = case elem' of
            (TxMetaText k, TxMetaText v) ->
                if k == cip83EncryptMethodKey && v == cip83EncryptPayloadValue then
                    -- omiting this element
                    acc
                else
                    (decryptedList, list ++ [elem'])
            (TxMetaText k, v) -> case decryptedList of
                toAdd : rest ->
                    if k == cip83EncryptPayloadKey then
                        (rest, list ++ [(TxMetaText k, toAdd)] )
                    else
                        (decryptedList, list ++ [(TxMetaText k, v)] )
                _ -> error "we have checked already in composePayload that there is enough elements in decrypedList"
            _ -> error "we have checked already in composePayload that there is pair (TxMetaText, something)"

        updateMetaMap v = case v of
            TxMetaMap list ->
                TxMetaMap $ snd $ L.foldl updateElem (decodedElems,[]) list
            _ -> error "we have checked already in composePayload that there is TxMetaMap"
