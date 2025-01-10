{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Wallet.Primitive.Types.MetadataEncryption
    ( metadataPBKDF2Config
    , cip20MetadataKey
    , cip83EncryptMethodKey
    , cip83EncryptPayloadKey
    , cip83EncryptPayloadValue

    , ErrMetadataDecryption (..)
    , fromMetadataEncrypted

    , ErrMetadataEncryption (..)
    , toMetadataEncrypted
    )
where

import Prelude

import Cardano.Api
    ( TxMetadata (..)
    , TxMetadataValue (..)
    , metadataValueFromJsonNoSchema
    , metadataValueToJsonNoSchema
    )
import Cardano.Api.Error
    ( displayError
    )
import Control.Monad
    ( when
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
import Data.Maybe
    ( fromJust
    , isJust
    , isNothing
    , mapMaybe
    )
import Data.Text
    ( Text
    )
import Data.Word
    ( Word64
    )

import qualified Cryptography.Cipher.AES256CBC as AES256CBC
import qualified Cryptography.KDF.PBKDF2 as PBKDF2
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

-- CIP references:
-- https://github.com/cardano-foundation/CIPs/tree/master/CIP-0020
-- https://github.com/cardano-foundation/CIPs/tree/master/CIP-0083

-- Metadata encryption/decryption config use in accordance to
-- CIP-83.
metadataPBKDF2Config :: PBKDF2Config SHA256
metadataPBKDF2Config = PBKDF2Config
    { hash = SHA256
    , iterations = 10000
    , keyLength = 32
    , ivLength = 16
    }

-- A key that identifies transaction metadata, defined in CIP-20 and used by
-- CIP-83.
cip20MetadataKey :: Word64
cip20MetadataKey = 674

cip83EncryptMethodKey :: Text
cip83EncryptMethodKey = "enc"

cip83EncryptPayloadKey :: Text
cip83EncryptPayloadKey = "msg"

cip83EncryptPayloadValue :: Text
cip83EncryptPayloadValue = "basic"

data ErrMetadataEncryption =
      ErrIncorrectRawMetadata
    | ErrCannotEncryptMetadata CipherError
    deriving (Show, Eq)

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
    -> TxMetadata
    -> Maybe ByteString
    -> Either ErrMetadataEncryption TxMetadata
toMetadataEncrypted pwd payload saltM =
    fmap updateTxMetadata . encryptMessage =<< extractMessage
  where
    secretKey, iv :: ByteString
    (secretKey, iv) = PBKDF2.generateKey metadataPBKDF2Config pwd saltM

    -- `msg` is embedded at the first level with the exact following value structure
    -- TxMetaList [TxMetaText txt1, ..., TxMetaText txtN]
    parseMessage :: TxMetadataValue -> Maybe [TxMetadataValue]
    parseMessage = \case
        TxMetaMap kvs ->
            case mapMaybe getValue kvs of
                [ ] -> Nothing
                vs -> Just vs
        _ ->
            Nothing
      where
        isText (TxMetaText _ ) = True
        isText _ = False

        valueStructure (TxMetaList txts) =
            all isText txts
        valueStructure _ = False

        getValue :: (TxMetadataValue, TxMetadataValue) -> Maybe TxMetadataValue
        getValue (TxMetaText k, v) =
            if k == cip83EncryptPayloadKey && valueStructure v then
                Just v
            else
                Nothing
        getValue _ = Nothing

    validKeyAndMessage :: Word64 -> TxMetadataValue -> Bool
    validKeyAndMessage k v = k == cip20MetadataKey && isJust (parseMessage v)

    extractMessage :: Either ErrMetadataEncryption TxMetadataValue
    extractMessage
        | [v] <- F.toList filteredMap =
            Right v
        | otherwise =
            Left ErrIncorrectRawMetadata
      where
        TxMetadata themap = payload
        filteredMap = Map.filterWithKey validKeyAndMessage themap

    encryptMessage :: TxMetadataValue -> Either ErrMetadataEncryption TxMetadataValue
    encryptMessage = \case
        TxMetaMap pairs ->
            TxMetaMap . reverse . L.nub . reverse . concat <$>
            mapM encryptPairIfQualifies pairs
        _ ->
            error "encryptMessage should have TxMetaMap value"
      where
        encryptPairIfQualifies
            :: (TxMetadataValue, TxMetadataValue)
            -> Either ErrMetadataEncryption [(TxMetadataValue, TxMetadataValue)]
        encryptPairIfQualifies = \case
            (TxMetaText "msg", m) -> do
                bimap ErrCannotEncryptMetadata toPair (encryptValue m)
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
        TxMetadata themap = payload

data ErrMetadataDecryption =
      ErrMissingMetadataKey
    | ErrMissingEncryptionMethod
    | ErrMissingValidEncryptionPayload
    | ErrCannotAesonDecodePayload Text
    | ErrMissingSalt
    | ErrCannotDecryptPayload CipherError
    | ErrEncryptedPayloadWrongBase
    deriving (Show, Eq)

-- When decryption is enabled we do the following:
-- (a) retrieve TxMetaMap under proper key, ie.674,
--     cip20MetadataKey
-- (b) check if there is ("enc", "basic") pair
-- (c) recreate each encrypted payload from chunks that are expected under proper key, ie.msg,
--     cip83EncryptPayloadKey. So
--     expect TxMetaList [TxMetaText chunk1, ..., TxMetaText chunkN]
--     and construct payload=chunk1+chunk2+...+chunkN
-- (d) decrypt payload and decode metadata
-- (e) update structure under msg key and remove ("enc", "basic") pair
fromMetadataEncrypted
    :: ByteString
    -> TxMetadata
    -> Either ErrMetadataDecryption TxMetadata
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
    extractTxt (TxMetaText txt) = Just txt
    extractTxt _ = Nothing
    extractPayload (TxMetaList chunks)=
        let extractedTxts = extractTxt <$> chunks
        in if any isNothing extractedTxts then
            T.empty
        else
            -- we are sure there is not Nothing in the extractedTxts
            foldl T.append T.empty $ fromJust <$> extractedTxts
    extractPayload _ = T.empty
    composePayload (TxMetadata themap) = do
        validValue <- case Map.lookup cip20MetadataKey themap of
            Nothing -> Left ErrMissingMetadataKey
            Just v -> pure v
        when (checkPresenceOfMethod validValue) $
            Left ErrMissingEncryptionMethod
        let payloads = getEncryptedPayload validValue
        if null payloads then
            Left ErrMissingValidEncryptionPayload
        else do
            let extracted = extractPayload <$> payloads
            when (T.empty `elem` extracted) $
                Left ErrMissingValidEncryptionPayload
            Right extracted

    decodeFromJSON =
        first (ErrCannotAesonDecodePayload . T.pack) .
        Aeson.eitherDecode . BL.fromStrict
    decrypt payload = case convertFromBase Base64 (T.encodeUtf8 payload) of
        Right payloadBS ->
            case AES256CBC.getSaltFromEncrypted payloadBS of
                     Nothing -> Left ErrMissingSalt
                     Just salt -> do
                         let (secretKey, iv) =
                                 PBKDF2.generateKey metadataPBKDF2Config pwd (Just salt)
                         decrypted <- bimap ErrCannotDecryptPayload fst
                             (AES256CBC.decrypt WithPadding secretKey iv payloadBS)
                         decodeFromJSON decrypted
        Left _ ->
            Left ErrEncryptedPayloadWrongBase

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
            . traverse
                ((\(k, v) -> (,) (convKey k) <$> conv v) . first Aeson.toText)
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
