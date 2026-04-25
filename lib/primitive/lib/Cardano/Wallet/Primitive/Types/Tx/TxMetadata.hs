{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Copyright: 2018-2022 IOHK, 2023 Cardano Foundation
-- License: Apache-2.0
--
-- Metadata embedded in transactions. Wallet-owned copy of the
-- @cardano-api@ surface.
--
-- The ledger already provides 'Cardano.Ledger.Metadata.Metadatum';
-- this module converts to and from it below. It keeps the wallet-facing
-- constructors and JSON schema helpers while the API and tests still use
-- the old @cardano-api@ metadata shape.
module Cardano.Wallet.Primitive.Types.Tx.TxMetadata
    ( -- * Types
      TxMetadata (..)
    , TxMetadataValue (..)

      -- * Constructing metadata
    , makeTransactionMetadata
    , metaTextChunks
    , metaBytesChunks

      -- * Validating metadata
    , validateTxMetadata
    , TxMetadataRangeError (..)

      -- * Conversion to\/from JSON
    , TxMetadataJsonSchema (..)
    , metadataFromJson
    , metadataToJson
    , metadataValueFromJsonNoSchema
    , metadataValueToJsonNoSchema
    , TxMetadataJsonError (..)
    , TxMetadataJsonSchemaError (..)

      -- * Internal conversion functions
    , toShelleyMetadata
    , fromShelleyMetadata
    , toShelleyMetadatum
    , fromShelleyMetadatum

      -- * Shared parsing utils
    , parseAll
    , pUnsigned
    , pSigned
    , pBytes
    )
where

import Control.Applicative
    ( Alternative (..)
    )
import Control.Monad
    ( guard
    , when
    )
import Data.Bifunctor
    ( bimap
    , first
    )
import Data.ByteString
    ( ByteString
    )
import Data.Map.Strict
    ( Map
    )
import Data.Maybe
    ( fromMaybe
    )
import Data.Text
    ( Text
    )
import Data.Word
    ( Word64
    )
import GHC.Exts
    ( IsList (..)
    )
import Prelude

import Cardano.Ledger.Binary qualified as CBOR
import Cardano.Ledger.Shelley.TxAuxData qualified as Shelley
import Codec.CBOR.Magic qualified as CBOR
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Text qualified as Aeson.Text
import Data.Attoparsec.ByteString.Char8 qualified as Atto
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Char8 qualified as B8
import Data.List qualified as L
import Data.Map.Lazy qualified as Map.Lazy
import Data.Map.Strict qualified as Map
import Data.Scientific qualified as Scientific
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Lazy qualified as T.Lazy
import Data.Text.Lazy.Builder qualified as T.Builder

-- ----------------------------------------------------------------------------
-- TxMetadata types
--

newtype TxMetadata
    = TxMetadata {unTxMetadata :: Map Word64 TxMetadataValue}
    deriving (Eq, Show)

data TxMetadataValue
    = TxMetaMap [(TxMetadataValue, TxMetadataValue)]
    | TxMetaList [TxMetadataValue]
    | TxMetaNumber Integer
    | TxMetaBytes ByteString
    | TxMetaText Text
    deriving (Eq, Ord, Show)

-- Note the order of constructors is the same as the ledger definitions
-- so that the Ord instance is consistent with the ledger one.
-- This is checked by prop_ord_distributive_TxMetadata

-- | Merge metadata maps. When there are clashing entries the left hand
-- side takes precedence.
instance Semigroup TxMetadata where
    TxMetadata m1 <> TxMetadata m2 = TxMetadata (m1 <> m2)

instance Monoid TxMetadata where
    mempty = TxMetadata mempty

makeTransactionMetadata :: Map Word64 TxMetadataValue -> TxMetadata
makeTransactionMetadata = TxMetadata

-- | Create a 'TxMetadataValue' from a 'Text' as a list of chunks of
-- an acceptable size.
metaTextChunks :: Text -> TxMetadataValue
metaTextChunks =
    TxMetaList
        . chunks
            txMetadataTextStringMaxByteLength
            TxMetaText
            (BS.length . T.encodeUtf8)
            utf8SplitAt
  where
    fromBuilder = T.Lazy.toStrict . T.Builder.toLazyText

    utf8SplitAt n =
        bimap fromBuilder fromBuilder
            . snd
            . T.foldl
                ( \(len, (left, right)) char ->
                    let sz =
                            BS.length
                                ( T.encodeUtf8
                                    (T.singleton char)
                                )
                    in  if len + sz > n
                            then
                                ( n + 1
                                ,
                                    ( left
                                    , right
                                        <> T.Builder.singleton
                                            char
                                    )
                                )
                            else
                                ( len + sz
                                ,
                                    ( left
                                        <> T.Builder.singleton
                                            char
                                    , right
                                    )
                                )
                )
                (0, (mempty, mempty))

-- | Create a 'TxMetadataValue' from a 'ByteString' as a list of
-- chunks of an acceptable size.
metaBytesChunks :: ByteString -> TxMetadataValue
metaBytesChunks =
    TxMetaList
        . chunks
            txMetadataByteStringMaxLength
            TxMetaBytes
            BS.length
            BS.splitAt

-- ----------------------------------------------------------------------------
-- Internal conversion functions
--

toShelleyMetadata
    :: Map Word64 TxMetadataValue -> Map Word64 Shelley.Metadatum
toShelleyMetadata = Map.map toShelleyMetadatum

toShelleyMetadatum :: TxMetadataValue -> Shelley.Metadatum
toShelleyMetadatum (TxMetaNumber x) = Shelley.I x
toShelleyMetadatum (TxMetaBytes x) = Shelley.B x
toShelleyMetadatum (TxMetaText x) = Shelley.S x
toShelleyMetadatum (TxMetaList xs) =
    Shelley.List
        [toShelleyMetadatum x | x <- xs]
toShelleyMetadatum (TxMetaMap xs) =
    Shelley.Map
        [ ( toShelleyMetadatum k
          , toShelleyMetadatum v
          )
        | (k, v) <- xs
        ]

fromShelleyMetadata
    :: Map Word64 Shelley.Metadatum -> Map Word64 TxMetadataValue
fromShelleyMetadata = Map.Lazy.map fromShelleyMetadatum

fromShelleyMetadatum :: Shelley.Metadatum -> TxMetadataValue
fromShelleyMetadatum (Shelley.I x) = TxMetaNumber x
fromShelleyMetadatum (Shelley.B x) = TxMetaBytes x
fromShelleyMetadatum (Shelley.S x) = TxMetaText x
fromShelleyMetadatum (Shelley.List xs) =
    TxMetaList
        [fromShelleyMetadatum x | x <- xs]
fromShelleyMetadatum (Shelley.Map xs) =
    TxMetaMap
        [ ( fromShelleyMetadatum k
          , fromShelleyMetadatum v
          )
        | (k, v) <- xs
        ]

-- | Transform a string-like structure into chunks with a maximum size;
-- Chunks are filled from left to right.
chunks
    :: Int
    -- ^ Chunk max size (inclusive)
    -> (str -> chunk)
    -- ^ Hoisting
    -> (str -> Int)
    -- ^ Measuring
    -> (Int -> str -> (str, str))
    -- ^ Splitting
    -> str
    -- ^ String
    -> [chunk]
chunks maxLength strHoist strLength strSplitAt str
    | strLength str > maxLength =
        let (h, t) = strSplitAt maxLength str
        in  strHoist h
                : chunks maxLength strHoist strLength strSplitAt t
    | otherwise =
        [strHoist str | strLength str > 0]

-- ----------------------------------------------------------------------------
-- Validate tx metadata
--

-- | Validate transaction metadata. This is for use with existing
-- constructed metadata values, e.g. constructed manually or decoded
-- from CBOR directly.
validateTxMetadata
    :: TxMetadata -> Either [(Word64, TxMetadataRangeError)] ()
validateTxMetadata (TxMetadata m) =
    case [ (k, err)
         | (k, v) <- toList m
         , err <- validateTxMetadataValue v
         ] of
        [] -> Right ()
        errs -> Left errs

validateTxMetadataValue :: TxMetadataValue -> [TxMetadataRangeError]
validateTxMetadataValue (TxMetaNumber n) =
    [ TxMetadataNumberOutOfRange n
    | n > fromIntegral (maxBound :: Word64)
        || n < negate (fromIntegral (maxBound :: Word64))
    ]
validateTxMetadataValue (TxMetaBytes bs) =
    [ TxMetadataBytesTooLong len
    | let len = BS.length bs
    , len > txMetadataByteStringMaxLength
    ]
validateTxMetadataValue (TxMetaText txt) =
    [ TxMetadataTextTooLong len
    | let len = BS.length (T.encodeUtf8 txt)
    , len > txMetadataTextStringMaxByteLength
    ]
validateTxMetadataValue (TxMetaList xs) =
    foldMap validateTxMetadataValue xs
validateTxMetadataValue (TxMetaMap kvs) =
    foldMap
        ( \(k, v) ->
            validateTxMetadataValue k
                <> validateTxMetadataValue v
        )
        kvs

-- | The maximum byte length of a transaction metadata text string
-- value.
txMetadataTextStringMaxByteLength :: Int
txMetadataTextStringMaxByteLength = 64

-- | The maximum length of a transaction metadata byte string value.
txMetadataByteStringMaxLength :: Int
txMetadataByteStringMaxLength = 64

-- | An error in transaction metadata due to an out-of-range value.
data TxMetadataRangeError
    = -- | The number is outside the maximum range of
      -- @-2^64-1 .. 2^64-1@.
      TxMetadataNumberOutOfRange !Integer
    | -- | The length of a text string metadatum value exceeds the
      -- maximum of 64 bytes as UTF8.
      TxMetadataTextTooLong !Int
    | -- | The length of a byte string metadatum value exceeds the
      -- maximum of 64 bytes.
      TxMetadataBytesTooLong !Int
    deriving (Eq, Show)

-- ----------------------------------------------------------------------------
-- JSON conversion
--

-- | Schema for JSON / tx-metadata conversion.
data TxMetadataJsonSchema
    = -- | Use the \"no schema\" mapping between JSON and tx metadata.
      TxMetadataJsonNoSchema
    | -- | Use the \"detailed schema\" mapping between JSON and tx
      -- metadata.
      TxMetadataJsonDetailedSchema
    deriving (Eq, Show)

-- | Convert a value from JSON into tx metadata, using the given choice
-- of mapping between JSON and tx metadata.
metadataFromJson
    :: TxMetadataJsonSchema
    -> Aeson.Value
    -> Either TxMetadataJsonError TxMetadata
metadataFromJson schema =
    \case
        Aeson.Object m ->
            fmap (TxMetadata . fromList)
                . mapM (uncurry metadataKeyPairFromJson)
                $ toList m
        _ -> Left TxMetadataJsonToplevelNotMap
  where
    metadataKeyPairFromJson
        :: Aeson.Key
        -> Aeson.Value
        -> Either
            TxMetadataJsonError
            (Word64, TxMetadataValue)
    metadataKeyPairFromJson k v = do
        k' <- convTopLevelKey k
        v' <-
            first
                (TxMetadataJsonSchemaError k' v)
                (metadataValueFromJson v)
        first
            (TxMetadataRangeError k' v)
            (validateMetadataValue v')
        return (k', v')

    convTopLevelKey
        :: Aeson.Key -> Either TxMetadataJsonError Word64
    convTopLevelKey key =
        case parseAll (pUnsigned <* Atto.endOfInput) k of
            Just n
                | n
                    <= fromIntegral (maxBound :: Word64) ->
                    Right (fromIntegral n)
            _ -> Left (TxMetadataJsonToplevelBadKey k)
      where
        k = Aeson.toText key

    validateMetadataValue
        :: TxMetadataValue
        -> Either TxMetadataRangeError ()
    validateMetadataValue v =
        case validateTxMetadataValue v of
            [] -> Right ()
            err : _ -> Left err

    metadataValueFromJson
        :: Aeson.Value
        -> Either TxMetadataJsonSchemaError TxMetadataValue
    metadataValueFromJson =
        case schema of
            TxMetadataJsonNoSchema ->
                metadataValueFromJsonNoSchema
            TxMetadataJsonDetailedSchema ->
                metadataValueFromJsonDetailedSchema

-- | Convert a tx metadata value into JSON, using the given choice of
-- mapping between JSON and tx metadata.
metadataToJson
    :: TxMetadataJsonSchema
    -> TxMetadata
    -> Aeson.Value
metadataToJson schema =
    \(TxMetadata mdMap) ->
        Aeson.object
            [ (Aeson.fromString (show k), metadataValueToJson v)
            | (k, v) <- toList mdMap
            ]
  where
    metadataValueToJson :: TxMetadataValue -> Aeson.Value
    metadataValueToJson =
        case schema of
            TxMetadataJsonNoSchema ->
                metadataValueToJsonNoSchema
            TxMetadataJsonDetailedSchema ->
                metadataValueToJsonDetailedSchema

-- ----------------------------------------------------------------------------
-- JSON conversion using the "no schema" style
--

metadataValueToJsonNoSchema :: TxMetadataValue -> Aeson.Value
metadataValueToJsonNoSchema = conv
  where
    conv :: TxMetadataValue -> Aeson.Value
    conv (TxMetaNumber n) = Aeson.Number (fromInteger n)
    conv (TxMetaBytes bs) =
        Aeson.String
            ( bytesPrefix
                <> T.decodeLatin1 (B16.encode bs)
            )
    conv (TxMetaText txt) = Aeson.String txt
    conv (TxMetaList vs) =
        Aeson.Array (fromList (map conv vs))
    conv (TxMetaMap kvs) =
        Aeson.object
            [ (convKey k, conv v)
            | (k, v) <- kvs
            ]

    convKey :: TxMetadataValue -> Aeson.Key
    convKey (TxMetaNumber n) = Aeson.fromString (show n)
    convKey (TxMetaBytes bs) =
        Aeson.fromText
            $ bytesPrefix
                <> T.decodeLatin1 (B16.encode bs)
    convKey (TxMetaText txt) = Aeson.fromText txt
    convKey v =
        Aeson.fromText
            . T.Lazy.toStrict
            . Aeson.Text.encodeToLazyText
            . conv
            $ v

metadataValueFromJsonNoSchema
    :: Aeson.Value
    -> Either
        TxMetadataJsonSchemaError
        TxMetadataValue
metadataValueFromJsonNoSchema = conv
  where
    conv
        :: Aeson.Value
        -> Either TxMetadataJsonSchemaError TxMetadataValue
    conv Aeson.Null = Left TxMetadataJsonNullNotAllowed
    conv Aeson.Bool{} = Left TxMetadataJsonBoolNotAllowed
    conv (Aeson.Number d) =
        case Scientific.floatingOrInteger d
                :: Either Double Integer of
            Left n -> Left (TxMetadataJsonNumberNotInteger n)
            Right n -> Right (TxMetaNumber n)
    conv (Aeson.String s)
        | Just s' <- T.stripPrefix bytesPrefix s
        , let bs' = T.encodeUtf8 s'
        , Right bs <- B16.decode bs'
        , not (B8.any (\c -> c >= 'A' && c <= 'F') bs') =
            Right (TxMetaBytes bs)
    conv (Aeson.String s) = Right (TxMetaText s)
    conv (Aeson.Array vs) =
        fmap TxMetaList
            . traverse conv
            $ toList vs
    conv (Aeson.Object kvs) =
        fmap
            ( TxMetaMap
                . sortCanonicalForCbor
            )
            . traverse
                ( (\(k, v) -> (,) (convKey k) <$> conv v)
                    . first Aeson.toText
                )
            $ toList kvs

    convKey :: Text -> TxMetadataValue
    convKey s =
        fromMaybe (TxMetaText s)
            $ parseAll
                ( (fmap TxMetaNumber pSigned <* Atto.endOfInput)
                    <|> ( fmap TxMetaBytes pBytes
                            <* Atto.endOfInput
                        )
                )
                s

-- | JSON strings that are base16 encoded and prefixed with
-- 'bytesPrefix' will be encoded as CBOR bytestrings.
bytesPrefix :: Text
bytesPrefix = "0x"

-- | Sorts the list by the first value in the tuple using the rules
-- for canonical CBOR (RFC 7049 section 3.9).
sortCanonicalForCbor
    :: [(TxMetadataValue, TxMetadataValue)]
    -> [(TxMetadataValue, TxMetadataValue)]
sortCanonicalForCbor =
    map snd
        . L.sortOn fst
        . map
            ( \e@(k, _) ->
                (CBOR.uintegerFromBytes $ serialiseKey k, e)
            )
  where
    serialiseKey =
        CBOR.serialize' CBOR.shelleyProtVer . toShelleyMetadatum

-- ----------------------------------------------------------------------------
-- JSON conversion using the "detailed schema" style
--

metadataValueToJsonDetailedSchema
    :: TxMetadataValue -> Aeson.Value
metadataValueToJsonDetailedSchema = conv
  where
    conv :: TxMetadataValue -> Aeson.Value
    conv (TxMetaNumber n) =
        singleFieldObject "int"
            . Aeson.Number
            $ fromInteger n
    conv (TxMetaBytes bs) =
        singleFieldObject "bytes"
            . Aeson.String
            $ T.decodeLatin1 (B16.encode bs)
    conv (TxMetaText txt) =
        singleFieldObject "string"
            . Aeson.String
            $ txt
    conv (TxMetaList vs) =
        singleFieldObject "list"
            . Aeson.Array
            $ fromList (map conv vs)
    conv (TxMetaMap kvs) =
        singleFieldObject "map"
            . Aeson.Array
            $ fromList
                [ Aeson.object
                    [("k", conv k), ("v", conv v)]
                | (k, v) <- kvs
                ]

    singleFieldObject name v = Aeson.object [(name, v)]

metadataValueFromJsonDetailedSchema
    :: Aeson.Value
    -> Either
        TxMetadataJsonSchemaError
        TxMetadataValue
metadataValueFromJsonDetailedSchema = conv
  where
    conv
        :: Aeson.Value
        -> Either
            TxMetadataJsonSchemaError
            TxMetadataValue
    conv (Aeson.Object m) =
        case toList m of
            [("int", Aeson.Number d)] ->
                case Scientific.floatingOrInteger d
                        :: Either Double Integer of
                    Left n ->
                        Left
                            (TxMetadataJsonNumberNotInteger n)
                    Right n -> Right (TxMetaNumber n)
            [("bytes", Aeson.String s)]
                | Right bs <-
                    B16.decode (T.encodeUtf8 s) ->
                    Right (TxMetaBytes bs)
            [("string", Aeson.String s)] ->
                Right (TxMetaText s)
            [("list", Aeson.Array vs)] ->
                fmap TxMetaList
                    . traverse conv
                    $ toList vs
            [("map", Aeson.Array kvs)] ->
                fmap TxMetaMap
                    . traverse convKeyValuePair
                    $ toList kvs
            [(key, v)]
                | key
                    `elem` [ "int"
                           , "bytes"
                           , "string"
                           , "list"
                           , "map"
                           ] ->
                    Left
                        ( TxMetadataJsonTypeMismatch
                            (Aeson.toText key)
                            v
                        )
            kvs ->
                Left
                    ( TxMetadataJsonBadObject
                        (first Aeson.toText <$> kvs)
                    )
    conv v = Left (TxMetadataJsonNotObject v)

    convKeyValuePair
        :: Aeson.Value
        -> Either
            TxMetadataJsonSchemaError
            (TxMetadataValue, TxMetadataValue)
    convKeyValuePair (Aeson.Object m)
        | KeyMap.size m == 2
        , Just k <- KeyMap.lookup "k" m
        , Just v <- KeyMap.lookup "v" m =
            (,) <$> conv k <*> conv v
    convKeyValuePair v = Left (TxMetadataJsonBadMapPair v)

-- ----------------------------------------------------------------------------
-- Shared JSON conversion error types
--

data TxMetadataJsonError
    = TxMetadataJsonToplevelNotMap
    | TxMetadataJsonToplevelBadKey !Text
    | TxMetadataJsonSchemaError
        !Word64
        !Aeson.Value
        !TxMetadataJsonSchemaError
    | TxMetadataRangeError
        !Word64
        !Aeson.Value
        !TxMetadataRangeError
    deriving (Eq, Show)

data TxMetadataJsonSchemaError
    = -- Only used for 'TxMetadataJsonNoSchema'
      TxMetadataJsonNullNotAllowed
    | TxMetadataJsonBoolNotAllowed
    | -- Used by both mappings
      TxMetadataJsonNumberNotInteger !Double
    | -- Only used for 'TxMetadataJsonDetailedSchema'
      TxMetadataJsonNotObject !Aeson.Value
    | TxMetadataJsonBadObject ![(Text, Aeson.Value)]
    | TxMetadataJsonBadMapPair !Aeson.Value
    | TxMetadataJsonTypeMismatch !Text !Aeson.Value
    deriving (Eq, Show)

-- ----------------------------------------------------------------------------
-- Shared parsing utils
--

parseAll :: Atto.Parser a -> Text -> Maybe a
parseAll p =
    either (const Nothing) Just
        . Atto.parseOnly p
        . T.encodeUtf8

pUnsigned :: Atto.Parser Integer
pUnsigned = do
    bs <- Atto.takeWhile1 Atto.isDigit
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
    when (B8.any hexUpper remaining)
        $ fail
            ( "Unexpected uppercase hex characters in "
                <> show remaining
            )
    case B16.decode remaining of
        Right bs -> return bs
        _ ->
            fail
                ( "Expecting base16 encoded string, found: "
                    <> show remaining
                )
  where
    hexUpper c = c >= 'A' && c <= 'F'
