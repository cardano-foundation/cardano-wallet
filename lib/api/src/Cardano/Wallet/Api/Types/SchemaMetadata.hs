{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
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
import Cardano.Wallet.Primitive.Types.Tx
    ( TxMetadata
    )
import Control.Applicative
    ( (<|>)
    )
import Control.Monad
    ( guard
    , when
    )
import Control.DeepSeq
    ( NFData
    )
import Data.Aeson
    ( FromJSON (parseJSON)
    , ToJSON (toJSON)
    )
import Data.Bifunctor
    ( first
    )
import Data.ByteString
    ( ByteString
    )
import Data.Maybe
    ( fromMaybe
    )
import Data.Text
    ( Text
    )
import GHC.Generics
    ( Generic
    )
import Prelude

import qualified Cardano.Ledger.Binary as CBOR
import qualified Cardano.Ledger.Shelley.TxAuxData as Ledger
import qualified Codec.CBOR.Magic as CBOR
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Aeson
import qualified Data.Aeson.KeyMap as Aeson
import qualified Data.Attoparsec.ByteString.Char8 as Atto
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as BS16
import qualified Data.ByteString.Char8 as BS8
import qualified Data.List as L
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
      , Right bs <- BS16.decode bs'
      , not (BS8.any (\c -> c >= 'A' && c <= 'F') bs')
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
    guard (not (BS.length bs > 1 && BS8.head bs == '0'))
    return $! BS.foldl' step 0 bs
  where
    step a w = a * 10 + fromIntegral (w - 48)

pSigned :: Atto.Parser Integer
pSigned = Atto.signed pUnsigned

pBytes :: Atto.Parser ByteString
pBytes = do
  _ <- Atto.string "0x"
  remaining <- Atto.takeByteString
  when (BS8.any hexUpper remaining) $ fail ("Unexpected uppercase hex characters in " <> show remaining)
  case BS16.decode remaining of
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
