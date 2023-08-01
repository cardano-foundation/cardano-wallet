{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Shared QuickCheck generators for wallet types.
--
-- Our convention is to let each test module define its own @Arbitrary@ orphans.
-- This module allows for code-reuse where desired, by providing generators.
module Cardano.Wallet.Primitive.Types.Tx.Metadata.Gen
    where

import Prelude

import Cardano.Api
    ( TxMetadata (..)
    , TxMetadataJsonSchema (..)
    , TxMetadataValue (..)
    , metadataFromJson
    )
import Data.Aeson
    ( ToJSON (..) )
import Data.ByteArray.Encoding
    ( Base (..), convertToBase )
import Data.List
    ( sortOn )
import Data.List.Extra
    ( nubOrdOn )
import Data.Text
    ( Text )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen

    , UnicodeString (..)

    , choose

    , listOf
    , listOf1
    , oneof
    , resize
    , scale
    , shrinkList
    , shrinkMap

    , suchThat
    , vector
    , vectorOf
    )

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

sizedMetadataValue :: Int -> Gen Aeson.Value
sizedMetadataValue 0 =
    oneof
        [ toJSON <$> arbitrary @Int
        , toJSON . ("0x"<>) . base16 <$> genByteString
        , toJSON <$> genTxMetaText
        ]
sizedMetadataValue n =
    oneof
        [ sizedMetadataValue 0
        , oneof
            [ toJSON . HM.fromList <$> resize n
                (listOf $ (,)
                    <$> genTxMetaText
                    <*> sizedMetadataValue (n-1)
                )
            , toJSON <$> resize n
                (listOf $ sizedMetadataValue (n-1)
                )
            ]
        ]

base16 :: BS.ByteString -> Text
base16 = T.decodeUtf8 . convertToBase Base16

genByteString :: Gen BS.ByteString
genByteString = B8.pack <$> (choose (0, 64) >>= vector)

shrinkByteString :: BS.ByteString -> [BS.ByteString]
shrinkByteString bs
    | n <= 1    = []
    | otherwise = [ BS.take (n `div` 2) bs, BS.drop (n `div` 2) bs ]
  where
    n = BS.length bs

genTxMetaText :: Gen Text
genTxMetaText =
    genUnchecked `suchThat` hasValidEncodedLength
  where
    genUnchecked :: Gen Text
    genUnchecked = T.pack . getUnicodeString <$> scale (min 64) arbitrary

    -- The UT8-encoded length of a metadata text value must not be greater
    -- than 64 bytes:
    hasValidEncodedLength :: Text -> Bool
    hasValidEncodedLength t = (&&)
        (encodedLength >   0)
        (encodedLength <= 64)
      where
        encodedLength :: Int
        encodedLength = BS.length $ T.encodeUtf8 t

shrinkTxMetaText :: Text -> [Text]
shrinkTxMetaText
    = filter (not . T.null)
    . shrinkMap (T.pack . getUnicodeString) (UnicodeString . T.unpack)

-- | Generates a 'TxMetadata' with arbitrary levels of nesting.
genNestedTxMetadata :: Gen TxMetadata
genNestedTxMetadata = do
    let (maxBreadth, maxDepth) = (3, 3)
    d <- scale (`mod` maxBreadth) $ listOf1 (sizedMetadataValue maxDepth)
    i <- vectorOf @Word (length d) arbitrary
    let json = toJSON $ HM.fromList $ zip i d
    case metadataFromJson TxMetadataJsonNoSchema json of
        Left e -> error $ show e <> ": " <> show (Aeson.encode json)
        Right metadata -> pure metadata

-- | Generates a 'TxMetadata' containing only simple values, without nesting.
genSimpleTxMetadata :: Gen TxMetadata
genSimpleTxMetadata = TxMetadata <$>
    (Map.singleton <$> arbitrary <*> genSimpleTxMetadataValue)

genSimpleTxMetadataValue :: Gen TxMetadataValue
genSimpleTxMetadataValue = oneof
    [ TxMetaNumber . fromIntegral <$> arbitrary @Int
    , TxMetaBytes <$> genByteString
    , TxMetaText <$> genTxMetaText
    ]

shrinkTxMetadata :: TxMetadata -> [TxMetadata]
shrinkTxMetadata (TxMetadata m) = TxMetadata . Map.fromList
    <$> shrinkList shrinkTxMetadataEntry (Map.toList m)
  where
    shrinkTxMetadataEntry (k, v) = (k,) <$> shrinkTxMetadataValue v

shrinkTxMetadataValue :: TxMetadataValue -> [TxMetadataValue]
shrinkTxMetadataValue (TxMetaMap xs) =
    TxMetaMap . sortOn fst . nubOrdOn fst <$> shrinkList shrinkPair xs
  where
    shrinkPair (k,v) =
        ((k,) <$> shrinkTxMetadataValue v) ++
        ((,v) <$> shrinkTxMetadataValue k)
shrinkTxMetadataValue (TxMetaList xs) =
    TxMetaList <$> filter (not . null) (shrinkList shrinkTxMetadataValue xs)
shrinkTxMetadataValue (TxMetaNumber i) = TxMetaNumber <$> shrink i
shrinkTxMetadataValue (TxMetaBytes b) = TxMetaBytes <$> shrinkByteString b
shrinkTxMetadataValue (TxMetaText s) = TxMetaText <$> shrinkTxMetaText s
