{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Shared QuickCheck generators for wallet types.
--
-- Our convention is to let each test module define its own @Arbitrary@ orphans.
-- This module allows for code-reuse where desired, by providing generators.
module Cardano.Wallet.Gen
    ( genMnemonic
    , genPercentage
    , shrinkPercentage
    , genLegacyAddress
    , genBlockHeader
    , genChainPoint
    , genSlot
    , genActiveSlotCoefficient
    , shrinkActiveSlotCoefficient
    , genSlotNo
    , shrinkSlotNo
    , genNestedTxMetadata
    , genSimpleTxMetadata
    , shrinkTxMetadata
    , genScript
    , genScriptCosigners
    , genScriptTemplate
    , genMockXPub
    , genNatural
    , genWalletId
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPub, xpubFromBytes )
import Cardano.Address.Script
    ( Cosigner (..), Script (..), ScriptTemplate (..) )
import Cardano.Api
    ( TxMetadata (..)
    , TxMetadataJsonSchema (..)
    , TxMetadataValue (..)
    , metadataFromJson
    )
import Cardano.Mnemonic
    ( ConsistentEntropy, EntropySize, Mnemonic, entropyToMnemonic )
import Cardano.Wallet.Primitive.AddressDiscovery.Shared
    ( retrieveAllCosigners )
import Cardano.Wallet.Primitive.Types
    ( ActiveSlotCoefficient (..)
    , BlockHeader (..)
    , ChainPoint (..)
    , Slot
    , SlotNo (..)
    , WalletId (..)
    , WithOrigin (..)
    )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..) )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Cardano.Wallet.Primitive.Types.ProtocolMagic
    ( ProtocolMagic (..) )
import Cardano.Wallet.Unsafe
    ( unsafeFromHex, unsafeMkEntropy, unsafeMkPercentage )
import Control.Monad
    ( replicateM )
import Crypto.Hash
    ( hash )
import Data.Aeson
    ( ToJSON (..) )
import Data.ByteArray.Encoding
    ( Base (..), convertToBase )
import Data.List
    ( sortOn )
import Data.List.Extra
    ( nubOrdOn )
import Data.Maybe
    ( fromMaybe )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Percentage (..), Quantity (..) )
import Data.Ratio
    ( denominator, numerator, (%) )
import Data.Text
    ( Text )
import Data.Word
    ( Word32 )
import GHC.TypeLits
    ( natVal )
import Numeric.Natural
    ( Natural )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , Positive (..)
    , UnicodeString (..)
    , arbitrarySizedNatural
    , choose
    , elements
    , frequency
    , listOf
    , listOf1
    , oneof
    , resize
    , scale
    , shrinkList
    , shrinkMap
    , sized
    , sublistOf
    , suchThat
    , vector
    , vectorOf
    )

import qualified Cardano.Byron.Codec.Cbor as CBOR
import qualified Codec.CBOR.Write as CBOR
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

-- | Generates an arbitrary mnemonic of a size according to the type parameter.
--
-- E.g:
-- >>> arbitrary = SomeMnemonic <$> genMnemonic @12
genMnemonic
    :: forall mw ent csz.
     ( ConsistentEntropy ent mw csz
     , EntropySize mw ~ ent
     )
    => Gen (Mnemonic mw)
genMnemonic = do
        let n = fromIntegral (natVal $ Proxy @(EntropySize mw)) `div` 8
        bytes <- BS.pack <$> vector n
        let ent = unsafeMkEntropy @(EntropySize mw) bytes
        return $ entropyToMnemonic ent

genPercentage :: Gen Percentage
genPercentage = unsafeMkPercentage . fromRational . toRational <$> genDouble
  where
    genDouble :: Gen Double
    genDouble = choose (0, 1)

shrinkPercentage :: Percentage -> [Percentage]
shrinkPercentage x = unsafeMkPercentage <$>
    ((% q) <$> shrink p) ++ (map (p %) . filter (/= 0) $ shrink q)
  where
    p = numerator $ getPercentage x
    q = denominator $ getPercentage x

genLegacyAddress
    :: Maybe ProtocolMagic
    -> Gen Address
genLegacyAddress pm = do
    bytes <- BS.pack <$> vector 64
    let (Just key) = xpubFromBytes bytes
    pure $ Address
        $ CBOR.toStrictByteString
        $ CBOR.encodeAddress key
        $ maybe [] (pure . CBOR.encodeProtocolMagicAttr) pm

--
-- Slotting
--


-- | Don't generate /too/ large slots
genSlotNo :: Gen SlotNo
genSlotNo = SlotNo . fromIntegral <$> arbitrary @Word32

shrinkSlotNo :: SlotNo -> [SlotNo]
shrinkSlotNo (SlotNo x) = map SlotNo $ shrink x

genChainPoint :: Gen ChainPoint
genChainPoint = frequency
    [ ( 1, pure ChainPointAtGenesis)  -- "common" but not "very common"
    , (40, toChainPoint <$> (genBlockHeader =<< genSlotNo))
    ]
  where
    toChainPoint (BlockHeader slot _ h _) = ChainPoint slot h

genSlot :: Gen Slot
genSlot = frequency
    [ ( 1, pure Origin)
    , (40, At <$> genSlotNo)
    ]

genBlockHeader :: SlotNo -> Gen BlockHeader
genBlockHeader sl = do
        BlockHeader sl (mockBlockHeight sl) <$> genHash <*> (Just <$> genHash)
      where
        mockBlockHeight :: SlotNo -> Quantity "block" Word32
        mockBlockHeight = Quantity . fromIntegral . unSlotNo

        genHash = elements
            [ Hash $ unsafeFromHex
                "aac1308b9868af89c396b08ff6f3cfea8e0859c94d1b3bc834baeaaff8645448"
            , Hash $ unsafeFromHex
                "d93b27cc7bb6fd2fe6ee42de5328c13606bb714a78475a41335207d2afd6026e"
            , Hash $ unsafeFromHex
                "63b8828e2eadc3f14b9b691fa9df76139a9c9b13a12ec862b324cc5a88f9fcc5"
            ]

genActiveSlotCoefficient :: Gen ActiveSlotCoefficient
genActiveSlotCoefficient = ActiveSlotCoefficient <$> choose (0.001, 1.0)

shrinkActiveSlotCoefficient :: ActiveSlotCoefficient -> [ActiveSlotCoefficient]
shrinkActiveSlotCoefficient (ActiveSlotCoefficient f)
        | f < 1 = [1]
        | otherwise = []

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

genNatural :: Gen Natural
genNatural = arbitrarySizedNatural

genScript :: [a] -> Gen (Script a)
genScript elems = scale (`div` 3) $ sized scriptTree
    where
        scriptTree 0 = oneof
            [ RequireSignatureOf <$> elements elems
            , ActiveFromSlot <$> genNatural
            , ActiveUntilSlot <$> genNatural
            ]
        scriptTree n = do
            Positive m <- arbitrary
            let n' = n `div` (m + 1)
            scripts' <- vectorOf m (scriptTree n')
            atLeast <- choose (1, fromIntegral m)
            elements
                [ RequireAllOf scripts'
                , RequireAnyOf scripts'
                , RequireSomeOf atLeast scripts'
                ]

genScriptCosigners :: Gen (Script Cosigner)
genScriptCosigners = do
    numOfCosigners <- choose (1,10)
    genScript $ Cosigner <$> [0..numOfCosigners]

genScriptTemplate :: Gen ScriptTemplate
genScriptTemplate = do
    script <- genScriptCosigners `suchThat` (not . null . retrieveAllCosigners)
    let scriptCosigners = retrieveAllCosigners script
    cosignersSubset <- sublistOf scriptCosigners `suchThat` (not . null)
    xpubs <- vectorOf (length cosignersSubset) genMockXPub
    pure $ ScriptTemplate (Map.fromList $ zip cosignersSubset xpubs) script

genMockXPub :: Gen XPub
genMockXPub = fromMaybe impossible . xpubFromBytes . BS.pack <$> genBytes
  where
    genBytes = vectorOf 64 arbitrary
    impossible = error "incorrect length in genMockXPub"

genWalletId :: Gen WalletId
genWalletId = do
    bytes <- BS.pack <$> replicateM 16 arbitrary
    pure $ WalletId (hash bytes)
