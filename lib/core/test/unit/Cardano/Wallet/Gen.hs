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
    , genActiveSlotCoefficient
    , shrinkActiveSlotCoefficient
    , genSlotNo
    , shrinkSlotNo
    , genTxMetadata
    , shrinkTxMetadata
    ) where

import Prelude

import Cardano.Address.Derivation
    ( xpubFromBytes )
import Cardano.Api.Typed
    ( TxMetadata (..) )
import Cardano.Mnemonic
    ( ConsistentEntropy, EntropySize, Mnemonic, entropyToMnemonic )
import Cardano.Wallet.Primitive.Types
    ( ActiveSlotCoefficient (..)
    , Address (..)
    , BlockHeader (..)
    , Hash (..)
    , ProtocolMagic (..)
    , SlotNo (..)
    )
import Cardano.Wallet.Unsafe
    ( unsafeMkEntropy, unsafeMkPercentage )
import Data.List
    ( sortOn )
import Data.List.Extra
    ( nubOn )
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
import Shelley.Spec.Ledger.MetaData
    ( MetaData, MetaDatum )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , PrintableString (..)
    , choose
    , elements
    , listOf
    , listOf1
    , oneof
    , resize
    , shrinkList
    , shrinkMap
    , vector
    , vectorOf
    )

import qualified Cardano.Byron.Codec.Cbor as CBOR
import qualified Codec.CBOR.Write as CBOR
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Shelley.Spec.Ledger.MetaData as MD

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

genBlockHeader :: SlotNo -> Gen BlockHeader
genBlockHeader sl = do
        BlockHeader sl (mockBlockHeight sl) <$> genHash <*> genHash
      where
        mockBlockHeight :: SlotNo -> Quantity "block" Word32
        mockBlockHeight = Quantity . fromIntegral . unSlotNo

        genHash = elements
            [ Hash "BLOCK01"
            , Hash "BLOCK02"
            , Hash "BLOCK03"
            ]

genActiveSlotCoefficient :: Gen ActiveSlotCoefficient
genActiveSlotCoefficient = ActiveSlotCoefficient <$> choose (0.001, 1.0)

shrinkActiveSlotCoefficient :: ActiveSlotCoefficient -> [ActiveSlotCoefficient]
shrinkActiveSlotCoefficient (ActiveSlotCoefficient f)
        | f < 1 = [1]
        | otherwise = []


maxMetaDatumDepth :: Int
maxMetaDatumDepth = 2

maxMetaDatumListLens :: Int
maxMetaDatumListLens = 5

sizedMetaDatum :: Int -> Gen MetaDatum
sizedMetaDatum 0 =
    oneof
        [ MD.I <$> arbitrary
        , MD.B <$> genByteString
        , MD.S <$> genCappedString 32
        ]
sizedMetaDatum n =
    oneof
        [ sizedMetaDatum 0
        , oneof
            -- FIXME: #2098
            --
            -- Allow empty maps and lists once fixed.
            [ MD.Map . sortOn fst . nubOn fst <$>
                resize maxMetaDatumListLens
                    (listOf1 $ (,)
                        <$> genMetaDatumJSONKey
                        <*> sizedMetaDatum (n-1)
                    )
            -- FIXME: #2098
            --
            -- jsonToMetadata does an implicit conversion of list of pairs to map,
            -- which fails to roundtrip back to the same value. Until this is
            -- fixed in #2098, we only allow list to contains non-ambiguous values.
            , MD.List <$> resize maxMetaDatumListLens (listOf1 (sizedMetaDatum 0))
            ]
        ]

genByteString :: Gen BS.ByteString
genByteString = BS.pack <$> arbitrary

shrinkByteString :: BS.ByteString -> [BS.ByteString]
shrinkByteString = shrinkMap BS.pack BS.unpack

genCappedString :: Int -> Gen Text
genCappedString sup = T.take sup . T.pack . getPrintableString <$> arbitrary

genMetaDatum :: Gen MetaDatum
genMetaDatum = sizedMetaDatum maxMetaDatumDepth

-- FIXME: #2098
-- We only expect metadata to roundtrip when coming from valid JSON.
-- JSON only allows keys as strings.
--
-- This is only necessary until #2098 is addressed and because the conversion
-- functions from cardano-api are doing some implicit conversions.
genMetaDatumJSONKey :: Gen MetaDatum
genMetaDatumJSONKey = MD.S <$> genCappedString 32

shrinkMetaDatum :: MetaDatum -> [MetaDatum]
shrinkMetaDatum (MD.Map xs) =
    MD.Map . sortOn fst . nubOn fst <$> shrinkList shrinkPair xs
  where
    shrinkPair (k,v) =
        ((k,) <$> shrinkMetaDatum v) ++
        ((,v) <$> shrinkMetaDatum k)
shrinkMetaDatum (MD.List xs) =
    MD.List <$> filter ((>= 1) . length) (shrinkList shrinkMetaDatum xs)
shrinkMetaDatum (MD.I i) = MD.I <$> shrink i
shrinkMetaDatum (MD.B b) = MD.B <$> shrinkByteString b
shrinkMetaDatum (MD.S s) = MD.S <$> shrinkMap T.pack T.unpack s

genMetaData :: Gen MetaData
genMetaData = do
    d <- listOf genMetaDatum
    i <- vectorOf (length d) arbitrary
    pure $ MD.MetaData $ Map.fromList $ zip i d

shrinkMetaData :: MetaData -> [MetaData]
shrinkMetaData (MD.MetaData m) = MD.MetaData . Map.fromList
    <$> shrinkList shrinkMetaDataEntry (Map.toList m)
  where
    shrinkMetaDataEntry (k, v) = (k,) <$> shrinkMetaDatum v

genTxMetadata :: Gen TxMetadata
genTxMetadata = TxMetadata <$> genMetaData

shrinkTxMetadata :: TxMetadata -> [TxMetadata]
shrinkTxMetadata (TxMetadata m) = TxMetadata <$> shrinkMetaData m
