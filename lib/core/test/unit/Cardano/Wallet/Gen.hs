{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Percentage (..), Quantity (..) )
import Data.Ratio
    ( denominator, numerator, (%) )
import Data.Word
    ( Word32 )
import GHC.TypeLits
    ( natVal )
import Shelley.Spec.Ledger.MetaData
    ( MetaData, MetaDatum )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , choose
    , elements
    , listOf
    , oneof
    , resize
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
        , MD.S <$> (T.pack <$> arbitrary)
        ]
sizedMetaDatum n =
    oneof
        [ MD.Map <$>
            (zip
                <$> (resize maxMetaDatumListLens (listOf (sizedMetaDatum (n-1))))
                <*> (listOf (sizedMetaDatum (n-1))))
        , MD.List <$> resize maxMetaDatumListLens (listOf (sizedMetaDatum (n-1)))
        , MD.I <$> arbitrary
        , MD.B <$> genByteString
        , MD.S <$> (T.pack <$> arbitrary)
        ]

genByteString :: Gen BS.ByteString
genByteString = BS.pack <$> arbitrary

genMetaDatum :: Gen MetaDatum
genMetaDatum = sizedMetaDatum maxMetaDatumDepth

genMetaData :: Gen MetaData
genMetaData = do
    d <- listOf genMetaDatum
    i <- vectorOf (length d) arbitrary
    pure $ MD.MetaData $ Map.fromList $ zip i d

shrinkMetaData :: MetaData -> [MetaData]
-- shrinkMetaData (MD.MetaData m) = MD.MetaData <$> shrinkMetaDatum m
shrinkMetaData _ = [] -- fixme: shrinking would be useful

genTxMetadata :: Gen TxMetadata
genTxMetadata = TxMetadata <$> genMetaData

shrinkTxMetadata :: TxMetadata -> [TxMetadata]
shrinkTxMetadata (TxMetadata m) = TxMetadata <$> shrinkMetaData m
