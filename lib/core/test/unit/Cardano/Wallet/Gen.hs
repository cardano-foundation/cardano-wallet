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
    , genSlotId
    , genActiveSlotCoefficient
    , shrinkActiveSlotCoefficient
    ) where

import Prelude

import Cardano.Address.Derivation
    ( xpubFromBytes )
import Cardano.Mnemonic
    ( ConsistentEntropy, EntropySize, Mnemonic, entropyToMnemonic )
import Cardano.Wallet.Primitive.Slotting
    ( flatSlot, unsafeEpochNo )
import Cardano.Wallet.Primitive.Types
    ( ActiveSlotCoefficient (..)
    , Address (..)
    , BlockHeader (..)
    , EpochLength (..)
    , Hash (..)
    , ProtocolMagic (..)
    , SlotId (..)
    , SlotInEpoch (..)
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
import Test.QuickCheck
    ( Arbitrary (..), Gen, choose, elements, vector )

import qualified Cardano.Byron.Codec.Cbor as CBOR
import qualified Codec.CBOR.Write as CBOR
import qualified Data.ByteString as BS

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


genSlotId :: EpochLength -> Gen SlotId
genSlotId (EpochLength el) | el > 0 = do
    ep <- choose (0, 10)
    sl <- choose (0, el - 1)
    return (SlotId (unsafeEpochNo ep) (SlotInEpoch sl))
genSlotId _ = error "genSlotId: epochLength must > 0"

genBlockHeader :: SlotId -> Gen BlockHeader
genBlockHeader sl = do
        BlockHeader sl (mockBlockHeight sl) <$> genHash <*> genHash
      where
        mockBlockHeight :: SlotId -> Quantity "block" Word32
        mockBlockHeight = Quantity . fromIntegral . flatSlot (EpochLength 200)

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
