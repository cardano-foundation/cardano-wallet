-- TODO: https://cardanofoundation.atlassian.net/browse/ADP-2841
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
#if __GLASGOW_HASKELL__ >= 902
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}
#endif

module Cardano.Pool.DB.Arbitrary
  ( MultiPoolCertificateSequence (..)
  , getMultiPoolCertificateSequence
  , SinglePoolCertificateSequence (..)
  , StakePoolsFixture (..)
  , ManyPoolCertificates (..)
  , genStakePoolMetadata
  , isValidSinglePoolCertificateSequence
  )
where

import Cardano.Pool.Metadata.Types
  ( StakePoolMetadata (..)
  , StakePoolMetadataHash (..)
  , StakePoolMetadataUrl (..)
  )
import Cardano.Pool.Types
  ( PoolId (..)
  , PoolOwner (..)
  , StakePoolTicker (..)
  )
import Cardano.Wallet.Gen
  ( genPercentage
  , genSlotNo
  , shrinkSlotNo
  )
import Cardano.Wallet.Primitive.Types
  ( BlockHeader (..)
  , CertificatePublicationTime (..)
  , EpochNo (..)
  , PoolCertificate (..)
  , PoolMetadataSource (..)
  , PoolRegistrationCertificate (..)
  , PoolRetirementCertificate (..)
  , Settings (..)
  , SlotInEpoch (..)
  , SlotNo (..)
  , SmashServer
  , getPoolCertificatePoolId
  , setPoolCertificatePoolId
  , unsafeEpochNo
  )
import Cardano.Wallet.Primitive.Types.Coin
  ( Coin (..)
  )
import Cardano.Wallet.Primitive.Types.Hash
  ( Hash (..)
  )
import Cardano.Wallet.Primitive.Types.Tx.TxOut.Gen
  ( genTxOutCoin
  , shrinkTxOutCoin
  )
import Control.Arrow
  ( second
  )
import Control.Monad
  ( foldM
  , replicateM
  )
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as B8
import Data.Function
  ( (&)
  )
import Data.Generics.Internal.VL.Lens
  ( view
  , (^.)
  )
import Data.List qualified as L
import Data.Maybe
  ( fromJust
  )
import Data.Ord
  ( Down (..)
  )
import Data.Quantity
  ( Quantity (..)
  )
import Data.Text
  ( Text
  )
import Data.Text qualified as T
import Data.Word
  ( Word32
  , Word64
  )
import Data.Word.Odd
  ( Word31
  )
import GHC.Generics
  ( Generic
  )
import Network.URI
  ( URI
  , parseURI
  )
import Test.QuickCheck
  ( Arbitrary (..)
  , Gen
  , PrintableString (..)
  , arbitraryBoundedEnum
  , arbitrarySizedBoundedIntegral
  , choose
  , elements
  , frequency
  , oneof
  , scale
  , shrinkIntegral
  , shuffle
  , vector
  , vectorOf
  )
import Test.QuickCheck.Arbitrary.Generic
  ( genericArbitrary
  , genericShrink
  )
import Test.QuickCheck.Extra
  ( reasonablySized
  )
import Prelude

{-------------------------------------------------------------------------------
                                 Modifiers
-------------------------------------------------------------------------------}

data StakePoolsFixture = StakePoolsFixture
  { poolSlots :: [(PoolId, BlockHeader)]
  , rollbackSlots :: [SlotNo]
  }
  deriving stock (Eq, Show)

genPrintableText :: Gen Text
genPrintableText = T.pack . getPrintableString <$> arbitrary

{-------------------------------------------------------------------------------
                                 Stake Pools
-------------------------------------------------------------------------------}

instance Arbitrary CertificatePublicationTime where
  arbitrary = CertificatePublicationTime <$> arbitrary <*> arbitrary
  shrink = genericShrink

instance Arbitrary SlotNo where
  arbitrary = genSlotNo
  shrink = shrinkSlotNo

instance Arbitrary SlotInEpoch where
  shrink (SlotInEpoch x) = SlotInEpoch <$> shrink x
  arbitrary = SlotInEpoch <$> choose (0, fromIntegral arbitraryChainLength)

instance Arbitrary EpochNo where
  shrink (EpochNo x) = EpochNo <$> shrink x
  arbitrary = unsafeEpochNo <$> choose (0, arbitraryEpochLength)

instance Arbitrary Word31 where
  arbitrary = arbitrarySizedBoundedIntegral
  shrink = shrinkIntegral

instance Arbitrary (Quantity "lovelace" Word64) where
  shrink (Quantity q) = [Quantity q' | q' <- shrink q]
  arbitrary = Quantity <$> arbitrary

instance Arbitrary Coin where
  shrink = shrinkTxOutCoin
  arbitrary = genTxOutCoin

arbitraryEpochLength :: Word32
arbitraryEpochLength = 100

arbitraryChainLength :: Word32
arbitraryChainLength = 10

-- NOTE Expected to have a high entropy
instance Arbitrary PoolId where
  arbitrary = PoolId . BS.pack <$> vector 32
  shrink (PoolId p) = do
    let
      s = BS.unpack p
    result <-
      [ -- Zero out everything:
        replicate 32 z
        , -- Zero out different halves:
          replicate 16 z <> drop 16 s
        , take 16 s <> replicate 16 z
        , -- Zero out different quarters:
          replicate 8 z <> drop 8 s
        , take 8 s <> replicate 8 z <> drop 16 s
        , take 16 s <> replicate 8 z <> drop 24 s
        , take 24 s <> replicate 8 z
        ]
    [PoolId $ BS.pack result | result /= s]
    where
      z = toEnum 0

-- NOTE Excepted to have a reasonably small entropy
instance Arbitrary PoolOwner where
  arbitrary = do
    byte <- elements ['0' .. '8']
    return $ PoolOwner $ B8.pack (replicate 32 byte)

instance Arbitrary PoolRegistrationCertificate where
  shrink regCert = do
    shrunkPoolId <- shrink $ view #poolId regCert
    shrunkPoolOwners <- shrinkOwners $ view #poolOwners regCert
    pure
      regCert
        { poolId = shrunkPoolId
        , poolOwners = shrunkPoolOwners
        }
    where
      shrinkOwners os =
        -- A valid registration certificate must have at least one owner:
        [ps | ps <- shrink os, not (null ps)]
  arbitrary =
    PoolRegistrationCertificate
      <$> arbitrary
      <*> genOwners
      <*> genPercentage
      <*> arbitrary
      <*> arbitrary
      <*> oneof [pure Nothing, Just <$> genMetadata]
    where
      genMetadata =
        (,)
          <$> fmap StakePoolMetadataUrl genURL
          <*> arbitrary
      genOwners = do
        -- A valid registration certificate must have at least one owner:
        ownerCount <- choose (1, 4)
        replicateM ownerCount arbitrary
      genURL = do
        protocol <- elements ["http", "https"]
        fstP <- elements ["cardano", "ada", "pool", "staking", "reward"]
        sndP <- elements ["rocks", "moon", "digital", "server", "fast"]
        extP <- elements [".io", ".dev", ".com", ".eu"]
        pure $ protocol <> "://" <> fstP <> "-" <> sndP <> extP

instance Arbitrary PoolRetirementCertificate where
  arbitrary =
    PoolRetirementCertificate
      <$> arbitrary
      <*> arbitrary
  shrink = genericShrink

instance Arbitrary PoolCertificate where
  arbitrary =
    oneof
      [ Registration
          <$> arbitrary
      , Retirement
          <$> arbitrary
      ]
  shrink = genericShrink

-- | Represents a valid sequence of registration and retirement certificates
--   for a single pool.
data SinglePoolCertificateSequence = SinglePoolCertificateSequence
  { getSinglePoolId :: PoolId
  , getSinglePoolCertificateSequence :: [PoolCertificate]
  }
  deriving (Eq, Show)

isValidSinglePoolCertificateSequence :: SinglePoolCertificateSequence -> Bool
isValidSinglePoolCertificateSequence
  (SinglePoolCertificateSequence sharedPoolId certificates) =
    allCertificatesReferToSamePool
      && firstCertificateIsNotRetirement
    where
      allCertificatesReferToSamePool =
        all (== sharedPoolId) (getPoolCertificatePoolId <$> certificates)
      firstCertificateIsNotRetirement = case certificates of
        [] -> True
        Registration _ : _ -> True
        Retirement _ : _ -> False

instance Arbitrary SinglePoolCertificateSequence where
  arbitrary = do
    sharedPoolId <- arbitrary
    frequency
      [ (1, genEmptySequence sharedPoolId)
      , (9, genNonEmptySequence sharedPoolId)
      ]
    where
      genEmptySequence sharedPoolId =
        pure $ SinglePoolCertificateSequence sharedPoolId []
      genNonEmptySequence sharedPoolId = do
        -- We must start with a registration certificate:
        certificates <-
          (:)
            <$> (Registration <$> arbitrary)
            <*> reasonablySized arbitrary
        pure
          $ SinglePoolCertificateSequence sharedPoolId
          $ setPoolCertificatePoolId sharedPoolId <$> certificates

  {- HLINT ignore "Functor law" -}
  shrink (SinglePoolCertificateSequence sharedPoolId certificates) =
    genericShrink certificates
      & fmap (fmap (setPoolCertificatePoolId sharedPoolId))
      & fmap (SinglePoolCertificateSequence sharedPoolId)
      & filter isValidSinglePoolCertificateSequence

-- | Represents valid sequences of registration and retirement certificates
--   for multiple pools.
--
-- Use 'getMultiPoolCertificateSequence' to obtain a single, flattened list
-- of pool certificates.
data MultiPoolCertificateSequence = MultiPoolCertificateSequence
  { getSerializationMethod :: ListSerializationMethod
  , getSinglePoolSequences :: [SinglePoolCertificateSequence]
  }
  deriving (Eq, Show)

instance Arbitrary MultiPoolCertificateSequence where
  arbitrary =
    MultiPoolCertificateSequence
      <$> arbitrary
      <*> scale (min 50) arbitrary
  shrink mpcs =
    [ MultiPoolCertificateSequence (getSerializationMethod mpcs) sequences
    | sequences <- shrink (getSinglePoolSequences mpcs)
    ]

getMultiPoolCertificateSequence
  :: MultiPoolCertificateSequence -> [PoolCertificate]
getMultiPoolCertificateSequence mpcs =
  serializeLists
    (getSerializationMethod mpcs)
    (getSinglePoolCertificateSequence <$> getSinglePoolSequences mpcs)

-- | Indicates a way to serialize a list of lists into a single list.
data ListSerializationMethod
  = Interleave
  | Concatenate
  deriving (Bounded, Enum, Eq, Show)

-- | Serializes a list of lists into a single list using the given
--   serialization method.
serializeLists :: ListSerializationMethod -> [[a]] -> [a]
serializeLists = \case
  Interleave -> interleave
  Concatenate -> concat

newtype ManyPoolCertificates cert
  = ManyPoolCertificates [(CertificatePublicationTime, cert)]
  deriving (Eq, Show, Generic)

instance Arbitrary cert => Arbitrary (ManyPoolCertificates cert) where
  shrink = genericShrink
  arbitrary = ManyPoolCertificates <$> reasonablySized arbitrary

-- Interleaves the given list of lists together in a fair way.
--
-- Example:
--
-- >>> interleave [["a1", "a2", "a3"], ["b1", "b2", "b3"], ["c1", "c2", "c3"]]
-- ["a1", "b1", "c1", "a2", "b2", "c3", "a3", "b3", "c3"]
--
interleave :: [[a]] -> [a]
interleave = concat . L.transpose

instance Arbitrary ListSerializationMethod where
  arbitrary = arbitraryBoundedEnum
  shrink = const []

instance Arbitrary StakePoolMetadataHash where
  arbitrary = fmap (StakePoolMetadataHash . BS.pack) (vector 32)

genStakePoolMetadata
  :: StakePoolMetadataUrl
  -> Gen StakePoolMetadata
genStakePoolMetadata (StakePoolMetadataUrl url) =
  StakePoolMetadata
    <$> genStakePoolTicker
    <*> genPrintableText
    <*> oneof [pure Nothing, Just <$> genPrintableText]
    <*> pure url

genStakePoolTicker :: Gen StakePoolTicker
genStakePoolTicker =
  (StakePoolTicker . T.pack)
    <$> (choose (3, 5) >>= \n -> vectorOf n (elements ['A', 'B' .. 'Z']))

instance Arbitrary StakePoolsFixture where
  -- Shrink the second element
  shrink (StakePoolsFixture (p0 : (p, bh) : ps) r) = map reconstruct $ shrink (bh ^. #slotNo)
    where
      reconstruct s =
        StakePoolsFixture
          (p0 : (p, bh {slotNo = s} :: BlockHeader) : ps)
          (map (\x -> if x == bh ^. #slotNo then s else x) r)
  -- Shrink the first element
  shrink (StakePoolsFixture ((p, bh) : ps) r) = map reconstruct $ shrink (bh ^. #slotNo)
    where
      reconstruct s =
        StakePoolsFixture
          ((p, bh {slotNo = s} :: BlockHeader) : ps)
          (map (\x -> if x == bh ^. #slotNo then s else x) r)
  shrink _ = []

  arbitrary = do
    poolsNumber <- choose (1, 5)
    pools <- vector poolsNumber
    slotsNumber <- choose (0, 2)
    firstSlot <- arbitrary
    slotsGenerated <-
      foldM (appendPair pools) [] (generateNextSlots [firstSlot] slotsNumber)
    rNum <- choose (1, slotsNumber + 1)
    rSlots <-
      (L.sortOn Down . take rNum) <$> shuffle (map snd slotsGenerated)
    pure $ StakePoolsFixture (second mkBlockHeader <$> slotsGenerated) rSlots
    where
      mkBlockHeader :: SlotNo -> BlockHeader
      mkBlockHeader s =
        BlockHeader
          { slotNo = s
          , blockHeight = Quantity 0
          , headerHash = Hash "00000000000000000000000000000001"
          , parentHeaderHash = Just $ Hash "00000000000000000000000000000000"
          }

      generateNextSlots :: [SlotNo] -> Int -> [SlotNo]
      generateNextSlots slots@(s : _) num =
        if (num < 1)
          then reverse slots
          else generateNextSlots ((s + 1) : slots) (num - 1)
      generateNextSlots [] _ = []

      appendPair
        :: [PoolId]
        -> [(PoolId, SlotNo)]
        -> SlotNo
        -> Gen [(PoolId, SlotNo)]
      appendPair pools pairs slot = do
        pool <- elements pools
        return $ (pool, slot) : pairs

instance Arbitrary URI where
  arbitrary =
    elements
      [ fromJust (parseURI "https://my.little.friend")
      , fromJust (parseURI "http://its-friday.com:8000")
      ]

instance Arbitrary SmashServer where
  shrink = genericShrink
  arbitrary = genericArbitrary

instance Arbitrary PoolMetadataSource where
  shrink = genericShrink
  arbitrary = genericArbitrary

instance Arbitrary Settings where
  shrink = genericShrink
  arbitrary = genericArbitrary
