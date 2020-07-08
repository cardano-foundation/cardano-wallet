{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Pool.DB.Arbitrary
    ( StakePoolsFixture (..)
    , genStakePoolMetadata
    ) where

import Prelude

import Cardano.Wallet.DummyTarget.Primitive.Types
    ( dummyGenesisParameters )
import Cardano.Wallet.Gen
    ( genPercentage )
import Cardano.Wallet.Primitive.Types
    ( BlockHeader (..)
    , CertificatePublicationTime (..)
    , EpochLength (..)
    , EpochNo (..)
    , Hash (..)
    , PoolCertificate (..)
    , PoolId (..)
    , PoolOwner (..)
    , PoolRegistrationCertificate (..)
    , PoolRetirementCertificate (..)
    , SlotId (..)
    , SlotInternalIndex (..)
    , SlotNo (..)
    , SlotParameters (..)
    , StakePoolMetadata (..)
    , StakePoolMetadataHash (..)
    , StakePoolMetadataUrl (..)
    , StakePoolTicker (..)
    , slotSucc
    , unsafeEpochNo
    )
import Control.Arrow
    ( second )
import Control.Monad
    ( foldM )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.Ord
    ( Down (..) )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text )
import Data.Word
    ( Word32, Word64 )
import Data.Word.Odd
    ( Word31 )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , PrintableString (..)
    , applyArbitrary2
    , arbitrarySizedBoundedIntegral
    , choose
    , elements
    , genericShrink
    , listOf
    , oneof
    , scale
    , shrinkIntegral
    , shrinkList
    , shuffle
    , vector
    , vectorOf
    )

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.List as L
import qualified Data.Text as T

{-------------------------------------------------------------------------------
                                 Modifiers
-------------------------------------------------------------------------------}

data StakePoolsFixture = StakePoolsFixture
    { poolSlots :: [(PoolId, BlockHeader)]
    , rollbackSlots :: [SlotId] }
    deriving stock (Eq, Show)

genPrintableText :: Gen Text
genPrintableText = T.pack . getPrintableString <$> arbitrary

{-------------------------------------------------------------------------------
                                 Stake Pools
-------------------------------------------------------------------------------}

instance Arbitrary CertificatePublicationTime where
    arbitrary = CertificatePublicationTime <$> arbitrary <*> arbitrary
    shrink = genericShrink

instance Arbitrary SlotId where
    shrink (SlotId ep sl) =
        uncurry SlotId <$> shrink (ep, sl)
    arbitrary = applyArbitrary2 SlotId

instance Arbitrary SlotInternalIndex where
    arbitrary = SlotInternalIndex <$> arbitrary
    shrink = fmap SlotInternalIndex . shrink . unSlotInternalIndex

instance Arbitrary SlotNo where
    shrink (SlotNo x) = SlotNo <$> shrink x
    arbitrary = SlotNo <$> choose (0, fromIntegral arbitraryChainLength)

instance Arbitrary EpochNo where
    shrink (EpochNo x) = EpochNo <$> shrink x
    arbitrary = unsafeEpochNo <$> choose (0, arbitraryEpochLength)

instance Arbitrary Word31 where
    arbitrary = arbitrarySizedBoundedIntegral
    shrink = shrinkIntegral

instance Arbitrary (Quantity "lovelace" Word64) where
    shrink (Quantity q) = [ Quantity q' | q' <- shrink q ]
    arbitrary = Quantity <$> arbitrary

arbitraryEpochLength :: Word32
arbitraryEpochLength = 100

arbitraryChainLength :: Word32
arbitraryChainLength = 10

-- NOTE Expected to have a high entropy
instance Arbitrary PoolId where
    arbitrary = do
        bytes <- vector 32
        return $ PoolId $ B8.pack bytes

-- NOTE Excepted to have a reasonnably small entropy
instance Arbitrary PoolOwner where
    arbitrary = do
        byte <- elements ['0'..'8']
        return $ PoolOwner $ B8.pack (replicate 32 byte)

instance Arbitrary PoolRegistrationCertificate where
    shrink (PoolRegistrationCertificate p xs m c pl md) =
        (\xs' -> PoolRegistrationCertificate p xs' m c pl md)
            <$> shrinkList (const []) xs
    arbitrary = PoolRegistrationCertificate
        <$> arbitrary
        <*> scale (`mod` 8) (listOf arbitrary)
        <*> genPercentage
        <*> fmap Quantity arbitrary
        <*> fmap Quantity arbitrary
        <*> oneof [pure Nothing, Just <$> genMetadata]
      where
        genMetadata = (,)
            <$> fmap StakePoolMetadataUrl genURL
            <*> arbitrary
        genURL  = do
            protocol <- elements [ "http", "https" ]
            fstP <- elements [ "cardano", "ada", "pool", "staking", "reward" ]
            sndP <- elements [ "rocks", "moon", "digital", "server", "fast" ]
            extP <- elements [ ".io", ".dev", ".com", ".eu" ]
            pure $ protocol <> "://" <> fstP <> "-" <> sndP <> extP

instance Arbitrary PoolRetirementCertificate where
    arbitrary = PoolRetirementCertificate
        <$> arbitrary
        <*> arbitrary
    shrink = genericShrink

instance Arbitrary PoolCertificate where
    arbitrary = oneof
        [ Registration
            <$> arbitrary
        , Retirement
            <$> arbitrary
        ]
    shrink = const []

instance Arbitrary StakePoolMetadataHash where
    arbitrary = fmap (StakePoolMetadataHash . BS.pack) (vector 32)

genStakePoolMetadata
    :: StakePoolMetadataUrl
    -> Gen StakePoolMetadata
genStakePoolMetadata (StakePoolMetadataUrl url) = StakePoolMetadata
    <$> genStakePoolTicker
    <*> genPrintableText
    <*> oneof [ pure Nothing, Just <$> genPrintableText ]
    <*> pure url

genStakePoolTicker :: Gen StakePoolTicker
genStakePoolTicker = (StakePoolTicker . T.pack) <$>
    (choose (3,5) >>= \n -> vectorOf n (elements ['A','B'..'Z']))

instance Arbitrary StakePoolsFixture where
    arbitrary = do
        poolsNumber <- choose (1, 100)
        pools <- vector poolsNumber
        slotsNumber <- choose (0, 200)
        firstSlot <- arbitrary
        slotsGenerated <-
            foldM (appendPair pools) [] (generateNextSlots [firstSlot] slotsNumber)
        rNum <- choose (1, slotsNumber + 1)
        rSlots <-
            (L.sortOn Down . take rNum) <$> shuffle (map snd slotsGenerated)
        pure $ StakePoolsFixture (second mkBlockHeader <$> slotsGenerated) rSlots
      where
        mkBlockHeader :: SlotId -> BlockHeader
        mkBlockHeader s = BlockHeader
            { slotId = s
            , blockHeight = Quantity 0
            , headerHash = Hash "00000000000000000000000000000001"
            , parentHeaderHash = Hash "00000000000000000000000000000000"
            }

        epochLength :: EpochLength
        epochLength = dummyGenesisParameters ^. #getEpochLength

        sp :: SlotParameters
        sp = SlotParameters
            epochLength
            (dummyGenesisParameters ^. #getSlotLength)
            (dummyGenesisParameters ^. #getGenesisBlockDate)
            (dummyGenesisParameters ^. #getActiveSlotCoefficient)

        generateNextSlots :: [SlotId] -> Int -> [SlotId]
        generateNextSlots slots@(s:_) num =
            if (num < 1) then
                reverse slots
            else
                generateNextSlots ((slotSucc sp s):slots) (num - 1)
        generateNextSlots [] _ = []

        appendPair
            :: [PoolId]
            -> [(PoolId, SlotId)]
            -> SlotId
            -> Gen [(PoolId, SlotId)]
        appendPair pools pairs slot = do
            pool <- elements pools
            return $ (pool,slot):pairs
