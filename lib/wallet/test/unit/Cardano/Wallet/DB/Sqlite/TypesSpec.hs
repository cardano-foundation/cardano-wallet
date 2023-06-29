{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.DB.Sqlite.TypesSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.DB.Sqlite.Types
    ( stdGenFromString
    )
import Cardano.Wallet.Gen
    ( genNestedTxMetadata
    , genSimpleTxMetadata
    , genSlotNo
    , shrinkSlotNo
    , shrinkTxMetadata
    )
import Cardano.Wallet.Primitive.Types
    ( EpochNo (..)
    , SlotInEpoch (..)
    , SlotNo
    )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity (..)
    )
import Cardano.Wallet.Primitive.Types.TokenQuantity.Gen
    ( genTokenQuantityFullRange
    , shrinkTokenQuantityFullRange
    )
import Cardano.Wallet.Primitive.Types.Tx
    ( TxMetadata
    , TxScriptValidity
    )
import Cardano.Wallet.Primitive.Types.Tx.Gen
    ( genTxScriptValidity
    , shrinkTxScriptValidity
    )
import Data.Either
    ( isLeft
    )
import Data.Proxy
    ( Proxy (..)
    )
import Data.Time.Clock.POSIX
    ( POSIXTime
    )
import Data.Typeable
    ( Typeable
    , typeRep
    )
import Data.Word
    ( Word64
    )
import Data.Word.Odd
    ( Word31
    )
import Database.Persist.Class
    ( PersistField (..)
    )
import System.Random
    ( StdGen
    , mkStdGen
    )
import System.Random.Internal
    ( StdGen (..)
    )
import System.Random.SplitMix
    ( seedSMGen
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    , shouldSatisfy
    )
import Test.QuickCheck
    ( Arbitrary (..)
    , NonNegative (..)
    , Property
    , arbitrarySizedBoundedIntegral
    , checkCoverage
    , cover
    , property
    , shrinkIntegral
    , shrinkMapBy
    , (===)
    )

spec :: Spec
spec = do
    describe "Values can be persisted and unpersisted successfully" $ do
        persistRoundtrip $ Proxy @SlotNo
        persistRoundtrip $ Proxy @POSIXTime
        persistRoundtrip $ Proxy @TokenQuantity
        persistRoundtrip $ Proxy @StdGen
        persistRoundtrip $ Proxy @(Nested TxMetadata)
        persistRoundtrip $ Proxy @(Simple TxMetadata)
        persistRoundtrip $ Proxy @TxScriptValidity

    describe "Backwards compatible instance PersistField StdGen" $ do
        it "rnd_state empty"
            $ stdGenFromString "1233322640 1"
            `shouldBe` Right
                (StdGen $ seedSMGen 1233322640 1)
        it "rnd_state"
            $ stdGenFromString "444941957 1872071452"
            `shouldBe` Right
                (StdGen $ seedSMGen 444941957 1872071452)
        it "malformed"
            $ stdGenFromString "1233322640"
            `shouldSatisfy` isLeft

    describe "Coverage checks for generators" $ do
        it "TokenQuantity"
            $ property prop_checkTokenQuantityCoverage

-- | Constructs a test to check that roundtrip persistence and unpersistence is
--   possible for values of the given type.
persistRoundtrip
    :: forall a
     . (Arbitrary a, Eq a, Show a, PersistField a, Typeable a)
    => Proxy a
    -> Spec
persistRoundtrip proxy =
    it
        ( "can persist and unpersist values of type '"
            <> show (typeRep proxy)
            <> "'"
        )
        ( property $ \a ->
            fromPersistValue (toPersistValue @a a) === Right a
        )

prop_checkTokenQuantityCoverage :: TokenQuantity -> Property
prop_checkTokenQuantityCoverage q =
    checkCoverage
        $ cover
            2
            (q == minTokenQuantity)
            "token quantity is smallest allowable"
        $ cover
            2
            (q == maxTokenQuantity)
            "token quantity is greatest allowable"
        $ cover
            2
            (q > minTokenQuantity && q < maxTokenQuantity)
            "token quantity is between smallest and greatest"
            True
  where
    minTokenQuantity :: TokenQuantity
    minTokenQuantity = TokenQuantity 0
    maxTokenQuantity :: TokenQuantity
    maxTokenQuantity = TokenQuantity $ fromIntegral $ maxBound @Word64

{-------------------------------------------------------------------------------
                              Arbitrary Instances
-------------------------------------------------------------------------------}

instance Arbitrary POSIXTime where
    arbitrary = do
        NonNegative int <- arbitrary @(NonNegative Int)
        pure (fromIntegral int)

instance Arbitrary SlotNo where
    arbitrary = genSlotNo
    shrink = shrinkSlotNo

instance Arbitrary EpochNo where
    arbitrary = EpochNo <$> arbitrary
    shrink (EpochNo n) = EpochNo <$> shrink n

instance Arbitrary SlotInEpoch where
    arbitrary = SlotInEpoch <$> arbitrary
    shrink (SlotInEpoch n) = SlotInEpoch <$> shrink n

instance Arbitrary Word31 where
    arbitrary = arbitrarySizedBoundedIntegral
    shrink = shrinkIntegral

instance Arbitrary TokenQuantity where
    arbitrary = genTokenQuantityFullRange
    shrink = shrinkTokenQuantityFullRange

newtype Nested a = Nested {unNested :: a} deriving (Eq, Show)
newtype Simple a = Simple {unSimple :: a} deriving (Eq, Show)

instance Arbitrary (Nested TxMetadata) where
    arbitrary = Nested <$> genNestedTxMetadata
    shrink = shrinkMapBy Nested unNested shrinkTxMetadata

instance Arbitrary (Simple TxMetadata) where
    arbitrary = Simple <$> genSimpleTxMetadata
    shrink = shrinkMapBy Simple unSimple shrinkTxMetadata

instance (PersistField a) => PersistField (Nested a) where
    toPersistValue = toPersistValue . unNested
    fromPersistValue = fmap Nested . fromPersistValue

instance (PersistField a) => PersistField (Simple a) where
    toPersistValue = toPersistValue . unSimple
    fromPersistValue = fmap Simple . fromPersistValue

instance Arbitrary StdGen where
    arbitrary = mkStdGen <$> arbitrary

instance Arbitrary TxScriptValidity where
    arbitrary = genTxScriptValidity
    shrink = shrinkTxScriptValidity
