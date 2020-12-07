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
    ()
import Cardano.Wallet.Gen
    ( genSlotNo, shrinkSlotNo )
import Cardano.Wallet.Primitive.Types
    ( EpochNo (..), SlotInEpoch (..), SlotNo )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity (..) )
import Cardano.Wallet.Primitive.Types.TokenQuantity.Gen
    ( genTokenQuantityMixed, shrinkTokenQuantityMixed )
import Data.Proxy
    ( Proxy (..) )
import Data.Time.Clock.POSIX
    ( POSIXTime )
import Data.Typeable
    ( Typeable, typeRep )
import Data.Word.Odd
    ( Word31 )
import Database.Persist.Class
    ( PersistField (..) )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Arbitrary (..)
    , NonNegative (..)
    , Property
    , arbitrarySizedBoundedIntegral
    , checkCoverage
    , cover
    , property
    , shrinkIntegral
    , (===)
    )

spec :: Spec
spec = do
    describe "Values can be persisted and unpersisted successfully" $ do
        persistRoundtrip $ Proxy @SlotNo
        persistRoundtrip $ Proxy @POSIXTime
        persistRoundtrip $ Proxy @TokenQuantity

    describe "Coverage checks for generators" $ do
        it "TokenQuantity" $
            property $ prop_checkTokenQuantityCoverage

-- | Constructs a test to check that roundtrip persistence and unpersistence is
--   possible for values of the given type.
persistRoundtrip
    :: forall a. (Arbitrary a, Eq a, Show a, PersistField a, Typeable a)
    => Proxy a
    -> Spec
persistRoundtrip proxy = it
    ("can persist and unpersist values of type '"
        <> show (typeRep proxy)
        <> "'")
    (property $ \a ->
        fromPersistValue (toPersistValue @a a) === Right a)

prop_checkTokenQuantityCoverage :: TokenQuantity -> Property
prop_checkTokenQuantityCoverage q = checkCoverage
    $ cover 2 (q == TokenQuantity 0)
        "token quantity is zero"
    $ cover 2 (TokenQuantity 0 < q && q < TokenQuantity smallPositiveValue)
        "token quantity is small and positive"
    $ cover 2 (TokenQuantity 0 > q && q > TokenQuantity smallNegativeValue)
        "token quantity is small and negative"
    $ cover 2 (q > TokenQuantity largePositiveValue)
        "token quantity is very large and positive"
    $ cover 2 (q < TokenQuantity largeNegativeValue)
        "token quantity is very large and negative"
    $ True
  where
    smallPositiveValue = 10
    smallNegativeValue = negate smallPositiveValue
    largePositiveValue = (10 :: Integer) ^ (100 :: Integer)
    largeNegativeValue = negate largePositiveValue

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
    arbitrary = genTokenQuantityMixed
    shrink = shrinkTokenQuantityMixed
