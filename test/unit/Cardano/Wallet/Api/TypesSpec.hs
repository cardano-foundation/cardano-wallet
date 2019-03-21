{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Api.TypesSpec (spec) where

import Prelude

import Cardano.Wallet
    ( mkWalletName, walletNameMaxLength, walletNameMinLength )
import Cardano.Wallet.Api.Types
    ( AddressPoolGap
    , ApiT (..)
    , PoolId (..)
    , Wallet (..)
    , WalletBalance (..)
    , WalletDelegation (..)
    , WalletId (..)
    , WalletName (..)
    , WalletPassphraseInfo (..)
    , WalletState (..)
    )
import Control.Monad
    ( replicateM )
import Data.Aeson
    ( FromJSON, ToJSON )
import Data.Either
    ( rights )
import Data.Quantity
    ( Percentage, Quantity (..) )
import Data.Typeable
    ( Typeable )
import Data.Word
    ( Word32, Word8 )
import Numeric.Natural
    ( Natural )
import Test.Aeson.GenericSpecs
    ( GoldenDirectoryOption (CustomDirectoryName)
    , Proxy (Proxy)
    , Settings
    , defaultSettings
    , goldenDirectoryOption
    , roundtripAndGoldenSpecsWithSettings
    , sampleSize
    , useModuleNameAsSubDirectory
    )
import Test.Hspec
    ( Spec, describe )
import Test.QuickCheck
    ( Arbitrary (..), arbitraryBoundedEnum, arbitraryPrintableChar, choose )
import Test.QuickCheck.Arbitrary.Generic
    ( genericArbitrary, genericShrink )
import Test.QuickCheck.Instances.Time
    ()

import qualified Data.Text as T
import qualified Data.UUID.Types as UUID

spec :: Spec
spec = do
    describe
        ("can perform roundtrip JSON serialization & deserialization, " <>
         "and match existing golden files") $ do
            roundtripAndGolden $ Proxy @ Wallet
            roundtripAndGolden $ Proxy @ (ApiT AddressPoolGap)
            roundtripAndGolden $ Proxy @ (ApiT WalletBalance)
            roundtripAndGolden $ Proxy @ (ApiT (WalletDelegation (ApiT PoolId)))
            roundtripAndGolden $ Proxy @ (ApiT WalletId)
            roundtripAndGolden $ Proxy @ (ApiT WalletName)
            roundtripAndGolden $ Proxy @ (ApiT WalletBalance)
            roundtripAndGolden $ Proxy @ (ApiT WalletPassphraseInfo)
            roundtripAndGolden $ Proxy @ (ApiT WalletState)

-- | Run JSON roundtrip & golden tests
--
-- Golden tests files are generated automatically on first run. On later runs
-- we check that the format stays the same. The golden files should be tracked
-- in git.
--
-- Example:
-- >>> roundtripAndGolden $ Proxy @ Wallet
--
-- ...will compare @ToJSON@ of @Wallet@ against `Wallet.json`. It may either
-- match and succeed, or fail and write `Wallet.faulty.json` to disk with the
-- new format. Faulty golden files should /not/ be commited.
--
-- The directory `test/data/Cardano/Wallet/Api` is used.
roundtripAndGolden
    :: forall a. (Arbitrary a, ToJSON a, FromJSON a, Typeable a)
    => Proxy a
    -> Spec
roundtripAndGolden = roundtripAndGoldenSpecsWithSettings settings
  where
    settings :: Settings
    settings = defaultSettings
        { goldenDirectoryOption =
            CustomDirectoryName "test/data/Cardano/Wallet/Api"
        , useModuleNameAsSubDirectory =
            False
        , sampleSize = 4
        }

{-------------------------------------------------------------------------------
                              Arbitrary Instances
-------------------------------------------------------------------------------}

instance Arbitrary (Quantity "lovelace" Natural) where
    shrink (Quantity 0) = []
    shrink _ = [Quantity 0]
    arbitrary = Quantity . fromIntegral <$> (arbitrary @Word8)

instance Arbitrary (Quantity "percent" Percentage) where
    arbitrary = Quantity <$> arbitraryBoundedEnum

instance Arbitrary Wallet where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary AddressPoolGap where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary WalletBalance where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary (WalletDelegation (ApiT PoolId)) where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary PoolId where
    arbitrary = PoolId . T.pack <$> replicateM 3 arbitraryPrintableChar

instance Arbitrary WalletId where
    arbitrary = WalletId . uuidFromWords <$> arbitrary

uuidFromWords :: (Word32, Word32, Word32, Word32) -> UUID.UUID
uuidFromWords (a, b, c, d) = UUID.fromWords a b c d

instance Arbitrary WalletName where
    arbitrary = do
        nameLength <- choose (walletNameMinLength, walletNameMaxLength)
        either (error "Unable to create arbitrary WalletName") id
            . mkWalletName
            . T.pack <$> replicateM nameLength arbitraryPrintableChar
    shrink =
        rights
            . fmap (mkWalletName . T.pack)
            . shrink
            . T.unpack
            . getWalletName

instance Arbitrary WalletPassphraseInfo where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary WalletState where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary a => Arbitrary (ApiT a) where
    arbitrary = ApiT <$> arbitrary
    shrink = fmap ApiT . shrink . getApiT
