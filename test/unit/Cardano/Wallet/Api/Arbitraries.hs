{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Api.Arbitraries () where

import Cardano.Wallet.Api.Types.Amount
import Cardano.Wallet.Api.Types.CurrencyUnit
import Cardano.Wallet.Api.Types.Percentage
import Cardano.Wallet.Api.Types.Wallet
import Cardano.Wallet.Api.Types.WalletAddressPoolGap
import Cardano.Wallet.Api.Types.WalletBalance
import Cardano.Wallet.Api.Types.WalletDelegation
import Cardano.Wallet.Api.Types.WalletDelegationStatus
import Cardano.Wallet.Api.Types.WalletId
import Cardano.Wallet.Api.Types.WalletName
import Cardano.Wallet.Api.Types.WalletPassphraseInfo
import Cardano.Wallet.Api.Types.WalletState
import Cardano.Wallet.Api.Types.WalletStateStatus

import Prelude

import Control.Monad
    ( replicateM )
import Data.Either
    ( fromRight )
import Data.Word
    ( Word32, Word8 )
import Test.QuickCheck
    ( Arbitrary (..)
    , arbitraryBoundedEnum
    , arbitrarySizedBoundedIntegral
    , choose
    )
import Test.QuickCheck.Arbitrary.Generic
    ( genericArbitrary, genericShrink )
import Test.QuickCheck.Instances.Time
    ()

import qualified Data.Text as T
import qualified Data.UUID.Types as UUID

instance Arbitrary Wallet where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary Amount where
    arbitrary = do
        (value :: Word8) <- arbitrarySizedBoundedIntegral
        pure $ Amount (fromIntegral value) Lovelace

instance Arbitrary CurrencyUnit where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary Percentage where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary WalletAddressPoolGap where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary WalletBalance where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary WalletDelegation where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary WalletDelegationStatus where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary WalletId where
    arbitrary = WalletId . uuidFromWords <$> arbitrary
    shrink (WalletId u) = WalletId . uuidFromWords <$> shrink (UUID.toWords u)

uuidFromWords :: (Word32, Word32, Word32, Word32) -> UUID.UUID
uuidFromWords (a, b, c, d) = UUID.fromWords a b c d

instance Arbitrary WalletName where
    arbitrary = do
        nameLength <- choose (walletNameMinLength, walletNameMaxLength)
        fromRight (error "Unable to create arbitrary WalletName")
            . mkWalletName
            . T.pack <$> replicateM nameLength (choose ('a', 'z'))

instance Arbitrary WalletPassphraseInfo where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary WalletState where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary WalletStateStatus where
    arbitrary = genericArbitrary
    shrink = genericShrink
