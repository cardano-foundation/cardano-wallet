{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Api.V2.Arbitraries where

import Cardano.Wallet.Api.V2.Types.Amount
import Cardano.Wallet.Api.V2.Types.CurrencyUnit
import Cardano.Wallet.Api.V2.Types.Percentage
import Cardano.Wallet.Api.V2.Types.Wallet
import Cardano.Wallet.Api.V2.Types.WalletAddressPoolGap
import Cardano.Wallet.Api.V2.Types.WalletBalance
import Cardano.Wallet.Api.V2.Types.WalletDelegation
import Cardano.Wallet.Api.V2.Types.WalletDelegationStatus
import Cardano.Wallet.Api.V2.Types.WalletId
import Cardano.Wallet.Api.V2.Types.WalletName
import Cardano.Wallet.Api.V2.Types.WalletPassphraseInfo
import Cardano.Wallet.Api.V2.Types.WalletState
import Cardano.Wallet.Api.V2.Types.WalletStateStatus

import Control.Monad
    ( replicateM )
import Data.Either
    ( fromRight )
import Data.Word
    ( Word32, Word8 )
import Prelude
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

import qualified Data.Text as Text
import qualified Data.UUID.Types as UUID

instance Arbitrary Wallet where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary Amount where
    arbitrary = do
        (value :: Word8) <- arbitrarySizedBoundedIntegral
        pure $ Amount (fromIntegral value) Lovelace

instance Arbitrary CurrencyUnit where
    arbitrary = pure Lovelace

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

instance Arbitrary WalletName where
    arbitrary = do
        let minLength = walletNameMinLength
        let maxLength = minLength + ((walletNameMaxLength - minLength) `div` 16)
        n <- choose (minLength, maxLength)
        fromRight (error "Unable to create arbitrary WalletName")
            . mkWalletName
            . Text.pack <$> replicateM n (choose ('a', 'z'))

instance Arbitrary WalletPassphraseInfo where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary WalletState where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary WalletStateStatus where
    arbitrary = genericArbitrary
    shrink = genericShrink

uuidFromWords :: (Word32, Word32, Word32, Word32) -> UUID.UUID
uuidFromWords (a,b,c,d) = UUID.fromWords a b c d



