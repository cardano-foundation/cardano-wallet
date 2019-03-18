{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.Api.V2.JsonSpec (spec) where

import Prelude

import Cardano.Wallet.Api.V2.Arbitraries
    ()
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

import Data.Aeson
    ( FromJSON, ToJSON, decode, encode )
import Test.Hspec
    ( Expectation, Spec, describe, it, shouldBe )
import Test.QuickCheck
    ( property )

spec :: Spec
spec =
    describe "can perform roundtrip JSON serialization & deserialization" $ do
        it "Amount" $
            property $ \a -> canRoundTrip (a :: Amount)
        it "CurrencyUnit" $
            property $ \a -> canRoundTrip (a :: CurrencyUnit)
        it "Percentage" $
            property $ \a -> canRoundTrip (a :: Percentage)
        it "Wallet" $
            property $ \a -> canRoundTrip (a :: Wallet)
        it "WalletAddressPoolGap" $
            property $ \a -> canRoundTrip (a :: WalletAddressPoolGap)
        it "WalletBalance" $
            property $ \a -> canRoundTrip (a :: WalletBalance)
        it "WalletDelegation" $
            property $ \a -> canRoundTrip (a :: WalletDelegation)
        it "WalletDelegationStatus" $
            property $ \a -> canRoundTrip (a :: WalletDelegationStatus)
        it "WalletId" $
            property $ \a -> canRoundTrip (a :: WalletId)
        it "WalletName" $
            property $ \a -> canRoundTrip (a :: WalletName)
        it "WalletPassphraseInfo" $
            property $ \a -> canRoundTrip (a :: WalletPassphraseInfo)
        it "WalletState" $
            property $ \a -> canRoundTrip (a :: WalletState)
        it "WalletStateStatus" $
            property $ \a -> canRoundTrip (a :: WalletStateStatus)

canRoundTrip :: Eq a => FromJSON a => ToJSON a => Show a => a -> Expectation
canRoundTrip a = decode (encode a) `shouldBe` Just a
