{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.Api.JsonSpec (exampleWallet, spec) where

import Prelude

import Cardano.Wallet.Api.Arbitraries
    ()
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

import Data.Aeson
    ( FromJSON, ToJSON, decode, eitherDecode, encode )
import Data.ByteString.Lazy
    ( ByteString )
import Data.Either
    ( isRight )
import Test.Hspec
    ( Expectation, Spec, describe, it, shouldBe, shouldSatisfy )
import Test.QuickCheck
    ( property )
import Text.RawString.QQ
    ( r )

spec :: Spec
spec = do
    describe "can perform basic JSON deserialization" $
        it "Wallet" $
            (eitherDecode exampleWallet :: Either String Wallet)
                `shouldSatisfy` isRight
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

exampleWallet :: ByteString
exampleWallet = [r|
    { "id" : "00000000-0000-0000-0000-000000000000"
    , "name" : "example wallet"
    , "addressPoolGap" : 50
    , "delegation" : { "status" : "not_delegating" }
    , "passphrase" : { "lastUpdatedAt" : "1864-05-02T22:19:08.077666613986Z" }
    , "state" :
        { "status" : "restoring"
        , "progress" :  { "quantity" : 100, "unit" : "percent" } }
    , "balance" :
        { "total"     : { "quantity" : 100, "unit" : "lovelace" }
        , "available" : { "quantity" : 100, "unit" : "lovelace" } } } |]
