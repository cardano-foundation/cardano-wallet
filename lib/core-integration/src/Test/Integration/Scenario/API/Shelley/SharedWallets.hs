{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Integration.Scenario.API.Shelley.SharedWallets
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Api.Types
    ( ApiSharedWallet (..)
    , DecodeAddress
    , DecodeStakeAddress
    , EncodeAddress (..)
    )
import Cardano.Wallet.Primitive.AddressDerivation
    ( DerivationIndex (..), PaymentAddress )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( ShelleyKey )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Monad.Trans.Resource
    ( runResourceT )
import Data.Quantity
    ( Quantity (..) )
import Test.Hspec
    ( SpecWith, describe, shouldBe, shouldNotBe )
import Test.Hspec.Extra
    ( it )
import Test.Integration.Framework.DSL
    ( Context (..)
    , Headers (..)
    , MnemonicLength (..)
    , Payload (..)
    , expectField
    , expectResponseCode
    , fixturePassphrase
    , genMnemonics
    , json
    , notDelegating
    , postSharedWallet
    , verify
    )

import qualified Network.HTTP.Types as HTTP

spec :: forall n.
    ( DecodeAddress n
    , DecodeStakeAddress n
    , EncodeAddress n
    , PaymentAddress n ShelleyKey
    ) => SpecWith Context
spec = describe "SHARED_WALLETS" $ do
    it "SHARED_WALLETS_CREATE_01 - Create an active shared wallet from root xprv" $ \ctx -> runResourceT $ do
        m15 <- liftIO $ genMnemonics M15
        m12 <- liftIO $ genMnemonics M12
        let payload = Json [json| {
                "name": "Shared Wallet",
                "mnemonic_sentence": #{m15},
                "mnemonic_second_factor": #{m12},
                "passphrase": #{fixturePassphrase},
                "account_index": "30H",
                "payment_script_template":
                    { "cosigners":
                        { "cosigner#0": "1423856bc91c49e928f6f30f4e8d665d53eb4ab6028bd0ac971809d514c92db11423856bc91c49e928f6f30f4e8d665d53eb4ab6028bd0ac971809d514c92db1" },
                      "template":
                          { "all":
                             [ "cosigner#0",
                               { "active_from": 120 }
                             ]
                          }
                    }
                } |]
        r <- postSharedWallet ctx Default payload
        verify (fst r, (\(Right (ApiSharedWallet (Right res))) -> Right res) $ snd r)
            [ expectResponseCode HTTP.status201
            , expectField
                    (#name . #getApiT . #getWalletName) (`shouldBe` "Shared Wallet")
            , expectField
                    (#addressPoolGap . #getApiT . #getAddressPoolGap) (`shouldBe` 20)
            , expectField (#balance . #available) (`shouldBe` Quantity 0)
            , expectField (#balance . #total) (`shouldBe` Quantity 0)
            , expectField (#balance . #reward) (`shouldBe` Quantity 0)
            , expectField (#assets . #total) (`shouldBe` mempty)
            , expectField (#assets . #available) (`shouldBe` mempty)
            , expectField #delegation (`shouldBe` notDelegating [])
            , expectField #passphrase (`shouldNotBe` Nothing)
            , expectField #delegationScriptTemplate (`shouldBe` Nothing)
            , expectField (#accountIndex . #getApiT) (`shouldBe` DerivationIndex 2147483678)
            ]

    it "SHARED_WALLETS_CREATE_02 - Create a pending shared wallet from root xprv" $ \ctx -> runResourceT $ do
        m15 <- liftIO $ genMnemonics M15
        m12 <- liftIO $ genMnemonics M12
        let payload = Json [json| {
                "name": "Shared Wallet",
                "mnemonic_sentence": #{m15},
                "mnemonic_second_factor": #{m12},
                "passphrase": #{fixturePassphrase},
                "account_index": "30H",
                "payment_script_template":
                    { "cosigners":
                        { "cosigner#0": "1423856bc91c49e928f6f30f4e8d665d53eb4ab6028bd0ac971809d514c92db11423856bc91c49e928f6f30f4e8d665d53eb4ab6028bd0ac971809d514c92db1" },
                      "template":
                          { "all":
                             [ "cosigner#0",
                               "cosigner#1",
                               { "active_from": 120 }
                             ]
                          }
                    }
                } |]
        r <- postSharedWallet ctx Default payload
        verify (fst r, (\(Right (ApiSharedWallet (Left res))) -> Right res) $ snd r)
            [ expectResponseCode HTTP.status201
            , expectField
                    (#name . #getApiT . #getWalletName) (`shouldBe` "Shared Wallet")
            , expectField
                    (#addressPoolGap . #getApiT . #getAddressPoolGap) (`shouldBe` 20)
            , expectField #delegationScriptTemplate (`shouldBe` Nothing)
            , expectField (#accountIndex . #getApiT) (`shouldBe` DerivationIndex 2147483678)
            ]

    it "SHARED_WALLETS_CREATE_03 - Create an active shared wallet from account xpub" $ \ctx -> runResourceT $ do
        let payload = Json [json| {
                "name": "Shared Wallet",
                "account_public_key": "1423856bc91c49e928f6f30f4e8d665d53eb4ab6028bd0ac971809d514c92db11423856bc91c49e928f6f30f4e8d665d53eb4ab6028bd0ac971809d514c92db1",
                "account_index": "30H",
                "payment_script_template":
                    { "cosigners":
                        { "cosigner#0": "1423856bc91c49e928f6f30f4e8d665d53eb4ab6028bd0ac971809d514c92db11423856bc91c49e928f6f30f4e8d665d53eb4ab6028bd0ac971809d514c92db1" },
                      "template":
                          { "all":
                             [ "cosigner#0",
                               { "active_from": 120 }
                             ]
                          }
                    }
                } |]
        r <- postSharedWallet ctx Default payload
        verify (fst r, (\(Right (ApiSharedWallet (Right res))) -> Right res) $ snd r)
            [ expectResponseCode HTTP.status201
            , expectField
                    (#name . #getApiT . #getWalletName) (`shouldBe` "Shared Wallet")
            , expectField
                    (#addressPoolGap . #getApiT . #getAddressPoolGap) (`shouldBe` 20)
            , expectField (#balance . #available) (`shouldBe` Quantity 0)
            , expectField (#balance . #total) (`shouldBe` Quantity 0)
            , expectField (#balance . #reward) (`shouldBe` Quantity 0)
            , expectField (#assets . #total) (`shouldBe` mempty)
            , expectField (#assets . #available) (`shouldBe` mempty)
            , expectField #delegation (`shouldBe` notDelegating [])
            , expectField #passphrase (`shouldBe` Nothing)
            , expectField #delegationScriptTemplate (`shouldBe` Nothing)
            , expectField (#accountIndex . #getApiT) (`shouldBe` DerivationIndex 2147483678)
            ]

    it "SHARED_WALLETS_CREATE_04 - Create a pending shared wallet from account xpub" $ \ctx -> runResourceT $ do
        let payload = Json [json| {
                "name": "Shared Wallet",
                "account_public_key": "1423856bc91c49e928f6f30f4e8d665d53eb4ab6028bd0ac971809d514c92db11423856bc91c49e928f6f30f4e8d665d53eb4ab6028bd0ac971809d514c92db2",
                "account_index": "30H",
                "payment_script_template":
                    { "cosigners":
                        { "cosigner#0": "1423856bc91c49e928f6f30f4e8d665d53eb4ab6028bd0ac971809d514c92db11423856bc91c49e928f6f30f4e8d665d53eb4ab6028bd0ac971809d514c92db1" },
                      "template":
                          { "all":
                             [ "cosigner#0",
                               "cosigner#1",
                               { "active_from": 120 }
                             ]
                          }
                    }
                } |]
        r <- postSharedWallet ctx Default payload
        verify (fst r, (\(Right (ApiSharedWallet (Left res))) -> Right res) $ snd r)
            [ expectResponseCode HTTP.status201
            , expectField
                    (#name . #getApiT . #getWalletName) (`shouldBe` "Shared Wallet")
            , expectField
                    (#addressPoolGap . #getApiT . #getAddressPoolGap) (`shouldBe` 20)
            , expectField #delegationScriptTemplate (`shouldBe` Nothing)
            , expectField (#accountIndex . #getApiT) (`shouldBe` DerivationIndex 2147483678)
            ]
