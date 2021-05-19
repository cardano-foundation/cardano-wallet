{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Integration.Scenario.API.Shared.Wallets
    ( spec
    ) where

import Prelude

import Cardano.Address.Script
    ( Cosigner (..), ScriptTemplate (..) )
import Cardano.Mnemonic
    ( MkSomeMnemonic (..) )
import Cardano.Wallet.Api.Types
    ( ApiAccountKeyShared (..)
    , ApiSharedWallet (..)
    , DecodeAddress
    , DecodeStakeAddress
    , EncodeAddress (..)
    , KeyFormat (..)
    )
import Cardano.Wallet.Primitive.AddressDerivation
    ( DerivationIndex (..), Passphrase (..), Role (..), hex )
import Cardano.Wallet.Primitive.AddressDiscovery.Shared
    ( CredentialType (..) )
import Cardano.Wallet.Primitive.SyncProgress
    ( SyncProgress (..) )
import Control.Monad
    ( forM )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Monad.Trans.Resource
    ( runResourceT )
import Data.Aeson
    ( ToJSON (..), Value (String) )
import Data.Bifunctor
    ( second )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( ToText (..) )
import Test.Hspec
    ( SpecWith, describe, shouldBe, shouldNotBe )
import Test.Hspec.Extra
    ( it )
import Test.Integration.Framework.DSL
    ( Context (..)
    , Headers (..)
    , MnemonicLength (..)
    , Payload (..)
    , accPubKeyFromMnemonics
    , deleteSharedWallet
    , eventually
    , expectErrorMessage
    , expectField
    , expectListField
    , expectListSize
    , expectResponseCode
    , fixturePassphrase
    , genMnemonics
    , genXPubs
    , getFromResponse
    , getSharedWallet
    , getSharedWalletKey
    , getWalletIdFromSharedWallet
    , json
    , listFilteredSharedWallets
    , notDelegating
    , patchSharedWallet
    , postAccountKeyShared
    , postSharedWallet
    , verify
    , walletId
    )
import Test.Integration.Framework.TestData
    ( errMsg403CannotUpdateThisCosigner
    , errMsg403CreateIllegal
    , errMsg403KeyAlreadyPresent
    , errMsg403NoDelegationTemplate
    , errMsg403NoSuchCosigner
    , errMsg403WalletAlreadyActive
    )

import qualified Data.ByteArray as BA
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Network.HTTP.Types as HTTP
import qualified Test.Hspec.Expectations.Lifted as Expectations

spec :: forall n.
    ( DecodeAddress n
    , DecodeStakeAddress n
    , EncodeAddress n
    ) => SpecWith Context
spec = describe "SHARED_WALLETS" $ do
    it "SHARED_WALLETS_CREATE_01 - Create an active shared wallet from root xprv" $ \ctx -> runResourceT $ do
        m15txt <- liftIO $ genMnemonics M15
        m12txt <- liftIO $ genMnemonics M12
        let (Right m15) = mkSomeMnemonic @'[ 15 ] m15txt
        let (Right m12) = mkSomeMnemonic @'[ 12 ] m12txt
        let passphrase = Passphrase $ BA.convert $ T.encodeUtf8 fixturePassphrase
        let index = 30
        let accXPubDerived = accPubKeyFromMnemonics m15 (Just m12) index passphrase
        let payloadPost = Json [json| {
                "name": "Shared Wallet",
                "mnemonic_sentence": #{m15txt},
                "mnemonic_second_factor": #{m12txt},
                "passphrase": #{fixturePassphrase},
                "account_index": "30H",
                "payment_script_template":
                    { "cosigners":
                        { "cosigner#0": #{accXPubDerived} },
                      "template":
                          { "all":
                             [ "cosigner#0",
                               { "active_from": 120 }
                             ]
                          }
                    }
                } |]
        rPost <- postSharedWallet ctx Default payloadPost
        verify (second (\(Right (ApiSharedWallet (Right res))) -> Right res) rPost)
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

        let wal = getFromResponse id rPost

        let payloadKey = Json [json|{
                "passphrase": #{fixturePassphrase},
                "format": "extended"
            }|]
        rKey <-
            postAccountKeyShared ctx wal (DerivationIndex $ 2147483648 + index) Default payloadKey

        verify rKey
            [ expectResponseCode HTTP.status202
            , expectField #format (`shouldBe` Extended)
            ]
        let (ApiAccountKeyShared bytes _) = getFromResponse id rKey
        T.decodeUtf8 (hex bytes) `Expectations.shouldBe` accXPubDerived

    it "SHARED_WALLETS_CREATE_02 - Create a pending shared wallet from root xprv" $ \ctx -> runResourceT $ do
        m15txt <- liftIO $ genMnemonics M15
        m12txt <- liftIO $ genMnemonics M12
        let (Right m15) = mkSomeMnemonic @'[ 15 ] m15txt
        let (Right m12) = mkSomeMnemonic @'[ 12 ] m12txt
        let passphrase = Passphrase $ BA.convert $ T.encodeUtf8 fixturePassphrase
        let index = 30
        let accXPubDerived = accPubKeyFromMnemonics m15 (Just m12) index passphrase
        let payload = Json [json| {
                "name": "Shared Wallet",
                "mnemonic_sentence": #{m15txt},
                "mnemonic_second_factor": #{m12txt},
                "passphrase": #{fixturePassphrase},
                "account_index": "30H",
                "payment_script_template":
                    { "cosigners":
                        { "cosigner#0": #{accXPubDerived} },
                      "template":
                          { "all":
                             [ "cosigner#0",
                               "cosigner#1",
                               { "active_from": 120 }
                             ]
                          }
                    }
                } |]
        rPost <- postSharedWallet ctx Default payload
        verify (second (\(Right (ApiSharedWallet (Left res))) -> Right res) rPost)
            [ expectResponseCode HTTP.status201
            , expectField
                    (#name . #getApiT . #getWalletName) (`shouldBe` "Shared Wallet")
            , expectField
                    (#addressPoolGap . #getApiT . #getAddressPoolGap) (`shouldBe` 20)
            , expectField #delegationScriptTemplate (`shouldBe` Nothing)
            , expectField (#accountIndex . #getApiT) (`shouldBe` DerivationIndex 2147483678)
            ]

        let wal = getFromResponse id rPost

        let payloadKey = Json [json|{
                "passphrase": #{fixturePassphrase},
                "format": "extended"
            }|]
        rKey <-
            postAccountKeyShared ctx wal (DerivationIndex $ 2147483648 + index) Default payloadKey

        verify rKey
            [ expectResponseCode HTTP.status202
            , expectField #format (`shouldBe` Extended)
            ]
        let (ApiAccountKeyShared bytes _) = getFromResponse id rKey
        T.decodeUtf8 (hex bytes) `Expectations.shouldBe` accXPubDerived

    it "SHARED_WALLETS_CREATE_03 - Create an active shared wallet from account xpub" $ \ctx -> runResourceT $ do
        (_, accXPubTxt):_ <- liftIO $ genXPubs 1
        let payload = Json [json| {
                "name": "Shared Wallet",
                "account_public_key": #{accXPubTxt},
                "account_index": "10H",
                "payment_script_template":
                    { "cosigners":
                        { "cosigner#0": #{accXPubTxt} },
                      "template":
                          { "all":
                             [ "cosigner#0",
                               { "active_from": 120 }
                             ]
                          }
                    }
                } |]
        r <- postSharedWallet ctx Default payload
        verify (second (\(Right (ApiSharedWallet (Right res))) -> Right res) r)
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
            , expectField (#accountIndex . #getApiT) (`shouldBe` DerivationIndex 2147483658)
            ]

    it "SHARED_WALLETS_CREATE_04 - Create a pending shared wallet from account xpub" $ \ctx -> runResourceT $ do
        (_, accXPubTxt):_ <- liftIO $ genXPubs 1
        let payload = Json [json| {
                "name": "Shared Wallet",
                "account_public_key": #{accXPubTxt},
                "account_index": "10H",
                "payment_script_template":
                    { "cosigners":
                        { "cosigner#0": #{accXPubTxt} },
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
        verify (second (\(Right (ApiSharedWallet (Left res))) -> Right res) r)
            [ expectResponseCode HTTP.status201
            , expectField
                    (#name . #getApiT . #getWalletName) (`shouldBe` "Shared Wallet")
            , expectField
                    (#addressPoolGap . #getApiT . #getAddressPoolGap) (`shouldBe` 20)
            , expectField #delegationScriptTemplate (`shouldBe` Nothing)
            , expectField (#accountIndex . #getApiT) (`shouldBe` DerivationIndex 2147483658)
            ]

    it "SHARED_WALLETS_CREATE_05 - Create an active shared wallet from root xprv with self" $ \ctx -> runResourceT $ do
        m15txt <- liftIO $ genMnemonics M15
        m12txt <- liftIO $ genMnemonics M12
        let (Right m15) = mkSomeMnemonic @'[ 15 ] m15txt
        let (Right m12) = mkSomeMnemonic @'[ 12 ] m12txt
        let passphrase = Passphrase $ BA.convert $ T.encodeUtf8 fixturePassphrase
        let index = 30
        let accXPubDerived = accPubKeyFromMnemonics m15 (Just m12) index passphrase
        let payloadPost = Json [json| {
                "name": "Shared Wallet",
                "mnemonic_sentence": #{m15txt},
                "mnemonic_second_factor": #{m12txt},
                "passphrase": #{fixturePassphrase},
                "account_index": "30H",
                "payment_script_template":
                    { "cosigners":
                        { "cosigner#0": #{accXPubDerived} },
                      "template":
                          { "all":
                             [ "cosigner#0",
                               { "active_from": 120 }
                             ]
                          }
                    }
                } |]
        rPost <- postSharedWallet ctx Default payloadPost
        verify rPost
            [ expectResponseCode HTTP.status201
            ]

        let wal = getFromResponse id rPost

        rDel <- deleteSharedWallet ctx wal
        expectResponseCode HTTP.status204 rDel

        let payloadPostWithSelf = Json [json| {
                "name": "Shared Wallet",
                "mnemonic_sentence": #{m15txt},
                "mnemonic_second_factor": #{m12txt},
                "passphrase": #{fixturePassphrase},
                "account_index": "30H",
                "payment_script_template":
                    { "cosigners":
                        { "cosigner#0": "self" },
                      "template":
                          { "all":
                             [ "cosigner#0",
                               { "active_from": 120 }
                             ]
                          }
                    }
                } |]
        rPostWithSelf <- postSharedWallet ctx Default payloadPostWithSelf
        verify rPostWithSelf
            [ expectResponseCode HTTP.status201
            ]

        let walWithSelf = getFromResponse id rPostWithSelf

        getWalletIdFromSharedWallet walWithSelf `Expectations.shouldBe` getWalletIdFromSharedWallet wal

    it "SHARED_WALLETS_CREATE_06 - Create an active shared wallet from account xpub with self" $ \ctx -> runResourceT $ do
        (_, accXPubTxt):_ <- liftIO $ genXPubs 1
        let payload = Json [json| {
                "name": "Shared Wallet",
                "account_public_key": #{accXPubTxt},
                "account_index": "10H",
                "payment_script_template":
                    { "cosigners":
                        { "cosigner#0": #{accXPubTxt} },
                      "template":
                          { "all":
                             [ "cosigner#0",
                               { "active_from": 120 }
                             ]
                          }
                    }
                } |]
        rPost <- postSharedWallet ctx Default payload
        verify rPost
            [ expectResponseCode HTTP.status201
            ]

        let wal = getFromResponse id rPost

        rDel <- deleteSharedWallet ctx wal
        expectResponseCode HTTP.status204 rDel

        let payloadWithSelf = Json [json| {
                "name": "Shared Wallet",
                "account_public_key": #{accXPubTxt},
                "account_index": "10H",
                "payment_script_template":
                    { "cosigners":
                        { "cosigner#0": "self" },
                      "template":
                          { "all":
                             [ "cosigner#0",
                               { "active_from": 120 }
                             ]
                          }
                    }
                } |]
        rPostWithSelf <- postSharedWallet ctx Default payloadWithSelf
        verify rPostWithSelf
            [ expectResponseCode HTTP.status201
            ]

        let walWithSelf = getFromResponse id rPostWithSelf

        getWalletIdFromSharedWallet walWithSelf `Expectations.shouldBe` getWalletIdFromSharedWallet wal

    it "SHARED_WALLETS_DELETE_01 - Delete of a shared wallet" $ \ctx -> runResourceT $ do
        let walName = "Shared Wallet" :: Text
        (_, payload) <- getAccountWallet walName
        rInit <- postSharedWallet ctx Default payload
        verify rInit
            [ expectResponseCode HTTP.status201 ]
        let wal = getFromResponse id rInit

        eventually "Wallet state = Ready" $ do
            r <- getSharedWallet ctx wal
            verify (second (\(Right (ApiSharedWallet (Right res))) -> Right res) r)
                [ expectField (#state . #getApiT) (`shouldBe` Ready) ]

        rDel <- deleteSharedWallet ctx wal
        expectResponseCode HTTP.status204 rDel

    it "SHARED_WALLETS_PATCH_01 - Add cosigner key in a pending shared wallet and transit it to the active shared wallet" $ \ctx -> runResourceT $ do
        [(accXPub0, accXPubTxt0),(accXPub1,accXPubTxt1)] <- liftIO $ genXPubs 2

        let payloadCreate = Json [json| {
                "name": "Shared Wallet",
                "account_public_key": #{accXPubTxt0},
                "account_index": "30H",
                "payment_script_template":
                    { "cosigners":
                        { "cosigner#0": #{accXPubTxt0} },
                      "template":
                          { "all":
                             [ "cosigner#0",
                               "cosigner#1",
                               { "active_from": 120 }
                             ]
                          }
                    }
                } |]
        rPost <- postSharedWallet ctx Default payloadCreate
        expectResponseCode HTTP.status201 rPost
        let wal@(ApiSharedWallet (Left pendingWal)) = getFromResponse id rPost
        let cosignerKeysPost = pendingWal ^. #paymentScriptTemplate
        liftIO $ cosigners cosignerKeysPost `shouldBe` Map.fromList [(Cosigner 0,accXPub0)]

        let payloadPatch = Json [json| {
                "cosigner#1": #{accXPubTxt1}
                } |]

        rPatch <- patchSharedWallet ctx wal Payment payloadPatch
        expectResponseCode HTTP.status200 rPatch
        let (ApiSharedWallet (Right activeWal)) = getFromResponse id rPatch
        let cosignerKeysPatch = activeWal ^. #paymentScriptTemplate
        liftIO $ cosigners cosignerKeysPatch `shouldBe` Map.fromList [(Cosigner 0,accXPub0), (Cosigner 1,accXPub1)]

    it "SHARED_WALLETS_PATCH_02 - Add cosigner for delegation script template" $ \ctx -> runResourceT $ do
        [(accXPub0, accXPubTxt0),(accXPub1,accXPubTxt1)] <- liftIO $ genXPubs 2
        let payloadCreate = Json [json| {
                "name": "Shared Wallet",
                "account_public_key": #{accXPubTxt0},
                "account_index": "30H",
                "payment_script_template":
                    { "cosigners":
                        { "cosigner#0": #{accXPubTxt0} },
                      "template":
                          { "all":
                             [ "cosigner#0",
                               { "active_from": 120 }
                             ]
                          }
                    },
                "delegation_script_template":
                    { "cosigners":
                        { "cosigner#0": #{accXPubTxt0} },
                      "template":
                          { "all":
                             [ "cosigner#0",
                               "cosigner#1",
                               { "active_from": 120 },
                               { "active_until": 100 }
                             ]
                          }
                    }
                } |]
        rPost <- postSharedWallet ctx Default payloadCreate
        expectResponseCode HTTP.status201 rPost
        let wal@(ApiSharedWallet (Left pendingWal)) = getFromResponse id rPost
        let cosignerKeysPostInPayment = pendingWal ^. #paymentScriptTemplate
        liftIO $ cosigners cosignerKeysPostInPayment `shouldBe` Map.fromList [(Cosigner 0,accXPub0)]
        let (Just cosignerKeysPostInDelegation) = pendingWal ^. #delegationScriptTemplate
        liftIO $ cosigners cosignerKeysPostInDelegation `shouldBe` Map.fromList [(Cosigner 0,accXPub0)]

        let payloadPatch = Json [json| {
                "cosigner#1": #{accXPubTxt1}
                } |]

        rPatch <- patchSharedWallet ctx wal Delegation payloadPatch
        expectResponseCode HTTP.status200 rPatch
        let (ApiSharedWallet (Right activeWal)) = getFromResponse id rPatch
        let (Just cosignerKeysPatch) = activeWal ^. #delegationScriptTemplate
        liftIO $ cosigners cosignerKeysPatch `shouldBe` Map.fromList [(Cosigner 0,accXPub0), (Cosigner 1,accXPub1)]

    it "SHARED_WALLETS_PATCH_03 - Cannot add cosigner key in an active shared wallet" $ \ctx -> runResourceT $ do
        [(_, accXPubTxt0),(_,accXPubTxt1)] <- liftIO $ genXPubs 2
        let payloadCreate = Json [json| {
                "name": "Shared Wallet",
                "account_public_key": #{accXPubTxt0},
                "account_index": "30H",
                "payment_script_template":
                    { "cosigners":
                        { "cosigner#0": #{accXPubTxt0} },
                      "template":
                          { "all":
                             [ "cosigner#0",
                               { "active_from": 120 }
                             ]
                          }
                    }
                } |]
        rPost <- postSharedWallet ctx Default payloadCreate
        expectResponseCode HTTP.status201 rPost
        let wal@(ApiSharedWallet (Right _activeWal)) = getFromResponse id rPost

        let payloadPatch = Json [json| {
                "cosigner#1": #{accXPubTxt1}
                } |]

        rPatch <- patchSharedWallet ctx wal Payment payloadPatch
        expectResponseCode HTTP.status403 rPatch
        expectErrorMessage errMsg403WalletAlreadyActive rPatch

    it "SHARED_WALLETS_PATCH_04 - Cannot add cosigner key when delegation script missing and cannot add already existant key to other cosigner" $ \ctx -> runResourceT $ do
        [(_, accXPubTxt0),(_,accXPubTxt1)] <- liftIO $ genXPubs 2
        let payloadCreate = Json [json| {
                "name": "Shared Wallet",
                "account_public_key": #{accXPubTxt0},
                "account_index": "30H",
                "payment_script_template":
                    { "cosigners":
                        { "cosigner#0": #{accXPubTxt0} },
                      "template":
                          { "all":
                             [ "cosigner#0",
                               "cosigner#1",
                               { "active_from": 120 }
                             ]
                          }
                    }
                } |]
        rPost <- postSharedWallet ctx Default payloadCreate
        expectResponseCode HTTP.status201 rPost
        let wal@(ApiSharedWallet (Left _pendingWal)) = getFromResponse id rPost

        let payloadPatch = Json [json| {
                "cosigner#1": #{accXPubTxt1}
                } |]

        rPatch <- patchSharedWallet ctx wal Delegation payloadPatch
        expectResponseCode HTTP.status403 rPatch
        expectErrorMessage errMsg403NoDelegationTemplate rPatch

    it "SHARED_WALLETS_PATCH_05 - Cannot create shared wallet when missing wallet's account public key in template" $ \ctx -> runResourceT $ do
        [(_, accXPubTxt0),(_,accXPubTxt1)] <- liftIO $ genXPubs 2
        let payloadCreate = Json [json| {
                "name": "Shared Wallet",
                "account_public_key": #{accXPubTxt0},
                "account_index": "30H",
                "payment_script_template":
                    { "cosigners":
                        { "cosigner#0": #{accXPubTxt1} },
                      "template":
                          { "all":
                             [ "cosigner#0",
                               "cosigner#1",
                               { "active_from": 120 }
                             ]
                          }
                    }
                } |]
        rPost <- postSharedWallet ctx Default payloadCreate
        expectResponseCode HTTP.status403 rPost
        expectErrorMessage errMsg403CreateIllegal rPost

    it "SHARED_WALLETS_PATCH_06 - Can add the same cosigner key for delegation script template but not payment one" $ \ctx -> runResourceT $ do
        [(_, accXPubTxt0),(_,accXPubTxt1)] <- liftIO $ genXPubs 2
        let payloadCreate = Json [json| {
                "name": "Shared Wallet",
                "account_public_key": #{accXPubTxt0},
                "account_index": "30H",
                "payment_script_template":
                    { "cosigners":
                        { "cosigner#0": #{accXPubTxt0} },
                      "template":
                          { "all":
                             [ "cosigner#0",
                               "cosigner#1",
                               { "active_from": 120 }
                             ]
                          }
                    },
                "delegation_script_template":
                    { "cosigners":
                        { "cosigner#0": #{accXPubTxt0} },
                      "template":
                          { "all":
                             [ "cosigner#0",
                               "cosigner#1",
                               { "active_from": 120 },
                               { "active_until": 100 }
                             ]
                          }
                    }
                } |]
        rPost <- postSharedWallet ctx Default payloadCreate
        expectResponseCode HTTP.status201 rPost
        let wal@(ApiSharedWallet (Left _pendingWal)) = getFromResponse id rPost

        let payloadPatch1 = Json [json| {
                "cosigner#1": #{accXPubTxt1}
                } |]
        rPatch1 <- patchSharedWallet ctx wal Delegation payloadPatch1
        expectResponseCode HTTP.status200 rPatch1

        let payloadPatch2 = Json [json| {
                "cosigner#0": #{accXPubTxt0}
                } |]
        rPatch2 <- patchSharedWallet ctx wal Payment payloadPatch2
        expectResponseCode HTTP.status403 rPatch2
        expectErrorMessage errMsg403CannotUpdateThisCosigner rPatch2

        let payloadPatch3 = Json [json| {
                "cosigner#1": #{accXPubTxt0}
                } |]
        rPatch3 <- patchSharedWallet ctx wal Payment payloadPatch3
        expectResponseCode HTTP.status403 rPatch3
        expectErrorMessage (errMsg403KeyAlreadyPresent (toText Payment)) rPatch3

        let payloadPatch4 = Json [json| {
                "cosigner#1": #{accXPubTxt1}
                } |]
        rPatch4 <- patchSharedWallet ctx wal Delegation payloadPatch4
        expectResponseCode HTTP.status403 rPatch4
        expectErrorMessage (errMsg403KeyAlreadyPresent (toText Delegation)) rPatch4

        let payloadPatch5 = Json [json| {
                "cosigner#7": #{accXPubTxt0}
                } |]
        rPatch5 <- patchSharedWallet ctx wal Payment payloadPatch5
        expectResponseCode HTTP.status403 rPatch5
        expectErrorMessage (errMsg403NoSuchCosigner (toText Payment) 7) rPatch5

    it "SHARED_WALLETS_PATCH_07 - Cannot update cosigner key in a pending shared wallet having the shared wallet's account key" $ \ctx -> runResourceT $ do
        [(_, accXPubTxt0),(_,accXPubTxt1)] <- liftIO $ genXPubs 2
        let payloadCreate = Json [json| {
                "name": "Shared Wallet",
                "account_public_key": #{accXPubTxt0},
                "account_index": "30H",
                "payment_script_template":
                    { "cosigners":
                        { "cosigner#0": #{accXPubTxt0} },
                      "template":
                          { "all":
                             [ "cosigner#0",
                               "cosigner#1",
                               { "active_from": 120 }
                             ]
                          }
                    }
                } |]
        rPost <- postSharedWallet ctx Default payloadCreate
        expectResponseCode HTTP.status201 rPost
        let wal@(ApiSharedWallet (Left _pendingWal)) = getFromResponse id rPost

        let payloadPatch = Json [json| {
                "cosigner#0": #{accXPubTxt1}
                } |]
        rPatch <- patchSharedWallet ctx wal Payment payloadPatch
        expectResponseCode HTTP.status403 rPatch
        expectErrorMessage errMsg403CannotUpdateThisCosigner rPatch

    it "SHARED_WALLETS_KEYS_01 - Getting verification keys works for active shared wallet" $ \ctx -> runResourceT $ do
        let walName = "Shared Wallet" :: Text
        (_, payload) <- getAccountWallet walName
        rPost <- postSharedWallet ctx Default payload
        verify rPost
            [ expectResponseCode HTTP.status201
            ]
        let wal@(ApiSharedWallet (Right _activeWal)) = getFromResponse id rPost

        (_, Right paymentKey) <- getSharedWalletKey ctx wal UtxoExternal (DerivationIndex 30) Nothing
        (_, Right stakeKey) <- getSharedWalletKey ctx wal MutableAccount (DerivationIndex 0) Nothing

        let (String paymentAddr) = toJSON paymentKey
        T.isPrefixOf "addr_shared_vk" paymentAddr `Expectations.shouldBe` True

        let (String stakeAddr) = toJSON stakeKey
        T.isPrefixOf "stake_shared_vk" stakeAddr `Expectations.shouldBe` True

    it "SHARED_WALLETS_KEYS_02 - Getting verification keys works for pending shared wallet" $ \ctx -> runResourceT $ do
        (_, accXPubTxt):_ <- liftIO $ genXPubs 1
        let payload = Json [json| {
                "name": "Shared Wallet",
                "account_public_key": #{accXPubTxt},
                "account_index": "10H",
                "payment_script_template":
                    { "cosigners":
                        { "cosigner#0": #{accXPubTxt} },
                      "template":
                          { "all":
                             [ "cosigner#0",
                               "cosigner#1",
                               { "active_from": 120 }
                             ]
                          }
                    }
                } |]
        rPost <- postSharedWallet ctx Default payload
        verify rPost
            [ expectResponseCode HTTP.status201
            ]
        let wal@(ApiSharedWallet (Left _pendingWal)) = getFromResponse id rPost

        (_, Right paymentKey) <- getSharedWalletKey ctx wal UtxoExternal (DerivationIndex 10) Nothing
        (_, Right stakeKey) <- getSharedWalletKey ctx wal MutableAccount (DerivationIndex 0) Nothing

        let (String paymentAddr) = toJSON paymentKey
        T.isPrefixOf "addr_shared_vk" paymentAddr `Expectations.shouldBe` True

        let (String stakeAddr) = toJSON stakeKey
        T.isPrefixOf "stake_shared_vk" stakeAddr `Expectations.shouldBe` True

        (_, Right paymentKey') <- getSharedWalletKey ctx wal UtxoExternal (DerivationIndex 10) (Just False)
        (_, Right stakeKey') <- getSharedWalletKey ctx wal MutableAccount (DerivationIndex 0) (Just False)

        paymentKey' `Expectations.shouldBe` paymentKey
        stakeKey' `Expectations.shouldBe` stakeKey

        (_, Right paymentKeyH) <- getSharedWalletKey ctx wal UtxoExternal (DerivationIndex 10) (Just True)
        (_, Right stakeKeyH) <- getSharedWalletKey ctx wal MutableAccount (DerivationIndex 0) (Just True)

        let (String paymentAddrH) = toJSON paymentKeyH
        T.isPrefixOf "addr_shared_vkh" paymentAddrH `Expectations.shouldBe` True

        let (String stakeAddrH) = toJSON stakeKeyH
        T.isPrefixOf "stake_shared_vkh" stakeAddrH `Expectations.shouldBe` True

    it "SHARED_WALLETS_LIST_01 - Created a wallet can be listed" $ \ctx -> runResourceT $ do
        let walName = "Shared Wallet" :: Text
        (_, payload) <- getAccountWallet walName
        rPost <- postSharedWallet ctx Default payload
        verify rPost
            [ expectResponseCode HTTP.status201
            ]
        let wal = getFromResponse id rPost

        let unwrap = \case
                ApiSharedWallet (Right res) -> res
                _ -> error "expecting active shared wallet"

        rl <- listFilteredSharedWallets (Set.singleton (getWalletIdFromSharedWallet wal ^. walletId) ) ctx
        verify (second (\(Right wals) -> Right $ unwrap <$> wals) rl)
            [ expectResponseCode HTTP.status200
            , expectListSize 1
            , expectListField 0
                  (#name . #getApiT . #getWalletName) (`shouldBe` walName)
            , expectListField 0
                    (#addressPoolGap . #getApiT . #getAddressPoolGap) (`shouldBe` 20)
            , expectListField 0 (#balance . #available) (`shouldBe` Quantity 0)
            , expectListField 0 (#balance . #total) (`shouldBe` Quantity 0)
            , expectListField 0 (#balance . #reward) (`shouldBe` Quantity 0)
            , expectListField 0 (#assets . #total) (`shouldBe` mempty)
            , expectListField 0 (#assets . #available) (`shouldBe` mempty)
            , expectListField 0 #delegation (`shouldBe` notDelegating [])
            , expectListField 0 #delegationScriptTemplate (`shouldBe` Nothing)
            , expectListField 0 (#accountIndex . #getApiT) (`shouldBe` DerivationIndex 2147483678)
            ]

    it "SHARED_WALLETS_LIST_01 - Wallets are listed from oldest to newest" $ \ctx -> runResourceT $ do
        let walNames = ["1", "2", "3"] :: [Text]
        wids <- forM walNames $ \walName -> do
            (_, payload) <- getAccountWallet walName
            rPost <- postSharedWallet ctx Default payload
            verify rPost
                [ expectResponseCode HTTP.status201
                ]
            let wal = getFromResponse id rPost
            pure (getWalletIdFromSharedWallet wal ^. walletId)

        let unwrap = \case
                ApiSharedWallet (Right res) -> res
                _ -> error "expecting active shared wallet"

        rl <- listFilteredSharedWallets (Set.fromList wids) ctx
        verify (second (\(Right wals) -> Right $ unwrap <$> wals) rl)
            [ expectResponseCode HTTP.status200
            , expectListSize 3
            , expectListField 0
                (#name . #getApiT . #getWalletName) (`shouldBe` "1")
            , expectListField 1
                (#name . #getApiT . #getWalletName) (`shouldBe` "2")
            , expectListField 2
                (#name . #getApiT . #getWalletName) (`shouldBe` "3")
            ]

    it "SHARED_WALLETS_LIST_02 - Deleted wallet not listed" $ \ctx -> runResourceT $ do
        let walName = "Shared Wallet" :: Text
        (_, payload) <- getAccountWallet walName
        rPost <- postSharedWallet ctx Default payload
        verify rPost
            [ expectResponseCode HTTP.status201
            ]
        let wal = getFromResponse id rPost

        rDel <- deleteSharedWallet ctx wal
        expectResponseCode HTTP.status204 rDel

        rl <- listFilteredSharedWallets (Set.singleton $ getWalletIdFromSharedWallet wal ^. walletId) ctx
        verify rl
            [ expectResponseCode HTTP.status200
            , expectListSize 0
            ]
  where
     getAccountWallet name = do
          (_, accXPubTxt):_ <- liftIO $ genXPubs 1
          let payload = Json [json| {
                  "name": #{name},
                  "account_public_key": #{accXPubTxt},
                  "account_index": "30H",
                  "payment_script_template":
                      { "cosigners":
                          { "cosigner#0": #{accXPubTxt} },
                        "template":
                            { "all":
                               [ "cosigner#0",
                                 { "active_from": 120 }
                               ]
                            }
                      }
                  } |]
          return (accXPubTxt, payload)
