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
    , ApiAddress
    , ApiFee (..)
    , ApiSharedWallet (..)
    , ApiTransaction
    , ApiWallet
    , DecodeAddress
    , DecodeStakeAddress
    , EncodeAddress (..)
    , KeyFormat (..)
    , WalletStyle (..)
    )
import Cardano.Wallet.Compat
    ( (^?) )
import Cardano.Wallet.Primitive.AddressDerivation
    ( DerivationIndex (..), Role (..) )
import Cardano.Wallet.Primitive.AddressDiscovery.Shared
    ( CredentialType (..) )
import Cardano.Wallet.Primitive.Passphrase
    ( Passphrase (..) )
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
import Data.Either.Combinators
    ( swapEither )
import Data.Generics.Internal.VL.Lens
    ( view, (^.) )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( ToText (..) )
import Test.Hspec
    ( SpecWith, describe )
import Test.Hspec.Expectations.Lifted
    ( shouldBe, shouldNotBe )
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
    , faucetAmt
    , fixturePassphrase
    , fixtureWallet
    , genMnemonics
    , genXPubs
    , getAccountKeyShared
    , getFromResponse
    , getSharedWallet
    , getSharedWalletKey
    , getWalletIdFromSharedWallet
    , hexText
    , json
    , listFilteredSharedWallets
    , minUTxOValue
    , notDelegating
    , patchSharedWallet
    , postAccountKeyShared
    , postSharedWallet
    , request
    , unsafeRequest
    , verify
    , walletId
    )
import Test.Integration.Framework.TestData
    ( errMsg403CannotUpdateThisCosigner
    , errMsg403CreateIllegal
    , errMsg403KeyAlreadyPresent
    , errMsg403NoDelegationTemplate
    , errMsg403NoSuchCosigner
    , errMsg403TemplateInvalidDuplicateXPub
    , errMsg403TemplateInvalidNoCosignerInScript
    , errMsg403TemplateInvalidScript
    , errMsg403TemplateInvalidUnknownCosigner
    , errMsg403WalletAlreadyActive
    , errMsg403WrongIndex
    )

import qualified Cardano.Wallet.Api.Link as Link
import qualified Data.ByteArray as BA
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Network.HTTP.Types as HTTP


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
        verify (fmap (view #wallet) <$> rPost)
            [ expectResponseCode HTTP.status201
            , expectField
                (traverse . #name . #getApiT . #getWalletName)
                (`shouldBe` "Shared Wallet")
            , expectField
                (traverse . #addressPoolGap . #getApiT . #getAddressPoolGap)
                (`shouldBe` 20)
            , expectField
                (traverse . #balance . #available) (`shouldBe` Quantity 0)
            , expectField
                (traverse . #balance . #total) (`shouldBe` Quantity 0)
            , expectField
                (traverse . #balance . #reward) (`shouldBe` Quantity 0)
            , expectField (traverse . #assets . #total)
                (`shouldBe` mempty)
            , expectField (traverse . #assets . #available)
                (`shouldBe` mempty)
            , expectField (traverse . #delegation)
                (`shouldBe` notDelegating [])
            , expectField (traverse . #passphrase)
                (`shouldNotBe` Nothing)
            , expectField (traverse . #delegationScriptTemplate)
                (`shouldBe` Nothing)
            , expectField (traverse . #accountIndex . #getApiT)
                (`shouldBe` DerivationIndex 2147483678)
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
        let ApiAccountKeyShared bytes _ _ = getFromResponse id rKey
        hexText bytes `shouldBe` accXPubDerived

        aKey <- getAccountKeyShared ctx wal (Just Extended)

        verify aKey
            [ expectResponseCode HTTP.status200
            , expectField #format (`shouldBe` Extended)
            ]
        let ApiAccountKeyShared bytes' _ _ = getFromResponse id aKey
        hexText bytes' `shouldBe` accXPubDerived

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
        verify (fmap (swapEither . view #wallet) <$> rPost)
            [ expectResponseCode HTTP.status201
            , expectField
                (traverse . #name . #getApiT . #getWalletName)
                (`shouldBe` "Shared Wallet")
            , expectField
                (traverse . #addressPoolGap . #getApiT . #getAddressPoolGap)
                (`shouldBe` 20)
            , expectField
                (traverse . #delegationScriptTemplate)
                (`shouldBe` Nothing)
            , expectField
                (traverse . #accountIndex . #getApiT)
                (`shouldBe` DerivationIndex 2147483678)
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
        let ApiAccountKeyShared bytes _ _ = getFromResponse id rKey
        hexText bytes `shouldBe` accXPubDerived

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
        rPost <- postSharedWallet ctx Default payload
        verify (fmap (view #wallet) <$> rPost)
            [ expectResponseCode HTTP.status201
            , expectField
                (traverse . #name . #getApiT . #getWalletName)
                (`shouldBe` "Shared Wallet")
            , expectField
                (traverse . #addressPoolGap . #getApiT . #getAddressPoolGap)
                (`shouldBe` 20)
            , expectField (traverse . #balance . #available)
                (`shouldBe` Quantity 0)
            , expectField (traverse . #balance . #total)
                (`shouldBe` Quantity 0)
            , expectField (traverse . #balance . #reward)
                (`shouldBe` Quantity 0)
            , expectField (traverse . #assets . #total)
                (`shouldBe` mempty)
            , expectField (traverse . #assets . #available)
                (`shouldBe` mempty)
            , expectField (traverse . #delegation)
                (`shouldBe` notDelegating [])
            , expectField (traverse . #passphrase)
                (`shouldBe` Nothing)
            , expectField (traverse . #delegationScriptTemplate)
                (`shouldBe` Nothing)
            , expectField
                (traverse . #accountIndex . #getApiT)
                (`shouldBe` DerivationIndex 2147483658)
            ]

        let wal = getFromResponse id rPost
        aKey <- getAccountKeyShared ctx wal (Just Extended)

        verify aKey
            [ expectResponseCode HTTP.status200
            , expectField #format (`shouldBe` Extended)
            ]
        let ApiAccountKeyShared bytes' _ _ = getFromResponse id aKey
        hexText bytes' `shouldBe` accXPubTxt

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
        verify (fmap (swapEither . view #wallet) <$> r)
            [ expectResponseCode HTTP.status201
            , expectField
                (traverse . #name . #getApiT . #getWalletName)
                (`shouldBe` "Shared Wallet")
            , expectField
                (traverse . #addressPoolGap . #getApiT . #getAddressPoolGap)
                (`shouldBe` 20)
            , expectField
                (traverse . #delegationScriptTemplate)
                (`shouldBe` Nothing)
            , expectField
                (traverse . #accountIndex . #getApiT)
                (`shouldBe` DerivationIndex 2147483658)
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

        getWalletIdFromSharedWallet walWithSelf `shouldBe` getWalletIdFromSharedWallet wal

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

        getWalletIdFromSharedWallet walWithSelf `shouldBe` getWalletIdFromSharedWallet wal

    it "SHARED_WALLETS_CREATE_07 - Incorrect script template due to NoCosignerInScript" $ \ctx -> runResourceT $ do
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
                             [ { "active_from": 120 }
                             ]
                          }
                    }
                } |]
        rPost <- postSharedWallet ctx Default payload
        expectResponseCode HTTP.status403 rPost
        expectErrorMessage errMsg403TemplateInvalidNoCosignerInScript rPost

    it "SHARED_WALLETS_CREATE_08 - Incorrect script template due to UnknownCosigner" $ \ctx -> runResourceT $ do
        (_, accXPubTxt):_ <- liftIO $ genXPubs 1
        let payload = Json [json| {
                "name": "Shared Wallet",
                "account_public_key": #{accXPubTxt},
                "account_index": "10H",
                "payment_script_template":
                    { "cosigners":
                        { "cosigner#1": #{accXPubTxt} },
                      "template":
                          { "all":
                             [ "cosigner#0",
                              { "active_from": 120 }
                             ]
                          }
                    }
                } |]
        rPost <- postSharedWallet ctx Default payload
        expectResponseCode HTTP.status403 rPost
        expectErrorMessage errMsg403TemplateInvalidUnknownCosigner rPost

    it "SHARED_WALLETS_CREATE_09 - Incorrect script template due to DuplicateXPub" $ \ctx -> runResourceT $ do
        (_, accXPubTxt):_ <- liftIO $ genXPubs 1
        let payload = Json [json| {
                "name": "Shared Wallet",
                "account_public_key": #{accXPubTxt},
                "account_index": "10H",
                "payment_script_template":
                    { "cosigners":
                        { "cosigner#0": #{accXPubTxt}, "cosigner#1": #{accXPubTxt} },
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
        expectResponseCode HTTP.status403 rPost
        expectErrorMessage errMsg403TemplateInvalidDuplicateXPub rPost

    it "SHARED_WALLETS_CREATE_10 - Incorrect script template due to WrongScript when recommended validation" $ \ctx -> runResourceT $ do
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
                               { "active_from": 120 },
                               "cosigner#0"
                             ]
                          }
                    }
                } |]
        rPost <- postSharedWallet ctx Default payload
        expectResponseCode HTTP.status403 rPost
        let reason = "The list inside a script has duplicate keys (which is not recommended)."
        expectErrorMessage (errMsg403TemplateInvalidScript reason) rPost

    it "SHARED_WALLETS_CREATE_11 - Correct script template when required validation" $ \ctx -> runResourceT $ do
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
                               { "active_from": 120 },
                               "cosigner#0"
                             ]
                          }
                    },
                "script_validation": "required"
                } |]
        rPost <- postSharedWallet ctx Default payload
        expectResponseCode HTTP.status201 rPost

    it "SHARED_WALLETS_CREATE_12 - Incorrect script template due to WrongScript - timelocks" $ \ctx -> runResourceT $ do
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
                               { "active_from": 120 },
                               { "active_until": 110 }
                             ]
                          }
                    }
                } |]
        rPost <- postSharedWallet ctx Default payload
        expectResponseCode HTTP.status403 rPost
        let reason = "The timelocks used are contradictory when used with 'all' (which is not recommended)."
        expectErrorMessage (errMsg403TemplateInvalidScript reason) rPost

    it "SHARED_WALLETS_CREATE_13 - Incorrect account index" $ \ctx -> runResourceT $ do
        [(_, accXPubTxt0)] <- liftIO $ genXPubs 1
        let payloadCreate = Json [json| {
                "name": "Shared Wallet",
                "account_public_key": #{accXPubTxt0},
                "account_index": "30",
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
        expectResponseCode HTTP.status403 rPost
        expectErrorMessage errMsg403WrongIndex rPost

    it "SHARED_WALLETS_DELETE_01 - Delete of a shared wallet" $ \ctx -> runResourceT $ do
        let walName = "Shared Wallet" :: Text
        (_, payload) <- getAccountWallet walName
        rInit <- postSharedWallet ctx Default payload
        verify rInit
            [ expectResponseCode HTTP.status201 ]
        let wal = getFromResponse id rInit

        eventually "Wallet state = Ready" $ do
            r <- getSharedWallet ctx wal
            expectField
                (traverse . #state . #getApiT)
                (`shouldBe` Ready)
                (fmap (view #wallet) <$> r)

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
                          { "any":
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

    it "SHARED_WALLETS_PATCH_04 - Cannot add cosigner key when delegation script missing and cannot add already existent key to other cosigner" $ \ctx -> runResourceT $ do
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
                          { "any":
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
        T.isPrefixOf "addr_shared_vk" paymentAddr `shouldBe` True

        let (String stakeAddr) = toJSON stakeKey
        T.isPrefixOf "stake_shared_vk" stakeAddr `shouldBe` True

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
        T.isPrefixOf "addr_shared_vk" paymentAddr `shouldBe` True

        let (String stakeAddr) = toJSON stakeKey
        T.isPrefixOf "stake_shared_vk" stakeAddr `shouldBe` True

        (_, Right paymentKey') <- getSharedWalletKey ctx wal UtxoExternal (DerivationIndex 10) (Just False)
        (_, Right stakeKey') <- getSharedWalletKey ctx wal MutableAccount (DerivationIndex 0) (Just False)

        paymentKey' `shouldBe` paymentKey
        stakeKey' `shouldBe` stakeKey

        (_, Right paymentKeyH) <- getSharedWalletKey ctx wal UtxoExternal (DerivationIndex 10) (Just True)
        (_, Right stakeKeyH) <- getSharedWalletKey ctx wal MutableAccount (DerivationIndex 0) (Just True)

        let (String paymentAddrH) = toJSON paymentKeyH
        T.isPrefixOf "addr_shared_vkh" paymentAddrH `shouldBe` True

        let (String stakeAddrH) = toJSON stakeKeyH
        T.isPrefixOf "stake_shared_vkh" stakeAddrH `shouldBe` True

    it "SHARED_WALLETS_LIST_01 - Created a wallet can be listed" $ \ctx -> runResourceT $ do
        let walName = "Shared Wallet" :: Text
        (_, payload) <- getAccountWallet walName
        rPost <- postSharedWallet ctx Default payload
        verify rPost
            [ expectResponseCode HTTP.status201
            ]
        let wal = getFromResponse id rPost

        rl <- listFilteredSharedWallets (Set.singleton (getWalletIdFromSharedWallet wal ^. walletId) ) ctx
        verify (fmap (fmap (view #wallet)) <$> rl)
            [ expectResponseCode HTTP.status200
            , expectListSize 1
            , expectListField 0
                (traverse . #name . #getApiT . #getWalletName)
                (`shouldBe` walName)
            , expectListField 0
                (traverse . #addressPoolGap . #getApiT . #getAddressPoolGap)
                (`shouldBe` 20)
            , expectListField 0
                (traverse . #balance . #available) (`shouldBe` Quantity 0)
            , expectListField 0 (traverse . #balance . #total) (`shouldBe` Quantity 0)
            , expectListField 0 (traverse . #balance . #reward) (`shouldBe` Quantity 0)
            , expectListField 0 (traverse . #assets . #total) (`shouldBe` mempty)
            , expectListField 0 (traverse . #assets . #available) (`shouldBe` mempty)
            , expectListField 0 (traverse . #delegation) (`shouldBe` notDelegating [])
            , expectListField 0 (traverse . #delegationScriptTemplate) (`shouldBe` Nothing)
            , expectListField 0 (traverse . #accountIndex . #getApiT) (`shouldBe` DerivationIndex 2147483678)
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
        let name = (^? (#wallet . traverse . #name . #getApiT . #getWalletName))
        rl <- listFilteredSharedWallets (Set.fromList wids) ctx
        verify (fmap (map name) <$> rl)
            [ expectResponseCode HTTP.status200
            , expectListSize 3
            , expectListField 0 traverse (`shouldBe` "1")
            , expectListField 1 traverse (`shouldBe` "2")
            , expectListField 2 traverse (`shouldBe` "3")
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

    it "SHARED_WALLETS_DISCOVER_01 - Shared wallets can discover its address" $ \ctx -> runResourceT $ do
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
                             [ "cosigner#0"
                             ]
                          }
                    }
                } |]
        rPost <- postSharedWallet ctx Default payload
        verify (fmap (view #wallet) <$> rPost)
            [ expectResponseCode HTTP.status201
            , expectField (traverse . #balance . #available) (`shouldBe` Quantity 0)
            ]
        let walShared@(ApiSharedWallet (Right wal)) = getFromResponse id rPost

        rAddr <- request @[ApiAddress n] ctx
            (Link.listAddresses @'Shared wal) Default Empty
        expectResponseCode HTTP.status200 rAddr
        let sharedAddrs = getFromResponse id rAddr
        let destination = (sharedAddrs !! 1) ^. #id

        wShelley <- fixtureWallet ctx
        let amt = minUTxOValue (_mainEra ctx)
        let payloadTx = Json [json|{
                "payments": [{
                    "address": #{destination},
                    "amount": {
                        "quantity": #{amt},
                        "unit": "lovelace"
                    }
                }],
                "passphrase": #{fixturePassphrase}
            }|]
        (_, ApiFee (Quantity _) (Quantity feeMax) _ _) <- unsafeRequest ctx
            (Link.getTransactionFeeOld @'Shelley wShelley) payloadTx
        let ep = Link.createTransactionOld @'Shelley
        rTx <- request @(ApiTransaction n) ctx (ep wShelley) Default payloadTx
        expectResponseCode HTTP.status202 rTx
        eventually "wShelley balance is decreased" $ do
            ra <- request @ApiWallet ctx
                (Link.getWallet @'Shelley wShelley) Default Empty
            expectField
                (#balance . #available)
                (`shouldBe` Quantity (faucetAmt - feeMax - amt)) ra

        rWal <- getSharedWallet ctx walShared
        verify (fmap (view #wallet) <$> rWal)
            [ expectResponseCode HTTP.status200
            , expectField (traverse . #balance . #available) (`shouldBe` Quantity amt)
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
