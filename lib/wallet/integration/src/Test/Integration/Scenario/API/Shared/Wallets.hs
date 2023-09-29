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
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.Integration.Scenario.API.Shared.Wallets
    ( spec
    ) where

import Prelude

import Cardano.Address.Script
    ( Cosigner (..)
    , ScriptTemplate (..)
    )
import Cardano.Mnemonic
    ( MkSomeMnemonic (..)
    )
import Cardano.Wallet.Address.Derivation
    ( DerivationIndex (..)
    , Role (..)
    )
import Cardano.Wallet.Address.Discovery.Sequential
    ( defaultAddressPoolGap
    , getAddressPoolGap
    )
import Cardano.Wallet.Address.Discovery.Shared
    ( CredentialType (..)
    )
import Cardano.Wallet.Api.Types
    ( ApiAccountKeyShared (..)
    , ApiActiveSharedWallet
    , ApiAddressWithPath
    , ApiConstructTransaction
    , ApiCosignerIndex (..)
    , ApiCredentialType (..)
    , ApiFee (..)
    , ApiScriptTemplate (..)
    , ApiSerialisedTransaction (..)
    , ApiSharedWallet (..)
    , ApiT (..)
    , ApiTransaction
    , ApiTxId (..)
    , ApiUtxoStatistics
    , ApiWallet
    , ApiWalletUtxoSnapshot
    , KeyFormat (..)
    , WalletStyle (..)
    )
import Cardano.Wallet.Api.Types.Error
    ( ApiErrorInfo (..)
    , ApiErrorSharedWalletNoSuchCosigner (..)
    )
import Cardano.Wallet.Compat
    ( (^?)
    )
import Cardano.Wallet.Primitive.NetworkId
    ( HasSNetworkId
    )
import Cardano.Wallet.Primitive.Passphrase
    ( Passphrase (..)
    )
import Cardano.Wallet.Primitive.SyncProgress
    ( SyncProgress (..)
    )
import Cardano.Wallet.Primitive.Types.Address
    ( AddressState (..)
    )
import Cardano.Wallet.Primitive.Types.Tx.TxMeta
    ( TxStatus (..)
    )
import Control.Monad
    ( forM
    , forM_
    )
import Control.Monad.IO.Class
    ( liftIO
    )
import Control.Monad.Trans.Resource
    ( runResourceT
    )
import Data.Aeson
    ( ToJSON (..)
    , Value (String)
    )
import Data.Either.Combinators
    ( swapEither
    )
import Data.Generics.Internal.VL.Lens
    ( view
    , (^.)
    )
import Data.Quantity
    ( Quantity (..)
    )
import Data.Text
    ( Text
    )
import Data.Text.Class
    ( ToText (..)
    )
import Numeric.Natural
    ( Natural
    )
import Test.Hspec
    ( SpecWith
    , describe
    )
import Test.Hspec.Expectations.Lifted
    ( shouldBe
    , shouldNotBe
    )
import Test.Hspec.Extra
    ( it
    )
import Test.Integration.Framework.DSL
    ( Context (..)
    , Headers (..)
    , MnemonicLength (..)
    , Payload (..)
    , bech32Text
    , between
    , decodeErrorInfo
    , deleteSharedWallet
    , emptySharedWallet
    , eventually
    , expectErrorMessage
    , expectField
    , expectListField
    , expectListSize
    , expectResponseCode
    , expectWalletUTxO
    , faucetAmt
    , fixturePassphrase
    , fixtureSharedWallet
    , fixtureWallet
    , genMnemonics
    , genXPubsBech32
    , getAccountKeyShared
    , getFromResponse
    , getResponse
    , getSharedWallet
    , getSharedWalletKey
    , getWalletIdFromSharedWallet
    , json
    , listFilteredSharedWallets
    , minUTxOValue
    , notDelegating
    , patchSharedWallet
    , postAccountKeyShared
    , postSharedWallet
    , request
    , sharedAccPubKeyFromMnemonics
    , signSharedTx
    , submitSharedTxWithWid
    , unsafeRequest
    , verify
    , walletId
    , (</>)
    )
import Test.Integration.Framework.TestData
    ( errMsg403CreateIllegal
    , errMsg403KeyAlreadyPresent
    , errMsg403TemplateInvalidDuplicateXPub
    , errMsg403TemplateInvalidNoCosignerInScript
    , errMsg403TemplateInvalidScript
    , errMsg403TemplateInvalidUnknownCosigner
    , errMsg403WrongIndex
    , errMsg404NoWallet
    , errMsg406
    )

import qualified Cardano.Wallet.Api.Link as Link
import qualified Codec.Binary.Bech32.TH as Bech32
import qualified Data.ByteArray as BA
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Network.HTTP.Types as HTTP

spec
    :: forall n
     . HasSNetworkId n
    => SpecWith Context
spec = describe "SHARED_WALLETS" $ do

    it "SHARED_WALLETS_CREATE_01 - \
        \Create an active shared wallet from root xprv" $
        \ctx -> runResourceT $ do

        m15txt <- liftIO $ genMnemonics M15
        m12txt <- liftIO $ genMnemonics M12
        let (Right m15) = mkSomeMnemonic @'[ 15 ] m15txt
        let (Right m12) = mkSomeMnemonic @'[ 12 ] m12txt
        let passphrase = Passphrase
                $ BA.convert
                $ T.encodeUtf8 fixturePassphrase
        let index = 30
        let accXPubDerived = sharedAccPubKeyFromMnemonics
                m15 (Just m12) index passphrase
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
                (`shouldBe` DerivationIndex 2_147_483_678)
            ]

    it "SHARED_WALLETS_CREATE_01 - \
        \Compare wallet ids" $
        \ctx -> runResourceT $ do

        m15txt <- liftIO $ genMnemonics M15
        m12txt <- liftIO $ genMnemonics M12
        let (Right m15) = mkSomeMnemonic @'[ 15 ] m15txt
        let (Right m12) = mkSomeMnemonic @'[ 12 ] m12txt
        let passphrase = Passphrase $
                BA.convert $ T.encodeUtf8 fixturePassphrase
        let index = 30
        let accXPubDerived =
                sharedAccPubKeyFromMnemonics m15 (Just m12) index passphrase
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
            ]

        let wal = getFromResponse Prelude.id rPost

        let payloadKey = Json [json|{
                "passphrase": #{fixturePassphrase},
                "format": "extended"
            }|]
        rKey <-
            postAccountKeyShared ctx wal
                (DerivationIndex $ 2_147_483_648 + index) Default payloadKey

        verify rKey
            [ expectResponseCode HTTP.status202
            , expectField #format (`shouldBe` Extended)
            ]
        let ApiAccountKeyShared bytes _ _ = getFromResponse Prelude.id rKey
        bech32Text acctHrp bytes `shouldBe` accXPubDerived

        aKey <- getAccountKeyShared ctx wal (Just Extended)

        verify aKey
            [ expectResponseCode HTTP.status200
            , expectField #format (`shouldBe` Extended)
            ]
        let ApiAccountKeyShared bytes' _ _ = getFromResponse Prelude.id aKey
        bech32Text acctHrp bytes' `shouldBe` accXPubDerived

        let (ApiSharedWallet (Right walActive)) = wal

        rDel <- request @ApiActiveSharedWallet ctx
                (Link.deleteWallet @'Shared walActive) Default Empty
        expectResponseCode HTTP.status204 rDel

        (_, accXPubTxt):_ <- liftIO $ genXPubsBech32 1
        let payloadAcctOther = Json [json| {
                "name": "Shared Wallet",
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
        rPostAcctOther <- postSharedWallet ctx Default payloadAcctOther
        verify (fmap (view #wallet) <$> rPostAcctOther)
            [ expectResponseCode HTTP.status201 ]
        let walAcctOther = getFromResponse Prelude.id rPostAcctOther
        let (ApiSharedWallet (Right walAcctOtherActive)) = walAcctOther

        (walAcctOtherActive ^. #id)  `shouldNotBe` (walActive ^. #id)

        let payloadAcctSame = Json [json| {
                "name": "Shared Wallet",
                "account_public_key": #{accXPubDerived},
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
        rPostAcctSame <- postSharedWallet ctx Default payloadAcctSame
        verify (fmap (view #wallet) <$> rPostAcctSame)
            [ expectResponseCode HTTP.status201 ]
        let walAcctSame = getFromResponse Prelude.id rPostAcctSame
        let (ApiSharedWallet (Right walAcctSameActive)) = walAcctSame

        (walAcctSameActive ^. #id)  `shouldBe` (walActive ^. #id)

        let payloadAcctSameOtherScript = Json [json| {
                "name": "Shared Wallet",
                "account_public_key": #{accXPubDerived},
                "account_index": "30H",
                "payment_script_template":
                    { "cosigners":
                        { "cosigner#0": #{accXPubDerived} },
                      "template":
                          { "all":
                             [ "cosigner#0",
                               { "active_from": 100 }
                             ]
                          }
                    }
                } |]
        rPostAcctSameOtherScript <-
            postSharedWallet ctx Default payloadAcctSameOtherScript
        verify (fmap (view #wallet) <$> rPostAcctSameOtherScript)
            [ expectResponseCode HTTP.status201 ]
        let walAcctSameOtherScript = getFromResponse Prelude.id rPostAcctSameOtherScript
        let (ApiSharedWallet (Right walAcctSameOtherScriptActive)) =
                walAcctSameOtherScript

        (walAcctSameOtherScriptActive ^. #id)  `shouldNotBe` (walActive ^. #id)

       -- In cardano-addresses
       -- \$ cat phrase.prv
       -- rib kiwi begin other second pool raise prosper inspire forum keep stereo option ride region
       --
       -- \$ cardano-address key from-recovery-phrase Shared < phrase.prv > root.shared_xsk
       --
       -- \$ cardano-address key child 1854H/1815H/0H < root.shared_xsk > acct.shared_xsk
       --
       -- \$ cardano-address key walletid --spending "all [cosigner#0]" < acct.shared_xsk
       -- 654a69cd246ab08aeb4d44837ff5d5ceddfbce20

    it "SHARED_WALLETS_CREATE_01 - \
        \golden test comparing wallet id" $
        \ctx -> runResourceT $ do

        let payloadPost = Json [json| {
                "name": "Shared Wallet",
                "mnemonic_sentence": ["rib","kiwi","begin","other","second","pool","raise","prosper","inspire","forum","keep","stereo","option","ride","region"],
                "passphrase": #{fixturePassphrase},
                "account_index": "0H",
                "payment_script_template":
                    { "cosigners":
                        { "cosigner#0": "self" },
                      "template":
                          { "all": ["cosigner#0"]
                          }
                    }
                } |]
        rPost <- postSharedWallet ctx Default payloadPost
        verify (fmap (view #wallet) <$> rPost)
            [ expectResponseCode HTTP.status201
            ]

        let wal = getFromResponse Prelude.id rPost
        let (ApiSharedWallet (Right walActive)) = wal

        toText (getApiT $ walActive ^. #id)  `shouldBe`
            "654a69cd246ab08aeb4d44837ff5d5ceddfbce20"

    it "SHARED_WALLETS_CREATE_02 - \
        \Create a pending shared wallet from root xprv" $
        \ctx -> runResourceT $ do

        m15txt <- liftIO $ genMnemonics M15
        m12txt <- liftIO $ genMnemonics M12
        let (Right m15) = mkSomeMnemonic @'[ 15 ] m15txt
        let (Right m12) = mkSomeMnemonic @'[ 12 ] m12txt
        let passphrase = Passphrase $ BA.convert $
                T.encodeUtf8 fixturePassphrase
        let index = 30
        let accXPubDerived =
                sharedAccPubKeyFromMnemonics m15 (Just m12) index passphrase
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
                (`shouldBe` DerivationIndex 2_147_483_678)
            ]

        let wal = getFromResponse Prelude.id rPost

        let payloadKey = Json [json|{
                "passphrase": #{fixturePassphrase},
                "format": "extended"
            }|]
        rKey <-
            postAccountKeyShared ctx wal
                (DerivationIndex $ 2_147_483_648 + index) Default payloadKey

        verify rKey
            [ expectResponseCode HTTP.status202
            , expectField #format (`shouldBe` Extended)
            ]
        let ApiAccountKeyShared bytes _ _ = getFromResponse Prelude.id rKey
        bech32Text acctHrp bytes `shouldBe` accXPubDerived

    it "SHARED_WALLETS_CREATE_03 - \
        \Create an active shared wallet from account xpub" $
        \ctx -> runResourceT $ do

        (_, accXPubTxt):_ <- liftIO $ genXPubsBech32 1
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
                (`shouldBe` DerivationIndex 2_147_483_658)
            ]

        let wal = getFromResponse Prelude.id rPost
        aKey <- getAccountKeyShared ctx wal (Just Extended)

        verify aKey
            [ expectResponseCode HTTP.status200
            , expectField #format (`shouldBe` Extended)
            ]
        let ApiAccountKeyShared bytes' _ _ = getFromResponse Prelude.id aKey
        bech32Text acctHrp bytes' `shouldBe` accXPubTxt

    it "SHARED_WALLETS_CREATE_04 - \
        \Create a pending shared wallet from account xpub" $
        \ctx -> runResourceT $ do

        (_, accXPubTxt):_ <- liftIO $ genXPubsBech32 1
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
                (`shouldBe` DerivationIndex 2_147_483_658)
            ]

    it "SHARED_WALLETS_CREATE_05 - \
        \Create an active shared wallet from root xprv with self" $
        \ctx -> runResourceT $ do

        m15txt <- liftIO $ genMnemonics M15
        m12txt <- liftIO $ genMnemonics M12
        let (Right m15) = mkSomeMnemonic @'[ 15 ] m15txt
        let (Right m12) = mkSomeMnemonic @'[ 12 ] m12txt
        let passphrase = Passphrase $ BA.convert $
                T.encodeUtf8 fixturePassphrase
        let index = 30
        let accXPubDerived =
                sharedAccPubKeyFromMnemonics m15 (Just m12) index passphrase
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

        let wal = getFromResponse Prelude.id rPost

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

        let walWithSelf@(ApiSharedWallet (Right activeWal)) =
                getFromResponse Prelude.id rPostWithSelf

        getWalletIdFromSharedWallet walWithSelf
            `shouldBe`
            getWalletIdFromSharedWallet wal

        rAddrsActive <- request @[ApiAddressWithPath n] ctx
            (Link.listAddresses @'Shared activeWal) Default Empty
        expectResponseCode HTTP.status200 rAddrsActive
        let g = fromIntegral $ getAddressPoolGap defaultAddressPoolGap
        expectListSize g rAddrsActive
        forM_ [0..(g-1)] $ \addrNum -> do
            expectListField
                addrNum (#state . #getApiT) (`shouldBe` Unused) rAddrsActive

    it "SHARED_WALLETS_CREATE_06 - \
        \Create an active shared wallet from account xpub with self" $
        \ctx -> runResourceT $ do

        (_, accXPubTxt):_ <- liftIO $ genXPubsBech32 1
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

        let wal = getFromResponse Prelude.id rPost

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

        let walWithSelf@(ApiSharedWallet (Right activeWal)) =
                getFromResponse Prelude.id rPostWithSelf

        getWalletIdFromSharedWallet walWithSelf
            `shouldBe` getWalletIdFromSharedWallet wal

        rAddrsActive <- request @[ApiAddressWithPath n] ctx
            (Link.listAddresses @'Shared activeWal) Default Empty
        expectResponseCode HTTP.status200 rAddrsActive
        let g = fromIntegral $ getAddressPoolGap defaultAddressPoolGap
        expectListSize g rAddrsActive
        forM_ [0..(g-1)] $ \addrNum -> do
            expectListField
                addrNum (#state . #getApiT) (`shouldBe` Unused) rAddrsActive

    it "SHARED_WALLETS_CREATE_07 - \
        \Incorrect script template due to NoCosignerInScript" $
        \ctx -> runResourceT $ do

        (_, accXPubTxt):_ <- liftIO $ genXPubsBech32 1
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

    it "SHARED_WALLETS_CREATE_08 - \
        \Incorrect script template due to UnknownCosigner" $
        \ctx -> runResourceT $ do

        (_, accXPubTxt):_ <- liftIO $ genXPubsBech32 1
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

    it "SHARED_WALLETS_CREATE_09 - \
        \Incorrect script template due to DuplicateXPub" $
        \ctx -> runResourceT $ do

        (_, accXPubTxt):_ <- liftIO $ genXPubsBech32 1
        let payload = Json [json| {
                "name": "Shared Wallet",
                "account_public_key": #{accXPubTxt},
                "account_index": "10H",
                "payment_script_template":
                    { "cosigners":
                        { "cosigner#0": #{accXPubTxt}
                        , "cosigner#1": #{accXPubTxt}
                        },
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

    it "SHARED_WALLETS_CREATE_10 - \
        \Incorrect script template due to WrongScript \
        \when recommended validation" $
        \ctx -> runResourceT $ do

        (_, accXPubTxt):_ <- liftIO $ genXPubsBech32 1
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
        let reason =
                "The list inside a script has duplicate keys \
                \(which is not recommended)."
        expectErrorMessage (errMsg403TemplateInvalidScript reason) rPost

    it "SHARED_WALLETS_CREATE_11 - \
        \Correct script template when required validation" $
        \ctx -> runResourceT $ do

        (_, accXPubTxt):_ <- liftIO $ genXPubsBech32 1
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

    it "SHARED_WALLETS_CREATE_12 - \
        \Incorrect script template due to WrongScript - timelocks" $
        \ctx -> runResourceT $ do

        (_, accXPubTxt):_ <- liftIO $ genXPubsBech32 1
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
        let reason =
                "The timelocks used are contradictory when used with 'all' \
                \(which is not recommended)."
        expectErrorMessage (errMsg403TemplateInvalidScript reason) rPost

    it "SHARED_WALLETS_CREATE_13 - \
        \Incorrect account index" $
        \ctx -> runResourceT $ do

        [(_, accXPubTxt0)] <- liftIO $ genXPubsBech32 1
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

    it "SHARED_WALLETS_CREATE_14 - Create wallet with one change address mode on" $ \ctx -> runResourceT $ do
        let verifyAddrs nTotal nUsed addrs = do
                liftIO (length addrs `shouldBe` nTotal)
                let onlyUsed = filter ((== Used) . (^. (#state . #getApiT))) addrs
                liftIO (length onlyUsed `shouldBe` nUsed)

        let listAddresses wal = do
                rAddr <- request @[ApiAddressWithPath n] ctx
                    (Link.listAddresses @'Shared wal) Default Empty
                expectResponseCode HTTP.status200 rAddr
                pure $ getResponse rAddr

        m15txt <- liftIO $ genMnemonics M15
        m12txt <- liftIO $ genMnemonics M12
        let (Right m15) = mkSomeMnemonic @'[ 15 ] m15txt
        let (Right m12) = mkSomeMnemonic @'[ 12 ] m12txt
        let passphrase = Passphrase $
                BA.convert $ T.encodeUtf8 fixturePassphrase
        let index = 30
        let accXPubDerived =
                sharedAccPubKeyFromMnemonics m15 (Just m12) index passphrase
        let payloadPost = Json [json| {
                "name": "Shared Wallet",
                "mnemonic_sentence": #{m15txt},
                "mnemonic_second_factor": #{m12txt},
                "passphrase": #{fixturePassphrase},
                "account_index": "30H",
                "one_change_address_mode": true,
                "payment_script_template":
                    { "cosigners":
                        { "cosigner#0": #{accXPubDerived} },
                      "template":
                          { "all":
                             [ "cosigner#0" ]
                          }
                    }
                } |]
        rPost <- postSharedWallet ctx Default payloadPost
        verify (fmap (swapEither . view #wallet) <$> rPost)
            [ expectResponseCode HTTP.status201
            ]
        let (ApiSharedWallet (Right walOneAddr)) =
                getFromResponse Prelude.id rPost

        -- new empty wallet has 20 unused external addresses and 0 used change addresses
        let initialTotal1 = 20
        let initialUsed1  = 0
        listAddresses walOneAddr
            >>= verifyAddrs initialTotal1 initialUsed1

        wFixture <- fixtureSharedWallet @n ctx
        -- new fixture wallet has 21 unused external addresses, 1 used external addresses
        -- (second one was used), and 0 used change addresses
        let initialTotal2 = 22
        let initialUsed2  = 1
        listAddresses wFixture
            >>= verifyAddrs initialTotal2 initialUsed2

        --send funds to one change address wallet
        let minUTxOValue' = minUTxOValue (_mainEra ctx)
        addrs1 <- listAddresses walOneAddr
        let destOneChange = (head addrs1) ^. #id
        let payloadTx amt destination = Json [json|{
                "payments": [{
                    "address": #{destination},
                    "amount": {
                        "quantity": #{amt},
                        "unit": "lovelace"
                    }
                }]
            }|]
        let realizeTx wSrc wDest amt dest = do
                rTx <- request @(ApiConstructTransaction n) ctx
                    (Link.createUnsignedTransaction @'Shared wSrc) Default
                    (payloadTx amt dest)
                verify rTx
                    [ expectResponseCode HTTP.status202
                    ]
                let (ApiSerialisedTransaction apiTx _) =
                        getFromResponse #transaction rTx
                signedTx <-
                    signSharedTx ctx wSrc apiTx
                        [ expectResponseCode HTTP.status202 ]
                submittedTx <- submitSharedTxWithWid ctx wSrc signedTx
                verify submittedTx
                    [ expectResponseCode HTTP.status202
                    ]
                let txid = getFromResponse #id submittedTx

                eventually "Transaction is discovered" $ do
                    request @(ApiTransaction n) ctx
                        (Link.getTransaction @'Shared wDest (ApiTxId txid)) Default Empty
                        >>= expectField #status (`shouldBe` ApiT InLedger)
                    request @(ApiTransaction n) ctx
                        (Link.getTransaction @'Shared wSrc (ApiTxId txid)) Default Empty
                        >>= expectField #status (`shouldBe` ApiT InLedger)
        forM_ [10, 10, 10] $ \num -> realizeTx wFixture walOneAddr (num * minUTxOValue') destOneChange

        -- new fixture wallet has 21 unused external addresses, 1 used external addresses
        -- (second one was used), and 3 used change addresses as there were three txs sent and each tx used new change
        -- address
        listAddresses wFixture
            >>= verifyAddrs (initialTotal2+3) (initialUsed2+3)
        -- the previously empty wallet has 20 unused external addresses and 0 used change addresses
        -- and 1 used external address as three txs choose the same address as destination address
        listAddresses walOneAddr
            >>= verifyAddrs (initialTotal1+1) (initialUsed1+1)

        addrs2 <- listAddresses wFixture
        let destFixture = (head addrs2) ^. #id
        forM_ [1,1,1,1,1] $ \num -> realizeTx walOneAddr wFixture (num * minUTxOValue') destFixture

        -- the fixture wallet has still 20 unused external addresses, 2 used external addresses (first and second),
        -- and 3 used change addresses as five txs sent to it used its first, already used,
        -- address. As initialTotal2 = 22 and initialUsed2  = 1
        listAddresses wFixture
            >>= verifyAddrs (initialTotal2+3) (initialUsed2+4)
        -- the one change address wallet has 20 unused external addresses and 1 used change addresses
        -- and 1 used external address even as it sent 5 txs outside
        listAddresses walOneAddr
            >>= verifyAddrs (initialTotal1+2) (initialUsed1+2)

        --let's switch off one change address mode
        let putData = Json [json| {
                "one_change_address_mode": false
                } |]
        let walIdOneChangeAddr = walOneAddr ^. walletId
        rPut <- request @ApiWallet ctx
            ("PUT", "v2/shared-wallets" </> walIdOneChangeAddr) Default putData
        verify rPut
            [ expectResponseCode HTTP.status200
            , expectField
                    (#addressPoolGap . #getApiT . #getAddressPoolGap)
                    (`shouldBe` 20)
            , expectField walletId (`shouldBe` walIdOneChangeAddr)
            ]


    it "SHARED_WALLETS_DELETE_01 - \
        \Delete of a shared wallet" $
        \ctx -> runResourceT $ do

        let walName = "Shared Wallet" :: Text
        (_, payload) <- getAccountWallet walName
        rInit <- postSharedWallet ctx Default payload
        verify rInit
            [ expectResponseCode HTTP.status201 ]
        let wal = getFromResponse Prelude.id rInit

        eventually "Wallet state = Ready" $ do
            r <- getSharedWallet ctx wal
            expectField
                (traverse . #state . #getApiT)
                (`shouldBe` Ready)
                (fmap (view #wallet) <$> r)

        rDel <- deleteSharedWallet ctx wal
        expectResponseCode HTTP.status204 rDel

    it "SHARED_WALLETS_PATCH_01 - \
        \Add cosigner key in a pending shared wallet and transit it to the \
        \active shared wallet" $
        \ctx -> runResourceT $ do

        [(accXPub0, accXPubTxt0),(accXPub1,accXPubTxt1)] <-
            liftIO $ genXPubsBech32 2

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
        let wal@(ApiSharedWallet (Left pendingWal)) = getFromResponse Prelude.id rPost
        let (ApiScriptTemplate cosignerKeysPost) =
                pendingWal ^. #paymentScriptTemplate
        liftIO $ cosigners cosignerKeysPost
            `shouldBe` Map.fromList [(Cosigner 0,accXPub0)]

        rAddrsPending <- request @[ApiAddressWithPath n] ctx
            (Link.listAddresses @'Shared pendingWal) Default Empty
        expectResponseCode HTTP.status200 rAddrsPending
        expectListSize 0 rAddrsPending

        let payloadPatch = Json [json| {
                "cosigner#1": #{accXPubTxt1}
                } |]

        rPatch <- patchSharedWallet ctx wal Payment payloadPatch
        expectResponseCode HTTP.status200 rPatch
        let (ApiSharedWallet (Right activeWal)) = getFromResponse Prelude.id rPatch
        let (ApiScriptTemplate cosignerKeysPatch) =
                activeWal ^. #paymentScriptTemplate
        liftIO $ cosigners cosignerKeysPatch
            `shouldBe` Map.fromList
                [ (Cosigner 0, accXPub0)
                , (Cosigner 1, accXPub1)
                ]

        rAddrsActive <- request @[ApiAddressWithPath n] ctx
            (Link.listAddresses @'Shared activeWal) Default Empty
        expectResponseCode HTTP.status200 rAddrsActive
        let g = fromIntegral $ getAddressPoolGap defaultAddressPoolGap
        expectListSize g rAddrsActive
        forM_ [0..(g-1)] $ \addrNum -> do
            expectListField addrNum (#state . #getApiT)
                (`shouldBe` Unused) rAddrsActive

    it "SHARED_WALLETS_PATCH_02 - \
        \Add cosigner for delegation script template" $
        \ctx -> runResourceT $ do

        [(accXPub0, accXPubTxt0),(accXPub1,accXPubTxt1)] <-
            liftIO $ genXPubsBech32 2

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
        let wal@(ApiSharedWallet (Left pendingWal)) = getFromResponse Prelude.id rPost
        let (ApiScriptTemplate cosignerKeysPostInPayment) =
                pendingWal ^. #paymentScriptTemplate
        liftIO $ cosigners cosignerKeysPostInPayment
            `shouldBe` Map.fromList [(Cosigner 0,accXPub0)]
        let (Just (ApiScriptTemplate cosignerKeysPostInDelegation)) =
                pendingWal ^. #delegationScriptTemplate
        liftIO $ cosigners cosignerKeysPostInDelegation
            `shouldBe` Map.fromList [(Cosigner 0,accXPub0)]

        let payloadPatch = Json [json| {
                "cosigner#1": #{accXPubTxt1}
                } |]

        rPatch <- patchSharedWallet ctx wal Delegation payloadPatch
        expectResponseCode HTTP.status200 rPatch
        let (ApiSharedWallet (Right activeWal)) = getFromResponse Prelude.id rPatch
        let (Just (ApiScriptTemplate cosignerKeysPatch)) =
                activeWal ^. #delegationScriptTemplate
        liftIO $ cosigners cosignerKeysPatch
            `shouldBe`
                Map.fromList
                    [ (Cosigner 0, accXPub0)
                    , (Cosigner 1, accXPub1)
                    ]

    it "SHARED_WALLETS_PATCH_03 - \
        \Cannot add cosigner key in an active shared wallet" $
        \ctx -> runResourceT $ do

        [(_, accXPubTxt0),(_,accXPubTxt1)] <- liftIO $ genXPubsBech32 2
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
        let wal@(ApiSharedWallet (Right _activeWal)) = getFromResponse Prelude.id rPost

        let payloadPatch = Json [json| {
                "cosigner#1": #{accXPubTxt1}
                } |]

        rPatch <- patchSharedWallet ctx wal Payment payloadPatch
        expectResponseCode HTTP.status403 rPatch
        decodeErrorInfo rPatch `shouldBe` SharedWalletActive

    it "SHARED_WALLETS_PATCH_04 - \
        \Cannot add cosigner key when delegation script missing and cannot \
        \add already existent key to other cosigner" $
        \ctx -> runResourceT $ do

        [(_, accXPubTxt0),(_,accXPubTxt1)] <- liftIO $ genXPubsBech32 2
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
        let wal@(ApiSharedWallet (Left _pendingWal)) = getFromResponse Prelude.id rPost

        let payloadPatch = Json [json| {
                "cosigner#1": #{accXPubTxt1}
                } |]

        rPatch <- patchSharedWallet ctx wal Delegation payloadPatch
        expectResponseCode HTTP.status403 rPatch
        decodeErrorInfo rPatch `shouldBe` SharedWalletNoDelegationTemplate

    it "SHARED_WALLETS_PATCH_05 - \
        \Cannot create shared wallet when missing wallet's account public \
        \key in template" $
        \ctx -> runResourceT $ do

        [(_, accXPubTxt0),(_,accXPubTxt1)] <- liftIO $ genXPubsBech32 2
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

    it "SHARED_WALLETS_PATCH_06 - \
        \Can add the same cosigner key for delegation script template but not \
        \payment one" $
        \ctx -> runResourceT $ do

        [(_, accXPubTxt0),(_,accXPubTxt1)] <- liftIO $ genXPubsBech32 2
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
        let wal@(ApiSharedWallet (Left _pendingWal)) = getFromResponse Prelude.id rPost

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
        decodeErrorInfo rPatch2 `shouldBe` SharedWalletCannotUpdateKey

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
        expectErrorMessage
            (errMsg403KeyAlreadyPresent (toText Delegation)) rPatch4

        let payloadPatch5 = Json [json| {
                "cosigner#7": #{accXPubTxt0}
                } |]
        rPatch5 <- patchSharedWallet ctx wal Payment payloadPatch5
        expectResponseCode HTTP.status403 rPatch5
        decodeErrorInfo rPatch5 `shouldBe` SharedWalletNoSuchCosigner
            ApiErrorSharedWalletNoSuchCosigner
                { cosignerIndex = ApiCosignerIndex 7
                , credentialType = ApiCredentialType Payment
                }

    it "SHARED_WALLETS_PATCH_07 - \
        \Cannot update cosigner key in a pending shared wallet having the \
        \shared wallet's account key" $
        \ctx -> runResourceT $ do

        [(_, accXPubTxt0),(_,accXPubTxt1)] <- liftIO $ genXPubsBech32 2
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
        let wal@(ApiSharedWallet (Left _pendingWal)) = getFromResponse Prelude.id rPost

        let payloadPatch = Json [json| {
                "cosigner#0": #{accXPubTxt1}
                } |]
        rPatch <- patchSharedWallet ctx wal Payment payloadPatch
        expectResponseCode HTTP.status403 rPatch
        decodeErrorInfo rPatch `shouldBe` SharedWalletCannotUpdateKey

    it "SHARED_WALLETS_KEYS_01 - \
        \Getting verification keys works for active shared wallet" $
        \ctx -> runResourceT $ do

        let walName = "Shared Wallet" :: Text
        (_, payload) <- getAccountWallet walName
        rPost <- postSharedWallet ctx Default payload
        verify rPost
            [ expectResponseCode HTTP.status201
            ]
        let wal@(ApiSharedWallet (Right _activeWal)) = getFromResponse Prelude.id rPost

        (_, Right paymentKey) <-
            getSharedWalletKey ctx wal
                UtxoExternal (DerivationIndex 30) Nothing
        (_, Right stakeKey) <-
            getSharedWalletKey ctx wal
                MutableAccount (DerivationIndex 0) Nothing

        let (String paymentAddr) = toJSON paymentKey
        T.isPrefixOf "addr_shared_vk" paymentAddr `shouldBe` True

        let (String stakeAddr) = toJSON stakeKey
        T.isPrefixOf "stake_shared_vk" stakeAddr `shouldBe` True

    it "SHARED_WALLETS_KEYS_02 - \
        \Getting verification keys works for pending shared wallet" $
        \ctx -> runResourceT $ do

        (_, accXPubTxt):_ <- liftIO $ genXPubsBech32 1
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
        let wal@(ApiSharedWallet (Left _pendingWal)) = getFromResponse Prelude.id rPost

        (_, Right paymentKey) <-
            getSharedWalletKey ctx wal
                UtxoExternal (DerivationIndex 10) Nothing
        (_, Right stakeKey) <-
            getSharedWalletKey ctx wal
                MutableAccount (DerivationIndex 0) Nothing

        let (String paymentAddr) = toJSON paymentKey
        T.isPrefixOf "addr_shared_vk" paymentAddr `shouldBe` True

        let (String stakeAddr) = toJSON stakeKey
        T.isPrefixOf "stake_shared_vk" stakeAddr `shouldBe` True

        (_, Right paymentKey') <-
            getSharedWalletKey ctx wal
                UtxoExternal (DerivationIndex 10) (Just False)
        (_, Right stakeKey') <-
            getSharedWalletKey ctx wal
                MutableAccount (DerivationIndex 0) (Just False)

        paymentKey' `shouldBe` paymentKey
        stakeKey' `shouldBe` stakeKey

        (_, Right paymentKeyH) <-
            getSharedWalletKey ctx wal
                UtxoExternal (DerivationIndex 10) (Just True)
        (_, Right stakeKeyH) <-
            getSharedWalletKey ctx wal
                MutableAccount (DerivationIndex 0) (Just True)

        let (String paymentAddrH) = toJSON paymentKeyH
        T.isPrefixOf "addr_shared_vkh" paymentAddrH `shouldBe` True

        let (String stakeAddrH) = toJSON stakeKeyH
        T.isPrefixOf "stake_shared_vkh" stakeAddrH `shouldBe` True

    it "SHARED_WALLETS_LIST_01 - \
        \Created a wallet can be listed" $
        \ctx -> runResourceT $ do

        let walName = "Shared Wallet" :: Text
        (_, payload) <- getAccountWallet walName
        rPost <- postSharedWallet ctx Default payload
        verify rPost
            [ expectResponseCode HTTP.status201
            ]
        let wal = getFromResponse Prelude.id rPost

        rl <- listFilteredSharedWallets
            (Set.singleton (getWalletIdFromSharedWallet wal ^. walletId) ) ctx
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
            , expectListField 0 (traverse . #balance . #total)
                (`shouldBe` Quantity 0)
            , expectListField 0 (traverse . #balance . #reward)
                (`shouldBe` Quantity 0)
            , expectListField 0 (traverse . #assets . #total)
                (`shouldBe` mempty)
            , expectListField 0 (traverse . #assets . #available)
                (`shouldBe` mempty)
            , expectListField 0 (traverse . #delegation)
                (`shouldBe` notDelegating [])
            , expectListField 0 (traverse . #delegationScriptTemplate)
                (`shouldBe` Nothing)
            , expectListField 0 (traverse . #accountIndex . #getApiT)
                (`shouldBe` DerivationIndex 2_147_483_678)
            ]

    it "SHARED_WALLETS_LIST_01 - \
        \Wallets are listed from oldest to newest" $
        \ctx -> runResourceT $ do

        let walNames = ["1", "2", "3"] :: [Text]
        wids <- forM walNames $ \walName -> do
            (_, payload) <- getAccountWallet walName
            rPost <- postSharedWallet ctx Default payload
            verify rPost
                [ expectResponseCode HTTP.status201
                ]
            let wal = getFromResponse Prelude.id rPost
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

    it "SHARED_WALLETS_LIST_02 - \
        \Deleted wallet not listed" $
        \ctx -> runResourceT $ do

        let walName = "Shared Wallet" :: Text
        (_, payload) <- getAccountWallet walName
        rPost <- postSharedWallet ctx Default payload
        verify rPost
            [ expectResponseCode HTTP.status201
            ]
        let wal = getFromResponse Prelude.id rPost

        rDel <- deleteSharedWallet ctx wal
        expectResponseCode HTTP.status204 rDel

        rl <- listFilteredSharedWallets
            (Set.singleton $ getWalletIdFromSharedWallet wal ^. walletId) ctx
        verify rl
            [ expectResponseCode HTTP.status200
            , expectListSize 0
            ]

    it "SHARED_WALLETS_DISCOVER_01 - \
        \Shared wallets can discover its address" $ \ctx -> runResourceT $ do
        (_, accXPubTxt):_ <- liftIO $ genXPubsBech32 1
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
            , expectField (traverse . #balance . #available)
                (`shouldBe` Quantity 0)
            ]
        let walShared@(ApiSharedWallet (Right wal)) = getFromResponse Prelude.id rPost

        rAddr <- request @[ApiAddressWithPath n] ctx
            (Link.listAddresses @'Shared wal) Default Empty
        expectResponseCode HTTP.status200 rAddr
        let sharedAddrs = getFromResponse Prelude.id rAddr
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
        (_, ApiFee (Quantity feeMin) (Quantity feeMax) _ _) <- unsafeRequest ctx
            (Link.getTransactionFeeOld @'Shelley wShelley) payloadTx
        let ep = Link.createTransactionOld @'Shelley
        rTx <- request @(ApiTransaction n) ctx (ep wShelley) Default payloadTx
        expectResponseCode HTTP.status202 rTx

        -- TODO Drop expectation https://cardanofoundation.atlassian.net/browse/ADP-2935
        expectField
            (#fee . #getQuantity)
            (between (feeMin, feeMax))
            rTx
        let Quantity fee = getFromResponse #fee rTx
        eventually "wShelley balance is decreased" $ do
            ra <- request @ApiWallet ctx
                (Link.getWallet @'Shelley wShelley) Default Empty
            expectField
                (#balance . #available)
                (`shouldBe` Quantity (faucetAmt - fee - amt)) ra

        rWal <- getSharedWallet ctx walShared
        verify (fmap (view #wallet) <$> rWal)
            [ expectResponseCode HTTP.status200
            , expectField (traverse . #balance . #available)
                (`shouldBe` Quantity amt)
            ]

    it "SHARED_WALLETS_UTXO_01 - \
       \Wallet's inactivity is reflected in utxo"$ \ctx -> runResourceT $ do
        (ApiSharedWallet (Right w)) <- emptySharedWallet ctx
        rStat <- request @ApiUtxoStatistics ctx
                 (Link.getUTxOsStatistics @'Shared w) Default Empty
        expectResponseCode HTTP.status200 rStat
        expectWalletUTxO [] (snd rStat)

    it "SHARED_WALLETS_UTXO_02 - Sending and receiving funds updates \
       \wallet's utxo." $ \ctx -> runResourceT $ do
        wSrc <- fixtureWallet ctx
        (ApiSharedWallet (Right wDest)) <- emptySharedWallet ctx

        --send funds
        rAddr <- request @[ApiAddressWithPath n] ctx
            (Link.listAddresses @'Shared wDest) Default Empty
        expectResponseCode HTTP.status200 rAddr
        let addrs = getFromResponse Prelude.id rAddr
        let destination = (addrs !! 1) ^. #id
        let coins :: [Natural]
            coins =
                [13_000_000, 43_000_000, 66_000_000, 101_000_000, 1339_000_000]
        let payments = flip map coins $ \c -> [json|{
                "address": #{destination},
                "amount": {
                    "quantity": #{c},
                    "unit": "lovelace"
                }}|]
        let payload = [json|{
                "payments": #{payments},
                "passphrase": "cardano-wallet"
                }|]

        rTrans <- request @(ApiTransaction n) ctx
            (Link.createTransactionOld @'Shelley wSrc) Default (Json payload)
        expectResponseCode HTTP.status202 rTrans

        eventually "Wallet balance is as expected" $ do
            wal <- getSharedWallet ctx (ApiSharedWallet (Right wDest))
            let balanceExp =
                    [ expectResponseCode HTTP.status200
                    , expectField (traverse . #balance . #available)
                        (`shouldBe` Quantity (fromIntegral $ sum coins))
                    ]
            verify (fmap (view #wallet) <$> wal) balanceExp

        --verify utxo
        rStat1 <- request @ApiUtxoStatistics ctx
            (Link.getUTxOsStatistics @'Shared wDest) Default Empty
        expectResponseCode HTTP.status200 rStat1
        expectWalletUTxO coins (snd rStat1)

    it "SHARED_WALLETS_UTXO_03 - Deleted wallet is not available \
       \for utxo" $ \ctx -> runResourceT $ do
        (ApiSharedWallet (Right w)) <- emptySharedWallet ctx
        _ <- request @ApiWallet ctx (Link.deleteWallet @'Shared w)
            Default Empty
        r <- request @ApiUtxoStatistics ctx (Link.getUTxOsStatistics @'Shared w)
            Default Empty
        expectResponseCode HTTP.status404 r
        expectErrorMessage (errMsg404NoWallet $ w ^. walletId) r

    describe "SHARED_WALLETS_UTXO_04 - HTTP headers" $ do
        let matrix =
                [ ( "No HTTP headers -> 200"
                  , None
                  , [ expectResponseCode HTTP.status200 ] )
                , ( "Accept: text/plain -> 406"
                  , Headers
                        [ ("Content-Type", "application/json")
                        , ("Accept", "text/plain") ]
                  , [ expectResponseCode HTTP.status406
                    , expectErrorMessage errMsg406 ]
                  )
                , ( "No Accept -> 200"
                  , Headers [ ("Content-Type", "application/json") ]
                  , [ expectResponseCode HTTP.status200 ]
                  )
                , ( "No Content-Type -> 200"
                  , Headers [ ("Accept", "application/json") ]
                  , [ expectResponseCode HTTP.status200 ]
                  )
                , ( "Content-Type: text/plain -> 200"
                  , Headers [ ("Content-Type", "text/plain") ]
                  , [ expectResponseCode HTTP.status200 ]
                  )
                ]
        forM_ matrix $ \(title, headers, expectations) -> it title $ \ctx -> runResourceT $ do
            (ApiSharedWallet (Right w)) <- emptySharedWallet ctx
            r <- request @ApiUtxoStatistics ctx (Link.getUTxOsStatistics @'Shared w) headers Empty
            verify r expectations

    it "SHARED_WALLETS_UTXO_SNAPSHOT_01 - \
        \Can generate UTxO snapshot of empty wallet" $
        \ctx -> runResourceT $ do
            (ApiSharedWallet (Right w)) <- emptySharedWallet ctx
            rSnap <- request @ApiWalletUtxoSnapshot ctx
                (Link.getWalletUtxoSnapshot @'Shared w) Default Empty
            expectResponseCode HTTP.status200 rSnap
            expectField #entries (`shouldBe` []) rSnap

    it "SHARED_WALLETS_UTXO_SNAPSHOT_02 - \
        \Can generate UTxO snapshot of pure-ada wallet" $
        \ctx -> runResourceT $ do
            w <- fixtureSharedWallet @n ctx
            rSnap <- request @ApiWalletUtxoSnapshot ctx
                (Link.getWalletUtxoSnapshot @'Shared w) Default Empty
            expectResponseCode HTTP.status200 rSnap
            let entries = getFromResponse #entries rSnap
            length entries `shouldBe` 1

  where
     acctHrp = [Bech32.humanReadablePart|acct_shared_xvk|]
     getAccountWallet name = do
          (_, accXPubTxt):_ <- liftIO $ genXPubsBech32 1
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
