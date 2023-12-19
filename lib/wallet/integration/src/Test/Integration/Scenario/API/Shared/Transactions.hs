{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
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

module Test.Integration.Scenario.API.Shared.Transactions
    ( spec
    ) where

import Prelude

import Cardano.Mnemonic
    ( MkSomeMnemonic (..)
    )
import Cardano.Wallet.Address.Derivation
    ( DerivationIndex (..)
    , Index (..)
    )
import Cardano.Wallet.Address.Derivation.SharedKey
    ( replaceCosignersWithVerKeys
    )
import Cardano.Wallet.Address.Discovery.Shared
    ( CredentialType (..)
    )
import Cardano.Wallet.Api.Types
    ( ApiAddress (..)
    , ApiAddressWithPath (..)
    , ApiAnyCertificate (..)
    , ApiCertificate (..)
    , ApiConstructTransaction (..)
    , ApiDecodedTransaction (..)
    , ApiScriptTemplate (..)
    , ApiSerialisedTransaction (..)
    , ApiSharedWallet (..)
    , ApiT (..)
    , ApiTransaction
    , ApiTxId (..)
    , ApiTxInput (..)
    , ApiTxInputGeneral (..)
    , ApiTxMetadata (..)
    , ApiTxOutputGeneral (..)
    , ApiWallet
    , ApiWalletOutput (..)
    , Iso8601Time (..)
    , WalletStyle (..)
    , fromApiEra
    , insertedAt
    )
import Cardano.Wallet.Api.Types.Amount
    ( ApiAmount (ApiAmount)
    )
import Cardano.Wallet.Api.Types.Error
    ( ApiErrorInfo (..)
    )
import Cardano.Wallet.Api.Types.Transaction
    ( mkApiWitnessCount
    )
import Cardano.Wallet.Pools
    ( StakePool
    )
import Cardano.Wallet.Primitive.NetworkId
    ( HasSNetworkId
    )
import Cardano.Wallet.Primitive.Passphrase
    ( Passphrase (..)
    )
import Cardano.Wallet.Primitive.Types
    ( SortOrder (..)
    )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..)
    )
import Cardano.Wallet.Primitive.Types.Tx
    ( TxMetadata (..)
    , TxMetadataValue (..)
    , TxScriptValidity (..)
    , cardanoTxIdeallyNoLaterThan
    )
import Cardano.Wallet.Primitive.Types.Tx.TxMeta
    ( Direction (..)
    , TxStatus (..)
    )
import Cardano.Wallet.Transaction
    ( AnyExplicitScript (..)
    , ScriptReference (..)
    , WitnessCount (..)
    , changeRoleInAnyExplicitScript
    )
import Control.Monad
    ( forM_
    )
import Control.Monad.IO.Unlift
    ( MonadUnliftIO (..)
    , liftIO
    )
import Control.Monad.Trans.Resource
    ( runResourceT
    )
import Data.Aeson
    ( toJSON
    )
import Data.Either.Combinators
    ( swapEither
    )
import Data.Function
    ( (&)
    )
import Data.Generics.Internal.VL.Lens
    ( view
    , (^.)
    )
import Data.Generics.Wrapped
    ( _Unwrapped
    )
import Data.Maybe
    ( isJust
    )
import Data.Text.Class
    ( FromText (..)
    , ToText (..)
    )
import Data.Time.Clock
    ( UTCTime
    , addUTCTime
    )
import Data.Time.Utils
    ( utcTimePred
    , utcTimeSucc
    )
import Numeric.Natural
    ( Natural
    )
import Test.Hspec
    ( SpecWith
    , describe
    , pendingWith
    )
import Test.Hspec.Expectations.Lifted
    ( shouldBe
    , shouldContain
    , shouldNotContain
    , shouldSatisfy
    )
import Test.Hspec.Extra
    ( it
    )
import Test.Integration.Framework.DSL
    ( Context (..)
    , Headers (..)
    , MnemonicLength (..)
    , Payload (..)
    , arbitraryStake
    , decodeErrorInfo
    , delegating
    , deleteSharedWallet
    , emptySharedWallet
    , emptyWallet
    , eventually
    , expectErrorMessage
    , expectField
    , expectListField
    , expectListSize
    , expectResponseCode
    , expectSuccess
    , faucetUtxoAmt
    , fixturePassphrase
    , fixtureSharedWallet
    , fixtureSharedWalletDelegating
    , fixtureWallet
    , fundSharedWallet
    , genMnemonics
    , getFromResponse
    , getResponse
    , getSharedWallet
    , json
    , listAddresses
    , minUTxOValue
    , notDelegating
    , notRetiringPools
    , patchSharedWallet
    , postSharedWallet
    , postWallet
    , request
    , sharedAccPubKeyFromMnemonics
    , signSharedTx
    , submitSharedTxWithWid
    , toQueryString
    , unsafeGetTransactionTime
    , unsafeRequest
    , utcIso8601ToText
    , verify
    , waitForNextEpoch
    , waitNumberOfEpochBoundaries
    , walletId
    , (.>)
    )
import Test.Integration.Framework.Request
    ( RequestException
    )
import Test.Integration.Framework.TestData
    ( errMsg400MinWithdrawalWrong
    , errMsg400StartTimeLaterThanEndTime
    , errMsg403EmptyUTxO
    , errMsg403Fee
    , errMsg403InvalidConstructTx
    , errMsg403MinUTxOValue
    , errMsg403MissingWitsInTransaction
    , errMsg404CannotFindTx
    , errMsg404NoWallet
    )

import qualified Cardano.Address.Script as CA
import qualified Cardano.Address.Style.Shelley as CA
import qualified Cardano.Api as Cardano
import qualified Cardano.Wallet.Api.Link as Link
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Network.HTTP.Types as HTTP

data TestCase a = TestCase
    { query :: T.Text
    , assertions :: [(HTTP.Status, Either RequestException a) -> IO ()]
    }

spec
    :: forall n
     . HasSNetworkId n
    => SpecWith Context
spec = describe "SHARED_TRANSACTIONS" $ do

    it "SHARED_TRANSACTIONS_CREATE_01 - \
        \Cannot create tx for a pending shared wallet" $
        \ctx -> runResourceT $ do

        m15txt <- liftIO $ genMnemonics M15
        m12txt <- liftIO $ genMnemonics M12
        let (Right m15) = mkSomeMnemonic @'[ 15 ] m15txt
        let (Right m12) = mkSomeMnemonic @'[ 12 ] m12txt
        let passphrase =
                Passphrase $ BA.convert $ T.encodeUtf8 fixturePassphrase
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
            ]

        let (ApiSharedWallet (Left wal)) = getResponse rPost

        let metadata =
                Json [json|{ "metadata": { "1": { "string": "hello" } } }|]

        rTx <- request @(ApiConstructTransaction n) ctx
            (Link.createUnsignedTransaction @'Shared wal) Default metadata
        verify rTx
            [ expectResponseCode HTTP.status403
            ]
        decodeErrorInfo rTx `shouldBe` SharedWalletIncomplete

    it "SHARED_TRANSACTIONS_CREATE_01 - \
        \Can create tx for an active shared wallet, typed metadata" $
        \ctx -> runResourceT $ do

        m15txt <- liftIO $ genMnemonics M15
        m12txt <- liftIO $ genMnemonics M12
        let payload = Json [json| {
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
                             [ "cosigner#0" ]
                          }
                    }
                } |]
        rPost <- postSharedWallet ctx Default payload
        verify (fmap (swapEither . view #wallet) <$> rPost)
            [ expectResponseCode HTTP.status201
            ]

        let walShared@(ApiSharedWallet (Right wal)) =
                getResponse rPost

        let metadata =
                Json [json|{ "metadata": { "1": { "string": "hello" } } }|]

        rTx1 <- request @(ApiConstructTransaction n) ctx
            (Link.createUnsignedTransaction @'Shared wal) Default metadata
        verify rTx1
            [ expectResponseCode HTTP.status403
            , expectErrorMessage errMsg403EmptyUTxO
            ]

        let amt = 10 * minUTxOValue (_mainEra ctx)
        fundSharedWallet @n ctx amt (pure walShared)

        rTx2 <- request @(ApiConstructTransaction n) ctx
            (Link.createUnsignedTransaction @'Shared wal) Default metadata
        verify rTx2
            [ expectResponseCode HTTP.status202
            , expectField (#coinSelection . #metadata) (`shouldSatisfy` isJust)
            , expectField (#fee . #toNatural) (`shouldSatisfy` (>0))
            ]

        let txCbor1 = getFromResponse #transaction rTx2
        let decodePayload1 = Json (toJSON txCbor1)
        rDecodedTx1 <- request @(ApiDecodedTransaction n) ctx
            (Link.decodeTransaction @'Shared wal) Default decodePayload1
        let expectedFee = getFromResponse (#fee . #toNatural) rTx2
        let metadata' =
                ApiT (TxMetadata (Map.fromList [(1,TxMetaText "hello")]))
        let decodedExpectations =
                [ expectResponseCode HTTP.status202
                , expectField (#fee . #toNatural) (`shouldBe` expectedFee)
                , expectField #withdrawals (`shouldBe` [])
                , expectField #collateral (`shouldBe` [])
                , expectField #metadata
                  (`shouldBe` (ApiTxMetadata (Just metadata')))
                , expectField #scriptValidity
                    (`shouldBe` (Just $ ApiT TxScriptValid))
                ]
        verify rDecodedTx1 decodedExpectations

        let (ApiSerialisedTransaction apiTx _) =
                getFromResponse #transaction rTx2
        signedTx <-
            signSharedTx ctx wal apiTx [ expectResponseCode HTTP.status202 ]
        let decodePayload2 = Json (toJSON signedTx)
        rDecodedTx2 <- request @(ApiDecodedTransaction n) ctx
            (Link.decodeTransaction @'Shared wal) Default decodePayload2
        verify rDecodedTx2 decodedExpectations

        -- Submit tx
        submittedTx <- submitSharedTxWithWid ctx wal signedTx
        verify submittedTx
            [ expectSuccess
            , expectResponseCode HTTP.status202
            ]

        let txid = getFromResponse #id submittedTx
        let queryTx = Link.getTransaction @'Shared wal (ApiTxId txid)
        rGetTx <- request @(ApiTransaction n) ctx queryTx Default Empty
        verify rGetTx
            [ expectResponseCode HTTP.status200
            , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
            ]

        -- Make sure only fee is deducted from shared Wallet
        eventually "Wallet balance is as expected" $ do
            rWal <- getSharedWallet ctx walShared
            verify (fmap (view #wallet) <$> rWal)
                [ expectResponseCode HTTP.status200
                , expectField
                    (traverse . #balance . #available . #toNatural)
                    (`shouldBe` (amt - expectedFee))
                ]

        eventually "Tx is in ledger finally" $ do
            rGetTx' <- request @(ApiTransaction n) ctx queryTx Default Empty
            verify rGetTx'
                [ expectResponseCode HTTP.status200
                , expectField (#status . #getApiT) (`shouldBe` InLedger)
                ]
            let listTxEp = Link.listTransactions @'Shared wal
            request @[ApiTransaction n] ctx listTxEp Default Empty
                >>= flip verify
                [ expectListField 1
                    (#direction . #getApiT) (`shouldBe` Incoming)
                , expectListField 1
                    (#status . #getApiT) (`shouldBe` InLedger)
                ]

    it "SHARED_TRANSACTIONS_CREATE_01 - \
        \Can create tx for an active shared wallet, untyped metadata" $
        \ctx -> runResourceT $ do

        m15txt <- liftIO $ genMnemonics M15
        m12txt <- liftIO $ genMnemonics M12
        let payload = Json [json| {
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
                             [ "cosigner#0" ]
                          }
                    }
                } |]
        rPost <- postSharedWallet ctx Default payload
        verify (fmap (swapEither . view #wallet) <$> rPost)
            [ expectResponseCode HTTP.status201
            ]

        let walShared@(ApiSharedWallet (Right wal)) =
                getResponse rPost

        let metadata = Json [json|{ "metadata": { "1": "hello"  } }|]

        let amt = 10 * minUTxOValue (_mainEra ctx)
        fundSharedWallet @n ctx amt (pure walShared)

        rTx <- request @(ApiConstructTransaction n) ctx
            (Link.createUnsignedTransaction @'Shared wal) Default metadata
        verify rTx
            [ expectResponseCode HTTP.status202
            , expectField (#coinSelection . #metadata) (`shouldSatisfy` isJust)
            , expectField (#fee . #toNatural) (`shouldSatisfy` (>0))
            ]

        -- checking metadata before signing using decodeTransaction
        let txCbor1 = getFromResponse #transaction rTx
        let decodePayload1 = Json (toJSON txCbor1)
        rDecodedTx1 <- request @(ApiDecodedTransaction n) ctx
            (Link.decodeTransaction @'Shared wal) Default decodePayload1
        let expectedFee = getFromResponse (#fee . #toNatural) rTx
        let metadata' =
                ApiT (TxMetadata (Map.fromList [(1,TxMetaText "hello")]))
        let decodedExpectations =
                [ expectResponseCode HTTP.status202
                , expectField (#fee . #toNatural) (`shouldBe` expectedFee)
                , expectField #withdrawals (`shouldBe` [])
                , expectField #collateral (`shouldBe` [])
                , expectField #metadata
                    (`shouldBe` (ApiTxMetadata (Just metadata')))
                , expectField #scriptValidity
                    (`shouldBe` (Just $ ApiT TxScriptValid))
                ]
        verify rDecodedTx1 decodedExpectations

        -- checking metadata before signing via directly inspecting serialized
        -- tx
        let getMetadata (Cardano.InAnyCardanoEra _ tx) = Cardano.getTxBody tx &
                \(Cardano.TxBody bodyContent) ->
                    Cardano.txMetadata bodyContent & \case
                        Cardano.TxMetadataNone ->
                            Nothing
                        Cardano.TxMetadataInEra _ (Cardano.TxMetadata m) ->
                            Just m

        let era = fromApiEra $ _mainEra ctx
        let txbinary1 = cardanoTxIdeallyNoLaterThan era $
                getApiT (txCbor1 ^. #serialisedTxSealed)
        case getMetadata txbinary1 of
            Nothing -> error "Tx doesn't include metadata"
            Just m  -> case Map.lookup 1 m of
                Nothing -> error "Tx doesn't include metadata"
                Just (Cardano.TxMetaText "hello") -> pure ()
                Just _ -> error "Tx metadata incorrect"

        let (ApiSerialisedTransaction apiTx _) =
                getFromResponse #transaction rTx
        signedTx <-
            signSharedTx ctx wal apiTx [ expectResponseCode HTTP.status202 ]

        -- checking metadata after signing using decodeTransaction
        let decodePayload2 = Json (toJSON signedTx)
        rDecodedTx2 <- request @(ApiDecodedTransaction n) ctx
            (Link.decodeTransaction @'Shared wal) Default decodePayload2
        verify rDecodedTx2 decodedExpectations

        -- checking metadata after signing via directly inspecting serialized tx
        let txbinary2 = cardanoTxIdeallyNoLaterThan era $
                getApiT (signedTx ^. #serialisedTxSealed)
        case getMetadata txbinary2 of
            Nothing -> error "Tx doesn't include metadata"
            Just m  -> case Map.lookup 1 m of
                Nothing -> error "Tx doesn't include metadata"
                Just (Cardano.TxMetaText "hello") -> pure ()
                Just _ -> error "Tx metadata incorrect"

        -- Submit tx
        submittedTx <- submitSharedTxWithWid ctx wal signedTx
        verify submittedTx
            [ expectSuccess
            , expectResponseCode HTTP.status202
            ]

        let txid = getFromResponse #id submittedTx
        let queryTx = Link.getTransaction @'Shared wal (ApiTxId txid)
        rGetTx <- request @(ApiTransaction n) ctx queryTx Default Empty
        verify rGetTx
            [ expectResponseCode HTTP.status200
            , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
            ]

        -- Make sure only fee is deducted from shared Wallet
        eventually "Wallet balance is as expected" $ do
            rWal <- getSharedWallet ctx walShared
            verify (fmap (view #wallet) <$> rWal)
                [ expectResponseCode HTTP.status200
                , expectField
                    (traverse . #balance . #available . #toNatural)
                    (`shouldBe` (amt - expectedFee))
                ]

        eventually "Tx is in ledger finally" $ do
            rGetTx' <- request @(ApiTransaction n) ctx queryTx Default Empty
            verify rGetTx'
                [ expectResponseCode HTTP.status200
                , expectField (#status . #getApiT) (`shouldBe` InLedger)
                ]
            let listTxEp = Link.listTransactions @'Shared wal
            request @[ApiTransaction n] ctx listTxEp Default Empty
                >>= flip verify
                [ expectListField 1
                    (#direction . #getApiT) (`shouldBe` Incoming)
                , expectListField 1
                    (#status . #getApiT) (`shouldBe` InLedger)
                ]

    it "SHARED_TRANSACTIONS_CREATE_01a -\
        \Empty payload is not allowed" $
        \ctx -> runResourceT $ do

        wa <- fixtureSharedWallet @n ctx
        let emptyPayload = Json [json|{}|]

        rTx <- request @(ApiConstructTransaction n) ctx
            (Link.createUnsignedTransaction @'Shared wa) Default emptyPayload
        verify rTx
            [ expectResponseCode HTTP.status403
            , expectErrorMessage errMsg403InvalidConstructTx
            ]

    it "SHARED_TRANSACTIONS_CREATE_01b - \
        \Validity interval only is not allowed" $
        \ctx -> runResourceT $ do

        wa <- fixtureSharedWallet @n ctx
        let validityInterval = Json [json|
                { "validity_interval":
                    { "invalid_before":
                        { "quantity": 10
                        , "unit": "second"
                        }
                    , "invalid_hereafter":
                        { "quantity": 50
                        , "unit": "second"
                        }
                    }
                }|]

        rTx <- request @(ApiConstructTransaction n) ctx
            (Link.createUnsignedTransaction @'Shared wa)
            Default validityInterval
        verify rTx
            [ expectResponseCode HTTP.status403
            , expectErrorMessage errMsg403InvalidConstructTx
            ]

    it "SHARED_TRANSACTIONS_CREATE_04a - \
        \Single Output Transaction with decode transaction - single party" $
        \ctx -> runResourceT $ do

        wa <- fixtureSharedWallet @n ctx
        wb <- emptyWallet ctx
        let amt = (minUTxOValue (_mainEra ctx) :: Natural)

        payload <- liftIO $ mkTxPayload ctx wb amt

        rTx <- request @(ApiConstructTransaction n) ctx
            (Link.createUnsignedTransaction @'Shared wa) Default payload
        verify rTx
            [ expectSuccess
            , expectResponseCode HTTP.status202
            , expectField (#coinSelection . #inputs)
                (`shouldSatisfy` (not . null))
            , expectField (#coinSelection . #outputs)
                (`shouldSatisfy` (not . null))
            , expectField (#coinSelection . #change)
                (`shouldSatisfy` (not . null))
            , expectField (#fee . #toNatural)
                (`shouldSatisfy` (> 0))
            ]
        let txCbor = getFromResponse #transaction rTx
        let decodePayload = Json (toJSON txCbor)
        rDecodedTxSource <- request @(ApiDecodedTransaction n) ctx
            (Link.decodeTransaction @'Shared wa) Default decodePayload
        rDecodedTxTarget <- request @(ApiDecodedTransaction n) ctx
            (Link.decodeTransaction @'Shelley wb) Default decodePayload

        let expectedFee = getFromResponse (#fee . #toNatural) rTx
        let sharedExpectationsBetweenWallets =
                [ expectResponseCode HTTP.status202
                , expectField (#fee . #toNatural)
                    (`shouldBe` expectedFee)
                , expectField #withdrawals
                    (`shouldBe` [])
                , expectField #collateral
                    (`shouldBe` [])
                , expectField #metadata
                    (`shouldBe` (ApiTxMetadata Nothing))
                , expectField #scriptValidity
                    (`shouldBe` (Just $ ApiT TxScriptValid))
                ]

        verify rDecodedTxTarget sharedExpectationsBetweenWallets

        let isInpOurs inp = case inp of
                ExternalInput _ -> False
                WalletInput _ -> True
        let areOurs = all isInpOurs
        addrs <- listAddresses @n ctx wb
        let addrIx = 1
        let addrDest = (addrs !! addrIx) ^. #id
        let expectedTxOutTarget = WalletOutput $ ApiWalletOutput
                { address = addrDest
                , amount = ApiAmount amt
                , assets = mempty
                , derivationPath = NE.fromList
                    [ ApiT (DerivationIndex 2_147_485_500)
                    , ApiT (DerivationIndex 2_147_485_463)
                    , ApiT (DerivationIndex 2_147_483_648)
                    , ApiT (DerivationIndex 0)
                    , ApiT (DerivationIndex $ fromIntegral addrIx)
                    ]
                }
        let isOutOurs out = case out of
                WalletOutput _ -> False
                ExternalOutput _ -> True

        verify rDecodedTxSource $
            sharedExpectationsBetweenWallets ++
            [ expectField #inputs
                (`shouldSatisfy` areOurs)
            , expectField #outputs
                (`shouldNotContain` [expectedTxOutTarget])
            -- Check that the change output is there:
            , expectField (#outputs)
                ((`shouldBe` 1) . length . filter isOutOurs)
            ]

        let (ApiSerialisedTransaction apiTx _) =
                getFromResponse #transaction rTx
        signedTx <-
            signSharedTx ctx wa apiTx [ expectResponseCode HTTP.status202 ]
        let decodePayload1 = Json (toJSON signedTx)
        rDecodedTxSource1 <- request @(ApiDecodedTransaction n) ctx
            (Link.decodeTransaction @'Shared wa) Default decodePayload1
        rDecodedTxTarget1 <- request @(ApiDecodedTransaction n) ctx
            (Link.decodeTransaction @'Shelley wb) Default decodePayload1
        verify rDecodedTxTarget1 sharedExpectationsBetweenWallets
        verify rDecodedTxSource1 $
            sharedExpectationsBetweenWallets ++
            [ expectField #inputs
                (`shouldSatisfy` areOurs)
            , expectField #outputs
                (`shouldNotContain` [expectedTxOutTarget])
            -- Check that the change output is there:
            , expectField (#outputs)
                ((`shouldBe` 1) . length . filter isOutOurs)
            ]

        -- Submit tx
        submittedTx <- submitSharedTxWithWid ctx wa signedTx
        verify submittedTx
            [ expectSuccess
            , expectResponseCode HTTP.status202
            ]

        let walShared = ApiSharedWallet (Right wa)

        eventually "Source wallet balance is decreased by amt + fee" $ do
            rWal <- getSharedWallet ctx walShared
            verify (fmap (view #wallet) <$> rWal)
                [ expectResponseCode HTTP.status200
                , expectField
                    (traverse . #balance . #available . #toNatural)
                    (`shouldBe` (faucetUtxoAmt - expectedFee - amt))
                ]

        eventually "Target wallet balance is increased by amt" $ do
            rWa <- request @ApiWallet ctx
                (Link.getWallet @'Shelley wb) Default Empty
            verify rWa
                [ expectSuccess
                , expectField
                        (#balance . #available . #toNatural)
                        (`shouldBe` amt)
                ]

    it "SHARED_TRANSACTIONS_CREATE_04b - \
        \Single Output Transaction Shared -> Shared" $
        \ctx -> runResourceT $ do

        wa <- fixtureSharedWallet @n ctx
        let srcWallet = ApiSharedWallet (Right wa)
        destWallet@(ApiSharedWallet (Right wb)) <- emptySharedWallet ctx

        -- prepare payload
        let amt = (minUTxOValue (_mainEra ctx) :: Natural)
        rAddr <- request @[ApiAddressWithPath n] ctx
            (Link.listAddresses @'Shared wb) Default Empty
        let addrs = getResponse rAddr
        let destination = (addrs !! 1) ^. #id
        let payload = Json [json|{
            "payments": [{
                "address": #{destination},
                "amount": {
                    "quantity": #{amt},
                    "unit": "lovelace"
                }
            }]
        }|]

        -- Construct tx
        rTx <- request @(ApiConstructTransaction n) ctx
            (Link.createUnsignedTransaction @'Shared wa) Default payload
        verify rTx
            [ expectSuccess
            , expectResponseCode HTTP.status202
            , expectField (#fee . #toNatural)
                (`shouldSatisfy` (> 0))
            ]

        let (ApiSerialisedTransaction apiTx _) =
                getFromResponse #transaction rTx

        -- Sign tx
        signedTx <-
            signSharedTx ctx wa apiTx [ expectResponseCode HTTP.status202 ]

        -- Submit tx
        submittedTx <- submitSharedTxWithWid ctx wa signedTx
        verify submittedTx
            [ expectSuccess
            , expectResponseCode HTTP.status202
            ]

        let expectedFee = getFromResponse (#fee . #toNatural) rTx
        let txid = getFromResponse #id submittedTx
        let linkSrc = Link.getTransaction @'Shared wa (ApiTxId txid)
        eventually "Source wallet balance is decreased by amt + fee" $ do
            rWal <- getSharedWallet ctx srcWallet
            verify (fmap (view #wallet) <$> rWal)
                [ expectResponseCode HTTP.status200
                , expectField
                    (traverse . #balance . #available . #toNatural)
                    (`shouldBe` (faucetUtxoAmt - expectedFee - amt))
                ]
            -- check outgoing tx in source wallet
            rSrc <- request @(ApiTransaction n) ctx linkSrc Default Empty
            verify rSrc
                [ expectResponseCode HTTP.status200
                , expectField (#amount . #toNatural)
                    (`shouldBe` amt + expectedFee)
                , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
                , expectField (#status . #getApiT) (`shouldBe` InLedger)
                , expectField (#fee . #toNatural) (`shouldBe` expectedFee)
                ]

        let linkDest = Link.getTransaction @'Shared wb (ApiTxId txid)
        eventually "Target wallet balance is increased by amt" $ do
            rWal <- getSharedWallet ctx destWallet
            verify (fmap (view #wallet) <$> rWal)
                [ expectResponseCode HTTP.status200
                , expectField
                    (traverse . #balance . #available . #toNatural)
                    (`shouldBe` amt)
                ]
            -- check incoming tx in destination wallet
            rDst <- request @(ApiTransaction n) ctx linkDest Default Empty
            verify rDst
                [ expectResponseCode HTTP.status200
                , expectField (#amount . #toNatural) (`shouldBe` amt)
                , expectField (#direction . #getApiT) (`shouldBe` Incoming)
                , expectField (#status . #getApiT) (`shouldBe` InLedger)
                , expectField (#fee . #toNatural) (`shouldBe` expectedFee)
                ]

    it "SHARED_TRANSACTIONS_CREATE_04c - \
        \Cannot spend less than minUTxOValue" $
        \ctx -> runResourceT $ do

        wa <- fixtureSharedWallet @n ctx
        wb <- emptyWallet ctx
        let amt = minUTxOValue (_mainEra ctx) - 1

        payload <- liftIO $ mkTxPayload ctx wb amt

        rTx <- request @(ApiConstructTransaction n) ctx
            (Link.createUnsignedTransaction @'Shared wa) Default payload
        verify rTx
            [ expectResponseCode HTTP.status403
            , expectErrorMessage errMsg403MinUTxOValue
            ]

    it "SHARED_TRANSACTIONS_CREATE_04d - \
        \Can't cover fee" $
        \ctx -> runResourceT $ do

        wa <- fixtureSharedWallet @n ctx
        wb <- emptyWallet ctx

        payload <- liftIO $ mkTxPayload ctx wb faucetUtxoAmt

        rTx <- request @(ApiConstructTransaction n) ctx
            (Link.createUnsignedTransaction @'Shared wa) Default payload
        verify rTx
            [ expectResponseCode HTTP.status403
            , expectErrorMessage errMsg403Fee
            ]

    it "SHARED_TRANSACTIONS_CREATE_04e - \
        \Multiple Output Tx to single wallet"
        $ \ctx -> runResourceT $ do

        wa <- fixtureSharedWallet @n ctx
        wb <- emptyWallet ctx
        addrs <- listAddresses @n ctx wb
        let amt = minUTxOValue (_mainEra ctx) :: Natural
        let destination1 = (addrs !! 1) ^. #id
        let destination2 = (addrs !! 2) ^. #id
        let payload = Json [json|{
                "payments": [{
                    "address": #{destination1},
                    "amount": {
                        "quantity": #{amt},
                        "unit": "lovelace"
                    }
                },
                {
                    "address": #{destination2},
                    "amount": {
                        "quantity": #{amt},
                        "unit": "lovelace"
                    }
                }]
            }|]

        rTx <- request @(ApiConstructTransaction n) ctx
            (Link.createUnsignedTransaction @'Shared wa) Default payload
        verify rTx
            [ expectSuccess
            , expectResponseCode HTTP.status202
            , expectField (#coinSelection . #inputs)
                (`shouldSatisfy` (not . null))
            , expectField (#coinSelection . #outputs)
                (`shouldSatisfy` (not . null))
            , expectField (#coinSelection . #change)
                (`shouldSatisfy` (not . null))
            , expectField (#fee . #toNatural)
                (`shouldSatisfy` (> 0))
            ]

    it "SHARED_TRANSACTIONS_CREATE_05a - \
        \Single Output Transaction with decode transaction - multi party" $
        \ctx -> runResourceT $ do

        (sharedWal1, sharedWal2) <- fixtureTwoPartySharedWallet ctx
        singleOutputTxTwoParty ctx sharedWal1 sharedWal2

    it "SHARED_TRANSACTIONS_CREATE_05b - \
        \Single Output Transaction with decode transaction - multi party" $
        \ctx -> runResourceT $ do

        (sharedWal1, sharedWal2, _, _, _, _) <-
            fixtureTwoPartySharedWalletPatched ctx
        singleOutputTxTwoParty ctx sharedWal1 sharedWal2

    it "SHARED_TRANSACTIONS_CREATE_05c - \
        \Single Output Transaction with decode transaction - multi party" $
        \ctx -> runResourceT $ do

        (sharedWal1, sharedWal2, sharedWal3) <-
            fixtureThreePartySharedWallet ctx

        -- check we see balance from two wallets
        rSharedWal1 <- getSharedWallet ctx (ApiSharedWallet (Right sharedWal1))
        rSharedWal2 <- getSharedWallet ctx (ApiSharedWallet (Right sharedWal2))
        rSharedWal3 <- getSharedWallet ctx (ApiSharedWallet (Right sharedWal3))

        let balanceExp =
                [ expectResponseCode HTTP.status200
                , expectField (traverse . #balance . #available)
                    (`shouldBe` ApiAmount faucetUtxoAmt)
                ]

        verify (fmap (view #wallet) <$> rSharedWal1) balanceExp
        verify (fmap (view #wallet) <$> rSharedWal2) balanceExp
        verify (fmap (view #wallet) <$> rSharedWal3) balanceExp

        wb <- emptyWallet ctx
        let amt = (minUTxOValue (_mainEra ctx) :: Natural)

        payload <- liftIO $ mkTxPayload ctx wb amt

        rTx <- request @(ApiConstructTransaction n) ctx
            (Link.createUnsignedTransaction @'Shared sharedWal1) Default payload
        verify rTx
            [ expectResponseCode HTTP.status202
            ]
        let txCbor = getFromResponse #transaction rTx
        let decodePayload1 = Json (toJSON txCbor)
        rDecodedTx1Wal1 <- request @(ApiDecodedTransaction n) ctx
            (Link.decodeTransaction @'Shared sharedWal1) Default decodePayload1
        rDecodedTx1Wal2 <- request @(ApiDecodedTransaction n) ctx
            (Link.decodeTransaction @'Shared sharedWal2) Default decodePayload1
        rDecodedTx1Wal3 <- request @(ApiDecodedTransaction n) ctx
            (Link.decodeTransaction @'Shared sharedWal3) Default decodePayload1
        rDecodedTx1Target <- request @(ApiDecodedTransaction n) ctx
            (Link.decodeTransaction @'Shelley wb) Default decodePayload1

        let expectedFee = getFromResponse (#fee . #toNatural) rTx
        let (ApiScriptTemplate scriptTemplate) =
                sharedWal1 ^. #paymentScriptTemplate
        let paymentScript =
                NativeExplicitScript
                (replaceCosignersWithVerKeys
                    CA.UTxOExternal scriptTemplate (Index 1))
                ViaSpending
        let noVerKeyWitness = mkApiWitnessCount WitnessCount
                { verificationKey = 0
                , scripts = [paymentScript]
                , bootstrap = 0
                }
        let witsExp1 =
                [ expectField (#witnessCount) (`shouldBe` noVerKeyWitness) ]

        verify rDecodedTx1Wal1 witsExp1
        verify rDecodedTx1Wal2 witsExp1
        verify rDecodedTx1Wal3 witsExp1

        -- for shelley wallet the script's verification key is unknown,
        -- it only is aware of its policy verification key
        let noVerKeyWitnessHex = mkApiWitnessCount WitnessCount
                { verificationKey = 0
                , scripts = [changeRoleInAnyExplicitScript CA.Unknown paymentScript]
                , bootstrap = 0
                }
        let witsExp1hex =
                [ expectField (#witnessCount) (`shouldBe` noVerKeyWitnessHex) ]
        verify rDecodedTx1Target witsExp1hex

        -- adding one witness
        let (ApiSerialisedTransaction apiTx1 _) =
                getFromResponse #transaction rTx
        signedTx1 <-
            signSharedTx ctx sharedWal1 apiTx1
                [ expectResponseCode HTTP.status202 ]
        let decodePayload2 = Json (toJSON signedTx1)
        rDecodedTx2Wal1 <- request @(ApiDecodedTransaction n) ctx
            (Link.decodeTransaction @'Shared sharedWal1) Default decodePayload2
        rDecodedTx2Wal2 <- request @(ApiDecodedTransaction n) ctx
            (Link.decodeTransaction @'Shared sharedWal2) Default decodePayload2
        rDecodedTx2Wal3 <- request @(ApiDecodedTransaction n) ctx
            (Link.decodeTransaction @'Shared sharedWal3) Default decodePayload2
        rDecodedTx2Target <- request @(ApiDecodedTransaction n) ctx
            (Link.decodeTransaction @'Shelley wb) Default decodePayload2

        let oneVerKeyWitness = mkApiWitnessCount WitnessCount
                { verificationKey = 1
                , scripts = [paymentScript]
                , bootstrap = 0
                }
        let witsExp2 =
                [ expectField (#witnessCount) (`shouldBe` oneVerKeyWitness) ]

        verify rDecodedTx2Wal1 witsExp2
        verify rDecodedTx2Wal2 witsExp2
        verify rDecodedTx2Wal3 witsExp2
        -- for shelley wallet the script's verification key is unknown,
        -- it only is aware of its policy verification key
        let oneVerKeyWitnessHex = mkApiWitnessCount WitnessCount
                { verificationKey = 1
                , scripts = [changeRoleInAnyExplicitScript CA.Unknown paymentScript]
                , bootstrap = 0
                }
        let witsExp2hex =
                [ expectField (#witnessCount) (`shouldBe` oneVerKeyWitnessHex) ]
        verify rDecodedTx2Target witsExp2hex

        submittedTx1 <- submitSharedTxWithWid ctx sharedWal1 signedTx1
        verify submittedTx1
            [ expectResponseCode HTTP.status403
            , expectErrorMessage (errMsg403MissingWitsInTransaction 2 1)
            ]

        -- adding the witness by the second participant make tx valid for
        -- submission
        let (ApiSerialisedTransaction apiTx2 _) = signedTx1
        signedTx3 <-
            signSharedTx ctx sharedWal2 apiTx2
                [ expectResponseCode HTTP.status202 ]

        -- now submission works
        submittedTx3 <- submitSharedTxWithWid ctx sharedWal1 signedTx3
        verify submittedTx3
            [ expectResponseCode HTTP.status202
            ]

       -- checking decreased balance of shared wallets
        eventually "wShared balance is decreased" $ do
            wal1 <- getSharedWallet ctx (ApiSharedWallet (Right sharedWal1))
            wal2 <- getSharedWallet ctx (ApiSharedWallet (Right sharedWal2))
            wal3 <- getSharedWallet ctx (ApiSharedWallet (Right sharedWal3))
            let balanceExp1 =
                    [ expectResponseCode HTTP.status200
                    , expectField (traverse . #balance . #available)
                        (`shouldBe` ApiAmount
                            (faucetUtxoAmt - expectedFee - amt)
                        )
                    ]
            verify (fmap (view #wallet) <$> wal1) balanceExp1
            verify (fmap (view #wallet) <$> wal2) balanceExp1
            verify (fmap (view #wallet) <$> wal3) balanceExp1

       -- checking the balance of target wallet has increased
        eventually "Target wallet balance is increased by amt" $ do
            rWa <- request @ApiWallet ctx
                (Link.getWallet @'Shelley wb) Default Empty
            verify rWa
                [ expectSuccess
                , expectField
                        (#balance . #available . #toNatural)
                        (`shouldBe` amt)
                ]

    it "SHARED_TRANSACTIONS_CREATE_06 - \
        \multi party wallet balance consistency before and after wallet \
        \delete" $
        \ctx -> runResourceT $ do

        ( sharedWal1
            , sharedWal2
            , payload1
            , accXPubDerived1
            , payload2
            , accXPubDerived2
            ) <- fixtureTwoPartySharedWalletPatched ctx
        singleOutputTxTwoParty ctx sharedWal1 sharedWal2

        -- reading the balance and checking consistency among parties
        (_, Right (ApiSharedWallet (Right wal1))) <-
            getSharedWallet ctx (ApiSharedWallet (Right sharedWal1))
        (_, Right (ApiSharedWallet (Right wal2))) <-
            getSharedWallet ctx (ApiSharedWallet (Right sharedWal2))
        let bal1 = wal1 ^. (#balance . #available)
        let bal2 = wal2 ^. (#balance . #available)
        bal1 `shouldBe` bal2

        --each party deletes its shared wallet
        rDel1 <- deleteSharedWallet ctx (ApiSharedWallet (Right wal1))
        expectResponseCode HTTP.status204 rDel1
        rDel2 <- deleteSharedWallet ctx (ApiSharedWallet (Right wal2))
        expectResponseCode HTTP.status204 rDel2

        -- creating wallets once again with patching
        rPost1 <- postSharedWallet ctx Default payload1
        verify (fmap (swapEither . view #wallet) <$> rPost1)
            [ expectResponseCode HTTP.status201
            ]
        let walPending1 = getResponse rPost1
        let payloadPatch1 = Json [json| {
                "cosigner#1": #{accXPubDerived2}
                } |]
        rPatch1 <- patchSharedWallet ctx walPending1 Payment payloadPatch1
        expectResponseCode HTTP.status200 rPatch1
        let walShared1 = getResponse rPatch1

        rPost2 <- postSharedWallet ctx Default payload2
        verify (fmap (swapEither . view #wallet) <$> rPost2)
            [ expectResponseCode HTTP.status201
            ]
        let walPending2 = getResponse rPost2
        let payloadPatch2 = Json [json| {
                "cosigner#0": #{accXPubDerived1}
                } |]
        rPatch2 <- patchSharedWallet ctx walPending2 Payment payloadPatch2
        expectResponseCode HTTP.status200 rPatch2
        let walShared2 = getResponse rPatch2

        --checking that balances are the same as before deletion
        eventually "balance is like before deletion" $ do
            wal1a <- getSharedWallet ctx walShared1
            wal2a <- getSharedWallet ctx walShared2
            let balanceExp =
                    [ expectResponseCode HTTP.status200
                    , expectField (traverse . #balance . #available)
                        (`shouldBe` bal1)
                    ]
            verify (fmap (view #wallet) <$> wal1a) balanceExp
            verify (fmap (view #wallet) <$> wal2a) balanceExp

    it "SHARED_TRANSACTIONS_LIST_01 -\
        \Can list Incoming and Outgoing transactions" $
        \ctx -> runResourceT $ do

        (wSrc, wDest) <- (,) <$> fixtureSharedWallet @n ctx <*> emptyWallet ctx
        addrs <- listAddresses @n ctx wDest

        let amt = minUTxOValue (_mainEra ctx) :: Natural
        let destination = (addrs !! 1) ^. #id
        let payload = Json [json|{
                "payments": [{
                    "address": #{destination},
                    "amount": {
                        "quantity": #{amt},
                        "unit": "lovelace"
                    }
                }],
                "passphrase": "cardano-wallet"
            }|]

        realizeTx ctx wSrc payload

        eventually "Wallet balance is as expected" $ do
            rGet <- request @ApiWallet ctx
                (Link.getWallet @'Shelley wDest) Default Empty
            verify rGet
                [ expectField
                    (#balance . #total) (`shouldBe` ApiAmount amt)
                , expectField
                    (#balance . #available) (`shouldBe` ApiAmount amt)
                ]

        -- Verify Tx list contains Incoming and Outgoing
        let link = Link.listTransactions @'Shared wSrc
        r <- request @([ApiTransaction n]) ctx link Default Empty
        expectResponseCode HTTP.status200 r

        verify r
            [ expectListField 0 (#direction . #getApiT) (`shouldBe` Outgoing)
            , expectListField 1 (#direction . #getApiT) (`shouldBe` Incoming)
            ]

    -- This scenario covers the following matrix of cases. Cases were generated
    -- using one of pairwise test cases generation tools available online.
    -- +---+----------+----------+------------+-----------+---------------+
    --     |  start   |   end    |   order    | max_count |  result       |
    -- +---+----------+----------+------------+-----------+---------------+
    --   1 | edge     | edge     | ascending  |  empty    |  2 ascending  |
    --   2 | edge     | edge + 1 | descending |  empty    |  2 descending |
    --   3 | edge     | edge - 1 | empty      |  empty    |  1st one      |
    --   4 | edge     | empty    | empty      |  empty    |  2 descending |
    --   5 | edge + 1 | edge + 1 | empty      |  empty    |  2nd one      |
    --   6 | edge + 1 | edge - 1 | empty      |  empty    |  none         |
    --   7 | edge + 1 | empty    | ascending  |  empty    |  2nd one      |
    --   8 | edge + 1 | edge     | descending |  empty    |  2nd one      |
    --   9 | edge - 1 | edge - 1 | ascending  |  empty    |  1st one      |
    --  10 | edge - 1 | empty    | descending |  empty    |  2 descending |
    --  11 | edge - 1 | edge     | empty      |  empty    |  2 descending |
    --  12 | edge - 1 | edge + 1 | empty      |  empty    |  2 descending |
    --  13 | empty    | empty    | empty      |  empty    |  2 descending |
    --  14 | empty    | edge     | empty      |  empty    |  2 descending |
    --  15 | empty    | edge + 1 | ascending  |  empty    |  2 ascending  |
    --  16 | empty    | edge - 1 | descending |  empty    |  1st one      |
    --  17 | t1       | t1       | empty      |  empty    |  1st one      |
    --  18 | t2       | t2       | descending |  empty    |  2nd one      |
    --  19 | empty    | empty    | empty      |    1      |  2nd one      |
    --  20 | empty    | empty    | ascending  |    1      |  1st one      |
    -- +---+----------+----------+------------+-----------+---------------+

    it "SHARED_TRANSACTIONS_LIST_02,03x -\
        \Can limit/order results with start, end and order"
        $ \ctx -> runResourceT $ do

        let amt1 = minUTxOValue (_mainEra ctx)
        let amt2 = 2 * amt1
        (wSrc, wDest@(ApiSharedWallet (Right walDest))) <-
            (,) <$> fixtureSharedWallet @n ctx <*> emptySharedWallet ctx

        -- destination wallet
        rAddr <- request @[ApiAddressWithPath n] ctx
            (Link.listAddresses @'Shared walDest) Default Empty
        expectResponseCode HTTP.status200 rAddr
        let addrs = getResponse rAddr
        let destAddr1 = (head addrs) ^. #id
        let destAddr2 = (addrs !! 1) ^. #id
        let payload destination amt = Json [json|{
                "payments": [{
                    "address": #{destination},
                    "amount": {
                        "quantity": #{amt},
                        "unit": "lovelace"
                    }
                }],
                "passphrase": "cardano-wallet"
            }|]

        -- post txs
        realizeTx ctx wSrc (payload destAddr1 amt1)
        eventually "wDest balance is increased" $ do
            wal <- getSharedWallet ctx wDest
            let balanceExp =
                    [ expectResponseCode HTTP.status200
                    , expectField (traverse . #balance . #available)
                        (`shouldBe` ApiAmount amt1)
                    ]
            verify (fmap (view #wallet) <$> wal) balanceExp

        realizeTx ctx wSrc (payload destAddr2 amt2)
        eventually "wDest balance is increased again" $ do
            wal <- getSharedWallet ctx wDest
            let balanceExp =
                    [ expectResponseCode HTTP.status200
                    , expectField (traverse . #balance . #available)
                        (`shouldBe` ApiAmount (amt1 + amt2))
                    ]
            verify (fmap (view #wallet) <$> wal) balanceExp

        txs <- eventually "I make sure there are exactly 2 transactions" $ do
            let linkList = listTransactionsFilteredByAddress walDest Nothing
            rl <- request @([ApiTransaction n]) ctx linkList Default Empty
            verify rl [expectListSize 2]
            pure (getResponse rl)

        let [Just t2, Just t1] = fmap (fmap (view #time) . insertedAt) txs
        let plusDelta, minusDelta :: UTCTime -> UTCTime
            plusDelta = addUTCTime (toEnum 1_000_000_000)
            minusDelta = addUTCTime (toEnum (-1_000_000_000))

        let matrix :: [TestCase [ApiTransaction n]] =
                [ TestCase -- 1
                    { query = toQueryString
                        [ ("start", utcIso8601ToText t1)
                        , ("end", utcIso8601ToText t2)
                        , ("order", "ascending")
                        ]
                    , assertions =
                        [ expectListSize 2
                        , expectListField 0 #amount (`shouldBe` ApiAmount amt1)
                        , expectListField 1 #amount (`shouldBe` ApiAmount amt2)
                        ]
                    }
                , TestCase -- 2
                    { query = toQueryString
                        [ ("start", utcIso8601ToText t1)
                        , ("end", utcIso8601ToText $ plusDelta t2)
                        , ("order", "descending")
                        ]
                    , assertions =
                        [ expectListSize 2
                        , expectListField 0 #amount (`shouldBe` ApiAmount amt2)
                        , expectListField 1 #amount (`shouldBe` ApiAmount amt1)
                        ]
                    }
                , TestCase -- 3
                    { query = toQueryString
                        [ ("start", utcIso8601ToText t1)
                        , ("end", utcIso8601ToText $ minusDelta t2)
                        ]
                    , assertions =
                        [ expectListSize 1
                        , expectListField 0 #amount (`shouldBe` ApiAmount amt1)
                        ]
                    }
                , TestCase -- 4
                    { query = toQueryString
                        [ ("start", utcIso8601ToText t1) ]
                    , assertions =
                        [ expectListSize 2
                        , expectListField 0 #amount (`shouldBe` ApiAmount amt2)
                        , expectListField 1 #amount (`shouldBe` ApiAmount amt1)
                        ]
                    }
                , TestCase --5
                    { query = toQueryString
                        [ ("start", utcIso8601ToText $ plusDelta t1)
                        , ("end", utcIso8601ToText $ plusDelta t2)
                        ]
                    , assertions =
                        [ expectListSize 1
                        , expectListField 0 #amount (`shouldBe` ApiAmount amt2)
                        ]
                    }
                , TestCase -- 6
                    { query = toQueryString
                        [ ("start", utcIso8601ToText $ plusDelta t1)
                        , ("end", utcIso8601ToText $ minusDelta t2)
                        ]
                    , assertions =
                        [ expectListSize 0 ]
                    }
                , TestCase -- 7
                    { query = toQueryString
                        [ ("start", utcIso8601ToText $ plusDelta t1)
                        , ("order", "ascending")
                        ]
                    , assertions =
                        [ expectListSize 1
                        , expectListField 0 #amount (`shouldBe` ApiAmount amt2)
                        ]
                    }
                , TestCase -- 8
                    { query = toQueryString
                        [ ("order", "descending")
                        , ("start", utcIso8601ToText $ plusDelta t1)
                        , ("end", utcIso8601ToText t2)
                        ]
                    , assertions =
                        [ expectListSize 1
                        , expectListField 0 #amount (`shouldBe` ApiAmount amt2)
                        ]
                    }
                , TestCase -- 9
                    { query = toQueryString
                        [ ("order", "ascending")
                        , ("start", utcIso8601ToText $ minusDelta t1)
                        , ("end", utcIso8601ToText $ minusDelta t2)
                        ]
                    , assertions =
                        [ expectListSize 1
                        , expectListField 0 #amount (`shouldBe` ApiAmount amt1)
                        ]
                    }
                , TestCase -- 10
                    { query = toQueryString
                        [ ("order", "descending")
                        , ("start", utcIso8601ToText $ minusDelta t1)
                        ]
                    , assertions =
                        [ expectListSize 2
                        , expectListField 0 #amount (`shouldBe` ApiAmount amt2)
                        , expectListField 1 #amount (`shouldBe` ApiAmount amt1)
                        ]
                    }
                , TestCase -- 11
                    { query = toQueryString
                        [ ("start", utcIso8601ToText $ minusDelta t1)
                        , ("end", utcIso8601ToText t2)
                        ]
                    , assertions =
                        [ expectListSize 2
                        , expectListField 0 #amount (`shouldBe` ApiAmount amt2)
                        , expectListField 1 #amount (`shouldBe` ApiAmount amt1)
                        ]
                    }
                , TestCase -- 12
                    { query = toQueryString
                        [ ("start", utcIso8601ToText $ minusDelta t1)
                        , ("end", utcIso8601ToText $ plusDelta t2)
                        ]
                    , assertions =
                        [ expectListSize 2
                        , expectListField 0 #amount (`shouldBe` ApiAmount amt2)
                        , expectListField 1 #amount (`shouldBe` ApiAmount amt1)
                        ]
                    }
                , TestCase -- 13
                    { query = mempty
                    , assertions =
                        [ expectListSize 2
                        , expectListField 0 #amount (`shouldBe` ApiAmount amt2)
                        , expectListField 1 #amount (`shouldBe` ApiAmount amt1)
                        ]
                    }
                , TestCase -- 14
                    { query = toQueryString
                        [ ("end", utcIso8601ToText t2) ]
                    , assertions =
                        [ expectListSize 2
                        , expectListField 0 #amount (`shouldBe` ApiAmount amt2)
                        , expectListField 1 #amount (`shouldBe` ApiAmount amt1)
                        ]
                    }
                , TestCase -- 15
                    { query = toQueryString
                        [ ("end", utcIso8601ToText $ plusDelta t2) ]
                    , assertions =
                        [ expectListSize 2
                        , expectListField 0 #amount (`shouldBe` ApiAmount amt2)
                        , expectListField 1 #amount (`shouldBe` ApiAmount amt1)
                        ]
                    }
                , TestCase -- 16
                    { query = toQueryString
                        [ ("end", utcIso8601ToText $ minusDelta t2) ]
                    , assertions =
                        [ expectListSize 1
                        , expectListField 0 #amount (`shouldBe` ApiAmount amt1)
                        ]
                    }
                , TestCase -- 17
                    { query = toQueryString
                        [ ("start", utcIso8601ToText t1)
                        , ("end", utcIso8601ToText t1)
                        ]
                    , assertions =
                        [ expectListSize 1
                        , expectListField 0 #amount (`shouldBe` ApiAmount amt1)
                        ]
                    }
                , TestCase -- 18
                    { query = toQueryString
                        [ ("start", utcIso8601ToText t2)
                        , ("end", utcIso8601ToText t2)
                        ]
                    , assertions =
                        [ expectListSize 1
                        , expectListField 0 #amount (`shouldBe` ApiAmount amt2)
                        ]
                    }
                , TestCase -- 19
                    { query = toQueryString
                        [ ("max_count", "1")
                        ]
                    , assertions =
                        [ expectListSize 1
                        , expectListField 0 #amount (`shouldBe` ApiAmount amt2)
                        ]
                    }
                , TestCase -- 20
                    { query = toQueryString
                        [ ("max_count", "1")
                        , ("order", "ascending")
                        ]
                    , assertions =
                        [ expectListSize 1
                        , expectListField 0 #amount (`shouldBe` ApiAmount amt1)
                        ]
                    }
                ]

        let withQuery q (method, link) = (method, link <> q)

        liftIO $ forM_ matrix $ \tc -> do
            let link =
                    withQuery (query tc) $
                    Link.listTransactions @'Shared walDest
            rf <- request @([ApiTransaction n]) ctx link Default Empty
            verify rf (assertions tc)

    describe "SHARED_TRANSACTIONS_LIST_02,03 - \
        \Faulty start, end, order values" $ do

        let orderErr = "Please specify one of the following values:\
            \ ascending, descending."
        let startEndErr = "Expecting ISO 8601 date-and-time format\
            \ (basic or extended), e.g. 2012-09-25T10:15:00Z."
        let queries :: [TestCase [ApiTransaction n]] =
                [
                  TestCase
                    { query = toQueryString [ ("start", "2009") ]
                    , assertions =
                         [ expectResponseCode HTTP.status400
                         , expectErrorMessage startEndErr
                         ]
                    }
                 , TestCase
                     { query = toQueryString
                         [ ("start", "2012-09-25T10:15:00Z")
                         , ("end", "2016-11-21")
                         ]
                     , assertions =
                         [ expectResponseCode HTTP.status400
                         , expectErrorMessage startEndErr
                         ]
                     }
                 , TestCase
                     { query = toQueryString
                         [ ("start", "2012-09-25")
                         , ("end", "2016-11-21T10:15:00Z")
                         ]
                     , assertions =
                         [ expectResponseCode HTTP.status400
                         , expectErrorMessage startEndErr
                         ]
                     }
                 , TestCase
                     { query = toQueryString
                         [ ("end", "2012-09-25T10:15:00Z")
                         , ("start", "2016-11-21")
                         ]
                     , assertions =
                         [ expectResponseCode HTTP.status400
                         , expectErrorMessage startEndErr
                         ]
                     }
                 , TestCase
                     { query = toQueryString [ ("order", "scending") ]
                     , assertions =
                        [ expectResponseCode HTTP.status400
                        , expectErrorMessage orderErr
                        ]
                     }
                 , TestCase
                     { query = toQueryString
                         [ ("start", "2012-09-25T10:15:00Z")
                         , ("order", "asc")
                         ]
                     , assertions =
                         [ expectResponseCode HTTP.status400
                         , expectErrorMessage orderErr
                         ]
                     }
                ]

        let withQuery q (method, link) = (method, link <> q)

        forM_ queries $ \tc -> it (T.unpack $ query tc) $
            \ctx -> runResourceT $ do
                (ApiSharedWallet (Right w)) <- emptySharedWallet ctx
                let link = withQuery (query tc) $
                        Link.listTransactions @'Shared w
                r <- request @([ApiTransaction n]) ctx link Default Empty
                liftIO $ verify r (assertions tc)

    it "SHARED_TRANSACTIONS_LIST_02 - \
        \Start time shouldn't be later than end time" $
        \ctx -> runResourceT $ do

            (ApiSharedWallet (Right w)) <- emptySharedWallet ctx
            let startTime = "2009-09-09T09:09:09Z"
            let endTime = "2001-01-01T01:01:01Z"
            let link = Link.listTransactions' @'Shared w
                    Nothing
                    (either (const Nothing) Just $ fromText $ T.pack startTime)
                    (either (const Nothing) Just $ fromText $ T.pack endTime)
                    Nothing
                    Nothing
                    Nothing
            r <- request @([ApiTransaction n]) ctx link Default Empty
            expectResponseCode HTTP.status400 r
            expectErrorMessage
                (errMsg400StartTimeLaterThanEndTime startTime endTime) r
            pure ()

    it "SHARED_TRANSACTIONS_LIST_03 - \
        \Minimum withdrawal shouldn't be 0" $
        \ctx -> runResourceT $ do

            (ApiSharedWallet (Right w)) <- emptySharedWallet ctx
            let link = Link.listTransactions' @'Shared w
                    (Just 0)
                    Nothing
                    Nothing
                    Nothing
                    Nothing
                    Nothing
            r <- request @([ApiTransaction n]) ctx link Default Empty
            expectResponseCode HTTP.status400 r
            expectErrorMessage errMsg400MinWithdrawalWrong r
            pure ()

    it "SHARED_TRANSACTIONS_LIST_03 - \
        \Minimum withdrawal can be 1, shows empty when no withdrawals" $
        \ctx -> runResourceT $ do

            (ApiSharedWallet (Right w)) <- emptySharedWallet ctx
            let link = Link.listTransactions' @'Shared w
                    (Just 1)
                    Nothing
                    Nothing
                    Nothing
                    Nothing
                    Nothing
            r <- request @([ApiTransaction n]) ctx link Default Empty
            expectResponseCode HTTP.status200 r
            let txs = getResponse r
            txs `shouldBe` []

    it "SHARED_TRANSACTIONS_LIST_04 - \
        \Deleted wallet" $
        \ctx -> runResourceT $ do

        (ApiSharedWallet (Right w)) <- emptySharedWallet ctx
        _ <- request @ApiWallet ctx
            (Link.deleteWallet @'Shared w) Default Empty
        r <- request @([ApiTransaction n]) ctx
            (Link.listTransactions @'Shared w)
            Default Empty
        expectResponseCode HTTP.status404 r
        expectErrorMessage (errMsg404NoWallet $ w ^. walletId) r

    it "SHARED_TRANSACTIONS_LIST_RANGE_01 - \
        \Transaction at time t is SELECTED by small ranges that cover it" $
        \ctx -> runResourceT $ do

            w <- fixtureSharedWalletWith ctx (minUTxOValue (_mainEra ctx))
            t <- unsafeGetTransactionTime =<< listAllSharedTransactions ctx w
            let (te, tl) = (utcTimePred t, utcTimeSucc t)
            txs1 <- listSharedTransactions ctx w (Just t ) (Just t ) Nothing
                Nothing
            txs2 <- listSharedTransactions ctx w (Just te) (Just t ) Nothing
                Nothing
            txs3 <- listSharedTransactions ctx w (Just t ) (Just tl) Nothing
                Nothing
            txs4 <- listSharedTransactions ctx w (Just te) (Just tl) Nothing
                Nothing
            length <$> [txs1, txs2, txs3, txs4] `shouldSatisfy` all (== 1)

    it "SHARED_TRANSACTIONS_LIST_RANGE_02 - \
        \Transaction at time t is NOT selected by range (t + 𝛿t, ...)" $
        \ctx -> runResourceT $ do

            w <- fixtureSharedWalletWith ctx (minUTxOValue (_mainEra ctx))
            t <- unsafeGetTransactionTime =<< listAllSharedTransactions ctx w
            let tl = utcTimeSucc t
            txs1 <- listSharedTransactions ctx w (Just tl) (Nothing) Nothing
                Nothing
            txs2 <- listSharedTransactions ctx w (Just tl) (Just tl) Nothing
                Nothing
            length <$> [txs1, txs2] `shouldSatisfy` all (== 0)

    it "SHARED_TRANSACTIONS_LIST_RANGE_03 - \
        \Transaction at time t is NOT selected by range (..., t - 𝛿t)" $
        \ctx -> runResourceT $ do

            w <- fixtureSharedWalletWith ctx (minUTxOValue (_mainEra ctx))
            t <- unsafeGetTransactionTime =<< listAllSharedTransactions ctx w
            let te = utcTimePred t
            txs1 <- listSharedTransactions ctx w (Nothing) (Just te) Nothing
                Nothing
            txs2 <- listSharedTransactions ctx w (Just te) (Just te) Nothing
                Nothing
            length <$> [txs1, txs2] `shouldSatisfy` all (== 0)

    it "SHARED_TRANSACTIONS_GET_01 - \
        \Can get Incoming and Outgoing transaction" $
        \ctx -> runResourceT $ do

        (wSrc, (ApiSharedWallet (Right walDest))) <-
            (,) <$> fixtureSharedWallet @n ctx <*> emptySharedWallet ctx
        -- post tx
        let amt = minUTxOValue (_mainEra ctx) :: Natural
        rAddr <- request @[ApiAddressWithPath n] ctx
            (Link.listAddresses @'Shared walDest) Default Empty
        expectResponseCode HTTP.status200 rAddr
        let addrs = getResponse rAddr
        let destAddr = (addrs !! 1) ^. #id
        let payload = Json [json|{
                "payments": [{
                    "address": #{destAddr},
                    "amount": {
                        "quantity": #{amt},
                        "unit": "lovelace"
                    }
                }],
                "passphrase": "cardano-wallet"
            }|]

        rTx <- request @(ApiConstructTransaction n) ctx
            (Link.createUnsignedTransaction @'Shared wSrc) Default payload
        verify rTx
            [ expectSuccess
            , expectResponseCode HTTP.status202
            ]
        let (ApiSerialisedTransaction apiTx _) =
                getFromResponse #transaction rTx
        signedTx <-
            signSharedTx ctx wSrc apiTx [ expectResponseCode HTTP.status202 ]
        submittedTx <- submitSharedTxWithWid ctx wSrc signedTx
        let txid = getFromResponse #id submittedTx
        let queryTx = Link.getTransaction @'Shared wSrc (ApiTxId txid)
        rGetTx <- request @(ApiTransaction n) ctx queryTx Default Empty
        verify rGetTx
            [ expectSuccess
            , expectResponseCode HTTP.status200
            , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
            , expectField (#status . #getApiT) (`shouldBe` Pending)
            ]

        eventually "Wallet balance is as expected" $ do
            rGet <- request @ApiWallet ctx
                (Link.getWallet @'Shared walDest) Default Empty
            verify rGet
                [ expectField
                        (#balance . #total) (`shouldBe` ApiAmount amt)
                , expectField
                        (#balance . #available) (`shouldBe` ApiAmount amt)
                ]

        eventually "Transactions are available and in ledger" $ do
            -- Verify Tx in source wallet is Outgoing and InLedger
            let linkSrc = Link.getTransaction @'Shared
                    wSrc (ApiTxId txid)
            r1 <- request @(ApiTransaction n) ctx linkSrc Default Empty
            verify r1
                [ expectResponseCode HTTP.status200
                , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
                , expectField (#status . #getApiT) (`shouldBe` InLedger)
                ]

            -- Verify Tx in destination wallet is Incoming and InLedger
            let linkDest = Link.getTransaction
                    @'Shared walDest (ApiTxId txid)
            r2 <- request @(ApiTransaction n) ctx linkDest Default Empty
            verify r2
                [ expectResponseCode HTTP.status200
                , expectField (#direction . #getApiT) (`shouldBe` Incoming)
                , expectField (#status . #getApiT) (`shouldBe` InLedger)
                ]

    it "SHARED_TRANSACTIONS_GET_02 - \
        \Deleted wallet" $
        \ctx -> runResourceT $ do

        (ApiSharedWallet (Right w)) <- emptySharedWallet ctx
        _ <- request @ApiWallet
            ctx (Link.deleteWallet @'Shared w) Default Empty
        let txid = ApiT $ Hash $ BS.pack $ replicate 32 1
        let link = Link.getTransaction @'Shared w (ApiTxId txid)
        r <- request @(ApiTransaction n) ctx link Default Empty
        expectResponseCode HTTP.status404 r
        expectErrorMessage (errMsg404NoWallet $ w ^. walletId) r

    it "SHARED_TRANSACTIONS_GET_03 - \
        \Using wrong transaction id" $
        \ctx -> runResourceT $ do

        (wSrc, (ApiSharedWallet (Right walDest))) <-
            (,) <$> fixtureSharedWallet @n ctx <*> emptySharedWallet ctx
        -- post tx
        let amt = minUTxOValue (_mainEra ctx) :: Natural
        rAddr <- request @[ApiAddressWithPath n] ctx
            (Link.listAddresses @'Shared walDest) Default Empty
        expectResponseCode HTTP.status200 rAddr
        let addrs = getResponse rAddr
        let destAddr = (addrs !! 1) ^. #id
        let payload = Json [json|{
                "payments": [{
                    "address": #{destAddr},
                    "amount": {
                        "quantity": #{amt},
                        "unit": "lovelace"
                    }
                }],
                "passphrase": "cardano-wallet"
            }|]

        rTx <- request @(ApiConstructTransaction n) ctx
            (Link.createUnsignedTransaction @'Shared wSrc) Default payload
        verify rTx
            [ expectSuccess
            , expectResponseCode HTTP.status202
            ]
        let (ApiSerialisedTransaction apiTx _) =
                getFromResponse #transaction rTx
        signedTx <-
            signSharedTx ctx wSrc apiTx [ expectResponseCode HTTP.status202 ]
        submittedTx <- submitSharedTxWithWid ctx wSrc signedTx
        let txid1 = getFromResponse #id submittedTx
        let queryTx = Link.getTransaction @'Shared wSrc (ApiTxId txid1)
        rGetTx <- request @(ApiTransaction n) ctx queryTx Default Empty
        verify rGetTx
            [ expectSuccess
            , expectResponseCode HTTP.status200
            , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
            , expectField (#status . #getApiT) (`shouldBe` Pending)
            ]

        let txid2 =  Hash $ BS.pack $ replicate 32 1
        let link = Link.getTransaction @'Shared wSrc (ApiTxId $ ApiT txid2)
        r <- request @(ApiTransaction n) ctx link Default Empty
        expectResponseCode HTTP.status404 r
        expectErrorMessage (errMsg404CannotFindTx $ toText txid2) r

    it "SHARED_TRANSACTIONS_DELEGATION_01 - \
        \Cannot delegate when wallet is missing a delegation script template" $
        \ctx -> runResourceT $ do

        (ApiSharedWallet (Right w)) <- emptySharedWallet ctx

        pool1:_ <- map (view $ _Unwrapped . #id) . snd <$>
            unsafeRequest @[ApiT StakePool]
            ctx (Link.listStakePools arbitraryStake) Empty

        let delegationJoin = Json [json|{
                "delegations": [{
                    "join": {
                        "pool": #{ApiT pool1},
                        "stake_key_index": "0H"
                    }
                }]
            }|]
        rTx <- request @(ApiConstructTransaction n) ctx
            (Link.createUnsignedTransaction @'Shared w) Default delegationJoin
        expectResponseCode HTTP.status403 rTx
        decodeErrorInfo rTx `shouldBe` DelegationInvalid

    it "SHARED_TRANSACTIONS_DELEGATION_01a - \
       \Can join stakepool, rejoin another and quit" $ \ctx -> runResourceT $ do

        (party1,party2) <- fixtureSharedWalletDelegating @n ctx
        let depositAmt = ApiAmount 1_000_000

        pool1:pool2:_ <- map (view #id) <$> notRetiringPools ctx

        let delegationJoin = Json [json|{
                "delegations": [{
                    "join": {
                        "pool": #{ApiT pool1},
                        "stake_key_index": "0H"
                    }
                }]
            }|]
        rTx1 <- request @(ApiConstructTransaction n) ctx
            (Link.createUnsignedTransaction @'Shared party1) Default delegationJoin
        verify rTx1
            [ expectResponseCode HTTP.status202
            , expectField (#coinSelection . #depositsTaken) (`shouldBe` [depositAmt])
            , expectField (#coinSelection . #depositsReturned) (`shouldBe` [])
            ]

        let stakeKeyDerPathParty1 = NE.fromList
                [ ApiT (DerivationIndex 2_147_485_502) --1854H
                , ApiT (DerivationIndex 2_147_485_463) --1815
                , ApiT (DerivationIndex 2_147_483_678) --30H
                , ApiT (DerivationIndex 2)
                , ApiT (DerivationIndex 0)
                ]
        let stakeKeyDerPathParty2 = NE.fromList
                [ ApiT (DerivationIndex 2_147_485_502) --1854H
                , ApiT (DerivationIndex 2_147_485_463) --1815
                , ApiT (DerivationIndex 2_147_483_688) --40H
                , ApiT (DerivationIndex 2)
                , ApiT (DerivationIndex 0)
                ]
        let registerStakeKeyCert path =
                WalletDelegationCertificate $ RegisterRewardAccount path
        let delegatingCert path =
                WalletDelegationCertificate $ JoinPool path (ApiT pool1)

        let txCbor1 = getFromResponse #transaction rTx1
        let decodePayload1 = Json (toJSON txCbor1)
        rDecodedTx1 <- request @(ApiDecodedTransaction n) ctx
            (Link.decodeTransaction @'Shared party1) Default decodePayload1
        rDecodedTx2 <- request @(ApiDecodedTransaction n) ctx
            (Link.decodeTransaction @'Shared party2) Default decodePayload1
        let expectedFee1 = getFromResponse (#fee . #toNatural) rTx1
        let decodedExpectations1 =
                [ expectResponseCode HTTP.status202
                , expectField (#fee . #toNatural) (`shouldBe` expectedFee1)
                , expectField #withdrawals (`shouldBe` [])
                , expectField #collateral (`shouldBe` [])
                , expectField #scriptValidity
                    (`shouldBe` (Just $ ApiT TxScriptValid))
                , expectField #depositsReturned (`shouldBe` [])
                , expectField #depositsTaken (`shouldBe` [depositAmt])
                ]
        let (ApiScriptTemplate pScriptTemplate) =
                party1 ^. #paymentScriptTemplate
        let paymentScript =
                ApiT $ NativeExplicitScript
                (replaceCosignersWithVerKeys
                    CA.UTxOExternal pScriptTemplate (Index 1))
                ViaSpending
        let (Just (ApiScriptTemplate dScriptTemplate)) =
                party1 ^. #delegationScriptTemplate
        let delegationScript =
                ApiT $ NativeExplicitScript
                (replaceCosignersWithVerKeys
                    CA.Stake dScriptTemplate (Index 0))
                ViaSpending
        let witsExp1 =
                [ expectField (#witnessCount . #scripts)
                      (`shouldContain` [delegationScript])
                , expectField (#witnessCount . #scripts)
                      (`shouldContain` [paymentScript])
                , expectField (#witnessCount . #verificationKey)
                      (`shouldBe` 0)
                , expectField (#witnessCount . #bootstrap)
                      (`shouldBe` 0)]

        let certExpectation1 =
                [expectField #certificates
                     (`shouldBe` [ registerStakeKeyCert stakeKeyDerPathParty1
                                 , delegatingCert stakeKeyDerPathParty1])]
        verify rDecodedTx1 (decodedExpectations1 ++ certExpectation1 ++ witsExp1)
        let certExpectation2 =
                [expectField #certificates
                     (`shouldBe` [ registerStakeKeyCert stakeKeyDerPathParty2
                                 , delegatingCert stakeKeyDerPathParty2])]
        verify rDecodedTx2 (decodedExpectations1 ++ certExpectation2 ++ witsExp1)
        let ApiSerialisedTransaction apiTx1 _ =
                getFromResponse #transaction rTx1
        signedTx1 <-
            signSharedTx ctx party1 apiTx1
                [ expectResponseCode HTTP.status202 ]
        let decodePayload2 = Json (toJSON signedTx1)
        rDecodedTx3 <- request @(ApiDecodedTransaction n) ctx
            (Link.decodeTransaction @'Shared party1) Default decodePayload2
        rDecodedTx4 <- request @(ApiDecodedTransaction n) ctx
            (Link.decodeTransaction @'Shared party2) Default decodePayload2

        -- there is now 1 witness for staking and 1 witness for payment from party1
        let witsExp2 =
                [ expectField (#witnessCount . #scripts)
                      (`shouldContain` [delegationScript])
                , expectField (#witnessCount . #scripts)
                      (`shouldContain` [paymentScript])
                , expectField (#witnessCount . #verificationKey)
                      (`shouldBe` 2)
                , expectField (#witnessCount . #bootstrap)
                      (`shouldBe` 0)]

        verify rDecodedTx3 (decodedExpectations1 ++ certExpectation1 ++ witsExp2)
        verify rDecodedTx4 (decodedExpectations1 ++ certExpectation2 ++ witsExp2)

        --missing party2' witness for script staking
        submittedTx1 <- submitSharedTxWithWid ctx party1 signedTx1
        verify submittedTx1
            [ expectResponseCode HTTP.status403
            , expectErrorMessage (errMsg403MissingWitsInTransaction 3 2)
            ]

        let ApiSerialisedTransaction apiTx2 _ = signedTx1
        signedTx2 <-
            signSharedTx ctx party2 apiTx2
                [ expectResponseCode HTTP.status202 ]

        let decodePayload3 = Json (toJSON signedTx2)
        rDecodedTx5 <- request @(ApiDecodedTransaction n) ctx
            (Link.decodeTransaction @'Shared party1) Default decodePayload3
        rDecodedTx6 <- request @(ApiDecodedTransaction n) ctx
            (Link.decodeTransaction @'Shared party2) Default decodePayload3

        -- there is now 1 witness for staking and 1 witness for payment from party1
        -- and 1 witness for staking and 1 witness for payment from party2.
        let witsExp3 =
                [ expectField (#witnessCount . #scripts)
                      (`shouldContain` [delegationScript])
                , expectField (#witnessCount . #scripts)
                      (`shouldContain` [paymentScript])
                , expectField (#witnessCount . #verificationKey)
                      (`shouldBe` 4)
                , expectField (#witnessCount . #bootstrap)
                      (`shouldBe` 0)]

        verify rDecodedTx5 (decodedExpectations1 ++ certExpectation1 ++ witsExp3)
        verify rDecodedTx6 (decodedExpectations1 ++ certExpectation2 ++ witsExp3)

        submittedTx2 <- submitSharedTxWithWid ctx party1 signedTx2
        verify submittedTx2
            [ expectResponseCode HTTP.status202
            ]

        eventually "Party1's wallet has joined pool and deposit info persists" $ do
            rJoin' <- request @(ApiTransaction n) ctx
                (Link.getTransaction @'Shared party1 (getResponse submittedTx2))
                Default Empty
            verify rJoin'
                [ expectResponseCode HTTP.status200
                , expectField (#status . #getApiT) (`shouldBe` InLedger)
                , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
                , expectField #depositTaken (`shouldBe` depositAmt)
                , expectField #depositReturned (`shouldBe` ApiAmount 0)
                , expectField #certificates
                     (`shouldBe` [ registerStakeKeyCert stakeKeyDerPathParty1
                                 , delegatingCert stakeKeyDerPathParty1])
                ]
        eventually "Party2's wallet has joined pool and deposit info persists" $ do
            rJoin' <- request @(ApiTransaction n) ctx
                (Link.getTransaction @'Shared party2 (getResponse submittedTx2))
                Default Empty
            verify rJoin'
                [ expectResponseCode HTTP.status200
                , expectField (#status . #getApiT) (`shouldBe` InLedger)
                , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
                , expectField #depositTaken (`shouldBe` depositAmt)
                , expectField #depositReturned (`shouldBe` ApiAmount 0)
                , expectField #certificates
                     (`shouldBe` [ registerStakeKeyCert stakeKeyDerPathParty2
                                 , delegatingCert stakeKeyDerPathParty2])
                ]

        let txId2 = getFromResponse #id submittedTx2
        let link party = Link.getTransaction @'Shared party (ApiTxId txId2)
        eventually "party1: delegation transaction is in ledger" $ do
            rSrc <- request @(ApiTransaction n) ctx (link party1) Default Empty
            verify rSrc
                [ expectResponseCode HTTP.status200
                , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
                , expectField (#status . #getApiT) (`shouldBe` InLedger)
                , expectField #metadata  (`shouldBe` Nothing)
                , expectField #inputs $ \inputs' -> do
                    inputs' `shouldSatisfy` all (isJust . source)
                ]
        eventually "party2: delegation transaction is in ledger" $ do
            rSrc <- request @(ApiTransaction n) ctx (link party2) Default Empty
            verify rSrc
                [ expectResponseCode HTTP.status200
                , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
                , expectField (#status . #getApiT) (`shouldBe` InLedger)
                , expectField #metadata  (`shouldBe` Nothing)
                , expectField #inputs $ \inputs' -> do
                    inputs' `shouldSatisfy` all (isJust . source)
                ]

        -- Epoch A: delegation tx happened.
        -- <-- 1st boundary (A / A+1)
        -- Epoch A+1: stake is registered to a chosen pool.
        -- <-- 2nd boundary (A+1 / A+2)
        -- Epoch A+2: stake is active, rewards start accumulating.
        waitNumberOfEpochBoundaries 2 ctx

        eventually "party1: Wallet is delegating to pool1" $ do
            request @ApiWallet ctx (Link.getWallet @'Shared party1) Default Empty
                >>= flip verify
                    [ expectField #delegation (`shouldBe` delegating (ApiT pool1) [])
                    ]
        eventually "party2: Wallet is delegating to pool1" $ do
            request @ApiWallet ctx (Link.getWallet @'Shared party2) Default Empty
                >>= flip verify
                    [ expectField #delegation (`shouldBe` delegating (ApiT pool1) [])
                    ]

        -- <-- 1st boundary (A+2 / A+3)
        -- Epoch A+3: rewards from epoch A+2 are calculated.
        -- <-- 2nd boundary (A+3 / A+4)
        -- Epoch A+4: rewards from epoch A+2 are paid out.
        waitNumberOfEpochBoundaries 2 ctx

        eventually "party1: Wallet gets rewards from pool1" $ do
            request @ApiWallet ctx (Link.getWallet @'Shared party1) Default Empty
                >>= flip verify
                [ expectField (#balance . #reward) (.> (ApiAmount 0)) ]

        eventually "party2: Wallet gets rewards from pool1" $ do
            request @ApiWallet ctx (Link.getWallet @'Shared party2) Default Empty
                >>= flip verify
                [ expectField (#balance . #reward) (.> (ApiAmount 0)) ]

        -- join another stake pool
        let delegationRejoin = Json [json|{
                "delegations": [{
                    "join": {
                        "pool": #{ApiT pool2},
                        "stake_key_index": "0H"
                    }
                }]
            }|]
        rTx2 <- request @(ApiConstructTransaction n) ctx
            (Link.createUnsignedTransaction @'Shared party1) Default delegationRejoin
        verify rTx2
            [ expectResponseCode HTTP.status202
            , expectField (#coinSelection . #depositsTaken) (`shouldBe` [])
            , expectField (#coinSelection . #depositsReturned) (`shouldBe` [])
            ]

        let ApiSerialisedTransaction apiTx3 _ =
                getFromResponse #transaction rTx2
        signedTx3 <-
            signSharedTx ctx party1 apiTx3
                [ expectResponseCode HTTP.status202 ]
        let ApiSerialisedTransaction apiTx4 _ = signedTx3
        signedTx4 <-
            signSharedTx ctx party2 apiTx4
                [ expectResponseCode HTTP.status202 ]

        submittedTx3 <- submitSharedTxWithWid ctx party1 signedTx4
        verify submittedTx3 [ expectResponseCode HTTP.status202 ]
        let delegatingCert2 path =
                WalletDelegationCertificate $ JoinPool path (ApiT pool2)

        let expectedFee2 = getFromResponse (#fee . #toNatural) rTx2
        let decodedExpectations2 =
                [ expectResponseCode HTTP.status202
                , expectField (#fee . #toNatural) (`shouldBe` expectedFee2)
                , expectField #withdrawals (`shouldBe` [])
                , expectField #collateral (`shouldBe` [])
                , expectField #scriptValidity
                    (`shouldBe` (Just $ ApiT TxScriptValid))
                , expectField #depositsReturned (`shouldBe` [])
                , expectField #depositsTaken (`shouldBe` [])
                ]
        let decodePayload4 = Json (toJSON signedTx4)
        --after first tx all funds are in change address of ix=0
        let paymentScript1 =
                ApiT $ NativeExplicitScript
                (replaceCosignersWithVerKeys
                    CA.UTxOInternal pScriptTemplate (Index 0))
                ViaSpending
        let witsExp4 =
                [ expectField (#witnessCount . #scripts)
                      (`shouldContain` [delegationScript])
                , expectField (#witnessCount . #scripts)
                      (`shouldContain` [paymentScript1])
                , expectField (#witnessCount . #verificationKey)
                      (`shouldBe` 4)
                , expectField (#witnessCount . #bootstrap)
                      (`shouldBe` 0)]
        rDecodedTx7 <- request @(ApiDecodedTransaction n) ctx
            (Link.decodeTransaction @'Shared party1) Default decodePayload4
        rDecodedTx8 <- request @(ApiDecodedTransaction n) ctx
            (Link.decodeTransaction @'Shared party2) Default decodePayload4
        let certExpectation3 =
                [expectField #certificates
                     (`shouldBe` [ delegatingCert2 stakeKeyDerPathParty1])]
        verify rDecodedTx7 (decodedExpectations2 ++ certExpectation3 ++ witsExp4)
        let certExpectation4 =
                [expectField #certificates
                     (`shouldBe` [ delegatingCert2 stakeKeyDerPathParty2])]
        verify rDecodedTx8 (decodedExpectations2 ++ certExpectation4 ++ witsExp4)

        waitNumberOfEpochBoundaries 2 ctx

        eventually "party1: Wallet is delegating to pool2" $ do
            request @ApiWallet ctx (Link.getWallet @'Shared party1) Default Empty
                >>= flip verify
                    [ expectField #delegation (`shouldBe` delegating (ApiT pool2) [])
                    ]
        eventually "party2: Wallet is delegating to pool2" $ do
            request @ApiWallet ctx (Link.getWallet @'Shared party2) Default Empty
                >>= flip verify
                    [ expectField #delegation (`shouldBe` delegating (ApiT pool2) [])
                    ]

        eventually "Party1's wallet has joined another pool" $ do
            rJoin' <- request @(ApiTransaction n) ctx
                (Link.getTransaction @'Shared party1
                    (getResponse submittedTx3))
                Default Empty
            verify rJoin'
                [ expectResponseCode HTTP.status200
                , expectField (#status . #getApiT) (`shouldBe` InLedger)
                , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
                , expectField #depositTaken (`shouldBe` ApiAmount 0)
                , expectField #depositReturned (`shouldBe` ApiAmount 0)
                , expectField #certificates
                     (`shouldBe` [ delegatingCert2 stakeKeyDerPathParty1])
                ]
        eventually "Party2's wallet has joined another pool" $ do
            rJoin' <- request @(ApiTransaction n) ctx
                (Link.getTransaction @'Shared party2
                    (getResponse submittedTx3))
                Default Empty
            verify rJoin'
                [ expectResponseCode HTTP.status200
                , expectField (#status . #getApiT) (`shouldBe` InLedger)
                , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
                , expectField #depositTaken (`shouldBe` ApiAmount 0)
                , expectField #depositReturned (`shouldBe` ApiAmount 0)
                , expectField #certificates
                     (`shouldBe` [ delegatingCert2 stakeKeyDerPathParty2])
                ]

        -- there's currently no withdrawals in the wallet
        rw1 <- request @[ApiTransaction n] ctx
            (Link.listTransactions' @'Shared party1 (Just 1)
                Nothing Nothing Nothing Nothing Nothing)
            Default Empty
        verify rw1 [ expectListSize 0 ]
        rw2 <- request @[ApiTransaction n] ctx
            (Link.listTransactions' @'Shared party2 (Just 1)
                Nothing Nothing Nothing Nothing Nothing)
            Default Empty
        verify rw2 [ expectListSize 0 ]

        dest <- emptyWallet ctx
        -- We can use rewards
        -- Tested by making an explicit withdrawal request to self.

        -- We wait for the start of a new epoch here
        -- so that there is a good chance that we spend all rewards
        -- in the next transaction, and don't receive any new rewards
        -- before that transaction has concluded.
        waitForNextEpoch ctx

        addrs <- listAddresses @n ctx dest
        let coin = minUTxOValue (_mainEra ctx) :: Natural
        let addr = (addrs !! 1) ^. #id
        let payloadWithdrawal = Json [json|
                { "payments":
                    [ { "address": #{addr}
                      , "amount":
                        { "quantity": #{coin}
                        , "unit": "lovelace"
                        }
                      }
                    ]
                , "passphrase": #{fixturePassphrase},
                  "withdrawal": "self"
                }|]

        rGet <- request @ApiWallet ctx (Link.getWallet @'Shared party1)
                    Default Empty
        let previousBalance = getFromResponse (#balance . #available) rGet

        rTx3 <- request @(ApiConstructTransaction n) ctx
            (Link.createUnsignedTransaction @'Shared party1) Default payloadWithdrawal
        verify rTx3
            [ expectResponseCode HTTP.status202
            , expectField (#coinSelection . #withdrawals) (`shouldSatisfy` (not . null))
            ]

        let ApiSerialisedTransaction apiTx5 _ =
                getFromResponse #transaction rTx3
        signedTx5 <-
            signSharedTx ctx party1 apiTx5
                [ expectResponseCode HTTP.status202 ]
        let ApiSerialisedTransaction apiTx6 _ = signedTx5
        signedTx6 <-
            signSharedTx ctx party2 apiTx6
                [ expectResponseCode HTTP.status202 ]

        submittedTx4 <- submitSharedTxWithWid ctx party1 signedTx6
        verify submittedTx4 [ expectResponseCode HTTP.status202 ]

        -- Rewards are have been consumed.
        eventually "Party1's wallet has consumed rewards" $ do
            request @ApiWallet ctx (Link.getWallet @'Shared party1) Default Empty
                >>= flip verify
                [ expectField (#balance . #reward) (`shouldBe` (ApiAmount 0))
                , expectField (#balance . #available) (.> previousBalance)
                ]

        eventually "Party2's wallet has consumed rewards" $ do
            request @ApiWallet ctx (Link.getWallet @'Shared party2) Default Empty
                >>= flip verify
                [ expectField (#balance . #reward) (`shouldBe` (ApiAmount 0))
                , expectField (#balance . #available) (.> previousBalance)
                ]

        -- now we can quit
        let delegationQuit = Json [json|{
                "delegations": [{
                    "quit": { "stake_key_index": "0H" }
                }]
            }|]
        rTx4 <- request @(ApiConstructTransaction n) ctx
            (Link.createUnsignedTransaction @'Shared party1) Default delegationQuit
        verify rTx4
            [ expectResponseCode HTTP.status202
            , expectField (#coinSelection . #depositsTaken) (`shouldBe` [])
            , expectField (#coinSelection . #depositsReturned) (`shouldBe` [depositAmt])
            ]

        let ApiSerialisedTransaction apiTx7 _ = getFromResponse #transaction rTx4
        signedTx7 <-
            signSharedTx ctx party1 apiTx7
                [ expectResponseCode HTTP.status202 ]
        let ApiSerialisedTransaction apiTx8 _ = signedTx7
        signedTx8 <-
            signSharedTx ctx party2 apiTx8
                [ expectResponseCode HTTP.status202 ]

        let delegatingCert3 path =
                WalletDelegationCertificate $ QuitPool path
        let decodePayload5 = Json (toJSON signedTx8)
        rDecodedTx9 <- request @(ApiDecodedTransaction n) ctx
            (Link.decodeTransaction @'Shared party1) Default decodePayload5
        rDecodedTx10 <- request @(ApiDecodedTransaction n) ctx
            (Link.decodeTransaction @'Shared party2) Default decodePayload5
        let certExpectation5 =
                [expectField #certificates
                     (`shouldBe` [ delegatingCert3 stakeKeyDerPathParty1])]
        verify rDecodedTx9 certExpectation5
        let certExpectation6 =
                [expectField #certificates
                     (`shouldBe` [ delegatingCert3 stakeKeyDerPathParty2])]
        verify rDecodedTx10 certExpectation6

        submittedTx5 <- submitSharedTxWithWid ctx party1 signedTx8
        verify submittedTx5 [ expectResponseCode HTTP.status202 ]

        waitNumberOfEpochBoundaries 2 ctx

        eventually "party1: Wallet is not delegating" $ do
            request @ApiWallet ctx (Link.getWallet @'Shared party1) Default Empty
                >>= flip verify
                    [ expectField #delegation (`shouldBe` notDelegating [])
                    ]
        eventually "party2: Wallet is not delegating" $ do
            request @ApiWallet ctx (Link.getWallet @'Shared party2) Default Empty
                >>= flip verify
                    [ expectField #delegation (`shouldBe` notDelegating [])
                    ]

        waitNumberOfEpochBoundaries 2 ctx

        eventually "Party1's wallet has quitted" $ do
            rJoin' <- request @(ApiTransaction n) ctx
                (Link.getTransaction @'Shared party1
                    (getResponse submittedTx5))
                Default Empty
            verify rJoin'
                [ expectResponseCode HTTP.status200
                , expectField (#status . #getApiT) (`shouldBe` InLedger)
                , expectField #depositTaken (`shouldBe` ApiAmount 0)
                , expectField #depositReturned (`shouldBe` depositAmt)
                , expectField #certificates
                     (`shouldBe` [ delegatingCert3 stakeKeyDerPathParty1])
                ]
        eventually "Party2's wallet has quitted" $ do
            rJoin' <- request @(ApiTransaction n) ctx
                (Link.getTransaction @'Shared party2
                    (getResponse submittedTx5))
                Default Empty
            verify rJoin'
                [ expectResponseCode HTTP.status200
                , expectField (#status . #getApiT) (`shouldBe` InLedger)
                , expectField #depositTaken (`shouldBe` ApiAmount 0)
                , expectField #depositReturned (`shouldBe` depositAmt)
                , expectField #certificates
                     (`shouldBe` [ delegatingCert3 stakeKeyDerPathParty2])
                ]

    it "SHARED_TRANSACTIONS_DELEGATION_02  \
       \Emulating multi-delegation using shared wallets" $ \ctx -> runResourceT $ do
        liftIO $ pendingWith "TODO: Fix multi-delegation with shared wallets flakiness, ADP-3084"
        -- creating empty parent Shelley wallet
        m15 <- liftIO $ genMnemonics M15
        m12 <- liftIO $ genMnemonics M12
        let payloadCreateParent = Json [json|{
                "name": "Parent Shelley Wallet",
                "mnemonic_sentence": #{m15},
                "mnemonic_second_factor": #{m12},
                "passphrase": #{fixturePassphrase}
             }|]
        rPostCreateParent <- postWallet ctx payloadCreateParent
        verify rPostCreateParent
            [ expectResponseCode HTTP.status201 ]
        let parentWal = getResponse rPostCreateParent

        -- financing the parent Shelley wallet
        rAddrShelley <- request @[ApiAddressWithPath n] ctx
            (Link.listAddresses @'Shelley parentWal) Default Empty
        expectResponseCode HTTP.status200 rAddrShelley
        let addrs = getResponse rAddrShelley
        let destination = (addrs !! 1) ^. #id
        wShelley <- fixtureWallet ctx
        let payloadTx = Json [json|{
                "payments": [{
                    "address": #{destination},
                    "amount": {
                        "quantity": #{faucetUtxoAmt},
                        "unit": "lovelace"
                    }
                }],
                "passphrase": #{fixturePassphrase}
            }|]
        let ep = Link.createTransactionOld @'Shelley
        rTx1 <- request @(ApiTransaction n) ctx (ep wShelley) Default payloadTx
        expectResponseCode HTTP.status202 rTx1
        eventually "Parent Shelley Wallet balance is as expected" $ do
            rGet <- request @ApiWallet ctx
                (Link.getWallet @'Shelley parentWal) Default Empty
            verify rGet
                [ expectField (#balance . #total)
                    (`shouldBe` ApiAmount faucetUtxoAmt)
                , expectField (#balance . #available)
                    (`shouldBe` ApiAmount faucetUtxoAmt)
                ]

        -- create two child shared wallets using the same mnemonic as parent
        -- Shelley wallet and different account indices
        let payloadCreateChild :: T.Text -> T.Text -> Payload
            payloadCreateChild ix name = Json [json| {
                "name": #{name},
                "mnemonic_sentence": #{m15},
                "mnemonic_second_factor": #{m12},
                "passphrase": #{fixturePassphrase},
                "account_index": #{ix},
                "payment_script_template":
                    { "cosigners":
                        { "cosigner#0": "self" },
                      "template":
                          { "all": ["cosigner#0"]
                          }
                    },
                "delegation_script_template":
                    { "cosigners":
                        { "cosigner#0": "self" },
                      "template":
                          { "all": ["cosigner#0"]
                          }
                    }
                } |]

        rPostCreateChild1 <- postSharedWallet ctx Default
            (payloadCreateChild "0H" "Shared Wallet 1")
        verify (fmap (view #wallet) <$> rPostCreateChild1)
            [ expectResponseCode HTTP.status201
            ]
        let sharedWal1 = getResponse rPostCreateChild1
        let (ApiSharedWallet (Right walActive1)) = sharedWal1

        rPostCreateChild2 <- postSharedWallet ctx Default
            (payloadCreateChild "1H" "Shared Wallet 2")
        let sharedWal2 = getResponse rPostCreateChild2
        let (ApiSharedWallet (Right walActive2)) = sharedWal2
        verify (fmap (view #wallet) <$> rPostCreateChild2)
            [ expectResponseCode HTTP.status201
            ]

        -- transfer money to the child shared wallets from parent wallet
        -- parent wallet has 100k ada (faucetUtxoAmt) and will transfer 40k
        -- to every child shared wallet
        let ada = (*) (1_000_000)
        let transfer :: Natural
            transfer = ada 40_000

        rAddrShared1 <- request @[ApiAddressWithPath n] ctx
            (Link.listAddresses @'Shared walActive1) Default Empty
        expectResponseCode HTTP.status200 rAddrShared1
        let addrs1 = getResponse rAddrShared1
        let destAddr1 = (addrs1 !! 1) ^. #id

        rAddrShared2 <- request @[ApiAddressWithPath n] ctx
            (Link.listAddresses @'Shared walActive2) Default Empty
        expectResponseCode HTTP.status200 rAddrShared2
        let addrs2 = getResponse rAddrShared2
        let destAddr2 = (addrs2 !! 1) ^. #id

        let payloadTx1 = Json [json|{
                "payments": [{
                    "address": #{destAddr1},
                    "amount": {
                        "quantity": #{transfer},
                        "unit": "lovelace"
                    }
                }, {
                    "address": #{destAddr2},
                    "amount": {
                        "quantity": #{transfer},
                        "unit": "lovelace"
                    }
                }],
                "passphrase": #{fixturePassphrase}
            }|]
        rTx2 <- request @(ApiTransaction n) ctx (ep parentWal) Default payloadTx1
        expectResponseCode HTTP.status202 rTx2

        eventually "Child shared wallet 1 balance is increased by target" $ do
            rGet <- request @ApiWallet ctx
                (Link.getWallet @'Shared walActive1) Default Empty
            verify rGet
                [ expectField
                        (#balance . #total) (`shouldBe` ApiAmount transfer)
                , expectField
                        (#balance . #available) (`shouldBe` ApiAmount transfer)
                ]
        eventually "Child shared wallet 2 balance is increased by target" $ do
            rGet <- request @ApiWallet ctx
                (Link.getWallet @'Shared walActive2) Default Empty
            verify rGet
                [ expectField
                        (#balance . #total) (`shouldBe` ApiAmount transfer)
                , expectField
                        (#balance . #available) (`shouldBe` ApiAmount transfer)
                ]

        -- child shared wallets delegate to different pools and get rewards
        -- after several epochs
        pool1:pool2:_ <- map (view #id) <$> notRetiringPools ctx

        let delegationJoin pool = Json [json|{
                "delegations": [{
                    "join": {
                        "pool": #{ApiT pool},
                        "stake_key_index": "0H"
                    }
                }]
            }|]
        -- one child shared wallet delegating to pool1
        rTx3 <- request @(ApiConstructTransaction n) ctx
            (Link.createUnsignedTransaction @'Shared walActive1) Default
            (delegationJoin pool1)
        verify rTx3
            [ expectResponseCode HTTP.status202 ]
        let (ApiSerialisedTransaction apiTx3 _) =
                getFromResponse #transaction rTx3
        signedTx3 <-
            signSharedTx ctx walActive1 apiTx3
                [ expectResponseCode HTTP.status202 ]
        submittedTx3 <- submitSharedTxWithWid ctx walActive1 signedTx3
        verify submittedTx3
            [ expectResponseCode HTTP.status202
            ]

        -- one child shared wallet delegating to pool2
        rTx4 <- request @(ApiConstructTransaction n) ctx
            (Link.createUnsignedTransaction @'Shared walActive2) Default
            (delegationJoin pool2)
        verify rTx4
            [ expectResponseCode HTTP.status202 ]
        let (ApiSerialisedTransaction apiTx4 _) =
                getFromResponse #transaction rTx4
        signedTx4 <-
            signSharedTx ctx walActive2 apiTx4
                [ expectResponseCode HTTP.status202 ]
        submittedTx4 <- submitSharedTxWithWid ctx walActive2 signedTx4
        verify submittedTx4
            [ expectResponseCode HTTP.status202
            ]

        -- checking awards
        eventually "Shared Wallet 1 is delegating to pool1" $ do
            request @ApiWallet ctx (Link.getWallet @'Shared walActive1) Default Empty
                >>= flip verify
                    [ expectField #delegation (`shouldBe` delegating (ApiT pool1) [])
                    ]
        eventually "Shared Wallet 2 is delegating to pool2" $ do
            request @ApiWallet ctx (Link.getWallet @'Shared walActive2) Default Empty
                >>= flip verify
                    [ expectField #delegation (`shouldBe` delegating (ApiT pool2) [])
                    ]

        waitNumberOfEpochBoundaries 4 ctx

        eventually "Shared Wallet 1 gets rewards from pool1" $ do
            r <- request @ApiWallet ctx (Link.getWallet @'Shared walActive1) Default Empty
            verify r
                [ expectField
                    (#balance . #reward)
                    (.> (ApiAmount 0))
                ]
        eventually "Shared Wallet 2 gets rewards from pool2" $ do
            r <- request @ApiWallet ctx (Link.getWallet @'Shared walActive2) Default Empty
            verify r
                [ expectField
                    (#balance . #reward)
                    (.> (ApiAmount 0))
                ]

        --sending back funds to parent with self withdrawal
        rGet1 <- request @ApiWallet ctx (Link.getWallet @'Shared walActive1)
                    Default Empty
        let (ApiAmount rewards1) = getFromResponse (#balance . #reward) rGet1
        rGet2 <- request @ApiWallet ctx (Link.getWallet @'Shared walActive2)
                    Default Empty
        let (ApiAmount rewards2) = getFromResponse (#balance . #reward) rGet2

        let payloadWithdrawal amt = Json [json|
                { "payments":
                    [ { "address": #{destination}
                      , "amount":
                        { "quantity": #{amt}
                        , "unit": "lovelace"
                        }
                      }
                    ]
                , "passphrase": #{fixturePassphrase},
                  "withdrawal": "self"
                }|]
        rTx5a <- request @(ApiConstructTransaction n) ctx
            (Link.createUnsignedTransaction @'Shared walActive1) Default
            (payloadWithdrawal $ transfer + rewards1 - ada 2)
        verify rTx5a
            [ expectResponseCode HTTP.status202 ]
        let expectedFee = getFromResponse (#fee . #toNatural) rTx5a

        rTx5 <- request @(ApiConstructTransaction n) ctx
            (Link.createUnsignedTransaction @'Shared walActive1) Default
            (payloadWithdrawal $ transfer + rewards1 - expectedFee - ada 1)
        verify rTx5
            [ expectResponseCode HTTP.status202 ]
        let (ApiSerialisedTransaction apiTx5 _) =
                getFromResponse #transaction rTx5
        signedTx5 <-
            signSharedTx ctx walActive1 apiTx5
                [ expectResponseCode HTTP.status202 ]
        submittedTx5 <- submitSharedTxWithWid ctx walActive1 signedTx5
        verify submittedTx5
            [ expectResponseCode HTTP.status202
            ]

        rTx6 <- request @(ApiConstructTransaction n) ctx
            (Link.createUnsignedTransaction @'Shared walActive2) Default
            (payloadWithdrawal $ transfer + rewards2 - expectedFee - ada 1)
        verify rTx6
            [ expectResponseCode HTTP.status202 ]
        let (ApiSerialisedTransaction apiTx6 _) =
                getFromResponse #transaction rTx6
        signedTx6 <-
            signSharedTx ctx walActive2 apiTx6
                [ expectResponseCode HTTP.status202 ]
        submittedTx6 <- submitSharedTxWithWid ctx walActive2 signedTx6
        verify submittedTx6
            [ expectResponseCode HTTP.status202
            ]

        eventually "Parent Shelley Wallet balance is higher than before" $ do
            rGet <- request @ApiWallet ctx
                (Link.getWallet @'Shelley parentWal) Default Empty
            verify rGet
                [ expectField (#balance . #total)
                    (.> (ApiAmount faucetUtxoAmt))
                , expectField (#balance . #available)
                    (.> (ApiAmount faucetUtxoAmt))
                ]

    it "SHARED_TRANSACTIONS_LIST_05 - filter address output side" $
        \ctx -> runResourceT $ do
        let minUTxOValue' = minUTxOValue (_mainEra ctx)
        let a1 = minUTxOValue'
        let a2 = 2 * minUTxOValue'
        let a3 = 3 * minUTxOValue'
        let a4 = 4 * minUTxOValue'
        let a5 = 5 * minUTxOValue'
        (wSrc, ApiSharedWallet (Right wDest)) <-
             (,) <$> fixtureSharedWalletWith ctx 100_000_000 <*> emptySharedWallet ctx

        -- initially empty wallet wDest should have two txs on address 0,
        -- one tx on address 1 and three txs on address 2
        sendAmtToAddr ctx wSrc wDest a1 0
        sendAmtToAddr ctx wSrc wDest a2 0
        sendAmtToAddr ctx wSrc wDest a3 1
        sendAmtToAddr ctx wSrc wDest a1 2
        sendAmtToAddr ctx wSrc wDest a4 2
        sendAmtToAddr ctx wSrc wDest a5 2

        eventually "There are exactly 6 transactions for wDest" $ do
            let linkList = listTransactionsFilteredByAddress wDest Nothing
            rl <- request @([ApiTransaction n]) ctx linkList Default Empty
            verify rl [expectListSize 6]

        rAddr <- request @[ApiAddressWithPath n] ctx
            (Link.listAddresses @'Shared wDest) Default Empty
        expectResponseCode HTTP.status200 rAddr
        let addrs = getResponse rAddr

        let addr0 = (head addrs) ^. #id
        let linkList0 = listTransactionsFilteredByAddress wDest (Just (apiAddress addr0))
        rl0 <- request @([ApiTransaction n]) ctx linkList0 Default Empty
        verify rl0 [expectListSize 2]
        let txs0 = getResponse rl0
        let amts0 = fmap (view #amount) txs0
        Set.fromList amts0 `shouldBe` Set.fromList (ApiAmount <$> [a1, a2])

        let addr1 = (addrs !! 1) ^. #id
        let linkList1 = listTransactionsFilteredByAddress wDest (Just (apiAddress addr1))
        rl1 <- request @([ApiTransaction n]) ctx linkList1 Default Empty
        verify rl1 [expectListSize 1]
        let txs1 = getResponse rl1
        let amts1 = fmap (view #amount) txs1
        amts1 `shouldBe` (ApiAmount <$> [a3])

        let addr2 = (addrs !! 2) ^. #id
        let linkList2 = listTransactionsFilteredByAddress wDest (Just (apiAddress addr2))
        rl2 <- request @([ApiTransaction n]) ctx linkList2 Default Empty
        verify rl2 [expectListSize 3]
        let txs2 = getResponse rl2
        let amts2 = fmap (view #amount) txs2
        Set.fromList amts2 `shouldBe` Set.fromList (ApiAmount <$> [a1, a4, a5])

    it "SHARED_TRANSACTIONS_LIST_06 - filter address input side" $ \ctx -> runResourceT $ do
        let minUTxOValue' = minUTxOValue (_mainEra ctx)
        let a1 = minUTxOValue'
        let a2 = fromIntegral $ oneAda * 5_000
        let a3 = fromIntegral $ oneAda * 10_000
        (wSrc, ApiSharedWallet (Right wDest)) <-
             (,) <$> fixtureSharedWalletWith ctx (fromIntegral $ oneAda * 50_000)
                 <*> emptySharedWallet ctx

        -- initially empty wallet wDest should have five txs on address 0 with
        -- just 5*a1 on it, one tx on address 1 with a2 and three txs on address
        -- 2 with 3*a3
        sendAmtToAddr ctx wSrc wDest a1 0
        sendAmtToAddr ctx wSrc wDest a1 0
        sendAmtToAddr ctx wSrc wDest a1 0
        sendAmtToAddr ctx wSrc wDest a1 0
        sendAmtToAddr ctx wSrc wDest a1 0

        sendAmtToAddr ctx wSrc wDest a3 2
        sendAmtToAddr ctx wSrc wDest a3 2
        sendAmtToAddr ctx wSrc wDest a3 2

        eventually "There are exactly 8 transactions for wDest" $ do
            let linkList = listTransactionsFilteredByAddress wDest Nothing
            rl <- request @([ApiTransaction n]) ctx linkList Default Empty
            verify rl [expectListSize 8]

        addrs <- listExternalAddresses ctx wDest

        -- from newly funded wallet we send back funds in such a way that we can
        -- indetify that address 2 was engaged in a given tx
        let a4 = fromIntegral $ oneAda * 29_990
        let addr2 = (addrs !! 2) ^. #id
        let linkList2a = listTransactionsFilteredByAddress wDest (Just (apiAddress addr2))
        rl2a <- request @([ApiTransaction n]) ctx linkList2a Default Empty
        verify rl2a [expectListSize 3]
        let txs2a = getResponse rl2a
        let amts2a = fmap (view #amount) txs2a
        amts2a `shouldBe` (ApiAmount <$> [a3, a3, a3])

        sendAmtToAddr ctx wDest wSrc a4 0
        eventually "There are exactly 9 transactions for wDest" $ do
            let linkList = listTransactionsFilteredByAddress wDest Nothing
            rl <- request @([ApiTransaction n]) ctx linkList Default Empty
            verify rl [expectListSize 9]

        rl2b <- request @([ApiTransaction n]) ctx linkList2a Default Empty
        verify rl2b [expectListSize 4]

        -- destination wallet is refunded on address 1
        let addr1 = (addrs !! 1) ^. #id
        let linkList1a = listTransactionsFilteredByAddress wDest (Just (apiAddress addr1))
        rl1a <- request @([ApiTransaction n]) ctx linkList1a Default Empty
        verify rl1a [expectListSize 0]

        sendAmtToAddr ctx wSrc wDest a2 1

        eventually "There are exactly 10 transactions for wDest" $ do
            let linkList = listTransactionsFilteredByAddress wDest Nothing
            rl <- request @([ApiTransaction n]) ctx linkList Default Empty
            verify rl [expectListSize 10]
        rl1b <- request @([ApiTransaction n]) ctx linkList1a Default Empty
        verify rl1b [expectListSize 1]

        -- next the funded wallet sends back funds in such a way that we can
        -- indetify address 1 was engaged in a given tx
        let a5 = fromIntegral $ oneAda * 4_990

        sendAmtToAddr ctx wDest wSrc a5 0
        eventually "There are exactly 11 transactions for wDest" $ do
            let linkList = listTransactionsFilteredByAddress wDest Nothing
            rl <- request @([ApiTransaction n]) ctx linkList Default Empty
            verify rl [expectListSize 11]

        rl1c <- request @([ApiTransaction n]) ctx linkList1a Default Empty
        verify rl1c [expectListSize 2]
  where
     listTransactionsFilteredByAddress wallet =
         Link.listTransactions' @'Shared wallet
         Nothing
         Nothing
         Nothing
         Nothing
         Nothing

     oneAda :: Integer
     oneAda = 1_000_000

     listExternalAddresses ctx w = do
         let link = Link.listAddresses @'Shared w
         r <- request @[ApiAddressWithPath n] ctx link Default Empty
         expectResponseCode HTTP.status200 r
         let isExternal (ApiAddressWithPath _ _
                 (_ NE.:| [_, _, (ApiT (DerivationIndex ix)), _] ) ) = ix == 0
             isExternal _ = False
         return (filter isExternal $ getResponse r)

     sendAmtToAddr ctx src dest amt addrIx = do
         addrs <- listExternalAddresses ctx dest
         let destination = (addrs !! addrIx) ^. #id
         let payload = Json [json|{
                 "payments": [{
                     "address": #{destination},
                     "amount": {
                         "quantity": #{amt},
                         "unit": "lovelace"
                     }
                 }]
             }|]

         rTx <- request @(ApiConstructTransaction n) ctx
             (Link.createUnsignedTransaction @'Shared src) Default payload
         verify rTx
             [ expectResponseCode HTTP.status202
             ]

         let (ApiSerialisedTransaction apiTx _) =
                 getFromResponse #transaction rTx
         signedTx <-
             signSharedTx ctx src apiTx [ expectResponseCode HTTP.status202 ]

         submittedTx <- submitSharedTxWithWid ctx src signedTx
         verify submittedTx
             [ expectSuccess
             , expectResponseCode HTTP.status202
             ]

         let txid = getFromResponse #id submittedTx
         let queryTx w = Link.getTransaction @'Shared w (ApiTxId txid)

         eventually "Tx is in ledger finally for dest wallet" $ do
             rGetTx' <- request @(ApiTransaction n) ctx (queryTx dest) Default Empty
             verify rGetTx'
                 [ expectResponseCode HTTP.status200
                 , expectField (#status . #getApiT) (`shouldBe` InLedger)
                 ]
         eventually "Tx is in ledger finally for src wallet" $ do
             rGetTx' <- request @(ApiTransaction n) ctx (queryTx src) Default Empty
             verify rGetTx'
                 [ expectResponseCode HTTP.status200
                 , expectField (#status . #getApiT) (`shouldBe` InLedger)
                 ]

     listSharedTransactions ctx w mStart mEnd mOrder mLimit = do
         let path = Link.listTransactions' @'Shared w
                    Nothing
                    (Iso8601Time <$> mStart)
                    (Iso8601Time <$> mEnd)
                    mOrder
                    mLimit
                    Nothing
         r <- request @[ApiTransaction n] ctx path Default Empty
         expectResponseCode HTTP.status200 r
         let txs = getResponse r
         return txs

     listAllSharedTransactions ctx w = do
         let path = Link.listTransactions' @'Shared w
                    Nothing Nothing Nothing (Just Descending) Nothing Nothing
         r <- request @[ApiTransaction n] ctx path Default Empty
         expectResponseCode HTTP.status200 r
         let txs = getResponse r
         return txs

     fixtureSharedWalletWith ctx amt = do
        (wSrc, wDest@(ApiSharedWallet (Right walDest))) <-
            (,) <$> fixtureSharedWallet @n ctx <*> emptySharedWallet ctx

        -- destination wallet
        rAddr <- request @[ApiAddressWithPath n] ctx
            (Link.listAddresses @'Shared walDest) Default Empty
        expectResponseCode HTTP.status200 rAddr
        let addrs = getResponse rAddr
        let destAddr = (addrs !! 1) ^. #id
        let payload = Json [json|{
                "payments": [{
                    "address": #{destAddr},
                    "amount": {
                        "quantity": #{amt},
                        "unit": "lovelace"
                    }
                }],
                "passphrase": "cardano-wallet"
            }|]

        -- post txs
        realizeTx ctx wSrc payload
        eventually "wDest balance is increased" $ do
            wal <- getSharedWallet ctx wDest
            let balanceExp =
                    [ expectResponseCode HTTP.status200
                    , expectField (traverse . #balance . #available)
                        (`shouldBe` ApiAmount amt)
                    ]
            verify (fmap (view #wallet) <$> wal) balanceExp
        pure walDest

     realizeTx ctx w payload = do
        rTx <- request @(ApiConstructTransaction n) ctx
            (Link.createUnsignedTransaction @'Shared w) Default payload
        verify rTx
            [ expectSuccess
            , expectResponseCode HTTP.status202
            ]
        let (ApiSerialisedTransaction apiTx _) =
                getFromResponse #transaction rTx
        signedTx <-
            signSharedTx ctx w apiTx [ expectResponseCode HTTP.status202 ]
        submittedTx <- submitSharedTxWithWid ctx w signedTx
        verify submittedTx
            [ expectResponseCode HTTP.status202
            ]

     fixtureTwoPartySharedWallet ctx = do

        let index = 30
        let passphrase = Passphrase $
                BA.convert $ T.encodeUtf8 fixturePassphrase

        -- first participant, cosigner 0
        m15txtA <- liftIO $ genMnemonics M15
        m12txtA <- liftIO $ genMnemonics M12
        let (Right m15A) = mkSomeMnemonic @'[ 15 ] m15txtA
        let (Right m12A) = mkSomeMnemonic @'[ 12 ] m12txtA
        let accXPubDerivedA =
                sharedAccPubKeyFromMnemonics m15A (Just m12A) index passphrase

        -- second participant, cosigner 1
        m15txtB <- liftIO $ genMnemonics M15
        m12txtB <- liftIO $ genMnemonics M12
        let (Right m15B) = mkSomeMnemonic @'[ 15 ] m15txtB
        let (Right m12B) = mkSomeMnemonic @'[ 12 ] m12txtB
        let accXPubDerivedB =
                sharedAccPubKeyFromMnemonics m15B (Just m12B) index passphrase

        -- payload
        let payload m15txt m12txt = Json [json| {
                "name": "Shared Wallet",
                "mnemonic_sentence": #{m15txt},
                "mnemonic_second_factor": #{m12txt},
                "passphrase": #{fixturePassphrase},
                "account_index": "30H",
                "payment_script_template":
                    { "cosigners":
                        { "cosigner#0": #{accXPubDerivedA}
                        , "cosigner#1": #{accXPubDerivedB}
                        },
                      "template":
                          { "all":
                             [ "cosigner#0", "cosigner#1" ]
                          }
                    }
                } |]

        rPostA <- postSharedWallet ctx Default (payload m15txtA m12txtA)
        verify (fmap (swapEither . view #wallet) <$> rPostA)
            [ expectResponseCode HTTP.status201
            ]
        let walShared1@(ApiSharedWallet (Right walA)) =
                getResponse rPostA

        rPostB <- postSharedWallet ctx Default (payload m15txtB m12txtB)
        verify (fmap (swapEither . view #wallet) <$> rPostB)
            [ expectResponseCode HTTP.status201
            ]
        let walShared2@(ApiSharedWallet (Right walB)) =
                getResponse rPostB

        fundSharedWallet @n
            ctx faucetUtxoAmt (NE.fromList [walShared1, walShared2])

        return (walA, walB)

     fixtureTwoPartySharedWalletPatched ctx = do

        let index = 30
        let passphrase = Passphrase $
                BA.convert $ T.encodeUtf8 fixturePassphrase

        -- first participant, cosigner 0
        m15txtA <- liftIO $ genMnemonics M15
        m12txtA <- liftIO $ genMnemonics M12
        let (Right m15A) = mkSomeMnemonic @'[ 15 ] m15txtA
        let (Right m12A) = mkSomeMnemonic @'[ 12 ] m12txtA
        let accXPubDerivedA =
                sharedAccPubKeyFromMnemonics m15A (Just m12A) index passphrase

        -- second participant, cosigner 1
        m15txtB <- liftIO $ genMnemonics M15
        m12txtB <- liftIO $ genMnemonics M12
        let (Right m15B) = mkSomeMnemonic @'[ 15 ] m15txtB
        let (Right m12B) = mkSomeMnemonic @'[ 12 ] m12txtB
        let accXPubDerivedB =
                sharedAccPubKeyFromMnemonics m15B (Just m12B) index passphrase

        -- payload for A
        let payloadA = Json [json| {
                "name": "Shared Wallet",
                "mnemonic_sentence": #{m15txtA},
                "mnemonic_second_factor": #{m12txtA},
                "passphrase": #{fixturePassphrase},
                "account_index": "30H",
                "payment_script_template":
                    { "cosigners":
                        { "cosigner#0": "self" },
                      "template":
                          { "all":
                             [ "cosigner#0", "cosigner#1" ]
                          }
                    }
                } |]

        rPostA <- postSharedWallet ctx Default payloadA
        verify (fmap (swapEither . view #wallet) <$> rPostA)
            [ expectResponseCode HTTP.status201
            ]
        let walPendingA = getResponse rPostA
        let payloadPatchA = Json [json| {
                "cosigner#1": #{accXPubDerivedB}
                } |]
        rPatchA <- patchSharedWallet ctx walPendingA Payment payloadPatchA
        expectResponseCode HTTP.status200 rPatchA
        let walShared1@(ApiSharedWallet (Right walA)) =
                getResponse rPatchA

        -- payload for B
        let payloadB = Json [json| {
                "name": "Shared Wallet",
                "mnemonic_sentence": #{m15txtB},
                "mnemonic_second_factor": #{m12txtB},
                "passphrase": #{fixturePassphrase},
                "account_index": "30H",
                "payment_script_template":
                    { "cosigners":
                        { "cosigner#1": "self" },
                      "template":
                          { "all":
                             [ "cosigner#0", "cosigner#1" ]
                          }
                    }
                } |]

        rPostB <- postSharedWallet ctx Default payloadB
        verify (fmap (swapEither . view #wallet) <$> rPostB)
            [ expectResponseCode HTTP.status201
            ]
        let walPendingB = getResponse rPostB
        let payloadPatchB = Json [json| {
                "cosigner#0": #{accXPubDerivedA}
                } |]
        rPatchB <- patchSharedWallet ctx walPendingB Payment payloadPatchB
        expectResponseCode HTTP.status200 rPatchB
        let walShared2@(ApiSharedWallet (Right walB)) =
                getResponse rPatchB

        fundSharedWallet @n ctx faucetUtxoAmt
            (NE.fromList [walShared1, walShared2])

        return
            (walA, walB, payloadA, accXPubDerivedA, payloadB, accXPubDerivedB)

     fixtureThreePartySharedWallet ctx = do

        let index = 30
        let passphrase = Passphrase $
                BA.convert $ T.encodeUtf8 fixturePassphrase

        -- first participant, cosigner 0
        m15txtA <- liftIO $ genMnemonics M15
        m12txtA <- liftIO $ genMnemonics M12
        let (Right m15A) = mkSomeMnemonic @'[ 15 ] m15txtA
        let (Right m12A) = mkSomeMnemonic @'[ 12 ] m12txtA
        let accXPubDerivedA =
                sharedAccPubKeyFromMnemonics m15A (Just m12A) index passphrase

        -- second participant, cosigner 1
        m15txtB <- liftIO $ genMnemonics M15
        m12txtB <- liftIO $ genMnemonics M12
        let (Right m15B) = mkSomeMnemonic @'[ 15 ] m15txtB
        let (Right m12B) = mkSomeMnemonic @'[ 12 ] m12txtB
        let accXPubDerivedB =
                sharedAccPubKeyFromMnemonics m15B (Just m12B) index passphrase

        -- third participant, cosigner 2
        m15txtC <- liftIO $ genMnemonics M15
        m12txtC <- liftIO $ genMnemonics M12
        let (Right m15C) = mkSomeMnemonic @'[ 15 ] m15txtC
        let (Right m12C) = mkSomeMnemonic @'[ 12 ] m12txtC
        let accXPubDerivedC =
                sharedAccPubKeyFromMnemonics m15C (Just m12C) index passphrase

        -- payload
        let payload m15txt m12txt = Json [json| {
                "name": "Shared Wallet",
                "mnemonic_sentence": #{m15txt},
                "mnemonic_second_factor": #{m12txt},
                "passphrase": #{fixturePassphrase},
                "account_index": "30H",
                "payment_script_template":
                    { "cosigners":
                        { "cosigner#0": #{accXPubDerivedA}
                        , "cosigner#1": #{accXPubDerivedB}
                        , "cosigner#2": #{accXPubDerivedC} },
                      "template":
                          { "some":
                               { "at_least": 2
                               , "from":
                                  [ "cosigner#0"
                                  , "cosigner#1"
                                  , "cosigner#2"
                                  ]
                               }
                          }
                    }
                } |]

        rPostA <- postSharedWallet ctx Default (payload m15txtA m12txtA)
        verify (fmap (swapEither . view #wallet) <$> rPostA)
            [ expectResponseCode HTTP.status201
            ]
        let walShared1@(ApiSharedWallet (Right walA)) =
                getResponse rPostA

        rPostB <- postSharedWallet ctx Default (payload m15txtB m12txtB)
        verify (fmap (swapEither . view #wallet) <$> rPostB)
            [ expectResponseCode HTTP.status201
            ]
        let walShared2@(ApiSharedWallet (Right walB)) =
                getResponse rPostB

        rPostC <- postSharedWallet ctx Default (payload m15txtC m12txtC)
        verify (fmap (swapEither . view #wallet) <$> rPostC)
            [ expectResponseCode HTTP.status201
            ]
        let walShared3@(ApiSharedWallet (Right walC)) =
                getResponse rPostC

        fundSharedWallet @n ctx faucetUtxoAmt
            (NE.fromList [walShared1, walShared2, walShared3])

        return (walA, walB, walC)

     mkTxPayload
         :: MonadUnliftIO m
         => Context
         -> ApiWallet
         -> Natural
         -> m Payload
     mkTxPayload ctx wDest amt = do
         addrs <- listAddresses @n ctx wDest
         let destination = (addrs !! 1) ^. #id
         return $ Json [json|{
                 "payments": [{
                     "address": #{destination},
                     "amount": {
                         "quantity": #{amt},
                         "unit": "lovelace"
                     }
                 }]
             }|]

     singleOutputTxTwoParty ctx sharedWal1 sharedWal2 = do
        -- check we see balance from two wallets
        rSharedWal1 <- getSharedWallet ctx (ApiSharedWallet (Right sharedWal1))
        rSharedWal2 <- getSharedWallet ctx (ApiSharedWallet (Right sharedWal2))

        let balanceExp =
                [ expectResponseCode HTTP.status200
                , expectField (traverse . #balance . #available)
                    (`shouldBe` ApiAmount faucetUtxoAmt)
                ]

        verify (fmap (view #wallet) <$> rSharedWal1) balanceExp
        verify (fmap (view #wallet) <$> rSharedWal2) balanceExp

        wb <- emptyWallet ctx
        let amt = (minUTxOValue (_mainEra ctx) :: Natural)

        payload <- liftIO $ mkTxPayload ctx wb amt

        rTx <- request @(ApiConstructTransaction n) ctx
            (Link.createUnsignedTransaction @'Shared sharedWal1) Default payload
        verify rTx
            [ expectSuccess
            , expectResponseCode HTTP.status202
            , expectField (#coinSelection . #inputs)
                (`shouldSatisfy` (not . null))
            , expectField (#coinSelection . #outputs)
                (`shouldSatisfy` (not . null))
            , expectField (#coinSelection . #change)
                (`shouldSatisfy` (not . null))
            , expectField (#fee . #toNatural)
                (`shouldSatisfy` (> 0))
            ]
        let txCbor = getFromResponse #transaction rTx
        let decodePayload1 = Json (toJSON txCbor)
        rDecodedTx1Wal1 <- request @(ApiDecodedTransaction n) ctx
            (Link.decodeTransaction @'Shared sharedWal1) Default decodePayload1
        rDecodedTx1Wal2 <- request @(ApiDecodedTransaction n) ctx
            (Link.decodeTransaction @'Shared sharedWal2) Default decodePayload1
        rDecodedTx1Target <- request @(ApiDecodedTransaction n) ctx
            (Link.decodeTransaction @'Shelley wb) Default decodePayload1

        let expectedFee = getFromResponse (#fee . #toNatural) rTx
        let sharedExpectationsBetweenWallets =
                [ expectResponseCode HTTP.status202
                , expectField (#fee . #toNatural) (`shouldBe` expectedFee)
                , expectField #withdrawals (`shouldBe` [])
                , expectField #collateral (`shouldBe` [])
                , expectField #metadata (`shouldBe` (ApiTxMetadata Nothing))
                , expectField #scriptValidity
                    (`shouldBe` (Just $ ApiT TxScriptValid))
                ]

        verify rDecodedTx1Target sharedExpectationsBetweenWallets

        let isInpOurs inp = case inp of
                ExternalInput _ -> False
                WalletInput _ -> True
        let areOurs = all isInpOurs
        addrs <- listAddresses @n ctx wb
        let addrIx = 1
        let addrDest = (addrs !! addrIx) ^. #id
        let expectedTxOutTarget = WalletOutput $ ApiWalletOutput
                { address = addrDest
                , amount = ApiAmount amt
                , assets = mempty
                , derivationPath = NE.fromList
                    [ ApiT (DerivationIndex 2_147_485_500)
                    , ApiT (DerivationIndex 2_147_485_463)
                    , ApiT (DerivationIndex 2_147_483_648)
                    , ApiT (DerivationIndex 0)
                    , ApiT (DerivationIndex $ fromIntegral addrIx)
                    ]
                }
        let isOutOurs out = case out of
                WalletOutput _ -> False
                ExternalOutput _ -> True

        let (ApiScriptTemplate scriptTemplate) =
                sharedWal1 ^. #paymentScriptTemplate
        let paymentScript =
                NativeExplicitScript
                (replaceCosignersWithVerKeys CA.UTxOExternal scriptTemplate
                (Index 1)) ViaSpending

        let noVerKeyWitness = mkApiWitnessCount WitnessCount
                { verificationKey = 0
                , scripts = [paymentScript]
                , bootstrap = 0
                }

        let decodeConstructedTxSharedWal =
                sharedExpectationsBetweenWallets ++
                [ expectField #inputs (`shouldSatisfy` areOurs)
                , expectField #outputs
                    (`shouldNotContain` [expectedTxOutTarget])
                -- Check that the change output is there:
                , expectField (#outputs)
                    ((`shouldBe` 1) . length . filter isOutOurs)
                ]
        let witsExp1 =
                [ expectField (#witnessCount) (`shouldBe` noVerKeyWitness) ]

        verify rDecodedTx1Wal1 (decodeConstructedTxSharedWal ++ witsExp1)
        verify rDecodedTx1Wal2 (decodeConstructedTxSharedWal ++ witsExp1)

        -- adding one witness
        let (ApiSerialisedTransaction apiTx1 _) =
                getFromResponse #transaction rTx
        signedTx1 <-
            signSharedTx ctx sharedWal1 apiTx1
                [ expectResponseCode HTTP.status202 ]
        let decodePayload2 = Json (toJSON signedTx1)
        rDecodedTx2Wal1 <- request @(ApiDecodedTransaction n) ctx
            (Link.decodeTransaction @'Shared sharedWal1) Default decodePayload2
        rDecodedTx2Wal2 <- request @(ApiDecodedTransaction n) ctx
            (Link.decodeTransaction @'Shared sharedWal2) Default decodePayload2
        rDecodedTx2Target <- request @(ApiDecodedTransaction n) ctx
            (Link.decodeTransaction @'Shelley wb) Default decodePayload2

        let oneVerKeyWitness = mkApiWitnessCount WitnessCount
                { verificationKey = 1
                , scripts = [paymentScript]
                , bootstrap = 0
                }
        let witsExp2 =
                [ expectField (#witnessCount) (`shouldBe` oneVerKeyWitness) ]

        verify rDecodedTx2Target sharedExpectationsBetweenWallets
        verify rDecodedTx2Wal1 (decodeConstructedTxSharedWal ++ witsExp2)
        verify rDecodedTx2Wal2 (decodeConstructedTxSharedWal ++ witsExp2)

        submittedTx1 <- submitSharedTxWithWid ctx sharedWal1 signedTx1
        verify submittedTx1
            [ expectResponseCode HTTP.status403
            , expectErrorMessage (errMsg403MissingWitsInTransaction 2 1)
            ]

        --adding the witness by the same participant does not change the
        --situation
        let (ApiSerialisedTransaction apiTx2 _) = signedTx1
        signedTx2 <-
            signSharedTx ctx sharedWal1 apiTx2
                [ expectResponseCode HTTP.status202 ]
        let decodePayload3 = Json (toJSON signedTx2)
        rDecodedTx3Wal1 <- request @(ApiDecodedTransaction n) ctx
            (Link.decodeTransaction @'Shared sharedWal1) Default decodePayload3
        rDecodedTx3Wal2 <- request @(ApiDecodedTransaction n) ctx
            (Link.decodeTransaction @'Shared sharedWal2) Default decodePayload3
        rDecodedTx3Target <- request @(ApiDecodedTransaction n) ctx
            (Link.decodeTransaction @'Shelley wb) Default decodePayload3
        verify rDecodedTx3Target sharedExpectationsBetweenWallets
        verify rDecodedTx3Wal1 (decodeConstructedTxSharedWal ++ witsExp2)
        verify rDecodedTx3Wal2 (decodeConstructedTxSharedWal ++ witsExp2)

        submittedTx2 <- submitSharedTxWithWid ctx sharedWal1 signedTx2
        verify submittedTx2
            [ expectResponseCode HTTP.status403
            , expectErrorMessage (errMsg403MissingWitsInTransaction 2 1)
            ]

        --adding the witness by the second participant make tx valid for
        --submission
        signedTx3 <-
            signSharedTx ctx sharedWal2 apiTx2
                [ expectResponseCode HTTP.status202 ]

        -- now submission works
        submittedTx3 <- submitSharedTxWithWid ctx sharedWal1 signedTx3
        verify submittedTx3
            [ expectResponseCode HTTP.status202
            ]

       -- checking decreased balance of shared wallets
        eventually "wShared balance is decreased" $ do
            wal1 <- getSharedWallet ctx (ApiSharedWallet (Right sharedWal1))
            wal2 <- getSharedWallet ctx (ApiSharedWallet (Right sharedWal2))
            let balanceExp1 =
                    [ expectResponseCode HTTP.status200
                    , expectField (traverse . #balance . #available)
                        (`shouldBe` ApiAmount
                            (faucetUtxoAmt - expectedFee - amt)
                        )
                    ]
            verify (fmap (view #wallet) <$> wal1) balanceExp1
            verify (fmap (view #wallet) <$> wal2) balanceExp1

       -- checking the balance of target wallet has increased
        eventually "Target wallet balance is increased by amt" $ do
            rWa <- request @ApiWallet ctx
                (Link.getWallet @'Shelley wb) Default Empty
            verify rWa
                [ expectSuccess
                , expectField
                        (#balance . #available . #toNatural)
                        (`shouldBe` amt)
                ]
