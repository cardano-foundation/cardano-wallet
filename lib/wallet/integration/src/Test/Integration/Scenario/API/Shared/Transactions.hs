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

module Test.Integration.Scenario.API.Shared.Transactions
    ( spec
    ) where

import Prelude

import Cardano.Address.Script
    ( KeyHash (..), Script (..) )
import Cardano.Mnemonic
    ( MkSomeMnemonic (..) )
import Cardano.Wallet.Api.Types
    ( ApiAddress
    , ApiConstructTransaction (..)
    , ApiDecodedTransaction (..)
    , ApiScriptTemplate (..)
    , ApiSerialisedTransaction (..)
    , ApiSharedWallet (..)
    , ApiT (..)
    , ApiTransaction
    , ApiTxId (..)
    , ApiTxInputGeneral (..)
    , ApiTxMetadata (..)
    , ApiTxOutputGeneral (..)
    , ApiWallet
    , ApiWalletOutput (..)
    , DecodeAddress
    , DecodeStakeAddress
    , EncodeAddress (..)
    , Iso8601Time (..)
    , WalletStyle (..)
    , fromApiEra
    , insertedAt
    )
import Cardano.Wallet.Api.Types.Error
    ( ApiErrorInfo (..) )
import Cardano.Wallet.Api.Types.Transaction
    ( mkApiWitnessCount )
import Cardano.Wallet.Pools
    ( StakePool )
import Cardano.Wallet.Primitive.AddressDerivation
    ( DerivationIndex (..), Index (..) )
import Cardano.Wallet.Primitive.AddressDerivation.SharedKey
    ( replaceCosignersWithVerKeys )
import Cardano.Wallet.Primitive.AddressDiscovery.Shared
    ( CredentialType (..) )
import Cardano.Wallet.Primitive.Passphrase
    ( Passphrase (..) )
import Cardano.Wallet.Primitive.Types
    ( SortOrder (..) )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Cardano.Wallet.Primitive.Types.Tx
    ( Direction (..)
    , TxMetadata (..)
    , TxMetadataValue (..)
    , TxScriptValidity (..)
    , TxStatus (..)
    , cardanoTxIdeallyNoLaterThan
    )
import Cardano.Wallet.Transaction
    ( AnyScript (..), WitnessCount (..) )
import Control.Monad
    ( forM_ )
import Control.Monad.IO.Unlift
    ( MonadUnliftIO (..), liftIO )
import Control.Monad.Trans.Resource
    ( runResourceT )
import Data.Aeson
    ( toJSON )
import Data.Either.Combinators
    ( swapEither )
import Data.Function
    ( (&) )
import Data.Generics.Internal.VL.Lens
    ( view, (^.) )
import Data.Generics.Wrapped
    ( _Unwrapped )
import Data.Maybe
    ( isJust )
import Data.Quantity
    ( Quantity (..) )
import Data.Text.Class
    ( FromText (..), ToText (..) )
import Data.Time.Clock
    ( UTCTime, addUTCTime )
import Data.Time.Utils
    ( utcTimePred, utcTimeSucc )
import Numeric.Natural
    ( Natural )
import Test.Hspec
    ( SpecWith, describe )
import Test.Hspec.Expectations.Lifted
    ( shouldBe, shouldNotContain, shouldSatisfy )
import Test.Hspec.Extra
    ( it )
import Test.Integration.Framework.DSL
    ( Context (..)
    , Headers (..)
    , MnemonicLength (..)
    , Payload (..)
    , arbitraryStake
    , decodeErrorInfo
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
    , fundSharedWallet
    , genMnemonics
    , getFromResponse
    , getSharedWallet
    , json
    , listAddresses
    , minUTxOValue
    , patchSharedWallet
    , postSharedWallet
    , request
    , sharedAccPubKeyFromMnemonics
    , signSharedTx
    , submitSharedTxWithWid
    , toQueryString
    , unsafeGetTransactionTime
    , unsafeRequest
    , utcIso8601ToText
    , verify
    , walletId
    )
import Test.Integration.Framework.Request
    ( RequestException )
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
import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Network.HTTP.Types as HTTP

data TestCase a = TestCase
    { query :: T.Text
    , assertions :: [(HTTP.Status, Either RequestException a) -> IO ()]
    }

spec :: forall n.
    ( DecodeAddress n
    , DecodeStakeAddress n
    , EncodeAddress n
    ) => SpecWith Context
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

        let (ApiSharedWallet (Left wal)) =
                getFromResponse Prelude.id rPost

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
                getFromResponse Prelude.id rPost

        let metadata =
                Json [json|{ "metadata": { "1": { "string": "hello" } } }|]

        rTx1 <- request @(ApiConstructTransaction n) ctx
            (Link.createUnsignedTransaction @'Shared wal) Default metadata
        verify rTx1
            [ expectResponseCode HTTP.status403
            , expectErrorMessage errMsg403EmptyUTxO
            ]

        let amt = 10 * minUTxOValue (_mainEra ctx)
        fundSharedWallet @n ctx amt (NE.fromList [walShared])

        rTx2 <- request @(ApiConstructTransaction n) ctx
            (Link.createUnsignedTransaction @'Shared wal) Default metadata
        verify rTx2
            [ expectResponseCode HTTP.status202
            , expectField (#coinSelection . #metadata) (`shouldSatisfy` isJust)
            , expectField (#fee . #getQuantity) (`shouldSatisfy` (>0))
            ]

        let txCbor1 = getFromResponse #transaction rTx2
        let decodePayload1 = Json (toJSON txCbor1)
        rDecodedTx1 <- request @(ApiDecodedTransaction n) ctx
            (Link.decodeTransaction @'Shared wal) Default decodePayload1
        let expectedFee = getFromResponse (#fee . #getQuantity) rTx2
        let metadata' =
                ApiT (TxMetadata (Map.fromList [(1,TxMetaText "hello")]))
        let decodedExpectations =
                [ expectResponseCode HTTP.status202
                , expectField (#fee . #getQuantity) (`shouldBe` expectedFee)
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
                    (traverse . #balance . #available . #getQuantity)
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
                getFromResponse Prelude.id rPost

        let metadata = Json [json|{ "metadata": { "1": "hello"  } }|]

        let amt = 10 * minUTxOValue (_mainEra ctx)
        fundSharedWallet @n ctx amt (NE.fromList [walShared])

        rTx <- request @(ApiConstructTransaction n) ctx
            (Link.createUnsignedTransaction @'Shared wal) Default metadata
        verify rTx
            [ expectResponseCode HTTP.status202
            , expectField (#coinSelection . #metadata) (`shouldSatisfy` isJust)
            , expectField (#fee . #getQuantity) (`shouldSatisfy` (>0))
            ]

        -- checking metadata before signing using decodeTransaction
        let txCbor1 = getFromResponse #transaction rTx
        let decodePayload1 = Json (toJSON txCbor1)
        rDecodedTx1 <- request @(ApiDecodedTransaction n) ctx
            (Link.decodeTransaction @'Shared wal) Default decodePayload1
        let expectedFee = getFromResponse (#fee . #getQuantity) rTx
        let metadata' =
                ApiT (TxMetadata (Map.fromList [(1,TxMetaText "hello")]))
        let decodedExpectations =
                [ expectResponseCode HTTP.status202
                , expectField (#fee . #getQuantity) (`shouldBe` expectedFee)
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
                    (traverse . #balance . #available . #getQuantity)
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
            , expectField (#fee . #getQuantity)
                (`shouldSatisfy` (> 0))
            ]
        let txCbor = getFromResponse #transaction rTx
        let decodePayload = Json (toJSON txCbor)
        rDecodedTxSource <- request @(ApiDecodedTransaction n) ctx
            (Link.decodeTransaction @'Shared wa) Default decodePayload
        rDecodedTxTarget <- request @(ApiDecodedTransaction n) ctx
            (Link.decodeTransaction @'Shelley wb) Default decodePayload

        let expectedFee = getFromResponse (#fee . #getQuantity) rTx
        let sharedExpectationsBetweenWallets =
                [ expectResponseCode HTTP.status202
                , expectField (#fee . #getQuantity)
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
                , amount = Quantity amt
                , assets = ApiT TokenMap.empty
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
                    (traverse . #balance . #available . #getQuantity)
                    (`shouldBe` (faucetUtxoAmt - expectedFee - amt))
                ]

        eventually "Target wallet balance is increased by amt" $ do
            rWa <- request @ApiWallet ctx
                (Link.getWallet @'Shelley wb) Default Empty
            verify rWa
                [ expectSuccess
                , expectField
                        (#balance . #available . #getQuantity)
                        (`shouldBe` amt)
                ]

    it "SHARED_TRANSACTIONS_CREATE_04b - \
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

    it "SHARED_TRANSACTIONS_CREATE_04c - \
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
            , expectField (#fee . #getQuantity)
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
                    (`shouldBe` Quantity faucetUtxoAmt)
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

        let expectedFee = getFromResponse (#fee . #getQuantity) rTx
        let (ApiScriptTemplate scriptTemplate) =
                sharedWal1 ^. #paymentScriptTemplate
        let paymentScript =
                NativeScript $
                replaceCosignersWithVerKeys
                    CA.UTxOExternal scriptTemplate (Index 1)
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
                , scripts = [changeRole CA.Unknown paymentScript]
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
                , scripts = [changeRole CA.Unknown paymentScript]
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
                        (`shouldBe` Quantity
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
                        (#balance . #available . #getQuantity)
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
        let walPending1 = getFromResponse Prelude.id rPost1
        let payloadPatch1 = Json [json| {
                "cosigner#1": #{accXPubDerived2}
                } |]
        rPatch1 <- patchSharedWallet ctx walPending1 Payment payloadPatch1
        expectResponseCode HTTP.status200 rPatch1
        let walShared1 = getFromResponse Prelude.id rPatch1

        rPost2 <- postSharedWallet ctx Default payload2
        verify (fmap (swapEither . view #wallet) <$> rPost2)
            [ expectResponseCode HTTP.status201
            ]
        let walPending2 = getFromResponse Prelude.id rPost2
        let payloadPatch2 = Json [json| {
                "cosigner#0": #{accXPubDerived1}
                } |]
        rPatch2 <- patchSharedWallet ctx walPending2 Payment payloadPatch2
        expectResponseCode HTTP.status200 rPatch2
        let walShared2 = getFromResponse Prelude.id rPatch2

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
                        (#balance . #total) (`shouldBe` Quantity amt)
                , expectField
                        (#balance . #available) (`shouldBe` Quantity amt)
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
    -- +---+----------+----------+------------+--------------+
    --     |  start   |   end    |   order    |    result    |
    -- +---+----------+----------+------------+--------------+
    --   1 | edge     | edge     | ascending  | 2 ascending  |
    --   2 | edge     | edge + 1 | descending | 2 descending |
    --   3 | edge     | edge - 1 | empty      | 1st one      |
    --   4 | edge     | empty    | empty      | 2 descending |
    --   5 | edge + 1 | edge + 1 | empty      | 2nd one      |
    --   6 | edge + 1 | edge - 1 | empty      | none         |
    --   7 | edge + 1 | empty    | ascending  | 2nd one      |
    --   8 | edge + 1 | edge     | descending | 2nd one      |
    --   9 | edge - 1 | edge - 1 | ascending  | 1st one      |
    --  10 | edge - 1 | empty    | descending | 2 descending |
    --  11 | edge - 1 | edge     | empty      | 2 descending |
    --  12 | edge - 1 | edge + 1 | empty      | 2 descending |
    --  13 | empty    | empty    | empty      | 2 descending |
    --  14 | empty    | edge     | empty      | 2 descending |
    --  15 | empty    | edge + 1 | ascending  | 2 ascending  |
    --  16 | empty    | edge - 1 | descending | 1st one      |
    --  17 | t1       | t1       | empty      | 1st one      |
    --  18 | t2       | t2       | descending | 2nd one      |
    -- +---+----------+----------+------------+--------------+

    it "SHARED_TRANSACTIONS_LIST_02,03x -\
        \Can limit/order results with start, end and order"
        $ \ctx -> runResourceT $ do

        let amt1 = minUTxOValue (_mainEra ctx)
        let amt2 = 2 * amt1
        (wSrc, wDest@(ApiSharedWallet (Right walDest))) <-
            (,) <$> fixtureSharedWallet @n ctx <*> emptySharedWallet ctx

        -- destination wallet
        rAddr <- request @[ApiAddress n] ctx
            (Link.listAddresses @'Shared walDest) Default Empty
        expectResponseCode HTTP.status200 rAddr
        let addrs = getFromResponse Prelude.id rAddr
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
                        (`shouldBe` Quantity amt1)
                    ]
            verify (fmap (view #wallet) <$> wal) balanceExp

        realizeTx ctx wSrc (payload destAddr2 amt2)
        eventually "wDest balance is increased again" $ do
            wal <- getSharedWallet ctx wDest
            let balanceExp =
                    [ expectResponseCode HTTP.status200
                    , expectField (traverse . #balance . #available)
                        (`shouldBe` Quantity (amt1 + amt2))
                    ]
            verify (fmap (view #wallet) <$> wal) balanceExp

        txs <- eventually "I make sure there are exactly 2 transactions" $ do
            let linkList = Link.listTransactions' @'Shared walDest
                    Nothing
                    Nothing
                    Nothing
                    Nothing
            rl <- request @([ApiTransaction n]) ctx linkList Default Empty
            verify rl [expectListSize 2]
            pure (getFromResponse Prelude.id rl)

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
                        , expectListField 0 #amount (`shouldBe` Quantity amt1)
                        , expectListField 1 #amount (`shouldBe` Quantity amt2)
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
                        , expectListField 0 #amount (`shouldBe` Quantity amt2)
                        , expectListField 1 #amount (`shouldBe` Quantity amt1)
                        ]
                    }
                , TestCase -- 3
                    { query = toQueryString
                        [ ("start", utcIso8601ToText t1)
                        , ("end", utcIso8601ToText $ minusDelta t2)
                        ]
                    , assertions =
                        [ expectListSize 1
                        , expectListField 0 #amount (`shouldBe` Quantity amt1)
                        ]
                    }
                , TestCase -- 4
                    { query = toQueryString
                        [ ("start", utcIso8601ToText t1) ]
                    , assertions =
                        [ expectListSize 2
                        , expectListField 0 #amount (`shouldBe` Quantity amt2)
                        , expectListField 1 #amount (`shouldBe` Quantity amt1)
                        ]
                    }
                , TestCase --5
                    { query = toQueryString
                        [ ("start", utcIso8601ToText $ plusDelta t1)
                        , ("end", utcIso8601ToText $ plusDelta t2)
                        ]
                    , assertions =
                        [ expectListSize 1
                        , expectListField 0 #amount (`shouldBe` Quantity amt2)
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
                        , expectListField 0 #amount (`shouldBe` Quantity amt2)
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
                        , expectListField 0 #amount (`shouldBe` Quantity amt2)
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
                        , expectListField 0 #amount (`shouldBe` Quantity amt1)
                        ]
                    }
                , TestCase -- 10
                    { query = toQueryString
                        [ ("order", "descending")
                        , ("start", utcIso8601ToText $ minusDelta t1)
                        ]
                    , assertions =
                        [ expectListSize 2
                        , expectListField 0 #amount (`shouldBe` Quantity amt2)
                        , expectListField 1 #amount (`shouldBe` Quantity amt1)
                        ]
                    }
                , TestCase -- 11
                    { query = toQueryString
                        [ ("start", utcIso8601ToText $ minusDelta t1)
                        , ("end", utcIso8601ToText t2)
                        ]
                    , assertions =
                        [ expectListSize 2
                        , expectListField 0 #amount (`shouldBe` Quantity amt2)
                        , expectListField 1 #amount (`shouldBe` Quantity amt1)
                        ]
                    }
                , TestCase -- 12
                    { query = toQueryString
                        [ ("start", utcIso8601ToText $ minusDelta t1)
                        , ("end", utcIso8601ToText $ plusDelta t2)
                        ]
                    , assertions =
                        [ expectListSize 2
                        , expectListField 0 #amount (`shouldBe` Quantity amt2)
                        , expectListField 1 #amount (`shouldBe` Quantity amt1)
                        ]
                    }
                , TestCase -- 13
                    { query = mempty
                    , assertions =
                        [ expectListSize 2
                        , expectListField 0 #amount (`shouldBe` Quantity amt2)
                        , expectListField 1 #amount (`shouldBe` Quantity amt1)
                        ]
                    }
                , TestCase -- 14
                    { query = toQueryString
                        [ ("end", utcIso8601ToText t2) ]
                    , assertions =
                        [ expectListSize 2
                        , expectListField 0 #amount (`shouldBe` Quantity amt2)
                        , expectListField 1 #amount (`shouldBe` Quantity amt1)
                        ]
                    }
                , TestCase -- 15
                    { query = toQueryString
                        [ ("end", utcIso8601ToText $ plusDelta t2) ]
                    , assertions =
                        [ expectListSize 2
                        , expectListField 0 #amount (`shouldBe` Quantity amt2)
                        , expectListField 1 #amount (`shouldBe` Quantity amt1)
                        ]
                    }
                , TestCase -- 16
                    { query = toQueryString
                        [ ("end", utcIso8601ToText $ minusDelta t2) ]
                    , assertions =
                        [ expectListSize 1
                        , expectListField 0 #amount (`shouldBe` Quantity amt1)
                        ]
                    }
                , TestCase -- 17
                    { query = toQueryString
                        [ ("start", utcIso8601ToText t1)
                        , ("end", utcIso8601ToText t1)
                        ]
                    , assertions =
                        [ expectListSize 1
                        , expectListField 0 #amount (`shouldBe` Quantity amt1)
                        ]
                    }
                , TestCase -- 18
                    { query = toQueryString
                        [ ("start", utcIso8601ToText t2)
                        , ("end", utcIso8601ToText t2)
                        ]
                    , assertions =
                        [ expectListSize 1
                        , expectListField 0 #amount (`shouldBe` Quantity amt2)
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
            r <- request @([ApiTransaction n]) ctx link Default Empty
            expectResponseCode HTTP.status200 r
            let txs = getFromResponse Prelude.id r
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
            txs2 <- listSharedTransactions ctx w (Just te) (Just t ) Nothing
            txs3 <- listSharedTransactions ctx w (Just t ) (Just tl) Nothing
            txs4 <- listSharedTransactions ctx w (Just te) (Just tl) Nothing
            length <$> [txs1, txs2, txs3, txs4] `shouldSatisfy` all (== 1)

    it "SHARED_TRANSACTIONS_LIST_RANGE_02 - \
        \Transaction at time t is NOT selected by range (t + 𝛿t, ...)" $
        \ctx -> runResourceT $ do

            w <- fixtureSharedWalletWith ctx (minUTxOValue (_mainEra ctx))
            t <- unsafeGetTransactionTime =<< listAllSharedTransactions ctx w
            let tl = utcTimeSucc t
            txs1 <- listSharedTransactions ctx w (Just tl) (Nothing) Nothing
            txs2 <- listSharedTransactions ctx w (Just tl) (Just tl) Nothing
            length <$> [txs1, txs2] `shouldSatisfy` all (== 0)

    it "SHARED_TRANSACTIONS_LIST_RANGE_03 - \
        \Transaction at time t is NOT selected by range (..., t - 𝛿t)" $
        \ctx -> runResourceT $ do

            w <- fixtureSharedWalletWith ctx (minUTxOValue (_mainEra ctx))
            t <- unsafeGetTransactionTime =<< listAllSharedTransactions ctx w
            let te = utcTimePred t
            txs1 <- listSharedTransactions ctx w (Nothing) (Just te) Nothing
            txs2 <- listSharedTransactions ctx w (Just te) (Just te) Nothing
            length <$> [txs1, txs2] `shouldSatisfy` all (== 0)

    it "SHARED_TRANSACTIONS_GET_01 - \
        \Can get Incoming and Outgoing transaction" $
        \ctx -> runResourceT $ do

        (wSrc, (ApiSharedWallet (Right walDest))) <-
            (,) <$> fixtureSharedWallet @n ctx <*> emptySharedWallet ctx
        -- post tx
        let amt = minUTxOValue (_mainEra ctx) :: Natural
        rAddr <- request @[ApiAddress n] ctx
            (Link.listAddresses @'Shared walDest) Default Empty
        expectResponseCode HTTP.status200 rAddr
        let addrs = getFromResponse Prelude.id rAddr
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
                        (#balance . #total) (`shouldBe` Quantity amt)
                , expectField
                        (#balance . #available) (`shouldBe` Quantity amt)
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
        rAddr <- request @[ApiAddress n] ctx
            (Link.listAddresses @'Shared walDest) Default Empty
        expectResponseCode HTTP.status200 rAddr
        let addrs = getFromResponse Prelude.id rAddr
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
        decodeErrorInfo rTx `shouldBe` StakingInvalid

    it "SHARED_TRANSACTIONS_DELEGATION_01a - \
       \Can join stakepool, rejoin another and quit" $ \ctx -> runResourceT $ do

        (party1,party2) <- fixtureSharedWalletDelegating @n ctx
        let depositAmt = Quantity 1_000_000

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
        rTx1 <- request @(ApiConstructTransaction n) ctx
            (Link.createUnsignedTransaction @'Shared party1) Default delegationJoin
        verify rTx1
            [ expectResponseCode HTTP.status202
            , expectField (#coinSelection . #depositsTaken) (`shouldBe` [depositAmt])
            , expectField (#coinSelection . #depositsReturned) (`shouldBe` [])
            ]

        let txCbor1 = getFromResponse #transaction rTx1
        let decodePayload1 = Json (toJSON txCbor1)
        rDecodedTx1 <- request @(ApiDecodedTransaction n) ctx
            (Link.decodeTransaction @'Shared party1) Default decodePayload1
        rDecodedTx2 <- request @(ApiDecodedTransaction n) ctx
            (Link.decodeTransaction @'Shared party2) Default decodePayload1
        let expectedFee = getFromResponse (#fee . #getQuantity) rTx1
        let decodedExpectations =
                [ expectResponseCode HTTP.status202
                , expectField (#fee . #getQuantity) (`shouldBe` expectedFee)
                , expectField #withdrawals (`shouldBe` [])
                , expectField #collateral (`shouldBe` [])
                , expectField #scriptValidity
                    (`shouldBe` (Just $ ApiT TxScriptValid))
                , expectField #depositsReturned (`shouldBe` [])
                , expectField #depositsTaken (`shouldBe` [depositAmt])
                ]
        verify rDecodedTx1 decodedExpectations
        verify rDecodedTx2 decodedExpectations

  where
     listSharedTransactions ctx w mStart mEnd mOrder = do
         let path = Link.listTransactions' @'Shared w
                    Nothing
                    (Iso8601Time <$> mStart)
                    (Iso8601Time <$> mEnd)
                    mOrder
         r <- request @[ApiTransaction n] ctx path Default Empty
         expectResponseCode HTTP.status200 r
         let txs = getFromResponse Prelude.id r
         return txs

     listAllSharedTransactions ctx w = do
         let path = Link.listTransactions' @'Shared w
                    Nothing Nothing Nothing (Just Descending)
         r <- request @[ApiTransaction n] ctx path Default Empty
         expectResponseCode HTTP.status200 r
         let txs = getFromResponse Prelude.id r
         return txs

     fixtureSharedWalletWith ctx amt = do
        (wSrc, wDest@(ApiSharedWallet (Right walDest))) <-
            (,) <$> fixtureSharedWallet @n ctx <*> emptySharedWallet ctx

        -- destination wallet
        rAddr <- request @[ApiAddress n] ctx
            (Link.listAddresses @'Shared walDest) Default Empty
        expectResponseCode HTTP.status200 rAddr
        let addrs = getFromResponse Prelude.id rAddr
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
                        (`shouldBe` Quantity amt)
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
                getFromResponse Prelude.id rPostA

        rPostB <- postSharedWallet ctx Default (payload m15txtB m12txtB)
        verify (fmap (swapEither . view #wallet) <$> rPostB)
            [ expectResponseCode HTTP.status201
            ]
        let walShared2@(ApiSharedWallet (Right walB)) =
                getFromResponse Prelude.id rPostB

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
        let walPendingA = getFromResponse Prelude.id rPostA
        let payloadPatchA = Json [json| {
                "cosigner#1": #{accXPubDerivedB}
                } |]
        rPatchA <- patchSharedWallet ctx walPendingA Payment payloadPatchA
        expectResponseCode HTTP.status200 rPatchA
        let walShared1@(ApiSharedWallet (Right walA)) =
                getFromResponse Prelude.id rPatchA

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
        let walPendingB = getFromResponse Prelude.id rPostB
        let payloadPatchB = Json [json| {
                "cosigner#0": #{accXPubDerivedA}
                } |]
        rPatchB <- patchSharedWallet ctx walPendingB Payment payloadPatchB
        expectResponseCode HTTP.status200 rPatchB
        let walShared2@(ApiSharedWallet (Right walB)) =
                getFromResponse Prelude.id rPatchB

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
                getFromResponse Prelude.id rPostA

        rPostB <- postSharedWallet ctx Default (payload m15txtB m12txtB)
        verify (fmap (swapEither . view #wallet) <$> rPostB)
            [ expectResponseCode HTTP.status201
            ]
        let walShared2@(ApiSharedWallet (Right walB)) =
                getFromResponse Prelude.id rPostB

        rPostC <- postSharedWallet ctx Default (payload m15txtC m12txtC)
        verify (fmap (swapEither . view #wallet) <$> rPostC)
            [ expectResponseCode HTTP.status201
            ]
        let walShared3@(ApiSharedWallet (Right walC)) =
                getFromResponse Prelude.id rPostC

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
                    (`shouldBe` Quantity faucetUtxoAmt)
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
            , expectField (#fee . #getQuantity)
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

        let expectedFee = getFromResponse (#fee . #getQuantity) rTx
        let sharedExpectationsBetweenWallets =
                [ expectResponseCode HTTP.status202
                , expectField (#fee . #getQuantity) (`shouldBe` expectedFee)
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
                , amount = Quantity amt
                , assets = ApiT TokenMap.empty
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
                NativeScript $
                replaceCosignersWithVerKeys CA.UTxOExternal scriptTemplate
                (Index 1)

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
                        (`shouldBe` Quantity
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
                        (#balance . #available . #getQuantity)
                        (`shouldBe` amt)
                ]

     changeRole :: CA.KeyRole -> AnyScript -> AnyScript
     changeRole role = \case
         NativeScript script ->
             let changeRole' = \case
                     RequireSignatureOf (KeyHash _ p) ->
                        RequireSignatureOf $ KeyHash role p
                     RequireAllOf xs ->
                        RequireAllOf (map changeRole' xs)
                     RequireAnyOf xs ->
                        RequireAnyOf (map changeRole' xs)
                     RequireSomeOf m xs ->
                        RequireSomeOf m (map changeRole' xs)
                     ActiveFromSlot s ->
                        ActiveFromSlot s
                     ActiveUntilSlot s ->
                        ActiveUntilSlot s
             in NativeScript $ changeRole' script
         PlutusScript _ -> error "wrong usage"
