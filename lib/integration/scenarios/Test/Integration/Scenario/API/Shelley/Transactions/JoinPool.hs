{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.Integration.Scenario.API.Shelley.Transactions.JoinPool (spec) where

import Prelude

import Cardano.Pool.Types
    ( decodePoolIdBech32
    )
import Cardano.Wallet.Address.Derivation
    ( DerivationIndex (..)
    )
import Cardano.Wallet.Api.Types
    ( ApiAnyCertificate (..)
    , ApiCertificate (..)
    , ApiConstructTransaction (..)
    , ApiDecodedTransaction
    , ApiSerialisedTransaction (..)
    , ApiT (..)
    , ApiTransaction
    , ApiTxId (..)
    , ApiTxInput (..)
    , ApiWallet
    , WalletStyle (..)
    )
import Cardano.Wallet.Api.Types.Amount
    ( ApiAmount (ApiAmount)
    )
import Cardano.Wallet.Api.Types.Era
    ( ApiEra (..)
    )
import Cardano.Wallet.Api.Types.Error
    ( ApiErrorInfo (..)
    , ApiErrorNoSuchPool (..)
    )
import Cardano.Wallet.Pools
    ( StakePool
    )
import Cardano.Wallet.Primitive.NetworkId
    ( HasSNetworkId
    )
import Cardano.Wallet.Primitive.Types.DRep
    ( DRep (..)
    )
import Cardano.Wallet.Primitive.Types.Tx.TxMeta
    ( Direction (..)
    , TxStatus (..)
    )
import Control.Monad.Trans.Resource
    ( runResourceT
    )
import Data.Aeson
    ( toJSON
    )
import Data.Function
    ( (&)
    )
import Data.Generics.Internal.VL.Lens
    ( view
    , (^.)
    )
import Data.Maybe
    ( fromMaybe
    , isJust
    )
import Data.Text
    ( Text
    )
import Test.Hspec
    ( SpecWith
    , describe
    )
import Test.Hspec.Expectations.Lifted
    ( shouldBe
    , shouldNotBe
    , shouldSatisfy
    )
import Test.Hspec.Extra
    ( it
    )
import Test.Integration.Framework.DSL
    ( Context (..)
    , Headers (..)
    , Payload (..)
    , arbitraryStake
    , counterexample
    , decodeErrorInfo
    , delegateToPool
    , delegating
    , emptyWallet
    , eventually
    , expectErrorInfo
    , expectErrorMessage
    , expectField
    , expectListField
    , expectListSize
    , expectResponseCode
    , expectSuccess
    , fixturePassphrase
    , fixtureWallet
    , fixtureWalletWith
    , getFromResponse
    , getResponse
    , json
    , listAddresses
    , minUTxOValue
    , noBabbage
    , noConway
    , notDelegating
    , notRetiringPools
    , request
    , rewardWallet
    , signTx
    , submitTxWithWid
    , unsafeRequest
    , verify
    , votingAndDelegating
    , waitForNextEpoch
    , waitNumberOfEpochBoundaries
    , (.<)
    , (.>)
    )

import qualified Cardano.Wallet.Api.Link as Link
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Network.HTTP.Types.Status as HTTP

spec :: forall n. HasSNetworkId n => SpecWith Context
spec = describe "NEW_SHELLEY_TRANSACTIONS" $ do
    it
        "TRANS_NEW_JOIN_01a - Can join stakepool, rejoin another and quit without voting in Babbage - old tx workflow"
        $ \ctx -> runResourceT $ do
            noConway ctx "withdraw possible"
            let initialAmt = 10 * minUTxOValue (_mainEra ctx)
            src <- fixtureWalletWith @n ctx [initialAmt]
            dest <- emptyWallet ctx

            let depositAmt = ApiAmount 1_000_000

            pool1 : pool2 : _ <- map (view #id) <$> notRetiringPools ctx

            let delegationJoin =
                    Json
                        [json|{
                "delegations": [{
                    "join": {
                        "pool": #{ApiT pool1},
                        "stake_key_index": "0H"
                    }
                }]
            }|]
            rTx1 <-
                request @(ApiConstructTransaction n)
                    ctx
                    (Link.createUnsignedTransaction @'Shelley src)
                    Default
                    delegationJoin
            verify
                rTx1
                [ expectResponseCode HTTP.status202
                , expectField
                    (#coinSelection . #depositsTaken)
                    (`shouldBe` [depositAmt])
                , expectField (#coinSelection . #depositsReturned) (`shouldBe` [])
                ]

            let ApiSerialisedTransaction apiTx1 _ = getFromResponse #transaction rTx1
            signedTx1 <-
                signTx ctx src apiTx1 [expectResponseCode HTTP.status202]

            -- as we are joining for the first time we expect two certificates
            let stakeKeyDerPath =
                    NE.fromList
                        [ ApiT (DerivationIndex 2_147_485_500)
                        , ApiT (DerivationIndex 2_147_485_463)
                        , ApiT (DerivationIndex 2_147_483_648)
                        , ApiT (DerivationIndex 2)
                        , ApiT (DerivationIndex 0)
                        ]
            let registerStakeKeyCert =
                    WalletDelegationCertificate $ RegisterRewardAccount stakeKeyDerPath
            let delegatingCert =
                    WalletDelegationCertificate $ JoinPool stakeKeyDerPath (ApiT pool1)

            let decodePayload1 = Json (toJSON signedTx1)
            rDecodedTx1 <-
                request @(ApiDecodedTransaction n)
                    ctx
                    (Link.decodeTransaction @'Shelley src)
                    Default
                    decodePayload1
            verify
                rDecodedTx1
                [ expectResponseCode HTTP.status202
                , expectField
                    #certificates
                    (`shouldBe` [registerStakeKeyCert, delegatingCert])
                , expectField #depositsTaken (`shouldBe` [depositAmt])
                , expectField #depositsReturned (`shouldBe` [])
                ]

            -- Submit tx
            submittedTx1 <- submitTxWithWid ctx src signedTx1
            verify
                submittedTx1
                [ expectSuccess
                , expectResponseCode HTTP.status202
                ]

            eventually "Wallet has joined pool and deposit info persists" $ do
                rJoin' <-
                    request @(ApiTransaction n)
                        ctx
                        ( Link.getTransaction @'Shelley
                            src
                            (getResponse submittedTx1)
                        )
                        Default
                        Empty
                verify
                    rJoin'
                    [ expectResponseCode HTTP.status200
                    , expectField (#status . #getApiT) (`shouldBe` InLedger)
                    , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
                    , expectField #depositTaken (`shouldBe` depositAmt)
                    , expectField #depositReturned (`shouldBe` ApiAmount 0)
                    ]

            let txId1 = getFromResponse #id submittedTx1
            let link = Link.getTransaction @'Shelley src (ApiTxId txId1)
            eventually "delegation transaction is in ledger" $ do
                request @(ApiTransaction n) ctx link Default Empty
                    >>= flip
                        verify
                        [ expectResponseCode HTTP.status200
                        , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
                        , expectField (#status . #getApiT) (`shouldBe` InLedger)
                        , expectField #metadata (`shouldBe` Nothing)
                        , expectField #inputs (`shouldSatisfy` all (isJust . source))
                        ]

            waitNumberOfEpochBoundaries 2 ctx

            let getSrcWallet =
                    let endpoint = Link.getWallet @'Shelley src
                    in  request @ApiWallet ctx endpoint Default Empty

            eventually "Wallet is delegating to pool1" $ do
                getSrcWallet
                    >>= flip
                        verify
                        [ expectField #delegation (`shouldBe` delegating (ApiT pool1) [])
                        ]

            waitNumberOfEpochBoundaries 2 ctx

            eventually "Wallet gets rewards from pool1" $ do
                getSrcWallet
                    >>= flip
                        verify
                        [expectField (#balance . #reward) (.> ApiAmount 0)]

            -- join another stake pool
            let delegationRejoin =
                    Json
                        [json|{
                "delegations": [{
                    "join": {
                        "pool": #{ApiT pool2},
                        "stake_key_index": "0H"
                    }
                }]
            }|]
            rTx2 <-
                request @(ApiConstructTransaction n)
                    ctx
                    (Link.createUnsignedTransaction @'Shelley src)
                    Default
                    delegationRejoin
            verify
                rTx2
                [ expectResponseCode HTTP.status202
                , expectField (#coinSelection . #depositsTaken) (`shouldBe` [])
                , expectField (#coinSelection . #depositsReturned) (`shouldBe` [])
                ]
            let ApiSerialisedTransaction apiTx2 _ = getFromResponse #transaction rTx2
            signedTx2 <-
                signTx ctx src apiTx2 [expectResponseCode HTTP.status202]
            let delegatingCert2 =
                    WalletDelegationCertificate $ JoinPool stakeKeyDerPath (ApiT pool2)

            let decodePayload2 = Json (toJSON signedTx2)
            rDecodedTx2 <-
                request @(ApiDecodedTransaction n)
                    ctx
                    (Link.decodeTransaction @'Shelley src)
                    Default
                    decodePayload2
            verify
                rDecodedTx2
                [ expectResponseCode HTTP.status202
                , expectField #certificates (`shouldBe` [delegatingCert2])
                , expectField #depositsTaken (`shouldBe` [])
                , expectField #depositsReturned (`shouldBe` [])
                ]
            submittedTx2 <- submitTxWithWid ctx src signedTx2
            verify
                submittedTx2
                [ expectSuccess
                , expectResponseCode HTTP.status202
                ]

            let txid2 = getFromResponse #id submittedTx2
            let queryTx2 = Link.getTransaction @'Shelley src (ApiTxId txid2)
            request @(ApiTransaction n) ctx queryTx2 Default Empty
                >>= flip
                    verify
                    [ expectResponseCode HTTP.status200
                    , expectField #depositTaken (`shouldBe` ApiAmount 0)
                    , expectField #depositReturned (`shouldBe` ApiAmount 0)
                    ]

            -- Wait for the certificate to be inserted
            eventually "Certificates are inserted" $ do
                let ep = Link.listTransactions @'Shelley src
                request @[ApiTransaction n] ctx ep Default Empty
                    >>= flip
                        verify
                        [ expectListField 1 (#direction . #getApiT) (`shouldBe` Outgoing)
                        , expectListField 1 (#status . #getApiT) (`shouldBe` InLedger)
                        ]

            waitNumberOfEpochBoundaries 2 ctx

            eventually "Wallet is delegating to pool2" $ do
                getSrcWallet
                    >>= flip
                        verify
                        [ expectField #delegation (`shouldBe` delegating (ApiT pool2) [])
                        ]

            -- there's currently no withdrawals in the wallet
            rw1 <-
                request @[ApiTransaction n]
                    ctx
                    ( Link.listTransactions' @'Shelley
                        src
                        (Just 1)
                        Nothing
                        Nothing
                        Nothing
                        Nothing
                        Nothing
                    )
                    Default
                    Empty
            verify rw1 [expectListSize 0]

            -- We can use rewards
            -- Tested by making an explicit withdrawal request to self.

            -- We wait for the start of a new epoch here
            -- so that there is a good chance that we spend all rewards
            -- in the next transaction, and don't receive any new rewards
            -- before that transaction has concluded.
            waitForNextEpoch ctx

            -- TODO: ADP-1192 (take care of withdrawals in new tx workflow)
            walletBeforeWithdrawal <- getResponse <$> getSrcWallet

            addrs <- listAddresses @n ctx dest
            let addr = (addrs !! 1) ^. #id
            let withdrawalAmount = minUTxOValue (_mainEra ctx)

            submittedWithdrawalTx <- do
                let endpoint = Link.createTransactionOld @'Shelley src
                request @(ApiTransaction n) ctx endpoint Default
                    $ Json
                        [json|
                    { "payments":
                        [ { "address": #{addr}
                        , "amount":
                            { "quantity": #{withdrawalAmount}
                            , "unit": "lovelace"
                            }
                        }
                        ]
                    , "passphrase": #{fixturePassphrase},
                    "withdrawal": "self"
                    }|]

            verify
                submittedWithdrawalTx
                [ expectField #amount (.> ApiAmount withdrawalAmount)
                , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
                ]

            eventually "Rewards have been consumed" $ do
                getSrcWallet
                    >>= flip
                        verify
                        [ expectField
                            (#balance . #reward . #toNatural)
                            (.< withdrawalAmount)
                        , -- should be 0, but in case new rewards acrue, let's just
                          -- require the reward balance to have decreased.
                          expectField
                            (#balance . #available)
                            (.> (walletBeforeWithdrawal ^. #balance . #available))
                        ]
                    & counterexample ("Wdrl: " <> show withdrawalAmount)

            -- now we can quit
            let delegationQuit =
                    Json
                        [json|{
                "delegations": [{
                    "quit": {
                        "stake_key_index": "0H"
                    }
                }]
            }|]
            rTx4 <-
                request @(ApiConstructTransaction n)
                    ctx
                    (Link.createUnsignedTransaction @'Shelley src)
                    Default
                    delegationQuit
            verify
                rTx4
                [ expectResponseCode HTTP.status202
                , expectField (#coinSelection . #depositsTaken) (`shouldBe` [])
                , expectField
                    (#coinSelection . #depositsReturned)
                    (`shouldBe` [depositAmt])
                ]
            let ApiSerialisedTransaction apiTx4 _ = getFromResponse #transaction rTx4
            signedTx4 <-
                signTx ctx src apiTx4 [expectResponseCode HTTP.status202]
            let quittingCert =
                    WalletDelegationCertificate $ QuitPool stakeKeyDerPath

            let decodePayload4 = Json (toJSON signedTx4)
            rDecodedTx4 <-
                request @(ApiDecodedTransaction n)
                    ctx
                    (Link.decodeTransaction @'Shelley src)
                    Default
                    decodePayload4
            verify
                rDecodedTx4
                [ expectResponseCode HTTP.status202
                , expectField #certificates (`shouldBe` [quittingCert])
                , expectField #depositsReturned (`shouldBe` [depositAmt])
                , expectField #depositsTaken (`shouldBe` [])
                ]
            submittedTx4 <- submitTxWithWid ctx src signedTx4
            verify
                submittedTx4
                [ expectSuccess
                , expectResponseCode HTTP.status202
                ]

            let txid3 = getFromResponse #id submittedTx4
            let queryTx3 = Link.getTransaction @'Shelley src (ApiTxId txid3)
            request @(ApiTransaction n) ctx queryTx3 Default Empty
                >>= flip
                    verify
                    [ expectResponseCode HTTP.status200
                    , expectField #depositTaken (`shouldBe` ApiAmount 0)
                    , expectField #depositReturned (`shouldBe` depositAmt)
                    ]

            waitNumberOfEpochBoundaries 2 ctx

            eventually "Wallet is not delegating" $ do
                getSrcWallet
                    >>= flip
                        verify
                        [expectField #delegation (`shouldBe` notDelegating [])]

            -- transaction history shows deposit returned
            request @(ApiTransaction n) ctx queryTx3 Default Empty
                >>= flip
                    verify
                    [ expectResponseCode HTTP.status200
                    , expectField #depositTaken (`shouldBe` ApiAmount 0)
                    , expectField #depositReturned (`shouldBe` depositAmt)
                    ]

    it
        "TRANS_NEW_JOIN_01a - Can join stakepool, rejoin another but not withdraw without voting in Conway - old tx workflow"
        $ \ctx -> runResourceT $ do
            noBabbage ctx "withdraw not possible if we have not voted"
            let initialAmt = 10 * minUTxOValue (_mainEra ctx)
            src <- fixtureWalletWith @n ctx [initialAmt]
            dest <- emptyWallet ctx

            let depositAmt = ApiAmount 1_000_000

            pool1 : pool2 : _ <- map (view #id) <$> notRetiringPools ctx

            let delegationJoin =
                    Json
                        [json|{
                "delegations": [{
                    "join": {
                        "pool": #{ApiT pool1},
                        "stake_key_index": "0H"
                    }
                }]
            }|]
            rTx1 <-
                request @(ApiConstructTransaction n)
                    ctx
                    (Link.createUnsignedTransaction @'Shelley src)
                    Default
                    delegationJoin
            verify
                rTx1
                [ expectResponseCode HTTP.status202
                , expectField
                    (#coinSelection . #depositsTaken)
                    (`shouldBe` [depositAmt])
                , expectField (#coinSelection . #depositsReturned) (`shouldBe` [])
                ]

            let ApiSerialisedTransaction apiTx1 _ = getFromResponse #transaction rTx1
            signedTx1 <-
                signTx ctx src apiTx1 [expectResponseCode HTTP.status202]

            -- as we are joining for the first time we expect two certificates
            let stakeKeyDerPath =
                    NE.fromList
                        [ ApiT (DerivationIndex 2_147_485_500)
                        , ApiT (DerivationIndex 2_147_485_463)
                        , ApiT (DerivationIndex 2_147_483_648)
                        , ApiT (DerivationIndex 2)
                        , ApiT (DerivationIndex 0)
                        ]
            let registerStakeKeyCert =
                    WalletDelegationCertificate $ RegisterRewardAccount stakeKeyDerPath
            let delegatingCert =
                    WalletDelegationCertificate $ JoinPool stakeKeyDerPath (ApiT pool1)

            let decodePayload1 = Json (toJSON signedTx1)
            rDecodedTx1 <-
                request @(ApiDecodedTransaction n)
                    ctx
                    (Link.decodeTransaction @'Shelley src)
                    Default
                    decodePayload1
            verify
                rDecodedTx1
                [ expectResponseCode HTTP.status202
                , expectField
                    #certificates
                    (`shouldBe` [registerStakeKeyCert, delegatingCert])
                , expectField #depositsTaken (`shouldBe` [depositAmt])
                , expectField #depositsReturned (`shouldBe` [])
                ]

            -- Submit tx
            submittedTx1 <- submitTxWithWid ctx src signedTx1
            verify
                submittedTx1
                [ expectSuccess
                , expectResponseCode HTTP.status202
                ]

            eventually "Wallet has joined pool and deposit info persists" $ do
                rJoin' <-
                    request @(ApiTransaction n)
                        ctx
                        ( Link.getTransaction @'Shelley
                            src
                            (getResponse submittedTx1)
                        )
                        Default
                        Empty
                verify
                    rJoin'
                    [ expectResponseCode HTTP.status200
                    , expectField (#status . #getApiT) (`shouldBe` InLedger)
                    , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
                    , expectField #depositTaken (`shouldBe` depositAmt)
                    , expectField #depositReturned (`shouldBe` ApiAmount 0)
                    ]

            let txId1 = getFromResponse #id submittedTx1
            let link = Link.getTransaction @'Shelley src (ApiTxId txId1)
            eventually "delegation transaction is in ledger" $ do
                request @(ApiTransaction n) ctx link Default Empty
                    >>= flip
                        verify
                        [ expectResponseCode HTTP.status200
                        , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
                        , expectField (#status . #getApiT) (`shouldBe` InLedger)
                        , expectField #metadata (`shouldBe` Nothing)
                        , expectField #inputs (`shouldSatisfy` all (isJust . source))
                        ]

            waitNumberOfEpochBoundaries 2 ctx

            let getSrcWallet =
                    let endpoint = Link.getWallet @'Shelley src
                    in  request @ApiWallet ctx endpoint Default Empty

            eventually "Wallet is delegating to pool1" $ do
                getSrcWallet
                    >>= flip
                        verify
                        [ expectField #delegation (`shouldBe` delegating (ApiT pool1) [])
                        ]

            waitNumberOfEpochBoundaries 2 ctx

            eventually "Wallet gets rewards from pool1" $ do
                getSrcWallet
                    >>= flip
                        verify
                        [expectField (#balance . #reward) (.> ApiAmount 0)]

            -- join another stake pool
            let delegationRejoin =
                    Json
                        [json|{
                "delegations": [{
                    "join": {
                        "pool": #{ApiT pool2},
                        "stake_key_index": "0H"
                    }
                }]
            }|]
            rTx2 <-
                request @(ApiConstructTransaction n)
                    ctx
                    (Link.createUnsignedTransaction @'Shelley src)
                    Default
                    delegationRejoin
            verify
                rTx2
                [ expectResponseCode HTTP.status202
                , expectField (#coinSelection . #depositsTaken) (`shouldBe` [])
                , expectField (#coinSelection . #depositsReturned) (`shouldBe` [])
                ]
            let ApiSerialisedTransaction apiTx2 _ = getFromResponse #transaction rTx2
            signedTx2 <-
                signTx ctx src apiTx2 [expectResponseCode HTTP.status202]
            let delegatingCert2 =
                    WalletDelegationCertificate $ JoinPool stakeKeyDerPath (ApiT pool2)

            let decodePayload2 = Json (toJSON signedTx2)
            rDecodedTx2 <-
                request @(ApiDecodedTransaction n)
                    ctx
                    (Link.decodeTransaction @'Shelley src)
                    Default
                    decodePayload2
            verify
                rDecodedTx2
                [ expectResponseCode HTTP.status202
                , expectField #certificates (`shouldBe` [delegatingCert2])
                , expectField #depositsTaken (`shouldBe` [])
                , expectField #depositsReturned (`shouldBe` [])
                ]
            submittedTx2 <- submitTxWithWid ctx src signedTx2
            verify
                submittedTx2
                [ expectSuccess
                , expectResponseCode HTTP.status202
                ]

            let txid2 = getFromResponse #id submittedTx2
            let queryTx2 = Link.getTransaction @'Shelley src (ApiTxId txid2)
            request @(ApiTransaction n) ctx queryTx2 Default Empty
                >>= flip
                    verify
                    [ expectResponseCode HTTP.status200
                    , expectField #depositTaken (`shouldBe` ApiAmount 0)
                    , expectField #depositReturned (`shouldBe` ApiAmount 0)
                    ]

            -- Wait for the certificate to be inserted
            eventually "Certificates are inserted" $ do
                let ep = Link.listTransactions @'Shelley src
                request @[ApiTransaction n] ctx ep Default Empty
                    >>= flip
                        verify
                        [ expectListField 1 (#direction . #getApiT) (`shouldBe` Outgoing)
                        , expectListField 1 (#status . #getApiT) (`shouldBe` InLedger)
                        ]

            waitNumberOfEpochBoundaries 2 ctx

            eventually "Wallet is delegating to pool2" $ do
                getSrcWallet
                    >>= flip
                        verify
                        [ expectField #delegation (`shouldBe` delegating (ApiT pool2) [])
                        ]

            -- there's currently no withdrawals in the wallet
            rw1 <-
                request @[ApiTransaction n]
                    ctx
                    ( Link.listTransactions' @'Shelley
                        src
                        (Just 1)
                        Nothing
                        Nothing
                        Nothing
                        Nothing
                        Nothing
                    )
                    Default
                    Empty
            verify rw1 [expectListSize 0]

            waitForNextEpoch ctx

            addrs <- listAddresses @n ctx dest
            let addr = (addrs !! 1) ^. #id
            let withdrawalAmount = minUTxOValue (_mainEra ctx)

            withdrawalFailure <- do
                let endpoint = Link.createTransactionOld @'Shelley src
                request @(ApiTransaction n) ctx endpoint Default
                    $ Json
                        [json|
                    { "payments":
                        [ { "address": #{addr}
                        , "amount":
                            { "quantity": #{withdrawalAmount}
                            , "unit": "lovelace"
                            }
                        }
                        ]
                    , "passphrase": #{fixturePassphrase},
                    "withdrawal": "self"
                    }|]

            decodeErrorInfo withdrawalFailure
                `shouldBe` WithdrawalNotPossibleWithoutVote

    it
        "TRANS_NEW_JOIN_01b - Cannot withdraw without voting in Conway - new tx workflow"
        $ \ctx -> runResourceT $ do
            noBabbage ctx "voting only Conway onwards"
            let initialAmt = 10 * minUTxOValue (_mainEra ctx)
            src <- fixtureWalletWith @n ctx [initialAmt]
            dest <- emptyWallet ctx

            let depositAmt = ApiAmount 1_000_000

            pool1 : _ <- map (view #id) <$> notRetiringPools ctx

            let delegationJoin =
                    Json
                        [json|{
                "delegations": [{
                    "join": {
                        "pool": #{ApiT pool1},
                        "stake_key_index": "0H"
                    }
                }]
            }|]
            rTx <-
                request @(ApiConstructTransaction n)
                    ctx
                    (Link.createUnsignedTransaction @'Shelley src)
                    Default
                    delegationJoin
            verify
                rTx
                [ expectResponseCode HTTP.status202
                , expectField
                    (#coinSelection . #depositsTaken)
                    (`shouldBe` [depositAmt])
                , expectField (#coinSelection . #depositsReturned) (`shouldBe` [])
                ]

            let ApiSerialisedTransaction apiTx _ = getFromResponse #transaction rTx
            signedTx <- signTx ctx src apiTx [expectResponseCode HTTP.status202]

            submittedTx <- submitTxWithWid ctx src signedTx
            verify
                submittedTx
                [ expectSuccess
                , expectResponseCode HTTP.status202
                ]

            eventually "Wallet has joined pool1 and deposit info persists" $ do
                rJoin' <-
                    request @(ApiTransaction n)
                        ctx
                        ( Link.getTransaction @'Shelley
                            src
                            (getResponse submittedTx)
                        )
                        Default
                        Empty
                verify
                    rJoin'
                    [ expectResponseCode HTTP.status200
                    , expectField (#status . #getApiT) (`shouldBe` InLedger)
                    , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
                    , expectField #depositTaken (`shouldBe` depositAmt)
                    , expectField #depositReturned (`shouldBe` ApiAmount 0)
                    ]

            waitNumberOfEpochBoundaries 2 ctx

            let getSrcWallet =
                    let endpoint = Link.getWallet @'Shelley src
                    in  request @ApiWallet ctx endpoint Default Empty

            eventually "Wallet is delegating to pool" $ do
                getSrcWallet
                    >>= flip
                        verify
                        [ expectField #delegation (`shouldBe` delegating (ApiT pool1) [])
                        ]

            waitNumberOfEpochBoundaries 2 ctx

            eventually "Wallet gets rewards from pool1" $ do
                getSrcWallet
                    >>= flip
                        verify
                        [expectField (#balance . #reward) (.> ApiAmount 0)]

            addrs <- listAddresses @n ctx dest
            let addr = (addrs !! 1) ^. #id
            let withdrawalAmount = minUTxOValue (_mainEra ctx)

            let withdrawalPayload =
                    Json
                        [json|
                { "payments":
                    [ { "address": #{addr}
                    , "amount":
                        { "quantity": #{withdrawalAmount}
                        , "unit": "lovelace"
                        }
                    }
                    ]
                , "passphrase": #{fixturePassphrase},
                "withdrawal": "self"
                }|]

            withdrawalFailure <-
                request @(ApiConstructTransaction n)
                    ctx
                    (Link.createUnsignedTransaction @'Shelley src)
                    Default
                    withdrawalPayload

            decodeErrorInfo withdrawalFailure
                `shouldBe` WithdrawalNotPossibleWithoutVote

    it
        "TRANS_NEW_JOIN_01b - Can withdraw without voting in Babbage - new tx workflow"
        $ \ctx -> runResourceT $ do
            noConway ctx "withdraw possible"
            let initialAmt = 10 * minUTxOValue (_mainEra ctx)
            src <- fixtureWalletWith @n ctx [initialAmt]
            dest <- emptyWallet ctx

            let depositAmt = ApiAmount 1_000_000

            pool1 : _ <- map (view #id) <$> notRetiringPools ctx

            let delegationJoin =
                    Json
                        [json|{
                "delegations": [{
                    "join": {
                        "pool": #{ApiT pool1},
                        "stake_key_index": "0H"
                    }
                }]
            }|]
            rTx <-
                request @(ApiConstructTransaction n)
                    ctx
                    (Link.createUnsignedTransaction @'Shelley src)
                    Default
                    delegationJoin
            verify
                rTx
                [ expectResponseCode HTTP.status202
                , expectField
                    (#coinSelection . #depositsTaken)
                    (`shouldBe` [depositAmt])
                , expectField (#coinSelection . #depositsReturned) (`shouldBe` [])
                ]

            let ApiSerialisedTransaction apiTx _ = getFromResponse #transaction rTx
            signedTx <- signTx ctx src apiTx [expectResponseCode HTTP.status202]

            submittedTx <- submitTxWithWid ctx src signedTx
            verify
                submittedTx
                [ expectSuccess
                , expectResponseCode HTTP.status202
                ]

            eventually "Wallet has joined pool1 and deposit info persists" $ do
                rJoin' <-
                    request @(ApiTransaction n)
                        ctx
                        ( Link.getTransaction @'Shelley
                            src
                            (getResponse submittedTx)
                        )
                        Default
                        Empty
                verify
                    rJoin'
                    [ expectResponseCode HTTP.status200
                    , expectField (#status . #getApiT) (`shouldBe` InLedger)
                    , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
                    , expectField #depositTaken (`shouldBe` depositAmt)
                    , expectField #depositReturned (`shouldBe` ApiAmount 0)
                    ]

            waitNumberOfEpochBoundaries 2 ctx

            let getSrcWallet =
                    let endpoint = Link.getWallet @'Shelley src
                    in  request @ApiWallet ctx endpoint Default Empty

            eventually "Wallet is delegating to pool" $ do
                getSrcWallet
                    >>= flip
                        verify
                        [ expectField #delegation (`shouldBe` delegating (ApiT pool1) [])
                        ]

            waitNumberOfEpochBoundaries 2 ctx

            eventually "Wallet gets rewards from pool1" $ do
                getSrcWallet
                    >>= flip
                        verify
                        [expectField (#balance . #reward) (.> ApiAmount 0)]

            addrs <- listAddresses @n ctx dest
            let addr = (addrs !! 1) ^. #id
            let withdrawalAmount = minUTxOValue (_mainEra ctx)

            let withdrawalPayload =
                    Json
                        [json|
                { "payments":
                    [ { "address": #{addr}
                    , "amount":
                        { "quantity": #{withdrawalAmount}
                        , "unit": "lovelace"
                        }
                    }
                    ]
                , "passphrase": #{fixturePassphrase},
                "withdrawal": "self"
                }|]

            withdrawalTx <-
                request @(ApiConstructTransaction n)
                    ctx
                    (Link.createUnsignedTransaction @'Shelley src)
                    Default
                    withdrawalPayload

            verify
                withdrawalTx
                [ expectResponseCode HTTP.status202
                ]

            let ApiSerialisedTransaction apiTx1 _ = getFromResponse #transaction withdrawalTx
            signedWithdrawalTx <-
                signTx ctx src apiTx1 [expectResponseCode HTTP.status202]

            submittedWithdrawalTx <- submitTxWithWid ctx src signedWithdrawalTx
            verify
                submittedWithdrawalTx
                [ expectSuccess
                , expectResponseCode HTTP.status202
                ]

            eventually "Reward balance is 0" $ do
                rWa <-
                    request @ApiWallet
                        ctx
                        (Link.getWallet @'Shelley src)
                        Default
                        Empty
                verify
                    rWa
                    [ expectSuccess
                    , expectField
                        (#balance . #reward . #toNatural)
                        (`shouldBe` 0)
                    ]

    it "TRANS_NEW_JOIN_01b - Invalid pool id" $ \ctx -> runResourceT $ do
        wa <- fixtureWallet ctx
        let invalidPoolId = T.replicate 32 "1"
        let delegation =
                Json
                    [json|{
                "delegations": [{
                    "join": {
                        "pool": #{invalidPoolId},
                        "stake_key_index": "0H"
                    }
                }]
            }|]
        rTx <-
            request @(ApiConstructTransaction n)
                ctx
                (Link.createUnsignedTransaction @'Shelley wa)
                Default
                delegation
        verify
            rTx
            [ expectResponseCode HTTP.status400
            , expectErrorMessage "Invalid stake pool id"
            ]

    it "TRANS_NEW_JOIN_01b - Absent pool id" $ \ctx -> runResourceT $ do
        wa <- fixtureWallet ctx
        let absentPoolIdBech32 = "pool1mgjlw24rg8sp4vrzctqxtf2nn29rjhtkq2kdzvf4tcjd5pl547k"
        (Right absentPoolId) <- pure $ decodePoolIdBech32 absentPoolIdBech32
        let delegation =
                Json
                    [json|{
                "delegations": [{
                    "join": {
                        "pool": #{absentPoolIdBech32},
                        "stake_key_index": "0H"
                    }
                }]
            }|]
        rTx <-
            request @(ApiConstructTransaction n)
                ctx
                (Link.createUnsignedTransaction @'Shelley wa)
                Default
                delegation
        verify
            rTx
            [ expectResponseCode HTTP.status404
            , expectErrorInfo
                $ flip shouldBe
                $ NoSuchPool
                $ ApiErrorNoSuchPool{poolId = absentPoolId}
            ]

    it "TRANS_NEW_JOIN_01c - Multidelegation not supported" $ \ctx -> runResourceT $ do
        wa <- fixtureWallet ctx
        let pool' = "pool1mgjlw24rg8sp4vrzctqxtf2nn29rjhtkq2kdzvf4tcjd5pl547k" :: Text
        let delegations =
                Json
                    [json|{
                "delegations": [{
                    "join": {
                        "pool": #{pool'},
                        "stake_key_index": "0H"
                    }
                },{
                    "quit": {
                        "stake_key_index": "0H"
                    }
                }]
            }|]
        rTx <-
            request @(ApiConstructTransaction n)
                ctx
                (Link.createUnsignedTransaction @'Shelley wa)
                Default
                delegations
        verify
            rTx
            [ expectResponseCode HTTP.status403
            ]
        decodeErrorInfo rTx `shouldBe` CreatedMultidelegationTransaction

    it "TRANS_NEW_JOIN_01d - Multiaccount not supported" $ \ctx -> runResourceT $ do
        wa <- fixtureWallet ctx
        let pool' = "pool1mgjlw24rg8sp4vrzctqxtf2nn29rjhtkq2kdzvf4tcjd5pl547k" :: Text
        let delegations =
                Json
                    [json|{
                "delegations": [{
                    "join": {
                        "pool": #{pool'},
                        "stake_key_index": "1H"
                    }
                }]
            }|]
        rTx <-
            request @(ApiConstructTransaction n)
                ctx
                (Link.createUnsignedTransaction @'Shelley wa)
                Default
                delegations
        verify
            rTx
            [ expectResponseCode HTTP.status403
            ]
        decodeErrorInfo rTx `shouldBe` CreatedMultiaccountTransaction

    it "TRANS_NEW_JOIN_01e - Can re-join and withdraw at once" $ \ctx -> runResourceT $ do
        (src, _) <- rewardWallet ctx
        let ApiT currentDelegation =
                fromMaybe
                    (error "wallet should be delegating")
                    (src ^. #delegation . #active . #target)
        pools <-
            map (view #id . getApiT) . snd
                <$> unsafeRequest @[ApiT StakePool]
                    ctx
                    (Link.listStakePools arbitraryStake)
                    Empty
        let newPool : _ = filter (/= currentDelegation) pools

        let delegationJoin =
                Json
                    [json|{
                "delegations": [{
                    "join": {
                        "pool": #{ApiT newPool},
                        "stake_key_index": "0H"
                    }
                }]
                , "withdrawal": "self"
            }|]
        rTx1 <-
            request @(ApiConstructTransaction n)
                ctx
                (Link.createUnsignedTransaction @'Shelley src)
                Default
                delegationJoin
        let ApiSerialisedTransaction apiTx1 _ = getFromResponse #transaction rTx1
        signedTx1 <-
            signTx ctx src apiTx1 [expectResponseCode HTTP.status202]
        submittedTx1 <- submitTxWithWid ctx src signedTx1
        verify
            submittedTx1
            [ expectSuccess
            , expectResponseCode HTTP.status202
            ]
        eventually "Wallet has joined pool and deposit info persists" $ do
            rJoin' <-
                request @(ApiTransaction n)
                    ctx
                    ( Link.getTransaction @'Shelley
                        src
                        (getResponse submittedTx1)
                    )
                    Default
                    Empty
            verify
                rJoin'
                [ expectResponseCode HTTP.status200
                , expectField (#status . #getApiT) (`shouldBe` InLedger)
                , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
                , expectField #withdrawals (`shouldNotBe` [])
                ]

        let txId1 = getFromResponse #id submittedTx1
        let link = Link.getTransaction @'Shelley src (ApiTxId txId1)

        eventually "delegation transaction is in ledger" $ do
            rSrc <- request @(ApiTransaction n) ctx link Default Empty
            verify
                rSrc
                [ expectResponseCode HTTP.status200
                , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
                , expectField (#status . #getApiT) (`shouldBe` InLedger)
                , expectField #metadata (`shouldBe` Nothing)
                , expectField #inputs $ \inputs' -> do
                    inputs' `shouldSatisfy` all (isJust . source)
                ]

        waitNumberOfEpochBoundaries 4 ctx

        eventually "Wallet gets rewards from newPool" $ do
            request @ApiWallet ctx (Link.getWallet @'Shelley src) Default Empty
                >>= flip
                    verify
                    [expectField (#balance . #reward) (.> ApiAmount 0)]

        let expectedDelegation =
                if _mainEra ctx >= ApiConway
                    then votingAndDelegating (ApiT newPool) (ApiT Abstain) []
                    else delegating (ApiT newPool) []

        eventually "Wallet is delegating to newPool" $ do
            request @ApiWallet ctx (Link.getWallet @'Shelley src) Default Empty
                >>= flip
                    verify
                    [ expectField #delegation (`shouldBe` expectedDelegation)
                    ]

    it "TRANS_NEW_JOIN_01f - Cannot re-join the same pool in Babbage" $ \ctx ->
        runResourceT $ do
            noConway ctx "re-joining the same pool outlawed before Conway"

            (src, pool1) <- delegateToPool @n ctx

            let delegationJoin =
                    Json
                        [json|{
                "delegations": [{
                    "join": {
                        "pool": #{ApiT pool1},
                        "stake_key_index": "0H"
                    }
                }]
            }|]
            rTx2 <-
                request @(ApiConstructTransaction n)
                    ctx
                    (Link.createUnsignedTransaction @'Shelley src)
                    Default
                    delegationJoin
            verify
                rTx2
                [ expectResponseCode HTTP.status403
                ]
            decodeErrorInfo rTx2 `shouldBe` PoolAlreadyJoined

    it "TRANS_NEW_JOIN_01f - Can re-join the same pool in Conway" $ \ctx ->
        runResourceT $ do
            noBabbage ctx "re-joining the same pool is permitted Conway onwards"

            (src, pool1) <- delegateToPool @n ctx

            let delegationJoin =
                    Json
                        [json|{
                "delegations": [{
                    "join": {
                        "pool": #{ApiT pool1},
                        "stake_key_index": "0H"
                    }
                }],
                "vote": "abstain"
            }|]
            rTx2 <-
                request @(ApiConstructTransaction n)
                    ctx
                    (Link.createUnsignedTransaction @'Shelley src)
                    Default
                    delegationJoin
            verify
                rTx2
                [ expectResponseCode HTTP.status202
                ]
            let ApiSerialisedTransaction apiTx2 _ = getFromResponse #transaction rTx2
            signedTx2 <-
                signTx ctx src apiTx2 [expectResponseCode HTTP.status202]

            submittedTx2 <- submitTxWithWid ctx src signedTx2
            verify
                submittedTx2
                [ expectSuccess
                , expectResponseCode HTTP.status202
                ]

            eventually "Wallet has joined pool and deposit info persists" $ do
                rJoin' <-
                    request @(ApiTransaction n)
                        ctx
                        ( Link.getTransaction @'Shelley
                            src
                            (getResponse submittedTx2)
                        )
                        Default
                        Empty
                verify
                    rJoin'
                    [ expectResponseCode HTTP.status200
                    ]

            waitNumberOfEpochBoundaries 2 ctx

            let getSrcWallet =
                    let endpoint = Link.getWallet @'Shelley src
                    in  request @ApiWallet ctx endpoint Default Empty
            eventually "Wallet is delegating to pool1 and voting abstain" $ do
                getSrcWallet
                    >>= flip
                        verify
                        [ expectField
                            #delegation
                            (`shouldBe` votingAndDelegating (ApiT pool1) (ApiT Abstain) [])
                        ]

            rTx3 <-
                request @(ApiConstructTransaction n)
                    ctx
                    (Link.createUnsignedTransaction @'Shelley src)
                    Default
                    delegationJoin
            verify
                rTx3
                [ expectResponseCode HTTP.status403
                ]
            decodeErrorInfo rTx3 `shouldBe` PoolAlreadyJoinedSameVote

            let voteAgain =
                    Json
                        [json|{
                "vote": "abstain"
            }|]
            rTx4 <-
                request @(ApiConstructTransaction n)
                    ctx
                    (Link.createUnsignedTransaction @'Shelley src)
                    Default
                    voteAgain
            verify
                rTx4
                [ expectResponseCode HTTP.status403
                ]
            decodeErrorInfo rTx4 `shouldBe` SameVote

    it
        "TRANS_NEW_JOIN_02 - Can join stakepool in case I have many UTxOs on 1 address"
        $ \ctx -> runResourceT $ do
            let amt = minUTxOValue (_mainEra ctx)
            src <- emptyWallet ctx
            wa <- fixtureWallet ctx

            -- Send ada to single wallet address to have [1A, 1A, 1A]
            addrs <- listAddresses @n ctx src
            let destination = (addrs !! 1) ^. #id
            let payload =
                    Json
                        [json|{
                "payments": [{
                    "address": #{destination},
                    "amount": {
                        "quantity": #{amt},
                        "unit": "lovelace"
                    }
                },
                {
                    "address": #{destination},
                    "amount": {
                        "quantity": #{amt},
                        "unit": "lovelace"
                    }
                },
                {
                    "address": #{destination},
                    "amount": {
                        "quantity": #{amt},
                        "unit": "lovelace"
                    }
                }]
            }|]

            -- send transaction
            rTx <-
                request @(ApiConstructTransaction n)
                    ctx
                    (Link.createUnsignedTransaction @'Shelley wa)
                    Default
                    payload
            verify rTx [expectSuccess]

            let ApiSerialisedTransaction apiTx _ = getFromResponse #transaction rTx
            signedTx <- signTx ctx wa apiTx [expectResponseCode HTTP.status202]

            submittedTx <- submitTxWithWid ctx wa signedTx
            verify submittedTx [expectSuccess]

            eventually "Target wallet balance is increased by amt and assets" $ do
                rWb <-
                    request @ApiWallet
                        ctx
                        (Link.getWallet @'Shelley src)
                        Default
                        Empty
                verify
                    rWb
                    [ expectSuccess
                    , expectField
                        (#balance . #available . #toNatural)
                        (`shouldBe` (3 * amt))
                    ]

            -- Delegate from src wallet
            pool1 : _ <-
                map (view #id . getApiT) . snd
                    <$> unsafeRequest
                        @[ApiT StakePool]
                        ctx
                        (Link.listStakePools arbitraryStake)
                        Empty

            let delegationJoin =
                    Json
                        [json|{
                "delegations": [{
                    "join": {
                        "pool": #{ApiT pool1},
                        "stake_key_index": "0H"
                    }
                }]
            }|]
            rTx1 <-
                request @(ApiConstructTransaction n)
                    ctx
                    (Link.createUnsignedTransaction @'Shelley src)
                    Default
                    delegationJoin
            verify
                rTx1
                [ expectResponseCode HTTP.status202
                , expectField
                    (#coinSelection . #depositsTaken)
                    (`shouldBe` [ApiAmount 1_000_000])
                , expectField (#coinSelection . #depositsReturned) (`shouldBe` [])
                ]

            let ApiSerialisedTransaction apiTx1 _ = getFromResponse #transaction rTx1
            signedTx1 <-
                signTx ctx src apiTx1 [expectResponseCode HTTP.status202]

            let decodePayload1 = Json (toJSON signedTx1)
            rDecodedTx1 <-
                request @(ApiDecodedTransaction n)
                    ctx
                    (Link.decodeTransaction @'Shelley src)
                    Default
                    decodePayload1
            verify
                rDecodedTx1
                [ expectResponseCode HTTP.status202
                , expectField #depositsTaken (`shouldBe` [ApiAmount 1_000_000])
                , expectField #depositsReturned (`shouldBe` [])
                ]

            -- Submit tx
            submittedTx1 <- submitTxWithWid ctx src signedTx1
            verify
                submittedTx1
                [ expectSuccess
                , expectResponseCode HTTP.status202
                ]

            eventually "Wallet has joined pool and deposit info persists" $ do
                rJoin' <-
                    request @(ApiTransaction n)
                        ctx
                        ( Link.getTransaction @'Shelley
                            src
                            (getResponse submittedTx1)
                        )
                        Default
                        Empty
                verify
                    rJoin'
                    [ expectResponseCode HTTP.status200
                    , expectField (#status . #getApiT) (`shouldBe` InLedger)
                    , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
                    -- , expectField #depositTaken (`shouldBe` ApiAmount 1000000)
                    -- , expectField #depositReturned (`shouldBe` ApiAmount 0)
                    ]

    it "TRANS_NEW_QUIT_01 - Cannot quit if not joined" $ \ctx -> runResourceT $ do
        wa <- fixtureWallet ctx
        let delegation =
                Json
                    [json|{
                "delegations": [{
                    "quit": {
                        "stake_key_index": "0H"
                    }
                }]
            }|]
        rTx <-
            request @(ApiConstructTransaction n)
                ctx
                (Link.createUnsignedTransaction @'Shelley wa)
                Default
                delegation
        verify
            rTx
            [ expectResponseCode HTTP.status403
            ]
        decodeErrorInfo rTx `shouldBe` NotDelegatingTo

    it
        "TRANS_NEW_QUIT_02a - Cannot quit with rewards without explicit withdrawal"
        $ \ctx -> runResourceT $ do
            (w, _) <- rewardWallet ctx
            let payload =
                    Json
                        [json|{
                "delegations": [{
                    "quit": {
                        "stake_key_index": "0H"
                    }
                }]
            }|]
            rTx <-
                request @(ApiConstructTransaction n)
                    ctx
                    (Link.createUnsignedTransaction @'Shelley w)
                    Default
                    payload
            verify
                rTx
                [ expectResponseCode HTTP.status403
                ]
            decodeErrorInfo rTx `shouldBe` NonNullRewards

    it
        "TRANS_NEW_QUIT_02b - Can quit with rewards with explicit withdrawal"
        $ \ctx -> runResourceT $ do
            (w, _) <- rewardWallet ctx

            let payload =
                    Json
                        [json|{
                "delegations": [{ "quit": { "stake_key_index": "0H" } }],
                "withdrawal": "self"
            }|]
            rUnsignedTx <-
                request @(ApiConstructTransaction n)
                    ctx
                    (Link.createUnsignedTransaction @'Shelley w)
                    Default
                    payload
            let ApiSerialisedTransaction unsignedTx _ =
                    getFromResponse #transaction rUnsignedTx
            verify
                rUnsignedTx
                [ expectField
                    (#coinSelection . #depositsReturned)
                    (`shouldBe` [ApiAmount 1_000_000])
                , expectField
                    (#coinSelection . #depositsTaken)
                    (`shouldBe` [])
                ]
            signedTx <-
                signTx ctx w unsignedTx [expectResponseCode HTTP.status202]
            submitTxWithWid ctx w signedTx
                >>= flip
                    verify
                    [ expectSuccess
                    , expectResponseCode HTTP.status202
                    ]
