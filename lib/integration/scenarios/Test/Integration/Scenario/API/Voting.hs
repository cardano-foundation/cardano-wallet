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

module Test.Integration.Scenario.API.Voting (spec) where

import Prelude

import Cardano.Wallet.Address.Derivation
    ( DerivationIndex (..)
    )
import Cardano.Wallet.Api.Types
    ( ApiAnyCertificate (..)
    , ApiCertificate (..)
    , ApiConstructTransaction (..)
    , ApiDecodedTransaction
    , ApiPoolSpecifier (..)
    , ApiSerialisedTransaction (..)
    , ApiT (..)
    , ApiTransaction
    , ApiTxId (..)
    , ApiWallet (..)
    , WalletStyle (..)
    )
import Cardano.Wallet.Api.Types.Amount
    ( ApiAmount (ApiAmount)
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
import Numeric.Natural
    ( Natural
    )
import Test.Hspec
    ( SpecWith
    , describe
    )
import Test.Hspec.Expectations.Lifted
    ( shouldBe
    )
import Test.Hspec.Extra
    ( it
    )
import Test.Integration.Framework.DSL
    ( Context (..)
    , Headers (..)
    , Payload (..)
    , counterexample
    , delegating
    , emptyWallet
    , eventually
    , expectField
    , expectResponseCode
    , expectSuccess
    , fixturePassphrase
    , fixtureWallet
    , getFromResponse
    , getResponse
    , joinStakePool
    , json
    , listAddresses
    , minUTxOValue
    , noBabbage
    , notDelegating
    , notRetiringPools
    , onlyVoting
    , quitStakePool
    , request
    , signTx
    , submitTxWithWid
    , verify
    , votingAndDelegating
    , waitForNextEpoch
    , waitNumberOfEpochBoundaries
    , (.<)
    , (.>)
    )

import qualified Cardano.Wallet.Api.Link as Link
import qualified Data.List.NonEmpty as NE
import qualified Network.HTTP.Types.Status as HTTP

spec :: forall n. HasSNetworkId n => SpecWith Context
spec = describe "VOTING_TRANSACTIONS" $ do
    it "VOTING_01a - Can vote and revote and then delegate" $ \ctx -> runResourceT $ do
        noBabbage ctx "voting supported in Conway onwards"
        src <- fixtureWallet ctx

        let depositAmt = ApiAmount 1_000_000
        getSrcWallet ctx src >>= flip verify
            [ expectField #delegation (`shouldBe` notDelegating [])
            ]

        -- voting and re-voting
        _ <- voteAndRevote ctx src depositAmt

        -- quitting, ie. deregistrating the stake key
        quit ctx src depositAmt

        -- voting and re-voting once again
        voting2 <- voteAndRevote ctx src depositAmt

        --Second delegation
        pool1 : _ : _ <- map (view #id) <$> notRetiringPools ctx

        let delegationJoin = Json [json|{
                "delegations": [{
                    "join": {
                        "pool": #{ApiT pool1},
                        "stake_key_index": "0H"
                    }
                }]
            }|]
        rTx3 <- request @(ApiConstructTransaction n) ctx
            (Link.createUnsignedTransaction @'Shelley src) Default delegationJoin
        verify rTx3
            [ expectResponseCode HTTP.status202
            , expectField (#coinSelection . #depositsTaken) (`shouldBe` [])
            , expectField (#coinSelection . #depositsReturned) (`shouldBe` [])
            ]

        let ApiSerialisedTransaction apiTx3 _ = getFromResponse #transaction rTx3
        signedTx3 <- signTx ctx src apiTx3 [ expectResponseCode HTTP.status202 ]

        submittedTx3 <- submitTxWithWid ctx src signedTx3
        verify submittedTx3
            [ expectSuccess
            , expectResponseCode HTTP.status202
            ]

        eventually "Wallet has joined pool and still voting" $ do
            rJoin' <- request @(ApiTransaction n) ctx
                (Link.getTransaction @'Shelley src
                    (getResponse submittedTx3))
                Default Empty
            verify rJoin'
                [ expectResponseCode HTTP.status200
                , expectField (#status . #getApiT) (`shouldBe` InLedger)
                , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
                , expectField #depositTaken (`shouldBe` ApiAmount 0)
                , expectField #depositReturned (`shouldBe` ApiAmount 0)
                ]

        let txId3 = getFromResponse #id submittedTx3
        let link = Link.getTransaction @'Shelley src (ApiTxId txId3)
        eventually "delegation transaction is in ledger" $ do
            request @(ApiTransaction n) ctx link Default Empty
                >>= flip verify
                [ expectResponseCode HTTP.status200
                , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
                , expectField (#status . #getApiT) (`shouldBe` InLedger)
                , expectField #metadata (`shouldBe` Nothing)
                ]

        waitNumberOfEpochBoundaries 2 ctx

        eventually "Wallet is delegating to pool1 and voting abstain" $ do
            getSrcWallet ctx src >>= flip verify
                [ expectField #delegation (`shouldBe` votingAndDelegating (ApiT pool1) voting2 [])
                ]

    it "VOTING_01b - Can vote and revote after delegation" $ \ctx -> runResourceT $ do
        noBabbage ctx "voting supported in Conway onwards"
        src <- fixtureWallet ctx

        pool1 : _pool2 : _ <- map (view #id) <$> notRetiringPools ctx
        let depositAmt = ApiAmount 1_000_000

        --First delegating
        let delegationJoin = Json [json|{
                "delegations": [{
                    "join": {
                        "pool": #{ApiT pool1},
                        "stake_key_index": "0H"
                    }
                }]
            }|]
        rTx1 <- request @(ApiConstructTransaction n) ctx
            (Link.createUnsignedTransaction @'Shelley src) Default delegationJoin
        verify rTx1
            [ expectResponseCode HTTP.status202
            , expectField (#coinSelection . #depositsTaken) (`shouldBe` [depositAmt])
            , expectField (#coinSelection . #depositsReturned) (`shouldBe` [])
            ]

        let ApiSerialisedTransaction apiTx1 _ = getFromResponse #transaction rTx1
        signedTx1 <- signTx ctx src apiTx1 [ expectResponseCode HTTP.status202 ]

        submittedTx1 <- submitTxWithWid ctx src signedTx1
        verify submittedTx1
            [ expectSuccess
            , expectResponseCode HTTP.status202
            ]

        eventually "Wallet has joined pool and deposit info persists" $ do
            rJoin' <- request @(ApiTransaction n) ctx
                (Link.getTransaction @'Shelley src
                    (getResponse submittedTx1))
                Default Empty
            verify rJoin'
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
                >>= flip verify
                [ expectResponseCode HTTP.status200
                , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
                , expectField (#status . #getApiT) (`shouldBe` InLedger)
                , expectField #metadata (`shouldBe` Nothing)
                ]

        waitNumberOfEpochBoundaries 2 ctx

        eventually "Wallet is delegating to pool1" $ do
            getSrcWallet ctx src >>= flip verify
                [ expectField #delegation (`shouldBe` delegating (ApiT pool1) [])
                ]

        -- we get rewards and try to withdraw
        -- it works now but at some point in Conway it is expected to fail
        -- as one will not be able to withdraw without voting.
        dest <- emptyWallet ctx
        waitForNextEpoch ctx

        walletBeforeWithdrawal <- getResponse <$> getSrcWallet ctx src

        addrs <- listAddresses @n ctx dest
        let addr = (addrs !! 1) ^. #id
        let withdrawalAmount = minUTxOValue (_mainEra ctx)

        waitNumberOfEpochBoundaries 4 ctx

        submittedWithdrawalTx <- do
            let endpoint = Link.createTransactionOld @'Shelley src
            request @(ApiTransaction n) ctx endpoint Default
                $ Json [json|
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

        verify submittedWithdrawalTx
            [ expectField #amount (.> ApiAmount withdrawalAmount)
            , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
            ]

        eventually "Rewards have been consumed" $ do
            getSrcWallet ctx src >>= flip verify
                [ expectField (#balance . #reward . #toNatural)
                    (.< withdrawalAmount)
                , expectField (#balance . #available)
                    (.>  (walletBeforeWithdrawal ^. #balance . #available))
                ] & counterexample ("Wdrl: " <> show withdrawalAmount)

        --Now voting
        let voteNoConfidence = Json [json|{
                "vote": "no_confidence"
            }|]
        rTx2 <- request @(ApiConstructTransaction n) ctx
            (Link.createUnsignedTransaction @'Shelley src) Default voteNoConfidence
        verify rTx2
            [ expectResponseCode HTTP.status202
            , expectField (#coinSelection . #depositsTaken) (`shouldBe` [])
            , expectField (#coinSelection . #depositsReturned) (`shouldBe` [])
            ]
        let ApiSerialisedTransaction apiTx2 _ = getFromResponse #transaction rTx2
        signedTx2 <- signTx ctx src apiTx2 [ expectResponseCode HTTP.status202 ]

        let voting1 = ApiT NoConfidence
        let votingCert1 =
                WalletDelegationCertificate $ CastVote stakeKeyDerPath voting1

        let decodePayload2 = Json (toJSON signedTx2)
        rDecodedTx2 <- request @(ApiDecodedTransaction n) ctx
            (Link.decodeTransaction @'Shelley src) Default decodePayload2
        verify rDecodedTx2
            [ expectResponseCode HTTP.status202
            , expectField #certificates (`shouldBe` [votingCert1])
            , expectField #depositsTaken (`shouldBe` [])
            , expectField #depositsReturned (`shouldBe` [])
            ]
        submittedTx2 <- submitTxWithWid ctx src signedTx2
        verify submittedTx2
            [ expectSuccess
            , expectResponseCode HTTP.status202
            ]

        eventually "Wallet has voted" $ do
            rJoin' <- request @(ApiTransaction n) ctx
                (Link.getTransaction @'Shelley src
                    (getResponse submittedTx2))
                Default Empty
            verify rJoin'
                [ expectResponseCode HTTP.status200
                , expectField (#status . #getApiT) (`shouldBe` InLedger)
                , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
                , expectField #depositTaken (`shouldBe` ApiAmount 0)
                , expectField #depositReturned (`shouldBe` ApiAmount 0)
                ]

        let txId2 = getFromResponse #id submittedTx2
        let link2 = Link.getTransaction @'Shelley src (ApiTxId txId2)
        eventually "Voting transaction is in ledger" $ do
            request @(ApiTransaction n) ctx link2 Default Empty
                >>= flip verify
                [ expectResponseCode HTTP.status200
                , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
                , expectField (#status . #getApiT) (`shouldBe` InLedger)
                ]

        waitNumberOfEpochBoundaries 2 ctx

        eventually "Wallet is both voting no confidence and delegating to pool1" $ do
            getSrcWallet ctx src >>= flip verify
                [ expectField #delegation
                      (`shouldBe` votingAndDelegating (ApiT pool1) voting1 [])
                ]

        let voteAbstain = Json [json|{
                "vote": "abstain"
            }|]
        rTx3 <- request @(ApiConstructTransaction n) ctx
            (Link.createUnsignedTransaction @'Shelley src) Default voteAbstain
        verify rTx3
            [ expectResponseCode HTTP.status202
            , expectField (#coinSelection . #depositsTaken) (`shouldBe` [])
            , expectField (#coinSelection . #depositsReturned) (`shouldBe` [])
            ]

        let ApiSerialisedTransaction apiTx3 _ = getFromResponse #transaction rTx3
        signedTx3 <- signTx ctx src apiTx3 [ expectResponseCode HTTP.status202 ]

        let voting2 = ApiT Abstain
        let votingCert2 =
                WalletDelegationCertificate $ CastVote stakeKeyDerPath voting2

        let decodePayload3 = Json (toJSON signedTx3)
        rDecodedTx3 <- request @(ApiDecodedTransaction n) ctx
            (Link.decodeTransaction @'Shelley src) Default decodePayload3
        verify rDecodedTx3
            [ expectResponseCode HTTP.status202
            , expectField #certificates (`shouldBe` [votingCert2])
            , expectField #depositsTaken (`shouldBe` [])
            , expectField #depositsReturned (`shouldBe` [])
            ]

        -- Submit tx
        submittedTx3 <- submitTxWithWid ctx src signedTx3
        verify submittedTx3
            [ expectSuccess
            , expectResponseCode HTTP.status202
            ]

        eventually "Wallet has voted again" $ do
            rJoin' <- request @(ApiTransaction n) ctx
                (Link.getTransaction @'Shelley src
                    (getResponse submittedTx3))
                Default Empty
            verify rJoin'
                [ expectResponseCode HTTP.status200
                , expectField (#status . #getApiT) (`shouldBe` InLedger)
                , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
                , expectField #depositTaken (`shouldBe` ApiAmount 0)
                , expectField #depositReturned (`shouldBe` ApiAmount 0)
                ]

        let txId3 = getFromResponse #id submittedTx3
        let link3 = Link.getTransaction @'Shelley src (ApiTxId txId3)
        eventually "Re-voting transaction is in ledger" $ do
            request @(ApiTransaction n) ctx link3 Default Empty
                >>= flip verify
                [ expectResponseCode HTTP.status200
                , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
                , expectField (#status . #getApiT) (`shouldBe` InLedger)
                ]

        waitNumberOfEpochBoundaries 2 ctx

        eventually "Wallet is both voting abstain and delegating to pool1" $ do
            getSrcWallet ctx src >>= flip verify
                [ expectField #delegation
                      (`shouldBe` votingAndDelegating (ApiT pool1) voting2 [])
                ]

    it "VOTING_01c - Can vote together with delegation" $ \ctx -> runResourceT $ do
        noBabbage ctx "voting supported in Conway onwards"
        src <- fixtureWallet ctx

        pool1 : pool2 : _ <- map (view #id) <$> notRetiringPools ctx
        let depositAmt = ApiAmount 1_000_000
        getSrcWallet ctx src >>= flip verify
            [ expectField #delegation (`shouldBe` notDelegating [])
            ]

        --First vote abstain and delegating to pool1
        let delegationJoinAbstain = Json [json|{
                "delegations": [{
                    "join": {
                        "pool": #{ApiT pool1},
                        "stake_key_index": "0H"
                    }
                }],
                "vote": "abstain"
            }|]
        rTx1 <- request @(ApiConstructTransaction n) ctx
            (Link.createUnsignedTransaction @'Shelley src) Default delegationJoinAbstain
        verify rTx1
            [ expectResponseCode HTTP.status202
            , expectField (#coinSelection . #depositsTaken) (`shouldBe` [depositAmt])
            , expectField (#coinSelection . #depositsReturned) (`shouldBe` [])
            ]

        let ApiSerialisedTransaction apiTx1 _ = getFromResponse #transaction rTx1
        signedTx1 <- signTx ctx src apiTx1 [ expectResponseCode HTTP.status202 ]

        let voting1 = ApiT Abstain
        let votingCert1 =
                WalletDelegationCertificate $ CastVote stakeKeyDerPath voting1
        let registerStakeKeyCert =
                WalletDelegationCertificate $ RegisterRewardAccount stakeKeyDerPath
        let delegatingCert1 =
                WalletDelegationCertificate $ JoinPool stakeKeyDerPath (ApiT pool1)

        let decodePayload1 = Json (toJSON signedTx1)
        rDecodedTx1 <- request @(ApiDecodedTransaction n) ctx
            (Link.decodeTransaction @'Shelley src) Default decodePayload1
        verify rDecodedTx1
            [ expectResponseCode HTTP.status202
            , expectField #certificates (`shouldBe` [registerStakeKeyCert, delegatingCert1, votingCert1])
            , expectField #depositsTaken (`shouldBe` [depositAmt])
            , expectField #depositsReturned (`shouldBe` [])
            ]

        submittedTx1 <- submitTxWithWid ctx src signedTx1
        verify submittedTx1
            [ expectSuccess
            , expectResponseCode HTTP.status202
            ]

        eventually "Wallet has joined pool 1, voted and deposit info persists" $ do
            rJoin' <- request @(ApiTransaction n) ctx
                (Link.getTransaction @'Shelley src
                    (getResponse submittedTx1))
                Default Empty
            verify rJoin'
                [ expectResponseCode HTTP.status200
                , expectField (#status . #getApiT) (`shouldBe` InLedger)
                , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
                , expectField #depositTaken (`shouldBe` depositAmt)
                , expectField #depositReturned (`shouldBe` ApiAmount 0)
                ]

        let txId1 = getFromResponse #id submittedTx1
        let link1 = Link.getTransaction @'Shelley src (ApiTxId txId1)
        eventually "delegation transaction is in ledger" $ do
            request @(ApiTransaction n) ctx link1 Default Empty
                >>= flip verify
                [ expectResponseCode HTTP.status200
                , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
                , expectField (#status . #getApiT) (`shouldBe` InLedger)
                , expectField #metadata (`shouldBe` Nothing)
                ]

        waitNumberOfEpochBoundaries 2 ctx

        eventually "Wallet is delegating to pool1 and voting abstain" $ do
            getSrcWallet ctx src >>= flip verify
                [ expectField #delegation (`shouldBe` votingAndDelegating (ApiT pool1) voting1 [])
                ]

        --Second vote no confidence and delegating to pool2
        let delegationJoinNoConfidence = Json [json|{
                "delegations": [{
                    "join": {
                        "pool": #{ApiT pool2},
                        "stake_key_index": "0H"
                    }
                }],
                "vote": "no_confidence"
            }|]
        rTx2 <- request @(ApiConstructTransaction n) ctx
            (Link.createUnsignedTransaction @'Shelley src) Default delegationJoinNoConfidence
        verify rTx2
            [ expectResponseCode HTTP.status202
            , expectField (#coinSelection . #depositsTaken) (`shouldBe` [])
            , expectField (#coinSelection . #depositsReturned) (`shouldBe` [])
            ]

        let ApiSerialisedTransaction apiTx2 _ = getFromResponse #transaction rTx2
        signedTx2 <- signTx ctx src apiTx2 [ expectResponseCode HTTP.status202 ]

        let voting2 = ApiT NoConfidence
        let votingCert2 =
                WalletDelegationCertificate $ CastVote stakeKeyDerPath voting2
        let delegatingCert2 =
                WalletDelegationCertificate $ JoinPool stakeKeyDerPath (ApiT pool2)

        let decodePayload2 = Json (toJSON signedTx2)
        rDecodedTx2 <- request @(ApiDecodedTransaction n) ctx
            (Link.decodeTransaction @'Shelley src) Default decodePayload2
        verify rDecodedTx2
            [ expectResponseCode HTTP.status202
            , expectField #certificates (`shouldBe` [delegatingCert2, votingCert2])
            , expectField #depositsTaken (`shouldBe` [])
            , expectField #depositsReturned (`shouldBe` [])
            ]

        submittedTx2 <- submitTxWithWid ctx src signedTx2
        verify submittedTx2
            [ expectSuccess
            , expectResponseCode HTTP.status202
            ]

        eventually "Wallet has joined pool 2 and re-voted" $ do
            rJoin' <- request @(ApiTransaction n) ctx
                (Link.getTransaction @'Shelley src
                    (getResponse submittedTx2)) Default Empty
            verify rJoin'
                [ expectResponseCode HTTP.status200
                , expectField (#status . #getApiT) (`shouldBe` InLedger)
                , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
                , expectField #depositTaken (`shouldBe` ApiAmount 0)
                , expectField #depositReturned (`shouldBe` ApiAmount 0)
                ]

        let txId2 = getFromResponse #id submittedTx2
        let link2 = Link.getTransaction @'Shelley src (ApiTxId txId2)
        eventually "delegation transaction is in ledger" $ do
            request @(ApiTransaction n) ctx link2 Default Empty
                >>= flip verify
                [ expectResponseCode HTTP.status200
                , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
                , expectField (#status . #getApiT) (`shouldBe` InLedger)
                , expectField #metadata (`shouldBe` Nothing)
                ]

        waitNumberOfEpochBoundaries 2 ctx

        eventually "Wallet is delegating to pool2 and voting no confidence" $ do
            getSrcWallet ctx src >>= flip verify
                [ expectField #delegation (`shouldBe` votingAndDelegating (ApiT pool2) voting2 [])
                ]

        -- we get rewards
        dest <- emptyWallet ctx
        waitForNextEpoch ctx

        walletBeforeWithdrawal <- getResponse <$> getSrcWallet ctx src

        addrs <- listAddresses @n ctx dest
        let addr = (addrs !! 1) ^. #id
        let withdrawalAmount = minUTxOValue (_mainEra ctx)

        submittedWithdrawalTx <- do
            let endpoint = Link.createTransactionOld @'Shelley src
            request @(ApiTransaction n) ctx endpoint Default
                $ Json [json|
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

        verify submittedWithdrawalTx
            [ expectField #amount (.> ApiAmount withdrawalAmount)
            , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
            ]

        eventually "Rewards have been consumed" $ do
            getSrcWallet ctx src >>= flip verify
                [ expectField (#balance . #reward . #toNatural)
                    (.< withdrawalAmount)
                , expectField (#balance . #available)
                    (.>  (walletBeforeWithdrawal ^. #balance . #available))
                ] & counterexample ("Wdrl: " <> show withdrawalAmount)

        -- now we quit
        quit ctx src depositAmt

    it "VOTING_01d - Can joinStakePool and quitStakePool" $ \ctx -> runResourceT $ do
        noBabbage ctx "voting supported in Conway onwards"

        let depositAmt = ApiAmount 1_000_000
        src <- fixtureWallet ctx
        dest <- emptyWallet ctx
        pool1 : pool2 : _ <- map (view #id) <$> notRetiringPools ctx

        getSrcWallet ctx src >>= flip verify
             [ expectField #delegation (`shouldBe` notDelegating [])
             ]

        -- Join Pool 1
        rJoin1 <- joinStakePool @n ctx (SpecificPool pool1) (src, fixturePassphrase)
        verify rJoin1
            [ expectResponseCode HTTP.status202
            , expectField (#status . #getApiT) (`shouldBe` Pending)
            , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
            , expectField #depositTaken (`shouldBe` depositAmt)
            ]
        eventually "Wallet has joined pool and deposit info persists" $ do
            let endpoint = Link.getTransaction @'Shelley src (getResponse rJoin1)
            request @(ApiTransaction n) ctx endpoint Default Empty
                >>= flip verify
                    [ expectResponseCode HTTP.status200
                    , expectField (#status . #getApiT) (`shouldBe` InLedger)
                    , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
                    , expectField #depositTaken (`shouldBe` depositAmt)
                    , expectField #depositReturned (`shouldBe` ApiAmount 0)
                    ]

        let txId1 = getFromResponse #id rJoin1
        let link1 = Link.getTransaction @'Shelley src (ApiTxId txId1)
        eventually "delegation transaction is in ledger" $ do
            rSrc <- request @(ApiTransaction n) ctx link1 Default Empty
            verify rSrc
                [ expectResponseCode HTTP.status200
                , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
                , expectField (#status . #getApiT) (`shouldBe` InLedger)
                , expectField #metadata (`shouldBe` Nothing)
                , expectField #depositTaken (`shouldBe` depositAmt)
                , expectField #depositReturned (`shouldBe` ApiAmount 0)
                ]

        let voting = ApiT Abstain
        eventually "Wallet is delegating to pool1 and voting abstain" $ do
            getSrcWallet ctx src >>= flip verify
                [ expectField #delegation (`shouldBe` votingAndDelegating (ApiT pool1) voting [])
                ]

        -- Join Pool 2
        rJoin2 <- joinStakePool @n ctx (SpecificPool pool2) (src, fixturePassphrase)
        verify
            rJoin2
            [ expectResponseCode HTTP.status202
            , expectField (#status . #getApiT) (`shouldBe` Pending)
            , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
            , expectField #depositTaken (`shouldBe` ApiAmount 0)
            ]
        eventually "Wallet has joined pool" $ do
            let endpoint = Link.getTransaction @'Shelley src (getResponse rJoin2)
            request @(ApiTransaction n) ctx endpoint Default Empty
                >>= flip verify
                    [ expectResponseCode HTTP.status200
                    , expectField (#status . #getApiT) (`shouldBe` InLedger)
                    , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
                    , expectField #depositTaken (`shouldBe` ApiAmount 0)
                    , expectField #depositReturned (`shouldBe` ApiAmount 0)
                    ]

        let txId2 = getFromResponse #id rJoin2
        let link2 = Link.getTransaction @'Shelley src (ApiTxId txId2)
        eventually "delegation transaction is in ledger" $ do
            rSrc <- request @(ApiTransaction n) ctx link2 Default Empty
            verify rSrc
                [ expectResponseCode HTTP.status200
                , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
                , expectField (#status . #getApiT) (`shouldBe` InLedger)
                , expectField #metadata (`shouldBe` Nothing)
                , expectField #depositTaken (`shouldBe` ApiAmount 0)
                , expectField #depositReturned (`shouldBe` ApiAmount 0)
                ]

        -- Epoch A: delegation tx is in the ledger.
        -- Epoch A+1: stake is registered to a chosen pool.
        -- Epoch A+2: stake is active, rewards start accumulating.
        -- Epoch A+3: rewards from epoch A+2 are calculated.
        -- Epoch A+4: rewards from epoch A+2 are paid out.
        waitNumberOfEpochBoundaries 4 ctx

        previousBalance <- eventually "Wallet gets rewards" $ do
            let endpoint = Link.getWallet @'Shelley src
            r <- request @ApiWallet ctx endpoint Default Empty
            verify r [expectField (#balance . #reward) (.> ApiAmount 0)]
            pure $ getFromResponse (#balance . #available) r

        -- can use rewards with an explicit withdrawal request to self.
        addrs <- listAddresses @n ctx dest
        let coin = minUTxOValue (_mainEra ctx) :: Natural
        let addr = (addrs !! 1) ^. #id
        let payloadWithdrawal = [json|
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

        waitForNextEpoch ctx
        rTx2 <- request @(ApiTransaction n)
                ctx
                (Link.createTransactionOld @'Shelley src)
                Default
                (Json payloadWithdrawal)
        verify rTx2
            [ expectField (#direction . #getApiT) (`shouldBe` Outgoing)
            ]

        -- Rewards are have been consumed.
        eventually "Wallet has consumed rewards" $ do
            request @ApiWallet ctx (Link.getWallet @'Shelley src) Default Empty
                >>= flip verify
                    [ expectField
                        (#balance . #reward)
                        (`shouldBe` (ApiAmount 0))
                    , expectField
                        (#balance . #available)
                        (.> previousBalance)
                    ]

        -- Quit delegation .
        rQuit <- quitStakePool @n ctx (src, fixturePassphrase)
        verify rQuit
            [ expectResponseCode HTTP.status202
            , expectField (#status . #getApiT) (`shouldBe` Pending)
            , expectField (#direction . #getApiT) (`shouldBe` Incoming)
            , expectField #depositTaken (`shouldBe` ApiAmount 0)
            , expectField #depositReturned (`shouldBe` depositAmt)
            ]
        let txid2 = getFromResponse Prelude.id rQuit

        eventually "Certificates are inserted after quitting a pool" $ do
            let epg = Link.getTransaction @'Shelley src txid2
            rlg <- request @(ApiTransaction n) ctx epg Default Empty
            verify rlg
                [ expectField
                    (#direction . #getApiT)
                    (`shouldBe` Incoming)
                , expectField
                    (#status . #getApiT)
                    (`shouldBe` InLedger)
                , expectField #depositTaken (`shouldBe` ApiAmount 0)
                , expectField #depositReturned (`shouldBe` depositAmt)
                ]

        eventually "Wallet is neither delegating nor voting" $ do
            getSrcWallet ctx src >>= flip verify
                [ expectField #delegation (`shouldBe` notDelegating []) ]
  where
    stakeKeyDerPath = NE.fromList
       [ ApiT (DerivationIndex 2_147_485_500)
       , ApiT (DerivationIndex 2_147_485_463)
       , ApiT (DerivationIndex 2_147_483_648)
       , ApiT (DerivationIndex 2)
       , ApiT (DerivationIndex 0)
       ]

    getSrcWallet ctx wal =
       let endpoint = Link.getWallet @'Shelley wal
       in request @ApiWallet ctx endpoint Default Empty

    quit ctx wal depositAmt = do
       -- quitting, ie. deregistrating the stake key
       let delegationQuit = Json [json|{
               "delegations": [{
                   "quit": {
                       "stake_key_index": "0H"
                   }
               }]
           }|]
       rTx <- request @(ApiConstructTransaction n) ctx
           (Link.createUnsignedTransaction @'Shelley wal) Default delegationQuit
       verify rTx
           [ expectResponseCode HTTP.status202
           , expectField (#coinSelection . #depositsTaken) (`shouldBe` [])
           , expectField (#coinSelection . #depositsReturned) (`shouldBe` [depositAmt])
           ]
       let ApiSerialisedTransaction apiTx _ = getFromResponse #transaction rTx
       signedTx <- signTx ctx wal apiTx [ expectResponseCode HTTP.status202 ]
       let quittingCert =
               WalletDelegationCertificate $ QuitPool stakeKeyDerPath

       let decodePayload = Json (toJSON signedTx)
       rDecodedTx <- request @(ApiDecodedTransaction n) ctx
           (Link.decodeTransaction @'Shelley wal) Default decodePayload
       verify rDecodedTx
           [ expectResponseCode HTTP.status202
           , expectField #certificates (`shouldBe` [quittingCert])
           , expectField #depositsReturned (`shouldBe` [depositAmt])
           , expectField #depositsTaken (`shouldBe` [])
           ]
       submittedTx <- submitTxWithWid ctx wal signedTx
       verify submittedTx
           [ expectSuccess
           , expectResponseCode HTTP.status202
           ]

       let txid = getFromResponse #id submittedTx
       let queryTx = Link.getTransaction @'Shelley wal (ApiTxId txid)
       request @(ApiTransaction n) ctx queryTx Default Empty
           >>= flip verify
           [ expectResponseCode HTTP.status200
           , expectField #depositTaken (`shouldBe` ApiAmount 0)
           , expectField #depositReturned (`shouldBe` depositAmt)
           ]

       waitNumberOfEpochBoundaries 2 ctx

       eventually "Wallet is neither delegating nor voting" $ do
           getSrcWallet ctx wal >>= flip verify
               [ expectField #delegation (`shouldBe` notDelegating []) ]

    voteAndRevote ctx wal depositAmt = do
       -- First voting
       let voteNoConfidence = Json [json|{
               "vote": "no_confidence"
           }|]
       rTx1 <- request @(ApiConstructTransaction n) ctx
           (Link.createUnsignedTransaction @'Shelley wal) Default voteNoConfidence
       verify rTx1
           [ expectResponseCode HTTP.status202
           , expectField (#coinSelection . #depositsTaken) (`shouldBe` [depositAmt])
           , expectField (#coinSelection . #depositsReturned) (`shouldBe` [])
           ]

       let ApiSerialisedTransaction apiTx1 _ = getFromResponse #transaction rTx1
       signedTx1 <- signTx ctx wal apiTx1 [ expectResponseCode HTTP.status202 ]

       -- as we are joining for the first time we expect two certificates
       let registerStakeKeyCert =
               WalletDelegationCertificate $ RegisterRewardAccount stakeKeyDerPath
       let voting1 = ApiT NoConfidence
       let votingCert1 =
               WalletDelegationCertificate $ CastVote stakeKeyDerPath voting1

       let decodePayload1 = Json (toJSON signedTx1)
       rDecodedTx1 <- request @(ApiDecodedTransaction n) ctx
           (Link.decodeTransaction @'Shelley wal) Default decodePayload1
       verify rDecodedTx1
           [ expectResponseCode HTTP.status202
           , expectField #certificates (`shouldBe` [registerStakeKeyCert, votingCert1])
           , expectField #depositsTaken (`shouldBe` [depositAmt])
           , expectField #depositsReturned (`shouldBe` [])
           ]

       -- Submit tx
       submittedTx1 <- submitTxWithWid ctx wal signedTx1
       verify submittedTx1
           [ expectSuccess
           , expectResponseCode HTTP.status202
           ]

       eventually "Wallet has voted and deposit info persists" $ do
           rJoin' <- request @(ApiTransaction n) ctx
               (Link.getTransaction @'Shelley wal
                   (getResponse submittedTx1))
               Default Empty
           verify rJoin'
               [ expectResponseCode HTTP.status200
               , expectField (#status . #getApiT) (`shouldBe` InLedger)
               , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
               , expectField #depositTaken (`shouldBe` depositAmt)
               , expectField #depositReturned (`shouldBe` ApiAmount 0)
               ]

       let txId1 = getFromResponse #id submittedTx1
       let link1 = Link.getTransaction @'Shelley wal (ApiTxId txId1)
       eventually "Voting transaction is in ledger" $ do
           request @(ApiTransaction n) ctx link1 Default Empty
               >>= flip verify
               [ expectResponseCode HTTP.status200
               , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
               , expectField (#status . #getApiT) (`shouldBe` InLedger)
               , expectField #metadata (`shouldBe` Nothing)
               ]

       waitNumberOfEpochBoundaries 2 ctx

       eventually "Wallet is voting no confidence" $ do
           getSrcWallet ctx wal >>= flip verify
               [ expectField #delegation (`shouldBe` onlyVoting voting1 [])
               ]

       let voteAbstain = Json [json|{
               "vote": "abstain"
           }|]
       rTx2 <- request @(ApiConstructTransaction n) ctx
           (Link.createUnsignedTransaction @'Shelley wal) Default voteAbstain
       verify rTx2
           [ expectResponseCode HTTP.status202
           , expectField (#coinSelection . #depositsTaken) (`shouldBe` [])
           , expectField (#coinSelection . #depositsReturned) (`shouldBe` [])
           ]

       let ApiSerialisedTransaction apiTx2 _ = getFromResponse #transaction rTx2
       signedTx2 <- signTx ctx wal apiTx2 [ expectResponseCode HTTP.status202 ]

       let voting2 = ApiT Abstain
       let votingCert2 =
               WalletDelegationCertificate $ CastVote stakeKeyDerPath voting2

       let decodePayload2 = Json (toJSON signedTx2)
       rDecodedTx2 <- request @(ApiDecodedTransaction n) ctx
           (Link.decodeTransaction @'Shelley wal) Default decodePayload2
       verify rDecodedTx2
           [ expectResponseCode HTTP.status202
           , expectField #certificates (`shouldBe` [votingCert2])
           , expectField #depositsTaken (`shouldBe` [])
           , expectField #depositsReturned (`shouldBe` [])
           ]

       -- Submit tx
       submittedTx2 <- submitTxWithWid ctx wal signedTx2
       verify submittedTx2
           [ expectSuccess
           , expectResponseCode HTTP.status202
           ]

       eventually "Wallet has voted again" $ do
           rJoin' <- request @(ApiTransaction n) ctx
               (Link.getTransaction @'Shelley wal
                   (getResponse submittedTx2))
               Default Empty
           verify rJoin'
               [ expectResponseCode HTTP.status200
               , expectField (#status . #getApiT) (`shouldBe` InLedger)
               , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
               , expectField #depositTaken (`shouldBe` ApiAmount 0)
               , expectField #depositReturned (`shouldBe` ApiAmount 0)
               ]

       let txId2 = getFromResponse #id submittedTx2
       let link2 = Link.getTransaction @'Shelley wal (ApiTxId txId2)
       eventually "Re-voting transaction is in ledger" $ do
           request @(ApiTransaction n) ctx link2 Default Empty
               >>= flip verify
               [ expectResponseCode HTTP.status200
               , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
               , expectField (#status . #getApiT) (`shouldBe` InLedger)
               , expectField #metadata (`shouldBe` Nothing)
               ]

       waitNumberOfEpochBoundaries 2 ctx

       eventually "Wallet is voting abstain" $ do
           getSrcWallet ctx wal >>= flip verify
               [ expectField #delegation (`shouldBe` onlyVoting voting2 [])
               ]

       return voting2
