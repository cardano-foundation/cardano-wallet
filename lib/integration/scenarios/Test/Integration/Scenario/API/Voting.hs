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

import Cardano.Wallet.Address.Derivation
    ( DerivationIndex (..)
    )
import Cardano.Wallet.Api.Types
    ( ApiAnyCertificate (..)
    , ApiCertificate (..)
    , ApiCoinSelection
    , ApiConstructTransaction (..)
    , ApiDRepCredential (..)
    , ApiDRepInfo (..)
    , ApiDRepSpecifier (..)
    , ApiDecodedTransaction
    , ApiFee
    , ApiPoolSpecifier (..)
    , ApiSerialisedTransaction (..)
    , ApiT (..)
    , ApiTransaction
    , ApiTxId (..)
    , ApiWallet (..)
    , ApiWalletDelegation
    , ApiWalletLedgerVote (..)
    , ApiWalletMigrationPlan
    , DRepStatus (..)
    , WalletStyle (..)
    )
import Cardano.Wallet.Api.Types.Amount
    ( ApiAmount (ApiAmount)
    )
import Cardano.Wallet.Api.Types.Error
    ( ApiErrorInfo (..)
    )
import Cardano.Wallet.Primitive.NetworkId
    ( HasSNetworkId
    )
import Cardano.Wallet.Primitive.Types.DRep
    ( DRep (..)
    , DRepID (..)
    , DRepKeyHash
    )
import Cardano.Wallet.Primitive.Types.Tx.TxMeta
    ( Direction (..)
    , TxStatus (..)
    )
import Control.Monad
    ( forM_
    )
import Control.Monad.IO.Unlift
    ( MonadIO (..)
    , MonadUnliftIO
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
    , shouldSatisfy
    )
import Test.Hspec.Extra
    ( it
    )
import Test.Integration.Framework.DSL
    ( Context (..)
    , Headers (..)
    , Payload (..)
    , RequestException
    , counterexample
    , decodeErrorInfo
    , delegating
    , emptyWallet
    , eventually
    , expectField
    , expectResponseCode
    , expectSuccess
    , fixturePassphrase
    , fixtureShelleyWallet
    , fixtureWallet
    , getDRep
    , getFromResponse
    , getResponse
    , joinDRep
    , joinStakePool
    , json
    , listAddresses
    , listDReps
    , listTransactions
    , minUTxOValue
    , noBabbage
    , noConway
    , notDelegating
    , notRetiringPools
    , onlyVoting
    , postWallet
    , quitStakePool
    , quitStakePoolUnsigned
    , request
    , rewardWallet
    , shouldHaveDelegationHistory
    , signTx
    , submitTxWithWid
    , suggestedDReps
    , verify
    , votingAndDelegating
    , waitForNextEpoch
    , waitNumberOfEpochBoundaries
    , (.<)
    , (.>)
    )
import Test.Integration.Framework.TestData
    ( payloadWith
    )
import Prelude

import qualified Cardano.Wallet.Api.Link as Link
import qualified Data.List.NonEmpty as NE
import qualified Network.HTTP.Types.Status as HTTP

spec :: forall n. HasSNetworkId n => SpecWith Context
spec = describe "VOTING_TRANSACTIONS" $ do
    it "VOTING_01a - Can vote and revote and then delegate" $ \ctx -> runResourceT $ do
        noBabbage ctx "voting supported in Conway onwards"
        src <- fixtureWallet ctx

        let depositAmt = ApiAmount 1_000_000
        getSrcWallet ctx src
            >>= flip
                verify
                [ expectField
                    #delegation
                    (`shouldHaveDelegationHistory` notDelegating [])
                ]

        -- voting and re-voting
        _ <- voteAndRevote ctx src depositAmt

        -- we are voting abstain now
        let voteAbstainAgain =
                Json
                    [json|{
                "vote": "abstain"
            }|]
        rTx2 <-
            request @(ApiConstructTransaction n)
                ctx
                (Link.createUnsignedTransaction @'Shelley src)
                Default
                voteAbstainAgain
        decodeErrorInfo rTx2 `shouldBe` SameVote

        -- quitting, ie. deregistrating the stake key
        quit ctx src depositAmt

        -- voting and re-voting once again
        voting2 <- voteAndRevote ctx src depositAmt

        -- Second delegation
        pool1 : _ : _ <- map (view #id) <$> notRetiringPools ctx

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
        rTx3 <-
            request @(ApiConstructTransaction n)
                ctx
                (Link.createUnsignedTransaction @'Shelley src)
                Default
                delegationJoin
        verify
            rTx3
            [ expectResponseCode HTTP.status202
            , expectField (#coinSelection . #depositsTaken) (`shouldBe` [])
            , expectField (#coinSelection . #depositsReturned) (`shouldBe` [])
            ]

        let ApiSerialisedTransaction apiTx3 _ = getFromResponse #transaction rTx3
        signedTx3 <-
            signTx ctx src apiTx3 [expectResponseCode HTTP.status202]

        submittedTx3 <- submitTxWithWid ctx src signedTx3
        verify
            submittedTx3
            [ expectSuccess
            , expectResponseCode HTTP.status202
            ]

        eventually "Wallet has joined pool and still voting" $ do
            rJoin' <-
                request @(ApiTransaction n)
                    ctx
                    ( Link.getTransaction @'Shelley
                        src
                        (getResponse submittedTx3)
                    )
                    Default
                    Empty
            verify
                rJoin'
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
                >>= flip
                    verify
                    [ expectResponseCode HTTP.status200
                    , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
                    , expectField (#status . #getApiT) (`shouldBe` InLedger)
                    , expectField #metadata (`shouldBe` Nothing)
                    ]

        waitNumberOfEpochBoundaries 2 ctx

        eventually "Wallet is delegating to pool1 and voting abstain" $ do
            getSrcWallet ctx src
                >>= flip
                    verify
                    [ expectField
                        #delegation
                        ( `shouldHaveDelegationHistory`
                            votingAndDelegating (ApiT pool1) voting2 []
                        )
                    ]

    it "VOTING_01b - Can vote and revote after delegation" $ \ctx -> runResourceT $ do
        noBabbage ctx "voting supported in Conway onwards"
        -- pendingWith "Node 10.6.2 ledger rule change: WithdrawalsNotInRewardsCERTS when delegation and voting are in separate transactions. See #5210"
        src <- fixtureWallet ctx

        pool1 : _pool2 : _ <- map (view #id) <$> notRetiringPools ctx
        let depositAmt = ApiAmount 1_000_000

        -- First delegating
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

        let registerStakeKeyCert = RegisterRewardAccount stakeKeyDerPath
        let delegatingCert = JoinPool stakeKeyDerPath (ApiT pool1)

        verify
            rTx1
            [ expectResponseCode HTTP.status202
            , expectField
                (#coinSelection . #depositsTaken)
                (`shouldBe` [depositAmt])
            , expectField (#coinSelection . #depositsReturned) (`shouldBe` [])
            , expectField
                (#coinSelection . #certificates)
                (`shouldBe` Just (registerStakeKeyCert NE.:| [delegatingCert]))
            ]

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
                    ]

        waitNumberOfEpochBoundaries 2 ctx

        eventually "Wallet is delegating to pool1" $ do
            getSrcWallet ctx src
                >>= flip
                    verify
                    [ expectField
                        #delegation
                        (`shouldHaveDelegationHistory` delegating (ApiT pool1) [])
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

        -- Now voting
        let voteNoConfidence =
                Json
                    [json|{
                "vote": "no_confidence"
            }|]
        rTx2 <-
            request @(ApiConstructTransaction n)
                ctx
                (Link.createUnsignedTransaction @'Shelley src)
                Default
                voteNoConfidence

        let voting1 = ApiT NoConfidence
        let votingCert1 = CastVote stakeKeyDerPath voting1

        verify
            rTx2
            [ expectResponseCode HTTP.status202
            , expectField (#coinSelection . #depositsTaken) (`shouldBe` [])
            , expectField (#coinSelection . #depositsReturned) (`shouldBe` [])
            , expectField
                (#coinSelection . #certificates)
                (`shouldBe` Just (votingCert1 NE.:| []))
            ]
        let ApiSerialisedTransaction apiTx2 _ = getFromResponse #transaction rTx2
        signedTx2 <-
            signTx ctx src apiTx2 [expectResponseCode HTTP.status202]

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
            , expectField
                #certificates
                (`shouldBe` [WalletDelegationCertificate votingCert1])
            , expectField #depositsTaken (`shouldBe` [])
            , expectField #depositsReturned (`shouldBe` [])
            ]
        submittedTx2 <- submitTxWithWid ctx src signedTx2
        verify
            submittedTx2
            [ expectSuccess
            , expectResponseCode HTTP.status202
            ]

        eventually "Wallet has voted" $ do
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
                , expectField (#status . #getApiT) (`shouldBe` InLedger)
                , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
                , expectField #depositTaken (`shouldBe` ApiAmount 0)
                , expectField #depositReturned (`shouldBe` ApiAmount 0)
                ]

        let txId2 = getFromResponse #id submittedTx2
        let link2 = Link.getTransaction @'Shelley src (ApiTxId txId2)
        eventually "Voting transaction is in ledger" $ do
            request @(ApiTransaction n) ctx link2 Default Empty
                >>= flip
                    verify
                    [ expectResponseCode HTTP.status200
                    , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
                    , expectField (#status . #getApiT) (`shouldBe` InLedger)
                    ]

        waitNumberOfEpochBoundaries 2 ctx

        eventually
            "Wallet is both voting no confidence and delegating to pool1"
            $ do
                getSrcWallet ctx src
                    >>= flip
                        verify
                        [ expectField
                            #delegation
                            ( `shouldHaveDelegationHistory`
                                votingAndDelegating (ApiT pool1) voting1 []
                            )
                        ]

        let voteAbstain =
                Json
                    [json|{
                "vote": "abstain"
            }|]
        rTx3 <-
            request @(ApiConstructTransaction n)
                ctx
                (Link.createUnsignedTransaction @'Shelley src)
                Default
                voteAbstain

        let voting2 = ApiT Abstain
        let votingCert2 = CastVote stakeKeyDerPath voting2

        verify
            rTx3
            [ expectResponseCode HTTP.status202
            , expectField (#coinSelection . #depositsTaken) (`shouldBe` [])
            , expectField (#coinSelection . #depositsReturned) (`shouldBe` [])
            , expectField
                (#coinSelection . #certificates)
                (`shouldBe` Just (votingCert2 NE.:| []))
            ]

        let ApiSerialisedTransaction apiTx3 _ = getFromResponse #transaction rTx3
        signedTx3 <-
            signTx ctx src apiTx3 [expectResponseCode HTTP.status202]

        let decodePayload3 = Json (toJSON signedTx3)
        rDecodedTx3 <-
            request @(ApiDecodedTransaction n)
                ctx
                (Link.decodeTransaction @'Shelley src)
                Default
                decodePayload3
        verify
            rDecodedTx3
            [ expectResponseCode HTTP.status202
            , expectField
                #certificates
                (`shouldBe` [WalletDelegationCertificate votingCert2])
            , expectField #depositsTaken (`shouldBe` [])
            , expectField #depositsReturned (`shouldBe` [])
            ]

        -- Submit tx
        submittedTx3 <- submitTxWithWid ctx src signedTx3
        verify
            submittedTx3
            [ expectSuccess
            , expectResponseCode HTTP.status202
            ]

        eventually "Wallet has voted again" $ do
            rJoin' <-
                request @(ApiTransaction n)
                    ctx
                    ( Link.getTransaction @'Shelley
                        src
                        (getResponse submittedTx3)
                    )
                    Default
                    Empty
            verify
                rJoin'
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
                >>= flip
                    verify
                    [ expectResponseCode HTTP.status200
                    , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
                    , expectField (#status . #getApiT) (`shouldBe` InLedger)
                    ]

        waitNumberOfEpochBoundaries 2 ctx

        eventually "Wallet is both voting abstain and delegating to pool1" $ do
            getSrcWallet ctx src
                >>= flip
                    verify
                    [ expectField
                        #delegation
                        ( `shouldHaveDelegationHistory`
                            votingAndDelegating (ApiT pool1) voting2 []
                        )
                    ]

        withdrawalSucesss <- do
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
            withdrawalSucesss
            [ expectField #amount (.> ApiAmount withdrawalAmount)
            , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
            ]

        eventually "Rewards have been consumed" $ do
            getSrcWallet ctx src
                >>= flip
                    verify
                    [ expectField
                        (#balance . #reward . #toNatural)
                        (.< withdrawalAmount)
                    , expectField
                        (#balance . #available)
                        (.> (walletBeforeWithdrawal ^. #balance . #available))
                    ]
                & counterexample ("Wdrl: " <> show withdrawalAmount)

    it "VOTING_01c - Can vote together with delegation" $ \ctx -> runResourceT $ do
        noBabbage ctx "voting supported in Conway onwards"
        src <- fixtureWallet ctx

        pool1 : pool2 : _ <- map (view #id) <$> notRetiringPools ctx
        let depositAmt = ApiAmount 1_000_000
        getSrcWallet ctx src
            >>= flip
                verify
                [ expectField
                    #delegation
                    (`shouldHaveDelegationHistory` notDelegating [])
                ]

        -- First vote abstain and delegating to pool1
        let delegationJoinAbstain =
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
        rTx1 <-
            request @(ApiConstructTransaction n)
                ctx
                (Link.createUnsignedTransaction @'Shelley src)
                Default
                delegationJoinAbstain

        let voting1 = ApiT Abstain
        let votingCert1 = CastVote stakeKeyDerPath voting1
        let registerStakeKeyCert = RegisterRewardAccount stakeKeyDerPath
        let delegatingCert1 = JoinPool stakeKeyDerPath (ApiT pool1)

        verify
            rTx1
            [ expectResponseCode HTTP.status202
            , expectField
                (#coinSelection . #depositsTaken)
                (`shouldBe` [depositAmt])
            , expectField (#coinSelection . #depositsReturned) (`shouldBe` [])
            , expectField
                (#coinSelection . #certificates)
                ( `shouldBe`
                    Just (registerStakeKeyCert NE.:| [delegatingCert1, votingCert1])
                )
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
            , expectField
                #certificates
                ( `shouldBe`
                    WalletDelegationCertificate
                        <$> [registerStakeKeyCert, delegatingCert1, votingCert1]
                )
            , expectField #depositsTaken (`shouldBe` [depositAmt])
            , expectField #depositsReturned (`shouldBe` [])
            ]

        submittedTx1 <- submitTxWithWid ctx src signedTx1
        verify
            submittedTx1
            [ expectSuccess
            , expectResponseCode HTTP.status202
            ]

        eventually "Wallet has joined pool 1, voted and deposit info persists" $ do
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
        let link1 = Link.getTransaction @'Shelley src (ApiTxId txId1)
        eventually "delegation transaction is in ledger" $ do
            request @(ApiTransaction n) ctx link1 Default Empty
                >>= flip
                    verify
                    [ expectResponseCode HTTP.status200
                    , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
                    , expectField (#status . #getApiT) (`shouldBe` InLedger)
                    , expectField #metadata (`shouldBe` Nothing)
                    ]

        waitNumberOfEpochBoundaries 2 ctx

        eventually "Wallet is delegating to pool1 and voting abstain" $ do
            getSrcWallet ctx src
                >>= flip
                    verify
                    [ expectField
                        #delegation
                        ( `shouldHaveDelegationHistory`
                            votingAndDelegating (ApiT pool1) voting1 []
                        )
                    ]

        -- Second vote no confidence and delegating to pool2
        let delegationJoinNoConfidence =
                Json
                    [json|{
                "delegations": [{
                    "join": {
                        "pool": #{ApiT pool2},
                        "stake_key_index": "0H"
                    }
                }],
                "vote": "no_confidence"
            }|]
        rTx2 <-
            request @(ApiConstructTransaction n)
                ctx
                (Link.createUnsignedTransaction @'Shelley src)
                Default
                delegationJoinNoConfidence

        let voting2 = ApiT NoConfidence
        let votingCert2 = CastVote stakeKeyDerPath voting2
        let delegatingCert2 = JoinPool stakeKeyDerPath (ApiT pool2)

        verify
            rTx2
            [ expectResponseCode HTTP.status202
            , expectField (#coinSelection . #depositsTaken) (`shouldBe` [])
            , expectField (#coinSelection . #depositsReturned) (`shouldBe` [])
            , expectField
                (#coinSelection . #certificates)
                (`shouldBe` Just (delegatingCert2 NE.:| [votingCert2]))
            ]

        let ApiSerialisedTransaction apiTx2 _ = getFromResponse #transaction rTx2
        signedTx2 <-
            signTx ctx src apiTx2 [expectResponseCode HTTP.status202]

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
            , expectField
                #certificates
                ( `shouldBe`
                    WalletDelegationCertificate <$> [delegatingCert2, votingCert2]
                )
            , expectField #depositsTaken (`shouldBe` [])
            , expectField #depositsReturned (`shouldBe` [])
            ]

        submittedTx2 <- submitTxWithWid ctx src signedTx2
        verify
            submittedTx2
            [ expectSuccess
            , expectResponseCode HTTP.status202
            ]

        eventually "Wallet has joined pool 2 and re-voted" $ do
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
                , expectField (#status . #getApiT) (`shouldBe` InLedger)
                , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
                , expectField #depositTaken (`shouldBe` ApiAmount 0)
                , expectField #depositReturned (`shouldBe` ApiAmount 0)
                ]

        let txId2 = getFromResponse #id submittedTx2
        let link2 = Link.getTransaction @'Shelley src (ApiTxId txId2)
        eventually "delegation transaction is in ledger" $ do
            request @(ApiTransaction n) ctx link2 Default Empty
                >>= flip
                    verify
                    [ expectResponseCode HTTP.status200
                    , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
                    , expectField (#status . #getApiT) (`shouldBe` InLedger)
                    , expectField #metadata (`shouldBe` Nothing)
                    ]

        waitNumberOfEpochBoundaries 2 ctx

        eventually "Wallet is delegating to pool2 and voting no confidence" $ do
            getSrcWallet ctx src
                >>= flip
                    verify
                    [ expectField
                        #delegation
                        ( `shouldHaveDelegationHistory`
                            votingAndDelegating (ApiT pool2) voting2 []
                        )
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
            getSrcWallet ctx src
                >>= flip
                    verify
                    [ expectField
                        (#balance . #reward . #toNatural)
                        (.< withdrawalAmount)
                    , expectField
                        (#balance . #available)
                        (.> (walletBeforeWithdrawal ^. #balance . #available))
                    ]
                & counterexample ("Wdrl: " <> show withdrawalAmount)

        -- now we quit
        quit ctx src depositAmt

    it "VOTING_01d - Can joinStakePool and quitStakePool" $ \ctx -> runResourceT $ do
        noBabbage ctx "voting supported in Conway onwards"

        let depositAmt = ApiAmount 1_000_000
        src <- fixtureWallet ctx
        dest <- emptyWallet ctx
        pool1 : pool2 : _ <- map (view #id) <$> notRetiringPools ctx

        getSrcWallet ctx src
            >>= flip
                verify
                [ expectField
                    #delegation
                    (`shouldHaveDelegationHistory` notDelegating [])
                ]

        -- Join Pool 1
        rJoin1 <-
            joinStakePool @n ctx (SpecificPool pool1) (src, fixturePassphrase)
        verify
            rJoin1
            [ expectResponseCode HTTP.status202
            , expectField (#status . #getApiT) (`shouldBe` Pending)
            , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
            , expectField #depositTaken (`shouldBe` depositAmt)
            ]
        eventually "Wallet has joined pool and deposit info persists" $ do
            let endpoint = Link.getTransaction @'Shelley src (getResponse rJoin1)
            request @(ApiTransaction n) ctx endpoint Default Empty
                >>= flip
                    verify
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
            verify
                rSrc
                [ expectResponseCode HTTP.status200
                , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
                , expectField (#status . #getApiT) (`shouldBe` InLedger)
                , expectField #metadata (`shouldBe` Nothing)
                , expectField #depositTaken (`shouldBe` depositAmt)
                , expectField #depositReturned (`shouldBe` ApiAmount 0)
                ]

        let voting = ApiT Abstain
        eventually "Wallet is delegating to pool1 and voting abstain" $ do
            getSrcWallet ctx src
                >>= flip
                    verify
                    [ expectField
                        #delegation
                        ( `shouldHaveDelegationHistory`
                            votingAndDelegating (ApiT pool1) voting []
                        )
                    ]

        -- Join Pool 2
        rJoin2 <-
            joinStakePool @n ctx (SpecificPool pool2) (src, fixturePassphrase)
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
                >>= flip
                    verify
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
            verify
                rSrc
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
        let payloadWithdrawal =
                [json|
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
        rTx2 <-
            request @(ApiTransaction n)
                ctx
                (Link.createTransactionOld @'Shelley src)
                Default
                (Json payloadWithdrawal)
        verify
            rTx2
            [ expectField (#direction . #getApiT) (`shouldBe` Outgoing)
            ]

        -- Rewards are have been consumed.
        eventually "Wallet has consumed rewards" $ do
            request @ApiWallet ctx (Link.getWallet @'Shelley src) Default Empty
                >>= flip
                    verify
                    [ expectField
                        (#balance . #reward)
                        (`shouldBe` (ApiAmount 0))
                    , expectField
                        (#balance . #available)
                        (.> previousBalance)
                    ]

        -- Quit delegation .
        rQuit <- quitStakePool @n ctx (src, fixturePassphrase)
        verify
            rQuit
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
            verify
                rlg
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
            getSrcWallet ctx src
                >>= flip
                    verify
                    [ expectField
                        #delegation
                        (`shouldHaveDelegationHistory` notDelegating [])
                    ]

    it "VOTING_01e - Cannot vote in Babbage"
        $ \ctx -> runResourceT $ do
            noConway ctx "voting outlawed before Conway"
            w <- fixtureWallet ctx
            rTx <- joinDRep @n ctx (SpecificDRep Abstain) (w, fixturePassphrase)
            verify
                rTx
                [ expectResponseCode HTTP.status403
                ]
            decodeErrorInfo rTx `shouldBe` VotingInInvalidEra

    it "VOTING_01f - Voting works in Conway in absence of pool delegation"
        $ \ctx -> runResourceT $ do
            noBabbage ctx "voting can be checked only in Conway onwards"
            w <- fixtureWallet ctx

            eventually "Initially wallet is neither delegating nor voting" $ do
                getSrcWallet ctx w
                    >>= flip
                        verify
                        [ expectField
                            #delegation
                            (`shouldHaveDelegationHistory` notDelegating [])
                        ]

            let voting1 = Abstain
            rTx1 <- joinDRep @n ctx (SpecificDRep voting1) (w, fixturePassphrase)
            verify
                rTx1
                [ expectResponseCode HTTP.status202
                ]

            eventually "Wallet is voting Abstain" $ do
                getSrcWallet ctx w
                    >>= flip
                        verify
                        [ expectField
                            #delegation
                            (`shouldHaveDelegationHistory` onlyVoting (ApiT voting1) [])
                        ]

            let voting2 = NoConfidence
            rTx2 <- joinDRep @n ctx (SpecificDRep voting2) (w, fixturePassphrase)
            verify
                rTx2
                [ expectResponseCode HTTP.status202
                ]

            eventually "Wallet is voting NoConfidence" $ do
                getSrcWallet ctx w
                    >>= flip
                        verify
                        [ expectField
                            #delegation
                            (`shouldHaveDelegationHistory` onlyVoting (ApiT voting2) [])
                        ]

            rTx3 <- joinDRep @n ctx (SpecificDRep voting2) (w, fixturePassphrase)
            verify
                rTx3
                [ expectResponseCode HTTP.status403
                ]
            decodeErrorInfo rTx3 `shouldBe` SameVote

    it
        "VOTING_01g - Voting works in Conway in presence of pool delegation"
        $ \ctx -> runResourceT $ do
            noBabbage ctx "voting can be checked only in Conway onwards"
            w <- fixtureWallet ctx

            eventually "Initially wallet is neither delegating nor voting" $ do
                getSrcWallet ctx w
                    >>= flip
                        verify
                        [ expectField
                            #delegation
                            (`shouldHaveDelegationHistory` notDelegating [])
                        ]

            pool1 : _pool2 : _ <- map (view #id) <$> notRetiringPools ctx

            -- Join Pool
            rJoin <-
                joinStakePool @n ctx (SpecificPool pool1) (w, fixturePassphrase)
            verify
                rJoin
                [ expectResponseCode HTTP.status202
                ]
            let voting1 = Abstain
            eventually "Wallet is voting Abstain" $ do
                getSrcWallet ctx w
                    >>= flip
                        verify
                        [ expectField
                            #delegation
                            ( `shouldHaveDelegationHistory`
                                votingAndDelegating (ApiT pool1) (ApiT voting1) []
                            )
                        ]

            let voting2 = NoConfidence
            rTx2 <- joinDRep @n ctx (SpecificDRep voting2) (w, fixturePassphrase)
            verify
                rTx2
                [ expectResponseCode HTTP.status202
                ]

            eventually "Wallet is voting NoConfidence" $ do
                getSrcWallet ctx w
                    >>= flip
                        verify
                        [ expectField
                            #delegation
                            ( `shouldHaveDelegationHistory`
                                votingAndDelegating (ApiT pool1) (ApiT voting2) []
                            )
                        ]

            rTx3 <- joinDRep @n ctx (SpecificDRep voting2) (w, fixturePassphrase)
            verify
                rTx3
                [ expectResponseCode HTTP.status403
                ]
            decodeErrorInfo rTx3 `shouldBe` SameVote

    it
        "VOTING_01h - Delegation works in Conway in presence of voting and does not change it"
        $ \ctx -> runResourceT $ do
            noBabbage ctx "voting can be checked only in Conway onwards"
            w <- fixtureWallet ctx

            eventually "Initially wallet is neither delegating nor voting" $ do
                getSrcWallet ctx w
                    >>= flip
                        verify
                        [ expectField
                            #delegation
                            (`shouldHaveDelegationHistory` notDelegating [])
                        ]

            pool1 : pool2 : _ <- map (view #id) <$> notRetiringPools ctx

            let voting = NoConfidence
            rTx <- joinDRep @n ctx (SpecificDRep voting) (w, fixturePassphrase)
            verify
                rTx
                [ expectResponseCode HTTP.status202
                ]

            eventually "Wallet is voting NoConfidence" $ do
                getSrcWallet ctx w
                    >>= flip
                        verify
                        [ expectField
                            #delegation
                            (`shouldHaveDelegationHistory` onlyVoting (ApiT voting) [])
                        ]

            -- Join Pool
            rJoin1 <-
                joinStakePool @n ctx (SpecificPool pool1) (w, fixturePassphrase)
            verify
                rJoin1
                [ expectResponseCode HTTP.status202
                ]
            eventually "Wallet is voting NoConfidence and delegating to pool1" $ do
                getSrcWallet ctx w
                    >>= flip
                        verify
                        [ expectField
                            #delegation
                            ( `shouldHaveDelegationHistory`
                                votingAndDelegating (ApiT pool1) (ApiT voting) []
                            )
                        ]
            rJoin2 <-
                joinStakePool @n ctx (SpecificPool pool2) (w, fixturePassphrase)
            verify
                rJoin2
                [ expectResponseCode HTTP.status202
                ]
            eventually "Wallet is voting NoConfidence and delegating to pool2" $ do
                getSrcWallet ctx w
                    >>= flip
                        verify
                        [ expectField
                            #delegation
                            ( `shouldHaveDelegationHistory`
                                votingAndDelegating (ApiT pool2) (ApiT voting) []
                            )
                        ]

    it
        "VOTING_01i - Delegation works in Conway in presence of voting and does not change it"
        $ \ctx -> runResourceT $ do
            noBabbage ctx "voting can be checked only in Conway onwards"
            w <- fixtureWallet ctx

            eventually "Initially wallet is neither delegating nor voting" $ do
                getSrcWallet ctx w
                    >>= flip
                        verify
                        [ expectField
                            #delegation
                            (`shouldHaveDelegationHistory` notDelegating [])
                        ]

            pool1 : pool2 : _ <- map (view #id) <$> notRetiringPools ctx

            let voting1 = Abstain

            -- Join Pool
            rJoin1 <-
                joinStakePool @n ctx (SpecificPool pool1) (w, fixturePassphrase)
            verify
                rJoin1
                [ expectResponseCode HTTP.status202
                ]
            eventually "Wallet is voting Abstain and delegating to pool1" $ do
                getSrcWallet ctx w
                    >>= flip
                        verify
                        [ expectField
                            #delegation
                            ( `shouldHaveDelegationHistory`
                                votingAndDelegating (ApiT pool1) (ApiT voting1) []
                            )
                        ]

            let voting2 = NoConfidence
            rTx <- joinDRep @n ctx (SpecificDRep voting2) (w, fixturePassphrase)
            verify
                rTx
                [ expectResponseCode HTTP.status202
                ]

            eventually "Wallet is voting NoConfidence and delegating to pool1" $ do
                getSrcWallet ctx w
                    >>= flip
                        verify
                        [ expectField
                            #delegation
                            ( `shouldHaveDelegationHistory`
                                votingAndDelegating (ApiT pool1) (ApiT voting2) []
                            )
                        ]

            -- Rejoin Pool
            rJoin2 <-
                joinStakePool @n ctx (SpecificPool pool2) (w, fixturePassphrase)
            verify
                rJoin2
                [ expectResponseCode HTTP.status202
                ]
            eventually "Wallet is voting NoConfidence and delegating to pool2" $ do
                getSrcWallet ctx w
                    >>= flip
                        verify
                        [ expectField
                            #delegation
                            ( `shouldHaveDelegationHistory`
                                votingAndDelegating (ApiT pool2) (ApiT voting2) []
                            )
                        ]

    it
        "VOTING_01j - Delegation works in Conway in presence of voting after wallet delations and restoration"
        $ \ctx -> runResourceT $ do
            noBabbage ctx "voting can be checked only in Conway onwards"
            (w, mnemonic) <- fixtureShelleyWallet ctx

            eventually "Initially wallet is neither delegating nor voting" $ do
                getSrcWallet ctx w
                    >>= flip
                        verify
                        [ expectField
                            #delegation
                            (`shouldHaveDelegationHistory` notDelegating [])
                        ]

            let voting = NoConfidence
            rTx <- joinDRep @n ctx (SpecificDRep voting) (w, fixturePassphrase)
            verify
                rTx
                [ expectResponseCode HTTP.status202
                ]

            eventually "Wallet is voting NoConfidence" $ do
                getSrcWallet ctx w
                    >>= flip
                        verify
                        [ expectField
                            #delegation
                            (`shouldHaveDelegationHistory` onlyVoting (ApiT voting) [])
                        ]

            -- delete the wallet
            rDel1 <-
                request @ApiWallet ctx (Link.deleteWallet @'Shelley w) Default Empty
            expectResponseCode HTTP.status204 rDel1

            -- restoring the wallet
            let payldCrt = payloadWith "Faucet Wallet" mnemonic
            rRestore1 <- postWallet ctx payldCrt
            verify
                rRestore1
                [ expectResponseCode HTTP.status201
                ]

            let w' = getFromResponse Prelude.id rRestore1
            eventually "Wallet is voting NoConfidence after restoration" $ do
                getSrcWallet ctx w'
                    >>= flip
                        verify
                        [ expectField
                            #delegation
                            (`shouldHaveDelegationHistory` onlyVoting (ApiT voting) [])
                        ]

            pool1 : _ : _ <- map (view #id) <$> notRetiringPools ctx

            -- Join Pool
            rJoin <-
                joinStakePool @n ctx (SpecificPool pool1) (w', fixturePassphrase)
            verify
                rJoin
                [ expectResponseCode HTTP.status202
                ]
            eventually "Wallet is voting NoConfidence and delegating to pool1" $ do
                getSrcWallet ctx w'
                    >>= flip
                        verify
                        [ expectField
                            #delegation
                            ( `shouldHaveDelegationHistory`
                                votingAndDelegating (ApiT pool1) (ApiT voting) []
                            )
                        ]

            -- delete the wallet once again
            rDel2 <-
                request @ApiWallet ctx (Link.deleteWallet @'Shelley w') Default Empty
            expectResponseCode HTTP.status204 rDel2

            -- restoring the wallet once again
            rRestore2 <- postWallet ctx payldCrt
            verify
                rRestore2
                [ expectResponseCode HTTP.status201
                ]

            let w'' = getFromResponse Prelude.id rRestore2
            eventually
                "Wallet is voting NoConfidence and deleagting to pool1 after restoration"
                $ do
                    getSrcWallet ctx w''
                        >>= flip
                            verify
                            [ expectField
                                #delegation
                                ( `shouldHaveDelegationHistory`
                                    votingAndDelegating (ApiT pool1) (ApiT voting) []
                                )
                            ]
    it
        "VOTING_02a - After its DRep retires, a wallet with rewards is \
        \refused every self-withdrawal until it delegates its vote again"
        $ \ctx -> runResourceT $ do
            noBabbage ctx "voting supported in Conway onwards"
            drepA <- liftIO $ _registerNewDRep ctx
            (w, _) <- rewardWallet ctx
            voteFor ctx w (keyHashDRep drepA)

            -- The retirement removes the vote delegation from the ledger,
            -- and leaves the wallet's history as it is.
            liftIO $ _retireDRep ctx drepA
            waitForLedgerVote ctx w ApiLedgerVoteNotDelegated
            expectHistoryVoting ctx w (keyHashDRep drepA)

            dest <- emptyWallet ctx
            addr : _ <- fmap (view #id) <$> listAddresses @n ctx dest
            let payments =
                    [json|[{
                        "address": #{addr},
                        "amount": {
                            "quantity": #{minUTxOValue (_mainEra ctx)},
                            "unit": "lovelace"
                        }
                    }]|]
                selfWithdrawal =
                    Json
                        [json|{
                    "payments": #{payments},
                    "withdrawal": "self"
                }|]
                selfWithdrawalSigned =
                    Json
                        [json|{
                    "payments": #{payments},
                    "passphrase": #{fixturePassphrase},
                    "withdrawal": "self"
                }|]
            available <-
                view (#balance . #available) . getResponse
                    <$> getSrcWallet ctx w

            -- The wallet decides from the vote delegation that it read from
            -- the ledger at the last node tip, so the refusals start once a
            -- read includes the retirement.
            eventually "payment-fees refuses the self-withdrawal" $ do
                r <-
                    request @ApiFee
                        ctx
                        (Link.getTransactionFeeOld @'Shelley w)
                        Default
                        selfWithdrawal
                verify r [expectResponseCode HTTP.status403]
                decodeErrorInfo r `shouldBe` WithdrawalNotPossibleWithoutVote
            expectNothingSubmitted ctx w available

            request @(ApiCoinSelection n)
                ctx
                (Link.selectCoins @'Shelley w)
                Default
                selfWithdrawal
                >>= expectRefusedWithoutVote ctx w available

            quitStakePoolUnsigned @n @'Shelley ctx w
                >>= expectRefusedWithoutVote ctx w available

            request @(ApiConstructTransaction n)
                ctx
                (Link.createUnsignedTransaction @'Shelley w)
                Default
                selfWithdrawal
                >>= expectRefusedWithoutVote ctx w available

            request @(ApiTransaction n)
                ctx
                (Link.createTransactionOld @'Shelley w)
                Default
                selfWithdrawalSigned
                >>= expectRefusedWithoutVote ctx w available

            quitStakePool @n ctx (w, fixturePassphrase)
                >>= expectRefusedWithoutVote ctx w available

            request @(ApiWalletMigrationPlan n)
                ctx
                (Link.createMigrationPlan @'Shelley w)
                Default
                (Json [json|{ "addresses": [#{addr}] }|])
                >>= expectRefusedWithoutVote ctx w available

            request @[ApiTransaction n]
                ctx
                (Link.migrateWallet @'Shelley w)
                Default
                ( Json
                    [json|{
                        "passphrase": #{fixturePassphrase},
                        "addresses": [#{addr}]
                    }|]
                )
                >>= expectRefusedWithoutVote ctx w available

            -- A vote delegation to a registered DRep makes the rewards
            -- withdrawable again.
            drepB <- liftIO $ _registerNewDRep ctx
            voteFor ctx w (keyHashDRep drepB)
            eventually "payment-fees accepts the self-withdrawal" $ do
                r <-
                    request @ApiFee
                        ctx
                        (Link.getTransactionFeeOld @'Shelley w)
                        Default
                        selfWithdrawal
                verify r [expectResponseCode HTTP.status202]
            rTx <-
                request @(ApiTransaction n)
                    ctx
                    (Link.createTransactionOld @'Shelley w)
                    Default
                    selfWithdrawalSigned
            verify
                rTx
                [ expectResponseCode HTTP.status202
                , expectField #withdrawals (`shouldSatisfy` (not . null))
                ]
            eventually "the self-withdrawal is in the ledger" $ do
                request @(ApiTransaction n)
                    ctx
                    (Link.getTransaction @'Shelley w (getResponse rTx))
                    Default
                    Empty
                    >>= flip
                        verify
                        [expectField (#status . #getApiT) (`shouldBe` InLedger)]

    it "VOTING_02b - A retired DRep is refused as a vote target" $ \ctx ->
        runResourceT $ do
            noBabbage ctx "voting supported in Conway onwards"
            drepA <- liftIO $ _registerNewDRep ctx
            (w, _) <- rewardWallet ctx
            voteFor ctx w (keyHashDRep drepA)

            liftIO $ _retireDRep ctx drepA
            waitForLedgerVote ctx w ApiLedgerVoteNotDelegated
            available <-
                view (#balance . #available) . getResponse
                    <$> getSrcWallet ctx w

            rJoin <-
                joinDRep @n
                    ctx
                    (SpecificDRep (keyHashDRep drepA))
                    (w, fixturePassphrase)
            verify rJoin [expectResponseCode HTTP.status404]
            decodeErrorInfo rJoin `shouldBe` NotFound
            expectNothingSubmitted ctx w available

            rConstruct <-
                request @(ApiConstructTransaction n)
                    ctx
                    (Link.createUnsignedTransaction @'Shelley w)
                    Default
                    (Json [json|{ "vote": #{ApiT (keyHashDRep drepA)} }|])
            verify rConstruct [expectResponseCode HTTP.status404]
            decodeErrorInfo rConstruct `shouldBe` NotFound
            expectNothingSubmitted ctx w available

            -- A registered DRep and the pre-defined voting options remain
            -- valid targets.
            drepB <- liftIO $ _registerNewDRep ctx
            forM_ [keyHashDRep drepB, Abstain, NoConfidence] $ \drep ->
                request @(ApiConstructTransaction n)
                    ctx
                    (Link.createUnsignedTransaction @'Shelley w)
                    Default
                    (Json [json|{ "vote": #{ApiT drep} }|])
                    >>= flip verify [expectResponseCode HTTP.status202]
            voteFor ctx w Abstain

    it
        "VOTING_02c - A retired DRep that registers again is accepted as \
        \a vote target, and its delegators still cannot withdraw"
        $ \ctx -> runResourceT $ do
            noBabbage ctx "voting supported in Conway onwards"
            drepA <- liftIO $ _registerNewDRep ctx
            (w, _) <- rewardWallet ctx
            voteFor ctx w (keyHashDRep drepA)

            liftIO $ _retireDRep ctx drepA
            liftIO $ _registerDRep ctx drepA
            waitForLedgerVote ctx w ApiLedgerVoteNotDelegated
            expectHistoryVoting ctx w (keyHashDRep drepA)

            -- The registration does not restore the vote delegation that
            -- the retirement removed, and the wallet's history still names
            -- the DRep, which is live again. The wallet decides from the
            -- ledger, so it refuses the self-withdrawal, which the ledger
            -- would refuse, once it has read a node tip that includes the
            -- retirement. The wallet holds rewards, so only the missing
            -- vote delegation can be the reason.
            dest <- emptyWallet ctx
            addr : _ <- fmap (view #id) <$> listAddresses @n ctx dest
            available <-
                view (#balance . #available) . getResponse
                    <$> getSrcWallet ctx w
            eventually "payment-fees refuses the self-withdrawal" $ do
                r <-
                    request @ApiFee
                        ctx
                        (Link.getTransactionFeeOld @'Shelley w)
                        Default
                        ( Json
                            [json|{
                                "payments": [{
                                    "address": #{addr},
                                    "amount": {
                                        "quantity": #{minUTxOValue (_mainEra ctx)},
                                        "unit": "lovelace"
                                    }
                                }],
                                "withdrawal": "self"
                            }|]
                        )
                verify r [expectResponseCode HTTP.status403]
                decodeErrorInfo r `shouldBe` WithdrawalNotPossibleWithoutVote
            expectNothingSubmitted ctx w available

            -- For the same reason a vote to the DRep is not a repeated vote.
            eventually "transactions-construct accepts the vote" $ do
                request @(ApiConstructTransaction n)
                    ctx
                    (Link.createUnsignedTransaction @'Shelley w)
                    Default
                    (Json [json|{ "vote": #{ApiT (keyHashDRep drepA)} }|])
                    >>= flip verify [expectResponseCode HTTP.status202]
            voteFor ctx w (keyHashDRep drepA)

    it "DREPS_01 - listDReps returns 200 with a JSON array in Conway era" $ \ctx ->
        runResourceT $ do
            noBabbage ctx "DRep listing requires Conway era"
            r <- listDReps ctx
            verify
                r
                [ expectResponseCode HTTP.status200
                ]

    it
        "DREPS_02 - listDReps returns 200 with empty array in pre-Conway era"
        $ \ctx ->
            runResourceT $ do
                noConway ctx "Pre-Conway returns empty DRep list"
                r <- listDReps ctx
                verify
                    r
                    [ expectResponseCode HTTP.status200
                    ]
                getFromResponse Prelude.id r `shouldBe` []

    it "DREPS_03 - list responses omit full metadata" $ \ctx ->
        runResourceT $ do
            noBabbage ctx "DRep listing requires Conway era"
            r <- listDReps ctx
            verify r [expectResponseCode HTTP.status200]
            let dreps = getFromResponse Prelude.id r :: [ApiDRepInfo]
            map drepInfoMetadata dreps `shouldSatisfy` all (== Nothing)

    it
        "DREPS_06 - suggestedDReps returns 200 with a JSON array in Conway era"
        $ \ctx ->
            runResourceT $ do
                noBabbage ctx "DRep suggested requires Conway era"
                r <- suggestedDReps ctx Nothing
                verify r [expectResponseCode HTTP.status200]

    it
        "DREPS_07 - suggestedDReps returns 200 with empty array in pre-Conway era"
        $ \ctx ->
            runResourceT $ do
                noConway ctx "Pre-Conway returns empty suggested DRep list"
                r <- suggestedDReps ctx Nothing
                verify r [expectResponseCode HTTP.status200]
                getFromResponse Prelude.id r `shouldBe` ([] :: [ApiDRepInfo])

    it "DREPS_08 - all suggested DReps have active status" $ \ctx ->
        runResourceT $ do
            noBabbage ctx "DRep suggested requires Conway era"
            r <- suggestedDReps ctx Nothing
            verify r [expectResponseCode HTTP.status200]
            let dreps = getFromResponse Prelude.id r :: [ApiDRepInfo]
            -- The suggested endpoint must only return active DReps.
            mapM_ (\info -> drepInfoStatus info `shouldBe` Active) dreps

    it
        "DREPS_09 - count=1 limits suggested DRep results to at most one entry"
        $ \ctx ->
            runResourceT $ do
                noBabbage ctx "DRep suggested requires Conway era"
                r <- suggestedDReps ctx (Just 1)
                verify r [expectResponseCode HTTP.status200]
                let dreps = getFromResponse Prelude.id r :: [ApiDRepInfo]
                length dreps `shouldSatisfy` (<= 1)

    it
        "DREPS_10 - suggested DReps only have key_hash or script_hash credentials"
        $ \ctx ->
            runResourceT $ do
                noBabbage ctx "DRep suggested requires Conway era"
                r <- suggestedDReps ctx Nothing
                verify r [expectResponseCode HTTP.status200]
                let dreps = getFromResponse Prelude.id r :: [ApiDRepInfo]
                mapM_
                    ( \info ->
                        credentialType (drepInfoCredential info)
                            `shouldSatisfy` (`elem` ["key_hash", "script_hash"])
                    )
                    dreps

    it "DREPS_12 - getDRep returns 404 for the always-abstain sentinel" $ \ctx ->
        runResourceT $ do
            noBabbage ctx "DRep querying requires Conway era"
            r <- getDRep ctx (SpecificDRep Abstain)
            verify r [expectResponseCode HTTP.status404]
            decodeErrorInfo r `shouldBe` NotFound

    it
        "DREPS_13 - getDRep returns 404 for the always-no-confidence sentinel"
        $ \ctx ->
            runResourceT $ do
                noBabbage ctx "DRep querying requires Conway era"
                r <- getDRep ctx (SpecificDRep NoConfidence)
                verify r [expectResponseCode HTTP.status404]
                decodeErrorInfo r `shouldBe` NotFound
  where
    stakeKeyDerPath =
        NE.fromList
            [ ApiT (DerivationIndex 2_147_485_500)
            , ApiT (DerivationIndex 2_147_485_463)
            , ApiT (DerivationIndex 2_147_483_648)
            , ApiT (DerivationIndex 2)
            , ApiT (DerivationIndex 0)
            ]

    getSrcWallet ctx wal =
        let endpoint = Link.getWallet @'Shelley wal
        in  request @ApiWallet ctx endpoint Default Empty

    keyHashDRep :: DRepKeyHash -> DRep
    keyHashDRep = FromDRepID . DRepFromKeyHash

    -- The DRep of the latest status in the wallet's delegation history.
    historyVoting :: ApiWalletDelegation -> Maybe (ApiT DRep)
    historyVoting history = case reverse (history ^. #next) of
        latest : _ -> latest ^. #voting
        [] -> history ^. #active . #voting

    expectHistoryVoting ctx wal drep =
        getSrcWallet ctx wal
            >>= flip
                verify
                [ expectField
                    #delegation
                    ((`shouldBe` Just (ApiT drep)) . historyVoting)
                ]

    -- Delegate the wallet's vote and wait until the delegation is in the
    -- ledger and in the wallet's history.
    voteFor ctx wal drep = do
        r <- joinDRep @n ctx (SpecificDRep drep) (wal, fixturePassphrase)
        verify r [expectResponseCode HTTP.status202]
        eventually "the vote delegation is in the ledger" $ do
            request @(ApiTransaction n)
                ctx
                (Link.getTransaction @'Shelley wal (getResponse r))
                Default
                Empty
                >>= flip
                    verify
                    [expectField (#status . #getApiT) (`shouldBe` InLedger)]
        eventually "the wallet's history names the DRep"
            $ expectHistoryVoting ctx wal drep
        waitForLedgerVote ctx wal (ApiLedgerVoteDelegated (ApiT drep))

    -- Wait until the wallet reports the given vote delegation from the
    -- ledger. The wallet reads it when the node tip changes, and decides
    -- from its last read.
    waitForLedgerVote ctx wal expected =
        eventually "the wallet reports the vote delegation of the ledger"
            $ getSrcWallet ctx wal
                >>= flip
                    verify
                    [ expectField
                        (#delegation . #ledgerVote)
                        (`shouldBe` Just expected)
                    ]

    expectNothingSubmitted
        :: MonadUnliftIO m => Context -> ApiWallet -> ApiAmount -> m ()
    expectNothingSubmitted ctx wal available = do
        getSrcWallet ctx wal
            >>= flip
                verify
                [expectField (#balance . #available) (`shouldBe` available)]
        txs <- listTransactions @n ctx wal Nothing Nothing Nothing Nothing
        [tx ^. #id | tx <- txs, tx ^. #status . #getApiT == Pending]
            `shouldBe` []

    expectRefusedWithoutVote
        :: (MonadUnliftIO m, Show a)
        => Context
        -> ApiWallet
        -> ApiAmount
        -> (HTTP.Status, Either RequestException a)
        -> m ()
    expectRefusedWithoutVote ctx wal available r = do
        verify r [expectResponseCode HTTP.status403]
        decodeErrorInfo r `shouldBe` WithdrawalNotPossibleWithoutVote
        expectNothingSubmitted ctx wal available

    quit ctx wal depositAmt = do
        -- quitting, ie. deregistrating the stake key
        let delegationQuit =
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
                (Link.createUnsignedTransaction @'Shelley wal)
                Default
                delegationQuit

        let quittingCert = QuitPool stakeKeyDerPath
        verify
            rTx
            [ expectResponseCode HTTP.status202
            , expectField (#coinSelection . #depositsTaken) (`shouldBe` [])
            , expectField
                (#coinSelection . #depositsReturned)
                (`shouldBe` [depositAmt])
            , expectField
                (#coinSelection . #certificates)
                (`shouldBe` Just (quittingCert NE.:| []))
            ]
        let ApiSerialisedTransaction apiTx _ = getFromResponse #transaction rTx
        signedTx <- signTx ctx wal apiTx [expectResponseCode HTTP.status202]

        let decodePayload = Json (toJSON signedTx)
        rDecodedTx <-
            request @(ApiDecodedTransaction n)
                ctx
                (Link.decodeTransaction @'Shelley wal)
                Default
                decodePayload
        verify
            rDecodedTx
            [ expectResponseCode HTTP.status202
            , expectField
                #certificates
                (`shouldBe` [WalletDelegationCertificate quittingCert])
            , expectField #depositsReturned (`shouldBe` [depositAmt])
            , expectField #depositsTaken (`shouldBe` [])
            ]
        submittedTx <- submitTxWithWid ctx wal signedTx
        verify
            submittedTx
            [ expectSuccess
            , expectResponseCode HTTP.status202
            ]

        let txid = getFromResponse #id submittedTx
        let queryTx = Link.getTransaction @'Shelley wal (ApiTxId txid)
        request @(ApiTransaction n) ctx queryTx Default Empty
            >>= flip
                verify
                [ expectResponseCode HTTP.status200
                , expectField #depositTaken (`shouldBe` ApiAmount 0)
                , expectField #depositReturned (`shouldBe` depositAmt)
                ]

        waitNumberOfEpochBoundaries 2 ctx

        eventually "Wallet is neither delegating nor voting" $ do
            getSrcWallet ctx wal
                >>= flip
                    verify
                    [ expectField
                        #delegation
                        (`shouldHaveDelegationHistory` notDelegating [])
                    ]

    voteAndRevote ctx wal depositAmt = do
        -- First voting
        let voteNoConfidence =
                Json
                    [json|{
               "vote": "no_confidence"
           }|]
        rTx1 <-
            request @(ApiConstructTransaction n)
                ctx
                (Link.createUnsignedTransaction @'Shelley wal)
                Default
                voteNoConfidence

        -- as we are joining for the first time we expect two certificates
        let registerStakeKeyCert =
                RegisterRewardAccount stakeKeyDerPath
        let voting1 = ApiT NoConfidence
        let votingCert1 =
                CastVote stakeKeyDerPath voting1

        verify
            rTx1
            [ expectResponseCode HTTP.status202
            , expectField
                (#coinSelection . #depositsTaken)
                (`shouldBe` [depositAmt])
            , expectField (#coinSelection . #depositsReturned) (`shouldBe` [])
            , expectField
                (#coinSelection . #certificates)
                (`shouldBe` Just (registerStakeKeyCert NE.:| [votingCert1]))
            ]

        let ApiSerialisedTransaction apiTx1 _ = getFromResponse #transaction rTx1
        signedTx1 <-
            signTx ctx wal apiTx1 [expectResponseCode HTTP.status202]

        let decodePayload1 = Json (toJSON signedTx1)
        rDecodedTx1 <-
            request @(ApiDecodedTransaction n)
                ctx
                (Link.decodeTransaction @'Shelley wal)
                Default
                decodePayload1
        verify
            rDecodedTx1
            [ expectResponseCode HTTP.status202
            , expectField
                #certificates
                ( `shouldBe`
                    [ WalletDelegationCertificate registerStakeKeyCert
                    , WalletDelegationCertificate votingCert1
                    ]
                )
            , expectField #depositsTaken (`shouldBe` [depositAmt])
            , expectField #depositsReturned (`shouldBe` [])
            ]

        -- Submit tx
        submittedTx1 <- submitTxWithWid ctx wal signedTx1
        verify
            submittedTx1
            [ expectSuccess
            , expectResponseCode HTTP.status202
            ]

        eventually "Wallet has voted and deposit info persists" $ do
            rJoin' <-
                request @(ApiTransaction n)
                    ctx
                    ( Link.getTransaction @'Shelley
                        wal
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
        let link1 = Link.getTransaction @'Shelley wal (ApiTxId txId1)
        eventually "Voting transaction is in ledger" $ do
            request @(ApiTransaction n) ctx link1 Default Empty
                >>= flip
                    verify
                    [ expectResponseCode HTTP.status200
                    , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
                    , expectField (#status . #getApiT) (`shouldBe` InLedger)
                    , expectField #metadata (`shouldBe` Nothing)
                    ]

        waitNumberOfEpochBoundaries 2 ctx

        eventually "Wallet is voting no confidence" $ do
            getSrcWallet ctx wal
                >>= flip
                    verify
                    [ expectField
                        #delegation
                        (`shouldHaveDelegationHistory` onlyVoting voting1 [])
                    ]

        let voteAbstain =
                Json
                    [json|{
               "vote": "abstain"
           }|]
        rTx2 <-
            request @(ApiConstructTransaction n)
                ctx
                (Link.createUnsignedTransaction @'Shelley wal)
                Default
                voteAbstain

        let voting2 = ApiT Abstain
        let votingCert2 = CastVote stakeKeyDerPath voting2

        verify
            rTx2
            [ expectResponseCode HTTP.status202
            , expectField (#coinSelection . #depositsTaken) (`shouldBe` [])
            , expectField (#coinSelection . #depositsReturned) (`shouldBe` [])
            , expectField
                (#coinSelection . #certificates)
                (`shouldBe` Just (votingCert2 NE.:| []))
            ]

        let ApiSerialisedTransaction apiTx2 _ = getFromResponse #transaction rTx2
        signedTx2 <-
            signTx ctx wal apiTx2 [expectResponseCode HTTP.status202]

        let decodePayload2 = Json (toJSON signedTx2)
        rDecodedTx2 <-
            request @(ApiDecodedTransaction n)
                ctx
                (Link.decodeTransaction @'Shelley wal)
                Default
                decodePayload2
        verify
            rDecodedTx2
            [ expectResponseCode HTTP.status202
            , expectField
                #certificates
                (`shouldBe` [WalletDelegationCertificate votingCert2])
            , expectField #depositsTaken (`shouldBe` [])
            , expectField #depositsReturned (`shouldBe` [])
            ]

        -- Submit tx
        submittedTx2 <- submitTxWithWid ctx wal signedTx2
        verify
            submittedTx2
            [ expectSuccess
            , expectResponseCode HTTP.status202
            ]

        eventually "Wallet has voted again" $ do
            rJoin' <-
                request @(ApiTransaction n)
                    ctx
                    ( Link.getTransaction @'Shelley
                        wal
                        (getResponse submittedTx2)
                    )
                    Default
                    Empty
            verify
                rJoin'
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
                >>= flip
                    verify
                    [ expectResponseCode HTTP.status200
                    , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
                    , expectField (#status . #getApiT) (`shouldBe` InLedger)
                    , expectField #metadata (`shouldBe` Nothing)
                    ]

        waitNumberOfEpochBoundaries 2 ctx

        eventually "Wallet is voting abstain" $ do
            getSrcWallet ctx wal
                >>= flip
                    verify
                    [ expectField
                        #delegation
                        (`shouldHaveDelegationHistory` onlyVoting voting2 [])
                    ]

        return voting2
