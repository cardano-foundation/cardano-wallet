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

{-# OPTIONS_GHC -Wno-unused-imports #-} -- temportary, until addRequiredSigners is fixed
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.Integration.Scenario.API.Conway (spec) where

import Prelude

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
    , notDelegating
    , eventually
    , expectField
    , expectResponseCode
    , expectSuccess
    , fixtureWalletWith
    , getFromResponse
    , getResponse
    , json
    , minUTxOValue
    , notDelegating
    , noBabbage
    , request
    , signTx
    , submitTxWithWid
    , verify
    , onlyVoting
    , waitNumberOfEpochBoundaries
    )

import qualified Cardano.Wallet.Api.Link as Link
import qualified Network.HTTP.Types.Status as HTTP
import qualified Data.List.NonEmpty as NE

spec :: forall n. HasSNetworkId n => SpecWith Context
spec = describe "VOTING_TRANSACTIONS" $ do
    it "VOTING_01a - Can vote and revote" $ \ctx -> runResourceT $ do
        noBabbage ctx "voting supported in Conway onwards"
        let initialAmt = 10 * minUTxOValue (_mainEra ctx)
        src <- fixtureWalletWith @n ctx [initialAmt]

        let getSrcWallet =
                let endpoint = Link.getWallet @'Shelley src
                 in request @ApiWallet ctx endpoint Default Empty
        eventually "Wallet is neither voting nor delegating" $ do
            getSrcWallet >>= flip verify
                [ expectField #delegation (`shouldBe` notDelegating [])
                ]

        let depositAmt = ApiAmount 1_000_000
        let voteNoConfidence = Json [json|{
                "vote": "no_confidence"
            }|]
        rTx1 <- request @(ApiConstructTransaction n) ctx
            (Link.createUnsignedTransaction @'Shelley src) Default voteNoConfidence
        verify rTx1
            [ expectResponseCode HTTP.status202
            , expectField (#coinSelection . #depositsTaken) (`shouldBe` [depositAmt])
            , expectField (#coinSelection . #depositsReturned) (`shouldBe` [])
            ]

        let ApiSerialisedTransaction apiTx1 _ = getFromResponse #transaction rTx1
        signedTx1 <- signTx ctx src apiTx1 [ expectResponseCode HTTP.status202 ]

        -- as we are joining for the first time we expect two certificates
        let stakeKeyDerPath = NE.fromList
                [ ApiT (DerivationIndex 2_147_485_500)
                , ApiT (DerivationIndex 2_147_485_463)
                , ApiT (DerivationIndex 2_147_483_648)
                , ApiT (DerivationIndex 2)
                , ApiT (DerivationIndex 0)
                ]
        let registerStakeKeyCert =
                WalletDelegationCertificate $ RegisterRewardAccount stakeKeyDerPath
        let voting1 = ApiT NoConfidence
        let votingCert1 =
                WalletDelegationCertificate $ CastVote stakeKeyDerPath voting1

        let decodePayload1 = Json (toJSON signedTx1)
        rDecodedTx1 <- request @(ApiDecodedTransaction n) ctx
            (Link.decodeTransaction @'Shelley src) Default decodePayload1
        verify rDecodedTx1
            [ expectResponseCode HTTP.status202
            , expectField #certificates (`shouldBe` [registerStakeKeyCert, votingCert1])
            , expectField #depositsTaken (`shouldBe` [depositAmt])
            , expectField #depositsReturned (`shouldBe` [])
            ]

        -- Submit tx
        submittedTx1 <- submitTxWithWid ctx src signedTx1
        verify submittedTx1
            [ expectSuccess
            , expectResponseCode HTTP.status202
            ]

        eventually "Wallet has voted and deposit info persists" $ do
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
        eventually "Voting transaction is in ledger" $ do
            request @(ApiTransaction n) ctx link1 Default Empty
                >>= flip verify
                [ expectResponseCode HTTP.status200
                , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
                , expectField (#status . #getApiT) (`shouldBe` InLedger)
                , expectField #metadata (`shouldBe` Nothing)
                ]

        waitNumberOfEpochBoundaries 2 ctx

        eventually "Wallet is voting" $ do
            getSrcWallet >>= flip verify
                [ expectField #delegation (`shouldBe` onlyVoting voting1 [])
                ]

        let voteAbstain = Json [json|{
                "vote": "abstain"
            }|]
        rTx2 <- request @(ApiConstructTransaction n) ctx
            (Link.createUnsignedTransaction @'Shelley src) Default voteAbstain
        verify rTx2
            [ expectResponseCode HTTP.status202
            , expectField (#coinSelection . #depositsTaken) (`shouldBe` [])
            , expectField (#coinSelection . #depositsReturned) (`shouldBe` [])
            ]

        let ApiSerialisedTransaction apiTx2 _ = getFromResponse #transaction rTx2
        signedTx2 <- signTx ctx src apiTx2 [ expectResponseCode HTTP.status202 ]

        let voting2 = ApiT Abstain
        let votingCert2 =
                WalletDelegationCertificate $ CastVote stakeKeyDerPath voting2

        let decodePayload2 = Json (toJSON signedTx2)
        rDecodedTx2 <- request @(ApiDecodedTransaction n) ctx
            (Link.decodeTransaction @'Shelley src) Default decodePayload2
        verify rDecodedTx2
            [ expectResponseCode HTTP.status202
            , expectField #certificates (`shouldBe` [votingCert2])
            , expectField #depositsTaken (`shouldBe` [])
            , expectField #depositsReturned (`shouldBe` [])
            ]

        -- Submit tx
        submittedTx2 <- submitTxWithWid ctx src signedTx2
        verify submittedTx2
            [ expectSuccess
            , expectResponseCode HTTP.status202
            ]

        eventually "Wallet has voted again" $ do
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
        eventually "Re-voting transaction is in ledger" $ do
            request @(ApiTransaction n) ctx link2 Default Empty
                >>= flip verify
                [ expectResponseCode HTTP.status200
                , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
                , expectField (#status . #getApiT) (`shouldBe` InLedger)
                , expectField #metadata (`shouldBe` Nothing)
                ]
