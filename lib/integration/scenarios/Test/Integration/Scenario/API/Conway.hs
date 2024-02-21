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
    , eventually
    , expectField
    , expectResponseCode
    , expectSuccess
    , fixtureWalletWith
    , getFromResponse
    , getResponse
    , json
    , minUTxOValue
    , noBabbage
    , request
    , signTx
    , submitTxWithWid
    , verify
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
        signedTx <- signTx ctx src apiTx1 [ expectResponseCode HTTP.status202 ]

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
        let votingCert =
                WalletDelegationCertificate $ CastVote stakeKeyDerPath (ApiT NoConfidence)

        let decodePayload = Json (toJSON signedTx)
        rDecodedTx <- request @(ApiDecodedTransaction n) ctx
            (Link.decodeTransaction @'Shelley src) Default decodePayload
        verify rDecodedTx
            [ expectResponseCode HTTP.status202
            , expectField #certificates (`shouldBe` [registerStakeKeyCert, votingCert])
            , expectField #depositsTaken (`shouldBe` [depositAmt])
            , expectField #depositsReturned (`shouldBe` [])
            ]

        -- Submit tx
        submittedTx <- submitTxWithWid ctx src signedTx
        verify submittedTx
            [ expectSuccess
            , expectResponseCode HTTP.status202
            ]

        eventually "Wallet has voted and deposit info persists" $ do
            rJoin' <- request @(ApiTransaction n) ctx
                (Link.getTransaction @'Shelley src
                    (getResponse submittedTx))
                Default Empty
            verify rJoin'
                [ expectResponseCode HTTP.status200
                , expectField (#status . #getApiT) (`shouldBe` InLedger)
                , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
                , expectField #depositTaken (`shouldBe` depositAmt)
                , expectField #depositReturned (`shouldBe` ApiAmount 0)
                ]

        let txId = getFromResponse #id submittedTx
        let link = Link.getTransaction @'Shelley src (ApiTxId txId)
        eventually "Voting transaction is in ledger" $ do
            request @(ApiTransaction n) ctx link Default Empty
                >>= flip verify
                [ expectResponseCode HTTP.status200
                , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
                , expectField (#status . #getApiT) (`shouldBe` InLedger)
                , expectField #metadata (`shouldBe` Nothing)
                ]
