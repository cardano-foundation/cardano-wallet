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

import Cardano.Wallet.Api.Types
    ( ApiConstructTransaction (..)
    , WalletStyle (..)
    )
import Cardano.Wallet.Primitive.NetworkId
    ( HasSNetworkId
    )
import Control.Monad.Trans.Resource
    ( runResourceT
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
    , expectField
    , expectResponseCode
    , fixtureWalletWith
    , json
    , minUTxOValue
    , noBabbage
    , request
    , verify
    )

import qualified Cardano.Wallet.Api.Link as Link
import qualified Network.HTTP.Types.Status as HTTP

spec :: forall n. HasSNetworkId n => SpecWith Context
spec = describe "VOTING_TRANSACTIONS" $ do
    it "VOTING_01a - Can vote and revote" $ \ctx -> runResourceT $ do
        noBabbage ctx "voting supported in Conway onwards"
        let initialAmt = 10 * minUTxOValue (_mainEra ctx)
        src <- fixtureWalletWith @n ctx [initialAmt]

        let voteNoConfidence = Json [json|{
                "vote": "no_confidence"
            }|]
        rTx1 <- request @(ApiConstructTransaction n) ctx
            (Link.createUnsignedTransaction @'Shelley src) Default voteNoConfidence
        verify rTx1
            [ expectResponseCode HTTP.status202
            , expectField (#coinSelection . #depositsReturned) (`shouldBe` [])
            ]
