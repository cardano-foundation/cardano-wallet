{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Integration.Byron.Scenario.API.Addresses
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Api.Types
    ( ApiAddress
    , ApiByronWallet
    , ApiT (..)
    , DecodeAddress
    , EncodeAddress
    , WalletStyle (..)
    )
import Cardano.Wallet.Primitive.AddressDerivation
    ( NetworkDiscriminant )
import Cardano.Wallet.Primitive.Types
    ( AddressState (..) )
import Control.Monad
    ( forM_ )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Test.Hspec
    ( SpecWith, describe, it, shouldBe )
import Test.Integration.Framework.DSL
    ( Context
    , Headers (..)
    , Payload (..)
    , emptyIcarusWallet
    , emptyRandomWallet
    , expectErrorMessage
    , expectField
    , expectListField
    , expectListSize
    , expectResponseCode
    , fixtureIcarusWallet
    , fixtureRandomWallet
    , getFromResponse
    , json
    , request
    , verify
    , walletId
    )
import Test.Integration.Framework.TestData
    ( errMsg403WrongPass, errMsg404NoWallet )

import qualified Cardano.Wallet.Api.Link as Link
import qualified Network.HTTP.Types.Status as HTTP

spec :: forall n t.
    ( DecodeAddress n
    , EncodeAddress n
    ) => SpecWith (Context t)
spec = do
    describe "BYRON_ADDRESSES" $ do
        scenario_ADDRESS_LIST_01 @n emptyRandomWallet
        scenario_ADDRESS_LIST_01 @n emptyIcarusWallet

        scenario_ADDRESS_LIST_02 @n fixtureRandomWallet
        scenario_ADDRESS_LIST_02 @n fixtureIcarusWallet

        scenario_ADDRESS_LIST_04 @n emptyRandomWallet
        scenario_ADDRESS_LIST_04 @n emptyIcarusWallet

        scenario_ADDRESS_CREATE_01 @n
        scenario_ADDRESS_CREATE_02 @n
        scenario_ADDRESS_CREATE_03 @n
        scenario_ADDRESS_CREATE_04 @n
        scenario_ADDRESS_CREATE_05 @n
        scenario_ADDRESS_CREATE_06 @n

scenario_ADDRESS_LIST_01
    :: forall (n :: NetworkDiscriminant) t.
        ( DecodeAddress n
        , EncodeAddress n
        )
    => (Context t -> IO ApiByronWallet)
    -> SpecWith (Context t)
scenario_ADDRESS_LIST_01 fixture = it title $ \ctx -> do
    w <- fixture ctx
    r <- request @[ApiAddress n] ctx (Link.listAddresses @'Byron w) Default Empty
    verify r [ expectResponseCode @IO HTTP.status200 ]
    let n = length $ getFromResponse id r
    forM_ [0..n-1] $ \addrIx -> do
        expectListField addrIx #state (`shouldBe` ApiT Unused) r
  where
    title = "ADDRESS_LIST_01 - Can list known addresses on a default wallet"

scenario_ADDRESS_LIST_02
    :: forall (n :: NetworkDiscriminant) t.
        ( DecodeAddress n
        , EncodeAddress n
        )
    => (Context t -> IO ApiByronWallet)
    -> SpecWith (Context t)
scenario_ADDRESS_LIST_02 fixture = it title $ \ctx -> do
    w <- fixture ctx

    -- filtering ?state=used
    rUsed <- request @[ApiAddress n] ctx
        (Link.listAddresses' @'Byron w (Just Used)) Default Empty
    verify rUsed
        [ expectResponseCode @IO HTTP.status200
        , expectListSize 10 -- NOTE fixture wallets have 10 faucet UTxOs
        ]
    let nUsed = length $ getFromResponse id rUsed
    forM_ [0..nUsed-1] $ \addrIx -> do
        expectListField addrIx #state (`shouldBe` ApiT Used) rUsed

    -- filtering ?state=unused
    rUnused <- request @[ApiAddress n] ctx
        (Link.listAddresses' @'Byron w (Just Unused)) Default Empty
    let nUnused = length $ getFromResponse id rUnused
    forM_ [0..nUnused-1] $ \addrIx -> do
        expectListField addrIx #state (`shouldBe` ApiT Unused) rUnused
  where
    title = "ADDRESS_LIST_02 - Can filter used and unused addresses"

scenario_ADDRESS_LIST_04
    :: forall (n :: NetworkDiscriminant) t.
        ( DecodeAddress n
        , EncodeAddress n
        )
    => (Context t -> IO ApiByronWallet)
    -> SpecWith (Context t)
scenario_ADDRESS_LIST_04 fixture = it title $ \ctx -> do
    w <- fixture ctx
    _ <- request @() ctx (Link.deleteWallet @'Byron w) Default Empty
    r <- request @[ApiAddress n] ctx (Link.listAddresses @'Byron w) Default Empty
    verify r
        [ expectResponseCode @IO HTTP.status404
        , expectErrorMessage $ errMsg404NoWallet $ w ^. walletId
        ]
  where
    title = "ADDRESS_LIST_04 - Delete wallet"

scenario_ADDRESS_CREATE_01
    :: forall (n :: NetworkDiscriminant) t.
        ( DecodeAddress n
        , EncodeAddress n
        )
    => SpecWith (Context t)
scenario_ADDRESS_CREATE_01 = it title $ \ctx -> do
    w <- emptyRandomWallet ctx
    let payload = Json [json| { "passphrase": "Secure Passphrase" }|]
    r <- request @(ApiAddress n) ctx (Link.postRandomAddress w) Default payload
    verify r
        [ expectResponseCode @IO HTTP.status201
        , expectField #state (`shouldBe` ApiT Unused)
        ]
  where
    title = "ADDRESS_CREATE_01 - Can create a random address without index"

scenario_ADDRESS_CREATE_02
    :: forall (n :: NetworkDiscriminant) t.
        ( DecodeAddress n
        , EncodeAddress n
        )
    => SpecWith (Context t)
scenario_ADDRESS_CREATE_02 = it title $ \ctx -> do
    w <- emptyIcarusWallet ctx
    let payload = Json [json| { "passphrase": "Secure Passphrase" }|]
    r <- request @(ApiAddress n) ctx (Link.postRandomAddress w) Default payload
    verify r [ expectResponseCode @IO HTTP.status403 ]
  where
    title = "ADDRESS_CREATE_02 - Creation if forbidden on Icarus wallets"

scenario_ADDRESS_CREATE_03
    :: forall (n :: NetworkDiscriminant) t.
        ( DecodeAddress n
        , EncodeAddress n
        )
    => SpecWith (Context t)
scenario_ADDRESS_CREATE_03 = it title $ \ctx -> do
    w <- emptyRandomWallet ctx
    let payload = Json [json| { "passphrase": "Give me all your money." }|]
    r <- request @(ApiAddress n) ctx (Link.postRandomAddress w) Default payload
    verify r
        [ expectResponseCode @IO HTTP.status403
        , expectErrorMessage errMsg403WrongPass
        ]
  where
    title = "ADDRESS_CREATE_03 - Cannot create a random address with wrong passphrase"

scenario_ADDRESS_CREATE_04
    :: forall (n :: NetworkDiscriminant) t.
        ( DecodeAddress n
        , EncodeAddress n
        )
    => SpecWith (Context t)
scenario_ADDRESS_CREATE_04 = it title $ \ctx -> do
    w <- emptyRandomWallet ctx

    let payload = Json [json| { "passphrase": "Secure Passphrase" }|]
    rA <- request @(ApiAddress n) ctx (Link.postRandomAddress w) Default payload
    verify rA [ expectResponseCode @IO HTTP.status201 ]
    let addr = getFromResponse id rA

    rL <- request @[ApiAddress n] ctx (Link.listAddresses @'Byron w) Default Empty
    verify rL
        [ expectResponseCode @IO HTTP.status200
        , expectListField 0 id (`shouldBe` addr)
        ]
  where
    title = "ADDRESS_CREATE_04 - Can list address after creating it"

scenario_ADDRESS_CREATE_05
    :: forall (n :: NetworkDiscriminant) t.
        ( DecodeAddress n
        , EncodeAddress n
        )
    => SpecWith (Context t)
scenario_ADDRESS_CREATE_05 = it title $ \ctx -> do
    w <- emptyRandomWallet ctx
    let payload = Json [json|
            { "passphrase": "Secure Passphrase"
            , "address_index": 14
            }|]
    r <- request @(ApiAddress n) ctx (Link.postRandomAddress w) Default payload
    verify r
        [ expectResponseCode @IO HTTP.status201
        , expectField #state (`shouldBe` ApiT Unused)
        ]
  where
    title = "ADDRESS_CREATE_05 - Can create an address and specify the index"

scenario_ADDRESS_CREATE_06
    :: forall (n :: NetworkDiscriminant) t.
        ( DecodeAddress n
        , EncodeAddress n
        )
    => SpecWith (Context t)
scenario_ADDRESS_CREATE_06 = it title $ \ctx -> do
    w <- emptyRandomWallet ctx
    let payload = Json [json|
            { "passphrase": "Secure Passphrase"
            , "address_index": 14
            }|]
    r0 <- request @(ApiAddress n) ctx (Link.postRandomAddress w) Default payload
    verify r0 [ expectResponseCode @IO HTTP.status201 ]
    r1 <- request @(ApiAddress n) ctx (Link.postRandomAddress w) Default payload
    verify r1 [ expectResponseCode @IO HTTP.status409 ]
  where
    title = "ADDRESS_CREATE_06 - Cannot create an address that already exists"
