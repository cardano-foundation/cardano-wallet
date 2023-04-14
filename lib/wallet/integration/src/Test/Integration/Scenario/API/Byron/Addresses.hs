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

module Test.Integration.Scenario.API.Byron.Addresses
    ( spec
    ) where

import Prelude

import Cardano.Mnemonic
    ( Mnemonic )
import Cardano.Wallet.Api.Types
    ( ApiAddressWithPath
    , ApiByronWallet
    , ApiPutAddressesData
    , ApiT (..)
    , WalletStyle (..)
    )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..), PaymentAddress )
import Cardano.Wallet.Primitive.AddressDerivation.Byron
    ( ByronKey )
import Cardano.Wallet.Primitive.AddressDerivation.Icarus
    ( IcarusKey )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( purposeBIP44 )
import Cardano.Wallet.Primitive.Types.Address
    ( AddressState (..) )
import Cardano.Wallet.Read.NetworkId
    ( HasSNetworkId (..), NetworkDiscriminant )
import Cardano.Wallet.Shelley.Compatibility
    ( encodeAddress )
import Control.Monad
    ( forM_ )
import Control.Monad.Trans.Resource
    ( ResourceT, runResourceT )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.Generics.Product.Positions
    ( position )
import Test.Hspec
    ( SpecWith, describe, shouldBe, shouldSatisfy )
import Test.Hspec.Extra
    ( it )
import Test.Integration.Framework.DSL
    ( Context
    , Headers (..)
    , Payload (..)
    , emptyIcarusWallet
    , emptyIcarusWalletMws
    , emptyRandomWallet
    , emptyRandomWalletMws
    , eventually
    , expectErrorMessage
    , expectField
    , expectListField
    , expectListSize
    , expectResponseCode
    , fixtureIcarusWallet
    , fixturePassphrase
    , fixtureRandomWallet
    , getFromResponse
    , icarusAddresses
    , isValidDerivationPath
    , isValidRandomDerivationPath
    , json
    , randomAddresses
    , request
    , verify
    , walletId
    )
import Test.Integration.Framework.TestData
    ( errMsg403CouldntIdentifyAddrAsMine
    , errMsg403NotAByronWallet
    , errMsg403WrongPass
    , errMsg404NoWallet
    )
import Web.HttpApiData
    ( ToHttpApiData (..) )

import qualified Cardano.Wallet.Api.Link as Link
import qualified Network.HTTP.Types.Status as HTTP

spec
    :: forall n
     . ( HasSNetworkId n
       , PaymentAddress n ByronKey 'CredFromKeyK
       , PaymentAddress n IcarusKey 'CredFromKeyK
       )
    => SpecWith Context
spec = do
    describe "BYRON_ADDRESSES" $ do
        scenario_ADDRESS_LIST_01 @n emptyRandomWallet 2
        scenario_ADDRESS_LIST_01 @n emptyIcarusWallet 5

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

        scenario_ADDRESS_IMPORT_01 @n emptyRandomWalletMws
        scenario_ADDRESS_IMPORT_02 @n emptyIcarusWalletMws
        scenario_ADDRESS_IMPORT_03 @n emptyRandomWalletMws
        scenario_ADDRESS_IMPORT_04 @n fixtureRandomWallet
        scenario_ADDRESS_IMPORT_05 @n 15_000 emptyRandomWalletMws
        scenario_ADDRESS_IMPORT_06 @n emptyRandomWalletMws

scenario_ADDRESS_LIST_01
    :: forall (n :: NetworkDiscriminant)
     . HasSNetworkId n
    => (Context -> ResourceT IO ApiByronWallet)
    -> Int
    -> SpecWith Context
scenario_ADDRESS_LIST_01 fixture derPathSize = it title $ \ctx -> runResourceT $ do
    w <- fixture ctx
    r <- request @[ApiAddressWithPath n] ctx (Link.listAddresses @'Byron w) Default Empty
    verify r [ expectResponseCode HTTP.status200 ]
    let n = length $ getFromResponse id r
    forM_ [0..n-1] $ \addrIx -> do
        expectListField addrIx #state (`shouldBe` ApiT Unused) r
        expectListField addrIx #derivationPath
            (`shouldSatisfy` (\d -> do
                                  if derPathSize == 5
                                    then isValidDerivationPath purposeBIP44 d
                                    else isValidRandomDerivationPath d)) r
  where
    title = "ADDRESS_LIST_01 - Can list known addresses on a default wallet"

scenario_ADDRESS_LIST_02
    :: forall (n :: NetworkDiscriminant)
     . HasSNetworkId n
    => (Context -> ResourceT IO ApiByronWallet)
    -> SpecWith Context
scenario_ADDRESS_LIST_02 fixture = it title $ \ctx -> runResourceT $ do
    w <- fixture ctx

    -- filtering ?state=used
    rUsed <- request @[ApiAddressWithPath n] ctx
        (Link.listAddresses' @'Byron w (Just Used)) Default Empty
    verify rUsed
        [ expectResponseCode HTTP.status200
        , expectListSize 10 -- NOTE fixture wallets have 10 faucet UTxOs
        ]
    let nUsed = length $ getFromResponse id rUsed
    forM_ [0..nUsed-1] $ \addrIx -> do
        expectListField addrIx #state (`shouldBe` ApiT Used) rUsed

    -- filtering ?state=unused
    rUnused <- request @[ApiAddressWithPath n] ctx
        (Link.listAddresses' @'Byron w (Just Unused)) Default Empty
    let nUnused = length $ getFromResponse id rUnused
    forM_ [0..nUnused-1] $ \addrIx -> do
        expectListField addrIx #state (`shouldBe` ApiT Unused) rUnused
  where
    title = "ADDRESS_LIST_02 - Can filter used and unused addresses"

scenario_ADDRESS_LIST_04
    :: forall (n :: NetworkDiscriminant)
     . HasSNetworkId n
    => (Context -> ResourceT IO ApiByronWallet)
    -> SpecWith Context
scenario_ADDRESS_LIST_04 fixture = it title $ \ctx -> runResourceT $ do
    w <- fixture ctx
    _ <- request @() ctx (Link.deleteWallet @'Byron w) Default Empty
    r <- request @[ApiAddressWithPath n] ctx (Link.listAddresses @'Byron w) Default Empty
    verify r
        [ expectResponseCode HTTP.status404
        , expectErrorMessage $ errMsg404NoWallet $ w ^. walletId
        ]
  where
    title = "ADDRESS_LIST_04 - Delete wallet"

scenario_ADDRESS_CREATE_01
    :: forall (n :: NetworkDiscriminant). HasSNetworkId n => SpecWith Context
scenario_ADDRESS_CREATE_01 = it title $ \ctx -> runResourceT $ do
    w <- emptyRandomWallet ctx
    let payload = Json [json| { "passphrase": #{fixturePassphrase} }|]
    r <- request @(ApiAddressWithPath n) ctx (Link.postRandomAddress w) Default payload
    verify r
        [ expectResponseCode HTTP.status201
        , expectField #state (`shouldBe` ApiT Unused)
        ]
  where
    title = "ADDRESS_CREATE_01 - Can create a random address without index"

scenario_ADDRESS_CREATE_02
    :: forall (n :: NetworkDiscriminant). HasSNetworkId n => SpecWith Context
scenario_ADDRESS_CREATE_02 = it title $ \ctx -> runResourceT $ do
    w <- emptyIcarusWallet ctx
    let payload = Json [json| { "passphrase": #{fixturePassphrase} }|]
    r <- request @(ApiAddressWithPath n) ctx (Link.postRandomAddress w) Default payload
    verify r
        [ expectResponseCode HTTP.status403
        , expectErrorMessage errMsg403NotAByronWallet
        ]
  where
    title = "ADDRESS_CREATE_02 - Creation is forbidden on Icarus wallets"

scenario_ADDRESS_CREATE_03
    :: forall (n :: NetworkDiscriminant). HasSNetworkId n => SpecWith Context
scenario_ADDRESS_CREATE_03 = it title $ \ctx -> runResourceT $ do
    w <- emptyRandomWallet ctx
    let payload = Json [json| { "passphrase": "Give me all your money." }|]
    r <- request @(ApiAddressWithPath n) ctx (Link.postRandomAddress w) Default payload
    verify r
        [ expectResponseCode HTTP.status403
        , expectErrorMessage errMsg403WrongPass
        ]
  where
    title = "ADDRESS_CREATE_03 - Cannot create a random address with wrong passphrase"

scenario_ADDRESS_CREATE_04
    :: forall (n :: NetworkDiscriminant). HasSNetworkId n => SpecWith Context
scenario_ADDRESS_CREATE_04 = it title $ \ctx -> runResourceT $ do
    w <- emptyRandomWallet ctx

    let payload = Json [json| { "passphrase": #{fixturePassphrase} }|]
    rA <- request @(ApiAddressWithPath n) ctx (Link.postRandomAddress w) Default payload
    verify rA [ expectResponseCode HTTP.status201 ]
    let addr = getFromResponse id rA

    rL <- request @[ApiAddressWithPath n] ctx (Link.listAddresses @'Byron w) Default Empty
    verify rL
        [ expectResponseCode HTTP.status200
        , expectListField 0 id (`shouldBe` addr)
        ]
  where
    title = "ADDRESS_CREATE_04 - Can list address after creating it"

scenario_ADDRESS_CREATE_05
    :: forall (n :: NetworkDiscriminant). HasSNetworkId n => SpecWith Context
scenario_ADDRESS_CREATE_05 = it title $ \ctx -> runResourceT $ do
    w <- emptyRandomWallet ctx
    let payload = Json [json|
            { "passphrase": #{fixturePassphrase}
            , "address_index": 2147483662
            }|]
    r <- request @(ApiAddressWithPath n) ctx (Link.postRandomAddress w) Default payload
    verify r
        [ expectResponseCode HTTP.status201
        , expectField #state (`shouldBe` ApiT Unused)
        ]
  where
    title = "ADDRESS_CREATE_05 - Can create an address and specify the index"

scenario_ADDRESS_CREATE_06
    :: forall (n :: NetworkDiscriminant). HasSNetworkId n => SpecWith Context
scenario_ADDRESS_CREATE_06 = it title $ \ctx -> runResourceT $ do
    w <- emptyRandomWallet ctx
    let payload = Json [json|
            { "passphrase": #{fixturePassphrase}
            , "address_index": 2147483662
            }|]
    r0 <- request @(ApiAddressWithPath n) ctx (Link.postRandomAddress w) Default payload
    verify r0 [ expectResponseCode HTTP.status201 ]
    r1 <- request @(ApiAddressWithPath n) ctx (Link.postRandomAddress w) Default payload
    verify r1
        [ expectResponseCode HTTP.status409
        , expectErrorMessage "I already know of such address."
        ]
  where
    title = "ADDRESS_CREATE_06 - Cannot create an address that already exists"

scenario_ADDRESS_IMPORT_01
    :: forall (n :: NetworkDiscriminant).
        ( HasSNetworkId n
        , PaymentAddress n ByronKey 'CredFromKeyK
        )
    => (Context -> ResourceT IO (ApiByronWallet, Mnemonic 12))
    -> SpecWith Context
scenario_ADDRESS_IMPORT_01 fixture = it title $ \ctx -> runResourceT $ do
    (w, mw) <- fixture ctx

    -- Get an unused address
    let addr = randomAddresses @n mw !! 42
    let (_, base) = Link.postRandomAddress w
    let link = base <> "/" <> encodeAddress (sNetworkId @n) addr
    r0 <- request @() ctx ("PUT", link) Default Empty
    verify r0
        [ expectResponseCode HTTP.status204
        ]

    -- Import it
    r1 <- request @[ApiAddressWithPath n] ctx (Link.listAddresses @'Byron w) Default Empty
    verify r1
        [ expectListField 0 #state (`shouldBe` ApiT Unused)
        , expectListField 0 (#id . position @1) (`shouldBe` ApiT addr)
        ]
  where
    title = "ADDRESS_IMPORT_01 - I can import an address from my wallet"

scenario_ADDRESS_IMPORT_02
    :: forall (n :: NetworkDiscriminant).
        ( HasSNetworkId n
        , PaymentAddress n IcarusKey 'CredFromKeyK
        )
    => (Context -> ResourceT IO (ApiByronWallet, Mnemonic 15))
    -> SpecWith Context
scenario_ADDRESS_IMPORT_02 fixture = it title $ \ctx -> runResourceT $ do
    (w, mw) <- fixture ctx

    let addr = icarusAddresses @n mw !! 42
    let (_, base) = Link.postRandomAddress w
    let link = base <> "/" <> encodeAddress (sNetworkId @n) addr
    r0 <- request @() ctx ("PUT", link) Default Empty
    verify r0
        [ expectResponseCode HTTP.status403
        , expectErrorMessage errMsg403NotAByronWallet
        ]
  where
    title = "ADDRESS_IMPORT_02 - I can't import an address on an Icarus wallet"

scenario_ADDRESS_IMPORT_03
    :: forall (n :: NetworkDiscriminant).
        ( HasSNetworkId n
        , PaymentAddress n ByronKey 'CredFromKeyK
        )
    => (Context -> ResourceT IO (ApiByronWallet, Mnemonic 12))
    -> SpecWith Context
scenario_ADDRESS_IMPORT_03 fixture = it title $ \ctx -> runResourceT $ do
    (w, mw) <- fixture ctx

    -- Get an unused address
    let addr = randomAddresses @n mw !! 42
    let (_, base) = Link.postRandomAddress w
    let link = base <> "/" <> encodeAddress (sNetworkId @n) addr

    -- Insert it twice
    r0 <- request @() ctx ("PUT", link) Default Empty
    verify r0 [ expectResponseCode HTTP.status204 ]
    r1 <- request @() ctx ("PUT", link) Default Empty
    verify r1 [ expectResponseCode HTTP.status204 ]
  where
    title = "ADDRESS_IMPORT_03 - I can import an unused address multiple times"

scenario_ADDRESS_IMPORT_04
    :: forall (n :: NetworkDiscriminant).
        ( HasSNetworkId n
        )
    => (Context -> ResourceT IO ApiByronWallet)
    -> SpecWith Context
scenario_ADDRESS_IMPORT_04 fixture = it title $ \ctx -> runResourceT $ do
    w <- fixture ctx

    -- Get a used address
    r0 <- request @[ApiAddressWithPath n] ctx
        (Link.listAddresses' @'Byron w (Just Used)) Default Empty
    let (addr:_) = getFromResponse id r0

    -- Re-insert it
    let (_, base) = Link.postRandomAddress w
    let link = base <> "/" <> toUrlPiece (addr ^. #id)
    r1 <- request @() ctx ("PUT", link) Default Empty
    verify r1 [ expectResponseCode HTTP.status204 ]

    -- Verify that the address is unchanged
    r2 <- request @[ApiAddressWithPath n] ctx
        (Link.listAddresses' @'Byron w (Just Used)) Default Empty
    verify r2 [ expectListField 0 id (`shouldBe` addr) ]
  where
    title = "ADDRESS_IMPORT_04 - I can import a used address (idempotence)"

scenario_ADDRESS_IMPORT_05
    :: forall (n :: NetworkDiscriminant).
        ( HasSNetworkId n
        , PaymentAddress n ByronKey 'CredFromKeyK
        )
    => Int
    -> (Context -> ResourceT IO (ApiByronWallet, Mnemonic 12))
    -> SpecWith Context
scenario_ADDRESS_IMPORT_05 addrNum fixture = it title $ \ctx -> runResourceT $ do
    (w, mw) <- fixture ctx

    -- Get unused addrNum addresses
    let addrs = map (\num -> encodeAddress (sNetworkId @n) $ randomAddresses @n mw !! num)
                [1 .. addrNum]
    let ep = Link.putRandomAddresses w
    let payload =
            Json [json|
                { addresses: #{addrs}
                }|]

    r0 <- request @(ApiPutAddressesData n) ctx ep Default payload
    verify r0
        [ expectResponseCode HTTP.status204
        ]

    eventually "Addresses are imported" $ do
      r1 <- request @[ApiAddressWithPath n] ctx (Link.listAddresses @'Byron w) Default Empty
      verify r1
          [ expectListSize addrNum
          ]
  where
    title = "ADDRESS_IMPORT_05 - I can import " <> show addrNum <>" of addresses"

scenario_ADDRESS_IMPORT_06
    :: forall (n :: NetworkDiscriminant).
        ( HasSNetworkId n
        , PaymentAddress n ByronKey 'CredFromKeyK
        )
    => (Context -> ResourceT IO (ApiByronWallet, Mnemonic 12))
    -> SpecWith Context
scenario_ADDRESS_IMPORT_06 fixture = it title $ \ctx -> runResourceT $ do
    (w, _)   <- fixture ctx
    (_, mw2) <- fixture ctx

    -- Get an unused address from other wallet
    let addr = randomAddresses @n mw2 !! 42
    let (_, base) = Link.postRandomAddress w
    let link = base <> "/" <> encodeAddress (sNetworkId @n) addr
    r0 <- request @() ctx ("PUT", link) Default Empty
    verify r0
        [ expectResponseCode HTTP.status403
        , expectErrorMessage errMsg403CouldntIdentifyAddrAsMine
        ]
  where
    title = "ADDRESS_IMPORT_06 - posting an unknown address to the wallet returns 403"
