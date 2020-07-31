{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Integration.Scenario.CLI.Byron.Addresses
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Api.Types
    ( ApiAddress (..), ApiAddressWithState, ApiByronWallet, ApiT (..) )
import Cardano.Wallet.Primitive.AddressDerivation
    ( NetworkDiscriminant )
import Cardano.Wallet.Primitive.Types
    ( AddressState (..) )
import Control.Monad
    ( forM_ )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.Proxy
    ( Proxy (..) )
import System.Command
    ( Exit (..), Stderr (..), Stdout (..) )
import System.Exit
    ( ExitCode (..) )
import Test.Hspec
    ( SpecWith, describe, it, shouldBe, shouldContain )
import Test.Integration.Framework.DSL
    ( Context
    , KnownCommand
    , createAddressViaCLI
    , deleteWalletViaCLI
    , emptyIcarusWallet
    , emptyIcarusWalletMws
    , emptyRandomWallet
    , emptyRandomWalletMws
    , expectCliField
    , expectCliListField
    , expectValidJSON
    , fixtureIcarusWallet
    , fixturePassphrase
    , fixtureRandomWallet
    , icarusAddresses
    , importAddressViaCLI
    , listAddressesViaCLI
    , randomAddresses
    , verify
    , walletId
    )
import Test.Integration.Framework.TestData
    ( cmdOk, errMsg403NotAByronWallet, errMsg403WrongPass, errMsg404NoWallet )

import qualified Data.Text as T

spec
    :: forall t. KnownCommand t
    => NetworkDiscriminant
    -> SpecWith (Context t)
spec n = do
    describe "BYRON_CLI_ADDRESSES" $ do
        scenario_ADDRESS_LIST_01  "random" emptyRandomWallet
        scenario_ADDRESS_LIST_01  "icarus" emptyIcarusWallet

        scenario_ADDRESS_LIST_02  "random" fixtureRandomWallet
        scenario_ADDRESS_LIST_02  "icarus" fixtureIcarusWallet

        scenario_ADDRESS_LIST_04  "random" emptyRandomWallet
        scenario_ADDRESS_LIST_04  "icarus" emptyIcarusWallet

        scenario_ADDRESS_CREATE_01
        scenario_ADDRESS_CREATE_02
        scenario_ADDRESS_CREATE_03
        scenario_ADDRESS_CREATE_04
        scenario_ADDRESS_CREATE_05
        scenario_ADDRESS_CREATE_06

        scenario_ADDRESS_IMPORT_01 n
        scenario_ADDRESS_IMPORT_02 n
        scenario_ADDRESS_IMPORT_03

        describe "CLI_ADDRESS_CREATE_07 - False indexes" $ do
            let outOfBoundIndexes =
                    [ "-1", "0", "2147483647", "4294967296"
                    , "-2147483648", "-4294967295"]
            let expectedMsgOutOfBound =
                    "Couldn't parse derivation index. Expected an integer\
                    \ between Index {getIndex = 2147483648}\
                    \ and Index {getIndex = 4294967295}"
            forM_ outOfBoundIndexes $ \idx ->
                scenario_ADDRESS_CREATE_07  idx expectedMsgOutOfBound

            let invalidIndexes = [ "patate", "1500sto900", "2147483648e"]
            let expectedMsginvalidIdx =
                    "Int is an integer number between\
                    \ -9223372036854775808 and 9223372036854775807."
            forM_ invalidIndexes $ \idx ->
                scenario_ADDRESS_CREATE_07  idx expectedMsginvalidIdx

scenario_ADDRESS_LIST_01
    :: forall  t.  KnownCommand t
    => String
    -> (Context t -> IO ApiByronWallet)
    -> SpecWith (Context t)
scenario_ADDRESS_LIST_01 walType fixture = it title $ \ctx -> do
    w <- fixture ctx
    let wid = T.unpack (w ^. walletId)
    (Exit c, Stdout out, Stderr err) <- listAddressesViaCLI @t ctx [wid]
    err `shouldBe` cmdOk
    c `shouldBe` ExitSuccess
    j <- expectValidJSON (Proxy @[ApiAddressWithState]) out
    let n = length j
    forM_ [0..(n-1)] $ \addrNum -> do
        expectCliListField
            addrNum (#state . #getApiT) (`shouldBe` Unused) j
  where
    title = "CLI_ADDRESS_LIST_01 - "
        ++ walType ++ " can list known addresses on a default wallet"

scenario_ADDRESS_LIST_02
    :: forall t. KnownCommand t
    => String
    -> (Context t -> IO ApiByronWallet)
    -> SpecWith (Context t)
scenario_ADDRESS_LIST_02 walType fixture = it title $ \ctx -> do
    w <- fixture ctx
    let wid = T.unpack (w ^. walletId)
    let args u = [ wid
                 , "--state", u
                 ]
    -- filtering --state=used
    (Exit c, Stdout out, Stderr err) <- listAddressesViaCLI @t ctx (args "used")
    err `shouldBe` cmdOk
    c `shouldBe` ExitSuccess
    j <- expectValidJSON (Proxy @[ApiAddressWithState]) out
    let n = length j
    forM_ [0..(n-1)] $ \addrNum -> do
        expectCliListField
            addrNum (#state . #getApiT) (`shouldBe` Used) j

    -- filtering --state unused
    (Exit c2, Stdout out2, Stderr err2) <- listAddressesViaCLI @t ctx (args "unused")
    err2 `shouldBe` cmdOk
    c2 `shouldBe` ExitSuccess
    j2 <- expectValidJSON (Proxy @[ApiAddressWithState]) out2
    let n2 = length j2
    forM_ [0..(n2-1)] $ \addrNum -> do
        expectCliListField
            addrNum (#state . #getApiT) (`shouldBe` Unused) j2
  where
    title = "CLI_ADDRESS_LIST_02 - "
        ++ walType ++ " can filter used and unused addresses"

scenario_ADDRESS_LIST_04
    :: forall t. KnownCommand t
    => String
    -> (Context t -> IO ApiByronWallet)
    -> SpecWith (Context t)
scenario_ADDRESS_LIST_04 walType fixture = it title $ \ctx -> do
    w <- fixture ctx
    let wid = w ^. walletId
    Exit cd <- deleteWalletViaCLI @t ctx $ T.unpack wid
    cd `shouldBe` ExitSuccess
    (Exit c, Stdout out, Stderr err) <- listAddressesViaCLI @t ctx [T.unpack wid]
    err `shouldContain` (errMsg404NoWallet wid)
    c `shouldBe` ExitFailure 1
    out `shouldBe` mempty
  where
    title = "CLI_ADDRESS_LIST_04 - " ++ walType ++ " deleted wallet"

scenario_ADDRESS_CREATE_01
    :: forall t.
        ( KnownCommand t
        )
    => SpecWith (Context t)
scenario_ADDRESS_CREATE_01 = it title $ \ctx -> do
    w <- emptyRandomWallet ctx
    let wid = T.unpack (w ^. walletId)
    (c, out, err) <- createAddressViaCLI @t ctx [wid] (T.unpack fixturePassphrase)
    T.unpack err `shouldContain` cmdOk
    c `shouldBe` ExitSuccess
    j <- expectValidJSON (Proxy @(ApiAddressWithState)) (T.unpack out)
    verify j [ expectCliField #state (`shouldBe` ApiT Unused) ]
  where
    title = "CLI_ADDRESS_CREATE_01 - Can create a random address without index"

scenario_ADDRESS_CREATE_02
    :: forall t. KnownCommand t
    => SpecWith (Context t)
scenario_ADDRESS_CREATE_02 = it title $ \ctx -> do
    w <- emptyIcarusWallet ctx
    let wid = T.unpack (w ^. walletId)
    (c, out, err) <- createAddressViaCLI @t ctx [wid] (T.unpack fixturePassphrase)
    T.unpack err `shouldContain` errMsg403NotAByronWallet
    c `shouldBe` ExitFailure 1
    out `shouldBe` mempty
  where
    title = "CLI_ADDRESS_CREATE_02 - Creation is forbidden on Icarus wallets"

scenario_ADDRESS_CREATE_03
    :: forall t. KnownCommand t
    => SpecWith (Context t)
scenario_ADDRESS_CREATE_03 = it title $ \ctx -> do
    w <- emptyRandomWallet ctx
    let wid = T.unpack (w ^. walletId)
    (c, out, err) <- createAddressViaCLI @t ctx [wid] "Give me all your money."
    T.unpack err `shouldContain` errMsg403WrongPass
    c `shouldBe` ExitFailure 1
    out `shouldBe` mempty
  where
    title = "ADDRESS_CREATE_03 - Cannot create a random address with wrong passphrase"

scenario_ADDRESS_CREATE_04
    :: forall t. KnownCommand t
    => SpecWith (Context t)
scenario_ADDRESS_CREATE_04 = it title $ \ctx -> do
    w <- emptyRandomWallet ctx
    let wid = T.unpack (w ^. walletId)
    (c, out, err) <- createAddressViaCLI @t ctx [wid] (T.unpack fixturePassphrase)
    T.unpack err `shouldContain` cmdOk
    c `shouldBe` ExitSuccess
    addr <- expectValidJSON (Proxy @(ApiAddressWithState)) (T.unpack out)

    (Exit cl, Stdout outl, Stderr errl) <- listAddressesViaCLI @t ctx [wid]
    errl `shouldBe` cmdOk
    cl `shouldBe` ExitSuccess
    j <- expectValidJSON (Proxy @[ApiAddressWithState]) outl
    expectCliListField 0 id (`shouldBe` addr) j
  where
    title = "CLI_ADDRESS_CREATE_04 - Can list address after creating it"

scenario_ADDRESS_CREATE_05
    :: forall t. KnownCommand t
    => SpecWith (Context t)
scenario_ADDRESS_CREATE_05 = it title $ \ctx -> do
    w <- emptyRandomWallet ctx
    let wid = T.unpack (w ^. walletId)
    let args = [ wid, "--address-index", "2147483662" ]
    (c, out, err) <- createAddressViaCLI @t ctx args (T.unpack fixturePassphrase)
    T.unpack err `shouldContain` cmdOk
    c `shouldBe` ExitSuccess
    j <- expectValidJSON (Proxy @(ApiAddressWithState)) (T.unpack out)
    verify j [ expectCliField #state (`shouldBe` ApiT Unused) ]
  where
    title = "CLI_ADDRESS_CREATE_05 - Can create an address and specify the index"

scenario_ADDRESS_CREATE_06
    :: forall t. KnownCommand t
    => SpecWith (Context t)
scenario_ADDRESS_CREATE_06 = it title $ \ctx -> do
    w <- emptyRandomWallet ctx
    let wid = T.unpack (w ^. walletId)
    let args = [ wid, "--address-index", "2147483662" ]
    let createTheSameAddr = createAddressViaCLI @t ctx args (T.unpack fixturePassphrase)
    (c, _, _) <- createTheSameAddr
    c `shouldBe` ExitSuccess

    (c2, out2, err2) <- createTheSameAddr
    T.unpack err2 `shouldContain` "I already know of such address."
    c2 `shouldBe` ExitFailure 1
    out2 `shouldBe` mempty
  where
    title = "CLI_ADDRESS_CREATE_06 - Cannot create an address that already exists"

scenario_ADDRESS_CREATE_07
    :: forall t. KnownCommand t
    => String
    -> String
    -> SpecWith (Context t)
scenario_ADDRESS_CREATE_07 index expectedMsg = it index $ \ctx -> do
    w <- emptyRandomWallet ctx
    let wid = T.unpack (w ^. walletId)
    let args = [ wid, "--address-index", index ]
    (c, out, err) <- createAddressViaCLI @t ctx args (T.unpack fixturePassphrase)
    T.unpack err `shouldContain` expectedMsg
    c `shouldBe` ExitFailure 1
    out `shouldBe` mempty

scenario_ADDRESS_IMPORT_01
    :: forall t. KnownCommand t
    => NetworkDiscriminant
    -> SpecWith (Context t)
scenario_ADDRESS_IMPORT_01 n = it title $ \ctx -> do
    (w, mw) <- emptyRandomWalletMws ctx
    let wid = T.unpack (w ^. walletId)
    let addr = T.unpack $ apiAddress $ randomAddresses n mw !! 42
    (Exit c, Stdout _out, Stderr err) <- importAddressViaCLI @t ctx [wid, addr]
    c `shouldBe` ExitSuccess
    err `shouldContain` cmdOk
  where
    title = "CLI_ADDRESS_IMPORT_01 - I can import an address from my wallet"

scenario_ADDRESS_IMPORT_02
    :: forall t . KnownCommand t
    => NetworkDiscriminant
    -> SpecWith (Context t)
scenario_ADDRESS_IMPORT_02 n = it title $ \ctx -> do
    (w, mw) <- emptyIcarusWalletMws ctx
    let wid = T.unpack (w ^. walletId)
    let addr = T.unpack $ apiAddress $ icarusAddresses n mw !! 42
    (Exit c, Stdout _out, Stderr err) <- importAddressViaCLI @t ctx [wid, addr]
    c `shouldBe` ExitFailure 1
    err `shouldContain` errMsg403NotAByronWallet
  where
    title = "CLI_ADDRESS_IMPORT_02 - I can't import an address on an Icarus wallets"

scenario_ADDRESS_IMPORT_03
    :: forall t. KnownCommand t
    => SpecWith (Context t)
scenario_ADDRESS_IMPORT_03 = it title $ \ctx -> do
    w <- emptyRandomWallet ctx
    let wid = T.unpack (w ^. walletId)
    let addr = "💩"
    (Exit c, Stdout _out, Stderr err) <- importAddressViaCLI @t ctx [wid, addr]
    c `shouldBe` ExitFailure 1
    err `shouldBe` "Unable to decode Address: not a valid Base58 encoded string.\n"
  where
    title = "CLI_ADDRESS_IMPORT_03 - I can't import a gibberish address"
