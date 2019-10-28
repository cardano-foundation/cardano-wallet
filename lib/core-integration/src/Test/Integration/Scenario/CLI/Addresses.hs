{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Integration.Scenario.CLI.Addresses
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Api.Types
    ( ApiAddress, getApiT )
import Cardano.Wallet.Primitive.AddressDerivation
    ( NetworkDiscriminant (..) )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( EncodeAddress (..) )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( defaultAddressPoolGap, getAddressPoolGap )
import Cardano.Wallet.Primitive.Types
    ( AddressState (..) )
import Control.Monad
    ( forM_ )
import Data.Generics.Internal.VL.Lens
    ( view, (^.) )
import Data.Proxy
    ( Proxy (..) )
import Data.Text
    ( Text )
import System.Command
    ( Exit (..), Stderr (..), Stdout (..) )
import System.Exit
    ( ExitCode (..) )
import Test.Hspec
    ( SpecWith, describe, it )
import Test.Hspec.Expectations.Lifted
    ( shouldBe, shouldContain )
import Test.Integration.Framework.DSL
    ( Context (..)
    , KnownCommand
    , balanceAvailable
    , deleteWalletViaCLI
    , emptyByronWallet
    , emptyWallet
    , emptyWalletWith
    , expectCliListItemFieldEqual
    , expectEventually'
    , expectValidJSON
    , fixtureWallet
    , getWalletEp
    , listAddressesViaCLI
    , postTransactionViaCLI
    , state
    , walletId
    )
import Test.Integration.Framework.TestData
    ( errMsg404NoWallet, falseWalletIds )

import qualified Data.Text as T

spec
    :: forall t n. (n ~ 'Testnet, KnownCommand t)
    => SpecWith (Context t)
spec = do

    it "ADDRESS_LIST_01 - Can list addresses - default poolGap" $ \ctx -> do
        let g = fromIntegral $ getAddressPoolGap defaultAddressPoolGap
        walId <- emptyWallet' ctx
        (Exit c, Stdout out, Stderr err) <- listAddressesViaCLI @t ctx [walId]
        err `shouldBe` "Ok.\n"
        c `shouldBe` ExitSuccess
        json <- expectValidJSON (Proxy @[ApiAddress n]) out
        length json `shouldBe` g
        forM_ [0..(g-1)] $ \addrNum -> do
            expectCliListItemFieldEqual addrNum state Unused json

    it "ADDRESS_LIST_01 - Can list addresses - non-default poolGap" $ \ctx -> do
        let addrPoolGap = 60
        walId <- emptyWalletWith' ctx
                    ("This is Wallet, OK?", "cardano-wallet", addrPoolGap)
        (Exit c, Stdout out, Stderr err) <- listAddressesViaCLI @t ctx [walId]
        err `shouldBe` "Ok.\n"
        c `shouldBe` ExitSuccess
        json <- expectValidJSON (Proxy @[ApiAddress n]) out
        length json `shouldBe` addrPoolGap
        forM_ [0..59] $ \addrNum -> do
            expectCliListItemFieldEqual addrNum state Unused json

    it "ADDRESS_LIST_02 - Can filter used and unused addresses" $ \ctx -> do
        let g = fromIntegral $ getAddressPoolGap defaultAddressPoolGap
        walId <- fixtureWallet' ctx
        (Exit c1, Stdout o1, Stderr e1)
            <- listAddressesViaCLI @t ctx ["--state", "used", walId]
        e1 `shouldBe` "Ok.\n"
        c1 `shouldBe` ExitSuccess
        j1 <- expectValidJSON (Proxy @[ApiAddress n]) o1
        length j1 `shouldBe` 10
        forM_ [0..9] $ \addrNum -> do
            expectCliListItemFieldEqual addrNum state Used j1
        (Exit c2, Stdout o2, Stderr e2)
            <- listAddressesViaCLI @t ctx ["--state", "unused", walId]
        e2 `shouldBe` "Ok.\n"
        c2 `shouldBe` ExitSuccess
        j2 <- expectValidJSON (Proxy @[ApiAddress n]) o2
        length j2 `shouldBe` g
        forM_ [0..(g-10)] $ \addrNum -> do
            expectCliListItemFieldEqual addrNum state Unused j2

    it "ADDRESS_LIST_02 - Shows nothing when there are no used addresses"
        $ \ctx -> do
        walId <- emptyWallet' ctx
        (Exit c1, Stdout o1, Stderr e1)
            <- listAddressesViaCLI @t ctx ["--state", "used", walId]
        e1 `shouldBe` "Ok.\n"
        c1 `shouldBe` ExitSuccess
        j1 <- expectValidJSON (Proxy @[ApiAddress n]) o1
        length j1 `shouldBe` 0

        (Exit c2, Stdout o2, Stderr e2)
            <- listAddressesViaCLI @t ctx ["--state", "unused", walId]
        e2 `shouldBe` "Ok.\n"
        c2 `shouldBe` ExitSuccess
        j2 <- expectValidJSON (Proxy @[ApiAddress n]) o2
        length j2 `shouldBe` 20
        forM_ [0..19] $ \addrNum -> do
            expectCliListItemFieldEqual addrNum state Unused j2

    describe "ADDRESS_LIST_02 - Invalid filters show error message" $ do
        let filters =
                [ "usedd"
                , "uused"
                , "unusedd"
                , "uunused"
                , "USED"
                , "UNUSED"
                , "-1000"
                , "44444444"
                , "*"
                ]
        forM_ filters $ \fil -> it ("--state=" <> fil) $ \ctx -> do
            walId <- emptyWallet' ctx
            (Exit c, Stdout o, Stderr e)
                <- listAddressesViaCLI @t ctx ["--state", fil, walId]
            let err = "Unable to decode the given value: \"" <> fil <> "\". Please\
                    \ specify one of the following values: used, unused."
            e `shouldContain` err
            c `shouldBe` ExitFailure 1
            o `shouldBe` ""

    it "ADDRESS_LIST_03 - Generates new address pool gap" $ \ctx -> do
        let initPoolGap = 10
        wSrc <- fixtureWallet' ctx
        wDest <- emptyWalletWith ctx ("Wallet", "cardano-wallet", initPoolGap)
        let widDest = T.unpack $ wDest ^. walletId

        -- make sure all addresses in address_pool_gap are 'Unused'
        (Exit c, Stdout o, Stderr e) <- listAddressesViaCLI @t ctx [widDest]
        e `shouldBe` "Ok.\n"
        c `shouldBe` ExitSuccess
        j <- expectValidJSON (Proxy @[ApiAddress n]) o
        length j `shouldBe` initPoolGap
        forM_ [0..initPoolGap - 1] $ \addrNum -> do
            expectCliListItemFieldEqual addrNum state Unused j

        -- run 10 transactions to make all addresses `Used`
        forM_ [0..initPoolGap - 1] $ \addrNum -> do
            let dest = encodeAddress @n (getApiT $ fst $ (j !! addrNum) ^. #id)
            let args = [wSrc, "--payment" , T.unpack $ "1@" <> dest]
            (cTx, _, _) <- postTransactionViaCLI @t ctx "cardano-wallet" args
            cTx `shouldBe` ExitSuccess

        -- make sure all transactions are in ledger
        expectEventually' ctx getWalletEp balanceAvailable 10 wDest

        -- verify new address_pool_gap has been created
        (Exit c1, Stdout o1, Stderr e1) <- listAddressesViaCLI @t ctx [widDest]
        e1 `shouldBe` "Ok.\n"
        c1 `shouldBe` ExitSuccess
        j1 <- expectValidJSON (Proxy @[ApiAddress n]) o1
        length j1 `shouldBe` 2*initPoolGap
        forM_ [0..initPoolGap - 1] $ \addrNum -> do
            expectCliListItemFieldEqual addrNum state Used j1
        forM_ [initPoolGap..2*initPoolGap - 1] $ \addrNum -> do
            expectCliListItemFieldEqual addrNum state Unused j1

    describe "ADDRESS_LIST_04 - False wallet ids" $ do
        forM_ falseWalletIds $ \(title, walId) -> it title $ \ctx -> do
            (Exit c, Stdout o, Stderr e) <- listAddressesViaCLI @t ctx [walId]
            o `shouldBe` ""
            c `shouldBe` ExitFailure 1
            if (title == "40 chars hex") then
                e `shouldContain`
                    errMsg404NoWallet "1111111111111111111111111111111111111111"
            else
                e `shouldContain`
                    "wallet id should be a hex-encoded string of 40 characters"

    it "ADDRESS_LIST_04 - 'almost' valid walletId" $ \ctx -> do
        wid <- emptyWallet' ctx
        (Exit c, Stdout o, Stderr e) <- listAddressesViaCLI @t ctx [wid ++ "0"]
        e `shouldContain`
            "wallet id should be a hex-encoded string of 40 characters"
        o `shouldBe` ""
        c `shouldBe` ExitFailure 1

    it "ADDRESS_LIST_04 - Deleted wallet" $ \ctx -> do
        wid <- emptyWallet' ctx
        Exit d <- deleteWalletViaCLI @t ctx wid
        d `shouldBe` ExitSuccess

        (Exit c, Stdout o, Stderr e) <- listAddressesViaCLI @t ctx [wid]
        e `shouldContain` errMsg404NoWallet (T.pack wid)
        o `shouldBe` ""
        c `shouldBe` ExitFailure 1

    it "BYRON_ADDRESS_LIST - Byron wallet on Shelley CLI" $ \ctx -> do
        wid <- emptyByronWallet' ctx
        (Exit c, Stdout o, Stderr e) <- listAddressesViaCLI @t ctx [wid]
        e `shouldContain` errMsg404NoWallet (T.pack wid)
        o `shouldBe` ""
        c `shouldBe` ExitFailure 1

  where
    emptyByronWallet' :: Context t -> IO String
    emptyByronWallet' = fmap (T.unpack . view walletId) . emptyByronWallet

    emptyWallet' :: Context t -> IO String
    emptyWallet' = fmap (T.unpack . view walletId) . emptyWallet

    emptyWalletWith' :: Context t -> (Text, Text, Int) -> IO String
    emptyWalletWith' ctx (name, pass, pg) =
        fmap (T.unpack . view walletId) (emptyWalletWith ctx (name, pass, pg))

    fixtureWallet' :: Context t -> IO String
    fixtureWallet' = fmap (T.unpack . view walletId) . fixtureWallet
