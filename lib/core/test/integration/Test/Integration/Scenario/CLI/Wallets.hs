{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Test.Integration.Scenario.CLI.Wallets
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Api.Types
    ( ApiWallet )
import Cardano.Wallet.Primitive.Types
    ( WalletDelegation (..) )
import Control.Monad
    ( forM_ )
import Data.Generics.Internal.VL.Lens
    ( view )
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
    , addressPoolGap
    , balanceAvailable
    , balanceTotal
    , createWalletViaCLI
    , delegation
    , deleteWalletViaCLI
    , emptyWallet
    , emptyWalletWith
    , expectCliFieldEqual
    , expectCliListItemFieldEqual
    , expectValidJSON
    , generateMnemonicsViaCLI
    , getWalletViaCLI
    , listWalletsViaCLI
    , updateWalletViaCLI
    , verify
    , walletId
    , walletName
    )
import Test.Integration.Framework.TestData
    ( cmdOk, falseWalletIds )

import qualified Data.Text as T

spec :: SpecWith (Context t)
spec = do
    describe "WALLETS_CREATE_05 - Can create wallet with different mnemonic sizes" $ do
        forM_ ["15", "18", "21", "24"] $ \(size) -> it size $ \ctx -> do
            let name = "Wallet created via CLI"
            Stdout mnemonics <- generateMnemonicsViaCLI ["--size", size]
            let pwd = "Secure passphrase"
            (c, out, err) <-
                createWalletViaCLI ctx [name] mnemonics "\n" pwd
            c `shouldBe` ExitSuccess
            T.unpack err `shouldContain` cmdOk
            j <- expectValidJSON (Proxy @ApiWallet) out
            expectCliFieldEqual walletName (T.pack name) j

    describe "WALLETS_CREATE_05 - Can't create wallet with wrong size of mnemonic" $ do
        forM_ ["9", "12"] $ \(size) -> it size $ \ctx -> do
            let name = "Wallet created via CLI"
            Stdout m1 <- generateMnemonicsViaCLI ["--size", size]
            Stdout m2 <- generateMnemonicsViaCLI ["--size", size]
            let pwd = "Secure passphrase"
            (c, out, err) <-
                createWalletViaCLI ctx [name] m1 m2 pwd
            c `shouldBe` (ExitFailure 1)
            out `shouldBe` ""
            T.unpack err `shouldContain` "Invalid number of words: 15, 18, 21\
                \ or 24 words are expected."
            (Stdout o, Stderr e) <- listWalletsViaCLI ctx
            o `shouldBe` "[]\n"
            e `shouldBe` cmdOk

    describe "WALLETS_CREATE_06 - Can create wallet with different mnemonic snd factor sizes" $ do
        forM_ ["9", "12"] $ \(size) -> it size $ \ctx -> do
            let name = "Wallet created via CLI"
            Stdout m1 <- generateMnemonicsViaCLI []
            Stdout m2 <- generateMnemonicsViaCLI ["--size", size]
            let pwd = "Secure passphrase"
            (c, out, err) <- createWalletViaCLI ctx [name] m1 m2 pwd
            c `shouldBe` ExitSuccess
            T.unpack err `shouldContain` cmdOk
            j <- expectValidJSON (Proxy @ApiWallet) out
            expectCliFieldEqual walletName (T.pack name) j

    describe "WALLETS_CREATE_06 - Can't create wallet with wrong size of mnemonic snd factor" $ do
        forM_ ["15", "18", "21", "24"] $ \(size) -> it size $ \ctx -> do
            let name = "Wallet created via CLI"
            Stdout m1 <- generateMnemonicsViaCLI ["--size", size]
            Stdout m2 <- generateMnemonicsViaCLI ["--size", size]
            let pwd = "Secure passphrase"
            (c, out, err) <- createWalletViaCLI ctx [name] m1 m2 pwd
            c `shouldBe` (ExitFailure 1)
            out `shouldBe` ""
            T.unpack err `shouldContain` "Invalid number of words: 9 or 12\
                \ words are expected."
            (Stdout o, Stderr e) <- listWalletsViaCLI ctx
            o `shouldBe` "[]\n"
            e `shouldBe` cmdOk

    it "WALLETS_GET_01 - Can get a wallet" $ \ctx -> do
        walId <- emptyWallet' ctx
        (Exit c, Stdout out, Stderr err) <- getWalletViaCLI ctx walId
        c `shouldBe` ExitSuccess
        err `shouldBe` cmdOk
        j <- expectValidJSON (Proxy @ApiWallet) out
        expectCliFieldEqual walletName "Empty Wallet" j

    describe "WALLETS_GET_03,04 - Cannot get wallets with false ids" $ do
        forM_ falseWalletIds $ \(title, walId) -> it title $ \ctx -> do
            (Exit c, Stdout out, Stderr err) <- getWalletViaCLI ctx walId
            out `shouldBe` ""
            c `shouldBe` ExitFailure 1
            if (title == "40 chars hex") then
                err `shouldBe` "I couldn't find a wallet with the given id:\
                    \ 1111111111111111111111111111111111111111\n"
            else
                err `shouldBe` "wallet id should be an hex-encoded string of\
                    \ 40 characters\n"

    it "WALLETS_LIST_01 - Can list wallets" $ \ctx -> do
        let name = "Wallet to be listed"
        w1 <- emptyWalletWith' ctx (name, "secure-passphrase", 21)
        _ <- emptyWallet' ctx
        (Exit c, Stdout out, Stderr err) <- listWalletsViaCLI ctx
        c `shouldBe` ExitSuccess
        err `shouldBe` cmdOk
        j <- expectValidJSON (Proxy @[ApiWallet]) out
        length j `shouldBe` 2
        verify j
            [ expectCliListItemFieldEqual 0 walletName name
            , expectCliListItemFieldEqual 0 addressPoolGap 21
            , expectCliListItemFieldEqual 0 balanceAvailable 0
            , expectCliListItemFieldEqual 0 balanceTotal 0
            , expectCliListItemFieldEqual 0 delegation (NotDelegating)
            , expectCliListItemFieldEqual 0 walletId (T.pack w1)
            ]

    it "WALLETS_UPDATE_01 - Can update wallet name" $ \ctx -> do
        walId <- emptyWallet' ctx
        let args = [walId, "--name", "new name"]
        (Exit c, Stdout out, Stderr err) <- updateWalletViaCLI ctx args
        c `shouldBe` ExitSuccess
        err `shouldBe` cmdOk
        j <- expectValidJSON (Proxy @ApiWallet) out
        expectCliFieldEqual walletName "new name" j

    it "WALLETS_DELETE_01 - Can delete wallet" $ \ctx -> do
        walId <- emptyWallet' ctx
        (Exit c, Stdout out, Stderr err) <- deleteWalletViaCLI ctx walId
        err `shouldBe` cmdOk
        c `shouldBe` ExitSuccess
        out `shouldBe` "\n"
        (Stdout o, Stderr e) <- listWalletsViaCLI ctx
        o `shouldBe` "[]\n"
        e `shouldBe` cmdOk

emptyWallet' :: Context t -> IO String
emptyWallet' = fmap (T.unpack . view walletId) . emptyWallet

emptyWalletWith' :: Context t -> (Text, Text, Int) -> IO String
emptyWalletWith' ctx (name, pass, pg) =
    fmap (T.unpack . view walletId) (emptyWalletWith ctx (name, pass, pg))
