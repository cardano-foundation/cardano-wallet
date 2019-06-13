{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Test.Integration.Scenario.CLI.Wallets
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Api.Types
    ( ApiWallet )
import Control.Monad
    ( forM_ )
import Data.Functor
    ( ($>) )
import Data.Generics.Internal.VL.Lens
    ( view )
import Data.Proxy
    ( Proxy (..) )
import System.Command
    ( Exit (..), Stderr (..), Stdout (..) )
import System.Exit
    ( ExitCode (..) )
import Test.Hspec
    ( SpecWith, describe, it )
import Test.Hspec.Expectations.Lifted
    ( shouldBe, shouldContain, shouldNotContain )
import Test.Integration.Framework.DSL
    ( Context (..)
    , createWalletViaCLI
    , deleteWalletViaCLI
    , emptyWallet
    , expectValidJSON
    , generateMnemonicsViaCLI
    , getWalletViaCLI
    , listWalletsViaCLI
    , updateWalletViaCLI
    , walletId
    )
import Test.Integration.Framework.TestData
    ( falseWalletIds )

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
            T.unpack err `shouldContain` "Ok.\n"
            out `shouldContain` name

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
            Stdout outList <- listWalletsViaCLI ctx
            outList `shouldNotContain` name

    describe "WALLETS_CREATE_06 - Can create wallet with different mnemonic snd factor sizes" $ do
        forM_ ["9", "12"] $ \(size) -> it size $ \ctx -> do
            let name = "Wallet created via CLI"
            Stdout m1 <- generateMnemonicsViaCLI []
            Stdout m2 <- generateMnemonicsViaCLI ["--size", size]
            let pwd = "Secure passphrase"
            (c, out, err) <- createWalletViaCLI ctx [name] m1 m2 pwd
            c `shouldBe` ExitSuccess
            T.unpack err `shouldContain` "Ok.\n"
            out `shouldContain` name

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
            Stdout outList <- listWalletsViaCLI ctx
            outList `shouldNotContain` name

    it "WALLETS_GET_01 - Can get a wallet" $ \ctx -> do
        walId <- emptyWallet' ctx
        (Exit c, Stdout out, Stderr err) <- getWalletViaCLI ctx walId
        err `shouldBe` "Ok.\n"
        _ <- expectValidJSON (Proxy @ApiWallet) out
        out `shouldContain` "Empty Wallet"
        c `shouldBe` ExitSuccess

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
        emptyWallet' ctx $> () <* emptyWallet' ctx
        (Exit c, Stdout out, Stderr err) <- listWalletsViaCLI ctx
        err `shouldBe` "Ok.\n"
        _ <- expectValidJSON (Proxy @[ApiWallet]) out
        out `shouldContain` "Empty Wallet"
        c `shouldBe` ExitSuccess

    it "WALLETS_UPDATE_01 - Can update wallet name" $ \ctx -> do
        walId <- emptyWallet' ctx
        let args = [walId, "--name", "new name"]
        (Exit c, Stdout out, Stderr err) <- updateWalletViaCLI ctx args
        err `shouldBe` "Ok.\n"
        _ <- expectValidJSON (Proxy @ApiWallet) out
        out `shouldContain` "new name"
        c `shouldBe` ExitSuccess

    it "WALLETS_DELETE_01 - Can delete wallet" $ \ctx -> do
        walId <- emptyWallet' ctx
        (Exit c, Stdout out, Stderr err) <- deleteWalletViaCLI ctx walId
        err `shouldBe` "Ok.\n"
        out `shouldNotContain` "CLI Wallet"
        c `shouldBe` ExitSuccess
  where
    emptyWallet' :: Context t -> IO String
    emptyWallet' = fmap (T.unpack . view walletId) . emptyWallet
