{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Test.Integration.Scenario.CLI.Wallets
    ( spec
    , specPorts
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
    let serverPort = Just "1337"
    describe "WALLETS_CREATE_05 - Can create wallet with different mnemonic sizes" $ do
        forM_ ["15", "18", "21", "24"] $ \(size) -> it size $ \_ -> do
            let walletName = "Wallet created via CLI"
            checkSimpleCreateWallet
                serverPort
                size
                walletName
                (`shouldBe` ExitSuccess)
                (`shouldContain` "Ok.\n")
                (`shouldContain` walletName)

    describe "WALLETS_CREATE_05 - Can't create wallet with wrong size of mnemonic" $ do
        forM_ ["9", "12"] $ \(size) -> it size $ \_ -> do
            let walletName = "Wallet created via CLI"
            Stdout mnemonics <- generateMnemonicsViaCLI ["--size", size]
            Stdout mnemonics2 <- generateMnemonicsViaCLI ["--size", size]
            let passphrase = "Secure passphrase"

            (c, out, err) <-
                createWalletViaCLI [walletName] mnemonics mnemonics2 passphrase serverPort
            c `shouldBe` (ExitFailure 1)
            out `shouldBe` ""
            T.unpack err `shouldContain` "Invalid number of words: 15, 18, 21\
                \ or 24 words are expected."
            Stdout outList <- listWalletsViaCLI
            outList `shouldNotContain` walletName

    describe "WALLETS_CREATE_06 - Can create wallet with different mnemonic snd factor sizes" $ do
        forM_ ["9", "12"] $ \(size) -> it size $ \_ -> do
            let walletName = "Wallet created via CLI"
            Stdout mnemonics <- generateMnemonicsViaCLI []
            Stdout mnemonics2 <- generateMnemonicsViaCLI ["--size", size]
            let passphrase = "Secure passphrase"

            (c, out, err) <-
                createWalletViaCLI [walletName] mnemonics mnemonics2 passphrase serverPort
            c `shouldBe` ExitSuccess
            T.unpack err `shouldContain` "Ok.\n"
            out `shouldContain` walletName

    describe "WALLETS_CREATE_06 - Can't create wallet with wrong size of mnemonic snd factor" $ do
        forM_ ["15", "18", "21", "24"] $ \(size) -> it size $ \_ -> do
            let walletName = "Wallet created via CLI"
            Stdout mnemonics <- generateMnemonicsViaCLI ["--size", size]
            Stdout mnemonics2 <- generateMnemonicsViaCLI ["--size", size]
            let passphrase = "Secure passphrase"

            (c, out, err) <-
                createWalletViaCLI [walletName] mnemonics mnemonics2 passphrase serverPort
            c `shouldBe` (ExitFailure 1)
            out `shouldBe` ""
            T.unpack err `shouldContain` "Invalid number of words: 9 or 12\
                \ words are expected."

            Stdout outList <- listWalletsViaCLI
            outList `shouldNotContain` walletName

    it "WALLETS_CREATE_06 - Cannot create wallet with wrong port" $ \_ -> do
        let walletName = "Wallet created via CLI"
        checkSimpleCreateWallet
            (Just "1338")
            "15"
            walletName
            (`shouldBe` ExitSuccess)
            (`shouldContain` "does not exist (Connection refused))\n")
            (`shouldNotContain` walletName)

    it "WALLETS_CREATE_06 - Cannot create wallet with default port" $ \_ -> do
        let walletName = "Wallet created via CLI"
        checkSimpleCreateWallet
            Nothing
            "15"
            walletName
            (`shouldBe` ExitSuccess)
            (`shouldContain` "does not exist (Connection refused))\n")
            (`shouldNotContain` walletName)

    it "WALLETS_GET_01 - Can get a wallet" $ \ctx -> do
        walId <- emptyWallet' ctx
        (Exit c, Stdout out, Stderr err) <- getWalletViaCLI walId
        err `shouldBe` "Ok.\n"
        _ <- expectValidJSON (Proxy @ApiWallet) out
        out `shouldContain` "Empty Wallet"
        c `shouldBe` ExitSuccess

    describe "WALLETS_GET_03,04 - Cannot get wallets with false ids" $ do
        forM_ falseWalletIds $ \(title, walId) -> it title $ \_ -> do
            (Exit c, Stdout out, Stderr err) <- getWalletViaCLI walId
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
        (Exit c, Stdout out, Stderr err) <- listWalletsViaCLI
        err `shouldBe` "Ok.\n"
        _ <- expectValidJSON (Proxy @[ApiWallet]) out
        out `shouldContain` "Empty Wallet"
        c `shouldBe` ExitSuccess

    it "WALLETS_UPDATE_01 - Can update wallet name" $ \ctx -> do
        walId <- emptyWallet' ctx
        let args = [walId, "--name", "new name"]
        (Exit c, Stdout out, Stderr err) <- updateWalletViaCLI args
        err `shouldBe` "Ok.\n"
        _ <- expectValidJSON (Proxy @ApiWallet) out
        out `shouldContain` "new name"
        c `shouldBe` ExitSuccess

    it "WALLETS_DELETE_01 - Can delete wallet" $ \ctx -> do
        walId <- emptyWallet' ctx
        (Exit c, Stdout out, Stderr err) <- deleteWalletViaCLI walId
        err `shouldBe` "Ok.\n"
        out `shouldNotContain` "CLI Wallet"
        c `shouldBe` ExitSuccess
  where
    emptyWallet' :: Context t -> IO String
    emptyWallet' = fmap (T.unpack . view walletId) . emptyWallet

specPorts :: Int -> SpecWith (Context t)
specPorts defaultPort = do
    it "PORTS_01 - Can create wallet with specifying port" $ \_ -> do
        let walletName = "Wallet created via CLI"
        checkSimpleCreateWallet
            (Just (show defaultPort))
            "15"
            walletName
            (`shouldBe` ExitSuccess)
            (`shouldContain` "Ok.\n")
            (`shouldContain` walletName)
    it "PORTS_02 - Can create wallet with relying on default port without specifying it" $ \_ -> do
        let walletName = "Wallet created via CLI"
        checkSimpleCreateWallet
            Nothing
            "15"
            walletName
            (`shouldBe` ExitSuccess)
            (`shouldContain` "Ok.\n")
            (`shouldContain` walletName)
    it "PORTS_03 - Cannot create wallet with wrong port" $ \_ -> do
        let walletName = "Wallet created via CLI"
        checkSimpleCreateWallet
            (Just (show (defaultPort + 1)))
            "15"
            walletName
            (`shouldBe` ExitSuccess)
            (`shouldContain` "does not exist (Connection refused))\n")
            (`shouldNotContain` walletName)

checkSimpleCreateWallet
    :: Maybe String
    -> String
    -> String
    -> (ExitCode -> IO ())
    -> (String -> IO ())
    -> (String -> IO ())
    -> IO ()
checkSimpleCreateWallet portM mnemonicSize walletName exitCodeCheck errCheck outCheck = do
    Stdout mnemonics <- generateMnemonicsViaCLI ["--size", mnemonicSize]
    let passphrase = "Secure passphrase"
    (c, out, err) <- createWalletViaCLI [walletName] mnemonics "\n" passphrase portM
    exitCodeCheck c
    errCheck (T.unpack err)
    outCheck out
