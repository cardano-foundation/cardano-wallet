{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.Integration.Scenario.CLI.Port
    ( spec
    ) where

import Prelude

import Cardano.CLI
    ( Port (..), getPort )
import Control.Monad
    ( forM_, void )
import Data.Generics.Internal.VL.Lens
    ( over )
import Data.Generics.Product.Typed
    ( HasType, typed )
import System.Command
    ( Stderr (..), Stdout (..) )
import System.Exit
    ( ExitCode (..) )
import System.IO
    ( hClose, hFlush, hPutStr )
import Test.Hspec
    ( SpecWith, describe )
import Test.Hspec.Expectations.Lifted
    ( shouldBe, shouldContain )
import Test.Hspec.Extra
    ( it )
import Test.Integration.Framework.DSL
    ( cardanoWalletCLI, commandName, createWalletViaCLI, deleteWalletViaCLI,
    generateMnemonicsViaCLI, getWalletViaCLI, listAddressesViaCLI,
    listWalletsViaCLI, postTransactionViaCLI, proc', runResourceT,
    updateWalletNameViaCLI )
import UnliftIO.Process
    ( waitForProcess, withCreateProcess )

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

spec
    :: forall s. (HasType (Port "wallet") s)
    => SpecWith s
spec = describe "COMMON_CLI_PORTS" $ do
    let overPort :: forall sym. HasType (Port sym) s => (Int -> Int) -> s -> s
        overPort fn = over (typed @(Port sym)) (\(Port p) -> Port $ fn p)

    it "PORT_01 - Can't reach server with wrong port (wallet list)" $ \ctx -> do
        let ctx' = overPort @"wallet" (+1) ctx
        (c :: ExitCode, Stdout (_ :: String), Stderr _) <-
            listWalletsViaCLI ctx'
        -- on Windows seems that not whole stderr is available to Stderr
        -- hence asserting only for exit code
        c `shouldBe` ExitFailure 1

    it "PORT_01 - Can't reach server with wrong port (wallet create)" $ \ctx -> runResourceT $ do
        let ctx' = overPort @"wallet" (+1) ctx
        let name = "Wallet created via CLI"
        Stdout mnemonics <- generateMnemonicsViaCLI ["--size", "15"]
        let pwd = "Secure passphrase"
        (c :: ExitCode, _, _) <-
            createWalletViaCLI ctx' [name] mnemonics "\n" pwd
        -- on Windows seems that not whole stderr is available to Stderr
        -- hence asserting only for exit code
        c `shouldBe` ExitFailure 1

    it "PORT_01 - Can't reach server with wrong port (wallet get)" $ \ctx -> do
        let ctx' = overPort @"wallet" (+1) ctx
        let wid = replicate 40 '0'
        (c :: ExitCode, Stdout (_ :: String), Stderr _) <-
            getWalletViaCLI ctx' wid
        -- on Windows seems that not whole stderr is available to Stderr
        -- hence asserting only for exit code
        c `shouldBe` ExitFailure 1

    it "PORT_01 - Can't reach server with wrong port (wallet delete)" $ \ctx -> do
        let ctx' = overPort @"wallet" (+1) ctx
        let wid = replicate 40 '0'
        (c :: ExitCode, Stdout (_ :: String), Stderr _) <-
            deleteWalletViaCLI ctx' wid
        -- on Windows seems that not whole stderr is available to Stderr
        -- hence asserting only for exit code
        c `shouldBe` ExitFailure 1

    it "PORT_01 - Can't reach server with wrong port (wallet update)" $ \ctx -> do
        let ctx' = overPort @"wallet" (+1) ctx
        let wid = replicate 40 '0'
        (c :: ExitCode, Stdout (_ :: String), Stderr _) <-
            updateWalletNameViaCLI ctx' [wid, "My Wallet"]
        -- on Windows seems that not whole stderr is available to Stderr
        -- hence asserting only for exit code
        c `shouldBe` ExitFailure 1

    it "PORT_01 - Can't reach server with wrong port (transction create)" $ \ctx -> do
        let ctx' = overPort @"wallet" (+1) ctx
        let addr =
                "37btjrVyb4KFjfnPUjgDKLiATLxgwBbeMAEr4vxgkq4Ea5nR6evtX99x2\
                \QFcF8ApLM4aqCLGvhHQyRJ4JHk4zVKxNeEtTJaPCeB86LndU2YvKUTEEm"
        (c :: ExitCode, _, _) <-
            postTransactionViaCLI ctx' passphrase
                [ replicate 40 '0'
                , "--payment", "14@" <> addr
                ]
        -- on Windows seems that not whole stderr is available to Stderr
        -- hence asserting only for exit code
        c `shouldBe` ExitFailure 1

    it "PORT_01 - Can't reach server with wrong port (address list)" $ \ctx -> do
        let ctx' = overPort @"wallet" (+1) ctx
        let wid = replicate 40 '0'
        (c :: ExitCode, Stdout (_ :: String), Stderr _) <-
            listAddressesViaCLI ctx' [wid]
        -- on Windows seems that not whole stderr is available to Stderr
        -- hence asserting only for exit code
        c `shouldBe` ExitFailure 1

    it "PORT_03 - Cannot omit --port when server uses random port (wallet list)" $ \_ -> do
        (_ :: ExitCode, Stdout (_ :: String), Stderr err) <-
            cardanoWalletCLI ["wallet", "list"]
        err `shouldContain` errConnectionRefused

    it "PORT_03 - Cannot omit --port when server uses random port (wallet create)" $ \_ -> do
        let args = ["wallet", "create", "from-recovery-phrase", "myWallet"]
        Stdout mnemonics <- generateMnemonicsViaCLI ["--size", "15"]
        let pwd = "Secure passphrase"
        let process = proc' commandName args
        withCreateProcess process $ \(Just stdin) (Just _) (Just stderr) h -> do
            hPutStr stdin mnemonics
            hPutStr stdin "\n"
            hPutStr stdin (pwd ++ "\n")
            hPutStr stdin (pwd ++ "\n")
            hFlush stdin
            hClose stdin
            void $ waitForProcess h
            err <- T.unpack <$> TIO.hGetContents stderr
            err `shouldContain` errConnectionRefused

    it "PORT_03 - Cannot omit --port when server uses random port (wallet get)" $ \_ -> do
        let wid = replicate 40 '0'
        (_ :: ExitCode, Stdout (_ :: String), Stderr err) <-
            cardanoWalletCLI ["wallet", "get", wid]
        err `shouldContain` errConnectionRefused

    it "PORT_03 - Cannot omit --port when server uses random port (wallet delete)" $ \_ -> do
        let wid = replicate 40 '0'
        (_ :: ExitCode, Stdout (_ :: String), Stderr err) <-
            cardanoWalletCLI ["wallet", "delete", wid]
        err `shouldContain` errConnectionRefused

    it "PORT_03 - Cannot omit --port when server uses random port (wallet update)" $ \_ -> do
        let wid = replicate 40 '0'
        (_ :: ExitCode, Stdout (_ :: String), Stderr err) <-
            cardanoWalletCLI ["wallet", "update", "name", wid, "My Wallet"]
        err `shouldContain` errConnectionRefused

    it "PORT_03 - Cannot omit --port when server uses random port (transaction create)" $ \_ -> do
        let addr =
                "37btjrVyb4KFjfnPUjgDKLiATLxgwBbeMAEr4vxgkq4Ea5nR6evtX99x2\
                \QFcF8ApLM4aqCLGvhHQyRJ4JHk4zVKxNeEtTJaPCeB86LndU2YvKUTEEm"
        let args =
                [ "transaction", "create"
                , replicate 40 '0' , "--payment", "14@" <> addr
                ]
        let process = proc' commandName args
        withCreateProcess process $ \(Just stdin) (Just _) (Just stderr) h -> do
            hPutStr stdin (passphrase ++ "\n")
            hFlush stdin
            hClose stdin
            void $ waitForProcess h
            err <- T.unpack <$> TIO.hGetContents stderr
            err `shouldContain` errConnectionRefused

    it "PORT_03 - Cannot omit --port when server uses random port (address list)" $ \_ -> do
        let wid = replicate 40 '0'
        (_ :: ExitCode, Stdout (_ :: String), Stderr err) <-
            cardanoWalletCLI ["address", "list", wid]
        err `shouldContain` errConnectionRefused

    describe "PORT_04 - Fail nicely when port is out-of-bounds" $ do
        let tests =
                [ ("serve", "--port", getPort minBound - 14)
                , ("serve", "--port", getPort maxBound + 42)
                ]
        forM_ tests $ \(cmd, opt, port) -> let args = [cmd, opt, show port] in
            it (unwords args) $ \_ -> do
                (exit, Stdout (_ :: String), Stderr err) <- cardanoWalletCLI args
                exit `shouldBe` ExitFailure 1
                err `shouldContain`
                    (  "expected a TCP port number between "
                    <> show (getPort minBound)
                    <> " and "
                    <> show (getPort maxBound)
                    )

{-------------------------------------------------------------------------------
                                  Helpers
-------------------------------------------------------------------------------}

passphrase :: String
passphrase = "cardano-wallet"

errConnectionRefused :: String
errConnectionRefused = "Connection refused"
