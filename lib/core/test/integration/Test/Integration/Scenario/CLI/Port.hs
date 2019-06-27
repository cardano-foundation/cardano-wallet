{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Integration.Scenario.CLI.Port
    ( specNegative
    , specCommon
    , specWithDefaultPort
    , specWithRandomPort
    ) where

import Prelude

import Cardano.CLI
    ( getPort )
import Control.Monad
    ( forM_, void )
import Data.Generics.Internal.VL.Lens
    ( over, (^.) )
import Data.Generics.Product.Typed
    ( HasType, typed )
import Network.Wai.Handler.Warp
    ( Port )
import System.Command
    ( Stderr (..), Stdout (..) )
import System.Exit
    ( ExitCode (..) )
import System.IO
    ( hClose, hFlush, hPutStr )
import System.Process
    ( waitForProcess, withCreateProcess )
import Test.Hspec
    ( SpecWith, describe, it )
import Test.Hspec.Expectations.Lifted
    ( shouldBe, shouldContain, shouldNotBe, shouldNotContain )
import Test.Integration.Framework.DSL
    ( cardanoWalletCLI
    , createWalletViaCLI
    , deleteWalletViaCLI
    , generateMnemonicsViaCLI
    , getWalletViaCLI
    , listAddressesViaCLI
    , listWalletsViaCLI
    , postTransactionViaCLI
    , proc'
    , updateWalletViaCLI
    )

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

specNegative :: SpecWith ()
specNegative =
    describe "PORT_04 - Fail nicely when port is out-of-bounds" $ do
        let tests =
                [ ("serve", "--port", getPort minBound - 14)
                , ("serve", "--port", getPort maxBound + 42)
                , ("serve", "--bridge-port", getPort minBound - 6)
                , ("serve", "--bridge-port", getPort maxBound + 523)
                , ("launch", "--port", getPort minBound - 1)
                , ("launch", "--port", getPort maxBound + 59375)
                , ("launch", "--bridge-port", getPort minBound - 5621)
                , ("launch", "--bridge-port", getPort maxBound + 1)
                ]
        forM_ tests $ \(cmd, opt, port) -> let args = [cmd, opt, show port] in
            it (unwords args) $ do
                (exit, Stdout (_ :: String), Stderr err) <- cardanoWalletCLI args
                exit `shouldBe` ExitFailure 1
                err `shouldContain`
                    (  "expected a TCP port number between "
                    <> show (getPort minBound)
                    <> " and "
                    <> show (getPort maxBound)
                    )

specCommon :: HasType Port s => SpecWith s
specCommon = do
    it "PORT_01 - Can't reach server with wrong port (wallet list)" $ \ctx -> do
        let ctx' = over (typed @Port) (+1) ctx
        (_ :: ExitCode, Stdout (_ :: String), Stderr err) <-
            listWalletsViaCLI ctx'
        err `shouldContain` errConnectionRefused

    it "PORT_01 - Can't reach server with wrong port (wallet create)" $ \ctx -> do
        let ctx' = over (typed @Port) (+1) ctx
        let name = "Wallet created via CLI"
        Stdout mnemonics <- generateMnemonicsViaCLI ["--size", "15"]
        let pwd = "Secure passphrase"
        (_ :: ExitCode, _, err) <-
            createWalletViaCLI ctx' [name] mnemonics "\n" pwd
        T.unpack err `shouldContain` errConnectionRefused

    it "PORT_01 - Can't reach server with wrong port (wallet get)" $ \ctx -> do
        let ctx' = over (typed @Port) (+1) ctx
        let wid = replicate 40 '0'
        (_ :: ExitCode, Stdout (_ :: String), Stderr err) <-
            getWalletViaCLI ctx' wid
        err `shouldContain` errConnectionRefused

    it "PORT_01 - Can't reach server with wrong port (wallet delete)" $ \ctx -> do
        let ctx' = over (typed @Port) (+1) ctx
        let wid = replicate 40 '0'
        (_ :: ExitCode, Stdout (_ :: String), Stderr err) <-
            deleteWalletViaCLI ctx' wid
        err `shouldContain` errConnectionRefused

    it "PORT_01 - Can't reach server with wrong port (wallet update)" $ \ctx -> do
        let ctx' = over (typed @Port) (+1) ctx
        let wid = replicate 40 '0'
        (_ :: ExitCode, Stdout (_ :: String), Stderr err) <-
            updateWalletViaCLI ctx' [wid, "--name", "My Wallet"]
        err `shouldContain` errConnectionRefused

    it "PORT_01 - Can't reach server with wrong port (transction create)" $ \ctx -> do
        let ctx' = over (typed @Port) (+1) ctx
        let addr =
                "37btjrVyb4KFjfnPUjgDKLiATLxgwBbeMAEr4vxgkq4Ea5nR6evtX99x2\
                \QFcF8ApLM4aqCLGvhHQyRJ4JHk4zVKxNeEtTJaPCeB86LndU2YvKUTEEm"
        (_ :: ExitCode, _, err) <-
            postTransactionViaCLI ctx' passphrase
                [ replicate 40 '0'
                , "--payment", "14@" <> addr
                ]
        T.unpack err `shouldContain` errConnectionRefused

    it "PORT_01 - Can't reach server with wrong port (address list)" $ \ctx -> do
        let ctx' = over (typed @Port) (+1) ctx
        let wid = replicate 40 '0'
        (_ :: ExitCode, Stdout (_ :: String), Stderr err) <-
            listAddressesViaCLI ctx' [wid]
        err `shouldContain` errConnectionRefused

specWithDefaultPort
    :: SpecWith s
specWithDefaultPort = do
    it "PORT_02 - Can omit --port when server uses default port (wallet list)" $ \_ -> do
        (_ :: ExitCode, Stdout (_ :: String), Stderr err) <-
            cardanoWalletCLI ["wallet", "list"]
        err `shouldNotContain` errConnectionRefused

    it "PORT_02 - Can omit --port when server uses default port (wallet create)" $ \_ -> do
        Stdout mnemonics <- generateMnemonicsViaCLI ["--size", "15"]
        let pwd = "Secure passphrase"
        let process = proc' "cardano-wallet" ["wallet", "create", "myWallet"]
        withCreateProcess process $ \(Just stdin) (Just _) (Just stderr) h -> do
            hPutStr stdin mnemonics
            hPutStr stdin "\n"
            hPutStr stdin (pwd ++ "\n")
            hPutStr stdin (pwd ++ "\n")
            hFlush stdin
            hClose stdin
            void $ waitForProcess h
            err <- T.unpack <$> TIO.hGetContents stderr
            err `shouldNotContain` errConnectionRefused

    it "PORT_02 - Can omit --port when server uses default port (wallet get)" $ \_ -> do
        let wid = replicate 40 '0'
        (_ :: ExitCode, Stdout (_ :: String), Stderr err) <-
            cardanoWalletCLI ["wallet", "get", wid]
        err `shouldNotContain` errConnectionRefused

    it "PORT_02 - Can omit --port when server uses default port (wallet delete)" $ \_ -> do
        let wid = replicate 40 '0'
        (_ :: ExitCode, Stdout (_ :: String), Stderr err) <-
            cardanoWalletCLI ["wallet", "delete", wid]
        err `shouldNotContain` errConnectionRefused

    it "PORT_02 - Can omit --port when server uses default port (wallet update)" $ \_ -> do
        let wid = replicate 40 '0'
        (_ :: ExitCode, Stdout (_ :: String), Stderr err) <-
            cardanoWalletCLI ["wallet", "update", wid, "--name", "My Wallet"]
        err `shouldNotContain` errConnectionRefused

    it "PORT_02 - Can omit --port when server uses default port (transaction create)" $ \_ -> do
        let addr =
                "37btjrVyb4KFjfnPUjgDKLiATLxgwBbeMAEr4vxgkq4Ea5nR6evtX99x2\
                \QFcF8ApLM4aqCLGvhHQyRJ4JHk4zVKxNeEtTJaPCeB86LndU2YvKUTEEm"
        let args =
                [ "transaction", "create"
                , replicate 40 '0' , "--payment", "14@" <> addr
                ]
        let process = proc' "cardano-wallet" args
        withCreateProcess process $ \(Just stdin) (Just _) (Just stderr) h -> do
            hPutStr stdin (passphrase ++ "\n")
            hFlush stdin
            hClose stdin
            void $ waitForProcess h
            err <- T.unpack <$> TIO.hGetContents stderr
            err `shouldNotContain` errConnectionRefused

    it "PORT_02 - Can omit --port when server uses default port (address list)" $ \_ -> do
        let wid = replicate 40 '0'
        (_ :: ExitCode, Stdout (_ :: String), Stderr err) <-
            cardanoWalletCLI ["address", "list", wid]
        err `shouldNotContain` errConnectionRefused

specWithRandomPort
    :: HasType Port s
    => Port
    -> SpecWith s
specWithRandomPort defaultPort = do
    it "PORT_03 - random port != default port" $ \ctx -> do
        (ctx ^. typed @Port) `shouldNotBe` defaultPort
        return () :: IO () -- Ease the type inference for the line above

    it "PORT_03 - Cannot omit --port when server uses random port (wallet list)" $ \_ -> do
        (_ :: ExitCode, Stdout (_ :: String), Stderr err) <-
            cardanoWalletCLI ["wallet", "list"]
        err `shouldContain` errConnectionRefused

    it "PORT_03 - Cannot omit --port when server uses random port (wallet create)" $ \_ -> do
        Stdout mnemonics <- generateMnemonicsViaCLI ["--size", "15"]
        let pwd = "Secure passphrase"
        let process = proc' "cardano-wallet" ["wallet", "create", "myWallet"]
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
            cardanoWalletCLI ["wallet", "update", wid, "--name", "My Wallet"]
        err `shouldContain` errConnectionRefused

    it "PORT_03 - Cannot omit --port when server uses random port (transaction create)" $ \_ -> do
        let addr =
                "37btjrVyb4KFjfnPUjgDKLiATLxgwBbeMAEr4vxgkq4Ea5nR6evtX99x2\
                \QFcF8ApLM4aqCLGvhHQyRJ4JHk4zVKxNeEtTJaPCeB86LndU2YvKUTEEm"
        let args =
                [ "transaction", "create"
                , replicate 40 '0' , "--payment", "14@" <> addr
                ]
        let process = proc' "cardano-wallet" args
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

{-------------------------------------------------------------------------------
                                  Helpers
-------------------------------------------------------------------------------}

passphrase :: String
passphrase = "cardano-wallet"

errConnectionRefused :: String
errConnectionRefused = "does not exist (Connection refused)"
