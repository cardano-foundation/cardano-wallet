{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.LauncherSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Api.Types
    ( ApiWallet )
import Cardano.Wallet.Primitive.Types
    ( WalletState (..) )
import Control.Exception
    ( finally )
import Control.Monad
    ( forM_ )
import Data.Proxy
    ( Proxy (..) )
import Data.Text.Class
    ( toText )
import Network.HTTP.Client
    ( defaultManagerSettings, newManager )
import System.Command
    ( Stdout (..) )
import System.Directory
    ( removeDirectory )
import System.Exit
    ( ExitCode (..) )
import System.IO.Temp
    ( withSystemTempDirectory )
import System.Process
    ( createProcess
    , proc
    , terminateProcess
    , waitForProcess
    , withCreateProcess
    )
import Test.Hspec
    ( Spec, describe, it )
import Test.Hspec.Expectations.Lifted
    ( shouldBe, shouldNotContain, shouldReturn )
import Test.Integration.Framework.DSL
    ( KnownCommand
    , createWalletViaCLI
    , expectEventually'
    , expectPathEventuallyExist
    , expectProcStdOutHas
    , expectValidJSON
    , generateMnemonicsViaCLI
    , getProcStream
    , proc'
    , state
    , waitForServer
    )
import Test.Integration.Framework.TestData
    ( versionLine )

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

spec :: forall t. (KnownCommand t) => Spec
spec = do
    describe "LAUNCH - cardano-wallet launch" $ do
        it "LAUNCH - Can start launcher with --state-dir" $ withTempDir $ \d -> do
            let args = ["launch", "--network", "testnet", "--random-port", "--state-dir", d]
            let process = proc' "cardano-wallet" args
            withCreateProcess process $ \_ (Just o) (Just e) ph -> do
                expectPathEventuallyExist d
                expectPathEventuallyExist (d <> "/testnet")
                expectPathEventuallyExist (d <> "/wallet.db")
                expectPathEventuallyExist (d <> "/wallet.db-shm")
                expectPathEventuallyExist (d <> "/wallet.db-wal")
                terminateProcess ph
                TIO.hGetContents o >>= TIO.putStrLn
                TIO.hGetContents e >>= TIO.putStrLn

        it "LAUNCH - Can start launcher with --state-dir (empty dir)" $ withTempDir $ \d -> do
            removeDirectory d
            let args = ["launch", "--network", "testnet", "--random-port", "--state-dir", d]
            let process = proc' "cardano-wallet" args
            withCreateProcess process $ \_ (Just o) (Just e) ph -> do
                expectPathEventuallyExist d
                expectPathEventuallyExist (d <> "/testnet")
                expectPathEventuallyExist (d <> "/wallet.db")
                expectPathEventuallyExist (d <> "/wallet.db-shm")
                expectPathEventuallyExist (d <> "/wallet.db-wal")
                terminateProcess ph
                TIO.hGetContents o >>= TIO.putStrLn
                TIO.hGetContents e >>= TIO.putStrLn

        it "LAUNCH - Restoration workers restart" $ withTempDir $ \d -> do
            let port = 8088 :: Int -- Arbitrary but known.
            let baseUrl = "http://localhost:" <> toText port <> "/"
            ctx <- (port,) . (baseUrl,) <$> newManager defaultManagerSettings
            let args = ["launch", "--port", show port, "--state-dir", d]
            let process = proc' "cardano-wallet" args
            wallet <- withCreateProcess process $ \_ (Just o) (Just e) ph -> do
                Stdout m <- generateMnemonicsViaCLI @t []
                waitForServer @t ctx
                let pwd = "passphrase"
                (_, out, _) <- createWalletViaCLI @t ctx ["n"] m "\n" pwd
                terminateProcess ph
                TIO.hGetContents o >>= TIO.putStrLn
                TIO.hGetContents e >>= TIO.putStrLn
                expectValidJSON (Proxy @ApiWallet) out
            withCreateProcess process $ \_ (Just o) (Just e) ph -> do
                waitForServer @t ctx
                expectEventually' ctx state Ready wallet `finally` do
                    terminateProcess ph
                    TIO.hGetContents o >>= TIO.putStrLn
                    TIO.hGetContents e >>= TIO.putStrLn

    describe "DaedalusIPC" $ do
        let tests =
                [ ["--random-port"]
                , ["--port", "8082"]
                , []
                ]
        forM_ tests $ \args -> do
            let title = "should reply with the port when asked " <> show args
            it title $ do
                let filepath = "test/integration/js/mock-daedalus.js"
                (_, _, _, ph) <- createProcess (proc filepath args)
                waitForProcess ph `shouldReturn` ExitSuccess

    describe "LOGGING - cardano-wallet launch logging" $ do
        it "LOGGING - Can log --verbose" $ \_ -> do
            let args = ["launch", "--verbose"]
            let process = proc' "cardano-wallet" args
            process `expectProcStdOutHas` versionLine
            process `expectProcStdOutHas` "Debug"
            process `expectProcStdOutHas` "Warning"
            process `expectProcStdOutHas` "Notice"
            process `expectProcStdOutHas` "Info"

        it "LOGGING - --quiet logs Error only" $ \_ -> do
            let args = ["launch", "--quiet"]
            let process = proc' "cardano-wallet" args
            (o, _) <- getProcStream process 10
            T.pack o `shouldBe` ""

        it "LOGGING - default logs Info" $ \_ -> do
            let args = ["launch"]
            let process = proc' "cardano-wallet" args
            (o, _) <- getProcStream process 5
            o `shouldNotContain` "Debug"
            process `expectProcStdOutHas` versionLine
            process `expectProcStdOutHas` "Warning"
            process `expectProcStdOutHas` "Notice"
            process `expectProcStdOutHas` "Info"

withTempDir :: (FilePath -> IO a) -> IO a
withTempDir = withSystemTempDirectory "integration-state"
