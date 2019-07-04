{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Test.Integration.Jormungandr.Scenario.CLI.Launcher
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
    ( shouldBe, shouldReturn )
import Test.Integration.Framework.DSL
    ( KnownCommand (..)
    , collectStreams
    , createWalletViaCLI
    , expectEventually'
    , expectPathEventuallyExist
    , expectValidJSON
    , generateMnemonicsViaCLI
    , proc'
    , shouldContainT
    , shouldNotContainT
    , state
    , waitForServer
    )
import Test.Integration.Framework.TestData
    ( versionLine )

import qualified Data.Text.IO as TIO

spec :: forall t. (KnownCommand t) => Spec
spec = do
    let block0 = "test/data/jormungandr/block0.bin"
    let leaders = "test/data/jormungandr/secret.yaml"
    describe "LAUNCH - cardano-wallet launch" $ do
        describe "LAUNCH - Can start launcher with --state-dir" $ do
            let tests =
                    [ ("existing dir", const $ pure ())
                    , ("non-existing dir", removeDirectory)
                    ]
            forM_ tests $ \(title, before) -> it title $ withTempDir $ \d -> do
                before d
                let args =
                        [ "launch"
                        , "--random-port"
                        , "--state-dir", d
                        , "--genesis-block", block0
                        , "--bft-leaders", leaders
                        ]
                let process = proc' (commandName @t) args
                withCreateProcess process $ \_ (Just o) (Just e) ph -> do
                    expectPathEventuallyExist d
                    expectPathEventuallyExist (d <> "/jormungandr")
                    expectPathEventuallyExist (d <> "/jormungandr-config.json")
                    expectPathEventuallyExist (d <> "/wallet.db")
                    terminateProcess ph
                    TIO.hGetContents o >>= TIO.putStrLn
                    TIO.hGetContents e >>= TIO.putStrLn

        it "LAUNCH - Restoration workers restart" $ withTempDir $ \d -> do
            let port = 8088 :: Int -- Arbitrary but known.
            let baseUrl = "http://localhost:" <> toText port <> "/"
            ctx <- (port,) . (baseUrl,) <$> newManager defaultManagerSettings
            let args =
                    [ "launch"
                    , "--port", show port
                    , "--state-dir", d
                    , "--genesis-block", block0
                    , "--bft-leaders", leaders
                    ]
            let process = proc' (commandName @t) args
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
        let defaultArgs =
                [ commandName @t
                , "launch"
                , "--genesis-block", block0
                , "--bft-leaders", leaders
                , "--quiet"
                ]
        let tests =
                [ defaultArgs ++ ["--random-port"]
                , defaultArgs ++ ["--port", "8082"]
                , defaultArgs
                ]
        forM_ tests $ \args -> do
            let title = "should reply with the port when asked " <> show args
            it title $ withTempDir $ \d -> do
                let filepath = "test/integration/js/mock-daedalus.js"
                let stateDir = ["--state-dir", d]
                (_, _, _, ph) <- createProcess (proc filepath (args ++ stateDir))
                waitForProcess ph `shouldReturn` ExitSuccess

    describe "LOGGING - cardano-wallet launch logging" $ do
        let defaultArgs =
                [ "launch"
                , "--genesis-block", block0
                , "--bft-leaders", leaders
                ]
        it "LOGGING - Launch can log --verbose" $ withTempDir $ \d -> do
            let args = defaultArgs ++ ["--state-dir", d, "--verbose"]
            let process = proc' (commandName @t) args
            (out, _) <- collectStreams (35, 0) process
            out `shouldContainT` versionLine
            out `shouldContainT` "Debug"
            out `shouldContainT` "Info"
            out `shouldContainT` "Notice"

        it "LOGGING - Launch --quiet logs Error only" $ withTempDir $ \d -> do
            let args = defaultArgs ++ ["--state-dir", d, "--quiet"]
            let process = proc' (commandName @t) args
            (out, err) <- collectStreams (10, 10) process
            out `shouldBe` mempty
            err `shouldBe` mempty

        it "LOGGING - Launch default logs Info" $ withTempDir $ \d -> do
            let args = defaultArgs ++ ["--state-dir", d]
            let process = proc' (commandName @t) args
            (out, _) <- collectStreams (20, 0) process
            out `shouldNotContainT` "Debug"
            out `shouldContainT` versionLine
            out `shouldContainT` "Info"
            out `shouldContainT` "Notice"

withTempDir :: (FilePath -> IO a) -> IO a
withTempDir = withSystemTempDirectory "integration-state"
