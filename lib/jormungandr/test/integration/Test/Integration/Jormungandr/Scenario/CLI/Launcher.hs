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
    ( Exit (..), Stderr (..), Stdout (..) )
import System.Directory
    ( removeDirectory )
import System.Exit
    ( ExitCode (..) )
import System.IO
    ( Handle )
import System.IO.Temp
    ( withSystemTempDirectory, withSystemTempFile )
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
    ( shouldBe, shouldContain, shouldReturn )
import Test.Integration.Framework.DSL
    ( KnownCommand (..)
    , cardanoWalletCLI
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
        it "LAUNCHx - Stop when --state-dir is an existing file" $ withTempFile $ \f _ -> do
            let args =
                    [ "launch"
                    , "--genesis-block", block0
                    , "--bft-leaders", leaders
                    , "--state-dir", f
                    ]

            (Exit c, Stdout o, Stderr e) <- cardanoWalletCLI @t args
            c `shouldBe` ExitFailure 1
            o `shouldBe` mempty
            e `shouldBe` f ++ " must be a directory, but it is a file. Exiting.\n"

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
                    expectPathEventuallyExist (d <> "/chain")
                    expectPathEventuallyExist (d <> "/jormungandr-config.json")
                    expectPathEventuallyExist (d <> "/wallet.db")
                  `finally` do
                    terminateProcess ph
                    TIO.hGetContents o >>= TIO.putStrLn
                    TIO.hGetContents e >>= TIO.putStrLn

        it "LAUNCH - Can start launcher with --state-dir (nested dir)" $ withTempDir $ \d -> do
            let dir = d ++ "/a/b/c/d/e/f/g"
            let args =
                    [ "launch"
                    , "--random-port"
                    , "--state-dir", dir
                    , "--genesis-block", block0
                    , "--bft-leaders", leaders
                    ]
            let process = proc' (commandName @t) args
            withCreateProcess process $ \_ (Just o) (Just e) ph -> do
                expectPathEventuallyExist dir
                expectPathEventuallyExist (dir <> "/chain")
                expectPathEventuallyExist (dir <> "/jormungandr-config.json")
                expectPathEventuallyExist (dir <> "/wallet.db")
              `finally` do
                terminateProcess ph
                TIO.hGetContents o >>= TIO.putStrLn
                TIO.hGetContents e >>= TIO.putStrLn

        it "LAUNCH - Non-Existing files for --genesis-block" $ do
            let block0' = block0 <> ".doesnexist"
            let args =
                    [ "launch"
                    , "--genesis-block", block0'
                    , "--bft-leaders", leaders
                    ]
            (Exit c, Stdout o, Stderr e) <- cardanoWalletCLI @t args
            c `shouldBe` ExitFailure 1
            o `shouldBe` mempty
            e `shouldContain`
                ("I couldn't find any file at the given location: " <> block0')

        it "LAUNCH - Invalid block0 file (not a serialized block)" $ do
            let args =
                    [ "launch"
                    , "--genesis-block", leaders
                    , "--bft-leaders", leaders
                    ]
            (Exit c, Stdout o, Stderr e) <- cardanoWalletCLI @t args
            c `shouldBe` ExitFailure 1
            o `shouldBe` mempty
            e `shouldContain`
                ("As far as I can tell, this isn't a valid block file: " <> leaders)

        it "LAUNCH - Non-Existing files for --bft-leaders" $ do
            let leaders' = leaders <> ".doesnexist"
            let args =
                    [ "launch"
                    , "--genesis-block", block0
                    , "--bft-leaders", leaders'
                    ]
            (Exit c, Stdout o, Stderr e) <- cardanoWalletCLI @t args
            c `shouldBe` ExitFailure 1
            o `shouldBe` mempty
            e `shouldContain`
                ("I couldn't find any file at the given location: " <> leaders')

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
                expectValidJSON (Proxy @ApiWallet) out
              `finally` do
                terminateProcess ph
                TIO.hGetContents o >>= TIO.putStrLn
                TIO.hGetContents e >>= TIO.putStrLn
            withCreateProcess process $ \_ (Just o) (Just e) ph -> do
                waitForServer @t ctx
                expectEventually' ctx state Ready wallet
              `finally` do
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

withTempFile :: (FilePath -> Handle -> IO a) -> IO a
withTempFile = withSystemTempFile "temp-file"
