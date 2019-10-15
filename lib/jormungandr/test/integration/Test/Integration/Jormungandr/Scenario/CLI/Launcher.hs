{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Test.Integration.Jormungandr.Scenario.CLI.Launcher
    ( spec
    ) where

import Prelude

import Cardano.CLI
    ( Port (..) )
import Cardano.Wallet.Api.Types
    ( ApiWallet )
import Cardano.Wallet.Jormungandr.Launch
    ( setupConfig, teardownConfig )
import Cardano.Wallet.Jormungandr.Network
    ( JormungandrConfig (..) )
import Cardano.Wallet.Primitive.Types
    ( SyncProgress (..) )
import Control.Exception
    ( bracket, finally )
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
    ( Spec, describe, it, pendingWith )
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
import Test.Utils.Ports
    ( findPort )

import qualified Data.Text.IO as TIO

spec :: forall t. (KnownCommand t) => Spec
spec = do
    let block0 = "test/data/jormungandr/block0.bin"
    let config = "test/data/jormungandr/config.yaml"
    let secret = "test/data/jormungandr/secret.yaml"
    describe "LAUNCH - cardano-wallet launch [SERIAL]" $ do
        it "LAUNCH - Stop when --state-dir is an existing file" $ withTempFile $ \f _ -> do
            withJArgs $ \jargs -> do
                let args =
                        [ "launch"
                        , "--state-dir", f
                        , "--"
                        ] ++ jargs
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
                withJArgs $ \jargs -> do
                    let args =
                            [ "launch"
                            , "--random-port"
                            , "--state-dir", d
                            , "--"
                            ] ++ jargs
                    let process = proc' (commandName @t) args
                    withCreateProcess process $ \_ (Just o) (Just e) ph -> do
                        expectPathEventuallyExist d
                        expectPathEventuallyExist (d <> "/wallets")
                      `finally` do
                        terminateProcess ph
                        TIO.hGetContents o >>= TIO.putStrLn
                        TIO.hGetContents e >>= TIO.putStrLn

        it "LAUNCH - Can start launcher with --state-dir (nested dir)" $ withTempDir $ \d -> do
            withJArgs $ \jargs -> do
                let dir = d ++ "/a/b/c/d/e/f/g"
                let args =
                        [ "launch"
                        , "--random-port"
                        , "--state-dir", dir
                        , "--"
                        ] ++ jargs
                let process = proc' (commandName @t) args
                withCreateProcess process $ \_ (Just o) (Just e) ph -> do
                    expectPathEventuallyExist dir
                    expectPathEventuallyExist (dir <> "/wallets")
                  `finally` do
                    terminateProcess ph
                    TIO.hGetContents o >>= TIO.putStrLn
                    TIO.hGetContents e >>= TIO.putStrLn

        it "LAUNCH - Missing --genesis-block option" $ do
            let args =
                    [ "launch"
                    , "--"
                    , "--config", config
                    ]
            (Exit c, Stdout o, Stderr _) <- cardanoWalletCLI @t args
            c `shouldBe` ExitFailure 1
            o `shouldContain`
                "I am looking for '--genesis-block-hash or --genesis-block' and it's not there"

        it "LAUNCH - Missing --config option" $ do
            let args =
                    [ "launch"
                    , "--"
                    , "--genesis-block", block0
                    ]
            (Exit c, Stdout o, Stderr _) <- cardanoWalletCLI @t args
            c `shouldBe` ExitFailure 1
            o `shouldContain`
                "I am looking for '--config' and it's not there"

        it "LAUNCH - Non-Existing file for --genesis-block" $ do
            let block0' = block0 <> ".doesnexist"
            let args =
                    [ "launch"
                    , "--"
                    , "--config", config
                    , "--genesis-block", block0'
                    ]
            (Exit c, Stdout o, Stderr _) <- cardanoWalletCLI @t args
            c `shouldBe` ExitFailure 1
            o `shouldContain`
                ("I couldn't find any file at the given location: " <> block0')

        it "LAUNCH - Non-Existing file for --config" $ do
            let config' = config <> ".doesnexist"
            let args =
                    [ "launch"
                    , "--"
                    , "--config", config'
                    , "--genesis-block", block0
                    ]
            (Exit c, Stdout o, Stderr _) <- cardanoWalletCLI @t args
            c `shouldBe` ExitFailure 1
            o `shouldContain`
                ("I couldn't find any file at the given location: " <> config')

        it "LAUNCH - Invalid block header hash (not base16)" $ do
            let h = "patate"
            let args =
                    [ "launch"
                    , "--"
                    , "--config", config
                    , "--genesis-block-hash", h
                    ]
            (Exit c, Stdout o, Stderr _) <- cardanoWalletCLI @t args
            c `shouldBe` ExitFailure 1
            o `shouldContain`
                ("As far as I can tell, this isn't a valid block hash: " <> h)

        it "LAUNCH - Invalid block header hash (not 32 bytes)" $ do
            let h = "000000000000"
            let args =
                    [ "launch"
                    , "--"
                    , "--config", config
                    , "--genesis-block-hash", h
                    ]
            (Exit c, Stdout o, Stderr _) <- cardanoWalletCLI @t args
            c `shouldBe` ExitFailure 1
            o `shouldContain`
                ("As far as I can tell, this isn't a valid block hash: " <> h)

        it "LAUNCH - Invalid block0 file (not a serialized block)" $ do
            let args =
                    [ "launch"
                    , "--"
                    , "--config", config
                    , "--genesis-block", config
                    ]
            (Exit c, Stdout o, Stderr _) <- cardanoWalletCLI @t args
            c `shouldBe` ExitFailure 1
            o `shouldContain`
                ("As far as I can tell, this isn't a valid block file: " <> config)

        it "LAUNCH - Invalid config file (not JSON)" $ do
            let args =
                    [ "launch"
                    , "--"
                    , "--config", block0
                    , "--genesis-block", block0
                    ]
            (Exit c, Stdout o, Stderr _) <- cardanoWalletCLI @t args
            c `shouldBe` ExitFailure 1
            o `shouldContain`
                ("As far as I can tell, this isn't a valid config file: " <> block0)

        it "LAUNCH - Invalid config file (not JSON)" $ do
            let args =
                    [ "launch"
                    , "--"
                    , "--config", block0
                    , "--genesis-block", block0
                    ]
            (Exit c, Stdout o, Stderr _) <- cardanoWalletCLI @t args
            c `shouldBe` ExitFailure 1
            o `shouldContain`
                ("As far as I can tell, this isn't a valid config file: " <> block0)

        it "LAUNCH - Invalid config file (missing rest configuration)" $ do
            let args =
                    [ "launch"
                    , "--"
                    , "--config", secret
                    , "--genesis-block", block0
                    ]
            (Exit c, Stdout o, Stderr _) <- cardanoWalletCLI @t args
            c `shouldBe` ExitFailure 1
            o `shouldContain`
                ("The REST api is not enabled in the configuration you're \
                \passing down to Jörmungandr, but I need this to be enabled!")

    it "LAUNCH - Restoration workers restart" $ withTempDir $ \d -> do
        pendingWith
            "The test fails unexpectedly in CI and simply hangs for minutes \
            \before eventually timing out. What's happening is unclear put \
            \prevents ongoing work to be integrated. So, disabling this \
            \while investigating the origin of the problem. \
            \See also: https://travis-ci.org/input-output-hk/cardano-wallet/jobs/565974586"
        port <- Port @"wallet" <$> findPort -- Arbitrary but known.
        let baseUrl = "http://localhost:" <> toText port <> "/"
        ctx <- (port,) . (baseUrl,) <$> newManager defaultManagerSettings
        let args =
                [ "launch"
                , "--port", show port
                , "--state-dir", d
                , "--"
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
        it "should reply with the port when asked --random-port" $ withTempDir $ \d -> do
            let filepath = "test/integration/js/mock-daedalus.js"
            withJArgs $ \jargs -> do
                let args =
                        [ commandName @t
                        , "launch"
                        , "--random-port"
                        , "--state-dir", d
                        , "--"
                        ] ++ jargs
                (_, _, _, ph) <- createProcess (proc filepath args)
                waitForProcess ph `shouldReturn` ExitSuccess

    describe "LOGGING - cardano-wallet launch logging [SERIAL]" $ do
        let defaultArgs = ["launch"]
        let nodeArgs = []
        it "LOGGING - Launch can log --verbose" $ withTempDir $ \d -> do
            pendingWith "See 'LAUNCH - Restoration workers restart'"
            let args = defaultArgs ++ ["--state-dir", d, "--verbose"] ++ nodeArgs
            let process = proc' (commandName @t) args
            (out, _) <- collectStreams (35, 0) process
            out `shouldContainT` versionLine
            out `shouldContainT` "Debug"
            out `shouldContainT` "Info"
            out `shouldContainT` "Notice"

        it "LOGGING - Launch --quiet logs Error only" $ withTempDir $ \d -> do
            pendingWith "See 'LAUNCH - Restoration workers restart'"
            let args = defaultArgs ++ ["--state-dir", d, "--quiet"] ++ nodeArgs
            let process = proc' (commandName @t) args
            (out, err) <- collectStreams (10, 10) process
            out `shouldBe` mempty
            err `shouldBe` mempty

        it "LOGGING - Launch default logs Info" $ withTempDir $ \d -> do
            pendingWith "See 'LAUNCH - Restoration workers restart'"
            let args = defaultArgs ++ ["--state-dir", d] ++ nodeArgs
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

withJArgs :: ([String] -> IO a) -> IO a
withJArgs io = bracket setupConfig teardownConfig $
    \((JormungandrConfig args _), _,_) -> io args
