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
import Cardano.Wallet.Primitive.Types
    ( SyncProgress (..) )
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
    ( terminateProcess, withCreateProcess )
import Test.Hspec
    ( Spec, describe, it, pendingWith )
import Test.Hspec.Expectations.Lifted
    ( shouldBe, shouldContain )
import Test.Integration.Framework.DSL
    ( KnownCommand (..)
    , cardanoWalletCLI
    , createWalletViaCLI
    , expectEventually'
    , expectPathEventuallyExist
    , expectValidJSON
    , generateMnemonicsViaCLI
    , getWalletEp
    , proc'
    , state
    , waitForServer
    )
import Test.Utils.Ports
    ( findPort )

import qualified Data.Text.IO as TIO

spec :: forall t. (KnownCommand t) => Spec
spec = do
    let block0 = "test/data/jormungandr/block0.bin"
    let secret = "test/data/jormungandr/secret.yaml"
    describe "LAUNCH - cardano-wallet launch [SERIAL]" $ do
        it "LAUNCH - Stop when --state-dir is an existing file" $ withTempFile $ \f _ -> do
            let args =
                    [ "launch"
                    , "--genesis-block", block0
                    , "--state-dir", f
                    , "--"
                    , "--secret", secret
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
                        , "--"
                        , "--secret", secret
                        ]
                let process = proc' (commandName @t) args
                withCreateProcess process $ \_ (Just o) (Just e) ph -> do
                    expectPathEventuallyExist d
                    expectPathEventuallyExist (d <> "/chain")
                    expectPathEventuallyExist (d <> "/jormungandr-config.yaml")
                    expectPathEventuallyExist (d <> "/wallets")
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
                    , "--"
                    , "--secret", secret
                    ]
            let process = proc' (commandName @t) args
            withCreateProcess process $ \_ (Just o) (Just e) ph -> do
                expectPathEventuallyExist dir
                expectPathEventuallyExist (dir <> "/chain")
                expectPathEventuallyExist (dir <> "/jormungandr-config.yaml")
                expectPathEventuallyExist (dir <> "/wallets")
              `finally` do
                terminateProcess ph
                TIO.hGetContents o >>= TIO.putStrLn
                TIO.hGetContents e >>= TIO.putStrLn

        it "LAUNCH - Non-Existing files for --genesis-block" $ do
            let block0' = block0 <> ".doesnexist"
            let args =
                    [ "launch"
                    , "--genesis-block", block0'
                    , "--"
                    , "--secret", secret
                    ]
            (Exit c, Stdout o, Stderr e) <- cardanoWalletCLI @t args
            c `shouldBe` ExitFailure 1
            o `shouldBe` mempty
            e `shouldContain`
                ("I couldn't find any file at the given location: " <> block0')

        it "LAUNCH - Invalid block0 file (not a serialized block)" $ do
            let args =
                    [ "launch"
                    , "--genesis-block", secret
                    , "--"
                    , "--secret", secret
                    ]
            (Exit c, Stdout o, Stderr _) <- cardanoWalletCLI @t args
            c `shouldBe` ExitFailure 1
            o `shouldContain` ("As far as I can tell, this isn't a valid block file: " <> secret)

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
                    , "--genesis-block", block0
                    , "--"
                    , "--secret", secret
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
                expectEventually' ctx getWalletEp state Ready wallet
              `finally` do
                terminateProcess ph
                TIO.hGetContents o >>= TIO.putStrLn
                TIO.hGetContents e >>= TIO.putStrLn

withTempDir :: (FilePath -> IO a) -> IO a
withTempDir = withSystemTempDirectory "integration-state"

withTempFile :: (FilePath -> Handle -> IO a) -> IO a
withTempFile = withSystemTempFile "temp-file"
