{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Test.Integration.Jormungandr.Scenario.CLI.Launcher
    ( spec
    ) where

import Prelude

import Cardano.BM.Data.Tracer
    ( nullTracer )
import Cardano.CLI
    ( Port (..) )
import Cardano.Launcher
    ( Command (..), StdStream (..), withBackendProcess )
import Cardano.Wallet.Api.Types
    ( ApiAccount )
import Cardano.Wallet.Network.Ports
    ( findPort )
import Cardano.Wallet.Primitive.SyncProgress
    ( SyncProgress (..) )
import Control.Exception
    ( finally )
import Control.Monad
    ( forM_, void )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
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
import System.FilePath
    ( (</>) )
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
    , eventually
    , expectCliField
    , expectPathEventuallyExist
    , expectValidJSON
    , generateMnemonicsViaCLI
    , getWalletViaCLI
    , proc'
    , waitForServer
    , walletId
    )
import Test.Utils.Paths
    ( getTestData )

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

spec :: forall t. (KnownCommand t) => Spec
spec = do
    let testData = $(getTestData) </> "jormungandr"
    let block0 = testData </> "block0.bin"
    let secret = testData </> "secret.yaml"
    let config = testData </> "config.yaml"
    describe "LAUNCH - cardano-wallet launch [SERIAL]" $ do
        it "LAUNCH - Stop when --state-dir is an existing file" $ withTempFile $ \f _ -> do
            let args =
                    [ "launch"
                    , "--genesis-block", block0
                    , "--state-dir", f
                    , "--"
                    , "--secret", secret
                    , "--config", config
                    ]
            (Exit c, Stdout _, Stderr e) <- cardanoWalletCLI @t args
            c `shouldBe` ExitFailure 1
            e `shouldContain` f ++ " must be a directory, but it is a file. Exiting.\n"

        describe "LAUNCH - Can start launcher with --state-dir" $ do
            let tests =
                    [ ("existing dir", const $ pure ())
                    , ("non-existing dir", removeDirectory)
                    ]
            forM_ tests $ \(title, before) -> it title $ withTempDir $ \d -> do
                before d
                let cmd = Command
                        (commandName @t)
                        [ "launch"
                        , "--random-port"
                        , "--state-dir", d
                        , "--genesis-block", block0
                        , "--"
                        , "--secret", secret
                        , "--config", config
                        ]
                        (pure ())
                        Inherit
                        Inherit
                void $ withBackendProcess nullTracer cmd $ do
                    expectPathEventuallyExist d
                    expectPathEventuallyExist (d </> "chain")
                    expectPathEventuallyExist (d </> "wallets")

        it "LAUNCH - Can start launcher with --state-dir (nested dir)" $ withTempDir $ \d -> do
            let dir = d </> "a" </> "b" </> "c"
            let cmd = Command
                    (commandName @t)
                    [ "launch"
                    , "--random-port"
                    , "--state-dir", dir
                    , "--genesis-block", block0
                    , "--"
                    , "--secret", secret
                    , "--config", config
                    ]
                    (pure ())
                    Inherit
                    Inherit
            void $ withBackendProcess nullTracer cmd $ do
                expectPathEventuallyExist dir
                expectPathEventuallyExist (dir </> "chain")
                expectPathEventuallyExist (dir </> "wallets")

        it "LAUNCH - Non-Existing files for --genesis-block" $ do
            let block0' = block0 <> ".doesnexist"
            let args =
                    [ "launch"
                    , "--genesis-block", block0'
                    , "--"
                    , "--secret", secret
                    , "--config", config
                    ]
            (Exit c, Stdout _, Stderr e) <- cardanoWalletCLI @t @_ @IO args
            c `shouldBe` ExitFailure 1
            e `shouldContain`
                ("I couldn't find any file at the given location: " <> block0')

        it "LAUNCH - Invalid block0 file (not a serialized block)" $ do
            let args =
                    [ "launch"
                    , "--genesis-block", secret
                    , "--"
                    , "--secret", secret
                    , "--config", config
                    ]
            (Exit c, Stdout _, Stderr _) <- cardanoWalletCLI @t @_ @IO args
            c `shouldBe` ExitFailure 33
            -- FIXME: https://github.com/input-output-hk/cardano-wallet/issues/2187
            -- o `shouldContain`
            --     ("As far as I can tell, this isn't a valid block file: " <> secret)

        it "LAUNCH - Conflicting --rest-listen in extra arguments" $ do
            let args =
                    [ "launch"
                    , "--genesis-block", block0
                    , "--"
                    , "--rest-listen", "127.0.0.1:8080"
                    ]
            (Exit c, Stdout _, Stderr e) <- cardanoWalletCLI @t @_ @IO args
            c `shouldBe` ExitFailure 1
            e `shouldContain`
                "The --rest-listen option is used by the 'launch' command."

        it "LAUNCH - Conflicting --storage in extra arguments" $ do
            let args =
                    [ "launch"
                    , "--genesis-block", block0
                    , "--"
                    , "--storage", "/tmp/whatever"
                    ]
            (Exit c, Stdout _, Stderr e) <- cardanoWalletCLI @t @_ @IO args
            c `shouldBe` ExitFailure 1
            e `shouldContain`
                "The --storage option is used by the 'launch' command."

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
                    , "--config", config
                    ]
            let process = proc' (commandName @t) args
            wallet <- withCreateProcess process $ \_ (Just o) (Just e) ph -> do
                Stdout m <- generateMnemonicsViaCLI @t []
                waitForServer @t ctx
                let pwd = "passphrase"
                (_, out, _) <- createWalletViaCLI @t ctx ["n"] m "\n" pwd
                expectValidJSON (Proxy @ApiAccount) out
              `finally` do
                terminateProcess ph
                TIO.hGetContents o >>= TIO.putStrLn
                TIO.hGetContents e >>= TIO.putStrLn
            withCreateProcess process $ \_ (Just o) (Just e) ph -> do
                waitForServer @t ctx
                eventually "Wallet state = Ready" $ do
                    Stdout og <- getWalletViaCLI @t ctx $ T.unpack (wallet ^. walletId)
                    jg <- expectValidJSON (Proxy @ApiAccount) og
                    expectCliField (#state . #getApiT) (`shouldBe` Ready) jg
              `finally` do
                terminateProcess ph
                TIO.hGetContents o >>= TIO.putStrLn
                TIO.hGetContents e >>= TIO.putStrLn

withTempDir :: (FilePath -> IO a) -> IO a
withTempDir = withSystemTempDirectory "integration-state"

withTempFile :: (FilePath -> Handle -> IO a) -> IO a
withTempFile = withSystemTempFile "temp-file"
