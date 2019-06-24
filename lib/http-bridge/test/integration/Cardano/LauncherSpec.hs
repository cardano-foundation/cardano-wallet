module Cardano.LauncherSpec
    ( spec
    ) where

import Prelude

import Control.Monad
    ( forM_ )
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
    ( shouldReturn )
import Test.Integration.Framework.DSL
    ( expectPathEventuallyExist, proc' )


import qualified Data.Text.IO as TIO

spec :: Spec
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

withTempDir :: (FilePath -> IO a) -> IO a
withTempDir = withSystemTempDirectory "integration-state"
