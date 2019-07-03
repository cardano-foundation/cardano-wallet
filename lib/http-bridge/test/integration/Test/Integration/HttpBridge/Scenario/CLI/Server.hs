{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Integration.HttpBridge.Scenario.CLI.Server
    ( spec
    ) where

import Prelude

import Control.Concurrent
    ( threadDelay )
import System.IO.Temp
    ( withSystemTempDirectory )
import System.Process
    ( terminateProcess, withCreateProcess )
import Test.Hspec
    ( SpecWith, describe, it )
import Test.Hspec.Expectations.Lifted
    ( shouldBe, shouldNotContain )
import Test.Integration.Framework.DSL
    ( Context (..)
    , KnownCommand (..)
    , expectPathEventuallyExist
    , expectProcStdOutHas
    , getProcStream
    , proc'
    )
import Test.Integration.Framework.TestData
    ( versionLine )

import qualified Data.Text as T

spec :: forall t. KnownCommand t => SpecWith (Context t)
spec = do
    describe "SERVER - cardano-wallet serve" $ do
        it "SERVER - Can start cardano-wallet serve --database" $ \_ -> do
            withTempDir $ \d -> do
                let db = d ++ "/db-file"
                let args = ["serve", "--database", db]
                let process = proc' (commandName @t) args
                withCreateProcess process $ \_ _ _ ph -> do
                    expectPathEventuallyExist db
                    terminateProcess ph
            threadDelay oneSecond

    describe "LOGGING - cardano-wallet serve logging" $ do
        it "LOGGING - Serve an log --verbose" $ \_ -> do
            let args = ["serve", "--verbose"]
            let process = proc' (commandName @t) args
            (process, 30) `expectProcStdOutHas` versionLine
            (process, 30) `expectProcStdOutHas` "Debug"
            (process, 30) `expectProcStdOutHas` "Notice"
            (process, 30) `expectProcStdOutHas` "Info"

        it "LOGGING - Serve --quiet logs Error only" $ \_ -> do
            let args = ["serve", "--quiet"]
            let process = proc' (commandName @t) args
            (o, e) <- getProcStream process 10
            T.pack o `shouldBe` ""
            T.pack e `shouldBe` ""

        it "LOGGING - Serve default logs Info" $ \_ -> do
            let args = ["serve"]
            let process = proc' (commandName @t) args
            (o, e) <- getProcStream process 5
            o `shouldNotContain` "Debug"
            T.pack e `shouldBe` ""
            -- 9 lines should be enough to get desired entries
            (process, 9) `expectProcStdOutHas` versionLine
            (process, 9) `expectProcStdOutHas` "Notice"
            (process, 9) `expectProcStdOutHas` "Info"

oneSecond :: Int
oneSecond = 1000000

withTempDir :: (FilePath -> IO a) -> IO a
withTempDir = withSystemTempDirectory "integration-state"
