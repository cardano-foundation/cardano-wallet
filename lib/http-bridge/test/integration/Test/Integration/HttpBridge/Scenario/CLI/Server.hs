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
        it "LOGGING - Can log --verbose" $ \_ -> do
            let args = ["serve", "--verbose"]
            let process = proc' (commandName @t) args
            process `expectProcStdOutHas` versionLine
            process `expectProcStdOutHas` "Debug"
            process `expectProcStdOutHas` "Warning"
            process `expectProcStdOutHas` "Notice"
            process `expectProcStdOutHas` "Info"

        it "LOGGING - --quiet logs Error only" $ \_ -> do
            let args = ["serve", "--quiet"]
            let process = proc' (commandName @t) args
            (o, e) <- getProcStream process 10
            T.pack o `shouldBe` ""
            T.pack e `shouldBe` ""

        it "LOGGING - default logs Info" $ \_ -> do
            let args = ["serve"]
            let process = proc' (commandName @t) args
            (o, e) <- getProcStream process 5
            o `shouldNotContain` "Debug"
            T.pack e `shouldBe` ""
            process `expectProcStdOutHas` versionLine
            process `expectProcStdOutHas` "Warning"
            process `expectProcStdOutHas` "Notice"
            process `expectProcStdOutHas` "Info"

oneSecond :: Int
oneSecond = 1000000

withTempDir :: (FilePath -> IO a) -> IO a
withTempDir = withSystemTempDirectory "integration-state"
