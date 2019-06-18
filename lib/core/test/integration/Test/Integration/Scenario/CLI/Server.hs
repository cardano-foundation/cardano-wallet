module Test.Integration.Scenario.CLI.Server
    ( spec
    ) where

import Prelude

import Cardano.Launcher
    ( Command (..), StdStream (..) )
import System.Directory
    ( doesFileExist )
import System.IO.Temp
    ( withSystemTempDirectory )
import Test.Hspec
    ( SpecWith, describe, it )
import Test.Hspec.Expectations.Lifted
    ( shouldReturn )
import Test.Integration.Framework.DSL
    ( Context (..), expectCmdStarts )

spec :: SpecWith (Context t)
spec = do
    describe "SERVER - cardano-wallet serve" $ do
        it "SERVER - Can just start cardano-wallet serve" $ \_ -> do
            let cardanoWalletServer = Command "stack"
                    [ "exec", "--", "cardano-wallet", "serve"
                    ] (return ())
                    Inherit
            expectCmdStarts cardanoWalletServer

        it "SERVER - Can start cardano-wallet serve --database" $ \_ -> do
            withTempDir $ \d -> do
                let dbFile = d ++ "/db-file"
                let cardanoWalletServer = Command "stack"
                        [ "exec", "--", "cardano-wallet", "serve"
                        , "--database", dbFile
                        ] (return ())
                        Inherit
                expectCmdStarts cardanoWalletServer
                doesFileExist dbFile `shouldReturn` True
                doesFileExist (dbFile ++ "-shm") `shouldReturn` True
                doesFileExist (dbFile ++ "-wal") `shouldReturn` True

withTempDir :: (FilePath -> IO a) -> IO a
withTempDir = withSystemTempDirectory "integration-state"
