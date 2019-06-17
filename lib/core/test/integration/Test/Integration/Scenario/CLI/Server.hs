module Test.Integration.Scenario.CLI.Server
    ( spec
    ) where

import Prelude

import Cardano.Launcher
    ( Command (..), StdStream (..) )
import System.Directory
    ( doesFileExist, removePathForcibly )
import Test.Hspec
    ( SpecWith, after_, describe, it )
import Test.Hspec.Expectations.Lifted
    ( shouldReturn )
import Test.Integration.Framework.DSL
    ( Context (..), expectCmdStarts )

spec :: SpecWith (Context t)
spec = after_ tearDown $ do
    describe "SERVER - cardano-wallet serve" $ do
        it "SERVER - Can start cardano-wallet serve without --database" $ \_ -> do
            let cardanoWalletServer = Command "stack"
                    [ "exec", "--", "cardano-wallet", "serve"
                    ] (return ())
                    Inherit
            expectCmdStarts cardanoWalletServer
            doesFileExist dbFile `shouldReturn` False
            doesFileExist (dbFile ++ "-shm") `shouldReturn` False
            doesFileExist (dbFile ++ "-wal") `shouldReturn` False

        it "SERVER - Can start cardano-wallet serve with --database" $ \_ -> do
            let cardanoWalletServer = Command "stack"
                    [ "exec", "--", "cardano-wallet", "serve"
                    , "--database", dbFile
                    ] (return ())
                    Inherit
            expectCmdStarts cardanoWalletServer
            doesFileExist dbFile `shouldReturn` True
            doesFileExist (dbFile ++ "-shm") `shouldReturn` True
            doesFileExist (dbFile ++ "-wal") `shouldReturn` True
 where
     dbFile = "./test/data/test-DB-File"
     tearDown = do
         removePathForcibly dbFile
         removePathForcibly (dbFile ++ "-shm")
         removePathForcibly (dbFile ++ "-wal")
