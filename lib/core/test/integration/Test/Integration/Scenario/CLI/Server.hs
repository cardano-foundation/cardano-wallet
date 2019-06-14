module Test.Integration.Scenario.CLI.Server
    ( spec
    ) where

import Prelude

import Cardano.Launcher
    ( Command (..), StdStream (..))
import Test.Hspec.Expectations.Lifted
    ( shouldBe )
import Control.Monad
    ( when )
import System.Directory
    ( doesFileExist, removePathForcibly )
import Test.Hspec
    ( SpecWith, after_, describe, it )
import Test.Integration.Framework.DSL
    ( Context (..), expectCmdStarts )

spec :: SpecWith (Context t)
spec = after_ tearDown $ do
    describe "SERVER - cardano-wallet server" $ do
        it "SERVER - Can start cardano-wallet server without --database" $ \_ -> do
            let cardanoWalletServer = Command
                    "cardano-wallet"
                    [ "server"
                    , "--random-port"
                    ] (return ())
                    Inherit
            expectCmdStarts cardanoWalletServer

            w1 <- doesFileExist dbFile
            w2 <- doesFileExist (dbFile ++ "-shm")
            w3 <- doesFileExist (dbFile ++ "-wal")
            w1 `shouldBe` False
            w2 `shouldBe` False
            w3 `shouldBe` False

        it "SERVER - Can start cardano-wallet server with --database" $ \_ -> do
            let cardanoWalletServer = Command
                    "cardano-wallet"
                    [ "server"
                    , "--random-port"
                    , "--database", dbFile
                    ] (return ())
                    Inherit
            expectCmdStarts cardanoWalletServer

            w1 <- doesFileExist dbFile
            w2 <- doesFileExist (dbFile ++ "-shm")
            w3 <- doesFileExist (dbFile ++ "-wal")
            w1 `shouldBe` True
            w2 `shouldBe` True
            w3 `shouldBe` True
 where
     dbFile = "./test/data/test-DB-File"
     tearDown = do
         f1 <- doesFileExist dbFile
         f2 <- doesFileExist (dbFile ++ "-shm")
         f3 <- doesFileExist (dbFile ++ "-wal")
         when f1 $ removePathForcibly dbFile
         when f2 $ removePathForcibly (dbFile ++ "-shm")
         when f3 $ removePathForcibly (dbFile ++ "-wal")
