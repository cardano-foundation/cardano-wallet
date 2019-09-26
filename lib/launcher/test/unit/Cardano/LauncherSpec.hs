{-# LANGUAGE TypeApplications #-}

module Cardano.LauncherSpec
    ( spec
    ) where

import Prelude

import Cardano.BM.Trace
    ( nullTracer )
import Cardano.Launcher
    ( Command (..), ProcessHasExited (..), StdStream (..), launch )
import Control.Concurrent.MVar
    ( newEmptyMVar, putMVar, tryReadMVar )
import Data.Text
    ( Text )
import Fmt
    ( pretty )
import System.Exit
    ( ExitCode (..) )
import Test.Hspec
    ( Spec, it, shouldBe, shouldReturn )

{-# ANN spec ("HLint: ignore Use head" :: String) #-}
spec :: Spec
spec = do
    it "Buildable Command" $ do
        let command = Command "server"
                [ "start"
                , "--port", "8080"
                , "--template", "mainnet"
                ] (pure ())
                Inherit

        pretty @_ @Text command `shouldBe`
            "server\n\
            \     start\n\
            \     --port 8080\n\
            \     --template mainnet\n"

    it "1st process exits with 0, others are cancelled" $ do
        let commands =
              [ Command "./test/data/once.sh" ["0"] (pure ()) Inherit
              , Command "./test/data/forever.sh" [] (pure ()) Inherit
              ]
        (ProcessHasExited name code) <- launch nullTracer commands
        name `shouldBe` cmdName (commands !! 0)
        code `shouldBe` ExitSuccess

    it "2nd process exits with 0, others are cancelled" $ do
        let commands =
              [ Command "./test/data/forever.sh" [] (pure ()) Inherit
              , Command "./test/data/once.sh" ["0"] (pure ()) Inherit
              ]
        (ProcessHasExited name code) <- launch nullTracer commands
        name `shouldBe` cmdName (commands !! 1)
        code `shouldBe` ExitSuccess

    it "1st process exits with 14, others are cancelled" $ do
        let commands =
              [ Command "./test/data/once.sh" ["14"] (pure ()) Inherit
              , Command "./test/data/forever.sh" [] (pure ()) Inherit
              ]
        (ProcessHasExited name code) <- launch nullTracer commands
        name `shouldBe` cmdName (commands !! 0)
        code `shouldBe` (ExitFailure 14)

    it "2nd process exits with 14, others are cancelled" $ do
        let commands =
              [ Command "./test/data/forever.sh" [] (pure ()) Inherit
              , Command "./test/data/once.sh" ["14"] (pure ()) Inherit
              ]
        (ProcessHasExited name code) <- launch nullTracer commands
        name `shouldBe` cmdName (commands !! 1)
        code `shouldBe` (ExitFailure 14)

    it "Process executes a command before they start" $ do
        mvar <- newEmptyMVar
        let before = putMVar mvar "executed"
        let commands =
                [ Command "./test/data/once.sh" ["0"] before Inherit
                ]
        (ProcessHasExited _ code) <- launch nullTracer commands
        code `shouldBe` ExitSuccess
        tryReadMVar mvar `shouldReturn` (Just @String "executed")

    it "Handles command not found" $ do
        let commands =
                [ Command "foobar" [] (pure ()) Inherit
                ]
        ProcessDidNotStart name _exc <- launch nullTracer commands
        name `shouldBe` "foobar"
