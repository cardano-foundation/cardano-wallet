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
              [ mockCommand 0 (pure ())
              , foreverCommand
              ]
        (ProcessHasExited name code) <- launch nullTracer commands
        name `shouldBe` cmdName (commands !! 0)
        code `shouldBe` ExitSuccess

    it "2nd process exits with 0, others are cancelled" $ do
        let commands =
              [ foreverCommand
              , mockCommand 0 (pure ())
              ]
        (ProcessHasExited name code) <- launch nullTracer commands
        name `shouldBe` cmdName (commands !! 1)
        code `shouldBe` ExitSuccess

    it "1st process exits with 14, others are cancelled" $ do
        let commands =
              [ mockCommand 14 (pure ())
              , foreverCommand
              ]
        (ProcessHasExited name code) <- launch nullTracer commands
        name `shouldBe` cmdName (commands !! 0)
        code `shouldBe` (ExitFailure 14)

    it "2nd process exits with 14, others are cancelled" $ do
        let commands =
              [ foreverCommand
              , mockCommand 14 (pure ())
              ]
        (ProcessHasExited name code) <- launch nullTracer commands
        name `shouldBe` cmdName (commands !! 1)
        code `shouldBe` (ExitFailure 14)

    it "Process executes a command before they start" $ do
        mvar <- newEmptyMVar
        let before = putMVar mvar "executed"
        let commands =
                [ mockCommand 0 before
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

-- | A command that will run for a short time then exit with the given status.
mockCommand :: Int -> IO () -> Command
mockCommand exitStatus before =
    Command "sh" ["-c", "sleep 1; exit " ++ show exitStatus] before Inherit

-- | A command that will run for longer than the other commands.
foreverCommand :: Command
foreverCommand = Command "sleep" ["30"] (pure ()) Inherit
