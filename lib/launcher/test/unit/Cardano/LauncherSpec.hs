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
import System.Info
    ( os )
import Test.Hspec
    ( Spec, it, shouldBe, shouldContain, shouldReturn )

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
              [ mockCommand True (pure ())
              , foreverCommand
              ]
        (ProcessHasExited name code) <- launch nullTracer commands
        name `shouldBe` cmdName (commands !! 0)
        code `shouldBe` ExitSuccess

    it "2nd process exits with 0, others are cancelled" $ do
        let commands =
              [ foreverCommand
              , mockCommand True (pure ())
              ]
        (ProcessHasExited name code) <- launch nullTracer commands
        name `shouldBe` cmdName (commands !! 1)
        code `shouldBe` ExitSuccess

    it "1st process exits with 3, others are cancelled" $ do
        let commands =
              [ mockCommand False (pure ())
              , foreverCommand
              ]
        (ProcessHasExited name code) <- launch nullTracer commands
        name `shouldBe` cmdName (commands !! 0)
        code `shouldBe` (ExitFailure 3)

    it "2nd process exits with 3, others are cancelled" $ do
        let commands =
              [ foreverCommand
              , mockCommand False (pure ())
              ]
        (ProcessHasExited name code) <- launch nullTracer commands
        name `shouldBe` cmdName (commands !! 1)
        code `shouldBe` (ExitFailure 3)

    it "Process executes a command before they start" $ do
        mvar <- newEmptyMVar
        let before = putMVar mvar "executed"
        let commands =
                [ mockCommand True before
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

    it "Sanity check System.Info.os" $
        ["linux", "darwin", "mingw32"] `shouldContain` [os]

-- | A command that will run for a short time.
mockCommand :: Bool -> IO () -> Command
mockCommand success before
    | isWindows && success =
        Command "TIMEOUT" ["1"] before Inherit
    | isWindows && not success =
        Command "CHOICE" ["/T", "1", "/C", "wat", "/D", "t"] before Inherit
    | otherwise =
        Command "sh" ["-c", "sleep 1; exit " ++ show exitStatus] before Inherit
        where exitStatus = if success then 0 else 3 :: Int

-- | A command that will run for longer than the other commands.
foreverCommand :: Command
foreverCommand
    | isWindows = Command "TIMEOUT" ["30"] (pure ()) Inherit
    | otherwise = Command "sleep" ["30"] (pure ()) Inherit

isWindows :: Bool
isWindows = os == "mingw32"
