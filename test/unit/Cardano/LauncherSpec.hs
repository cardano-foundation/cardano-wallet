{-# LANGUAGE TypeApplications #-}

module Cardano.LauncherSpec
    ( spec
    ) where

import Prelude

import Cardano.Launcher
    ( Command (..), ProcessHasExited (..), StdStream (..), launch )
import Control.Concurrent.MVar
    ( newEmptyMVar, putMVar, tryReadMVar )
import System.Exit
    ( ExitCode (..) )
import Test.Hspec
    ( Spec, it, shouldBe, shouldReturn )


{-# ANN spec ("HLint: ignore Use head" :: String) #-}
spec :: Spec
spec = do
    it "1st process exits with 0, others are cancelled" $ do
        let commands =
              [ Command "./test/data/Launcher/once.sh" ["0"] (pure ()) Inherit
              , Command "./test/data/Launcher/forever.sh" [] (pure ()) Inherit
              ]
        (ProcessHasExited name code) <- launch commands
        name `shouldBe` cmdName (commands !! 0)
        code `shouldBe` ExitSuccess

    it "2nd process exits with 0, others are cancelled" $ do
        let commands =
              [ Command "./test/data/Launcher/forever.sh" [] (pure ()) Inherit
              , Command "./test/data/Launcher/once.sh" ["0"] (pure ()) Inherit
              ]
        (ProcessHasExited name code) <- launch commands
        name `shouldBe` cmdName (commands !! 1)
        code `shouldBe` ExitSuccess

    it "1st process exits with 14, others are cancelled" $ do
        let commands =
              [ Command "./test/data/Launcher/once.sh" ["14"] (pure ()) Inherit
              , Command "./test/data/Launcher/forever.sh" [] (pure ()) Inherit
              ]
        (ProcessHasExited name code) <- launch commands
        name `shouldBe` cmdName (commands !! 0)
        code `shouldBe` (ExitFailure 14)

    it "2nd process exits with 14, others are cancelled" $ do
        let commands =
              [ Command "./test/data/Launcher/forever.sh" [] (pure ()) Inherit
              , Command "./test/data/Launcher/once.sh" ["14"] (pure ()) Inherit
              ]
        (ProcessHasExited name code) <- launch commands
        name `shouldBe` cmdName (commands !! 1)
        code `shouldBe` (ExitFailure 14)

    it "Process executes a command before they start" $ do
        mvar <- newEmptyMVar
        let before = putMVar mvar "executed"
        let commands =
                [ Command "./test/data/Launcher/once.sh" ["0"] before Inherit
                ]
        (ProcessHasExited _ code) <- launch commands
        code `shouldBe` ExitSuccess
        tryReadMVar mvar `shouldReturn` (Just @String "executed")
