import Control.Monad
    ( forM_ )
import Launcher
import Prelude
import System.Exit
    ( ExitCode (..) )
import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "Launcher.monitor" $ do
        it "node + wallet" $ do
            let commands =
                  [ Command "./app/launcher/mock/node-exit-0.sh" []
                  , Command "./app/launcher/mock/wallet.sh" []
                  ]
            running <- launch commands
            (ProcessHasExited name code) <- monitor running
            putStrLn "killing"
            forM_ running kill
            putStrLn "has killed"
            name `shouldBe` cmdName (commands !! 0)
            code `shouldBe` ExitSuccess
            -- Not quite working

        it "trivial" $
            True `shouldBe` True
