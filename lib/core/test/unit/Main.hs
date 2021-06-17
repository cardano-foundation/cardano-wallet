module Main where

import Prelude

import Cardano.Startup
    ( withUtf8Encoding )
import Control.Concurrent
import Test.Hspec.Runner
    ( defaultConfig, hspecWith )
import Test.Utils.Startup
    ( withLineBuffering )

import qualified Spec

main :: IO ()
main = withLineBuffering $ withUtf8Encoding $ do
    _ <- forkIO monitor
    hspecWith defaultConfig Spec.spec
  where
    monitor = do
        putStrLn "I'm alive! Hydra, please don't stop me."
        threadDelay $ 10 * 1000000
        monitor
