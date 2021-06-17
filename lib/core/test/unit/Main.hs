module Main where

import Cardano.Startup
    ( withUtf8Encoding )
import Prelude
import qualified Spec
import System.IO
    ( BufferMode (..), hSetBuffering, stderr, stdout )
import Test.Hspec.Runner

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    withUtf8Encoding $ hspecWith defaultConfig Spec.spec
