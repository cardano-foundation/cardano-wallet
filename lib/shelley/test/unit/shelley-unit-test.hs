module Main where

import Cardano.Wallet.Prelude

import Cardano.Wallet.Startup
    ( withUtf8Encoding )
import Test.Hspec.Extra
    ( hspecMain )

import qualified Spec

main :: IO ()
main = withUtf8Encoding $ hspecMain Spec.spec
