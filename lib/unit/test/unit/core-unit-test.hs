module Main where

import Cardano.Crypto.Init
    ( cryptoInit
    )
import Cardano.Crypto.WalletHD.Encrypted
    ( withFastKdfForTesting
    )
import Main.Utf8
    ( withUtf8
    )
import System.Mem
    ( performMajorGC
    )
import Test.Hspec.Extra
    ( hspecMain
    )
import Prelude

import qualified Spec

main :: IO ()
main = withUtf8 $ do
    cryptoInit
    withFastKdfForTesting $ do
        hspecMain Spec.spec
        performMajorGC
