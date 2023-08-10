module Main where

import Cardano.Wallet.Spec
    ( spec
    )
import Main.Utf8
    ( withUtf8
    )
import Test.Syd
    ( sydTest
    )

main :: IO ()
main = withUtf8 $ sydTest spec
