module Main where

import Cardano.Wallet.Spec
    ( environmentSpec, walletSpec )
import Main.Utf8
    ( withUtf8 )
import Test.Syd
    ( sydTest )

main :: IO ()
main = withUtf8 do
    sydTest environmentSpec
    sydTest walletSpec
