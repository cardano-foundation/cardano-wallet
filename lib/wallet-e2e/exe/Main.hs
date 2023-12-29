module Main where

import qualified Test.Syd.OptParse as SydTest

import Cardano.Wallet.Spec
    ( effectsSpec
    , walletSpec
    )
import Main.Utf8
    ( withUtf8
    )
import Options
    ( withTestOptions
    )
import Test.Syd
    ( sydTestWith
    )

main :: IO ()
main = withUtf8 $ withTestOptions $ \testNetwork traceConfiguration ->
    sydTestWith SydTest.defaultSettings{SydTest.settingRetries = 1} do
        effectsSpec
        walletSpec traceConfiguration testNetwork
