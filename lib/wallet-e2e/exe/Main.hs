module Main where

import qualified Test.Syd.OptParse as SydTest

import Cardano.Wallet.Spec
    ( effectsSpec
    , walletSpec
    )
import Main.Utf8
    ( withUtf8
    )
import Options.Generic
    ( ParseRecord
    , getRecord
    )
import Path
    ( SomeBase (..)
    , parseSomeDir
    )
import Path.IO
    ( AnyPath (makeAbsolute)
    )
import Test.Syd
    ( sydTestWith
    )

newtype Options = Options {stateDir :: FilePath}
    deriving stock (Generic)
    deriving anyclass (ParseRecord)

main :: IO ()
main = withUtf8 do
    Options{stateDir} <- getRecord "E2E test suite for cardano-wallet"
    stateDirectory <-
        parseSomeDir stateDir
            >>= \case
                Abs absDir -> pure absDir
                Rel relDir -> makeAbsolute relDir
    sydTestWith SydTest.defaultSettings{SydTest.settingRetries = 1} do
        effectsSpec
        walletSpec stateDirectory
