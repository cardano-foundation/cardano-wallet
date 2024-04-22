import qualified Test.Syd.OptParse as SydTest

import Cardano.Wallet.Spec
    ( effectsSpec
    , walletSpec
    )
import Cardano.Wallet.Spec.Options
    ( withTestOptions
    )
import Main.Utf8
    ( withUtf8
    )
import Test.Syd
    ( sydTestWith
    )

main :: IO ()
main = withUtf8 $ withTestOptions $ \testNetwork traceConfiguration ->
    sydTestWith SydTest.defaultSettings{SydTest.settingRetries = 1} do
        effectsSpec
        walletSpec traceConfiguration testNetwork
