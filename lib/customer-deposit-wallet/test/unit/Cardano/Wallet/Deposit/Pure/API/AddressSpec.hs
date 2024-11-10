module Cardano.Wallet.Deposit.Pure.API.AddressSpec
    ( spec
    )
where

import Prelude

import Cardano.Wallet.Deposit.Pure.API.Address
    ( decodeAddress
    , encodeAddress
    )
import Control.Monad
    ( forM_
    )
import Test.Cardano.Ledger.Core.Arbitrary
    ()
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    )
import Test.QuickCheck
    ( Arbitrary (..)
    , forAll
    , (===)
    )

spec :: Spec
spec = do
    describe "address codec" $ do
        it "rountrips correctly on random addresses" $ forAll arbitrary $ \x ->
            decodeAddress (encodeAddress x)
                === Right x
        it "roundtrips correctly on some addresses from online examples"
            $ do
                let testCases =
                        [ "addr1z92l7rnra7sxjn5qv5fzc4fwsrrm29mgkleqj9a0y46j5lyjz4gwd3njhyqwntdkcm8rrgapudajydteywgtuvl6etjs9nqzg5"
                        , "addr_test1wppg9l6relcpls4u667twqyggkrpfrs5cdge9hhl9cv2upchtch0h"
                        , "37btjrVyb4KDXBNC4haBVPCrro8AQPHwvCMp3RFhhSVWwfFmZ6wwzSK6JK1hY6wHNmtrpTf1kdbva8TCneM2YsiXT7mrzT21EacHnPpz5YyUdj64na"
                        ]
                forM_ testCases $ \addr ->
                    encodeAddress <$> decodeAddress addr
                        `shouldBe` Right addr
