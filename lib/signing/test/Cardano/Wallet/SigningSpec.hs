module Cardano.Wallet.SigningSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Signing
    ( sign
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    )

import qualified Data.ByteString as BS

spec :: Spec
spec = do
    describe "sign dummy" $ do
        it "golden 1" $
            sign `shouldBe` BS.empty
