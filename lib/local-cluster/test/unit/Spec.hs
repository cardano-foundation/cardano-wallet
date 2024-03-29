module Spec where

import Prelude

import Cardano.Wallet.Faucet.Gen
    ( genFaucetFunds
    )
import Cardano.Wallet.Faucet.Yaml
    ( retrieveFunds
    , saveFunds
    )
import Cardano.Wallet.Launch.Cluster.FileOf
    ( FileOf (..)
    )
import System.IO.Temp
    ( withSystemTempFile
    )
import System.Path
    ( absFile
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    )
import Test.QuickCheck
    ( forAll
    )

spec :: Spec
spec = do
    describe "FaucetFunds serialization" $ do
        it "should roundtrip" $ forAll genFaucetFunds $ \funds ->
            withSystemTempFile "funds" $ \fp _h -> do
                let file = FileOf . absFile $ fp
                saveFunds file funds
                funds' <- retrieveFunds file
                funds' `shouldBe` funds
