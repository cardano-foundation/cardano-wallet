{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-missing-local-signatures #-}

module Cardano.Wallet.Launch.Cluster.Faucet.SerializeSpec
    ( spec
    )
where

import Cardano.Wallet.Faucet.Gen.Address
    ( NetworkTag (..)
    )
import Cardano.Wallet.Launch.Cluster.Faucet.Gen
    ( genFaucetFunds
    )
import Cardano.Wallet.Launch.Cluster.Faucet.Serialize
    ( retrieveFunds
    , saveFunds
    )
import Cardano.Wallet.Launch.Cluster.FileOf
    ( FileOf (..)
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
import "extra" System.IO.Extra
    ( withTempFile
    )
import Prelude

spec :: Spec
spec = do
    describe "FaucetFunds serialization" $ do
        let roundTripTest tag =
                forAll (genFaucetFunds [tag])
                    $ \funds ->
                        withTempFile $ \fp -> do
                            let file = FileOf . absFile $ fp
                            saveFunds file funds
                            funds' <- retrieveFunds file
                            funds' `shouldBe` funds
        it "should roundtrip for Testnet addresses"
            $ roundTripTest TestnetTag
        it "should roundtrip for Mainnet addresses"
            $ roundTripTest MainnetTag
