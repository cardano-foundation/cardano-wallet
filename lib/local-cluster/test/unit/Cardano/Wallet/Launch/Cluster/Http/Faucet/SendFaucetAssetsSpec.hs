{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Launch.Cluster.Http.Faucet.SendFaucetAssetsSpec
    ( spec
    ) where

import Cardano.Wallet.Launch.Cluster.Http.Faucet.SendFaucetAssets
    ( genSendFaucetAssets
    )
import Cardano.Wallet.Primitive.NetworkId
    ( NetworkDiscriminant (..)
    )
import Data.Aeson
    ( FromJSON
    , Result (..)
    , ToJSON
    , fromJSON
    , toJSON
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
import Prelude

jsonRoundtrip :: (ToJSON a, FromJSON a, Eq a, Show a) => a -> IO ()
jsonRoundtrip a = fromJSON (toJSON a) `shouldBe` Success a

spec :: Spec
spec = do
    describe "SendFaucetAssets" $ do
        it "json instances roundtrips for Mainnet" $ do
            forAll (genSendFaucetAssets @Mainnet) jsonRoundtrip
        it "json instances roundtrips for Testnet" $ do
            forAll (genSendFaucetAssets @(Testnet 42)) jsonRoundtrip
