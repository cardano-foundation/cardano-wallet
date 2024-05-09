{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Launch.Cluster.Http.Faucet.APISpec
    ( spec
    )
where

import Prelude

import Cardano.Wallet.Launch.Cluster.Http.Faucet.SendFaucetAssets
    ( genSendFaucetAssets
    )
import Cardano.Wallet.Primitive.NetworkId
    ( NetworkDiscriminant (Mainnet)
    )
import Data.Aeson
    ( FromJSON (..)
    , Result (..)
    , ToJSON (..)
    , fromJSON
    )
import Data.OpenApi
    ( ToSchema
    , validateToJSON
    )
import Test.Hspec
    ( Expectation
    , Spec
    , describe
    , it
    , shouldBe
    )
import Test.QuickCheck
    ( forAll
    )

jsonRoundtrip :: (ToJSON a, FromJSON a, Eq a, Show a) => a -> IO ()
jsonRoundtrip a = fromJSON (toJSON a) `shouldBe` Success a

validate :: (ToJSON t, ToSchema t) => t -> Expectation
validate x = validateToJSON x `shouldBe` []

spec :: Spec
spec = do
    describe "/send/assets endpoint" $ do
        it "json response roundtrips"
            $ forAll
                (genSendFaucetAssets @Mainnet)
                jsonRoundtrip
        it "json response validates random data"
            $ forAll
                (genSendFaucetAssets @Mainnet)
                validate
