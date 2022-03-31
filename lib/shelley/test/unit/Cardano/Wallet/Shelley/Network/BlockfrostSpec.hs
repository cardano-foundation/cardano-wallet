module Cardano.Wallet.Shelley.Network.BlockfrostSpec (spec) where

import Prelude

import Cardano.Api
    ( AnyCardanoEra (..), CardanoEra (..) )
import Cardano.Wallet.Primitive.Types
    ( EpochNo )
import Cardano.Wallet.Shelley.Network.Blockfrost
    ( eraByEpoch )
import Data.Foldable
    ( for_ )
import Test.Hspec
    ( Spec, describe, it, shouldBe )

spec :: Spec
spec = describe "Blockfrost Network" $ do
    it "determines era by epoch" $ do
        for_ epochEras $ \(epoch, era) ->
            eraByEpoch epoch `shouldBe` Right era
  where
    epochEras :: [(EpochNo, AnyCardanoEra)]
    epochEras =
        [ (329, AnyCardanoEra AlonzoEra)
        , (298, AnyCardanoEra AlonzoEra)
        , (297, AnyCardanoEra AlonzoEra)
        , (295, AnyCardanoEra AlonzoEra)
        , (290, AnyCardanoEra AlonzoEra)
        , (289, AnyCardanoEra MaryEra)
        , (260, AnyCardanoEra MaryEra)
        , (251, AnyCardanoEra MaryEra)
        , (250, AnyCardanoEra AllegraEra)
        , (240, AnyCardanoEra AllegraEra)
        , (236, AnyCardanoEra AllegraEra)
        , (235, AnyCardanoEra ShelleyEra)
        , (220, AnyCardanoEra ShelleyEra)
        , (202, AnyCardanoEra ShelleyEra)
        , (201, AnyCardanoEra ByronEra)
        , (001, AnyCardanoEra ByronEra)
        , (000, AnyCardanoEra ByronEra)
        ]
