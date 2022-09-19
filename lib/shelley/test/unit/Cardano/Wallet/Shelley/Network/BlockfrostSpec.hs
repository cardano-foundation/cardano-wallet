module Cardano.Wallet.Shelley.Network.BlockfrostSpec (spec) where

import Prelude

import Blockfrost.Client
    ( TransactionMetaCBOR (..) )
import Cardano.Api
    ( AnyCardanoEra (..)
    , CardanoEra (..)
    , NetworkId (Mainnet)
    , TxMetadata (..)
    , TxMetadataValue (..)
    )
import Cardano.Wallet.Primitive.Types
    ( EpochNo )
import Cardano.Wallet.Shelley.Network.Blockfrost
    ( eraByEpoch, unmarshalMetadata )
import Data.Foldable
    ( for_ )
import Test.Hspec
    ( Spec, describe, it, shouldBe )

import qualified Data.Map as Map

spec :: Spec
spec = describe "Blockfrost Network" $ do
    it "determines era by epoch" $ do
        let epochEras :: [(EpochNo, AnyCardanoEra)]
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
                , (208, AnyCardanoEra ShelleyEra)
                , (207, AnyCardanoEra ByronEra)
                , (001, AnyCardanoEra ByronEra)
                , (000, AnyCardanoEra ByronEra)
                ]
        for_ epochEras $ \(epoch, era) ->
            eraByEpoch Mainnet epoch `shouldBe` Right era

    it "unmarshals metadata value" $ do
        let actualResult = unmarshalMetadata
                [ TransactionMetaCBOR "0" $
                    Just "A1006763617264616E6F"
                , TransactionMetaCBOR "1" $
                    Just "A1010E"
                , TransactionMetaCBOR "2" $
                    Just "A10244CAFEBABE"
                , TransactionMetaCBOR "3" $
                    Just "A103830E182A6431333337"
                , TransactionMetaCBOR "4" $
                    Just "A104A2636B65796576616C75650E182A"
                ]
        let expectedResult = Right $ TxMetadata $ Map.fromList
                [ (0, TxMetaText "cardano")
                , (1, TxMetaNumber 14)
                , (2, TxMetaBytes "\xca\xfe\xba\xbe")
                , (3, TxMetaList
                        [ TxMetaNumber 14
                        , TxMetaNumber 42
                        , TxMetaText "1337"
                        ]
                  )
                , (4, TxMetaMap
                        [ (TxMetaText "key", TxMetaText "value")
                        , (TxMetaNumber 14, TxMetaNumber 42)
                        ]
                  )
                ]

        actualResult `shouldBe` expectedResult

