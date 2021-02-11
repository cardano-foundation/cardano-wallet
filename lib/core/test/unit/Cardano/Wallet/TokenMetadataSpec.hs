{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.TokenMetadataSpec where

import Prelude

import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( AssetId (..) )
import Cardano.Wallet.Primitive.Types.TokenPolicy
    ( AssetLogo (..)
    , AssetMetadata (..)
    , AssetURL (..)
    , AssetUnit (..)
    , TokenPolicyId (..)
    , nullTokenName
    )
import Cardano.Wallet.TokenMetadata
import Cardano.Wallet.TokenMetadata.MockServer
    ( assetIdFromSubject, queryServerStatic, withMetadataServer )
import Cardano.Wallet.Unsafe
    ( unsafeFromBase64, unsafeFromHex, unsafeFromText )
import Control.Tracer
    ( nullTracer )
import Data.Aeson
    ( eitherDecodeFileStrict )
import Network.URI
    ( parseURI )
import System.FilePath
    ( (</>) )
import Test.Hspec
    ( Spec, describe, it, shouldBe, shouldReturn )
import Test.Utils.Paths
    ( getTestData )

spec :: Spec
spec = describe "Token Metadata" $ do
    describe "JSON decoding" $ do
        it "golden1.json" $
            eitherDecodeFileStrict golden1File
                `shouldReturn` Right (BatchResponse golden1Properties)

        it "metadataFromProperties" $
            map metadataFromProperties golden1Properties
                `shouldBe` (Just <$> [golden1Metadata0,golden1Metadata1,golden1Metadata2])

    describe "Mock server" $ do
        it "testing empty req" $
            withMetadataServer (queryServerStatic golden1File) $ \srv -> do
                client <- newMetadataClient nullTracer (Just srv)
                getTokenMetadata client [] `shouldReturn` Right []
        it "golden1.json" $
            withMetadataServer (queryServerStatic golden1File) $ \srv -> do
                client <- newMetadataClient nullTracer (Just srv)
                let subj = "7f71940915ea5fe85e840f843c929eba467e6f050475bad1f10b9c27"
                let aid = AssetId (UnsafeTokenPolicyId (unsafeFromText subj)) nullTokenName
                getTokenMetadata client [assetIdFromSubject (Subject subj)]
                    `shouldReturn` Right [(aid, golden1Metadata0)]
        it "ill-formatted entry doesn't make the entire response fail to parse" $ do
            withMetadataServer (queryServerStatic golden2File) $ \srv -> do
                client <- newMetadataClient nullTracer (Just srv)
                let aid subj  =  AssetId (UnsafeTokenPolicyId (unsafeFromText subj)) nullTokenName
                let aid1 = aid "7f71940915ea5fe85e840f843c929eba467e6f050475bad1f10b9c27"
                let aid2 = aid "bad00000000000000000000000000000000000000000000000000000"
                getTokenMetadata client [aid1, aid2 ]
                    `shouldReturn` Right [(aid1, golden1Metadata0)]
        it "missing subject" $
            withMetadataServer (queryServerStatic golden1File) $ \srv -> do
                client <- newMetadataClient nullTracer (Just srv)
                let aid = AssetId (UnsafeTokenPolicyId (Hash "a")) nullTokenName
                res <- getTokenMetadata client [aid]
                res `shouldBe` Right []

  where
    dir = $(getTestData) </> "Cardano" </> "Wallet" </> "TokenMetadata"

    golden1File = dir </> "golden1.json"
    golden1Metadata0 =
        AssetMetadata "SteveToken" "A sample description"
            Nothing Nothing Nothing Nothing
    golden1Metadata1 =
        AssetMetadata "Token1" "description1"
            Nothing Nothing Nothing Nothing
    golden1Metadata2 =
        AssetMetadata "Token2" "description2"
            (Just "acr2")
            (AssetURL <$> parseURI "https://iohk.io")
            (Just $ AssetLogo $ unsafeFromBase64 "QWxtb3N0IGEgbG9nbw==")
            (Just $ AssetUnit "unit2" 14)
    golden2File = dir </> "golden2.json"

    sig s k = Signature (unsafeFromHex s) (unsafeFromHex k)

    golden1Properties =
        [ SubjectProperties
            { subject = "7f71940915ea5fe85e840f843c929eba467e6f050475bad1f10b9c27"
            , owner = Just $ sig "62e800b8c540b218396174f9c42fc253ab461961e20a4cc8ed4ba8b3fdff760cf8422e80d2504829a1d84458093880f02629524416f895b802cb9211f5145808" "25912b3081c20782aaa576af51ef3b17d7370d9fdf6641fec28012678ac1d179"
            , properties =
                ( Just $ Property (Right "SteveToken")
                    [ sig "7ef6ed44ba9456737ef8d2e31596fdafb66d5775ac1a254086a553b666516e5895bb0c6b7ba8bef1f6b4d9bd9253b4449d1354de2f9e043ea4eb43fd42f87108" "0ee262f062528667964782777917cd7139e19e8eb2c591767629e4200070c661"
                    , sig "c95cf87b74d1e4d3b413c927c65de836f0905ba2cd176c7cbff83d8b886b30fe1560c542c1f77bb88280dff55c2d267c9840fe36560fb13ba4a78b6429e51500" "7c3bfe2a11290a9b6ea054b4d0932678f88130511cfbfe3f634ee77d71edebe7"
                    , sig "f88692b13212bac8121151a99a4de4d5244e5f63566babd2b8ac20950ede74073af0570772b3ce3d11b72e972079199f02306e947cd5fcca688a9d4664eddb04" "8899d0777f399fffd44f72c85a8aa51605123a7ebf20bba42650780a0c81096a"
                    , sig "c2b30fa5f2c09323d81e5050af681c023089d832d0b85d05f60f4278fba3011ab03e6bd9bd2b8649080a368ecfe51573cd232efe8f1e7ca69ff8334ced7b6801" "d40688a3eeda1f229c64efc56dd53b363ff981f71a7462f78c8cc444117a03db"
                    ]
                , Just $ Property (Right "A sample description")
                    [ sig "83ef5c04882e43e5f1c8e9bc386bd51cdda163f5cbd1996d1d066238de063d4b79b1648b48aec63dddff05649911ca116579842c8e9a08a3bc7ae1a0ec7ef000" "1446c9d327b0f07aa691014c08578867674f3a88b36f2017a58c37a8a7799058"
                    , sig "4e29a00feaeb24b25315f0eac28bbfc550dabfb847bf6a06cb8086120201f90c64fab778037d0ef009ab4669121a38fe9b8c0a6aec99c68366c5187c0889520a" "1910312a9a6998c7e4f585dc138f85a90f50a28397b8ea05eb23355fb8ea4fa0"
                    , sig "ce939acca5677bc6d436bd8f054ed8fb03d143e0a9792c1f58592c43f175e89bb72d4d7114c1474b86e0d8fbf7807f4506325b56fcc6b87b2cb7002872527106" "4c5bbbbe7caaa18372aa8edc1ef2d2a770d18a5c2d142b9d695619c3365dd297"
                    , sig "5a1d55048234d92057dfd1938f49935a33751ee604b7dbd02a315418ced6f0836a51107512b192eae6133403bb437c6850b1af1c62c3b17a372acce77adf9903" "57fa73123c3b39489c4d6c2ff3cab9952e56e556daab9f8f333bc5ca6984fa5e"
                    , sig "e13c9ba5b084dc126d34f3f1120fff75495b64a41a98a69071b5c5ed01bb9d273f51d570cf4fdaa42969fa2c775c12ec05c496cd8f61323d343970136781f60e" "8cc8963b65ddd0a49f7ce1acc2915d8baff505bbc4f8727a22bd1d28f8ad6632"
                    ]
                , Nothing
                , Nothing
                , Nothing
                , Nothing
                )
           }
        , SubjectProperties
            { subject = "missing sigs"
            , owner = Nothing
            , properties =
                ( Just $ Property (Right "Token1") []
                , Just $ Property (Right "description1") []
                , Nothing
                , Nothing
                , Nothing
                , Nothing
                )
            }
        , SubjectProperties
            { subject = "extra fields"
            , owner = Nothing
            , properties =
                ( Just $ Property (Right "Token2") []
                , Just $ Property (Right "description2") []
                , Just $ Property (Right "acr2") []
                , Just $ Property (parseAssetURL "https://iohk.io") []
                , Just $ Property (Right (AssetLogo $ unsafeFromBase64 "QWxtb3N0IGEgbG9nbw==")) []
                , Just $ Property (Right (AssetUnit "unit2" 14)) []
                )
           }
        ]

parseAssetURL :: String -> Either String AssetURL
parseAssetURL = maybe (Left "tests error") (Right . AssetURL) . parseURI
