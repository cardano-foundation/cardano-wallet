{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Cardano.Wallet.DRep.WorkerSpec
    ( spec
    ) where

import Cardano.Pool.DB
    ( DBLayer (..)
    )
import Cardano.Wallet.DRep.Metadata
    ( defaultIpfsGatewayUrl
    )
import Cardano.Wallet.DRep.Worker
    ( runDRepMetadataCycle
    )
import Cardano.Wallet.DummyTarget.Primitive.Types
    ( dummyNetworkLayer
    , dummyTimeInterpreter
    )
import Cardano.Wallet.Network
    ( NetworkLayer (..)
    )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..)
    )
import Cardano.Wallet.Primitive.Types.DRep
    ( DRepAnchor (..)
    , DRepID (..)
    , DRepKeyHash (..)
    , DRepMetaReference (..)
    , DRepMetadata (..)
    , DRepRegistration (..)
    , encodeDRepIDBech32
    )
import Cryptography.Hash.Blake
    ( blake2b256
    )
import Data.ByteString
    ( ByteString
    )
import Data.Default
    ( def
    )
import Data.FileEmbed
    ( embedFile
    , makeRelativeToProject
    )
import Data.IORef
    ( IORef
    , newIORef
    , readIORef
    , writeIORef
    )
import Data.Streaming.Network
    ( bindRandomPortTCP
    )
import Data.Text
    ( Text
    )
import Network.Connection
    ( TLSSettings (..)
    )
import Network.HTTP.Client
    ( Manager
    , newManager
    )
import Network.HTTP.Client.TLS
    ( mkManagerSettings
    )
import Network.HTTP.Types.Status
    ( status200
    , status404
    )
import Network.Socket
    ( close
    )
import Network.Wai
    ( Application
    , responseLBS
    )
import System.FilePath
    ( (</>)
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    )
import UnliftIO
    ( async
    , bracket
    , cancel
    , link
    )
import UnliftIO.Temporary
    ( withSystemTempDirectory
    )
import Prelude

import qualified Cardano.Pool.DB.MVar as MVar
import qualified Data.ByteArray.Encoding as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.Set as Set
import qualified Data.Text.Encoding as T
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WarpTLS as WarpTLS

spec :: Spec
spec = describe "Cardano.Wallet.DRep.Worker" $ do
    it "fetches and stores hash-verified metadata over HTTPS"
        $ withMetadataFixture validMetadataBytes
        $ \Fixture{..} -> do
            runDRepMetadataCycle
                networkLayer
                db
                manager
                defaultIpfsGatewayUrl

            cached <- readMetadata db anchorHashHex
            cached `shouldBe` Just validMetadata

            mappedHash <- readAnchorHash db $ encodeDRepIDBech32 drepId
            mappedHash `shouldBe` Just anchorHashHex

    it "backs off after an HTTPS metadata hash mismatch"
        $ withMetadataFixture invalidMetadataBytes
        $ \Fixture{..} -> do
            runDRepMetadataCycle
                networkLayer
                db
                manager
                defaultIpfsGatewayUrl

            cachedAfterFailure <- readMetadata db anchorHashHex
            cachedAfterFailure `shouldBe` Nothing
            isFailed <- isRecentlyFailed db anchorHashHex
            isFailed `shouldBe` True

            writeIORef servedMetadata $ Just validMetadataBytes
            runDRepMetadataCycle
                networkLayer
                db
                manager
                defaultIpfsGatewayUrl

            cachedDuringBackoff <- readMetadata db anchorHashHex
            cachedDuringBackoff `shouldBe` Nothing

    it "does not cache hash-valid but invalid CIP-0119 metadata"
        $ withMetadataFixtureExpecting invalidCip0119Bytes invalidCip0119Bytes
        $ \Fixture{..} -> do
            runDRepMetadataCycle
                networkLayer
                db
                manager
                defaultIpfsGatewayUrl

            cached <- readMetadata db anchorHashHex
            cached `shouldBe` Nothing
            isFailed <- isRecentlyFailed db anchorHashHex
            isFailed `shouldBe` True

    it "records a missing HTTPS resource as a failed fetch"
        $ withMetadataFixture validMetadataBytes
        $ \Fixture{..} -> do
            writeIORef servedMetadata Nothing
            runDRepMetadataCycle
                networkLayer
                db
                manager
                defaultIpfsGatewayUrl

            cached <- readMetadata db anchorHashHex
            cached `shouldBe` Nothing
            isFailed <- isRecentlyFailed db anchorHashHex
            isFailed `shouldBe` True

data Fixture = Fixture
    { db :: DBLayer IO
    , manager :: Manager
    , networkLayer :: NetworkLayer IO ()
    , servedMetadata :: IORef (Maybe ByteString)
    , anchorHashHex :: Text
    , drepId :: DRepID
    }

withMetadataFixture
    :: ByteString
    -> (Fixture -> IO a)
    -> IO a
withMetadataFixture servedBytes =
    withMetadataFixtureExpecting servedBytes validMetadataBytes

withMetadataFixtureExpecting
    :: ByteString
    -> ByteString
    -> (Fixture -> IO a)
    -> IO a
withMetadataFixtureExpecting servedBytes expectedBytes action =
    withSystemTempDirectory "drep-metadata" $ \dir -> do
        let certificatePath = dir </> "server.crt"
            keyPath = dir </> "server.key"
        BS.writeFile certificatePath serverCertificate
        BS.writeFile keyPath serverKey
        servedMetadata <- newIORef $ Just servedBytes
        withHttpsServer certificatePath keyPath servedMetadata $ \baseUrl -> do
            manager <-
                newManager
                    $ mkManagerSettings
                        (TLSSettingsSimple True False False def)
                        Nothing
            db <- MVar.newDBLayer dummyTimeInterpreter
            let drepId =
                    DRepFromKeyHash
                        $ DRepKeyHash
                        $ BS.replicate 28 0x01
                anchorHash = blake2b256 expectedBytes
                anchorHashHex = hexBS anchorHash
                registration =
                    DRepRegistration
                        { drepRegId = drepId
                        , drepRegExpiryEpoch = 500
                        , drepRegAnchor =
                            Just
                                DRepAnchor
                                    { drepAnchorUrl =
                                        T.decodeUtf8
                                            $ B8.pack
                                            $ baseUrl <> "drep.json"
                                    , drepAnchorHash = anchorHash
                                    }
                        , drepRegDeposit = Coin 500_000_000
                        , drepRegVotingPower = Coin 1_000_000
                        , drepRegIsActive = True
                        }
                networkLayer =
                    dummyNetworkLayer
                        { listDReps = pure $ Just [registration]
                        }
            action Fixture{..}

withHttpsServer
    :: FilePath
    -> FilePath
    -> IORef (Maybe ByteString)
    -> (String -> IO a)
    -> IO a
withHttpsServer certificatePath keyPath servedMetadata action =
    bracket (bindRandomPortTCP "127.0.0.1") (close . snd)
        $ \(port, socket) ->
            bracket
                (async $ WarpTLS.runTLSSocket tlsSettings warpSettings socket app)
                cancel
                $ \server -> do
                    link server
                    action $ "https://localhost:" <> show port <> "/"
  where
    tlsSettings = WarpTLS.tlsSettings certificatePath keyPath
    warpSettings = Warp.setHost "127.0.0.1" Warp.defaultSettings
    app :: Application
    app _ respond = do
        readIORef servedMetadata >>= \case
            Just bytes ->
                respond
                    $ responseLBS
                        status200
                        [("Content-Type", "application/json")]
                        (BL.fromStrict bytes)
            Nothing -> respond $ responseLBS status404 [] ""

serverCertificate :: ByteString
serverCertificate =
    $( makeRelativeToProject
        "../application-tls/test/data/PKIs/1/server/server.crt"
        >>= embedFile
     )

serverKey :: ByteString
serverKey =
    $( makeRelativeToProject
        "../application-tls/test/data/PKIs/1/server/server.key"
        >>= embedFile
     )

validMetadataBytes :: ByteString
validMetadataBytes =
    "{\"body\":{\"givenName\":\"Alice\",\"objectives\":\"Promote decentralisation\",\"motivations\":\"Long-time community member\",\"qualifications\":\"10 years in DLT\",\"paymentAddress\":\"addr_test1\",\"doNotList\":false,\"references\":[{\"label\":\"Website\",\"uri\":\"https://alice.example.com\"}]}}"

invalidMetadataBytes :: ByteString
invalidMetadataBytes =
    "{\"body\":{\"givenName\":\"Mallory\",\"doNotList\":false}}"

invalidCip0119Bytes :: ByteString
invalidCip0119Bytes =
    "{\"body\":{\"doNotList\":false}}"

validMetadata :: DRepMetadata
validMetadata =
    DRepMetadata
        { drepMetaName = "Alice"
        , drepMetaObjectives = Just "Promote decentralisation"
        , drepMetaMotivations = Just "Long-time community member"
        , drepMetaQualifications = Just "10 years in DLT"
        , drepMetaPaymentAddress = Just "addr_test1"
        , drepMetaDoNotList = False
        , drepMetaReferences =
            [ DRepMetaReference
                { drepMetaRefLabel = "Website"
                , drepMetaRefUri = "https://alice.example.com"
                }
            ]
        }

hexBS :: ByteString -> Text
hexBS = T.decodeUtf8 . BA.convertToBase BA.Base16

readMetadata :: DBLayer IO -> Text -> IO (Maybe DRepMetadata)
readMetadata DBLayer{atomically, getDRepMetadata} hash =
    atomically $ getDRepMetadata hash

readAnchorHash :: DBLayer IO -> Text -> IO (Maybe Text)
readAnchorHash DBLayer{atomically, getDRepAnchorHash} drepId =
    atomically $ getDRepAnchorHash drepId

isRecentlyFailed :: DBLayer IO -> Text -> IO Bool
isRecentlyFailed DBLayer{atomically, recentlyFailedDRepHashes} hash =
    Set.member hash <$> atomically recentlyFailedDRepHashes
