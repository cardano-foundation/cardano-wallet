{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Cardano.Wallet.DRep.WorkerSpec
    ( spec
    ) where

import Cardano.Pool.DB
    ( DBLayer (..)
    )
import Cardano.Pool.DB.MVar
    ( newDBLayer
    )
import Cardano.Wallet.DRep.Worker
    ( WorkerConfig (..)
    , defaultWorkerConfig
    , runOneCycle
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
    , DRepMetadata (..)
    , DRepRegistration (..)
    )
import Control.Concurrent
    ( threadDelay
    )
import Cryptography.Hash.Blake
    ( blake2b256
    )
import Data.Default
    ( def
    )
import Data.FileEmbed
    ( embedFile
    , makeRelativeToProject
    )
import Data.Text
    ( Text
    )
import Data.Time.Clock.POSIX
    ( POSIXTime
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
import System.IO.Temp
    ( withSystemTempDirectory
    )
import Test.Hspec
    ( Spec
    , SpecWith
    , around
    , describe
    , it
    , shouldBe
    , shouldSatisfy
    )
import UnliftIO
    ( async
    , bracket
    , cancel
    , link
    )
import Prelude

import qualified Data.ByteArray.Encoding as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Streaming.Network as SN
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WarpTLS as WarpTLS

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = describe "Cardano.Wallet.DRep.Worker" $ do
    describe "runOneCycle (logic)" $ around withLogicServer logicTests
    describe "runOneCycle (HTTPS)" $ around withHttpsIntegrationServer httpsTests

-- ---------------------------------------------------------------------------
-- Logic tests — driven by a real local HTTPS server
-- ---------------------------------------------------------------------------

logicTests :: SpecWith (Int, Manager)
logicTests = do
    it "fetches metadata for DReps with unfetched anchors" $ \(port, mgr) -> do
        db <- newDBLayer dummyTimeInterpreter
        let url = mkUrl port "/valid.json"
        let anchor = DRepAnchor{drepAnchorUrl = url, drepAnchorHash = validJsonHash}
        let nl = dummyNetworkLayer{listDReps = pure (Just [mkReg 1 (Just anchor)])}
        runOneCycle nl db mgr (testCfg 0)
        case db of
            DBLayer{atomically, getAllDRepMetadata} -> do
                meta <- atomically getAllDRepMetadata
                Map.size meta `shouldBe` 1

    it "skips DReps with no anchor" $ \(_, mgr) -> do
        db <- newDBLayer dummyTimeInterpreter
        let nl = dummyNetworkLayer{listDReps = pure (Just [mkReg 1 Nothing])}
        runOneCycle nl db mgr (testCfg 0)
        case db of
            DBLayer{atomically, getAllDRepMetadata} -> do
                meta <- atomically getAllDRepMetadata
                Map.size meta `shouldBe` 0

    it "skips DReps whose anchor is already cached" $ \(port, mgr) -> do
        db <- newDBLayer dummyTimeInterpreter
        let url = mkUrl port "/valid.json"
        let anchor = DRepAnchor{drepAnchorUrl = url, drepAnchorHash = validJsonHash}
        let hexH = hexBS validJsonHash
        let nl = dummyNetworkLayer{listDReps = pure (Just [mkReg 1 (Just anchor)])}
        case db of
            DBLayer{atomically, putDRepMetadata} ->
                atomically $ putDRepMetadata hexH testMeta
        runOneCycle nl db mgr (testCfg 0)
        case db of
            DBLayer{atomically, getAllDRepMetadata} -> do
                meta <- atomically getAllDRepMetadata
                Map.size meta `shouldBe` 1

    it "records a fetch attempt on hash mismatch" $ \(port, mgr) -> do
        db <- newDBLayer dummyTimeInterpreter
        let url = mkUrl port "/valid.json"
        let wrongHash = BS.replicate 32 0xFF
        let anchor = DRepAnchor{drepAnchorUrl = url, drepAnchorHash = wrongHash}
        let nl = dummyNetworkLayer{listDReps = pure (Just [mkReg 1 (Just anchor)])}
        runOneCycle nl db mgr (testCfg 0)
        case db of
            DBLayer{atomically, getAllDRepMetadata, recentlyFailedDRepHashes} -> do
                meta <- atomically getAllDRepMetadata
                Map.size meta `shouldBe` 0
                failed <- atomically recentlyFailedDRepHashes
                Set.size failed `shouldBe` 1

    it "records a fetch attempt on HTTP error (404)" $ \(port, mgr) -> do
        db <- newDBLayer dummyTimeInterpreter
        let url = mkUrl port "/not-found"
        let anchor =
                DRepAnchor
                    { drepAnchorUrl = url
                    , drepAnchorHash = BS.replicate 32 0x00
                    }
        let nl = dummyNetworkLayer{listDReps = pure (Just [mkReg 1 (Just anchor)])}
        runOneCycle nl db mgr (testCfg 0)
        case db of
            DBLayer{atomically, getAllDRepMetadata, recentlyFailedDRepHashes} -> do
                meta <- atomically getAllDRepMetadata
                Map.size meta `shouldBe` 0
                failed <- atomically recentlyFailedDRepHashes
                Set.size failed `shouldBe` 1

    it "records a fetch attempt when fetch times out" $ \(port, mgr) -> do
        db <- newDBLayer dummyTimeInterpreter
        let url = mkUrl port "/slow"
        let anchor =
                DRepAnchor
                    { drepAnchorUrl = url
                    , drepAnchorHash = BS.replicate 32 0x00
                    }
        let nl = dummyNetworkLayer{listDReps = pure (Just [mkReg 1 (Just anchor)])}
        let timeoutCfg = (testCfg 0){workerFetchTimeoutMicros = 1}
        runOneCycle nl db mgr timeoutCfg
        case db of
            DBLayer{atomically, getAllDRepMetadata, recentlyFailedDRepHashes} -> do
                meta <- atomically getAllDRepMetadata
                Map.size meta `shouldBe` 0
                failed <- atomically recentlyFailedDRepHashes
                Set.size failed `shouldBe` 1

    it "skips DReps in the backoff window" $ \(port, mgr) -> do
        db <- newDBLayer dummyTimeInterpreter
        let url = mkUrl port "/valid.json"
        let anchor = DRepAnchor{drepAnchorUrl = url, drepAnchorHash = validJsonHash}
        let hexH = hexBS validJsonHash
        let nl = dummyNetworkLayer{listDReps = pure (Just [mkReg 1 (Just anchor)])}
        case db of
            DBLayer{atomically, putDRepFetchAttempt} ->
                atomically $ putDRepFetchAttempt (url, hexH)
        runOneCycle nl db mgr (testCfg 0)
        case db of
            DBLayer{atomically, getAllDRepMetadata} -> do
                meta <- atomically getAllDRepMetadata
                Map.size meta `shouldBe` 0

    it "triggers GC when interval has elapsed" $ \(port, mgr) -> do
        db <- newDBLayer dummyTimeInterpreter
        let url = mkUrl port "/valid.json"
        let anchor = DRepAnchor{drepAnchorUrl = url, drepAnchorHash = validJsonHash}
        let nl = dummyNetworkLayer{listDReps = pure (Just [mkReg 1 (Just anchor)])}
        case db of
            DBLayer{atomically, putDRepMetadata} ->
                atomically $ putDRepMetadata "orphan" testMeta
        let gcCfg = (testCfg 0){workerGCIntervalSeconds = 0}
        runOneCycle nl db mgr gcCfg
        case db of
            DBLayer{atomically, readLastDRepMetadataGC} -> do
                mGCTime <- atomically readLastDRepMetadataGC
                mGCTime `shouldSatisfy` (Nothing /=)

    it "does not trigger GC before interval" $ \(_, mgr) -> do
        db <- newDBLayer dummyTimeInterpreter
        let nl = dummyNetworkLayer{listDReps = pure (Just [])}
        let now = 1_000_000
        case db of
            DBLayer{atomically, putLastDRepMetadataGC} ->
                atomically $ putLastDRepMetadataGC now
        let noGcCfg = (testCfg now){workerGCIntervalSeconds = 3600}
        runOneCycle nl db mgr noGcCfg
        case db of
            DBLayer{atomically, readLastDRepMetadataGC} -> do
                mGCTime <- atomically readLastDRepMetadataGC
                mGCTime `shouldBe` Just now

-- ---------------------------------------------------------------------------
-- HTTPS integration tests
-- ---------------------------------------------------------------------------

httpsTests :: SpecWith (Int, Manager)
httpsTests = do
    it "successfully fetches and stores CIP-0119 metadata from a real HTTPS server"
        $ \(port, mgr) -> do
            db <- newDBLayer dummyTimeInterpreter
            let url = mkUrl port "/valid.json"
            let anchor =
                    DRepAnchor
                        { drepAnchorUrl = url
                        , drepAnchorHash = validJsonHash
                        }
            let nl = dummyNetworkLayer{listDReps = pure (Just [mkReg 1 (Just anchor)])}
            runOneCycle
                nl
                db
                mgr
                (defaultWorkerConfig "https://ipfs.example.com/ipfs/" 0)
            case db of
                DBLayer{atomically, getAllDRepMetadata} -> do
                    meta <- atomically getAllDRepMetadata
                    Map.size meta `shouldBe` 1

    it "records a fetch attempt when the server returns 404" $ \(port, mgr) -> do
        db <- newDBLayer dummyTimeInterpreter
        let url = mkUrl port "/missing"
        let anchor =
                DRepAnchor
                    { drepAnchorUrl = url
                    , drepAnchorHash = BS.replicate 32 0x00
                    }
        let nl = dummyNetworkLayer{listDReps = pure (Just [mkReg 1 (Just anchor)])}
        runOneCycle
            nl
            db
            mgr
            (defaultWorkerConfig "https://ipfs.example.com/ipfs/" 0)
        case db of
            DBLayer{atomically, recentlyFailedDRepHashes} -> do
                failed <- atomically recentlyFailedDRepHashes
                Set.size failed `shouldBe` 1

    it "records a fetch attempt when the hash does not match" $ \(port, mgr) -> do
        db <- newDBLayer dummyTimeInterpreter
        let url = mkUrl port "/valid.json"
        let wrongHash = BS.replicate 32 0xAB
        let anchor =
                DRepAnchor
                    { drepAnchorUrl = url
                    , drepAnchorHash = wrongHash
                    }
        let nl = dummyNetworkLayer{listDReps = pure (Just [mkReg 1 (Just anchor)])}
        runOneCycle
            nl
            db
            mgr
            (defaultWorkerConfig "https://ipfs.example.com/ipfs/" 0)
        case db of
            DBLayer{atomically, recentlyFailedDRepHashes} -> do
                failed <- atomically recentlyFailedDRepHashes
                Set.size failed `shouldBe` 1

-- ---------------------------------------------------------------------------
-- Server setup
-- ---------------------------------------------------------------------------

-- | Build an HTTPS URL pointing to the local test server.
mkUrl :: Int -> Text -> Text
mkUrl port path = "https://127.0.0.1:" <> T.pack (show port) <> path

-- | Set up a local HTTPS server for logic tests.
withLogicServer :: ((Int, Manager) -> IO ()) -> IO ()
withLogicServer action =
    withTempCerts $ \certPath keyPath -> do
        mgr <- insecureManager
        withHttpsServer certPath keyPath logicApp $ \port ->
            action (port, mgr)

-- | Set up a local HTTPS integration server for httpsTests.
withHttpsIntegrationServer :: ((Int, Manager) -> IO ()) -> IO ()
withHttpsIntegrationServer action =
    withTempCerts $ \certPath keyPath -> do
        mgr <- insecureManager
        withHttpsServer certPath keyPath httpsApp $ \port ->
            action (port, mgr)

-- | Start a warp-tls server on a random port, run the action, then stop.
withHttpsServer :: FilePath -> FilePath -> Application -> (Int -> IO ()) -> IO ()
withHttpsServer certPath keyPath app action =
    bracket (SN.bindRandomPortTCP "127.0.0.1") (close . snd) $ \(port, sock) ->
        bracket
            ( async
                $ WarpTLS.runTLSSocket
                    (WarpTLS.tlsSettings certPath keyPath)
                    (Warp.setHost "127.0.0.1" Warp.defaultSettings)
                    sock
                    app
            )
            cancel
            $ \server -> do
                link server
                threadDelay 50_000
                action port

-- | WAI application used for logic tests.
logicApp :: Application
logicApp req respond = case Wai.rawPathInfo req of
    "/valid.json" ->
        respond
            $ responseLBS
                status200
                [("Content-Type", "application/json")]
                (LBS.fromStrict validJsonBody)
    "/not-found" ->
        respond $ responseLBS status404 [] "Not Found"
    "/slow" -> do
        threadDelay 10_000_000
        respond $ responseLBS status200 [] "{\"givenName\":\"Slow\"}"
    _ ->
        respond $ responseLBS status404 [] "Not Found"

-- | WAI application used for HTTPS integration tests.
httpsApp :: Application
httpsApp req respond = case Wai.rawPathInfo req of
    "/valid.json" ->
        respond
            $ responseLBS
                status200
                [("Content-Type", "application/json")]
                (LBS.fromStrict validJsonBody)
    _ ->
        respond $ responseLBS status404 [] "Not Found"

-- ---------------------------------------------------------------------------
-- TLS infrastructure
-- ---------------------------------------------------------------------------

-- | Write embedded test cert and key to temp files, run action with paths.
withTempCerts :: (FilePath -> FilePath -> IO a) -> IO a
withTempCerts action =
    withSystemTempDirectory "drep-worker-test" $ \dir -> do
        let certPath = dir <> "/server.crt"
            keyPath = dir <> "/server.key"
        BS.writeFile certPath serverCertBytes
        BS.writeFile keyPath serverKeyBytes
        action certPath keyPath

-- | Build an HTTP 'Manager' that skips TLS certificate validation.
-- Used only in tests; never in production code.
insecureManager :: IO Manager
insecureManager =
    newManager
        $ mkManagerSettings
            (TLSSettingsSimple True False False def)
            Nothing

-- | Embedded TLS server certificate (from test PKI).
serverCertBytes :: BS.ByteString
serverCertBytes =
    $( makeRelativeToProject "test/data/PKIs/1/server/server.crt"
        >>= embedFile
     )

-- | Embedded TLS server private key (from test PKI).
serverKeyBytes :: BS.ByteString
serverKeyBytes =
    $( makeRelativeToProject "test/data/PKIs/1/server/server.key"
        >>= embedFile
     )

-- ---------------------------------------------------------------------------
-- Test data
-- ---------------------------------------------------------------------------

-- | A minimal valid CIP-0119 JSON body.
validJsonBody :: BS.ByteString
validJsonBody = "{\"givenName\":\"Test DRep\"}"

-- | The Blake2b-256 hash of 'validJsonBody'.
validJsonHash :: BS.ByteString
validJsonHash = blake2b256 validJsonBody

-- | Hex-encode a ByteString.
hexBS :: BS.ByteString -> Text
hexBS = T.decodeUtf8 . BA.convertToBase BA.Base16

-- | Build a minimal 'DRepRegistration' with the given anchor.
mkReg :: Int -> Maybe DRepAnchor -> DRepRegistration
mkReg n anchor =
    DRepRegistration
        { drepRegId = DRepFromKeyHash (DRepKeyHash (BS.replicate 28 (fromIntegral n)))
        , drepRegExpiryEpoch = 500
        , drepRegAnchor = anchor
        , drepRegDeposit = Coin 500_000_000
        , drepRegVotingPower = Coin 0
        , drepRegIsActive = True
        }

-- | A 'WorkerConfig' for tests: 5-second fetch timeout and instant GC.
testCfg :: POSIXTime -> WorkerConfig
testCfg now =
    ( defaultWorkerConfig "https://ipfs.example.com/ipfs/" 1_000_000
    )
        { workerFetchTimeoutMicros = 5_000_000
        , workerGCIntervalSeconds = 0
        , workerGetTime = pure now
        }

-- | A minimal 'DRepMetadata' fixture.
testMeta :: DRepMetadata
testMeta =
    DRepMetadata
        { drepMetaName = "Test DRep"
        , drepMetaObjectives = Nothing
        , drepMetaMotivations = Nothing
        , drepMetaQualifications = Nothing
        , drepMetaPaymentAddress = Nothing
        , drepMetaDoNotList = False
        , drepMetaReferences = []
        }
