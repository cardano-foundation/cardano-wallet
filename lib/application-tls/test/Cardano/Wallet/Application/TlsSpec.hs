{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Cardano.Wallet.Application.TlsSpec
    ( spec
    ) where

import Cardano.Wallet.Application.Tls
    ( TlsConfiguration (..)
    , clientManagerSettings
    , requireClientAuth
    )
import Cardano.X509.Configuration
    ( CertDescription (..)
    , ConfigurationKey (..)
    , DirConfiguration (..)
    , decodeConfigFile
    , fromConfiguration
    , genCertificate
    )
import Control.Monad
    ( unless
    , void
    )
import Data.ByteString.Lazy
    ( ByteString
    )
import Data.Default
    ( def
    )
import Data.Function
    ( (&)
    )
import Data.Streaming.Network
    ( bindRandomPortTCP
    )
import Data.X509.Extra
    ( encodePEM
    , genRSA256KeyPair
    )
import Network.HTTP.Client
    ( HttpException (..)
    , HttpExceptionContent (..)
    , Response
    , defaultManagerSettings
    , httpLbs
    , newManager
    , parseRequest
    , responseStatus
    )
import Network.HTTP.Client.TLS
    ( mkManagerSettings
    )
import Network.HTTP.Types.Status
    ( Status (..)
    )
import Network.Socket
    ( Socket
    , close
    )
import Network.Wai
    ( responseLBS
    )
import System.Directory
    ( createDirectoryIfMissing
    , doesDirectoryExist
    )
import System.FilePath
    ( takeFileName
    , (<.>)
    , (</>)
    )
import System.IO
    ( hPutStrLn
    , stderr
    )
import System.X509
    ( getSystemCertificateStore
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    , shouldThrow
    )
import Test.Utils.Paths
    ( getTestData
    )
import Test.Utils.Platform
    ( pendingOnWine
    )
import UnliftIO
    ( MonadIO (liftIO)
    , async
    , bracket
    , link
    )
import Prelude

import qualified Data.ByteString as BS
import qualified Network.HTTP.Types.Status as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WarpTLS as Warp

spec :: Spec
spec = describe "TLS Client Authentication" $ do
    it "Can create a TLS default manager" $ do
        void $ newManager defaultManagerSettings

    it "Check security program is available" $ do
        void getSystemCertificateStore

    it "Can create a TLS manager" $ do
        void $ newManager $ mkManagerSettings def Nothing

    it "Respond to authenticated client if TLS is enabled" $ do
        pendingOnWine "CertOpenSystemStoreW is failing under Wine"
        withListeningSocket "*" $ \(port, socket) -> do
            tlsSv <- rootPKI 1 "server"
            tlsCl <- rootPKI 1 "client"
            link =<< async (start tlsSv socket app)
            response <- pingHttps tlsCl port
            responseStatus response
                `shouldBe` Http.Status
                    { statusCode = 200
                    , statusMessage = "Ok"
                    }

    it "Deny client with wrong certificate if TLS is enabled" $ do
        pendingOnWine "CertOpenSystemStoreW is failing under Wine"
        withListeningSocket "*" $ \(port, socket) -> do
            tlsSv <- rootPKI 1 "server"
            tlsCl <- rootPKI 2 "client"
            link =<< async (start tlsSv socket app)
            pingHttps tlsCl port `shouldThrow` \case
                HttpExceptionRequest _ (InternalException _) -> True
                _ -> False

    it "Deny server with wrong certificate authority" $ do
        withListeningSocket "*" $ \(port, socket) -> do
            tlsSv <- rootPKI 2 "server"
            tlsCl <- rootPKI 2 "client"
            wrongCa <- rootPKI 1 "client"
            link =<< async (start tlsSv socket app)
            pingHttps tlsCl{tlsCaCert = tlsCaCert wrongCa} port
                `shouldThrow` \case
                    HttpExceptionRequest _ (InternalException _) -> True
                    _ -> False

    it "Deny server with a mismatched IP address" $ do
        withListeningSocket "*" $ \(port, socket) -> do
            tlsSv <- rootPKI 1 "server"
            tlsCl <- rootPKI 1 "client"
            link =<< async (start tlsSv socket app)
            pingHttpsHost tlsCl "127.0.0.2" port
                `shouldThrow` \case
                    HttpExceptionRequest _ (InternalException _) -> True
                    _ -> False

    it "Properly deny HTTP connection if TLS is enabled" $ do
        withListeningSocket "*" $ \(port, socket) -> do
            tlsSv <- rootPKI 1 "server"
            link =<< async (start tlsSv socket app)
            response <- pingHttp port
            responseStatus response
                `shouldBe` Http.Status
                    { statusCode = 426
                    , statusMessage = "Upgrade Required"
                    }

rootPKI :: Int -> FilePath -> IO TlsConfiguration
rootPKI i subdir = do
    let dir = $(getTestData) </> "PKIs" </> show i
    exists <- doesDirectoryExist dir
    unless exists $ do
        hPutStrLn stderr $ "rootPKI: There's no PKI for index #" <> show i
        genPKI dir
        hPutStrLn stderr $ "rootPKI: Created " <> dir
    pure
        TlsConfiguration
            { tlsCaCert = dir </> "ca.crt"
            , tlsSvCert = dir </> subdir </> subdir <.> "crt"
            , tlsSvKey = dir </> subdir </> subdir <.> "key"
            }

genPKI :: FilePath -> IO ()
genPKI dir = do
    cfg <- decodeConfigFile (ConfigurationKey "dev") confFile
    (caDesc, certDescs) <-
        fromConfiguration cfg dirConf genRSA256KeyPair <$> genRSA256KeyPair
    genCertificate (findCert "client" certDescs) >>= writePEM "client"
    genCertificate (findCert "server" certDescs) >>= writePEM "server"
    genCertificate caDesc >>= writeCert "ca"
  where
    dirConf =
        DirConfiguration
            { outDirServer = dir </> "server"
            , outDirClients = dir </> "client"
            , outDirCA = Just dir
            }
    confFile = $(getTestData) </> "PKIs" </> "cardano-sl-x509.yaml"
    writePEM f (key, cert) = do
        createDirectoryIfMissing True (dir </> f)
        let base = dir </> f </> f
        let cert' = encodePEM cert
        let key' = encodePEM key
        BS.writeFile (base <.> "crt") cert'
        BS.writeFile (base <.> "key") key'
        BS.writeFile (base <.> "pem") $ key' <> "\n" <> cert' <> "\n"
    writeCert f = BS.writeFile (dir </> f <.> "crt") . encodePEM . snd

    findCert outDir certs = case filter ((== outDir) . takeFileName . certOutDir) certs of
        (c : _) -> c
        [] -> error "findCert: no matching certificate found"

warpSettings :: Warp.Settings
warpSettings =
    Warp.defaultSettings
        -- NOTE By default, Warp prints any exception on stdout, which is kinda
        -- annoying...
        & Warp.setOnException (\_ _ -> pure ())

app :: Wai.Application
app _request respond =
    respond
        $ responseLBS Http.status200 [] "All your bases are belong to us!"

pingHttp :: Int -> IO (Response ByteString)
pingHttp port = do
    r <- parseRequest $ "GET http://127.0.0.1:" <> show port
    m <- newManager defaultManagerSettings
    httpLbs r m

pingHttps :: TlsConfiguration -> Int -> IO (Response ByteString)
pingHttps tls = pingHttpsHost tls "127.0.0.1"

pingHttpsHost
    :: TlsConfiguration -> String -> Int -> IO (Response ByteString)
pingHttpsHost tls host port = do
    r <- parseRequest $ "GET https://" <> host <> ":" <> show port
    m <- newManager =<< clientManagerSettings tls
    httpLbs r m

-- | Start the application server, using the given settings and a bound socket.
start
    :: TlsConfiguration
    -> Socket
    -> Wai.Application
    -> IO ()
start tls = Warp.runTLSSocket (requireClientAuth tls) warpSettings

-- | Run an action with a TCP socket bound to a random port
withListeningSocket
    :: Warp.HostPreference
    -- ^ Which host to bind.
    -> ((Warp.Port, Socket) -> IO a)
    -- ^ Action to run with listening socket.
    -> IO a
withListeningSocket hostPreference = bracket acquire release
  where
    acquire = bindAndListen
    -- Note: These Data.Streaming.Network functions also listen on the socket,
    -- even though their name just says "bind".
    bindAndListen = bindRandomPortTCP hostPreference
    release (_, socket) = liftIO $ close socket
