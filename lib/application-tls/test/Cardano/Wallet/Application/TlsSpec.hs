{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Cardano.Wallet.Application.TlsSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Application.Tls
    ( TlsConfiguration (..)
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
import Data.X509
    ( CertificateChain (..)
    )
import Data.X509.CertificateStore
    ( makeCertificateStore
    )
import Data.X509.Extra
    ( encodePEM
    , genRSA256KeyPair
    )
import Data.X509.File
    ( readKeyFile
    , readSignedObject
    )
import Network.Connection
    ( TLSSettings (..)
    )
import Network.HTTP.Client
    ( HttpException (..)
    , HttpExceptionContent (..)
    , ManagerSettings (..)
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
import Network.TLS
    ( ClientHooks (..)
    , ClientParams (..)
    , Credentials (..)
    , Shared (..)
    , Supported (..)
    , defaultParamsClient
    , noSessionManager
    )
import Network.TLS.Extra.Cipher
    ( ciphersuite_default
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

import qualified Data.ByteString as BS
import qualified Network.HTTP.Types.Status as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WarpTLS as Warp

spec :: Spec
spec = describe "TLS Client Authentication" $ do
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

    findCert outDir = head . filter ((== outDir) . takeFileName . certOutDir)

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
pingHttps tls port = do
    r <- parseRequest $ "GET https://127.0.0.1:" <> show port
    m <- newManager =<< mkHttpsManagerSettings tls
    httpLbs r m

-- | Construct a 'ManagerSettings' for a client application using the given TLS
-- configuration. The 'TlsConfiguration' is slightly _abused_ here as the
-- @tlsSvCert@ and @tlsSvKey@ are actually pointing to **client** credentials.
mkHttpsManagerSettings
    :: TlsConfiguration
    -> IO ManagerSettings
mkHttpsManagerSettings TlsConfiguration{tlsCaCert, tlsSvCert, tlsSvKey} = do
    params <-
        clientParams
            <$> readSignedObject tlsCaCert
            <*> readCredentials tlsSvCert tlsSvKey
    pure $ mkManagerSettings (TLSSettings params) sockSettings
  where
    sockSettings = Nothing
    clientParams caChain credentials =
        (defaultParamsClient "127.0.0.1" "")
            { clientUseMaxFragmentLength = Nothing
            , clientUseServerNameIndication = True
            , clientWantSessionResume = Nothing
            , clientShared = clientShared caChain credentials
            , clientHooks = clientHooks credentials
            , clientSupported = clientSupported
            }

    clientShared caChain credentials =
        def
            { sharedCredentials = Credentials [credentials]
            , sharedCAStore = makeCertificateStore caChain
            , sharedSessionManager = noSessionManager
            }

    clientHooks credentials =
        def
            { onCertificateRequest = const . return . Just $ credentials
            , onServerCertificate = \_ _ _ _ -> pure []
            }

    clientSupported =
        def
            { supportedCiphers = ciphersuite_default
            }

    readCredentials certFile keyFile =
        (,) . CertificateChain
            <$> readSignedObject certFile
            <*> (head <$> readKeyFile keyFile)

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
