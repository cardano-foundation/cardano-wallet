{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Optional TLS support for mutual client-server authentication on top of a Wai
-- application.
module Cardano.Wallet.Application.Tls
    ( TlsConfiguration (..)
    , clientManagerSettings
    , requireClientAuth
    ) where

import Data.Default
    ( Default (..)
    )
import Data.X509
    ( CertificateChain (..)
    , ExtKeyUsagePurpose (..)
    , HashALG (..)
    )
import Data.X509.CertificateStore
    ( makeCertificateStore
    )
import Data.X509.Extra
    ( validateDefaultWithIP
    )
import Data.X509.File
    ( readKeyFile
    , readSignedObject
    )
import Data.X509.Validation
    ( ValidationChecks (..)
    , ValidationHooks (..)
    )
import Network.HTTP.Client
    ( ManagerSettings
    )
import Network.HTTP.Client.TLS
    ( mkManagerSettings
    )
import Network.TLS
    ( CertificateRejectReason (..)
    , CertificateUsage (..)
    , ClientHooks (..)
    , ClientParams (..)
    , Credentials (..)
    , ServerHooks (..)
    , Shared (..)
    , Supported (..)
    , defaultParamsClient
    , noSessionManager
    )
import Network.TLS.Extra.Cipher
    ( ciphersuite_default
    )
import Network.Wai.Handler.WarpTLS
    ( TLSSettings (..)
    , tlsSettingsChain
    )
import Prelude

import qualified Data.X509.Validation as X509
import qualified Network.Connection as Connection

-- | Path to a x.509 PKI for mutual client-server authentication.
data TlsConfiguration = TlsConfiguration
    { tlsCaCert :: !FilePath
    , tlsSvCert :: !FilePath
    , tlsSvKey :: !FilePath
    }
    deriving (Show)

clientManagerSettings :: TlsConfiguration -> IO ManagerSettings
clientManagerSettings TlsConfiguration{tlsCaCert, tlsSvCert, tlsSvKey} = do
    credentials <- readCredentials tlsSvCert tlsSvKey
    caChain <- readSignedObject tlsCaCert
    pure
        $ mkManagerSettings
            (Connection.TLSSettings $ clientParams caChain credentials)
            Nothing
  where
    clientParams caChain credentials =
        (defaultParamsClient "127.0.0.1" "")
            { clientUseServerNameIndication = True
            , clientWantSessionResume = Nothing
            , clientShared =
                def
                    { sharedCredentials = Credentials [credentials]
                    , sharedCAStore = makeCertificateStore caChain
                    , sharedSessionManager = noSessionManager
                    }
            , clientHooks =
                def
                    { onCertificateRequest = const . pure . Just $ credentials
                    , onServerCertificate = validateDefaultWithIP
                    }
            , clientSupported = def{supportedCiphers = ciphersuite_default}
            }

    readCredentials certFile keyFile = do
        certs <- readSignedObject certFile
        readKeyFile keyFile >>= \case
            key : _ -> pure (CertificateChain certs, key)
            [] -> fail "TLS client key file is empty"

-- Create TLS settings for a Warp Handler from the given TLS configuration.
-- These settings will expect clients to provide a valid TLS certificate during
-- handshake. To be valid, a client certificate must:
--
-- - Have been signed by the same authority (CA).
-- - Have a 'Key Usage Purpose' set to 'Client'
requireClientAuth
    :: TlsConfiguration
    -> TLSSettings
requireClientAuth TlsConfiguration{tlsCaCert, tlsSvCert, tlsSvKey} =
    tlsSettings
        { tlsWantClientCert = True
        , tlsServerHooks =
            def
                { onClientCertificate =
                    fmap certificateUsageFromValidations . validateCertificate
                }
        }
  where
    tlsSettings =
        tlsSettingsChain tlsSvCert [tlsCaCert] tlsSvKey

    -- NOTE
    -- This checks makes sense only for remote services, to validate that the
    -- fully qualified hostname from the certificate matches the one from the
    -- service we're trying to reach. This is of little use for a server
    -- validation.
    serviceID =
        ("", "")

    certificateUsageFromValidations =
        maybe
            CertificateUsageAccept
            (CertificateUsageReject . CertificateRejectOther)

    -- By default, X509.Validation validates the certificate names against the host
    -- which is irrelevant when checking the client certificate (but relevant for
    -- the client when checking the server's certificate).
    hooks =
        def
            { hookValidateName = \_ _ -> []
            }

    -- Here we add extra checks as the ones performed by default to enforce that
    -- the client certificate is actually _meant_ to be used for client auth.
    -- This should prevent server certificates to be used to authenticate
    -- against the server.
    checks =
        def
            { checkStrictOrdering = True
            , checkLeafKeyPurpose = [KeyUsagePurpose_ClientAuth]
            }

    -- This solely verify that the provided certificate is valid and was signed by authority we
    -- recognize (tpCaPath).
    -- Note: we use readSignedObject + makeCertificateStore rather than
    -- readCertificateStore because the latter fails on Windows when
    -- given a single PEM file (it calls CertOpenSystemStore internally).
    validateCertificate cert = do
        caCerts <- readSignedObject tlsCaCert
        let store = makeCertificateStore caCerts
        fromX509FailedReasons
            <$> X509.validate HashSHA256 hooks checks store def serviceID cert

    fromX509FailedReasons reasons =
        case reasons of
            [] -> Nothing
            _ -> Just (show reasons)
