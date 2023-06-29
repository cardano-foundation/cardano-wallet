{-# LANGUAGE NamedFieldPuns #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Optional TLS support for mutual client-server authentication on top of a Wai
-- application.
module Cardano.Wallet.Api.Http.Server.Tls
    ( TlsConfiguration (..)
    , requireClientAuth
    ) where

import Prelude

import Data.Default
    ( Default (..)
    )
import Data.X509
    ( ExtKeyUsagePurpose (..)
    , HashALG (..)
    )
import Data.X509.CertificateStore
    ( readCertificateStore
    )
import Data.X509.Validation
    ( ValidationChecks (..)
    , ValidationHooks (..)
    )
import Network.TLS
    ( CertificateRejectReason (..)
    , CertificateUsage (..)
    , ServerHooks (..)
    )
import Network.Wai.Handler.WarpTLS
    ( TLSSettings (..)
    , tlsSettingsChain
    )

import qualified Data.X509.Validation as X509

-- | Path to a x.509 PKI for mutual client-server authentication.
data TlsConfiguration = TlsConfiguration
    { tlsCaCert :: !FilePath
    , tlsSvCert :: !FilePath
    , tlsSvKey :: !FilePath
    }
    deriving (Show)

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
        maybe CertificateUsageAccept (CertificateUsageReject . CertificateRejectOther)

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
    -- recognize (tpCaPath)
    validateCertificate cert = do
        mstore <- readCertificateStore tlsCaCert
        maybe
            (pure $ Just "Cannot init a store, unable to validate client certificates")
            (fmap fromX509FailedReasons . validateStore)
            mstore
      where
        validateStore store =
            X509.validate HashSHA256 hooks checks store def serviceID cert

    fromX509FailedReasons reasons =
        case reasons of
            [] -> Nothing
            _ -> Just (show reasons)
