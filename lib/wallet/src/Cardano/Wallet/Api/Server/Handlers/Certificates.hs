{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Wallet.Api.Server.Handlers.Certificates
  (parseCertificates, toApiAnyCertificate)
  where

import Prelude

import Cardano.Wallet.Api
    ( ApiLayer )
import Cardano.Wallet.Api.Server.Error
    ( IsServerError (..), apiError, liftE, liftHandler, showT )
import Cardano.Wallet.Api.Types
    ( ApiAnyCertificate, ApiErrorCode (UnexpectedError) )
import Cardano.Wallet.Api.Types.Certificate
    ( mkApiAnyCertificate )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (CredFromKeyK) )
import Cardano.Wallet.Primitive.Types
    ( Certificate, WalletId )
import Cardano.Wallet.Read.Primitive.Tx.Features.Certificates
    ( extractCertificates )
import Cardano.Wallet.Read.Tx.CBOR
    ( TxCBOR )
import Cardano.Wallet.Registry
    ( WorkerCtx )
import Codec.CBOR.Read
    ( DeserialiseFailure )
import Data.Typeable
    ( Typeable )
import Servant.Server
    ( Handler, err500 )

import qualified Cardano.Wallet as W

newtype ErrParseCBOR = ErrParseCBOR DeserialiseFailure
    deriving (Eq, Show)

instance IsServerError ErrParseCBOR where
    toServerError (ErrParseCBOR df) =
        apiError err500 UnexpectedError $ mconcat
            [ "Error while trying to parse a transaction CBOR from the database"
            , showT df
            ]

-- | Parse CBOR certificates and throw a server deserialize error if failing.
--
-- Missing CBOR in the tx is mapped to empty certificates. This is a hack until
-- it's required for the users to restore their wallet.
parseCertificates :: Maybe TxCBOR -> Handler [Certificate]
parseCertificates Nothing = pure []
parseCertificates (Just cbor) = case extractCertificates cbor of
    Left df -> liftE $ ErrParseCBOR df
    Right cs -> pure cs

-- | Compute a function to promote a certificate to its API type
toApiAnyCertificate
    :: forall ctx s k n.
        ( ctx ~ ApiLayer s k 'CredFromKeyK
        , Typeable s
        , Typeable n
        )
    => WorkerCtx ctx
    -> WalletId
    -> Handler (Certificate -> ApiAnyCertificate n)
toApiAnyCertificate wrk wid = do
    (acct, _, acctPath) <-
        liftHandler $ W.readRewardAccount @_ @s @k @n wrk wid
    pure $ mkApiAnyCertificate acct acctPath
