{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Wallet.Api.Server.Handlers.Certificates
  (extractCertificates, mkToApiAnyCertificate)
  where

import Cardano.Wallet.Api
    ( ApiLayer )
import Cardano.Wallet.Api.Server.Error
    ( liftHandler )
import Cardano.Wallet.Api.Types
    ( ApiAnyCertificate )
import Cardano.Wallet.Api.Types.Certificate
    ( mkApiAnyCertificate )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (CredFromKeyK) )
import Cardano.Wallet.Primitive.Types
    ( Certificate, WalletId )
import Cardano.Wallet.Read
    ( Tx )
import Cardano.Wallet.Read.Eras
    ( EraFun (..), K (..) )
import Cardano.Wallet.Read.Primitive.Tx.Features.Certificates
    ( certificates )
import Cardano.Wallet.Read.Tx.Certificates
    ( getEraCertificates )
import Cardano.Wallet.Registry
    ( WorkerCtx )
import Control.Category
    ( (.) )
import Data.Typeable
    ( Typeable )
import Prelude hiding
    ( (.) )
import Servant.Server
    ( Handler )

import qualified Cardano.Wallet as W

-- | Tx era dependent certificate extractions
extractCertificates :: EraFun Tx (K [Certificate])
extractCertificates = certificates . getEraCertificates

-- | Compute a function to promote a certificate to its API type
mkToApiAnyCertificate
    :: forall ctx s k n.
        ( ctx ~ ApiLayer s k 'CredFromKeyK
        , Typeable s
        , Typeable n
        )
    => WorkerCtx ctx
    -> WalletId
    -> Handler (Certificate -> ApiAnyCertificate n)
mkToApiAnyCertificate wrk wid = do
    (acct, _, acctPath) <-
        liftHandler $ W.readRewardAccount @_ @s @k @n wrk wid
    pure $ mkApiAnyCertificate acct acctPath
