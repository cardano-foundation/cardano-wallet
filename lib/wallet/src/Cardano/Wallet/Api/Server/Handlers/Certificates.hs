{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2020 IOHK
-- License: Apache-2.0
--

module Cardano.Wallet.Api.Server.Handlers.Certificates
    ( getApiAnyCertificates
    )
    where

import Cardano.Wallet.Api
    ( ApiLayer )
import Cardano.Wallet.Api.Server.Error
    ( liftHandler )
import Cardano.Wallet.Api.Server.Handlers.TxCBOR
    ( ParsedTxCBOR (..) )
import Cardano.Wallet.Api.Types.Certificate
    ( ApiAnyCertificate, mkApiAnyCertificate )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (CredFromKeyK) )
import Cardano.Wallet.Primitive.Types
    ( WalletId )
import Cardano.Wallet.Registry
    ( WorkerCtx )
import Data.Typeable
    ( Typeable )
import Prelude hiding
    ( (.) )
import Servant.Server
    ( Handler )

import qualified Cardano.Wallet as W

-- | Compute a function to promote a certificate to its API type
getApiAnyCertificates
    :: forall ctx s k n.
        ( ctx ~ ApiLayer s k 'CredFromKeyK
        , Typeable s
        , Typeable n
        )
    => WorkerCtx ctx
    -> WalletId
    -> ParsedTxCBOR
    -> Handler [ApiAnyCertificate n]
getApiAnyCertificates wrk wid ParsedTxCBOR{..} = do
    (acct, _, acctPath) <-
        liftHandler $ W.readRewardAccount @_ @s @k @n wrk wid
    pure $ mkApiAnyCertificate acct acctPath <$> certificates
