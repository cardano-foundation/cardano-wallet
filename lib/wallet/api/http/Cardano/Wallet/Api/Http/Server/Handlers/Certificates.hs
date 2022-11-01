{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2020 IOHK
-- License: Apache-2.0
--

module Cardano.Wallet.Api.Http.Server.Handlers.Certificates
    ( getApiAnyCertificates
    )
    where

import Cardano.Wallet.Api
    ( ApiLayer )
import Cardano.Wallet.Api.Http.Server.Error
    ( liftHandler )
import Cardano.Wallet.Api.Http.Server.Handlers.TxCBOR
    ( ParsedTxCBOR (..) )
import Cardano.Wallet.Api.Types.Certificate
    ( ApiAnyCertificate, mkApiAnyCertificate )
import Cardano.Wallet.DB
    ( DBLayer )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (CredFromKeyK) )
import Cardano.Wallet.Primitive.Types
    ( WalletId )
import Cardano.Wallet.Registry
    ( WorkerCtx )
import Data.Generics.Internal.VL
    ( (^.) )
import Data.Generics.Product
    ( typed )
import Data.Typeable
    ( Typeable )
import Prelude hiding
    ( (.) )
import Servant.Server
    ( Handler )

import qualified Cardano.Wallet as W

-- | Promote certificates of a transaction to API type,
-- using additional context from the 'WorkerCtx'.
getApiAnyCertificates
    :: forall s k n
     . (Typeable s, Typeable n)
    => WorkerCtx (ApiLayer s k 'CredFromKeyK)
    -> WalletId
    -> ParsedTxCBOR
    -> Handler [ApiAnyCertificate n]
getApiAnyCertificates wrk wid ParsedTxCBOR{certificates} = do
    (acct, _, acctPath) <- liftHandler $ W.readRewardAccount @s @k @n db wid
    pure $ mkApiAnyCertificate acct acctPath <$> certificates
  where
    db = wrk ^. typed @(DBLayer IO s k)
