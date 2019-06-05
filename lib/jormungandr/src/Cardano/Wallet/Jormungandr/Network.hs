{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: MIT
--
--
--
-- This module allows the wallet to retrieve blocks from a known @Jormungandr@
-- node. This is done by providing a @NetworkLayer@ with some logic building on
-- top of an underlying @JormungandrLayer@ HTTP client.
module Cardano.Wallet.Jormungandr.Network
    ( JormungandrLayer (..)
    , mkJormungandrLayer

    -- * Re-export
    , BaseUrl (..)
    , newManager
    , defaultManagerSettings
    , Scheme (..)
    ) where

import Prelude

import Cardano.Wallet.Jormungandr.Api
    ( BlockId, GetTipId, api )
import Cardano.Wallet.Network
    ( ErrNetworkUnreachable (..) )
import Control.Exception
    ( Exception )
import Control.Monad.Catch
    ( throwM )
import Control.Monad.Trans.Except
    ( ExceptT (..) )
import Data.Proxy
    ( Proxy (..) )
import Network.HTTP.Client
    ( Manager, defaultManagerSettings, newManager )
import Servant.Client
    ( BaseUrl (..), ClientM, Scheme (..), client, mkClientEnv, runClientM )
import Servant.Client.Core
    ( ServantError (..) )
import Servant.Links
    ( Link, safeLink )

-- TODO: Implement a NetworkLayer

{-------------------------------------------------------------------------------
                            Jormungandr Client
-------------------------------------------------------------------------------}

-- | Endpoints of the jormungandr REST API.
newtype JormungandrLayer m = JormungandrLayer
    { getTipId
        :: ExceptT ErrNetworkUnreachable m BlockId
    }

-- | Construct a 'JormungandrLayer'-client
--
-- >>> mgr <- newManager defaultManagerSettings
-- >>> j = mkJormungandrLayer mgr (BaseUrl Http "localhost" 8080 "")
-- >>> runExceptT $ getTipId j
-- BlockId (Hash {getHash = "&\198@\163\222\t\183C\152\193L\160\161\&7\236x\245\229\EOT\175\177\167\131\190\b\b/\174\212\177:\179"})
mkJormungandrLayer
    :: Manager -> BaseUrl -> JormungandrLayer IO
mkJormungandrLayer mgr baseUrl = JormungandrLayer
    { getTipId = ExceptT $ do
        let ctx = safeLink api (Proxy @GetTipId)
        run cGetTipId >>= defaultHandler ctx
    }
  where
    run :: ClientM a -> IO (Either ServantError a)
    run query = runClientM query (mkClientEnv mgr baseUrl)

    defaultHandler
        :: Link
        -> Either ServantError a
        -> IO (Either ErrNetworkUnreachable a)
    defaultHandler ctx = \case
        Right c -> return $ Right c

        -- The node has not started yet or has exited.
        -- This could be recovered from by either waiting for the node
        -- initialise, or restarting the node.
        Left (ConnectionError e) ->
            return $ Left $ ErrNetworkUnreachable e

        -- Other errors (status code, decode failure, invalid content type
        -- headers). These are considered to be programming errors, so crash.
        Left e -> do
            throwM (ErrUnexpectedNetworkFailure ctx e)

    cGetTipId = client api

data ErrUnexpectedNetworkFailure
    = ErrUnexpectedNetworkFailure Link ServantError
    deriving (Show)

instance Exception ErrUnexpectedNetworkFailure
