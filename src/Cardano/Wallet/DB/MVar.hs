{-# LANGUAGE RankNTypes #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: MIT
--
-- Dummy implementation of the database-layer, using MVar. This may be good for
-- state-machine testing in order to compare it with an implementation on a real
-- data store.

module Cardano.Wallet.DB.MVar
    ( newDBLayer
    ) where

import Prelude

import Cardano.Wallet.DB
    ( DBLayer (..) )
import Control.Concurrent.MVar
    ( modifyMVar_, newMVar, readMVar )
import Control.DeepSeq
    ( deepseq )

import qualified Data.Map.Strict as Map


-- | Instantiate a new in-memory "database" layer that simply stores data in
-- a local MVar. Data vanishes if the software is shut down.
newDBLayer :: forall s. IO (DBLayer IO s)
newDBLayer = do
    wallets <- newMVar mempty
    return $ DBLayer
        { putCheckpoints = \key cps ->
            cps `deepseq` (modifyMVar_ wallets (return . Map.insert key cps))
        , readCheckpoints = \key ->
            Map.lookup key <$> readMVar wallets
        , readWallets =
            Map.keys <$> readMVar wallets
        }
