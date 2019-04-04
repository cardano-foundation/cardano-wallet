{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
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
    ( DBLayer (..), ErrPutTxHistory (..), PrimaryKey (..) )
import Cardano.Wallet.Primitive.Model
    ( Wallet )
import Cardano.Wallet.Primitive.Types
    ( Hash, Tx, TxMeta, WalletId )
import Control.Concurrent.MVar
    ( modifyMVar, modifyMVar_, newMVar, readMVar )
import Control.DeepSeq
    ( deepseq )
import Control.Monad.Trans.Except
    ( ExceptT (..) )
import Data.Map.Strict
    ( Map )

import qualified Data.Map.Strict as Map

-- | Instantiate a new in-memory "database" layer that simply stores data in
-- a local MVar. Data vanishes if the software is shut down.
newDBLayer :: forall s. IO (DBLayer IO s)
newDBLayer = do
    db <- newMVar (mempty
        :: Map (PrimaryKey WalletId) (Wallet s, Map (Hash "Tx") (Tx, TxMeta)))
    return $ DBLayer
        { putCheckpoint = \key cp ->
            let
                alter = \case
                    Nothing -> Just (cp, mempty)
                    Just (_, history) -> Just (cp, history)
            in
                cp `deepseq` modifyMVar_ db (return . (Map.alter alter key))

        , readCheckpoint = \key ->
            fmap fst . Map.lookup key <$> readMVar db

        , readWallets =
            Map.keys <$> readMVar db

        , putTxHistory = \key@(PrimaryKey wid) txs' -> ExceptT $ do
            let alter = \case
                    Nothing -> Left (ErrNoSuchWallet wid)
                    Just (cp, txs) -> Right (Just (cp, txs <> txs'))
            let handle m = \case
                    Left err -> return (m, Left err)
                    Right m' -> return (m', Right ())
            txs' `deepseq` modifyMVar db (\m -> handle m $ Map.alterF alter key m)

        , readTxHistory = \key ->
            maybe mempty snd . Map.lookup key <$> readMVar db
        }
