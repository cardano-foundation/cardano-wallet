{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Copyright: © 2022 IOHK
License: Apache-2.0

Implementation of a 'QueryStore' for 'TxWalletsHistory'.
-}
module Cardano.Wallet.DB.Store.Wallets.Layer
    ( QueryTxWalletsHistory (..)
    , QueryStoreTxWalletsHistory
    , newQueryStoreTxWalletsHistory
    ) where

import Prelude

import Cardano.Wallet.DB.Sqlite.Schema
    ( TxMeta (..) )
import Cardano.Wallet.DB.Sqlite.Types
    ( TxId (..) )
import Cardano.Wallet.DB.Store.Meta.Model
    ( TxMetaHistory (relations) )
import Cardano.Wallet.DB.Store.QueryStore
    ( QueryStore (..) )
import Cardano.Wallet.DB.Store.Transactions.Model
    ( TxRelation )
import Cardano.Wallet.DB.Store.Wallets.Model
    ( DeltaTxWalletsHistory (..) )
import Cardano.Wallet.DB.Store.Wallets.Store
    ( mkStoreTxWalletsHistory, mkStoreWalletsMeta )
import Control.Monad.Class.MonadSTM
    ( MonadSTM
    , atomically
    , newTVarIO
    , readTVar
    , writeTVar
    )
import Control.Monad.Class.MonadThrow
    ( MonadEvaluate, MonadMask, MonadThrow )
import Data.DBVar
    ( Store (..), loadDBVar, initDBVar, readDBVar, updateDBVar )
import Data.Delta
    ( Delta (..) )
import Data.Foldable
    ( toList )
import Database.Persist.Sql
    ( SqlPersistT )

import qualified Cardano.Wallet.DB.Store.Transactions.Layer as TxSet
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Data.Map.Strict as Map

{-----------------------------------------------------------------------------
    Query type
------------------------------------------------------------------------------}
data QueryTxWalletsHistory b where
    GetByTxId :: TxId -> QueryTxWalletsHistory (Maybe TxRelation)
    One :: W.WalletId -> TxId -> QueryTxWalletsHistory (Maybe TxMeta)
    All :: W.WalletId -> QueryTxWalletsHistory [TxMeta]

{-----------------------------------------------------------------------------
    Query Store type
------------------------------------------------------------------------------}
type QueryStoreTxWalletsHistory =
    QueryStore (SqlPersistT IO) QueryTxWalletsHistory DeltaTxWalletsHistory

newQueryStoreTxWalletsHistory
    :: forall m. m ~ SqlPersistT IO
    => m QueryStoreTxWalletsHistory
newQueryStoreTxWalletsHistory = do
    let txsQueryStore = TxSet.mkDBTxSet

    storeWalletsMeta <- newCachedStore mkStoreWalletsMeta
    let storeTxWalletsHistory = mkStoreTxWalletsHistory
            (store txsQueryStore)   -- on disk
            storeWalletsMeta        -- on disk

    let readAllMetas :: W.WalletId -> m [TxMeta]
        readAllMetas wid = do
            Right wmetas <- loadS storeWalletsMeta
            pure
                . maybe [] (toList . relations)
                $ Map.lookup wid wmetas

        query :: forall a. QueryTxWalletsHistory a -> SqlPersistT IO a
        query = \case
            GetByTxId txid ->
                queryS txsQueryStore $ TxSet.GetByTxId txid
            One wid txid -> do
                Right wmetas <- loadS storeWalletsMeta
                pure $ do
                    metas <- Map.lookup wid wmetas
                    Map.lookup txid . relations $ metas
            All wid ->
                readAllMetas wid

    pure QueryStore
        { queryS = query
        , store = Store
            { loadS = loadS storeTxWalletsHistory
            , writeS = writeS storeTxWalletsHistory
            , updateS = \_ da -> do
                -- BUG: The following operations are very expensive for large
                -- wallets.
                -- Solution: Do not load `txSet`,
                -- more `storeWalletsMeta` into memory.
                -- This requires removing 'GarbageCollectTxWalletsHistory'
                Right txSet <- loadS (store txsQueryStore)
                Right wmetas <- loadS storeWalletsMeta
                updateS storeTxWalletsHistory (txSet,wmetas) da
            }
        }


-- | Create an identical 'Store',
-- except that `loadS` is cached in memory through a 'DBVar'.
--
-- TODO: More attention to exceptions and thread safety.
newCachedStore
    ::  ( MonadSTM m, MonadThrow m, MonadEvaluate m, MonadMask m
        , Delta da
        )
    => Store m da
    -> m (Store m da)
newCachedStore stor = do
    varvar <- newTVarIO Nothing
    let loadVar = do
            mvar <- atomically $ readTVar varvar
            case mvar of
                Nothing -> do
                    var <- loadDBVar stor
                    atomically $ writeTVar varvar (Just var)
                    pure var
                Just var -> pure var
    pure $ Store
        { loadS = do
            var <- loadVar
            Right <$> readDBVar var
        , writeS = \a -> do
            var <- initDBVar stor a
            atomically $ writeTVar varvar (Just var)
        , updateS = \_ da -> do
            var <- loadVar
            updateDBVar var da
        }
