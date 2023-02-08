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
    ( TxMetaHistory (relations), mkTxMetaHistory )
import Cardano.Wallet.DB.Store.QueryStore
    ( QueryStore (..) )
import Cardano.Wallet.DB.Store.Transactions.Model
    ( DeltaTxSet (..), TxRelation, mkTxSet )
import Cardano.Wallet.DB.Store.Wallets.Model
    ( DeltaTxWalletsHistory (..), walletsLinkedTransactions )
import Cardano.Wallet.DB.Store.Wallets.Store
    ( mkStoreTxWalletsHistory, mkStoreWalletsMeta )
import Data.DBVar
    ( Store (..), loadDBVar, modifyDBVar, readDBVar, updateDBVar )
import Data.DeltaMap
    ( DeltaMap (..) )
import Data.Foldable
    ( toList )
import Database.Persist.Sql
    ( SqlPersistT )

import qualified Cardano.Wallet.DB.Store.Meta.Model as TxMetaStore
import qualified Cardano.Wallet.DB.Store.Transactions.Layer as TxSet
import qualified Cardano.Wallet.DB.Store.Transactions.Model as TxSet
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
    transactionsDBVar <- loadDBVar mkStoreWalletsMeta

    let readAllMetas :: W.WalletId -> m [TxMeta]
        readAllMetas wid = do
            wmetas <- readDBVar transactionsDBVar
            pure
                . maybe [] (toList . relations)
                $ Map.lookup wid wmetas

        query :: forall a. QueryTxWalletsHistory a -> SqlPersistT IO a
        query = \case
            GetByTxId txid ->
                queryS txsQueryStore $ TxSet.GetByTxId txid
            One wid txid -> do
                wmetas <- readDBVar transactionsDBVar
                pure $ do
                    metas <- Map.lookup wid wmetas
                    Map.lookup txid . relations $ metas
            All wid ->
                readAllMetas wid

    let updateTxSet = updateS (store txsQueryStore) undefined
        update = \case
            ChangeTxMetaWalletsHistory wid change ->
                updateDBVar transactionsDBVar $ Adjust wid change
            RemoveWallet wid -> do
                updateDBVar transactionsDBVar $ Delete wid
                update $ GarbageCollectTxWalletsHistory
            ExpandTxWalletsHistory wid cs -> do
                updateTxSet
                    . Append
                    . mkTxSet
                    $ fst <$> cs
                modifyDBVar transactionsDBVar $ \mtxmh ->
                    ( case Map.lookup wid mtxmh of
                    Nothing -> Insert wid (mkTxMetaHistory wid cs)
                    Just _ -> Adjust wid
                        $ TxMetaStore.Expand
                        $ mkTxMetaHistory wid cs
                    , ()
                    )
            RollbackTxWalletsHistory wid slot -> do
                updateDBVar transactionsDBVar
                    $ Adjust wid
                    $ TxMetaStore.Manipulate
                    $ TxMetaStore.RollBackTxMetaHistory slot
                update undefined GarbageCollectTxWalletsHistory

            -- TODO as part of ADP-1043
            -- Remove GarbageCollectTxWalletsHistory
            -- in favor of `RollbackTo` + separate storage for Submissions
            GarbageCollectTxWalletsHistory -> do
                mtxmh <- readDBVar transactionsDBVar
                -- BUG: The following operation is very expensive for large
                -- wallets. Apply TODO above instead.
                Right mtxh <- loadS (store txsQueryStore)
                let txsToDelete
                        = Map.keys
                        . Map.withoutKeys (TxSet.relations mtxh)
                            -- needs info about existing transactions
                        $ walletsLinkedTransactions mtxmh
                mapM_ (updateTxSet . DeleteTx) txsToDelete

    pure QueryStore
        { queryS = query
        , store = Store
            { loadS = loadS mkStoreTxWalletsHistory
            , writeS = writeS mkStoreTxWalletsHistory
            , updateS = const update
            }
        }
