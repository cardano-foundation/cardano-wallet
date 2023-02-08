
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Copyright: © 2022 IOHK
License: Apache-2.0

Implementation of a store for 'TxWalletsHistory'

-}
module Cardano.Wallet.DB.Store.Wallets.Store
    ( mkStoreTxWalletsHistory
    , DeltaTxWalletsHistory(..)
    , mkStoreWalletsMeta
    ) where

import Prelude

import Cardano.Wallet.DB.Sqlite.Schema
    ( EntityField (..), TxMeta )
import Cardano.Wallet.DB.Store.Meta.Model
    ( DeltaTxMetaHistory, TxMetaHistory, mkTxMetaHistory )
import Cardano.Wallet.DB.Store.Meta.Store
    ( mkStoreMetaTransactions )
import Cardano.Wallet.DB.Store.Transactions.Model
    ( DeltaTxSet (Append, DeleteTx), TxSet (..), mkTxSet )
import Cardano.Wallet.DB.Store.Transactions.Store
    ( mkStoreTransactions )
import Cardano.Wallet.DB.Store.Wallets.Model
    ( DeltaTxWalletsHistory (..), inAnyWallet, walletsLinkedTransactions )
import Control.Applicative
    ( liftA2 )
import Control.Monad
    ( forM, forM_ )
import Control.Monad.Except
    ( ExceptT (ExceptT), lift, runExceptT )
import Data.DBVar
    ( Store (..) )
import Data.DeltaMap
    ( DeltaMap (..) )
import Data.Generics.Internal.VL
    ( view )
import Data.List
    ( nub )
import Data.Map.Strict
    ( Map )
import Database.Persist.Sql
    ( SqlPersistT, deleteWhere, entityVal, selectList, (==.) )

import qualified Cardano.Wallet.DB.Store.Meta.Model as TxMetaStore
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Data.Map.Strict as Map

-- | Store for 'WalletsMeta' of multiple different wallets.
mkStoreWalletsMeta :: Store
        (SqlPersistT IO)
        (DeltaMap W.WalletId DeltaTxMetaHistory)
mkStoreWalletsMeta =
    Store
    { loadS = load
    , writeS = write
    , updateS = update
    }
  where
    write reset = forM_ (Map.assocs reset) $ \(wid, ms) ->
        writeS (mkStoreMetaTransactions wid) ms
    update :: Map W.WalletId TxMetaHistory
        -> DeltaMap W.WalletId DeltaTxMetaHistory
        -> SqlPersistT IO ()
    update _ (Insert wid ms) = do
        writeS (mkStoreMetaTransactions wid) ms
    update _ (Delete wid) = do
        deleteWhere [TxMetaWalletId ==. wid ]
    update old (Adjust wid xda) =
        case Map.lookup wid old of
            Nothing -> pure ()
            Just old' ->
                updateS (mkStoreMetaTransactions wid) old' xda
    load = runExceptT $ do
        wids <- lift $ fmap (view #txMetaWalletId . entityVal)
            <$> selectList @TxMeta [] []
        fmap Map.fromList
            $ forM (nub wids) $ \wid -> (wid,)
                <$> ExceptT (loadS $ mkStoreMetaTransactions wid)

mkStoreTxWalletsHistory
    :: Store (SqlPersistT IO) DeltaTxSet
    -> Store (SqlPersistT IO) (DeltaMap W.WalletId DeltaTxMetaHistory)
    -> Store (SqlPersistT IO) DeltaTxWalletsHistory
mkStoreTxWalletsHistory storeTransactions storeWalletsMeta =
    Store
    { loadS =
        liftA2 (,)
            <$> loadS storeTransactions
            <*> loadS storeWalletsMeta
    , writeS = \(txSet,wmetas) -> do
        writeS storeTransactions txSet
        writeS storeWalletsMeta wmetas
    , updateS = \(txSet,wmetas) -> \case
            RollbackTxWalletsHistory wid slot -> do
                -- roll back metas for this wallet
                updateS mkStoreWalletsMeta wmetas
                    $ Adjust wid
                    $ TxMetaStore.Manipulate
                    $ TxMetaStore.RollBackTxMetaHistory slot

                -- delete transactions that have been rolled back and
                -- are not in any other wallet
                case Map.lookup wid wmetas of
                    Nothing -> pure ()
                    Just metas -> do
                        let txsToDelete = snd $
                                TxMetaStore.rollbackTxMetaHistory slot metas
                            otherWallets = Map.delete wid wmetas
                            shouldKeepTx txid = inAnyWallet txid otherWallets
                            deletions = filter (not . shouldKeepTx) txsToDelete

                        forM_ deletions
                            $ updateS storeTransactions txSet . DeleteTx
            GarbageCollectTxWalletsHistory ->
                garbageCollectTxWalletsHistory txSet wmetas
            RemoveWallet wid -> do
                updateS storeWalletsMeta wmetas $ Delete wid
                let wmetas2 = Map.delete wid wmetas
                garbageCollectTxWalletsHistory txSet wmetas2
            ExpandTxWalletsHistory wid cs -> do
                updateS storeTransactions txSet
                    $ Append
                    $ mkTxSet
                    $ fst <$> cs
                updateS storeWalletsMeta wmetas
                    $ case Map.lookup wid wmetas of
                        Nothing -> Insert wid (mkTxMetaHistory wid cs)
                        Just _ -> Adjust wid
                            $ TxMetaStore.Expand
                            $ mkTxMetaHistory wid cs
    }
  where
    garbageCollectTxWalletsHistory txSet wmetas =
        mapM_ (updateS storeTransactions txSet . DeleteTx)
            $ Map.keys
            $ Map.withoutKeys (relations txSet)
            $ walletsLinkedTransactions wmetas