
{-# OPTIONS_GHC -Wno-redundant-constraints#-}
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
    ( mkStoreWalletsMeta
    , mkStoreTxWalletsHistory
    , DeltaTxWalletsHistory(..)
    ) where

import Prelude

import Cardano.Wallet.DB.Sqlite.Schema
    ( EntityField (..), TxMeta )
import Cardano.Wallet.DB.Store.Meta.Model
    ( DeltaTxMetaHistory, mkTxMetaHistory )
import Cardano.Wallet.DB.Store.Meta.Store
    ( mkStoreMetaTransactions )
import Cardano.Wallet.DB.Store.Transactions.Model
    ( DeltaTxSet (Append, DeleteTx), TxSet (..), mkTxSet )
import Cardano.Wallet.DB.Store.Wallets.Model
    ( DeltaTxWalletsHistory (..)
    , transactionsToDeleteOnRollback
    , walletsLinkedTransactions
    )
import Control.Applicative
    ( liftA2 )
import Control.Exception
    ( SomeException (..) )
import Control.Monad
    ( forM, forM_ )
import Control.Monad.Class.MonadThrow
    ( MonadThrow, throwIO )
import Control.Monad.Except
    ( ExceptT (ExceptT), lift, runExceptT )
import Data.DBVar
    ( Store (..), updateLoad )
import Data.Delta
    ( Base, Delta )
import Data.DeltaMap
    ( DeltaMap (..) )
import Data.Generics.Internal.VL
    ( view )
import Data.List
    ( nub )
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
    update _ (Insert wid ms) = do
        writeS (mkStoreMetaTransactions wid) ms
    update _ (Delete wid) = do
        deleteWhere [TxMetaWalletId ==. wid ]
    update mold da@(Adjust wid xda) = updateLoad load throwIO f mold da
      where
        f old _ = case Map.lookup wid old of
            Nothing -> pure ()
            Just old' -> updateS (mkStoreMetaTransactions wid) (Just old') xda
    load = runExceptT $ do
        wids <- lift $ fmap (view #txMetaWalletId . entityVal)
            <$> selectList @TxMeta [] []
        fmap Map.fromList
            $ forM (nub wids) $ \wid -> (wid,)
                <$> ExceptT (loadS $ mkStoreMetaTransactions wid)

mkStoreTxWalletsHistory
    :: (Monad m, MonadThrow m)
    => Store m DeltaTxSet
    -> Store m (DeltaMap W.WalletId DeltaTxMetaHistory)
    -> Store m DeltaTxWalletsHistory
mkStoreTxWalletsHistory storeTransactions storeWalletsMeta =
    let load = liftA2 (,)
            <$> loadS storeTransactions
            <*> loadS storeWalletsMeta
        write = \(txSet,txMetaHistory) -> do
            writeS storeTransactions txSet
            writeS storeWalletsMeta txMetaHistory
        update ma delta =
            let (mTxSet,mWmetas) = (fst <$> ma, snd <$> ma)
            in  case delta of
            RollbackTxWalletsHistory wid slot -> do
                wmetas <- loadWhenNothing mWmetas storeWalletsMeta
                updateS storeWalletsMeta (Just wmetas)
                    $ Adjust wid
                    $ TxMetaStore.Manipulate
                    $ TxMetaStore.RollBackTxMetaHistory slot
                let deletions = transactionsToDeleteOnRollback wid slot wmetas
                forM_ deletions
                    $ updateS storeTransactions mTxSet . DeleteTx
            RemoveWallet wid -> do
                wmetas <- loadWhenNothing mWmetas storeWalletsMeta
                txSet <- loadWhenNothing mTxSet storeTransactions
                updateS storeWalletsMeta (Just wmetas) $ Delete wid
                let wmetas2 = Map.delete wid wmetas
                garbageCollectTxWalletsHistory txSet wmetas2
            ExpandTxWalletsHistory wid cs -> do
                wmetas <- loadWhenNothing mWmetas storeWalletsMeta
                updateS storeTransactions mTxSet
                    $ Append
                    $ mkTxSet
                    $ fst <$> cs
                updateS storeWalletsMeta (Just wmetas)
                    $ case Map.lookup wid wmetas of
                        Nothing -> Insert wid (mkTxMetaHistory wid cs)
                        Just _ -> Adjust wid
                            $ TxMetaStore.Expand
                            $ mkTxMetaHistory wid cs
    in Store { loadS = load, writeS = write, updateS = update }
  where
    garbageCollectTxWalletsHistory txSet wmetas =
        mapM_ (updateS storeTransactions (Just txSet) . DeleteTx)
            $ Map.keys
            $ Map.withoutKeys (relations txSet)
            $ walletsLinkedTransactions wmetas

-- | Call 'loadS' from a 'Store' if the value is not already in memory.
loadWhenNothing
    :: (Monad m, MonadThrow m, Delta da)
    => Maybe (Base da) -> Store m da -> m (Base da)
loadWhenNothing (Just a) _ = pure a
loadWhenNothing Nothing store =
    loadS store >>= \case
        Left (SomeException e) -> throwIO e
        Right a -> pure a
