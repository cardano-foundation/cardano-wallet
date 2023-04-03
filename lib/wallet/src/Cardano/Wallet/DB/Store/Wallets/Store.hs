
{-# OPTIONS_GHC -Wno-redundant-constraints#-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Copyright: © 2022 IOHK
License: Apache-2.0

Implementation of a store for 'TxWalletsHistory'

-}
module Cardano.Wallet.DB.Store.Wallets.Store
    ( mkStoreTxWalletsHistory
    , DeltaTxWalletsHistory(..)
    ) where

import Prelude

import Cardano.Wallet.DB.Store.Meta.Model
    ( DeltaTxMetaHistory, mkTxMetaHistory )
import Cardano.Wallet.DB.Store.Transactions.Model
    ( DeltaTxSet (..), mkTxSet )
import Cardano.Wallet.DB.Store.Wallets.Model
    ( DeltaTxWalletsHistory (..) )
import Control.Applicative
    ( liftA2 )
import Control.Exception
    ( SomeException (..) )
import Control.Monad.Class.MonadThrow
    ( MonadThrow, throwIO )
import Data.DBVar
    ( Store (..) )
import Data.Delta
    ( Base, Delta )

import qualified Cardano.Wallet.DB.Store.Meta.Model as TxMetaStore

mkStoreTxWalletsHistory
    :: (Monad m, MonadThrow m)
    => Store m DeltaTxSet
    -> Store m DeltaTxMetaHistory
    -> Store m DeltaTxWalletsHistory
mkStoreTxWalletsHistory storeTransactions storeMeta =
    let load = liftA2 (,)
            <$> loadS storeTransactions
            <*> loadS storeMeta
        write = \(txSet,txMetaHistory) -> do
            writeS storeTransactions txSet
            writeS storeMeta txMetaHistory
        update ma delta =
            let (mTxSet,mWmetas) = (fst <$> ma, snd <$> ma)
            in  case delta of
            RollbackTxWalletsHistory slot -> do
                wmetas <- loadWhenNothing mWmetas storeMeta
                updateS storeMeta (Just wmetas)
                    $ TxMetaStore.Rollback slot
                let ( _metas', toBeDeletedTxSet)
                        = TxMetaStore.rollbackTxMetaHistory slot wmetas
                updateS storeTransactions mTxSet
                    $ DeleteTxs toBeDeletedTxSet

            ExpandTxWalletsHistory wid cs -> do
                wmetas <- loadWhenNothing mWmetas storeMeta
                updateS storeTransactions mTxSet
                    $ Append
                    $ mkTxSet
                    $ fst <$> cs
                updateS storeMeta (Just wmetas)
                    $ TxMetaStore.Expand
                    $ mkTxMetaHistory wid cs
    in Store { loadS = load, writeS = write, updateS = update }

-- | Call 'loadS' from a 'Store' if the value is not already in memory.
loadWhenNothing
    :: (Monad m, MonadThrow m, Delta da)
    => Maybe (Base da) -> Store m da -> m (Base da)
loadWhenNothing (Just a) _ = pure a
loadWhenNothing Nothing store =
    loadS store >>= \case
        Left (SomeException e) -> throwIO e
        Right a -> pure a
