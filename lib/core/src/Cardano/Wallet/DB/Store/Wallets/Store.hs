
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
    , mkStoreWalletsMeta ) where

import Prelude

import Cardano.Wallet.DB.Sqlite.Schema
    ( EntityField (TxMetaWalletId), TxMeta )
import Cardano.Wallet.DB.Store.Meta.Model
    ( DeltaTxMetaHistory (Manipulate), TxMetaHistory, mkTxMetaHistory )
import Cardano.Wallet.DB.Store.Meta.Store
    ( mkStoreMetaTransactions )
import Cardano.Wallet.DB.Store.Transactions.Model
    ( DeltaTxHistory (..), TxHistoryF (..), mkTxHistory )
import Cardano.Wallet.DB.Store.Transactions.Store
    ( mkStoreTransactions )
import Cardano.Wallet.DB.Store.Wallets.Model
    ( DeltaTxWalletsHistory (..), walletsLinkedTransactions )
import Control.Applicative
    ( liftA2 )
import Control.Exception
    ( SomeException )
import Control.Monad
    ( forM, forM_ )
import Control.Monad.Except
    ( ExceptT (ExceptT), runExceptT )
import Data.DBVar
    ( Store (..) )
import Data.DeltaMap
    ( DeltaMap (..) )
import Data.Generics.Internal.VL
    ( view )
import Data.List
    ( nub )
import Database.Persist.Sql
    ( SqlPersistT, deleteWhere, entityVal, selectList, (==.) )

import qualified Cardano.Wallet.DB.Store.Meta.Model as TxMetaStore
import qualified Cardano.Wallet.DB.Store.Transactions.Model as TxStore
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Data.Map.Strict as Map

-- | Store for a map of 'DeltaTxMetaHistory' of multiple different wallets.
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
    write reset = forM_ (Map.assocs reset) $ \(wid,metas)
        -> writeS (mkStoreMetaTransactions wid) metas
    update _ (Insert wid metas) =
        writeS (mkStoreMetaTransactions wid) metas
    update _ (Delete wid) = do
        deleteWhere [TxMetaWalletId ==. wid ]
    update _ (Adjust wid da) =
        updateS (mkStoreMetaTransactions wid) undefined da
    load :: SqlPersistT
            IO
            (Either SomeException (Map.Map W.WalletId TxMetaHistory))
    load = do
        wids <- nub . fmap (view #txMetaWalletId . entityVal)
            <$> selectList @TxMeta [] []
        runExceptT $ do
            xs <- forM wids $ ExceptT . loadS . mkStoreMetaTransactions
            pure $ Map.fromList $ zip wids xs

-- | Store for 'DeltaTxWalletsHistory'.
mkStoreTxWalletsHistory
    :: Store (SqlPersistT IO) DeltaTxWalletsHistory
mkStoreTxWalletsHistory =
    Store
    { loadS =
            liftA2 (,) <$> loadS mkStoreTransactions <*> loadS mkStoreWalletsMeta
    , writeS = \(txHistory,txMetaHistory) -> do
            writeS mkStoreTransactions txHistory
            writeS mkStoreWalletsMeta txMetaHistory
    , updateS = \(txh@(TxHistoryF mtxh),mtxmh) -> \case
            ExpandTxWalletsHistory wid cs -> do
                updateS mkStoreTransactions txh
                    $ TxStore.Append
                    $ mkTxHistory
                    $ fst <$> cs
                -- see also Store.Wallets.Model.garbageCollectEmptyWallets
                updateS mkStoreWalletsMeta mtxmh
                    $ case Map.lookup wid mtxmh of
                        Nothing ->
                            Insert wid $ mkTxMetaHistory wid cs
                        Just _ ->
                            Adjust wid
                            $ TxMetaStore.Expand
                            $ mkTxMetaHistory wid cs
            ChangeTxMetaWalletsHistory wid change
                -> updateS mkStoreWalletsMeta mtxmh
                $ Adjust wid
                $ Manipulate change
            GarbageCollectTxWalletsHistory -> mapM_
                (updateS mkStoreTransactions txh . DeleteTx)
                $ Map.keys
                $ Map.withoutKeys mtxh
                $ walletsLinkedTransactions mtxmh
            RemoveWallet wid -> updateS mkStoreWalletsMeta mtxmh $ Delete wid
    }

