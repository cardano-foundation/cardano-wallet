
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
    ) where

import Prelude

import Cardano.Wallet.DB.Sqlite.Schema
    ( EntityField (..), LocalTxSubmission, TxMeta )
import Cardano.Wallet.DB.Store.Meta.Model
    ( TxMetaHistory (TxMetaHistory), mkTxMetaHistory )
import Cardano.Wallet.DB.Store.Meta.Store
    ( mkStoreMetaTransactions )
import Cardano.Wallet.DB.Store.Submissions.Model
    ( TxLocalSubmissionHistory (TxLocalSubmissionHistory) )
import Cardano.Wallet.DB.Store.Submissions.Store
    ( mkStoreSubmissions )
import Cardano.Wallet.DB.Store.Transactions.Model
    ( TxHistoryF (..) )
import Cardano.Wallet.DB.Store.TransactionsWithCBOR.Model
    ( DeltaTx (..), TxHistoryWithCBOR (TxHistoryWithCBOR) )
import Cardano.Wallet.DB.Store.TransactionsWithCBOR.Store
    ( mkStoreTransactionsWithCBOR )
import Cardano.Wallet.DB.Store.Wallets.Model
    ( DeltaTxWalletsHistory (..)
    , DeltaWalletsMetaWithSubmissions (..)
    , mkTxHistoryWithCBORs
    , walletsLinkedTransactions
    )
import Control.Applicative
    ( liftA2 )
import Control.Monad
    ( forM, forM_ )
import Control.Monad.Except
    ( ExceptT (ExceptT), lift, runExceptT )
import Data.DBVar
    ( Store (..) )
import Data.Delta
    ( apply )
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
import qualified Data.Set as Set


mkStoreWalletMetaWithSubmissions :: W.WalletId -> Store
        (SqlPersistT IO)
        (DeltaWalletsMetaWithSubmissions)
mkStoreWalletMetaWithSubmissions wid =
    Store
    { loadS = load
    , writeS = write
    , updateS = update
    }
  where
    write (metas,subs) = do
        writeS (mkStoreMetaTransactions wid) metas
        writeS (mkStoreSubmissions wid) subs
    update (metas, subs) xda = do
        localTxToBeRemoved <- case xda of
            ChangeMeta da -> do
                updateS (mkStoreMetaTransactions wid) undefined da
                let TxLocalSubmissionHistory subs' = subs
                    TxMetaHistory metas' = apply da metas
                pure $ Set.difference (Map.keysSet subs') (Map.keysSet metas')
            ChangeSubmissions da -> do
                updateS (mkStoreSubmissions wid) undefined da
                let TxLocalSubmissionHistory subs' = apply da subs
                    TxMetaHistory metas' = metas
                pure $ Set.difference (Map.keysSet subs') (Map.keysSet metas')
        -- the next operation is potentially already executed via cascade delete
        -- but we enforce it to avoid that DB feature in the future
        forM_ localTxToBeRemoved $ \txId ->
                deleteWhere
                    [ LocalTxSubmissionWalletId ==. wid
                    , LocalTxSubmissionTxId ==. txId
                    ]
    load = runExceptT $ do
        metas <- ExceptT $ loadS (mkStoreMetaTransactions wid)
        subs <- ExceptT $ loadS (mkStoreSubmissions wid)
        pure (metas, subs)

-- | Store for 'WalletsMeta' of multiple different wallets.
mkStoreWalletsMetaWithSubmissions :: Store
        (SqlPersistT IO)
        (DeltaMap W.WalletId DeltaWalletsMetaWithSubmissions)
mkStoreWalletsMetaWithSubmissions =
    Store
    { loadS = load
    , writeS = write
    , updateS = update
    }
  where
    write reset = forM_ (Map.assocs reset) $ \(wid, ms) ->
        writeS (mkStoreWalletMetaWithSubmissions wid) ms
    update :: Map W.WalletId (TxMetaHistory, TxLocalSubmissionHistory)
        -> DeltaMap W.WalletId DeltaWalletsMetaWithSubmissions
        -> SqlPersistT IO ()
    update _ (Insert wid ms) = do
        writeS (mkStoreWalletMetaWithSubmissions wid) ms
    update _ (Delete wid) = do
        deleteWhere [TxMetaWalletId ==. wid ]
        deleteWhere [LocalTxSubmissionWalletId ==. wid ]
    update old (Adjust wid xda) =
        case Map.lookup wid old of
            Nothing -> pure ()
            Just old' ->
                updateS (mkStoreWalletMetaWithSubmissions wid) old' xda
    load = runExceptT $ do
        wids <- lift $ fmap (view #txMetaWalletId . entityVal)
            <$> selectList @TxMeta [] []
        subsWids <- lift $ fmap (view #localTxSubmissionWalletId . entityVal)
            <$> selectList @LocalTxSubmission [] []
        fmap Map.fromList
            $ forM (nub $ wids <> subsWids) $ \wid -> (wid,)
                <$> ExceptT (loadS $ mkStoreWalletMetaWithSubmissions wid)


mkStoreTxWalletsHistory :: Store (SqlPersistT IO) DeltaTxWalletsHistory
mkStoreTxWalletsHistory =
    Store
    { loadS =
          liftA2 (,)
            <$> loadS mkStoreTransactionsWithCBOR
            <*> loadS mkStoreWalletsMetaWithSubmissions
    , writeS = \(txHistory,txMetaHistory) -> do
          writeS mkStoreTransactionsWithCBOR txHistory
          writeS mkStoreWalletsMetaWithSubmissions txMetaHistory
    , updateS = \(txh@(TxHistoryWithCBOR (TxHistoryF mtxh) _) ,mtxmh) -> \case
            ChangeTxMetaWalletsHistory wid change
                -> updateS mkStoreWalletsMetaWithSubmissions mtxmh
                $ Adjust wid change
            GarbageCollectTxWalletsHistory -> mapM_
                (updateS mkStoreTransactionsWithCBOR txh . DeleteTx)
                $ Map.keys
                $ Map.withoutKeys mtxh
                $ walletsLinkedTransactions mtxmh
            RemoveWallet wid -> updateS mkStoreWalletsMetaWithSubmissions mtxmh
                $ Delete wid
            ExpandTxWalletsHistory wid cs -> do
                updateS mkStoreTransactionsWithCBOR txh
                    $ Append
                    $ mkTxHistoryWithCBORs
                    $ fst <$> cs
                updateS mkStoreWalletsMetaWithSubmissions mtxmh
                    $ case Map.lookup wid mtxmh of
                        Nothing -> Insert wid (mkTxMetaHistory wid cs, mempty)
                        Just _ -> Adjust wid
                            $ ChangeMeta
                            $ TxMetaStore.Expand
                            $ mkTxMetaHistory wid cs
    }
