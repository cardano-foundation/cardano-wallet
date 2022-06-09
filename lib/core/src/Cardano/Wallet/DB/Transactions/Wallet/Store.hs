
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Wallet.DB.Transactions.Wallet.Store where

import Prelude

import Cardano.Wallet.DB.Sqlite.Schema
    ( EntityField (TxMetaWalletId), TxMeta )
import Cardano.Wallet.DB.Transactions.Meta.Model
    ( DeltaTxMetaHistory (ExpandTxMetaHistory)
    , DeltaTxMetaHistoryAny (DeltaTxMetaHistoryAny)
    , TxMetaHistory
    , TxMetaOp (Manipulation)
    , mkTxMetaHistory
    )
import Cardano.Wallet.DB.Transactions.Meta.Store
    ( mkStoreTransactionsMeta )
import Cardano.Wallet.DB.Transactions.Transaction.Model
    ( DeltaTxHistory (ExpandTxHistory), TxHistory, mkTxHistory )
import Cardano.Wallet.DB.Transactions.Transaction.Store
    ( mkStoreTransactions )
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
import Data.Delta
    ( Base, Delta, apply )
import Data.DeltaMap
    ( DeltaMap (..) )
import Data.Function
    ( (&) )
import Data.Generics.Internal.VL
    ( view )
import Data.List
    ( nub )
import Data.Map.Strict
    ( Map )
import Database.Persist.Sql
    ( SqlPersistT, deleteWhere, entityVal, selectList, (==.) )
import Fmt
    ( Buildable (..) )

import qualified Cardano.Wallet.Primitive.Types as W
import qualified Cardano.Wallet.Primitive.Types.Tx as W
import qualified Data.Map.Strict as Map

{-------------------------------------------------------------------------------
    WalletState Store
-------------------------------------------------------------------------------}
-- | Store for 'WalletsMeta' of multiple different wallets.
mkStoreWalletsMeta
    :: Store (SqlPersistT IO)
        (DeltaMap W.WalletId DeltaTxMetaHistoryAny)
mkStoreWalletsMeta = Store { loadS = load, writeS = write, updateS = update}
  where
    -- write :: Base (DeltaMap W.WalletId DeltaTxMetaHistory) -> SqlPersistT IO ()
    write reset = forM_ (Map.assocs reset) $ \(wid, metas) ->
        writeS (mkStoreTransactionsMeta wid) metas
    -- update
    --     ::Base (DeltaMap W.WalletId DeltaTxMetaHistory)
    --     -> DeltaMap W.WalletId DeltaTxMetaHistory
    --     -> SqlPersistT IO ()
    update _ (Insert wid metas) =
        writeS (mkStoreTransactionsMeta wid) metas
    update _ (Delete wid) = do
        deleteWhere [TxMetaWalletId ==. wid]
    update _ (Adjust wid da) =
        updateS (mkStoreTransactionsMeta wid) undefined da
    load :: SqlPersistT IO
                (Either SomeException (Map.Map W.WalletId TxMetaHistory))
    load = do
        wids <- nub . fmap (view #txMetaWalletId  . entityVal)
            <$> selectList @TxMeta [] []
        runExceptT $ do
            xs <- forM wids $ ExceptT . loadS . mkStoreTransactionsMeta
            pure $ Map.fromList $ zip wids xs


data DeltaTxWalletsHistory
    = ExpandTxWalletsHistory W.WalletId [(W.Tx, W.TxMeta)]
    | ChangeTxMetaWalletsHistory W.WalletId (DeltaTxMetaHistory 'Manipulation)
    | GarbageCollectTxWalletsHistory
    deriving (Show, Eq)

instance Buildable DeltaTxWalletsHistory where build = build . show
instance Delta DeltaTxWalletsHistory where
    type Base DeltaTxWalletsHistory = (TxHistory, Map W.WalletId TxMetaHistory)
    apply (ExpandTxWalletsHistory wid cs) (txh, mtxmh) =
        ( apply (ExpandTxHistory $ mkTxHistory $  fst <$> cs) txh
        , mtxmh & case Map.lookup wid mtxmh of
            Nothing -> apply @(DeltaMap _ DeltaTxMetaHistoryAny)
                $ Insert wid
                $ mkTxMetaHistory wid cs
            Just _ -> apply @(DeltaMap _ DeltaTxMetaHistoryAny)
                $ Adjust wid
                $ DeltaTxMetaHistoryAny
                $ ExpandTxMetaHistory
                $ mkTxMetaHistory wid cs
        )
    apply (ChangeTxMetaWalletsHistory wid change) (txh, mtxmh) =
        (txh, mtxmh & apply (Adjust wid $ DeltaTxMetaHistoryAny change))
    apply GarbageCollectTxWalletsHistory (_txh, mtxmh) =
        (error "not implemented", mtxmh)

mkStoreTxWalletsHistory
    :: Store (SqlPersistT IO)
        DeltaTxWalletsHistory
mkStoreTxWalletsHistory = Store
    { loadS = liftA2 (,)
        <$> loadS mkStoreTransactions
        <*> loadS mkStoreWalletsMeta
    , writeS = \(txHistory, txMetaHistory) -> do
        writeS mkStoreTransactions txHistory
        writeS mkStoreWalletsMeta txMetaHistory
    , updateS = \(txh, mtxmh) -> \case
        ExpandTxWalletsHistory wid cs -> do
            updateS mkStoreTransactions txh
                $ ExpandTxHistory
                $ mkTxHistory
                $ fst <$> cs
            updateS mkStoreWalletsMeta mtxmh
                $ Adjust wid
                $ DeltaTxMetaHistoryAny
                $ ExpandTxMetaHistory
                $ mkTxMetaHistory wid cs
        ChangeTxMetaWalletsHistory wid change ->
            updateS mkStoreWalletsMeta mtxmh
                $ Adjust wid
                $ DeltaTxMetaHistoryAny change
        GarbageCollectTxWalletsHistory  ->
            error "not implemented"

    }




