{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.Wallet.DB.Transactions.Delete (deletePendingOrExpiredTx) where

import Cardano.Wallet.DB.Sqlite.Schema
    ( EntityField (TxMetaStatus, TxMetaTxId, TxMetaWalletId) )
import Cardano.Wallet.DB.Sqlite.Types
    ( TxId (TxId) )
import Database.Persist.Sql
    ( SqlPersistT, deleteWhereCount, selectFirst, (<-.), (==.) )
import Prelude

import qualified Cardano.Wallet.Primitive.Types as W
import qualified Cardano.Wallet.Primitive.Types.Hash as W
import qualified Cardano.Wallet.Primitive.Types.Tx as W

-- | Delete the transaction, but only if it's not in ledger.
-- Returns non-zero if this was a success.
deletePendingOrExpiredTx
    :: W.WalletId
    -> W.Hash "Tx"
    -> SqlPersistT IO Int
deletePendingOrExpiredTx wid tid = do
    let filt = [ TxMetaWalletId ==. wid, TxMetaTxId ==. (TxId tid) ]
    selectFirst ((TxMetaStatus ==. W.InLedger):filt) [] >>= \case
        Just _ -> pure 0  -- marked in ledger - refuse to delete
        Nothing -> fromIntegral <$> deleteWhereCount
            ((TxMetaStatus <-. [W.Pending, W.Expired]):filt)
