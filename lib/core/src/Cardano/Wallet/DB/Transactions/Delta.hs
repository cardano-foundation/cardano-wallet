{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
module Cardano.Wallet.DB.Transactions.Delta where
import Cardano.Wallet.DB.Transactions.Types
    (
      TxHistoryF(TxHistoryF, txHistory_relations), TxHistory )
import Cardano.Wallet.Primitive.Types.Hash (Hash)
import Fmt ( Buildable(..) )
import Data.Delta ( Delta(..) )
import Cardano.Wallet.DB.Sqlite.Schema
    ( TxMeta(TxMeta, txMetaScriptValidity, txMetaFee,
             txMetaSlotExpires, txMetadata, txMetaAmount, txMetaBlockHeight,
             txMetaSlot, txMetaDirection, txMetaStatus, txMetaWalletId,
             txMetaTxId) )
import Prelude
import Control.Monad (guard)
import Cardano.Wallet.DB.Sqlite.Types (TxId(TxId))
import Cardano.Wallet.Primitive.Types.Tx (TxStatus(InLedger))

data DeltaTxHistory =
    DeltaTxHistory TxHistory
    | PruneTxHistory (Hash "Tx")
    | AgeTxHistory

instance Buildable DeltaTxHistory where
    build (DeltaTxHistory txs) =
        "DeltaTxHistory " <> build (length $ txHistory_relations txs)
    build (PruneTxHistory h) =
        "PruneTxHistory " <> build (show h)
    build AgeTxHistory =
        "AgeTxHistory "

instance Delta DeltaTxHistory where
    type Base DeltaTxHistory = TxHistory
    apply (DeltaTxHistory txs) h = h <> txs
    apply (PruneTxHistory tid) (TxHistoryF txs)
        = TxHistoryF $ do
        tx@(TxMeta {..}, _) <- txs
        guard $
            txMetaTxId == TxId tid && txMetaStatus /= InLedger
        pure tx
    apply AgeTxHistory _ = undefined
