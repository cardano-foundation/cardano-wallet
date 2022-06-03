{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
module Cardano.Wallet.DB.Transactions.Delta where
import Cardano.Wallet.DB.Sqlite.Schema
    ( TxMeta (..) )
import Cardano.Wallet.DB.Sqlite.Types
    ( TxId (TxId) )
import Cardano.Wallet.DB.Transactions.Types
    ( TxHistory, TxHistoryF (TxHistoryF, txHistory_relations) )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash )
import Cardano.Wallet.Primitive.Types.Tx
    ( TxStatus (InLedger) )
import Control.Monad
    ( guard )
import Data.Delta
    ( Delta (..) )
import Fmt
    ( Buildable (..) )
import Prelude

import qualified Cardano.Wallet.Primitive.Types as W
import qualified Cardano.Wallet.Primitive.Types.Tx as W

data DeltaTxHistory =
    DeltaTxHistory TxHistory
    | PruneTxHistory (Hash "Tx")
    | AgeTxHistory W.SlotNo

instance Buildable DeltaTxHistory where
    build (DeltaTxHistory txs) =
        "DeltaTxHistory " <> build (length $ txHistory_relations txs)
    build (PruneTxHistory h) =
        "PruneTxHistory " <> build (show h)
    build (AgeTxHistory slot) =
        "AgeTxHistory " <> build slot

instance Delta DeltaTxHistory where
    type Base DeltaTxHistory = TxHistory
    apply (DeltaTxHistory txs) h = h <> txs
    apply (PruneTxHistory tid) (TxHistoryF txs)
        = TxHistoryF $ do
        tx@(TxMeta {..}, _) <- txs
        guard $
            txMetaTxId == TxId tid && txMetaStatus /= InLedger
        pure tx
    apply (AgeTxHistory tip) (TxHistoryF txs) = TxHistoryF $ do
        (meta@TxMeta {..}, tx) <- txs
        let newstatus = if txMetaSlotExpires <= Just tip && txMetaStatus == W.Pending
                then W.Expired else txMetaStatus
        pure (meta{txMetaStatus = newstatus}, tx)

