{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Wallet.DB.Transactions.Delta where
import Cardano.Wallet.DB.Sqlite.Schema
    ( TxMeta (..) )
import Cardano.Wallet.DB.Sqlite.Types
    ( TxId (TxId) )
import Cardano.Wallet.DB.Transactions.Types
    ( TxHistory, TxHistoryF (TxHistoryF))
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash )
import Cardano.Wallet.Primitive.Types.Tx
    ( TxStatus (InLedger) )
import Data.Bifunctor
    ( first )
import Data.Delta
    ( Delta (..) )
import Data.Functor
    ( (<&>) )
import Fmt
    ( Buildable (..) )
import Prelude

import qualified Cardano.Wallet.Primitive.Types as W
import qualified Cardano.Wallet.Primitive.Types.Tx as W
import qualified Data.Map.Strict as M
import Control.Monad (when)
import Data.Function ((&))
import Cardano.Wallet.DB.Transactions.Model (mkTxHistory)

data DeltaTxHistory
    = ExpandTxHistory W.WalletId [(W.Tx, W.TxMeta)]
    | PruneTxHistory (Hash "Tx")
    | AgeTxHistory W.SlotNo
    | RollBackTxHistory W.SlotNo

instance Buildable DeltaTxHistory where
    build (ExpandTxHistory _ txs) =
        "ExpandTxHistory " <> build (length txs)
    build (PruneTxHistory h) =
        "PruneTxHistory " <> build (show h)
    build (AgeTxHistory slot) =
        "AgeTxHistory " <> build slot
    build (RollBackTxHistory slot) =
        "RollbackTxHistory " <> build slot

instance Delta DeltaTxHistory where
    type Base DeltaTxHistory = TxHistory
    apply (ExpandTxHistory wid txs) h = h <> mkTxHistory wid txs
    apply (PruneTxHistory tid) (TxHistoryF txs) = TxHistoryF $
        M.alter f (TxId tid) txs
        where
            f (Just tx@(TxMeta {..}, _)) = if
                txMetaStatus /= InLedger then Just tx
                else Nothing
            f Nothing = Nothing
    apply (AgeTxHistory tip) (TxHistoryF txs) = TxHistoryF
        $ txs <&> first
                do \meta@TxMeta {..} ->
                    let newstatus =
                            if isExpired tip meta
                            then W.Expired
                            else txMetaStatus
                    in meta{txMetaStatus = newstatus}
    apply (RollBackTxHistory point) (TxHistoryF txs) = TxHistoryF
        $ txs & M.mapMaybe
                (\(meta, tx) ->
                    (,) <$> rescheduleOrForget point meta <*> pure tx
                )
        where
            rescheduleOrForget :: W.SlotNo -> TxMeta -> Maybe TxMeta
            rescheduleOrForget forkSlot meta = do
                let isAfter = txMetaSlot meta > point
                let isIncoming = txMetaDirection meta == W.Incoming
                when (isIncoming && isAfter) Nothing
                pure $ if isAfter
                    then meta
                            { txMetaSlot = forkSlot , txMetaStatus = W.Pending }
                    else meta

isExpired :: W.SlotNo -> TxMeta -> Bool
isExpired tip TxMeta {..}
    = txMetaSlotExpires <= Just tip && txMetaStatus == W.Pending
