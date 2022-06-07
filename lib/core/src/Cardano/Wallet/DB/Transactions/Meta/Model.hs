{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Wallet.DB.Transactions.Meta.Model where

import Prelude

import Cardano.Wallet.DB.Sqlite.Schema
    ( TxMeta (..) )
import Cardano.Wallet.DB.Sqlite.Types
    ( TxId (..) )
import Control.Monad
    ( guard )
import Data.Delta
    ( Delta (..) )
import Data.Function
    ( (&) )
import Data.Functor
    ( (<&>) )
import Data.Generics.Internal.VL
    ( (^.) )
import Data.Map.Strict
    ( Map )
import Data.Quantity
    ( Quantity (getQuantity) )
import Fmt
    ( Buildable (build) )
import GHC.Generics
    ( Generic )

import qualified Cardano.Wallet.Primitive.Types as W
import qualified Cardano.Wallet.Primitive.Types.Coin as W
import qualified Cardano.Wallet.Primitive.Types.Tx as W
import Data.Foldable
    ( fold )
import qualified Data.Map.Strict as Map

newtype TxMetaHistory = TxMetaHistory
    { txMetaHistory_relations :: Map TxId TxMeta }
    deriving (Generic, Eq, Show, Monoid, Semigroup)

instance Buildable TxMetaHistory where
    build txs = "TxMetaHistory "
        <> build (length $ txMetaHistory_relations txs)

data DeltaTxMetaHistory
    = ExpandTxMetaHistory TxMetaHistory
    | PruneTxMetaHistory TxId
    | AgeTxMetaHistory W.SlotNo
    | RollBackTxMetaHistory W.SlotNo
    deriving (Show, Eq, Generic)

instance Buildable DeltaTxMetaHistory where
    build (ExpandTxMetaHistory txs) =
        "ExpandTxMetaHistory " <> build (length $ txMetaHistory_relations txs)
    build (PruneTxMetaHistory h) =
        "PruneTxMEtaHistory " <> build (show h)
    build (AgeTxMetaHistory slot) =
        "AgeTxMetaHistory " <> build slot
    build (RollBackTxMetaHistory slot) =
        "RollbackTxMetaHistory " <> build slot

instance Delta DeltaTxMetaHistory where
    type Base DeltaTxMetaHistory = TxMetaHistory
    apply (ExpandTxMetaHistory txs) h = h <> txs
    apply (PruneTxMetaHistory tid) (TxMetaHistory txs) = TxMetaHistory $
        Map.alter f tid txs
        where
            f (Just tx@(TxMeta {..})) = if
                txMetaStatus == W.InLedger then Just tx
                else Nothing
            f Nothing = Nothing
    apply (AgeTxMetaHistory tip) (TxMetaHistory txs) = TxMetaHistory
        $ txs <&>
                do \meta@TxMeta {..} ->
                    let newstatus =
                            if isExpired tip meta
                            then W.Expired
                            else txMetaStatus
                    in meta{txMetaStatus = newstatus}
    apply (RollBackTxMetaHistory point) (TxMetaHistory txs) = TxMetaHistory
        $ txs & Map.mapMaybe (rescheduleOrForget point )
        where
            rescheduleOrForget :: W.SlotNo -> TxMeta -> Maybe TxMeta
            rescheduleOrForget forkSlot meta = do
                let
                    isAfter = txMetaSlot meta > point
                    isIncoming = txMetaDirection meta == W.Incoming
                guard $ not $ isIncoming && isAfter
                Just $ if isAfter
                            then meta
                                    { txMetaSlot = forkSlot
                                    , txMetaStatus = W.Pending
                                    }
                            else meta

isExpired :: W.SlotNo -> TxMeta -> Bool
isExpired tip TxMeta {..}
    = txMetaSlotExpires <= Just tip && txMetaStatus == W.Pending

mkTxMetaEntity
    :: W.WalletId -> W.Tx -> W.TxMeta -> TxMeta
mkTxMetaEntity wid tx derived  =
    TxMeta
        { txMetaTxId = TxId $ tx ^. #txId
        , txMetaWalletId = wid
        , txMetaStatus = derived ^. #status
        , txMetaDirection = derived ^. #direction
        , txMetaSlot = derived ^. #slotNo
        , txMetaBlockHeight = getQuantity (derived ^. #blockHeight)
        , txMetaAmount = derived ^. #amount
        , txMetaFee = fromIntegral . W.unCoin <$> W.fee tx
        , txMetaSlotExpires = derived ^. #expiry
        , txMetadata = W.metadata tx
        , txMetaScriptValidity = W.scriptValidity tx <&> \case
                W.TxScriptValid -> True
                W.TxScriptInvalid -> False
        }

mkTxMetaHistory :: W.WalletId -> [(W.Tx,W.TxMeta)] -> TxMetaHistory 
mkTxMetaHistory wid txs = TxMetaHistory $ fold $ do 
    (tx, meta) <- txs 
    let relation = mkTxMetaEntity wid tx meta
    pure $ Map.singleton (TxId $ tx ^. #txId) relation

overTxMetaHistory
    :: TxMetaHistory 
    -> (Map TxId TxMeta 
    -> Map TxId TxMeta) 
    -> TxMetaHistory
overTxMetaHistory (TxMetaHistory x) f = TxMetaHistory $ f x
