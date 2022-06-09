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
{-# LANGUAGE StandaloneDeriving #-}

module Cardano.Wallet.DB.Transactions.Meta.Model
    ( DeltaTxMetaHistoryAny (..)
    , DeltaTxMetaHistory (..)
    , TxMetaHistory (..)
    , mkTxMetaHistory
    , TxMetaOp(..)
    , overTxMetaHistory
    )
where

import Prelude

import Cardano.Wallet.DB.Sqlite.Schema
    ( TxMeta (..) )
import Cardano.Wallet.DB.Sqlite.Types
    ( TxId (..) )
import Control.Monad
    ( MonadPlus (mzero) )
import Data.Delta
    ( Delta (..) )
import Data.Foldable
    ( fold )
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
import qualified Data.Map.Strict as Map

newtype TxMetaHistory = TxMetaHistory
    { txMetaHistory_relations :: Map TxId TxMeta }
    deriving (Generic, Eq, Show, Monoid, Semigroup)

instance Buildable TxMetaHistory where
    build txs = "TxMetaHistory "
        <> build (length $ txMetaHistory_relations txs)

data TxMetaOp = Expansion | Manipulation
data DeltaTxMetaHistory x where
    ExpandTxMetaHistory :: TxMetaHistory -> DeltaTxMetaHistory 'Expansion
    PruneTxMetaHistory :: TxId -> DeltaTxMetaHistory 'Manipulation
    AgeTxMetaHistory :: W.SlotNo -> DeltaTxMetaHistory 'Manipulation
    RollBackTxMetaHistory :: W.SlotNo -> DeltaTxMetaHistory 'Manipulation

data DeltaTxMetaHistoryAny = forall x . DeltaTxMetaHistoryAny
    (DeltaTxMetaHistory x)

deriving instance Show DeltaTxMetaHistoryAny

deriving instance Show (DeltaTxMetaHistory x)
deriving instance Eq (DeltaTxMetaHistory x)
instance Buildable DeltaTxMetaHistoryAny where
    build = build . show

instance Buildable (DeltaTxMetaHistory x) where
    build (ExpandTxMetaHistory txs) =
        "ExpandTxMetaHistory " <> build (length $ txMetaHistory_relations txs)
    build (PruneTxMetaHistory h) =
        "PruneTxMEtaHistory " <> build (show h)
    build (AgeTxMetaHistory slot) =
        "AgeTxMetaHistory " <> build slot
    build (RollBackTxMetaHistory slot) =
        "RollbackTxMetaHistory " <> build slot

newtype InfiniteNothing a = InfiniteNothing (Maybe a) deriving Eq
instance Ord a => Ord (InfiniteNothing a) where
    compare  (InfiniteNothing Nothing) (InfiniteNothing Nothing) = EQ
    compare  (InfiniteNothing Nothing) _ = GT
    compare  _  (InfiniteNothing Nothing) = LT
    compare  (InfiniteNothing (Just x)) (InfiniteNothing (Just y))
        = compare x y

instance Delta DeltaTxMetaHistoryAny  where
    type Base DeltaTxMetaHistoryAny = TxMetaHistory
    apply (DeltaTxMetaHistoryAny (ExpandTxMetaHistory txs)) h = h <> txs
    apply (DeltaTxMetaHistoryAny (PruneTxMetaHistory tid)) (TxMetaHistory txs)
        = TxMetaHistory
        $ Map.alter f tid txs
        where
            f (Just tx@(TxMeta {..})) = if
                txMetaStatus == W.InLedger then Just tx
                else Nothing
            f Nothing = Nothing
    apply (DeltaTxMetaHistoryAny (AgeTxMetaHistory tip)) (TxMetaHistory txs)
        = TxMetaHistory $ txs <&>
            \meta@TxMeta {..} ->
                if txMetaStatus == W.Pending
                    && InfiniteNothing txMetaSlotExpires
                        <= InfiniteNothing (Just tip)
                then meta{txMetaStatus = W.Expired}
                else meta
    apply (DeltaTxMetaHistoryAny (RollBackTxMetaHistory point))
        (TxMetaHistory txs)
        = TxMetaHistory
            $ Map.mapMaybe (rescheduleOrForget point) txs
        where
            rescheduleOrForget :: W.SlotNo -> TxMeta -> Maybe TxMeta
            rescheduleOrForget forkSlot meta =
                let isAfter = txMetaSlot meta > point
                    isIncoming = txMetaDirection meta == W.Incoming
                in case (isAfter, isIncoming) of
                    (True, True) -> mzero
                    (True, False) -> Just $ meta
                        { txMetaSlot = forkSlot
                        , txMetaStatus = W.Pending
                        }
                    _ -> Just meta



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
