{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

{- |
 Copyright: © 2018-2022 IOHK
 License: Apache-2.0

Pure, low level model for a collection of "meta transactions",
i.e. additional data ('TxMeta') that the wallet stores for each transaction.
Meta transactions are encoded "as" expressed in DB tables.

-}
module Cardano.Wallet.DB.Store.Meta.Model
    ( DeltaTxMetaHistory(..)
    , ManipulateTxMetaHistory(..)
    , TxMetaHistory(..)
    , mkTxMetaHistory
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

-- | A collection of `TxMeta`, indexed by transaction identifier.
newtype TxMetaHistory =
    TxMetaHistory { relations :: Map TxId TxMeta }
    deriving ( Generic, Eq, Show, Monoid, Semigroup )

instance Buildable TxMetaHistory where
    build txs =
        "TxMetaHistory "
        <> build (length $ relations txs)

-- | Meta changes that can be issued independently from the transaction store.
data ManipulateTxMetaHistory
    = PruneTxMetaHistory TxId
    | AgeTxMetaHistory W.SlotNo
    | RollBackTxMetaHistory W.SlotNo
    deriving ( Eq, Show )

-- | All meta-transactions changes, including the addition of new
-- meta-transactions, which has to be done in sync with the transactions store.
data DeltaTxMetaHistory
    = Manipulate ManipulateTxMetaHistory
    | Expand TxMetaHistory
    deriving (Show, Eq)

instance Buildable DeltaTxMetaHistory where
    build = build . show

instance Delta DeltaTxMetaHistory where
    type Base DeltaTxMetaHistory = TxMetaHistory
    apply (Expand txs) h = h <> txs
    apply (Manipulate d) h = apply d h

instance Delta ManipulateTxMetaHistory where
    type Base ManipulateTxMetaHistory = TxMetaHistory
    apply (PruneTxMetaHistory tid) (TxMetaHistory txs) =
        TxMetaHistory $ Map.alter f tid txs
      where
        f (Just tx@(TxMeta {..})) =
            if txMetaStatus == W.InLedger
                then Just tx
                else Nothing
        f Nothing = Nothing
    apply (AgeTxMetaHistory tip) (TxMetaHistory txs) =
        TxMetaHistory
        $ txs <&> \meta@TxMeta {..} ->
            if txMetaStatus == W.Pending && isExpired txMetaSlotExpires
            then meta { txMetaStatus = W.Expired }
            else meta
      where
        isExpired Nothing = False
        isExpired (Just tip') = tip' <= tip
    apply (RollBackTxMetaHistory point) (TxMetaHistory txs) =
        TxMetaHistory $ Map.mapMaybe rescheduleOrForget txs
      where
        rescheduleOrForget :: TxMeta -> Maybe TxMeta
        rescheduleOrForget meta =
            let
                isAfter = txMetaSlot meta > point
                isIncoming = txMetaDirection meta == W.Incoming
            in case (isAfter, isIncoming) of
                   (True,True) -> mzero
                   (True,False) -> Just
                       $ meta
                       { txMetaSlot = point, txMetaStatus = W.Pending }
                   _ -> Just meta

mkTxMetaEntity :: W.WalletId -> W.Tx -> W.TxMeta -> TxMeta
mkTxMetaEntity wid tx derived =
    TxMeta
    { txMetaTxId = TxId $ tx ^. #txId
    , txMetaWalletId = wid
    , txMetaStatus = derived ^. #status
    , txMetaDirection = derived ^. #direction
    , txMetaSlot = derived ^. #slotNo
    , txMetaBlockHeight = getQuantity
          (derived ^. #blockHeight)
    , txMetaAmount = derived ^. #amount
    , txMetaFee = fromIntegral . W.unCoin <$> W.fee tx
    , txMetaSlotExpires = derived ^. #expiry
    , txMetadata = W.metadata tx
    , txMetaScriptValidity = W.scriptValidity tx <&> \case
          W.TxScriptValid -> True
          W.TxScriptInvalid -> False
    }

mkTxMetaHistory :: W.WalletId -> [(W.Tx, W.TxMeta)] -> TxMetaHistory
mkTxMetaHistory wid txs = TxMetaHistory $
    Map.fromList
        [ (TxId $ tx ^. #txId, mkTxMetaEntity wid tx meta)
            | (tx, meta) <- txs
        ]


