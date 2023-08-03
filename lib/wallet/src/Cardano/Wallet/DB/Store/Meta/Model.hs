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
    , TxMetaHistory(..)
    , mkTxMetaHistory
    , rollbackTxMetaHistory
    , mkTxMetaFromEntity
    ) where

import Prelude

import Cardano.Wallet.DB.Sqlite.Schema
    ( TxMeta (..) )
import Cardano.Wallet.DB.Sqlite.Types
    ( TxId (..) )
import Data.Delta
    ( Delta (..) )
import Data.Functor
    ( (<&>) )
import Data.Generics.Internal.VL
    ( (^.) )
import Data.Map.Strict
    ( Map )
import Data.Quantity
    ( Quantity (..) )
import Data.Set
    ( Set )
import Fmt
    ( Buildable (build) )
import GHC.Generics
    ( Generic )

import qualified Cardano.Wallet.Primitive.Types as W
import qualified Cardano.Wallet.Primitive.Types.Coin as W
import qualified Cardano.Wallet.Primitive.Types.Tx as W
import qualified Cardano.Wallet.Primitive.Types.Tx.TxMeta as W
import qualified Data.Map.Strict as Map

{-----------------------------------------------------------------------------
    Type
------------------------------------------------------------------------------}
-- | A collection of `TxMeta`, indexed by transaction identifier.
newtype TxMetaHistory =
    TxMetaHistory { relations :: Map TxId TxMeta }
    deriving ( Generic, Eq, Show, Monoid, Semigroup )

instance Buildable TxMetaHistory where
    build txs =
        "TxMetaHistory "
        <> build (length $ relations txs)

{-----------------------------------------------------------------------------
    Operations
------------------------------------------------------------------------------}


-- | All meta-transactions changes, including the addition of new
-- meta-transactions, which has to be done in sync with the transactions store.
data DeltaTxMetaHistory
    = Rollback W.SlotNo
    | Expand TxMetaHistory
    deriving (Show, Eq)

instance Buildable DeltaTxMetaHistory where
    build = build . show

instance Delta DeltaTxMetaHistory where
    type Base DeltaTxMetaHistory = TxMetaHistory
    apply (Expand txs) h = txs <> h
    apply (Rollback s) h = fst $ rollbackTxMetaHistory s h

-- | Rollback a 'TxMetaHistory' to a given slot.
-- Returns the new 'TxMetaHistory' as well as the 'TxId's that
-- have been /deleted/ due to the rollback.
rollbackTxMetaHistory
    :: W.SlotNo -> TxMetaHistory -> (TxMetaHistory, Set TxId)
rollbackTxMetaHistory point (TxMetaHistory txs) =
    (TxMetaHistory new, Map.keysSet deleted)
  where
    (deleted, new) = Map.mapEither keepOrForget txs

    keepOrForget :: TxMeta -> Either () TxMeta
    keepOrForget meta
        | txMetaSlot meta > point = Left ()
        | otherwise = Right meta

{-----------------------------------------------------------------------------
    Type conversion helpers
------------------------------------------------------------------------------}
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

-- | Compute a 'TxMetaHistory' for a wallet.
mkTxMetaHistory :: W.WalletId -> [(W.Tx, W.TxMeta)] -> TxMetaHistory
mkTxMetaHistory wid txs = TxMetaHistory $
    Map.fromList
        [ (TxId $ tx ^. #txId, mkTxMetaEntity wid tx meta)
            | (tx, meta) <- txs
        ]

mkTxMetaFromEntity :: TxMeta -> W.TxMeta
mkTxMetaFromEntity TxMeta{..} = W.TxMeta
    { W.status = txMetaStatus
    , W.direction = txMetaDirection
    , W.slotNo = txMetaSlot
    , W.blockHeight = Quantity (txMetaBlockHeight)
    , amount = txMetaAmount
    , W.expiry = txMetaSlotExpires
    }
