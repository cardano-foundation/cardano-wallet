{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Wallet.DB.Sqlite where

import Prelude

import Cardano.Wallet.DB.SqliteTypes
    ( AddressScheme, TxId )
import Data.Text
    ( Text )
import Data.Time.Clock
    ( UTCTime )
import Data.Word
    ( Word32 )
import Database.Persist.TH
import GHC.Generics
    ( Generic (..) )

import qualified Cardano.Wallet.Primitive.Types as W

-- fixme: need tables for wallet AddressPool

share
    [ mkPersist sqlSettings { mpsPrefixFields = False }
    , mkMigrate "migrateAll"
    ]
    [persistLowerCase|

-- Wallet IDs, address discovery state, and metadata.
Wallet
  walId                W.WalletId              sql=wallet_id
  walName              Text                    sql=name
  walPassphraseLastUpdatedAt   UTCTime         sql=passphrase_last_updated_at
  walStatus            W.WalletState           sql=status
  walDelegation        Text Maybe              sql=delegation
  walAddressScheme     AddressScheme           sql=address_discovery

  Primary walId
  deriving Show Generic

-- The private key for each wallet. This is in a separate table
-- simply so that "SELECT * FROM wallet" won't print keys.
WalletPrivateKey                               sql=private_key
  walPrivateKeyWalId   W.WalletId              sql=wallet_id
  walPrivateKey        Text                    sql=private_key

  Primary walPrivateKeyWalId
  Foreign Wallet fk_wallet_private_key walPrivateKeyWalId

  deriving Show Generic

-- Maps a transaction ID to its metadata (which is calculated
-- when applying blocks).
-- TxMeta is specific to a wallet because multiple wallets may
-- have the same transaction with different metdata values.
-- The associated inputs and outputs of the transaction are in
-- the TxIn and TxOut tables.
TxMeta
  txId                  TxId                   sql=tx_id
  txMetaWalletId        W.WalletId             sql=wallet_id
  txStatus              W.TxStatus             sql=status
  txMetaDirection       W.Direction            sql=direction
  txMetaSlotId          W.SlotId               sql=slot_id
  txMetaAmount          W.Coin                 sql=amount

  Primary txId txMetaWalletId
  Foreign Wallet fk_wallet_tx_meta txMetaWalletId
  deriving Show Generic

-- A transaction input associated with TxMeta.
-- There is no wallet ID because these values depend only on the
-- transaction, not the wallet.
-- txInputTxId is referred to by TxMeta and PendingTx
TxIn
  txInputTxId           TxId                   sql=tx_id
  txInputSourceTxId     TxId                   sql=source_id
  txInputSourceIndex    Word32                 sql=source_index
  txInputAddress        Text                   sql=address
  txInputAmount         W.Coin                 sql=amount

  Primary txInputTxId txInputSourceTxId txInputSourceIndex
  -- fixme: add index on tx_id
  deriving Show Generic

-- A transaction output associated with TxMeta.
-- There is no wallet ID because these values depend only on the
-- transaction, not the wallet.
-- txOutputTxId is referred to by TxMeta and PendingTx
TxOut
  txOutputTxId          TxId                   sql=tx_id
  txOutputIndex         Word32                 sql=index
  txOutputAddress       Text                   sql=address
  txOutputAmount        W.Coin                 sql=amount

  Primary txOutputTxId txOutputIndex
  -- fixme: add index on tx_id
  deriving Show Generic

-- A checkpoint for a given wallet is referred to by (wallet_id, slot_id).
-- Checkpoint data such as UTxO will refer to this table.
Checkpoint
  checkpointWalId       W.WalletId             sql=wallet_id
  checkpointWalSlot     W.SlotId               sql=slot_id

  Primary checkpointWalId checkpointWalSlot
  Foreign Wallet fk_wallet_checkpoint checkpointWalId

  deriving Show Generic

-- The UTxO for a given wallet checkpoint is a
-- one-to-one mapping from TxIn -> TxOut.
-- This table does not need to refer to the TxIn or TxOut tables.
-- All necessary information for the UTxO is in this table.
UTxO                                            sql=utxo
  -- The wallet checkpoint (wallet_id, slot_id)
  utxoWalletId        W.WalletId             sql=wallet_id
  utxoWalletSlot      W.SlotId               sql=slot_id

  -- TxIn
  utxoInputId          TxId                   sql=input_tx_id
  utxoInputIndex       Word32                 sql=input_index

  -- TxOut
  utxoOutputId        TxId                   sql=output_tx_id
  utxoOutputIndex     Word32                 sql=output_index

  Primary utxoWalletId utxoWalletSlot utxoInputId utxoInputIndex utxoOutputId utxoOutputIndex
  Foreign Checkpoint fk_checkpoint_utxo utxoWalletId utxoWalletSlot

  deriving Show Generic

-- The pending transactions for a wallet checkpoint.
PendingTx
  -- The wallet checkpoint (wallet_id, slot_id)
  pendingTxWalletId    W.WalletId             sql=wallet_id
  pendingTxSlotId      W.SlotId               sql=slot_id

  -- Transaction TxIn and TxOut
  pendingTxId2         TxId                   sql=tx_id

  Primary pendingTxWalletId pendingTxSlotId pendingTxId2
  Foreign Checkpoint fk_pending_tx pendingTxWalletId pendingTxSlotId

  deriving Show Generic
|]
