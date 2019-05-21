{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: MIT
--
-- Auto-generated Sqlite & Persistent machinery via Template-Haskell. This has
-- been moved into a separate file so that we can treat it slightly differently
-- when computing code-coverage.
--
-- More than 6K lines end-up being generated from the instructions below! As a
-- result, we're going to ignore code-coverage on the following module and, no
-- hand-written functions should be written in this module!

module Cardano.Wallet.DB.Sqlite.TH where

import Prelude

import Cardano.Wallet.DB.Sqlite.Types
    ( AddressPoolXPub, TxId, sqlSettings' )
import Data.Text
    ( Text )
import Data.Time.Clock
    ( UTCTime )
import Data.Word
    ( Word32 )
import Database.Persist.TH
    ( mkDeleteCascade, mkMigrate, mkPersist, persistLowerCase, share )
import GHC.Generics
    ( Generic (..) )
import Numeric.Natural
    ( Natural )

import qualified Cardano.Wallet.Primitive.AddressDiscovery as W
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Data.ByteString.Char8 as B8

share
    [ mkPersist sqlSettings'
    , mkDeleteCascade sqlSettings'
    , mkMigrate "migrateAll"
    ]
    [persistLowerCase|

-- Wallet IDs, address discovery state, and metadata.
Wallet
    walTableId                 W.WalletId     sql=wallet_id
    walTableName               Text           sql=name
    walTablePassphraseLastUpdatedAt  UTCTime Maybe  sql=passphrase_last_updated_at
    walTableStatus             W.WalletState  sql=status
    walTableDelegation         Text Maybe     sql=delegation

    Primary walTableId
    deriving Show Generic

-- The private key for each wallet. This is in a separate table simply so that
-- "SELECT * FROM wallet" won't print keys.
PrivateKey                                  sql=private_key
    privateKeyTableWalletId  W.WalletId     sql=wallet_id
    privateKeyTableRootKey   B8.ByteString  sql=root
    privateKeyTableHash      B8.ByteString  sql=hash

    Primary privateKeyTableWalletId
    Foreign Wallet fk_wallet_private_key privateKeyTableWalletId

    deriving Show Generic

-- Maps a transaction ID to its metadata (which is calculated when applying
-- blocks).
--
-- TxMeta is specific to a wallet because multiple wallets may have the same
-- transaction with different metadata values. The associated inputs and outputs
-- of the transaction are in the TxIn and TxOut tables.
TxMeta
    txMetaTableTxId       TxId         sql=tx_id
    txMetaTableWalletId   W.WalletId   sql=wallet_id
    txMetaTableStatus     W.TxStatus   sql=status
    txMetaTableDirection  W.Direction  sql=direction
    txMetaTableSlotId     W.SlotId     sql=slot
    txMetaTableAmount     Natural      sql=amount

    Primary txMetaTableTxId txMetaTableWalletId
    Foreign Wallet fk_wallet_tx_meta txMetaTableWalletId
    deriving Show Generic

-- A transaction input associated with TxMeta.
--
-- There is no wallet ID because these values depend only on the transaction,
-- not the wallet. txInputTableTxId is referred to by TxMeta and PendingTx
TxIn
    txInputTableTxId         TxId        sql=tx_id
    txInputTableSourceTxId   TxId        sql=source_id
    txInputTableSourceIndex  Word32      sql=source_index

    Primary txInputTableTxId txInputTableSourceTxId txInputTableSourceIndex
    deriving Show Generic

-- A transaction output associated with TxMeta.
--
-- There is no wallet ID because these values depend only on the transaction,
-- not the wallet. txOutputTableTxId is referred to by TxMeta and PendingTx
TxOut
    txOutputTableTxId     TxId        sql=tx_id
    txOutputTableIndex    Word32      sql=index
    txOutputTableAddress  W.Address   sql=address
    txOutputTableAmount   W.Coin      sql=amount

    Primary txOutputTableTxId txOutputTableIndex
    deriving Show Generic

-- A checkpoint for a given wallet is referred to by (wallet_id, slot).
-- Checkpoint data such as UTxO will refer to this table.
Checkpoint
    checkpointTableWalletId    W.WalletId  sql=wallet_id
    checkpointTableSlot        W.SlotId    sql=slot

    Primary checkpointTableWalletId checkpointTableSlot
    Foreign Wallet fk_wallet_checkpoint checkpointTableWalletId

    deriving Show Generic

-- The UTxO for a given wallet checkpoint is a one-to-one mapping from TxIn ->
-- TxOut. This table does not need to refer to the TxIn or TxOut tables. All
-- necessary information for the UTxO is in this table.
UTxO                                     sql=utxo

    -- The wallet checkpoint (wallet_id, slot)
    utxoTableWalletId        W.WalletId  sql=wallet_id
    utxoTableCheckpointSlot  W.SlotId    sql=slot

    -- TxIn
    utxoTableInputId         TxId        sql=input_tx_id
    utxoTableInputIndex      Word32      sql=input_index

    -- TxOut
    utxoTableOutputAddress   W.Address   sql=output_address
    utxoTableOutputCoin      W.Coin      sql=output_coin

    Primary
        utxoTableWalletId
        utxoTableCheckpointSlot
        utxoTableInputId
        utxoTableInputIndex
        utxoTableOutputAddress
        utxoTableOutputCoin

    Foreign Checkpoint fk_checkpoint_utxo utxoTableWalletId utxoTableCheckpointSlot
    deriving Show Generic

-- The pending transactions for a wallet checkpoint.
PendingTx

    -- The wallet checkpoint (wallet_id, slot)
    pendingTxTableWalletId        W.WalletId  sql=wallet_id
    pendingTxTableCheckpointSlot  W.SlotId    sql=slot

    -- Transaction TxIn and TxOut
    pendingTxTableId2             TxId        sql=tx_id

    Primary pendingTxTableWalletId pendingTxTableCheckpointSlot pendingTxTableId2
    Foreign Checkpoint fk_pending_tx pendingTxTableWalletId pendingTxTableCheckpointSlot
    deriving Show Generic

-- State for sequential scheme address discovery
SeqState

    -- The wallet checkpoint (wallet_id, slot)
    seqStateTableWalletId        W.WalletId  sql=wallet_id
    seqStateTableCheckpointSlot  W.SlotId    sql=slot

    UniqueSeqState seqStateTableWalletId seqStateTableCheckpointSlot
    Foreign Checkpoint fk_checkpoint_seq_state seqStateTableWalletId seqStateTableCheckpointSlot
    deriving Show Generic

-- Address pool attributes.
AddressPool
    addressPoolAccountPubKey        AddressPoolXPub
    addressPoolGap                  W.AddressPoolGap

    deriving Show Generic

-- Mapping of pool addresses to indices.
AddressPoolIndex
    indexAddressPool   AddressPoolId
    indexAddress       W.Address
    indexNumber        Word32

    deriving Show Generic

-- Sequential address discovery scheme -- internal address pool
-- associated with state record.
SeqStateInternalPool
    seqStateInternalPoolSeqStateId   SeqStateId
    seqStateInternalPoolAddressPool  AddressPoolId
    UniqueSeqStateInternalPool seqStateInternalPoolSeqStateId seqStateInternalPoolAddressPool
    Primary seqStateInternalPoolSeqStateId
    deriving Show Generic

-- Sequential address discovery scheme -- external address pool
-- associated with state record.
SeqStateExternalPool
    seqStateExternalPoolSeqStateId   SeqStateId
    seqStateExternalPoolAddressPool  AddressPoolId
    UniqueSeqStateExternalPool seqStateExternalPoolSeqStateId seqStateExternalPoolAddressPool
    Primary seqStateExternalPoolSeqStateId
    deriving Show Generic

-- Sequential address discovery scheme -- pending change indexes
SeqStatePendingIx
    seqStatePendingIxSeqStateId     SeqStateId
    seqStatePendingIxIndex          Word32

    Primary seqStatePendingIxSeqStateId seqStatePendingIxIndex
    deriving Show Generic
|]
