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
-- License: Apache-2.0
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
    ( AddressPoolXPub, BlockId, TxId, sqlSettings' )
import Data.Text
    ( Text )
import Data.Time.Clock
    ( UTCTime )
import Data.Word
    ( Word32, Word64 )
import Database.Persist.TH
    ( mkDeleteCascade, mkMigrate, mkPersist, persistLowerCase, share )
import GHC.Generics
    ( Generic (..) )
import Numeric.Natural
    ( Natural )
import System.Random
    ( StdGen )

import qualified Cardano.Wallet.Primitive.AddressDerivation.Sequential as W
import qualified Cardano.Wallet.Primitive.AddressDiscovery.Sequential as W
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
    walId                 W.WalletId     sql=wallet_id
    walCreationTime       UTCTime        sql=creation_time
    walName               Text           sql=name
    walPassphraseLastUpdatedAt  UTCTime Maybe  sql=passphrase_last_updated_at
    walStatus             W.WalletState  sql=status
    walDelegation         Text Maybe     sql=delegation

    Primary walId
    deriving Show Generic

-- The private key for each wallet. This is in a separate table simply so that
-- "SELECT * FROM wallet" won't print keys.
PrivateKey                             sql=private_key
    privateKeyWalletId  W.WalletId     sql=wallet_id
    privateKeyRootKey   B8.ByteString  sql=root
    privateKeyHash      B8.ByteString  sql=hash

    Primary privateKeyWalletId
    Foreign Wallet fk_wallet_private_key privateKeyWalletId

    deriving Show Generic

-- Maps a transaction ID to its metadata (which is calculated when applying
-- blocks).
--
-- TxMeta is specific to a wallet because multiple wallets may have the same
-- transaction with different metadata values. The associated inputs and outputs
-- of the transaction are in the TxIn and TxOut tables.
TxMeta
    txMetaTxId       TxId         sql=tx_id
    txMetaWalletId   W.WalletId   sql=wallet_id
    txMetaStatus     W.TxStatus   sql=status
    txMetaDirection  W.Direction  sql=direction
    txMetaSlotId     W.SlotId     sql=slot
    txMetaAmount     Natural      sql=amount

    Primary txMetaTxId txMetaWalletId
    Foreign Wallet fk_wallet_tx_meta txMetaWalletId
    deriving Show Generic

-- A transaction input associated with TxMeta.
--
-- There is no wallet ID because these values depend only on the transaction,
-- not the wallet. txInputTxId is referred to by TxMeta
TxIn
    txInputTxId           TxId          sql=tx_id
    txInputOrder          Int           sql=order
    txInputSourceTxId     TxId          sql=source_tx_id
    txInputSourceIndex    Word32        sql=source_index
    txInputSourceAmount   W.Coin Maybe  sql=source_amount default=NULL

    Primary txInputTxId txInputSourceTxId txInputSourceIndex
    deriving Show Generic

-- A transaction output associated with TxMeta.
--
-- There is no wallet ID because these values depend only on the transaction,
-- not the wallet. txOutputTxId is referred to by TxMeta
TxOut
    txOutputTxId     TxId        sql=tx_id
    txOutputIndex    Word32      sql=index
    txOutputAddress  W.Address   sql=address
    txOutputAmount   W.Coin      sql=amount

    Primary txOutputTxId txOutputIndex
    deriving Show Generic

-- A checkpoint for a given wallet is referred to by (wallet_id, slot).
-- Volatile checkpoint data such as AD state will refer to this table.
Checkpoint
    checkpointWalletId    W.WalletId  sql=wallet_id
    checkpointSlot        W.SlotId    sql=slot
    checkpointParent      BlockId     sql=parent_block_id
    checkpointBlockHeight Word64      sql=block_height

    Primary checkpointWalletId checkpointSlot
    Foreign Wallet fk_wallet_checkpoint checkpointWalletId

    deriving Show Generic

-- The UTxO for a given wallet checkpoint is a one-to-one mapping from TxIn ->
-- TxOut. This table does not need to refer to the TxIn or TxOut tables. All
-- necessary information for the UTxO is in this table.
UTxO                                sql=utxo

    -- The wallet checkpoint (wallet_id, slot)
    utxoWalletId        W.WalletId  sql=wallet_id

    -- This UTxO is only available for checkpoints between
    -- slot and slot_spent.
    utxoSlot        W.SlotId        sql=slot
    utxoSlotSpent   W.SlotId Maybe  sql=slot_spent

    -- TxIn
    utxoInputId         TxId        sql=input_tx_id
    utxoInputIndex      Word32      sql=input_index

    -- TxOut
    utxoOutputAddress   W.Address   sql=output_address
    utxoOutputCoin      W.Coin      sql=output_coin

    Primary
        utxoWalletId
        utxoInputId
        utxoInputIndex
        utxoSlot

    Foreign Wallet fk_wallet_utxo utxoWalletId
    deriving Show Generic

-- Sequential scheme address discovery state
-- which does not belong to a particular checkpoint.
SeqState
    seqStateWalletId        W.WalletId        sql=wallet_id
    seqStateExternalGap     W.AddressPoolGap  sql=external_gap
    seqStateInternalGap     W.AddressPoolGap  sql=internal_gap
    seqStateAccountXPub     AddressPoolXPub   sql=account_xpub

    Primary seqStateWalletId
    Foreign Wallet fk_wallet_seq_state seqStateWalletId
    deriving Show Generic

-- Mapping of pool addresses to indices, and the slot
-- when they were discovered.
SeqStateAddress
    seqStateAddressWalletId     W.WalletId     sql=wallet_id
    seqStateAddressSlot         W.SlotId       sql=slot

    seqStateAddressAddress      W.Address      sql=address
    seqStateAddressIndex        Word32         sql=address_ix
    seqStateAddressChangeChain  W.ChangeChain  sql=change_chain

    deriving Show Generic

-- Sequential scheme address discovery state
-- which belongs to a checkpoint.
SeqStateCheckpoint
    -- The wallet checkpoint (wallet_id, slot)
    seqStateCheckpointWalletId  W.WalletId        sql=wallet_id
    seqStateCheckpointSlot      W.SlotId          sql=slot

    UniqueSeqStateCheckpoint seqStateCheckpointWalletId seqStateCheckpointSlot
    Foreign Checkpoint fk_checkpoint_seq_state seqStateCheckpointWalletId seqStateCheckpointSlot
    deriving Show Generic

-- Sequential address discovery scheme -- pending change indexes
SeqStatePendingIx                                        sql=seq_state_pending
    seqStatePendingIxCheckpointId  SeqStateCheckpointId  sql=checkpoint_id
    seqStatePendingIxIndex         Word32                sql=pending_ix

    Primary seqStatePendingIxCheckpointId seqStatePendingIxIndex
    deriving Show Generic

-- Random scheme address discovery state
-- which does not belong to a particular checkpoint.
RndState
    rndStateWalletId        W.WalletId        sql=wallet_id
    rndStateAccountIndex    Word32            sql=account_ix

    Primary rndStateWalletId
    Foreign Wallet fk_wallet_rnd_state rndStateWalletId
    deriving Show Generic

-- The set of discovered addresses.
RndStateAddress
    rndStateAddressWalletId      W.WalletId        sql=wallet_id
    rndStateAddressSlot          W.SlotId          sql=slot
    rndStateAddressAccountIndex  Word32            sql=account_ix
    rndStateAddressIndex         Word32            sql=address_ix
    rndStateAddressAddress       W.Address         sql=address

    Primary
        rndStateAddressWalletId
        rndStateAddressSlot
        rndStateAddressAccountIndex
        rndStateAddressIndex
        rndStateAddressAddress
    deriving Show Generic

-- Random scheme address discovery state
-- which belongs a checkpoint.
RndStateCheckpoint
    -- The wallet checkpoint (wallet_id, slot)
    rndStateCheckpointWalletId  W.WalletId        sql=wallet_id
    rndStateCheckpointSlot      W.SlotId          sql=slot
    rndStateCheckpointGen       StdGen            sql=gen

    UniqueRndStateCheckpoint rndStateCheckpointWalletId rndStateCheckpointSlot
    Foreign Checkpoint fk_checkpoint_rnd_state rndStateCheckpointWalletId rndStateCheckpointSlot
    deriving Show Generic

-- The set of pending change addresses.
RndStatePendingAddress
    rndStatePendingAddressCheckpointId  RndStateCheckpointId  sql=checkpoint_id
    rndStatePendingAddressAccountIndex  Word32                sql=account_ix
    rndStatePendingAddressIndex         Word32                sql=address_ix
    rndStatePendingAddressAddress       W.Address             sql=address

    Primary
        rndStatePendingAddressCheckpointId
        rndStatePendingAddressAccountIndex
        rndStatePendingAddressIndex
        rndStatePendingAddressAddress
    deriving Show Generic

|]
