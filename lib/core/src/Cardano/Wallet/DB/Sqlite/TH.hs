{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright: © 2018-2020 IOHK
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

import Cardano.Slotting.Slot
    ( SlotNo )
import Cardano.Wallet.DB.Sqlite.Types
    ( BlockId, HDPassphrase, TxId, sqlSettings' )
import Data.Quantity
    ( Percentage (..) )
import Data.Text
    ( Text )
import Data.Time.Clock
    ( UTCTime )
import Data.Word
    ( Word16, Word32, Word64 )
import Database.Persist.Class
    ( AtLeastOneUniqueKey (..), OnlyOneUniqueKey (..) )
import Database.Persist.TH
    ( mkDeleteCascade, mkMigrate, mkPersist, persistLowerCase, share )
import GHC.Generics
    ( Generic (..) )
import Numeric.Natural
    ( Natural )
import System.Random
    ( StdGen )

import qualified Cardano.Wallet.Primitive.AddressDerivation as W
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
    walId                       W.WalletId                sql=wallet_id
    walCreationTime             UTCTime                   sql=creation_time
    walName                     Text                      sql=name
    walPassphraseLastUpdatedAt  UTCTime Maybe             sql=passphrase_last_updated_at
    walPassphraseScheme         W.PassphraseScheme Maybe  sql=passphrase_scheme

    Primary walId
    deriving Show Generic

-- The private key for each wallet. This is in a separate table simply so that
-- "SELECT * FROM wallet" won't print keys.
PrivateKey                             sql=private_key
    privateKeyWalletId  W.WalletId     sql=wallet_id
    privateKeyRootKey   B8.ByteString  sql=root
    privateKeyHash      B8.ByteString  sql=hash

    Primary privateKeyWalletId
    Foreign Wallet fk_wallet_private_key privateKeyWalletId ! ON DELETE CASCADE
    deriving Show Generic

-- Maps a transaction ID to its metadata (which is calculated when applying
-- blocks).
--
-- TxMeta is specific to a wallet because multiple wallets may have the same
-- transaction with different metadata values. The associated inputs and outputs
-- of the transaction are in the TxIn and TxOut tables.
TxMeta
    txMetaTxId         TxId         sql=tx_id
    txMetaWalletId     W.WalletId   sql=wallet_id
    txMetaStatus       W.TxStatus   sql=status
    txMetaDirection    W.Direction  sql=direction
    txMetaSlot         SlotNo       sql=slot
    txMetaBlockHeight  Word32       sql=block_height
    txMetaAmount       Natural      sql=amount

    Primary txMetaTxId txMetaWalletId
    Foreign Wallet fk_wallet_tx_meta txMetaWalletId ! ON DELETE CASCADE
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
    txInputSourceAmount   W.Coin        sql=source_amount

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

-- | A transaction withdrawal associated with TxMeta.
--
-- There is no wallet ID because these values depend only on the transaction,
-- not the wallet. txOutputTxId is referred to by TxMeta
TxWithdrawal
    txWithdrawalTxId    TxId                sql=tx_id
    txWithdrawalAmount  W.Coin              sql=amount
    txWithdrawalAccount W.ChimericAccount   sql=account

    Primary txWithdrawalTxId txWithdrawalAccount
    deriving Show Generic

-- A checkpoint for a given wallet is referred to by (wallet_id, slot).
-- Volatile checkpoint data such as AD state will refer to this table.
Checkpoint
    checkpointWalletId          W.WalletId   sql=wallet_id
    checkpointSlot              SlotNo       sql=slot
    checkpointHeaderHash        BlockId      sql=header_hash
    checkpointParentHash        BlockId      sql=parent_header_hash
    checkpointBlockHeight       Word32       sql=block_height
    checkpointGenesisHash       BlockId      sql=genesis_hash
    checkpointGenesisStart      UTCTime      sql=genesis_start
    checkpointFeePolicyUnused   Text         sql=fee_policy
    checkpointSlotLength        Word64       sql=slot_length
    checkpointEpochLength       Word32       sql=epoch_length
    checkpointTxMaxSizeUnused   Word16       sql=tx_max_size
    checkpointEpochStability    Word32       sql=epoch_stability
    checkpointActiveSlotCoeff   Double       sql=active_slot_coeff

    Primary checkpointWalletId checkpointSlot
    Foreign Wallet checkpoint checkpointWalletId ! ON DELETE CASCADE
    deriving Show Generic

ProtocolParameters
    protocolParametersWalletId              W.WalletId      sql=wallet_id
    protocolParametersFeePolicy             W.FeePolicy     sql=fee_policy
    protocolParametersTxMaxSize             Word16          sql=tx_max_size
    protocolParametersDecentralizationLevel Percentage      sql=decentralization_level
    protocolParametersDesiredNumberOfPools  Word16          sql=desired_pool_number
    protocolParametersMinimumUtxoValue      W.Coin          sql=minimum_utxo_value
    protocolParametersHardforkEpoch         W.EpochNo Maybe sql=hardfork_epoch
    Primary protocolParametersWalletId
    Foreign Wallet fk_wallet_protocol_parameters protocolParametersWalletId ! ON DELETE CASCADE
    deriving Show Generic

-- Track whether the wallet's stake key is registered or not.
StakeKeyCertificate
    stakeKeyCertWalletId             W.WalletId            sql=wallet_id
    stakeKeyCertSlot                 SlotNo                sql=slot
    stakeKeyCertType                 W.StakeKeyCertificate sql=type

    Primary stakeKeyCertWalletId stakeKeyCertSlot
    Foreign Wallet stakeKeyRegistration stakeKeyCertWalletId ! ON DELETE CASCADE
    deriving Show Generic

-- Store known delegation certificates for a particular wallet
DelegationCertificate
    certWalletId             W.WalletId     sql=wallet_id
    certSlot                 SlotNo         sql=slot
    certPoolId               W.PoolId Maybe sql=delegation

    Primary certWalletId certSlot
    Foreign Wallet delegationCertificate certWalletId ! ON DELETE CASCADE
    deriving Show Generic

-- Latest balance of the reward account associated with
-- the stake key of this wallet.
DelegationReward
    rewardWalletId           W.WalletId     sql=wallet_id
    rewardAccountBalance     Word64         sql=account_balance

    Primary rewardWalletId
    Foreign Wallet delegationReward rewardWalletId ! ON DELETE CASCADE
    deriving Show Generic

-- The UTxO for a given wallet checkpoint is a one-to-one mapping from TxIn ->
-- TxOut. This table does not need to refer to the TxIn or TxOut tables. All
-- necessary information for the UTxO is in this table.
UTxO                                sql=utxo

    -- The wallet checkpoint (wallet_id, slot)
    utxoWalletId        W.WalletId  sql=wallet_id
    utxoSlot            SlotNo      sql=slot

    -- TxIn
    utxoInputId         TxId        sql=input_tx_id
    utxoInputIndex      Word32      sql=input_index

    -- TxOut
    utxoOutputAddress   W.Address   sql=output_address
    utxoOutputCoin      W.Coin      sql=output_coin

    Primary utxoWalletId utxoSlot utxoInputId utxoInputIndex
    Foreign Checkpoint utxo utxoWalletId utxoSlot ! ON DELETE CASCADE
    deriving Show Generic

-- Sequential scheme address discovery state
-- which does not belong to a particular checkpoint.
SeqState
    seqStateWalletId        W.WalletId        sql=wallet_id
    seqStateExternalGap     W.AddressPoolGap  sql=external_gap
    seqStateInternalGap     W.AddressPoolGap  sql=internal_gap
    seqStateAccountXPub     B8.ByteString     sql=account_xpub
    seqStateRewardXPub      B8.ByteString     sql=reward_xpub

    Primary seqStateWalletId
    Foreign Wallet seq_state seqStateWalletId ! ON DELETE CASCADE
    deriving Show Generic

-- Mapping of pool addresses to indices, and the slot
-- when they were discovered.
SeqStateAddress
    seqStateAddressWalletId         W.WalletId         sql=wallet_id
    seqStateAddressSlot             SlotNo             sql=slot
    seqStateAddressAddress          W.Address          sql=address
    seqStateAddressIndex            Word32             sql=address_ix
    seqStateAddressAccountingStyle  W.AccountingStyle  sql=accounting_style
    seqStateAddressStatus           W.AddressState     sql=status

    Primary
        seqStateAddressWalletId
        seqStateAddressSlot
        seqStateAddressAddress
        seqStateAddressIndex
        seqStateAddressAccountingStyle
    Foreign Checkpoint seq_state_address seqStateAddressWalletId seqStateAddressSlot ! ON DELETE CASCADE
    deriving Show Generic

-- Sequential address discovery scheme -- pending change indexes
SeqStatePendingIx                            sql=seq_state_pending
    seqStatePendingWalletId     W.WalletId   sql=wallet_id
    seqStatePendingIxIndex      Word32       sql=pending_ix

    Primary seqStatePendingWalletId seqStatePendingIxIndex
    Foreign Wallet seq_state_address_pending seqStatePendingWalletId ! ON DELETE CASCADE
    deriving Show Generic

-- Random scheme address discovery state
-- which does not belong to a particular checkpoint.
RndState
    rndStateWalletId        W.WalletId        sql=wallet_id
    rndStateAccountIndex    Word32            sql=account_ix
    rndStateGen             StdGen            sql=gen
    rndStateHdPassphrase    HDPassphrase      sql=hd_passphrase

    Primary rndStateWalletId
    Foreign Wallet rnd_state rndStateWalletId ! ON DELETE CASCADE
    deriving Show Generic

-- The set of discovered addresses.
RndStateAddress
    rndStateAddressWalletId      W.WalletId        sql=wallet_id
    rndStateAddressSlot          SlotNo            sql=slot
    rndStateAddressAccountIndex  Word32            sql=account_ix
    rndStateAddressIndex         Word32            sql=address_ix
    rndStateAddressAddress       W.Address         sql=address
    rndStateAddressStatus        W.AddressState    sql=status

    Primary
        rndStateAddressWalletId
        rndStateAddressSlot
        rndStateAddressAccountIndex
        rndStateAddressIndex
        rndStateAddressAddress
    Foreign Checkpoint rnd_state_address rndStateAddressWalletId rndStateAddressSlot ! ON DELETE CASCADE
    deriving Show Generic

-- The set of pending change addresses.
RndStatePendingAddress
    rndStatePendingAddressWalletId      W.WalletId  sql=wallet_id
    rndStatePendingAddressAccountIndex  Word32      sql=account_ix
    rndStatePendingAddressIndex         Word32      sql=address_ix
    rndStatePendingAddressAddress       W.Address   sql=address

    Primary
        rndStatePendingAddressWalletId
        rndStatePendingAddressAccountIndex
        rndStatePendingAddressIndex
        rndStatePendingAddressAddress
    Foreign Wallet rnd_state_pending_address rndStatePendingAddressWalletId ! ON DELETE CASCADE
    deriving Show Generic
|]
