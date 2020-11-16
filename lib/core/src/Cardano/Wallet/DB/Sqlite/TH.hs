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
    walId                       W.AccountId               sql=wallet_id
    walCreationTime             UTCTime                   sql=creation_time
    walName                     Text                      sql=name
    walPassphraseLastUpdatedAt  UTCTime Maybe             sql=passphrase_last_updated_at
    walPassphraseScheme         W.PassphraseScheme Maybe  sql=passphrase_scheme

    Primary walId
    deriving Show Generic

-- The private key for each wallet. This is in a separate table simply so that
-- "SELECT * FROM wallet" won't print keys.
PrivateKey                             sql=private_key
    privateKeyAccountId W.AccountId    sql=wallet_id
    privateKeyRootKey   B8.ByteString  sql=root
    privateKeyHash      B8.ByteString  sql=hash

    Primary privateKeyAccountId
    Foreign Wallet fk_wallet_private_key privateKeyAccountId ! ON DELETE CASCADE
    deriving Show Generic

-- Maps a transaction ID to its metadata (which is calculated when applying
-- blocks).
--
-- TxMeta is specific to a wallet because multiple wallets may have the same
-- transaction with different metadata values. The associated inputs and outputs
-- of the transaction are in the TxIn and TxOut tables.
--
-- Transactions with status=Pending have an expiry slot.
-- If not accepted on the chain before the expiry slot they
-- will be removed from the pending set and get status=Expired.
TxMeta
    txMetaTxId              TxId                sql=tx_id
    txMetaAccountId         W.AccountId         sql=wallet_id
    txMetaStatus            W.TxStatus          sql=status
    txMetaDirection         W.Direction         sql=direction
    txMetaSlot              SlotNo              sql=slot
    txMetaBlockHeight       Word32              sql=block_height
    txMetaAmount            Natural             sql=amount
    txMetaData              W.TxMetadata Maybe  sql=data
    txMetaSlotExpires       SlotNo Maybe        sql=slot_expires

    Primary txMetaTxId txMetaAccountId
    Foreign Wallet fk_wallet_tx_meta txMetaAccountId ! ON DELETE CASCADE
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
    checkpointAccountId         W.AccountId  sql=wallet_id
    checkpointSlot              SlotNo       sql=slot
    checkpointHeaderHash        BlockId      sql=header_hash
    checkpointParentHash        BlockId      sql=parent_header_hash
    checkpointBlockHeight       Word32       sql=block_height
    checkpointGenesisHash       BlockId      sql=genesis_hash
    checkpointGenesisStart      UTCTime      sql=genesis_start
    checkpointFeePolicyUnused   Text         sql=fee_policy
    checkpointSlotLengthUnused  Word64       sql=slot_length
    checkpointEpochLengthUnused Word32       sql=epoch_length
    checkpointTxMaxSizeUnused   Word16       sql=tx_max_size
    checkpointEpochStability    Word32       sql=epoch_stability
    checkpointActiveSlotCoeffUnused Double       sql=active_slot_coeff

    Primary checkpointAccountId checkpointSlot
    Foreign Wallet checkpoint checkpointAccountId ! ON DELETE CASCADE
    deriving Show Generic

ProtocolParameters
    protocolParametersAccountId             W.AccountId     sql=wallet_id
    protocolParametersFeePolicy             W.FeePolicy     sql=fee_policy
    protocolParametersTxMaxSize             Word16          sql=tx_max_size
    protocolParametersDecentralizationLevel Percentage      sql=decentralization_level
    protocolParametersDesiredNumberOfPools  Word16          sql=desired_pool_number
    protocolParametersMinimumUtxoValue      W.Coin          sql=minimum_utxo_value
    protocolParametersHardforkEpoch         W.EpochNo Maybe sql=hardfork_epoch
    Primary protocolParametersAccountId
    Foreign Wallet fk_wallet_protocol_parameters protocolParametersAccountId ! ON DELETE CASCADE
    deriving Show Generic

-- Track whether the wallet's stake key is registered or not.
StakeKeyCertificate
    stakeKeyCertAccountId            W.AccountId           sql=wallet_id
    stakeKeyCertSlot                 SlotNo                sql=slot
    stakeKeyCertType                 W.StakeKeyCertificate sql=type

    Primary stakeKeyCertAccountId stakeKeyCertSlot
    Foreign Wallet stakeKeyRegistration stakeKeyCertAccountId ! ON DELETE CASCADE
    deriving Show Generic

-- Store known delegation certificates for a particular wallet
DelegationCertificate
    certAccountId            W.AccountId    sql=wallet_id
    certSlot                 SlotNo         sql=slot
    certPoolId               W.PoolId Maybe sql=delegation

    Primary certAccountId certSlot
    Foreign Wallet delegationCertificate certAccountId ! ON DELETE CASCADE
    deriving Show Generic

-- Latest balance of the reward account associated with
-- the stake key of this wallet.
DelegationReward
    rewardAccountId          W.AccountId    sql=wallet_id
    rewardAccountBalance     Word64         sql=account_balance

    Primary rewardAccountId
    Foreign Wallet delegationReward rewardAccountId ! ON DELETE CASCADE
    deriving Show Generic

-- The UTxO for a given wallet checkpoint is a one-to-one mapping from TxIn ->
-- TxOut. This table does not need to refer to the TxIn or TxOut tables. All
-- necessary information for the UTxO is in this table.
UTxO                                sql=utxo

    -- The wallet checkpoint (wallet_id, slot)
    utxoAccountId       W.AccountId sql=wallet_id
    utxoSlot            SlotNo      sql=slot

    -- TxIn
    utxoInputId         TxId        sql=input_tx_id
    utxoInputIndex      Word32      sql=input_index

    -- TxOut
    utxoOutputAddress   W.Address   sql=output_address
    utxoOutputCoin      W.Coin      sql=output_coin

    Primary utxoAccountId utxoSlot utxoInputId utxoInputIndex
    Foreign Checkpoint utxo utxoAccountId utxoSlot ! ON DELETE CASCADE
    deriving Show Generic

-- Sequential scheme address discovery state
-- which does not belong to a particular checkpoint.
SeqState
    seqStateAccountId         W.AccountId        sql=wallet_id
    seqStateExternalGap       W.AddressPoolGap   sql=external_gap
    seqStateInternalGap       W.AddressPoolGap   sql=internal_gap
    seqStateAccountXPub       B8.ByteString      sql=account_xpub
    seqStateRewardXPub        B8.ByteString      sql=reward_xpub
    seqStateDerivationPrefix  W.DerivationPrefix sql=derivation_prefix

    Primary seqStateAccountId
    Foreign Wallet seq_state seqStateAccountId ! ON DELETE CASCADE
    deriving Show Generic

-- Mapping of pool addresses to indices, and the slot
-- when they were discovered.
SeqStateAddress
    seqStateAddressAccountId        W.AccountId        sql=wallet_id
    seqStateAddressSlot             SlotNo             sql=slot
    seqStateAddressAddress          W.Address          sql=address
    seqStateAddressIndex            Word32             sql=address_ix
    seqStateAddressAccountingStyle  W.AccountingStyle  sql=accounting_style
    seqStateAddressStatus           W.AddressState     sql=status

    Primary
        seqStateAddressAccountId
        seqStateAddressSlot
        seqStateAddressAddress
        seqStateAddressIndex
        seqStateAddressAccountingStyle
    Foreign Checkpoint seq_state_address seqStateAddressAccountId seqStateAddressSlot ! ON DELETE CASCADE
    deriving Show Generic

-- Sequential address discovery scheme -- pending change indexes
SeqStatePendingIx                            sql=seq_state_pending
    seqStatePendingAccountId    W.AccountId  sql=wallet_id
    seqStatePendingIxIndex      Word32       sql=pending_ix

    Primary seqStatePendingAccountId seqStatePendingIxIndex
    Foreign Wallet seq_state_address_pending seqStatePendingAccountId ! ON DELETE CASCADE
    deriving Show Generic

-- Random scheme address discovery state
-- which does not belong to a particular checkpoint.
RndState
    rndStateAccountId       W.AccountId        ql=wallet_id
    rndStateAccountIndex    Word32            sql=account_ix
    rndStateGen             StdGen            sql=gen
    rndStateHdPassphrase    HDPassphrase      sql=hd_passphrase

    Primary rndStateAccountId
    Foreign Wallet rnd_state rndStateAccountId ! ON DELETE CASCADE
    deriving Show Generic

-- The set of discovered addresses.
RndStateAddress
    rndStateAddressAccountId     W.AccountId       sql=wallet_id
    rndStateAddressSlot          SlotNo            sql=slot
    rndStateAddressAccountIndex  Word32            sql=account_ix
    rndStateAddressIndex         Word32            sql=address_ix
    rndStateAddressAddress       W.Address         sql=address
    rndStateAddressStatus        W.AddressState    sql=status

    Primary
        rndStateAddressAccountId
        rndStateAddressSlot
        rndStateAddressAccountIndex
        rndStateAddressIndex
        rndStateAddressAddress
    Foreign Checkpoint rnd_state_address rndStateAddressAccountId rndStateAddressSlot ! ON DELETE CASCADE
    deriving Show Generic

-- The set of pending change addresses.
RndStatePendingAddress
    rndStatePendingAddressAccountId     W.AccountId  sql=wallet_id
    rndStatePendingAddressAccountIndex  Word32      sql=account_ix
    rndStatePendingAddressIndex         Word32      sql=address_ix
    rndStatePendingAddressAddress       W.Address   sql=address

    Primary
        rndStatePendingAddressAccountId
        rndStatePendingAddressAccountIndex
        rndStatePendingAddressIndex
        rndStatePendingAddressAddress
    Foreign Wallet rnd_state_pending_address rndStatePendingAddressAccountId ! ON DELETE CASCADE
    deriving Show Generic
|]
