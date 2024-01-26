{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright: © 2018-2023 IOHK
-- License: Apache-2.0
--
-- Auto-generated Sqlite & Persistent machinery via Template-Haskell. This has
-- been moved into a separate file so that we can treat it slightly differently
-- when computing code-coverage.
--
-- More than 6K lines end-up being generated from the instructions below! As a
-- result, we're going to ignore code-coverage on the following module and, no
-- hand-written functions should be written in this module!

module Cardano.Wallet.DB.Sqlite.Schema where

import Prelude

import Cardano.Address.Script
    ( Cosigner
    , Script
    )
import Cardano.Slotting.Slot
    ( SlotNo
    )
import Cardano.Wallet.Address.Discovery.Shared
    ( CredentialType
    )
import Cardano.Wallet.DB.Sqlite.Types
    ( BlockHeight
    , BlockId
    , HDPassphrase
    , TxId
    , TxSubmissionStatusEnum (..)
    , sqlSettings'
    )
import Cardano.Wallet.DB.Store.UTxOHistory.Model
    ( Pruned
    , Spent
    )
import Cardano.Wallet.Primitive.Types
    ( Slot
    )
import Data.Percentage
    ( Percentage (..)
    )
import Data.Text
    ( Text
    )
import Data.Time.Clock
    ( UTCTime
    )
import Data.Word
    ( Word16
    , Word32
    , Word64
    , Word8
    )
import Database.Persist.TH
    ( mkMigrate
    , mkPersist
    , persistLowerCase
    , share
    )
import GHC.Generics
    ( Generic (..)
    )
import System.Random
    ( StdGen
    )

import qualified Cardano.Wallet.Address.Derivation as W
import qualified Cardano.Wallet.Address.Discovery as W
import qualified Cardano.Wallet.Address.Discovery.Sequential as W
import qualified Cardano.Wallet.Primitive.Passphrase.Types as W
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Cardano.Wallet.Primitive.Types.Address as W
import qualified Cardano.Wallet.Primitive.Types.AssetName as W
import qualified Cardano.Wallet.Primitive.Types.Coin as W
import qualified Cardano.Wallet.Primitive.Types.TokenPolicyId as W
import qualified Cardano.Wallet.Primitive.Types.TokenQuantity as W
import qualified Cardano.Wallet.Primitive.Types.Tx as W
import qualified Cardano.Wallet.Primitive.Types.Tx.TxMeta as W
import qualified Data.ByteString.Char8 as B8

share
    [ mkPersist sqlSettings'
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
    walGenesisHash              BlockId                   sql=genesis_hash
    walGenesisStart             UTCTime                   sql=genesis_start

    Primary walId
    deriving Show Generic

-- The private key for each wallet. This is in a separate table simply so that
-- "SELECT * FROM wallet" won't print keys.
PrivateKey                             sql=private_key
    privateKeyWalletId  W.WalletId     sql=wallet_id
    privateKeyRootKey   B8.ByteString  sql=root
    privateKeyHash      B8.ByteString  sql=hash

    Primary privateKeyWalletId
    Foreign Wallet OnDeleteCascade fk_wallet_private_key privateKeyWalletId
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
    txMetaWalletId          W.WalletId          sql=wallet_id
    txMetaStatus            W.TxStatus          sql=status
    txMetaDirection         W.Direction         sql=direction
    txMetaSlot              SlotNo              sql=slot
    txMetaBlockHeight       Word32              sql=block_height
    txMetaAmount            W.Coin              sql=amount
    txMetadata              W.TxMetadata Maybe  sql=data
    txMetaSlotExpires       SlotNo Maybe        sql=slot_expires
    txMetaFee               Word64 Maybe        sql=fee
    txMetaScriptValidity    Bool Maybe          sql=script_validity

    Primary txMetaTxId txMetaWalletId
    Foreign Wallet OnDeleteCascade fk_wallet_tx_meta txMetaWalletId
    deriving Show Generic Eq

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
    deriving Show Generic Eq

-- A collateral input associated with TxMeta.
--
-- There is no wallet ID because these values depend only on the transaction,
-- not the wallet. txCollateralTxId is referred to by TxMeta
TxCollateral
    txCollateralTxId         TxId   sql=tx_id
    txCollateralOrder        Int    sql=order
    txCollateralSourceTxId   TxId   sql=source_tx_id
    txCollateralSourceIndex  Word32 sql=source_index
    txCollateralSourceAmount W.Coin sql=source_amount

    Primary txCollateralTxId txCollateralSourceTxId txCollateralSourceIndex
    deriving Show Generic Eq

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
    deriving Show Generic Eq

-- A token quantity associated with a TxOut.
--
-- Each row within TxOut can have many associated rows within TxOutToken.
-- Each row within TxOutToken refers to just a single row within TxOut.
--
TxOutToken
    txOutTokenTxId      TxId              sql=tx_id
    txOutTokenTxIndex   Word32            sql=tx_index
    txOutTokenPolicyId  W.TokenPolicyId   sql=token_policy_id
    txOutTokenName      W.AssetName       sql=token_name
    txOutTokenQuantity  W.TokenQuantity   sql=token_quantity

    Primary txOutTokenTxId txOutTokenTxIndex txOutTokenPolicyId txOutTokenName
    Foreign TxOut OnDeleteCascade txOut txOutTokenTxId txOutTokenTxIndex
    deriving Show Generic Eq

-- A transaction collateral return output associated with a TxMeta.
--
-- There is no wallet ID because these values depend only on the transaction,
-- not the wallet. A txCollateralOutTxId is referred to by a TxMeta.
--
-- Unlike ordinary outputs, a given transaction may have at most one collateral
-- return output. Therefore there is no index field.
--
TxCollateralOut
    txCollateralOutTxId    TxId      sql=tx_id
    txCollateralOutAddress W.Address sql=address
    txCollateralOutAmount  W.Coin    sql=amount

    Primary txCollateralOutTxId
    deriving Show Generic Eq

-- A token quantity associated with a TxCollateralOut.
--
-- Each row within TxCollateralOut can have many associated rows within
-- TxCollateralOutToken.
--
-- Each row within TxCollateralOutToken refers to just a single row
-- within TxCollateralOut.
--
-- Unlike ordinary outputs, a given transaction may have at most one collateral
-- return output. Therefore there is no index field.
--
TxCollateralOutToken
    txCollateralOutTokenTxId     TxId            sql=tx_id
    txCollateralOutTokenPolicyId W.TokenPolicyId sql=token_policy_id
    txCollateralOutTokenName     W.AssetName     sql=token_name
    txCollateralOutTokenQuantity W.TokenQuantity sql=token_quantity

    Primary txCollateralOutTokenTxId txCollateralOutTokenPolicyId txCollateralOutTokenName
    Foreign TxCollateralOut OnDeleteCascade txCollateralOut txCollateralOutTokenTxId
    deriving Show Generic Eq

-- | A transaction withdrawal associated with TxMeta.
--
-- There is no wallet ID because these values depend only on the transaction,
-- not the wallet. txOutputTxId is referred to by TxMeta
TxWithdrawal
    txWithdrawalTxId    TxId                sql=tx_id
    txWithdrawalAmount  W.Coin              sql=amount
    txWithdrawalAccount W.RewardAccount     sql=account

    Primary txWithdrawalTxId txWithdrawalAccount
    deriving Show Generic Eq

-- A checkpoint for a given wallet is referred to by (wallet_id, slot).
-- Volatile checkpoint data such as AD state will refer to this table.
Checkpoint
    checkpointWalletId          W.WalletId   sql=wallet_id
    checkpointSlot              SlotNo       sql=slot
    checkpointHeaderHash        BlockId      sql=header_hash
    checkpointParentHash        BlockId      sql=parent_header_hash
    checkpointBlockHeight       Word32       sql=block_height

    Primary checkpointWalletId checkpointSlot
    Foreign Wallet OnDeleteCascade checkpoint checkpointWalletId
    deriving Show Generic

ProtocolParameters
    protocolParametersWalletId              W.WalletId      sql=wallet_id
    protocolParametersFeePolicy             W.FeePolicy     sql=fee_policy
    protocolParametersTxMaxSize             Word16          sql=tx_max_size
    protocolParametersDecentralizationLevel Percentage      sql=decentralization_level
    protocolParametersDesiredNumberOfPools  Word16          sql=desired_pool_number
    protocolParametersMinimumUtxoValue      W.Coin          sql=minimum_utxo_value
    protocolParametersHardforkEpoch         W.EpochNo Maybe sql=hardfork_epoch
    protocolParametersKeyDeposit            W.Coin          sql=key_deposit

    Primary protocolParametersWalletId
    Foreign Wallet OnDeleteCascade fk_wallet_protocol_parameters protocolParametersWalletId
    deriving Show Generic

-- Latest balance of the reward account associated with
-- the stake key of this wallet.
DelegationReward
    rewardWalletId           W.WalletId     sql=wallet_id
    rewardAccountBalance     Word64         sql=account_balance

    Primary rewardWalletId
    Foreign Wallet OnDeleteCascade delegationReward rewardWalletId
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
    Foreign Checkpoint OnDeleteCascade utxo utxoWalletId utxoSlot
    deriving Show Generic

-- A token quantity associated with a UTxO entry.
--
-- Each row within UTxO can have many associated rows within UTxOToken.
-- Each row within UTxOToken refers to just a single row within UTxO.
--
UTxOToken                               sql=utxo_token
    utxoTokenWalletId  W.WalletId       sql=wallet_id
    utxoTokenSlot      SlotNo           sql=slot
    utxoTokenTxId      TxId             sql=tx_id
    utxoTokenTxIndex   Word32           sql=tx_index
    utxoTokenPolicyId  W.TokenPolicyId  sql=token_policy_id
    utxoTokenName      W.AssetName      sql=token_name
    utxoTokenQuantity  W.TokenQuantity  sql=token_quantity

    Primary
        utxoTokenWalletId
        utxoTokenSlot
        utxoTokenTxId
        utxoTokenTxIndex
        utxoTokenPolicyId
        utxoTokenName

    -- FIXME:
    --
    -- Ideally, we'd like to define a foreign key constraint on the UTxO table,
    -- as each row in UTxOToken refers to a unique row in UTxO.
    --
    -- Unfortunately, there's a bug in our version of persistent that breaks
    -- foreign key constraints where the foreign table has a manually-defined
    -- table name. In the case of the UTxO, the table name is "utxo", and not
    -- "u_tx_o" (which is what the auto-generated table name would be).
    --
    -- To work around this bug, we instead define a foreign key constraint on
    -- the Checkpoint table. Under the current design of rollback, this gives
    -- equivalent behaviour, as we always delete UTxO entries by deleting their
    -- parent checkpoints.
    --
    Foreign Checkpoint OnDeleteCascade utxot utxoTokenWalletId utxoTokenSlot

    deriving Show Generic

-- Sequential scheme address discovery state
-- which does not belong to a particular checkpoint.
SeqState
    seqStateWalletId          W.WalletId           sql=wallet_id
    seqStateExternalGap       W.AddressPoolGap     sql=external_gap
    seqStateInternalGap       W.AddressPoolGap     sql=internal_gap
    seqStateAccountXPub       B8.ByteString        sql=account_xpub
    seqStatePolicyXPub        B8.ByteString Maybe  sql=policy_xpub
    seqStateRewardXPub        B8.ByteString        sql=reward_xpub
    seqStateDerivationPrefix  W.DerivationPrefix   sql=derivation_prefix
    seqStateChangeAddrMode    W.ChangeAddressMode  sql=change_addr_mode

    Primary seqStateWalletId
    Foreign Wallet OnDeleteCascade seq_state seqStateWalletId
    deriving Show Generic

-- Mapping of pool addresses to indices, and the slot
-- when they were discovered.
SeqStateAddress
    seqStateAddressWalletId         W.WalletId         sql=wallet_id
    seqStateAddressSlot             SlotNo             sql=slot
    seqStateAddressAddress          W.Address          sql=address
    seqStateAddressIndex            Word32             sql=address_ix
    seqStateAddressRole             W.Role             sql=role
    seqStateAddressStatus           W.AddressState     sql=status

    Primary
        seqStateAddressWalletId
        seqStateAddressSlot
        seqStateAddressAddress
        seqStateAddressIndex
        seqStateAddressRole
    Foreign Checkpoint OnDeleteCascade seq_state_address seqStateAddressWalletId seqStateAddressSlot
    deriving Show Generic

-- Sequential address discovery scheme -- pending change indexes
SeqStatePendingIx                            sql=seq_state_pending
    seqStatePendingWalletId     W.WalletId   sql=wallet_id
    seqStatePendingIxIndex      Word32       sql=pending_ix

    Primary seqStatePendingWalletId seqStatePendingIxIndex
    Foreign Wallet OnDeleteCascade seq_state_address_pending seqStatePendingWalletId
    deriving Show Generic

-- Random scheme address discovery state
-- which does not belong to a particular checkpoint.
RndState
    rndStateWalletId        W.WalletId        sql=wallet_id
    rndStateAccountIndex    Word32            sql=account_ix
    rndStateGen             StdGen            sql=gen
    rndStateHdPassphrase    HDPassphrase      sql=hd_passphrase

    Primary rndStateWalletId
    Foreign Wallet OnDeleteCascade rnd_state rndStateWalletId
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
    Foreign Checkpoint OnDeleteCascade rnd_state_address rndStateAddressWalletId rndStateAddressSlot
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
    Foreign Wallet OnDeleteCascade rnd_state_pending_address rndStatePendingAddressWalletId
    deriving Show Generic

-- Shared Wallet
SharedState
    sharedStateWalletId                 W.WalletId                sql=wallet_id
    sharedStateAccountXPub              B8.ByteString             sql=account_xpub
    sharedStateScriptGap                W.AddressPoolGap          sql=pool_gap
    sharedStatePaymentScript            (Script Cosigner)         sql=payment_script
    sharedStateDelegationScript         (Script Cosigner) Maybe   sql=delegation_script
    sharedStateRewardAccount            W.RewardAccount Maybe     sql=reward_account
    sharedStateDerivationPrefix         W.DerivationPrefix        sql=derivation_prefix
    sharedStateChangeAddrMode           W.ChangeAddressMode       sql=change_addr_mode

    Primary sharedStateWalletId
    Foreign Wallet OnDeleteCascade shared_state sharedStateWalletId
    deriving Show Generic

-- Shared Wallet -- pending change indexes
SharedStatePendingIx                            sql=shared_state_pending
    sharedStatePendingWalletId     W.WalletId   sql=wallet_id
    sharedStatePendingIxIndex      Word32       sql=pending_ix

    Primary sharedStatePendingWalletId sharedStatePendingIxIndex
    Foreign Wallet OnDeleteCascade shared_state_address_pending sharedStatePendingWalletId
    deriving Show Generic

CosignerKey
    cosignerKeyWalletId                  W.WalletId                sql=wallet_id
    cosignerKeyCredential                CredentialType            sql=credential
    cosignerKeyAccountXPub               B8.ByteString             sql=account_xpub
    cosignerKeyIndex                     Word8                     sql=cosigner_index

    Primary
        cosignerKeyWalletId
        cosignerKeyCredential
        cosignerKeyIndex
    Foreign Wallet OnDeleteCascade cosigner_key cosignerKeyWalletId
    deriving Show Generic

CBOR
    cborTxId       TxId             sql=tx_id
    cborTxCBOR     B8.ByteString    sql=tx_cbor
    cborTxEra      Word16           sql=tx_era

    Primary cborTxId
    deriving Show Generic Eq

Submissions
    submissionTxId                  TxId                sql=tx_id
    submissionTx                    W.SealedTx          sql=tx
    submissionExpiration            SlotNo              sql=expiration
    submissionAcceptance            (Maybe SlotNo)      sql=acceptance
    submissionWallet                W.WalletId          sql=wallet_id
    submissionStatus                TxSubmissionStatusEnum sql=status
    submissionMetaSlot              SlotNo              sql=slot
    submissionMetaBlockHeight       BlockHeight         sql=block_height
    submissionMetaAmount            W.Coin              sql=amount
    submissionMetaDirection         W.Direction         sql=direction
    submissionMetaResubmitted       SlotNo              sql=resubmitted

    Primary submissionTxId
    deriving Show Generic Eq

SubmissionsSlots
    submissionsSlotsFinality SlotNo sql=finality
    submissionsSlotsTip SlotNo sql=tip
    submissionsSlotsWallet W.WalletId sql=wallet_id

    Primary submissionsSlotsWallet

DeltaUTxOValue
    deltaUTxOValueWalletId W.WalletId sql=wallet_id
    deltaUTxOValueCreation Slot sql=slot
    deltaUTxOValueSpent Spent nullable sql=spent
    deltaUTxOValueTxInTx TxId sql=tx_in_tx
    deltaUTxOValueTxInIx Word32 sql=tx_in_ix
    deltaUTxOValueTxOut B8.ByteString sql=tx_out
    deltaUTxOValueBoot Bool sql=boot

    Primary  deltaUTxOValueWalletId deltaUTxOValueTxInTx deltaUTxOValueTxInIx deltaUTxOValueBoot
    deriving Show Generic Eq

DeltaUTxOSlots
    deltaUTxOSlotsWallet W.WalletId sql=wallet_id
    deltaUTxOSlotsFinality Pruned nullable sql=finality
    deltaUTxOSlotsTip Slot sql=tip

    Primary deltaUTxOSlotsWallet
    deriving Show Generic Eq
|]
