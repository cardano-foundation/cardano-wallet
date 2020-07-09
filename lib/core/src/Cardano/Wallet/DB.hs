{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Database / Persistence layer for the wallet backend. This is where we define
-- the interface allowing us to store and fetch various data on our wallets.

module Cardano.Wallet.DB
    ( -- * Interface
      DBLayer (..)
    , DBFactory (..)
    , PrimaryKey(..)
    , cleanDB

      -- * Checkpoints
    , sparseCheckpoints

      -- * Errors
    , ErrRemovePendingTx (..)
    , ErrNoSuchWallet(..)
    , ErrWalletAlreadyExists(..)
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPrv )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..) )
import Cardano.Wallet.Primitive.Model
    ( Wallet )
import Cardano.Wallet.Primitive.Types
    ( BlockHeader
    , DelegationCertificate
    , Hash
    , ProtocolParameters
    , Range (..)
    , SlotNo (..)
    , SortOrder (..)
    , TransactionInfo
    , Tx (..)
    , TxMeta
    , TxStatus
    , WalletId
    , WalletMetadata
    )
import Control.Monad.Fail
    ( MonadFail )
import Control.Monad.IO.Class
    ( MonadIO )
import Control.Monad.Trans.Except
    ( ExceptT, runExceptT )
import Data.Quantity
    ( Quantity (..) )
import Data.Word
    ( Word32, Word64 )
import Numeric.Natural
    ( Natural )

import qualified Data.List as L

-- | Instantiate database layers at will
data DBFactory m s k = DBFactory
    { withDatabase :: forall a. WalletId -> (DBLayer m s k -> IO a) -> IO a
        -- ^ Creates a new or use an existing database, maintaining an open
        -- connection so long as necessary

    , removeDatabase :: WalletId -> IO ()
        -- ^ Erase any trace of the database

    , listDatabases :: IO [WalletId]
        -- ^ List existing wallet database found on disk.
    }

-- | A Database interface for storing various things in a DB. In practice,
-- we'll need some extra contraints on the wallet state that allows us to
-- serialize and unserialize it (e.g. @forall s. (Serialize s) => ...@)
--
-- NOTE:
--
-- We can't use record accessors on the DBLayer as it carries an existential
-- within its constructor. We are forced to pattern-match on the `DBLayer`
-- record type in order to be able to use its methods in any context. With
-- NamedFieldPuns, or RecordWildCards, this can be quite easy:
--
-- @
-- myFunction DBLayer{..} = do
--     ...
--
-- myOtherFunction DBLayer{atomically,initializeWallet} = do
--     ...
-- @
--
-- Alternatively, in some other context where the database may not be a function
-- argument but come from a different source, it is possible to simply rely on
-- 'Data.Function.(&)' to easily pattern match on it:
--
-- @
-- myFunction arg0 arg1 = db & \DBLayer{..} -> do
--     ...
--   where
--     db = ...
-- @
--
-- Note that it isn't possible to simply use a @where@ clause or a @let@ binding
-- here as the semantic for those are slightly different: we really need a
-- pattern match here!
data DBLayer m s k = forall stm. (MonadIO stm, MonadFail stm) => DBLayer
    { initializeWallet
        :: PrimaryKey WalletId
        -> Wallet s
        -> WalletMetadata
        -> [(Tx, TxMeta)]
        -> ProtocolParameters
        -> ExceptT ErrWalletAlreadyExists stm ()
        -- ^ Initialize a database entry for a given wallet. 'putCheckpoint',
        -- 'putWalletMeta', 'putTxHistory' or 'putProtocolParameters' will
        -- actually all fail if they are called _first_ on a wallet.

    , removeWallet
        :: PrimaryKey WalletId
        -> ExceptT ErrNoSuchWallet stm ()
        -- ^ Remove a given wallet and all its associated data (checkpoints,
        -- metadata, tx history ...)

    , listWallets
        :: stm [PrimaryKey WalletId]
        -- ^ Get the list of all known wallets in the DB, possibly empty.

    , putCheckpoint
        :: PrimaryKey WalletId
        -> Wallet s
        -> ExceptT ErrNoSuchWallet stm ()
        -- ^ Replace the current checkpoint for a given wallet. We do not handle
        -- rollbacks yet, and therefore only stores the latest available
        -- checkpoint.
        --
        -- If the wallet doesn't exist, this operation returns an error.

    , readCheckpoint
        :: PrimaryKey WalletId
        -> stm (Maybe (Wallet s))
        -- ^ Fetch the most recent checkpoint of a given wallet.
        --
        -- Return 'Nothing' if there's no such wallet.

    , listCheckpoints
        :: PrimaryKey WalletId
        -> stm [BlockHeader]
        -- ^ List all known checkpoint tips, ordered by slot ids from the oldest
        -- to the newest.

    , putWalletMeta
        :: PrimaryKey WalletId
        -> WalletMetadata
        -> ExceptT ErrNoSuchWallet stm ()
        -- ^ Replace an existing wallet metadata with the given one.
        --
        -- If the wallet doesn't exist, this operation returns an error

    , readWalletMeta
        :: PrimaryKey WalletId
        -> stm (Maybe WalletMetadata)
        -- ^ Fetch a wallet metadata, if they exist.
        --
        -- Return 'Nothing' if there's no such wallet.

    , isStakeKeyRegistered
        :: PrimaryKey WalletId
        -> ExceptT ErrNoSuchWallet stm Bool

    , putDelegationCertificate
        :: PrimaryKey WalletId
        -> DelegationCertificate
        -> SlotNo
        -> ExceptT ErrNoSuchWallet stm ()
        -- ^ Binds a stake pool id to a wallet. This will have an influence on
        -- the wallet metadata: the last known certificate will indicate to
        -- which pool a wallet is currently delegating.
        --
        -- This is done separately from 'putWalletMeta' because certificate
        -- declarations are:
        --
        -- 1. Stored on-chain.
        -- 2. Affected by rollbacks (or said differently, tied to a 'SlotNo').

    , putDelegationRewardBalance
        :: PrimaryKey WalletId
        -> Quantity "lovelace" Word64
        -> ExceptT ErrNoSuchWallet stm ()
        -- ^ Store the latest known reward account balance.
        --
        -- This is separate from checkpoints because the data corresponds to the
        -- node tip.
        -- This is separate from putWalletMeta because it's not meta data

    , readDelegationRewardBalance
        :: PrimaryKey WalletId
        -> stm (Quantity "lovelace" Word64)
        -- ^ Get the reward account balance.
        --
        -- Returns zero if the wallet isn't found or if wallet hasn't delegated
        -- stake.

    , putTxHistory
        :: PrimaryKey WalletId
        -> [(Tx, TxMeta)]
        -> ExceptT ErrNoSuchWallet stm ()
        -- ^ Augments the transaction history for a known wallet.
        --
        -- If an entry for a particular transaction already exists it is not
        -- altered nor merged (just ignored).
        --
        -- If the wallet doesn't exist, this operation returns an error.

    , readTxHistory
        :: PrimaryKey WalletId
        -> Maybe (Quantity "lovelace" Natural)
        -> SortOrder
        -> Range SlotNo
        -> Maybe TxStatus
        -> stm [TransactionInfo]
        -- ^ Fetch the current transaction history of a known wallet, ordered by
        -- descending slot number.
        --
        -- Returns an empty list if the wallet isn't found.

    , getTx
        :: PrimaryKey WalletId
        -> Hash "Tx"
        -> ExceptT ErrNoSuchWallet stm (Maybe TransactionInfo)
        -- ^ Fetch the latest transaction by id, returns Nothing when the
        -- transaction isn't found.
        --
        -- If the wallet doesn't exist, this operation returns an error.

    , removePendingTx
        :: PrimaryKey WalletId
        -> Hash "Tx"
        -> ExceptT ErrRemovePendingTx stm ()
        -- ^ Remove a pending transaction.

    , putPrivateKey
        :: PrimaryKey WalletId
        -> (k 'RootK XPrv, Hash "encryption")
        -> ExceptT ErrNoSuchWallet stm ()
        -- ^ Store or replace a private key for a given wallet. Note that wallet
        -- _could_ be stored and manipulated without any private key associated
        -- to it. A private key is only seldomly required for very specific
        -- operations (like transaction signing).

    , readPrivateKey
        :: PrimaryKey WalletId
        -> stm (Maybe (k 'RootK XPrv, Hash "encryption"))
        -- ^ Read a previously stored private key and its associated passphrase
        -- hash.

    , putProtocolParameters
        :: PrimaryKey WalletId
        -> ProtocolParameters
        -> ExceptT ErrNoSuchWallet stm ()
        -- ^ Store protocol parameters for the node tip.

    , readProtocolParameters
        :: PrimaryKey WalletId
        -> stm (Maybe ProtocolParameters)
        -- ^ Read the previously stored node tip protocol parameters.

    , rollbackTo
        :: PrimaryKey WalletId
        -> SlotNo
        -> ExceptT ErrNoSuchWallet stm SlotNo
        -- ^ Drops all checkpoints and transaction data after the given slot.
        --
        -- Returns the actual slot to which the database has rolled back. This
        -- slot is guaranteed to be earlier than (or identical to) the given
        -- point of rollback but can't be guaranteed to be exactly the same
        -- because the database may only keep sparse checkpoints.

    , prune
        :: PrimaryKey WalletId
        -> ExceptT ErrNoSuchWallet stm ()
        -- ^ Prune database entities and remove entities that can be discarded.

    , atomically
        :: forall a. stm a -> m a
        -- ^ Execute operations of the database in isolation and atomically.
    }

-- | Can't perform given operation because there's no wallet
newtype ErrNoSuchWallet
    = ErrNoSuchWallet WalletId -- Wallet is gone or doesn't exist yet
    deriving (Eq, Show)

-- | Can't perform removing pending transaction
data ErrRemovePendingTx
    = ErrRemovePendingTxNoSuchWallet ErrNoSuchWallet
    | ErrRemovePendingTxNoSuchTransaction (Hash "Tx")
    | ErrRemovePendingTxTransactionNoMorePending (Hash "Tx")
    deriving (Eq, Show)

-- | Can't perform given operation because there's no transaction
newtype ErrNoSuchTransaction
    = ErrNoSuchTransaction (Hash "Tx")
    deriving (Eq, Show)

-- | Forbidden operation was executed on an already existing wallet
newtype ErrWalletAlreadyExists
    = ErrWalletAlreadyExists WalletId -- Wallet already exists in db
    deriving (Eq, Show)

-- | A primary key which can take many forms depending on the value. This may
-- become a type family as we move forward, but for now, it illustrate that
-- some queries are ran against some sort of store;
--
-- As a matter of fact, we may manipulate multiple wallets at the same time, so,
-- functions like 'enqueueCheckpoint' needs to be associated to a corresponding
-- wallet. Some other may not because they are information valid for all wallets
-- (like for instance, the last known network tip).
newtype PrimaryKey key = PrimaryKey key
    deriving (Show, Eq, Ord)

-- | Clean a database by removing all wallets.
cleanDB :: DBLayer m s k -> m ()
cleanDB DBLayer{..} = atomically $
    listWallets >>= mapM_ (runExceptT . removeWallet)

-- | Storing EVERY checkpoints in the database is quite expensive and useless.
-- We make the following assumptions:
--
-- - We can't rollback for more than `k=epochStability` blocks in the past
-- - It is pretty fast to re-sync a few hundred blocks
-- - Small rollbacks may occur more often than long one
--
-- So, as we insert checkpoints, we make sure to:
--
-- - Prune any checkpoint that more than `k` blocks in the past
-- - Keep only one checkpoint every 100 blocks
-- - But still keep ~10 most recent checkpoints to cope with small rollbacks
--
-- __Example 1__: Inserting `cp153`
--
--  ℹ: `cp142` is discarded and `cp153` inserted.
--
--  @
--  Currently in DB:
-- ┌───┬───┬───┬─  ──┬───┐
-- │cp000 │cp100 │cp142 │..    ..│cp152 │
-- └───┴───┴───┴─  ──┴───┘
--  Want in DB:
-- ┌───┬───┬───┬─  ──┬───┐
-- │cp000 │cp100 │cp143 │..    ..│cp153 │
-- └───┴───┴───┴─  ──┴───┘
--  @
--
--
--  __Example 2__: Inserting `cp111`
--
--  ℹ: `cp100` is kept and `cp111` inserted.
--
--  @
--  Currently in DB:
-- ┌───┬───┬───┬─  ──┬───┐
-- │cp000 │cp100 │cp101 │..    ..│cp110 │
-- └───┴───┴───┴─  ──┴───┘
--  Want in DB:
-- ┌───┬───┬───┬─  ──┬───┐
-- │cp000 │cp100 │cp101 │..    ..│cp111 │
-- └───┴───┴───┴─  ──┴───┘
--  @
--
-- NOTE: There might be cases where the chain following "fails" (because, for
-- example, the node has switch to a different chain, different by more than k),
-- and in such cases, we have no choice but rolling back from genesis.
-- Therefore, we need to keep the very first checkpoint in the database, no
-- matter what.
sparseCheckpoints
    :: Quantity "block" Word32
        -- ^ Epoch Stability, i.e. how far we can rollback
    -> Quantity "block" Word32
        -- ^ A given block height
    -> [Word32]
        -- ^ The list of checkpoint heights that should be kept in DB.
sparseCheckpoints epochStability blkH =
    let
        gapsSize = 100
        edgeSize = 10

        k = getQuantity epochStability
        h = getQuantity blkH
        minH =
            let x = if h < k then 0 else h - k
            in gapsSize * (x `div` gapsSize)

        initial   = 0
        longTerm  = [minH,minH+gapsSize..h]
        shortTerm = if h < edgeSize
            then [0..h]
            else [h-edgeSize,h-edgeSize+1..h]
    in
        L.sort $ L.nub $ initial : (longTerm ++ shortTerm)
