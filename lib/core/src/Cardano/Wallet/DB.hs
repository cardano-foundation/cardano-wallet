{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Copyright: © 2018-2019 IOHK
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

import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..), XPrv )
import Cardano.Wallet.Primitive.Model
    ( Wallet )
import Cardano.Wallet.Primitive.Types
    ( BlockHeader
    , DefineTx (..)
    , Hash
    , Range (..)
    , SlotId (..)
    , SortOrder (..)
    , TxMeta
    , TxStatus
    , WalletId
    , WalletMetadata
    )
import Control.Exception
    ( Exception )
import Control.Monad.Trans.Except
    ( ExceptT, runExceptT )
import Data.Quantity
    ( Quantity (..) )
import Data.Word
    ( Word32 )
import GHC.Generics
    ( Generic )

import qualified Data.List as L

-- | Instantiate database layers at will
data DBFactory m s t k = DBFactory
    { withDatabase :: WalletId -> (DBLayer m s t k -> IO ()) -> IO ()
        -- ^ Creates a new or use an existing database, maintaining an open
        -- connection so long as necessary

    , removeDatabase :: WalletId -> IO ()
        -- ^ Erase any trace of the database
    } deriving (Generic)

-- | A Database interface for storing various things in a DB. In practice,
-- we'll need some extra contraints on the wallet state that allows us to
-- serialize and unserialize it (e.g. @forall s. (Serialize s) => ...@)
data DBLayer m s t k = DBLayer
    { createWallet
        :: PrimaryKey WalletId
        -> Wallet s t
        -> WalletMetadata
        -> [(Tx t, TxMeta)]
        -> ExceptT ErrWalletAlreadyExists m ()
        -- ^ Initialize a database entry for a given wallet. 'putCheckpoint',
        -- 'putWalletMeta' or 'putTxHistory' will actually all fail if they are
        -- called _first_ on a wallet.

    , removeWallet
        :: PrimaryKey WalletId
        -> ExceptT ErrNoSuchWallet m ()
        -- ^ Remove a given wallet and all its associated data (checkpoints,
        -- metadata, tx history ...)

    , listWallets
        :: m [PrimaryKey WalletId]
        -- ^ Get the list of all known wallets in the DB, possibly empty.

    , putCheckpoint
        :: PrimaryKey WalletId
        -> Wallet s t
        -> ExceptT ErrNoSuchWallet m ()
        -- ^ Replace the current checkpoint for a given wallet. We do not handle
        -- rollbacks yet, and therefore only stores the latest available
        -- checkpoint.
        --
        -- If the wallet doesn't exist, this operation returns an error.

    , readCheckpoint
        :: PrimaryKey WalletId
        -> m (Maybe (Wallet s t))
        -- ^ Fetch the most recent checkpoint of a given wallet.
        --
        -- Return 'Nothing' if there's no such wallet.

    , listCheckpoints
        :: PrimaryKey WalletId
        -> m [BlockHeader]
        -- ^ List all known checkpoint tips, ordered by slot ids from the oldest
        -- to the newest.

    , putWalletMeta
        :: PrimaryKey WalletId
        -> WalletMetadata
        -> ExceptT ErrNoSuchWallet m ()
        -- ^ Replace an existing wallet metadata with the given one.
        --
        -- If the wallet doesn't exist, this operation returns an error

    , readWalletMeta
        :: PrimaryKey WalletId
        -> m (Maybe WalletMetadata)
        -- ^ Fetch a wallet metadata, if they exist.
        --
        -- Return 'Nothing' if there's no such wallet.

    , putTxHistory
        :: (DefineTx t)
        => PrimaryKey WalletId
        -> [(Tx t, TxMeta)]
        -> ExceptT ErrNoSuchWallet m ()
        -- ^ Augments the transaction history for a known wallet.
        --
        -- If an entry for a particular transaction already exists it is not
        -- altered nor merged (just ignored).
        --
        -- If the wallet doesn't exist, this operation returns an error.

    , readTxHistory
        :: PrimaryKey WalletId
        -> SortOrder
        -> Range SlotId
        -> Maybe TxStatus
        -> m [(Tx t, TxMeta)]
        -- ^ Fetch the current transaction history of a known wallet, ordered by
        -- descending slot number.
        --
        -- Returns an empty list if the wallet isn't found.

    , putPrivateKey
        :: PrimaryKey WalletId
        -> (k 'RootK XPrv, Hash "encryption")
        -> ExceptT ErrNoSuchWallet m ()
        -- ^ Store or replace a private key for a given wallet. Note that wallet
        -- _could_ be stored and manipulated without any private key associated
        -- to it. A private key is only seldomly required for very specific
        -- operations (like transaction signing).

    , readPrivateKey
        :: PrimaryKey WalletId
        -> m (Maybe (k 'RootK XPrv, Hash "encryption"))
        -- ^ Read a previously stored private key and its associated passphrase
        -- hash.

    , rollbackTo
        :: PrimaryKey WalletId
        -> SlotId
        -> ExceptT ErrNoSuchWallet m ()
        -- ^ Drops all checkpoints and transaction data after the given slot.

    , prune
        :: PrimaryKey WalletId
        -> ExceptT ErrNoSuchWallet m ()
        -- ^ Prune database entities and remove entities that can be discarded.

    , removePendingTx
        :: PrimaryKey WalletId
        -> Hash "Tx"
        -> ExceptT ErrRemovePendingTx m ()
        -- ^ Remove a pending transaction.

    , withLock
        :: forall e a. ()
        => ExceptT e m a
        -> ExceptT e m a
    }

-- | Can't perform given operation because there's no wallet
newtype ErrNoSuchWallet
    = ErrNoSuchWallet WalletId -- Wallet is gone or doesn't exist yet
    deriving (Eq, Show)

-- | Can't perform removing pending transaction
data ErrRemovePendingTx
    = ErrRemovePendingTxNoSuchWallet WalletId
    | ErrRemovePendingTxNoSuchTransaction (Hash "Tx")
    | ErrRemovePendingTxTransactionNoMorePending (Hash "Tx")
    deriving (Eq, Show)

instance Exception ErrRemovePendingTx

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
cleanDB :: Monad m => DBLayer m s t k -> m ()
cleanDB db = listWallets db >>= mapM_ (runExceptT . removeWallet db)

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
