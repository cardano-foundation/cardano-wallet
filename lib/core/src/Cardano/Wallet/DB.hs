{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
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
    , cleanDB

      -- * Checkpoints
    , sparseCheckpoints
    , SparseCheckpointsConfig (..)
    , defaultSparseCheckpointsConfig
    , gapSize

      -- * Errors
    , ErrBadFormat(..)
    , ErrNoSuchWallet(..)
    , ErrWalletAlreadyExists(..)
    , ErrNoSuchTransaction (..)
    , ErrRemoveTx (..)
    , ErrPutLocalTxSubmission (..)
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPrv )
import Cardano.Wallet.DB.WalletState
    ( DeltaMap, DeltaWalletState )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..) )
import Cardano.Wallet.Primitive.Model
    ( Wallet )
import Cardano.Wallet.Primitive.Passphrase
    ( PassphraseHash )
import Cardano.Wallet.Primitive.Types
    ( ChainPoint
    , DelegationCertificate
    , GenesisParameters
    , Range (..)
    , Slot
    , SlotNo (..)
    , SortOrder (..)
    , WalletId
    , WalletMetadata
    )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash )
import Cardano.Wallet.Primitive.Types.Tx
    ( LocalTxSubmissionStatus
    , SealedTx
    , TransactionInfo
    , Tx (..)
    , TxMeta
    , TxStatus
    )
import Control.Monad.IO.Class
    ( MonadIO )
import Control.Monad.Trans.Except
    ( ExceptT, runExceptT )
import Data.DBVar
    ( DBVar )
import Data.Quantity
    ( Quantity (..) )
import Data.Word
    ( Word32, Word8 )
import UnliftIO.Exception
    ( Exception )

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
-- we'll need some extra constraints on the wallet state that allows us to
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
        :: WalletId
        -> Wallet s
        -> WalletMetadata
        -> [(Tx, TxMeta)]
        -> GenesisParameters
        -> ExceptT ErrWalletAlreadyExists stm ()
        -- ^ Initialize a database entry for a given wallet. 'putCheckpoint',
        -- 'putWalletMeta', 'putTxHistory' or 'putProtocolParameters' will
        -- actually all fail if they are called _first_ on a wallet.

    , removeWallet
        :: WalletId
        -> ExceptT ErrNoSuchWallet stm ()
        -- ^ Remove a given wallet and all its associated data (checkpoints,
        -- metadata, tx history ...)

    , listWallets
        :: stm [WalletId]
        -- ^ Get the list of all known wallets in the DB, possibly empty.

    , walletsDB
        :: DBVar stm (DeltaMap WalletId (DeltaWalletState s))
        -- ^ 'DBVar' containing the 'WalletState' of each wallet in the database.
        -- Currently contains all 'Checkpoints' of the 'UTxO' and the
        -- 'Discoveries', as well as the 'Prologue' of the address discovery state.
        -- 
        -- Intended to replace 'putCheckpoint' and 'readCheckpoint' in the short-term,
        -- and all other functions in the long-term.

    , putCheckpoint
        :: WalletId
        -> Wallet s
        -> ExceptT ErrNoSuchWallet stm ()
        -- ^ Replace the current checkpoint for a given wallet. We do not handle
        -- rollbacks yet, and therefore only stores the latest available
        -- checkpoint.
        --
        -- If the wallet doesn't exist, this operation returns an error.

    , readCheckpoint
        :: WalletId
        -> stm (Maybe (Wallet s))
        -- ^ Fetch the most recent checkpoint of a given wallet.
        --
        -- Return 'Nothing' if there's no such wallet.

    , listCheckpoints
        :: WalletId
        -> stm [ChainPoint]
        -- ^ List all known checkpoint tips, ordered by slot ids from the oldest
        -- to the newest.

    , putWalletMeta
        :: WalletId
        -> WalletMetadata
        -> ExceptT ErrNoSuchWallet stm ()
        -- ^ Replace an existing wallet metadata with the given one.
        --
        -- If the wallet doesn't exist, this operation returns an error

    , readWalletMeta
        :: WalletId
        -> stm (Maybe WalletMetadata)
        -- ^ Fetch a wallet metadata, if they exist.
        --
        -- Return 'Nothing' if there's no such wallet.

    , isStakeKeyRegistered
        :: WalletId
        -> ExceptT ErrNoSuchWallet stm Bool

    , putDelegationCertificate
        :: WalletId
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
        :: WalletId
        -> Coin
        -> ExceptT ErrNoSuchWallet stm ()
        -- ^ Store the latest known reward account balance.
        --
        -- This is separate from checkpoints because the data corresponds to the
        -- node tip.
        -- This is separate from putWalletMeta because it's not meta data

    , readDelegationRewardBalance
        :: WalletId
        -> stm Coin
        -- ^ Get the reward account balance.
        --
        -- Returns zero if the wallet isn't found or if wallet hasn't delegated
        -- stake.

    , putTxHistory
        :: WalletId
        -> [(Tx, TxMeta)]
        -> ExceptT ErrNoSuchWallet stm ()
        -- ^ Augments the transaction history for a known wallet.
        --
        -- If an entry for a particular transaction already exists it is not
        -- altered nor merged (just ignored).
        --
        -- If the wallet doesn't exist, this operation returns an error.

    , readTxHistory
        :: WalletId
        -> Maybe Coin
        -> SortOrder
        -> Range SlotNo
        -> Maybe TxStatus
        -> stm [TransactionInfo]
        -- ^ Fetch the current transaction history of a known wallet, ordered by
        -- descending slot number.
        --
        -- Returns an empty list if the wallet isn't found.

    , getTx
        :: WalletId
        -> Hash "Tx"
        -> ExceptT ErrNoSuchWallet stm (Maybe TransactionInfo)
        -- ^ Fetch the latest transaction by id, returns Nothing when the
        -- transaction isn't found.
        --
        -- If the wallet doesn't exist, this operation returns an error.

    , putLocalTxSubmission
        :: WalletId
        -> Hash "Tx"
        -> SealedTx
        -> SlotNo
        -> ExceptT ErrPutLocalTxSubmission stm ()
        -- ^ Add or update a transaction in the local submission pool with the
        -- most recent submission slot.

    , readLocalTxSubmissionPending
        :: WalletId
        -> stm [LocalTxSubmissionStatus SealedTx]
        -- ^ List all transactions from the local submission pool which are
        -- still pending as of the latest checkpoint of the given wallet. The
        -- slot numbers for first submission and most recent submission are
        -- included.

    , updatePendingTxForExpiry
        :: WalletId
        -> SlotNo
        -> ExceptT ErrNoSuchWallet stm ()
        -- ^ Removes any expired transactions from the pending set and marks
        -- their status as expired.

    , removePendingOrExpiredTx
        :: WalletId
        -> Hash "Tx"
        -> ExceptT ErrRemoveTx stm ()
        -- ^ Manually remove a pending transaction.

    , putPrivateKey
        :: WalletId
        -> (k 'RootK XPrv, PassphraseHash)
        -> ExceptT ErrNoSuchWallet stm ()
        -- ^ Store or replace a private key for a given wallet. Note that wallet
        -- _could_ be stored and manipulated without any private key associated
        -- to it. A private key is only seldomly required for very specific
        -- operations (like transaction signing).

    , readPrivateKey
        :: WalletId
        -> stm (Maybe (k 'RootK XPrv, PassphraseHash))
        -- ^ Read a previously stored private key and its associated passphrase
        -- hash.

    , readGenesisParameters
        :: WalletId
        -> stm (Maybe GenesisParameters)
        -- ^ Read the *Byron* genesis parameters.

    , rollbackTo
        :: WalletId
        -> Slot
        -> ExceptT ErrNoSuchWallet stm ChainPoint
        -- ^ Drops all checkpoints and transaction data which
        -- have appeared after the given 'ChainPoint'.
        --
        -- Returns the actual 'ChainPoint' to which the database has rolled back.
        -- Its slot is guaranteed to be earlier than (or identical to) the given
        -- point of rollback but can't be guaranteed to be exactly the same
        -- because the database may only keep sparse checkpoints.

    , prune
        :: WalletId
        -> Quantity "block" Word32
        -> ExceptT ErrNoSuchWallet stm ()
        -- ^ Prune database entities and remove entities that can be discarded.
        --
        -- The second argument represents the stability window, or said
        -- length of the deepest rollback.

    , atomically
        :: forall a. stm a -> m a
        -- ^ Execute operations of the database in isolation and atomically.
    }

-- | Can't read the database file because it's in a bad format
-- (corrupted, too old, …)
data ErrBadFormat
    = ErrBadFormatAddressPrologue
    | ErrBadFormatCheckpoints
    deriving (Eq,Show)

instance Exception ErrBadFormat

-- | Can't perform given operation because there's no wallet
newtype ErrNoSuchWallet
    = ErrNoSuchWallet WalletId -- Wallet is gone or doesn't exist yet
    deriving (Eq, Show)

-- | Can't add a transaction to the local tx submission pool.
data ErrPutLocalTxSubmission
    = ErrPutLocalTxSubmissionNoSuchWallet ErrNoSuchWallet
    | ErrPutLocalTxSubmissionNoSuchTransaction ErrNoSuchTransaction
    deriving (Eq, Show)

-- | Can't remove pending or expired transaction.
data ErrRemoveTx
    = ErrRemoveTxNoSuchWallet ErrNoSuchWallet
    | ErrRemoveTxNoSuchTransaction ErrNoSuchTransaction
    | ErrRemoveTxAlreadyInLedger (Hash "Tx")
    deriving (Eq, Show)

-- | Indicates that the specified transaction hash is not found in the
-- transaction history of the given wallet.
data ErrNoSuchTransaction
    = ErrNoSuchTransaction WalletId (Hash "Tx")
    deriving (Eq, Show)

-- | Forbidden operation was executed on an already existing wallet
newtype ErrWalletAlreadyExists
    = ErrWalletAlreadyExists WalletId -- Wallet already exists in db
    deriving (Eq, Show)

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
    :: SparseCheckpointsConfig
        -- ^ Parameters for the function.
    -> Quantity "block" Word32
        -- ^ A given block height
    -> [Word32]
        -- ^ The list of checkpoint heights that should be kept in DB.
sparseCheckpoints cfg blkH  =
    let
        SparseCheckpointsConfig{edgeSize,epochStability} = cfg
        g = gapSize cfg
        h = getQuantity blkH
        e = fromIntegral edgeSize

        minH =
            let x = if h < epochStability + g then 0 else h - epochStability - g
            in g * (x `div` g)

        initial   = 0
        longTerm  = [minH,minH+g..h]
        shortTerm = if h < e
            then [0..h]
            else [h-e,h-e+1..h]
    in
        L.sort (L.nub $ initial : (longTerm ++ shortTerm))

-- | Captures the configuration for the `sparseCheckpoints` function.
--
-- NOTE: large values of 'edgeSize' aren't recommended as they would mean
-- storing many unnecessary checkpoints. In Ouroboros Praos, there's a
-- reasonable probability for small forks of a few blocks so it makes sense to
-- maintain a small part that is denser near the edge.
data SparseCheckpointsConfig = SparseCheckpointsConfig
    { edgeSize :: Word8
    , epochStability :: Word32
    } deriving Show

-- | A sensible default to use in production. See also 'SparseCheckpointsConfig'
defaultSparseCheckpointsConfig :: Quantity "block" Word32 -> SparseCheckpointsConfig
defaultSparseCheckpointsConfig (Quantity epochStability) =
    SparseCheckpointsConfig
        { edgeSize = 5
        , epochStability
        }

-- | A reasonable gap size used internally in 'sparseCheckpoints'.
--
-- 'Reasonable' means that it's not _too frequent_ and it's not too large. A
-- value that is too small in front of k would require generating much more
-- checkpoints than necessary.
--
-- A value that is larger than `k` may have dramatic consequences in case of
-- deep rollbacks.
--
-- As a middle ground, we current choose `k / 3`, which is justified by:
--
-- - The current speed of the network layer (several thousands blocks per seconds)
-- - The current value of k = 2160
--
-- So, `k / 3` = 720, which should remain around a second of time needed to catch
-- up in case of large rollbacks.
gapSize :: SparseCheckpointsConfig -> Word32
gapSize SparseCheckpointsConfig{epochStability} =
    epochStability `div` 3
