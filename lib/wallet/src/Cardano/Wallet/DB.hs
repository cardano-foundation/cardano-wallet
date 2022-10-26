{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

    -- * DBLayer building blocks
    , DBLayerCollection (..)
    , DBWallets (..)
    , DBCheckpoints (..)
    , DBWalletMeta (..)
    , DBDelegation (..)
    , DBTxHistory (..)
    , DBPendingTxs (..)
    , DBPrivateKey (..)
    , mkDBLayerFromParts

      -- * Errors
    , ErrBadFormat(..)
    , ErrWalletAlreadyExists(..)
    , ErrNoSuchTransaction (..)
    , ErrRemoveTx (..)
    , ErrPutLocalTxSubmission (..)
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPrv )
import Cardano.Wallet.DB.WalletState
    ( DeltaMap, DeltaWalletState, ErrNoSuchWallet (..) )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..) )
import Cardano.Wallet.Primitive.Model
    ( Wallet, currentTip )
import Cardano.Wallet.Primitive.Passphrase
    ( PassphraseHash )
import Cardano.Wallet.Primitive.Types
    ( BlockHeader
    , ChainPoint
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
    , TransactionInfo (..)
    , Tx (..)
    , TxMeta (..)
    , TxStatus
    )
import Control.Monad.IO.Class
    ( MonadIO )
import Control.Monad.Trans.Except
    ( ExceptT (..), runExceptT )
import Data.DBVar
    ( DBVar )
import Data.List
    ( sortOn )
import Data.Ord
    ( Down (..) )
import Data.Quantity
    ( Quantity (..) )
import Data.Word
    ( Word32 )
import UnliftIO.Exception
    ( Exception )

import qualified Data.Map.Strict as Map

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

{-----------------------------------------------------------------------------
    Build DBLayer from smaller parts
------------------------------------------------------------------------------}
{- Note [TransitionDBLayer]

In order to allow modularization of the wallet logic,
we want to transition the monolithic 'DBLayer' type into a more modular
set of database functions.

Design notes:

* Ideally, we want to transition everything into 'DBVar'.
However, memory consumption for the 'TxHistory' is an issue for large wallets.
Hence

    * By transition to records first, we may choose *not* to transition to
      'DBVar' for the 'TxHistory' in the future.

    * But we still need to work on disentangling the pending transactions
      (formerly: local tx submission) from the 'TxHistory'.

* We choose not to remove the legacy 'DBLayer' type for now, as we still
have the state machine unit tests for it. It is less development effort
to delete them wholesale rather than maintaining them.

-}
data DBLayerCollection stm m s k = DBLayerCollection
    { dbWallets :: DBWallets stm s
    , dbCheckpoints :: DBCheckpoints stm s
    , dbWalletMeta :: DBWalletMeta stm
    , dbDelegation :: WalletId -> DBDelegation stm
    , dbTxHistory :: DBTxHistory stm
    , dbPendingTxs :: DBPendingTxs stm
    , dbPrivateKey :: WalletId -> DBPrivateKey stm k

    -- The following two functions will need to be split up
    -- and distributed the smaller layer parts as well.
    , rollbackTo_
        :: WalletId
        -> Slot
        -> ExceptT ErrNoSuchWallet stm ChainPoint
    , prune_
        :: WalletId
        -> Quantity "block" Word32
        -> ExceptT ErrNoSuchWallet stm ()
    , atomically_
        :: forall a. stm a -> m a
    }

{- HLINT ignore mkDBLayerFromParts "Avoid lambda" -}
-- | Create a legacy 'DBLayer' from smaller database layers.
mkDBLayerFromParts
    :: forall stm m s k. (MonadIO stm, MonadFail stm)
    => DBLayerCollection stm m s k -> DBLayer m s k
mkDBLayerFromParts DBLayerCollection{..} = DBLayer
    { initializeWallet = initializeWallet_ dbWallets
    , removeWallet = removeWallet_ dbWallets
    , listWallets = listWallets_ dbWallets
    , walletsDB = walletsDB_ dbCheckpoints
    , putCheckpoint = putCheckpoint_ dbCheckpoints
    , readCheckpoint = readCheckpoint_ dbCheckpoints
    , listCheckpoints = listCheckpoints_ dbCheckpoints
    , putWalletMeta = putWalletMeta_ dbWalletMeta
    , readWalletMeta = readWalletMeta_ dbWalletMeta
    , isStakeKeyRegistered = \wid -> wrapNoSuchWallet wid $
        isStakeKeyRegistered_ (dbDelegation wid)
    , putDelegationCertificate = \wid a b -> wrapNoSuchWallet wid $
        putDelegationCertificate_ (dbDelegation wid) a b
    , putDelegationRewardBalance = \wid a -> wrapNoSuchWallet wid $
        putDelegationRewardBalance_ (dbDelegation wid) a
    , readDelegationRewardBalance = \wid ->
        readDelegationRewardBalance_ (dbDelegation wid)
    , putTxHistory = \wid a -> wrapNoSuchWallet wid $
        putTxHistory_ dbTxHistory wid a
    , readTxHistory = \wid minWithdrawal order range status ->
        readCurrentTip wid >>= \case
            Just tip -> do
                tinfos <- (readTxHistory_ dbTxHistory) wid range status tip
                pure
                    . sortTransactionsBySlot order
                    . filterMinWithdrawal minWithdrawal
                    $ tinfos
            Nothing ->
                pure []
    , getTx = \wid txid -> wrapNoSuchWallet wid $ do
        Just tip <- readCurrentTip wid -- wallet exists
        (getTx_ dbTxHistory) wid txid tip
    , putLocalTxSubmission = putLocalTxSubmission_ dbPendingTxs
    , readLocalTxSubmissionPending = readLocalTxSubmissionPending_ dbPendingTxs
    , updatePendingTxForExpiry = updatePendingTxForExpiry_ dbPendingTxs
    , removePendingOrExpiredTx = removePendingOrExpiredTx_ dbPendingTxs
    , putPrivateKey = \wid a -> wrapNoSuchWallet wid $
        putPrivateKey_ (dbPrivateKey wid) a
    , readPrivateKey = \wid ->
        readPrivateKey_ (dbPrivateKey wid)
    , readGenesisParameters = readGenesisParameters_ dbWallets
    , rollbackTo = rollbackTo_
    , prune = prune_
    , atomically = atomically_
    }
  where
    wrapNoSuchWallet
        :: WalletId
        -> stm a
        -> ExceptT ErrNoSuchWallet stm a
    wrapNoSuchWallet wid action = ExceptT $
        hasWallet_ dbWallets wid >>= \case
            False -> pure $ Left $ ErrNoSuchWallet wid
            True  -> Right <$> action

    readCurrentTip :: WalletId -> stm (Maybe BlockHeader)
    readCurrentTip =
        (fmap . fmap) currentTip . readCheckpoint_ dbCheckpoints

-- | A database layer for a collection of wallets
data DBWallets stm s = DBWallets
    { initializeWallet_
        :: WalletId
        -> Wallet s
        -> WalletMetadata
        -> [(Tx, TxMeta)]
        -> GenesisParameters
        -> ExceptT ErrWalletAlreadyExists stm ()
        -- ^ Initialize a database entry for a given wallet. 'putCheckpoint',
        -- 'putWalletMeta', 'putTxHistory' or 'putProtocolParameters' will
        -- actually all fail if they are called _first_ on a wallet.

    , readGenesisParameters_
        :: WalletId
        -> stm (Maybe GenesisParameters)
        -- ^ Read the *Byron* genesis parameters.

    , removeWallet_
        :: WalletId
        -> ExceptT ErrNoSuchWallet stm ()
        -- ^ Remove a given wallet and all its associated data (checkpoints,
        -- metadata, tx history ...)

    , listWallets_
        :: stm [WalletId]
        -- ^ Get the list of all known wallets in the DB, possibly empty.

    , hasWallet_
        :: WalletId
        -> stm Bool
        -- ^ Check whether the wallet with 'WalletId' is present
        -- in the database.
    }

-- | A database layer for storing wallet states.
data DBCheckpoints stm s = DBCheckpoints
    { walletsDB_
        :: DBVar stm (DeltaMap WalletId (DeltaWalletState s))
        -- ^ 'DBVar' containing the 'WalletState' of each wallet in the database.
        -- Currently contains all 'Checkpoints' of the 'UTxO' and the
        -- 'Discoveries', as well as the 'Prologue' of the address discovery state.
        --
        -- Intended to replace 'putCheckpoint' and 'readCheckpoint' in the short-term,
        -- and all other functions in the long-term.

    , putCheckpoint_
        :: WalletId
        -> Wallet s
        -> ExceptT ErrNoSuchWallet stm ()
        -- ^ Replace the current checkpoint for a given wallet. We do not handle
        -- rollbacks yet, and therefore only stores the latest available
        -- checkpoint.
        --
        -- If the wallet doesn't exist, this operation returns an error.

    , readCheckpoint_
        :: WalletId
        -> stm (Maybe (Wallet s))
        -- ^ Fetch the most recent checkpoint of a given wallet.
        --
        -- Return 'Nothing' if there's no such wallet.

    , listCheckpoints_
        :: WalletId
        -> stm [ChainPoint]
        -- ^ List all known checkpoint tips, ordered by slot ids from the oldest
        -- to the newest.
    }

-- | A database layer for storing 'WalletMetadata'.
data DBWalletMeta stm = DBWalletMeta
    { putWalletMeta_
        :: WalletId
        -> WalletMetadata
        -> ExceptT ErrNoSuchWallet stm ()
        -- ^ Replace an existing wallet metadata with the given one.
        --
        -- If the wallet doesn't exist, this operation returns an error

    , readWalletMeta_
        :: WalletId
        -> stm (Maybe WalletMetadata)
        -- ^ Fetch a wallet metadata, if they exist.
        --
        -- Return 'Nothing' if there's no such wallet.
    }

-- | A database layer for storing delegation certificates
-- and the reward balance.
data DBDelegation stm = DBDelegation
    { isStakeKeyRegistered_
        :: stm Bool

    , putDelegationCertificate_
        :: DelegationCertificate
        -> SlotNo
        -> stm ()
        -- ^ Binds a stake pool id to a wallet. This will have an influence on
        -- the wallet metadata: the last known certificate will indicate to
        -- which pool a wallet is currently delegating.
        --
        -- This is done separately from 'putWalletMeta' because certificate
        -- declarations are:
        --
        -- 1. Stored on-chain.
        -- 2. Affected by rollbacks (or said differently, tied to a 'SlotNo').

    , putDelegationRewardBalance_
        :: Coin
        -> stm ()
        -- ^ Store the latest known reward account balance.
        --
        -- This is separate from checkpoints because the data corresponds to the
        -- node tip.
        -- This is separate from putWalletMeta because it's not meta data

    , readDelegationRewardBalance_
        :: stm Coin
        -- ^ Get the reward account balance.
        --
        -- Returns zero if the wallet hasn't delegated stake.
    }

-- | A database layer that stores the transaction history.
data DBTxHistory stm = DBTxHistory
    { putTxHistory_
        :: WalletId
        -> [(Tx, TxMeta)]
        -> stm ()
        -- ^ Augments the transaction history for a known wallet.
        --
        -- If an entry for a particular transaction already exists it is not
        -- altered nor merged (just ignored).
        --
        -- If the wallet does not exist, the function may throw
        -- an error, but need not.

    , readTxHistory_
        :: WalletId
        -> Range SlotNo
        -> Maybe TxStatus
        -> BlockHeader
        -> stm [TransactionInfo]
        -- ^ Fetch the current transaction history of a known wallet, ordered by
        -- descending slot number.
        --
        -- Returns an empty list if the wallet isn't found.

    , getTx_
        :: WalletId
        -> Hash "Tx"
        -> BlockHeader
        -> stm (Maybe TransactionInfo)
        -- ^ Fetch the latest transaction by id, returns Nothing when the
        -- transaction isn't found.
        --
        -- If the wallet doesn't exist, this operation returns an error.
    }

-- | A database layer for storing pending transactions.
data DBPendingTxs stm = DBPendingTxs
    { putLocalTxSubmission_
        :: WalletId
        -> Hash "Tx"
        -> SealedTx
        -> SlotNo
        -> ExceptT ErrPutLocalTxSubmission stm ()
        -- ^ Add or update a transaction in the local submission pool with the
        -- most recent submission slot.

    , readLocalTxSubmissionPending_
        :: WalletId
        -> stm [LocalTxSubmissionStatus SealedTx]
        -- ^ List all transactions from the local submission pool which are
        -- still pending as of the latest checkpoint of the given wallet. The
        -- slot numbers for first submission and most recent submission are
        -- included.

    , updatePendingTxForExpiry_
        :: WalletId
        -> SlotNo
        -> ExceptT ErrNoSuchWallet stm ()
        -- ^ Removes any expired transactions from the pending set and marks
        -- their status as expired.

    , removePendingOrExpiredTx_
        :: WalletId
        -> Hash "Tx"
        -> ExceptT ErrRemoveTx stm ()
        -- ^ Manually remove a pending transaction.
    }

-- | A database layer for storing the private key.
data DBPrivateKey stm k = DBPrivateKey
    { putPrivateKey_
        :: (k 'RootK XPrv, PassphraseHash)
        -> stm ()
        -- ^ Store or replace a private key for a given wallet. Note that wallet
        -- _could_ be stored and manipulated without any private key associated
        -- to it. A private key is only seldomly required for very specific
        -- operations (like transaction signing).

    , readPrivateKey_
        :: stm (Maybe (k 'RootK XPrv, PassphraseHash))
        -- ^ Read a previously stored private key and its associated passphrase
        -- hash.
    }

{-----------------------------------------------------------------------------
    Helper functions
------------------------------------------------------------------------------}
-- | Clean a database by removing all wallets.
cleanDB :: DBLayer m s k -> m ()
cleanDB DBLayer{..} = atomically $
    listWallets >>= mapM_ (runExceptT . removeWallet)

-- | Sort transactions by slot number.
sortTransactionsBySlot
    :: SortOrder -> [TransactionInfo] -> [TransactionInfo]
sortTransactionsBySlot = \case
    Ascending -> sortOn
        $ (,) <$> slotNo . txInfoMeta <*> Down . txInfoId
    Descending -> sortOn
        $ (,) <$> (Down . slotNo . txInfoMeta) <*> txInfoId

-- | Keep all transactions where at least one withdrawal is
-- above a given minimum amount.
filterMinWithdrawal
    :: Maybe Coin -> [TransactionInfo] -> [TransactionInfo]
filterMinWithdrawal Nothing = id
filterMinWithdrawal (Just minWithdrawal) = filter p
  where
    p = any (>= minWithdrawal) . Map.elems . txInfoWithdrawals

{-----------------------------------------------------------------------------
    Errors
------------------------------------------------------------------------------}
-- | Can't read the database file because it's in a bad format
-- (corrupted, too old, …)
data ErrBadFormat
    = ErrBadFormatAddressPrologue
    | ErrBadFormatCheckpoints
    deriving (Eq,Show)

instance Exception ErrBadFormat

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
