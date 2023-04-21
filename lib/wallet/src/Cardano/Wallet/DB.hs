{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Database / Persistence layer for the wallet backend. This is where we define
-- the interface allowing us to store and fetch various data on our wallets.

module Cardano.Wallet.DB
    ( -- * Interface
      DBLayer (..)
    , DBOpen (..)
    , DBFactory (..)

    -- * DBLayer building blocks
    , DBLayerCollection (..)
    , DBWallets (..)
    , DBCheckpoints (..)
    , DBWalletMeta (..)
    , DBDelegation (..)
    , DBTxHistory (..)
    , DBPrivateKey (..)
    , mkDBLayerFromParts

    , hoistDBLayer
      -- * Errors
    , module Cardano.Wallet.DB.Errors
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPrv )
import Cardano.Wallet.DB.Errors
import Cardano.Wallet.DB.Store.Submissions.Layer
    ( getInSubmissionTransaction, getInSubmissionTransactions )
import Cardano.Wallet.DB.Store.Submissions.Operations
    ( DeltaTxSubmissions
    , SubmissionMeta (..)
    , TxSubmissions
    , TxSubmissionsStatus
    )
import Cardano.Wallet.DB.Store.Transactions.Decoration
    ( TxInDecorator )
import Cardano.Wallet.DB.Store.Transactions.TransactionInfo
    ( mkTransactionInfoFromReadTx )
import Cardano.Wallet.DB.WalletState
    ( DeltaMap
    , DeltaWalletState
    , DeltaWalletState1 (UpdateSubmissions)
    , WalletState (submissions)
    , adjustNoSuchWallet
    )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..) )
import Cardano.Wallet.Primitive.Model
    ( Wallet, currentTip )
import Cardano.Wallet.Primitive.Passphrase
    ( PassphraseHash )
import Cardano.Wallet.Primitive.Slotting
    ( TimeInterpreter, epochOf, hoistTimeInterpreter, interpretQuery )
import Cardano.Wallet.Primitive.Types
    ( BlockHeader
    , ChainPoint
    , DelegationCertificate
    , EpochNo (..)
    , GenesisParameters
    , Range (..)
    , Slot
    , SlotNo (..)
    , SortOrder (..)
    , WalletDelegation (..)
    , WalletId
    , WalletMetadata (..)
    )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash )
import Cardano.Wallet.Primitive.Types.Tx
    ( SealedTx, TransactionInfo (..), Tx (..), TxMeta (..), TxStatus )
import Cardano.Wallet.Read.Eras
    ( EraValue )
import Cardano.Wallet.Read.Tx.Cardano
    ( fromSealedTx )
import Cardano.Wallet.Submissions.Submissions
    ( TxStatusMeta (..), txStatus )
import Cardano.Wallet.Submissions.TxStatus
    ( _Expired, _InSubmission )
import Cardano.Wallet.Transaction.Built
    ( BuiltTx )
import Control.Lens
    ( has )
import Control.Monad
    ( join )
import Control.Monad.IO.Class
    ( MonadIO, liftIO )
import Control.Monad.Trans.Except
    ( ExceptT (..) )
import Data.DBVar
    ( DBVar, modifyDBMaybe, readDBVar )
import Data.Functor
    ( (<&>) )
import Data.Generics.Internal.VL
    ( (^.) )
import Data.Kind
    ( Type )
import Data.List
    ( sortOn )
import Data.Maybe
    ( catMaybes )
import Data.Ord
    ( Down (..) )
import Data.Quantity
    ( Quantity (..) )
import Data.Traversable
    ( for )
import Data.Word
    ( Word32 )
import GHC.Num
    ( Natural )

import qualified Cardano.Wallet.DB.Store.Submissions.Layer as Sbms
import qualified Cardano.Wallet.Primitive.Types.Tx.SealedTx as WST
import qualified Cardano.Wallet.Primitive.Types.Tx.TxMeta as WTxMeta
import qualified Cardano.Wallet.Read.Tx as Read
import qualified Cardano.Wallet.Submissions.TxStatus as Subm
import qualified Data.Map.Strict as Map

{-----------------------------------------------------------------------------
    DBFactory
------------------------------------------------------------------------------}
-- | A facility for managing a mapping between 'WalletId's and
-- databases of wallet states.
--
-- In our use case, this will typically be a directory of database files,
-- or a 'Map' of in-memory SQLite databases.
data DBFactory m s k = DBFactory
    { withDatabase :: forall a. WalletId -> (DBLayer m s k -> IO a) -> IO a
        -- ^ Creates a new or use an existing database, maintaining an open
        -- connection so long as necessary

    , removeDatabase :: WalletId -> IO ()
        -- ^ Erase any trace of the database

    , listDatabases :: IO [WalletId]
        -- ^ List existing wallet database found on disk.
    }

{-----------------------------------------------------------------------------
    DBOpen
------------------------------------------------------------------------------}
-- | An open database which can store the state of a single wallet,
-- where all tables exist and have been migrated if necessary.
--
-- However, the database does not necessarily contain a valid wallet state yet.
--
-- In our use case, this will be typically be an open SQLite database
-- (file or in-memory).
newtype DBOpen stm m s (k :: Depth -> Type -> Type) = DBOpen
    { atomically :: forall a. stm a -> m a
        -- ^ Execute a sequence of database operations atomically.
    }

{-----------------------------------------------------------------------------
    DBLayer
------------------------------------------------------------------------------}
-- | An open database which can store the state of one wallet with a specific
-- 'WalletId'.
--
-- Even though this database is fixed to a specific 'WalletId',
-- the database does not necessarily contain a valid wallet state yet.
--
-- Caveat: When using this type, you have to pattern match, e.g. use it like this
-- @db & \DBLayer{..} -> …@. See Note [DBLayerRecordFields].
--
-- This type is polymorphic in the state type @s@,
-- but we often need additional constraints on the wallet state that allows us
-- to serialize and unserialize it (e.g. @forall s. (Serialize s) => ...@).
data DBLayer m s k = forall stm. (MonadIO stm, MonadFail stm) => DBLayer
    { walletId_ :: WalletId
    , initializeWallet
        :: Wallet s
        -> WalletMetadata
        -> [(Tx, TxMeta)]
        -> GenesisParameters
        -> ExceptT ErrWalletAlreadyInitialized stm ()
        -- ^ Initialize a database entry for a given wallet. 'putCheckpoint',
        -- 'putWalletMeta', 'putTxHistory' or 'putProtocolParameters' will
        -- actually all fail if they are called _first_ on a wallet.

    , walletsDB
        :: DBVar stm (DeltaMap WalletId (DeltaWalletState s))
        -- ^ 'DBVar' containing the 'WalletState' of each wallet in the database.
        -- Currently contains all 'Checkpoints' of the 'UTxO' and the
        -- 'Discoveries', as well as the 'Prologue' of the address discovery state.
        --
        -- Intended to replace 'putCheckpoint' and 'readCheckpoint' in the short-term,
        -- and all other functions in the long-term.

    , putCheckpoint
        :: Wallet s
        -> ExceptT ErrWalletNotInitialized stm ()
        -- ^ Replace the current checkpoint for a given wallet. We do not handle
        -- rollbacks yet, and therefore only stores the latest available
        -- checkpoint.
        --
        -- If the wallet doesn't exist, this operation returns an error.

    , readCheckpoint
        :: stm (Maybe (Wallet s))
        -- ^ Fetch the most recent checkpoint of a given wallet.
        --
        -- Return 'Nothing' if there's no such wallet.

    , listCheckpoints
        :: stm [ChainPoint]
        -- ^ List all known checkpoint tips, ordered by slot ids from the oldest
        -- to the newest.

    , putWalletMeta
        :: WalletMetadata
        -> ExceptT ErrWalletNotInitialized stm ()
        -- ^ Replace an existing wallet metadata with the given one.
        --
        -- If the wallet doesn't exist, this operation returns an error

    , readWalletMeta
        :: stm (Maybe (WalletMetadata, WalletDelegation))
        -- ^ Fetch a wallet metadata, if they exist.
        --
        -- Return 'Nothing' if there's no such wallet.

    , isStakeKeyRegistered
        :: ExceptT ErrWalletNotInitialized stm Bool

    , putDelegationCertificate
        :: DelegationCertificate
        -> SlotNo
        -> ExceptT ErrWalletNotInitialized stm ()
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
        :: Coin
        -> ExceptT ErrWalletNotInitialized stm ()
        -- ^ Store the latest known reward account balance.
        --
        -- This is separate from checkpoints because the data corresponds to the
        -- node tip.
        -- This is separate from putWalletMeta because it's not meta data

    , readDelegationRewardBalance
        :: stm Coin
        -- ^ Get the reward account balance.
        --
        -- Returns zero if the wallet isn't found or if wallet hasn't delegated
        -- stake.

    , putTxHistory
        :: [(Tx, TxMeta)]
        -> ExceptT ErrWalletNotInitialized stm ()
        -- ^ Augments the transaction history for a known wallet.
        --
        -- If an entry for a particular transaction already exists it is not
        -- altered nor merged (just ignored).
        --
        -- If the wallet doesn't exist, this operation returns an error.

    , readTransactions
        :: Maybe Coin
        -> SortOrder
        -> Range SlotNo
        -> Maybe TxStatus
        -> Maybe Natural
        -> stm [TransactionInfo]
        -- ^ Fetch the current transaction history of a known wallet, ordered by
        -- descending slot number.
        --
        -- Returns an empty list if the wallet isn't found.

    , getTx
        :: WalletId
        -> Hash "Tx"
        -> ExceptT ErrWalletNotInitialized stm (Maybe TransactionInfo)
        -- ^ Fetch the latest transaction by id, returns Nothing when the
        -- transaction isn't found.
        --
        -- If the wallet doesn't exist, this operation returns an error.

    , addTxSubmission
        :: WalletId
        -> BuiltTx
        -> SlotNo
        -> ExceptT ErrWalletNotInitialized stm ()
        -- ^ Add a /new/ transaction to the local submission pool
        -- with the most recent submission slot.

    , resubmitTx
        :: WalletId
        -> Hash "Tx"
        -> SealedTx -- TODO: ADP-2596 really not needed
        -> SlotNo
        -> ExceptT ErrWalletNotInitialized stm ()
        -- ^ Resubmit a transaction.

    , readLocalTxSubmissionPending
        :: WalletId
        -> stm [TxSubmissionsStatus]
        -- ^ List all transactions from the local submission pool which are
        -- still pending as of the latest checkpoint of the given wallet. The
        -- slot numbers for first submission and most recent submission are
        -- included.

    , rollForwardTxSubmissions
        :: WalletId
        -> SlotNo
        -> [(SlotNo, Hash "Tx")]
        -> ExceptT ErrWalletNotInitialized stm ()
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
        -> ExceptT ErrWalletNotInitialized stm ()
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
        -> ExceptT ErrWalletNotInitialized stm ChainPoint
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
        -> SlotNo
        -> ExceptT ErrWalletNotInitialized stm ()
        -- ^ Prune database entities and remove entities that can be discarded.
        --
        -- The second argument represents the stability window, or said
        -- length of the deepest rollback.
        --
        -- The third argument is the finality slot, or said
        -- most recent stable slot

    , atomically
        :: forall a. stm a -> m a
        -- ^ Execute operations of the database in isolation and atomically.
    }

{- Note [DBLayerRecordFields]

We can't use record accessors on the DBLayer as it carries an /existential/
within its constructor. We are forced to pattern-match on the `DBLayer`
record type in order to be able to use its methods in any context. With
NamedFieldPuns, or RecordWildCards, this can be quite easy:

@
myFunction DBLayer{..} = do
    ...

myOtherFunction DBLayer{atomically,initializeWallet} = do
    ...
@

Alternatively, in some other context where the database may not be a function
argument but come from a different source, it is possible to simply rely on
'Data.Function.(&)' to easily pattern match on it:

@
myFunction arg0 arg1 = db & \DBLayer{..} -> do
    ...
  where
    db = ...
@

Note that it isn't possible to simply use a @where@ clause or a @let@ binding
here as the semantic for those are slightly different: we really need a
pattern match here!
-}

hoistDBLayer :: (forall a . m a -> n a) -> DBLayer m s k -> DBLayer n s k
hoistDBLayer f DBLayer{..} = DBLayer {atomically = f . atomically, ..}

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
    , dbDelegation :: DBDelegation stm
    , dbTxHistory :: DBTxHistory stm
    , dbPrivateKey :: WalletId -> DBPrivateKey stm k

    -- The following two functions will need to be split up
    -- and distributed the smaller layer parts as well.
    , rollbackTo_
        :: WalletId
        -> Slot
        -> ExceptT ErrWalletNotInitialized stm ChainPoint
    , prune_
        :: WalletId
        -> Quantity "block" Word32
        -> SlotNo
        -> ExceptT ErrWalletNotInitialized stm ()
    , atomically_
        :: forall a. stm a -> m a
    }

{- HLINT ignore mkDBLayerFromParts "Avoid lambda" -}
-- | Create a legacy 'DBLayer' from smaller database layers.
mkDBLayerFromParts
    :: forall stm m s k. (MonadIO stm, MonadFail stm)
    => TimeInterpreter IO
    -> WalletId
    -> DBLayerCollection stm m s k
    -> DBLayer m s k
mkDBLayerFromParts ti wid_ DBLayerCollection{..} = DBLayer
    { walletId_ = wid_
    , initializeWallet = initializeWallet_ dbWallets
    , walletsDB = walletsDB_ dbCheckpoints
    , putCheckpoint = putCheckpoint_ dbCheckpoints
    , readCheckpoint = readCheckpoint'
    , listCheckpoints = listCheckpoints_ dbCheckpoints
    , putWalletMeta = putWalletMeta_ dbWalletMeta
    , readWalletMeta = do
        readCheckpoint' >>= \case
            Nothing -> pure Nothing
            Just cp -> do
                currentEpoch <- liftIO $
                    interpretQuery ti (epochOf $ cp ^. #currentTip . #slotNo)
                del <- readDelegation_ dbDelegation currentEpoch
                mwm <- readWalletMeta_ dbWalletMeta
                pure $ mwm <&> (, del)
    , isStakeKeyRegistered = wrapNoSuchWallet wid_ $
        isStakeKeyRegistered_ dbDelegation
    , putDelegationCertificate = \a b -> wrapNoSuchWallet wid_ $
        putDelegationCertificate_ dbDelegation a b
    , putDelegationRewardBalance = \a -> wrapNoSuchWallet wid_ $
        putDelegationRewardBalance_ dbDelegation a
    , readDelegationRewardBalance = readDelegationRewardBalance_ dbDelegation
    , putTxHistory = \a -> wrapNoSuchWallet wid_ $
        putTxHistory_ dbTxHistory a
    , readTransactions = \minWithdrawal order range status limit ->
        readCurrentTip >>= \case
            Just tip -> do
                inLedgers <- if status `elem` [Nothing, Just WTxMeta.InLedger]
                    then readTxHistory_ dbTxHistory range tip limit order
                    else pure []
                let isInSubmission = has (txStatus . _InSubmission)
                    isExpired = has (txStatus . _Expired)
                    whichSubmission :: TxSubmissionsStatus -> Bool
                    whichSubmission = case status of
                        Nothing -> (||) <$> isInSubmission <*> isExpired
                        Just WTxMeta.Pending -> isInSubmission
                        Just WTxMeta.Expired -> isExpired
                        Just WTxMeta.InLedger -> const False
                inSubmissionsRaw <- withSubmissions wid_ [] $
                        pure
                            . filter whichSubmission
                            . getInSubmissionTransactions
                inSubmissions :: [TransactionInfo] <- fmap catMaybes
                    $ for inSubmissionsRaw $
                        mkTransactionInfo
                            (hoistTimeInterpreter liftIO ti)
                            (mkDecorator_ dbTxHistory) tip
                        . fmap snd
                pure
                    . maybe id (take . fromIntegral) limit
                    . sortTransactionsBySlot order
                    . filterMinWithdrawal minWithdrawal
                    $ inLedgers <> inSubmissions
            Nothing -> pure []
    , getTx = \wid txid -> wrapNoSuchWallet wid $ do
        readCurrentTip >>= \case
            Just tip -> do
                historical <- getTx_ dbTxHistory txid tip
                case historical of
                    Just tx -> pure $ Just tx
                    Nothing ->  withSubmissions wid Nothing $ \submissions -> do
                        let inSubmission =
                                getInSubmissionTransaction txid submissions
                        fmap join $ for inSubmission $
                            mkTransactionInfo
                            (hoistTimeInterpreter liftIO ti)
                            (mkDecorator_ dbTxHistory) tip
                                . fmap snd
            Nothing -> pure Nothing
    , addTxSubmission = \wid builtTx slotNo  -> updateSubmissions' wid
            mapNoSuchWallet
            $ \_ -> Right [Sbms.addTxSubmission builtTx slotNo]
    , readLocalTxSubmissionPending = \wid -> withSubmissions wid [] $ \xs -> do
        pure $ filter (has $ txStatus . _InSubmission) $
            getInSubmissionTransactions xs
    , resubmitTx = \wid hash _ slotNo -> updateSubmissions' wid mapNoSuchWallet
            $ Right . Sbms.resubmitTx hash slotNo
    , rollForwardTxSubmissions = \wid tip txs -> updateSubmissions' wid
            mapNoSuchWallet
            $ \_ -> Right [Sbms.rollForwardTxSubmissions tip txs]
    , removePendingOrExpiredTx = \wid txid ->
            updateSubmissions' wid (ErrRemoveTxNoSuchWallet . mapNoSuchWallet)
                $ \xs ->
                pure <$> Sbms.removePendingOrExpiredTx xs txid
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
    withSubmissions wid def action = do
        mstate <- fmap (Map.lookup wid)
            $ readDBVar $ walletsDB_ dbCheckpoints
        case mstate of
            Nothing -> pure def
            Just state -> action $ submissions state
    mapNoSuchWallet _ = ErrWalletNotInitialized
    updateSubmissions' = updateSubmissions dbCheckpoints
    readCheckpoint' = readCheckpoint_ dbCheckpoints
    wrapNoSuchWallet
        :: WalletId
        -> stm a
        -> ExceptT ErrWalletNotInitialized stm a
    wrapNoSuchWallet wid action = ExceptT $
        hasWallet_ dbWallets wid >>= \case
            False -> pure $ Left ErrWalletNotInitialized
            True  -> Right <$> action

    readCurrentTip :: stm (Maybe BlockHeader)
    readCurrentTip =
        fmap currentTip <$> readCheckpoint_ dbCheckpoints

-- | Update the transaction submission state of a wallet.
updateSubmissions
    :: Monad stm
    => DBCheckpoints stm s
    -> WalletId
    -> (ErrNoSuchWallet -> e)
    -> (TxSubmissions -> Either e [DeltaTxSubmissions])
    -> ExceptT e stm ()
updateSubmissions dbCheckpoints wid emap xs
    = ExceptT
        $ modifyDBMaybe (walletsDB_ dbCheckpoints)
        $ adjustNoSuchWallet wid emap
        $ \submissionsData ->
            (\ys -> ([UpdateSubmissions ys], ()))
                <$>  xs (submissions submissionsData)

-- | A database layer for a collection of wallets
data DBWallets stm s = DBWallets
    { initializeWallet_
        :: Wallet s
        -> WalletMetadata
        -> [(Tx, TxMeta)]
        -> GenesisParameters
        -> ExceptT ErrWalletAlreadyInitialized stm ()
        -- ^ Initialize a database entry for a given wallet. 'putCheckpoint',
        -- 'putWalletMeta', 'putTxHistory' or 'putProtocolParameters' will
        -- actually all fail if they are called _first_ on a wallet.

    , readGenesisParameters_
        :: WalletId
        -> stm (Maybe GenesisParameters)
        -- ^ Read the *Byron* genesis parameters.

    , getWalletId_
        :: ExceptT ErrWalletNotInitialized stm WalletId
        -- ^ Get the 'WalletId' of the wallet stored in the DB.

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
        :: Wallet s
        -> ExceptT ErrWalletNotInitialized stm ()
        -- ^ Replace the current checkpoint for a given wallet. We do not handle
        -- rollbacks yet, and therefore only stores the latest available
        -- checkpoint.
        --
        -- If the wallet doesn't exist, this operation returns an error.

    , readCheckpoint_
        :: stm (Maybe (Wallet s))
        -- ^ Fetch the most recent checkpoint of a given wallet.
        --
        -- Return 'Nothing' if there's no such wallet.

    , listCheckpoints_
        :: stm [ChainPoint]
        -- ^ List all known checkpoint tips, ordered by slot ids from the oldest
        -- to the newest.
    }

-- | A database layer for storing 'WalletMetadata'.
data DBWalletMeta stm = DBWalletMeta
    { putWalletMeta_
        :: WalletMetadata
        -> ExceptT ErrWalletNotInitialized stm ()
        -- ^ Replace an existing wallet metadata with the given one.
        --
        -- If the wallet doesn't exist, this operation returns an error

    , readWalletMeta_
        :: stm (Maybe WalletMetadata)
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
    , readDelegation_
        :: EpochNo
        -> stm WalletDelegation
    }

-- | A database layer that stores the transaction history.
data DBTxHistory stm = DBTxHistory
    { putTxHistory_
        :: [(Tx, TxMeta)]
        -> stm ()
        -- ^ Augments the transaction history for a known wallet.
        --
        -- If an entry for a particular transaction already exists it is not
        -- altered nor merged (just ignored).
        --
        -- If the wallet does not exist, the function may throw
        -- an error, but need not.

    , readTxHistory_
        :: Range SlotNo
        -> BlockHeader
        -> Maybe Natural
        -> SortOrder
        -> stm [TransactionInfo]
        -- ^ Fetch the current transaction history of a known wallet, ordered by
        -- descending slot number.
        --
        -- Returns an empty list if the wallet isn't found.

    , getTx_
        :: Hash "Tx"
        -> BlockHeader
        -> stm (Maybe TransactionInfo)
        -- ^ Fetch the latest transaction by id, returns Nothing when the
        -- transaction isn't found.
        --
        -- If the wallet doesn't exist, this operation returns an error.

    , mkDecorator_ :: TxInDecorator (EraValue Read.Tx) stm
        -- ^ Resolve TxIn for a given Tx.
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

mkTransactionInfo
    :: Monad stm
    => TimeInterpreter stm
    -> TxInDecorator (EraValue Read.Tx) stm
    -> BlockHeader
    -> TxStatusMeta SubmissionMeta SlotNo WST.SealedTx
    -> stm (Maybe TransactionInfo)
mkTransactionInfo ti decorator tip = \case
        ( TxStatusMeta ( Subm.InSubmission _slot tx) meta)
            -> make WTxMeta.Pending tx meta
        ( TxStatusMeta ( Subm.Expired _slot tx) meta)
            -> make WTxMeta.Expired tx meta
        ( TxStatusMeta ( Subm.InLedger _ _slot tx) meta)
            -> make WTxMeta.InLedger tx meta
            -- Note: `meta` will contain the slot and block height
            -- when the transaction was submitted,
            -- not the slot or block height when the transaction
            -- was accepted into the ledger.
        _ -> pure Nothing
  where
    make s tx meta = do
        let readTx = fromSealedTx tx
        decorate <- decorator readTx
        Just <$> mkTransactionInfoFromReadTx
            ti tip readTx decorate meta s
