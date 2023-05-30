{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

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
    , DBLayerParams(..)
    , DBFresh (..)

    -- * DBLayer building blocks
    , DBLayerCollection (..)
    , DBCheckpoints (..)
    , DBDelegation (..)
    , DBTxHistory (..)
    , mkDBLayerFromParts

    , hoistDBLayer
      -- * Errors
    , module Cardano.Wallet.DB.Errors
    , hoistDBFresh
    ) where

import Prelude

import Cardano.Wallet.DB.Errors
import Cardano.Wallet.DB.Store.Submissions.Layer
    ( getInSubmissionTransaction, getInSubmissionTransactions )
import Cardano.Wallet.DB.Store.Submissions.Operations
    ( SubmissionMeta (..), TxSubmissions, TxSubmissionsStatus )
import Cardano.Wallet.DB.Store.Transactions.Decoration
    ( TxInDecorator )
import Cardano.Wallet.DB.Store.Transactions.TransactionInfo
    ( mkTransactionInfoFromReadTx )
import Cardano.Wallet.DB.Store.Wallets.Layer
    ( QueryTxWalletsHistory )
import Cardano.Wallet.DB.Store.Wallets.Model
    ( DeltaTxWalletsHistory )
import Cardano.Wallet.DB.WalletState
    ( DeltaWalletState, WalletState (submissions), updateSubmissions )
import Cardano.Wallet.Primitive.Model
    ( Wallet, currentTip )
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
import Control.Lens
    ( has )
import Control.Monad
    ( join )
import Control.Monad.IO.Class
    ( MonadIO, liftIO )
import Control.Monad.Trans.Except
    ( ExceptT (..), mapExceptT )
import Data.DBVar
    ( DBVar, readDBVar )
import Data.Generics.Internal.VL
    ( (^.) )
import Data.List
    ( sortOn )
import Data.Maybe
    ( catMaybes )
import Data.Ord
    ( Down (..) )
import Data.Store
    ( Store (..) )
import Data.Traversable
    ( for )
import GHC.Num
    ( Natural )

import qualified Cardano.Wallet.DB.Store.Submissions.Layer as Sbms
import qualified Cardano.Wallet.Primitive.Types.Tx.SealedTx as WST
import qualified Cardano.Wallet.Primitive.Types.Tx.TxMeta as WTxMeta
import qualified Cardano.Wallet.Read.Tx as Read
import qualified Cardano.Wallet.Submissions.TxStatus as Subm
import qualified Data.Delta.Update as Delta
import qualified Data.Map.Strict as Map

{-----------------------------------------------------------------------------
    DBFactory
------------------------------------------------------------------------------}
-- | A facility for managing a mapping between 'WalletId's and
-- databases of wallet states.
--
-- In our use case, this will typically be a directory of database files,
-- or a 'Map' of in-memory SQLite databases.
data DBFactory m s = DBFactory
    { withDatabase :: forall a. WalletId -> (DBFresh m s -> IO a) -> IO a
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
newtype DBOpen stm m s = DBOpen
    { atomically :: forall a. stm a -> m a
        -- ^ Execute a sequence of database operations atomically.
    }

{-----------------------------------------------------------------------------
    DBFresh
------------------------------------------------------------------------------}
-- | Necessary arguments to create a new wallet.
data DBLayerParams s = DBLayerParams
    { dBLayerParamsState :: Wallet s
    , dBLayerParamsMetadata :: WalletMetadata
    , dBLayerParamsHistory :: [(Tx, TxMeta)]
    , dBLayerParamsGenesisParameters :: GenesisParameters
    }
    deriving (Eq, Show)

-- | An open database which can store the state of one wallet with a specific
-- 'WalletId'.
--
-- This database does not necessarily contain a valid wallet state yet.
-- You can initialize a valid wallet state with 'bootDBLayer',
-- or load a valid wallet state from the database using 'loadDBLayer'.
data DBFresh m s = DBFresh
    { bootDBLayer
        :: DBLayerParams s
        -> ExceptT ErrWalletAlreadyInitialized m (DBLayer m s)
        -- ^ Initialize a database
    , loadDBLayer :: ExceptT ErrWalletNotInitialized m (DBLayer m s)
        -- ^ Load an existing database
    }

hoistDBFresh :: Functor m => (forall a. m a -> n a) -> DBFresh m s -> DBFresh n s
hoistDBFresh f (DBFresh boot load) = DBFresh
    { bootDBLayer = \params -> mapExceptT f $ hoistDBLayer f <$> boot params
    , loadDBLayer = mapExceptT f $ hoistDBLayer f <$> load
    }

{-----------------------------------------------------------------------------
    DBLayer
------------------------------------------------------------------------------}
-- | An open database which contains the state of one wallet with a specific
-- 'WalletId'. You can access the state using 'walletState'.
--
-- Caveat: When using this type, you have to pattern match, e.g. use it like this
-- @db & \DBLayer{..} -> …@. See Note [DBLayerRecordFields].
--
-- This type is polymorphic in the state type @s@,
-- but we often need additional constraints on the wallet state that allows us
-- to serialize and unserialize it (e.g. @forall s. (Serialize s) => ...@).
data DBLayer m s = forall stm. (MonadIO stm, MonadFail stm) => DBLayer
    { walletId_ :: WalletId

    , walletState
        :: DBVar stm (DeltaWalletState s)
        -- ^ 'DBVar' containing the 'WalletState' of each wallet in the database.
        -- Currently contains all 'Checkpoints' of the 'UTxO' and the
        -- 'Discoveries', as well as the 'Prologue' of the address discovery state.
        --
        -- Intended to replace 'putCheckpoint' and 'readCheckpoint' in the short-term,
        -- and all other functions in the long-term.

    , transactionsStore :: Store stm QueryTxWalletsHistory DeltaTxWalletsHistory
        -- ^ 'Store' containing all transactions of all wallets in the database.

    , readCheckpoint :: stm (Wallet s)
        -- ^ Fetch the most recent checkpoint of a given wallet.

    , listCheckpoints
        :: stm [ChainPoint]
        -- ^ List all known checkpoint tips, ordered by slot ids from the oldest
        -- to the newest.

    , readDelegation :: stm WalletDelegation

    , putDelegationCertificate
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

    , putDelegationRewardBalance :: Coin -> stm ()
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

    , putTxHistory :: [(Tx, TxMeta)] -> stm ()
        -- ^ Augments the transaction history for a known wallet.
        --
        -- If an entry for a particular transaction already exists it is not
        -- altered nor merged (just ignored).

    , readTransactions
        :: Maybe Coin
        -> SortOrder
        -> Range SlotNo
        -> Maybe TxStatus
        -> Maybe Natural
        -> stm [TransactionInfo]
        -- ^ Fetch the current transaction history of a known wallet, ordered by
        -- descending slot number.

    , getTx
        :: Hash "Tx"
        -> stm (Maybe TransactionInfo)
        -- ^ Fetch the latest transaction by id, returns Nothing when the
        -- transaction isn't found.

    , resubmitTx
        :: Hash "Tx"
        -> SealedTx -- TODO: ADP-2596 really not needed
        -> SlotNo
        -> stm ()
        -- ^ Resubmit a transaction.

    , rollForwardTxSubmissions
        :: SlotNo
        -> [(SlotNo, Hash "Tx")]
        -> stm ()
        -- ^ Removes any expired transactions from the pending set and marks
        -- their status as expired.

    , readGenesisParameters
        :: stm (Maybe GenesisParameters)
        -- ^ Read the *Byron* genesis parameters.

    , rollbackTo
        :: Slot
        -> stm ChainPoint
        -- ^ Drops all checkpoints and transaction data which
        -- have appeared after the given 'ChainPoint'.
        --
        -- Returns the actual 'ChainPoint' to which the database has rolled back.
        -- Its slot is guaranteed to be earlier than (or identical to) the given
        -- point of rollback but can't be guaranteed to be exactly the same
        -- because the database may only keep sparse checkpoints.

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

hoistDBLayer :: (forall a . m a -> n a) -> DBLayer m s -> DBLayer n s
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
data DBLayerCollection stm m s = DBLayerCollection
    { dbCheckpoints :: DBCheckpoints stm s
    , dbDelegation :: DBDelegation stm
    , dbTxHistory :: DBTxHistory stm

    -- The following two functions will need to be split up
    -- and distributed the smaller layer parts as well.
    , rollbackTo_
        :: Slot
        -> stm ChainPoint
    , atomically_
        :: forall a. stm a -> m a
    , transactionsStore_
        :: Store stm QueryTxWalletsHistory DeltaTxWalletsHistory
    }

{- HLINT ignore mkDBLayerFromParts "Avoid lambda" -}
-- | Create a legacy 'DBLayer' from smaller database layers.
mkDBLayerFromParts
    :: forall stm m s. (MonadIO stm, MonadFail stm)
    => TimeInterpreter IO
    -> WalletId
    -> DBLayerCollection stm m s
    -> DBLayer m s
mkDBLayerFromParts ti wid_ DBLayerCollection{..} = DBLayer
    { walletId_ = wid_
    , walletState = walletsDB_ dbCheckpoints
    , transactionsStore = transactionsStore_
    , readCheckpoint = readCheckpoint'
    , listCheckpoints = listCheckpoints_ dbCheckpoints
    , readDelegation = do
        readCheckpoint' >>= \cp -> do
            currentEpoch <- liftIO $
                interpretQuery ti (epochOf $ cp ^. #currentTip . #slotNo)
            readDelegation_ dbDelegation currentEpoch
    , putDelegationCertificate = putDelegationCertificate_ dbDelegation
    , putDelegationRewardBalance = putDelegationRewardBalance_ dbDelegation
    , readDelegationRewardBalance = readDelegationRewardBalance_ dbDelegation
    , putTxHistory = putTxHistory_ dbTxHistory
    , readTransactions = \minWithdrawal order range status limit ->
        readCurrentTip >>= \tip -> do
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
                inSubmissionsRaw <- withSubmissions $
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
    , getTx = \txid -> readCurrentTip >>= \tip -> do
        historical <- getTx_ dbTxHistory txid tip
        case historical of
            Just tx -> pure $ Just tx
            Nothing ->  withSubmissions $ \submissions -> do
                let inSubmission =
                        getInSubmissionTransaction txid submissions
                fmap join $ for inSubmission $
                    mkTransactionInfo
                    (hoistTimeInterpreter liftIO ti)
                    (mkDecorator_ dbTxHistory) tip
                        . fmap snd
    , resubmitTx = \hash _ slotNo ->
            updateSubmissionsNoError dbCheckpoints
                $ Sbms.resubmitTx hash slotNo
    , rollForwardTxSubmissions = \tip txs ->
            updateSubmissionsNoError dbCheckpoints
                $ \_ -> Sbms.rollForwardTxSubmissions tip txs
    , readGenesisParameters = readGenesisParameters_ dbCheckpoints
    , rollbackTo = rollbackTo_
    , atomically = atomically_
    }
  where
    withSubmissions :: forall a. (TxSubmissions -> stm a) -> stm a
    withSubmissions action = do
        state <- readDBVar $ walletsDB_ dbCheckpoints
        action $ submissions state
    readCheckpoint' = readCheckpoint_ dbCheckpoints
    readCurrentTip :: stm BlockHeader
    readCurrentTip = currentTip <$> readCheckpoint_ dbCheckpoints
    updateSubmissionsNoError db
        = Delta.onDBVar (walletsDB_ db)
        . updateSubmissions
        . Delta.update

-- | A database layer for storing wallet states.
data DBCheckpoints stm s = DBCheckpoints
    { walletsDB_
        :: DBVar stm (DeltaWalletState s)
        -- ^ 'DBVar' containing the 'WalletState' of each wallet in the database.
        -- Currently contains all 'Checkpoints' of the 'UTxO' and the
        -- 'Discoveries', as well as the 'Prologue' of the address discovery state.
        --
        -- Intended to replace 'putCheckpoint' and 'readCheckpoint' in the short-term,
        -- and all other functions in the long-term.

    , readCheckpoint_ :: stm (Wallet s)
        -- ^ Fetch the most recent checkpoint of a given wallet.
        --
        -- Return 'Nothing' if there's no such wallet.

    , listCheckpoints_
        :: stm [ChainPoint]
        -- ^ List all known checkpoint tips, ordered by slot ids from the oldest
        -- to the newest.

    , readGenesisParameters_
        :: stm (Maybe GenesisParameters)
        -- ^ Read the *Byron* genesis parameters.
    }

-- | A database layer for storing delegation certificates
-- and the reward balance.
data DBDelegation stm = DBDelegation
    { putDelegationCertificate_
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
