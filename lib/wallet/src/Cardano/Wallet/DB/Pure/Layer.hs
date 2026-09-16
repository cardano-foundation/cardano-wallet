{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Dummy implementation of the database-layer, using 'MVar'. This may be good
-- for testing to compare with an implementation on a real data store, or to use
-- when compiling the wallet for targets which don't have SQLite.
module Cardano.Wallet.DB.Pure.Layer
    ( withBootDBLayer
    , throwErrorReadDB
    ) where

import Cardano.Wallet.DB
    ( DBLayer (..)
    , DBLayerParams
    )
import Cardano.Wallet.DB.Pure.Implementation
    ( Database
    , Err (..)
    , ModelOp
    , mInitializeWallet
    , mListCheckpoints
    , mPutTxHistory
    , mReadCheckpoint
    , mReadGenesisParameters
    , mReadTxHistory
    , mRollbackTo
    )
import Cardano.Wallet.DB.Sqlite.Types
    ( DappSubmissionStatusEnum (..)
    , TxId
    )
import Cardano.Wallet.DB.Store.Submissions.Operations
    ( DurableSubmission (..)
    , DurableSubmissionInput (..)
    , DurableSubmissionInsert (..)
    )
import Cardano.Wallet.Primitive.Slotting
    ( TimeInterpreter
    )
import Cardano.Wallet.Primitive.Types
    ( SortOrder (..)
    , WalletId
    )
import Cardano.Wallet.Primitive.Types.Tx.TransactionInfo
    ( TransactionInfo (..)
    )
import Control.Applicative
    ( (<|>)
    )
import Control.Concurrent.MVar
    ( MVar
    , modifyMVar
    , newMVar
    , withMVar
    )
import Control.Monad
    ( join
    )
import Control.Monad.IO.Class
    ( MonadIO (..)
    )
import Data.Functor.Identity
    ( Identity (..)
    )
import Data.List
    ( find
    , nub
    , sort
    )
import Data.Map.Strict
    ( Map
    )
import Data.Maybe
    ( fromMaybe
    )
import Data.Time.Clock
    ( UTCTime
    )
import Data.Word
    ( Word32
    , Word64
    )
import UnliftIO.Exception
    ( Exception
    , throwIO
    )
import Prelude

import qualified Cardano.Wallet.Primitive.Types.Range as Range
import qualified Data.Map.Strict as Map

-- | Instantiate a new in-memory "database" layer that simply stores data in
-- a local MVar. Data vanishes if the software is shut down.
withBootDBLayer
    :: forall m s
     . (MonadIO m)
    => TimeInterpreter Identity
    -> WalletId
    -> DBLayerParams s
    -> (DBLayer IO s -> m ())
    -> m ()
withBootDBLayer timeInterpreter wid params k = do
    lock <- liftIO $ newMVar ()
    db <- liftIO $ newMVar $ mInitializeWallet wid params
    durable <- liftIO $ newMVar []
    k
        $ DBLayer
            { {-----------------------------------------------------------------------
                                            Wallets
              -----------------------------------------------------------------------}

              walletId_ = wid
            , {-----------------------------------------------------------------------
                                          Checkpoints
              -----------------------------------------------------------------------}
              walletState = error "MVar.walletState: not implemented"
            , transactionsStore = error "MVar.transactionsStore: not implemented"
            , readCheckpoint = throwErrorReadDB db mReadCheckpoint
            , listCheckpoints = fromMaybe [] <$> readDBMaybe db mListCheckpoints
            , rollbackTo =
                noErrorAlterDB db
                    . mRollbackTo
            , {-----------------------------------------------------------------------
                                           Tx History
              -----------------------------------------------------------------------}

              putTxHistory = noErrorAlterDB db . mPutTxHistory
            , readTransactions = \minWithdrawal order range mstatus _mlimit maddress ->
                fmap (fromMaybe [])
                    $ readDBMaybe db
                    $ mReadTxHistory
                        timeInterpreter
                        minWithdrawal
                        order
                        range
                        mstatus
                        maddress
            , -- TODO: shift implementation to mGetTx
              getTx = \tid -> do
                txInfos <-
                    fmap (fromMaybe [])
                        $ readDBMaybe db
                        $ mReadTxHistory
                            timeInterpreter
                            Nothing
                            Descending
                            Range.everything
                            Nothing
                            Nothing
                let txPresent (TransactionInfo{..}) = txInfoId == tid
                case filter txPresent txInfos of
                    [] -> pure Nothing
                    t : _ -> pure $ Just t
            , {-----------------------------------------------------------------------
                                             Pending Tx
              -----------------------------------------------------------------------}

              resubmitTx =
                error "resubmitTx not tested in State Machine tests"
            , rollForwardTxSubmissions =
                error "rollForwardTxSubmissions not tested in State Machine tests"
            , {-----------------------------------------------------------------------
                                       Protocol Parameters
              -----------------------------------------------------------------------}

              readGenesisParameters = join <$> readDBMaybe db mReadGenesisParameters
            , {-----------------------------------------------------------------------
                                            Execution
              -----------------------------------------------------------------------}

              getSchemaVersion =
                error "getSchemaVersion not tested in State Machine tests"
            , atomically = \action -> withMVar lock $ \() -> action
            , insertDurableSubmission = insertPureDurable durable
            , claimDurableSubmissionAttempt = claimPureDurable durable wid
            , updateDurableSubmission = updatePureDurable durable
            , readDurableSubmissions = readPureDurable durable wid
            }

data PureDurableSubmission = PureDurableSubmission
    { pureSubmission :: DurableSubmission
    , pureInputs :: [DurableSubmissionInput]
    , pureClaimsActive :: Bool
    }

insertPureDurable
    :: MVar [PureDurableSubmission]
    -> DurableSubmission
    -> [DurableSubmissionInput]
    -> IO DurableSubmissionInsert
insertPureDurable store submission requested =
    modifyMVar store $ \rows ->
        case find (sameSubmission submission . pureSubmission) rows of
            Just stored
                | valid
                    && sameIdentity submission (pureSubmission stored)
                    && claims == pureInputs stored ->
                    pure
                        ( rows
                        , DurableSubmissionReplay $ pureSubmission stored
                        )
                | otherwise ->
                    pure (rows, DurableSubmissionIdentityConflict)
            Nothing
                | not valid || any (overlaps claims) rows ->
                    pure (rows, DurableSubmissionInputConflict)
                | otherwise ->
                    pure
                        ( PureDurableSubmission submission claims True : rows
                        , DurableSubmissionAuthorized
                        )
  where
    valid = length requested == length (nub requested)
    claims = normalizeInputs requested

claimPureDurable
    :: MVar [PureDurableSubmission]
    -> WalletId
    -> TxId
    -> Word64
    -> UTCTime
    -> IO (Maybe DurableSubmission)
claimPureDurable store walletId txId generation started =
    modifyMVar store $ \rows ->
        let (result, updated) = unzip $ claim <$> rows
        in  pure (updated, foldr (<|>) Nothing result)
  where
    claim stored@PureDurableSubmission{pureSubmission = submission}
        | durableWalletId submission == walletId
            && durableTxId submission == txId
            && durableAuthorized submission
            && durableStatus submission == AuthorizedE
            && durableAttemptGeneration submission == generation =
            let broadcasting =
                    submission
                        { durableStatus = BroadcastingE
                        , durableBroadcastGeneration = Just generation
                        , durableBroadcastStarted = Just started
                        }
            in  (Just broadcasting, stored{pureSubmission = broadcasting})
        | otherwise = (Nothing, stored)

updatePureDurable
    :: MVar [PureDurableSubmission]
    -> DurableSubmission
    -> IO ()
updatePureDurable store replacement =
    modifyMVar store $ \rows -> do
        let prior = find (sameSubmission replacement . pureSubmission) rows
            restoring =
                durableStatus replacement == SubmittedE
                    && maybe
                        False
                        ((== InLedgerDappE) . durableStatus . pureSubmission)
                        prior
            replacementInputs = maybe [] pureInputs prior
        if restoring
            && any (overlaps replacementInputs) (filter (not . sameRow) rows)
            then
                fail
                    "cannot roll back a submission while another active claim owns one of its inputs"
            else pure (replace <$> rows, ())
  where
    sameRow = sameSubmission replacement . pureSubmission
    replace stored
        | sameRow stored =
            stored
                { pureSubmission = replacement
                , pureClaimsActive =
                    case durableStatus replacement of
                        RejectedE -> False
                        ExpiredDappE -> False
                        InLedgerDappE -> False
                        _ -> True
                }
        | otherwise = stored

readPureDurable
    :: MVar [PureDurableSubmission]
    -> WalletId
    -> IO [DurableSubmission]
readPureDurable store walletId =
    withMVar store
        $ pure
            . fmap pureSubmission
            . filter ((== walletId) . durableWalletId . pureSubmission)

normalizeInputs
    :: [DurableSubmissionInput] -> [DurableSubmissionInput]
normalizeInputs =
    sort
        . Map.elems
        . Map.fromListWith min
        . fmap (\input -> (inputKey input, input))

overlaps :: [DurableSubmissionInput] -> PureDurableSubmission -> Bool
overlaps requested PureDurableSubmission{pureInputs, pureClaimsActive} =
    pureClaimsActive
        && any (`Map.member` claimed) (inputKey <$> requested)
  where
    claimed :: Map (TxId, Word32) ()
    claimed = Map.fromList $ (\input -> (inputKey input, ())) <$> pureInputs

inputKey :: DurableSubmissionInput -> (TxId, Word32)
inputKey DurableSubmissionInput{durableInputTxId, durableInputIndex} =
    (durableInputTxId, durableInputIndex)

sameSubmission :: DurableSubmission -> DurableSubmission -> Bool
sameSubmission left right =
    durableWalletId left == durableWalletId right
        && durableTxId left == durableTxId right

sameIdentity :: DurableSubmission -> DurableSubmission -> Bool
sameIdentity left right =
    sameSubmission left right
        && durableSealedTx left == durableSealedTx right

-- | Read the database, but return 'Nothing' if the operation fails.
readDBMaybe
    :: MonadIO m
    => MVar (Database WalletId s xprv)
    -> ModelOp WalletId s xprv a
    -> m (Maybe a)
readDBMaybe db = fmap (either (const Nothing) Just) . readDB db

-- | Apply an operation to the model database, then update the mutable variable.
-- Failures are converted to 'Err' using the provided function.
-- Failures that cannot be converted are rethrown as 'MVarDBError'.
alterDB
    :: MonadIO m
    => (Err -> Maybe err)
    -- ^ Error type converter
    -> MVar (Database WalletId s xprv)
    -- ^ The database variable
    -> ModelOp WalletId s xprv a
    -- ^ Operation to run on the database
    -> m (Either err a)
alterDB convertErr db op = liftIO $ modifyMVar db (bubble . op)
  where
    bubble (Left e, !db') = case convertErr e of
        Just e' -> pure (db', Left e')
        Nothing -> throwIO $ MVarDBError e
    bubble (Right a, !db') = pure (db', Right a)

noErrorAlterDB
    :: MonadIO m
    => MVar (Database WalletId s xprv)
    -> ModelOp WalletId s xprv a
    -> m a
noErrorAlterDB db op = do
    r <- alterDB (const Nothing) db op
    case r of
        Left e -> throwIO $ MVarDBError e
        Right a -> pure a

throwErrorReadDB
    :: MonadIO m
    => MVar (Database WalletId s xprv)
    -> ModelOp WalletId s xprv b
    -> m b
throwErrorReadDB db op = do
    mr <- readDB db op
    case mr of
        Left e -> throwIO $ MVarDBError e
        Right r -> pure r

-- | Run a query operation on the model database.
readDB
    :: MonadIO m
    => MVar (Database WalletId s xprv)
    -- ^ The database variable
    -> ModelOp WalletId s xprv a
    -- ^ Operation to run on the database
    -> m (Either Err a)
readDB = alterDB Just -- >>= either (throwIO . MVarDBError) pure

-- | Error which happens when model returns an unexpected value.
newtype MVarDBError = MVarDBError Err
    deriving (Show)

instance Exception MVarDBError
