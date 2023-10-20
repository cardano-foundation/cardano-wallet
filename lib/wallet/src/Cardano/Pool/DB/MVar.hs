{-# LANGUAGE DataKinds #-}
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

module Cardano.Pool.DB.MVar
    ( newDBLayer
    ) where

import Prelude

import Cardano.Pool.DB
    ( DBLayer (..)
    , ErrPointAlreadyExists (..)
    )
import Cardano.Pool.DB.Model
    ( ModelOp
    , PoolDatabase
    , PoolErr (..)
    , emptyPoolDatabase
    , mCleanDatabase
    , mCleanPoolMetadata
    , mListHeaders
    , mListPoolLifeCycleData
    , mListRegisteredPools
    , mListRetiredPools
    , mPutDelistedPools
    , mPutFetchAttempt
    , mPutHeader
    , mPutLastMetadataGC
    , mPutPoolMetadata
    , mPutPoolProduction
    , mPutPoolRegistration
    , mPutPoolRetirement
    , mPutSettings
    , mPutStakeDistribution
    , mReadCursor
    , mReadDelistedPools
    , mReadLastMetadataGC
    , mReadPoolLifeCycleStatus
    , mReadPoolMetadata
    , mReadPoolProduction
    , mReadPoolRegistration
    , mReadPoolRetirement
    , mReadSettings
    , mReadStakeDistribution
    , mReadSystemSeed
    , mReadTotalProduction
    , mRemovePools
    , mRemoveRetiredPools
    , mRollbackTo
    , mUnfetchedPoolMetadataRefs
    )
import Cardano.Wallet.Primitive.Slotting
    ( TimeInterpreter
    )
import Control.DeepSeq
    ( deepseq
    )
import Control.Monad
    ( void
    )
import Control.Monad.Trans.Except
    ( ExceptT (..)
    )
import Control.Monad.Trans.State.Strict
    ( runStateT
    )
import Data.Either
    ( fromRight
    )
import Data.Functor.Identity
    ( Identity
    )
import Data.Tuple
    ( swap
    )
import UnliftIO.Exception
    ( Exception
    , throwIO
    )
import UnliftIO.MVar
    ( MVar
    , modifyMVar
    , newMVar
    )

-- | Instantiate a new in-memory "database" layer that simply stores data in
-- a local MVar. Data vanishes if the software is shut down.
newDBLayer :: TimeInterpreter Identity -> IO (DBLayer IO)
newDBLayer timeInterpreter = do
    db <- newMVar emptyPoolDatabase
    pure $ mkDBLayer db
  where
    mkDBLayer db = DBLayer {..}
      where
        readPoolRegistration =
            readPoolDB db . mReadPoolRegistration

        readPoolRetirement =
            readPoolDB db . mReadPoolRetirement

        putPoolProduction sl pool = ExceptT $
            pool `deepseq`
                alterPoolDB errPointAlreadyExists db (mPutPoolProduction sl pool)

        readPoolProduction =
            readPoolDB db . mReadPoolProduction timeInterpreter

        readTotalProduction =
            readPoolDB db mReadTotalProduction

        putStakeDistribution a0 a1 =
            void $ alterPoolDB (const Nothing) db (mPutStakeDistribution a0 a1)

        readStakeDistribution =
            readPoolDB db . mReadStakeDistribution

        readPoolProductionCursor =
            readPoolDB db . mReadCursor

        putPoolRegistration cpt cert = void
              $ alterPoolDB (const Nothing) db
              $ mPutPoolRegistration cpt cert

        readPoolLifeCycleStatus =
            readPoolDB db . mReadPoolLifeCycleStatus

        putPoolRetirement cpt cert = void
            $ alterPoolDB (const Nothing) db
            $ mPutPoolRetirement cpt cert

        unfetchedPoolMetadataRefs =
            readPoolDB db . mUnfetchedPoolMetadataRefs

        putFetchAttempt =
            void . alterPoolDB (const Nothing) db . mPutFetchAttempt

        listRegisteredPools =
            readPoolDB db mListRegisteredPools

        listRetiredPools =
            readPoolDB db . mListRetiredPools

        listPoolLifeCycleData =
            readPoolDB db . mListPoolLifeCycleData

        putPoolMetadata a0 a1 =
            void $ alterPoolDB (const Nothing) db (mPutPoolMetadata a0 a1)

        removePoolMetadata =
            void $ alterPoolDB (const Nothing) db mCleanPoolMetadata

        readSystemSeed =
            modifyMVar db (fmap swap . mReadSystemSeed)

        rollbackTo =
            void . alterPoolDB (const Nothing) db . mRollbackTo timeInterpreter

        putDelistedPools =
            void . alterPoolDB (const Nothing) db . mPutDelistedPools

        readDelistedPools =
            readPoolDB db mReadDelistedPools

        removePools =
            void . alterPoolDB (const Nothing) db . mRemovePools

        removeRetiredPools =
            fmap (fromRight [])
                . alterPoolDB (const Nothing) db
                . mRemoveRetiredPools

        putHeader =
            void . alterPoolDB (const Nothing) db . mPutHeader

        listHeaders =
            readPoolDB db . mListHeaders

        readSettings = readPoolDB db mReadSettings

        putSettings =
            void . alterPoolDB (const Nothing) db . mPutSettings

        readLastMetadataGC = readPoolDB db mReadLastMetadataGC

        putLastMetadataGC =
            void . alterPoolDB (const Nothing) db . mPutLastMetadataGC

        cleanDB =
            void $ alterPoolDB (const Nothing) db mCleanDatabase

        readPoolMetadata = readPoolDB db mReadPoolMetadata

        atomically = id

alterPoolDB
    :: (PoolErr -> Maybe err)
    -- ^ Error type converter
    -> MVar PoolDatabase
    -- ^ The database variable
    -> ModelOp a
    -- ^ Operation to run on the database
    -> IO (Either err a)
alterPoolDB convertErr dbVar op =
    modifyMVar dbVar $ \db ->
        case runStateT op db of
            Left e -> case convertErr e of
                Just e' -> pure (db, Left e')
                Nothing -> throwIO $ MVarPoolDBError e
            Right (result, dbUpdated) ->
                pure (dbUpdated, Right result)

readPoolDB
    :: MVar PoolDatabase
    -- ^ The database variable
    -> ModelOp a
    -- ^ Operation to run on the database
    -> IO a
readPoolDB db op =
    alterPoolDB Just db op >>= either (throwIO . MVarPoolDBError) pure

errPointAlreadyExists
    :: PoolErr
    -> Maybe ErrPointAlreadyExists
errPointAlreadyExists (PointAlreadyExists slotid) =
    Just (ErrPointAlreadyExists slotid)

newtype MVarPoolDBError = MVarPoolDBError PoolErr
    deriving (Show)

instance Exception MVarPoolDBError
