{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Cardano.Pool.DB.Properties
    ( properties
    , withDB
    , newMemoryDBLayer
    ) where

import Prelude

import Cardano.BM.Configuration.Model
    ( Configuration )
import Cardano.BM.Data.LogItem
    ( LogObject (..) )
import Cardano.BM.Trace
    ( traceInTVarIO )
import Cardano.DB.Sqlite
    ( SqliteContext )
import Cardano.Pool.DB
    ( DBLayer (..), ErrSlotAlreadyExists (..) )
import Cardano.Pool.DB.Arbitrary
    ( StakePoolsFixture (..) )
import Cardano.Pool.DB.Sqlite
    ( newDBLayer )
import Cardano.Wallet.Primitive.Types
    ( PoolId, SlotId (..) )
import Cardano.Wallet.Unsafe
    ( unsafeRunExceptT )
import Control.Monad
    ( forM_ )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Monad.Trans.Except
    ( runExceptT )
import Data.Function
    ( on )
import Data.Functor
    ( ($>) )
import Data.List.Extra
    ( nubOrd )
import Data.Map.Strict
    ( Map )
import Data.Text
    ( Text )
import Data.Word
    ( Word64 )
import GHC.Conc
    ( TVar, newTVarIO )
import Test.Hspec
    ( Expectation
    , Spec
    , SpecWith
    , beforeAll
    , beforeWith
    , describe
    , it
    , shouldBe
    , shouldReturn
    )
import Test.QuickCheck
    ( Gen
    , Property
    , choose
    , classify
    , conjoin
    , counterexample
    , elements
    , property
    , shuffle
    , (==>)
    )
import Test.QuickCheck.Monadic
    ( assert, monadicIO, monitor )

import qualified Cardano.Pool.DB.MVar as MVar
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- | Provide a DBLayer to a Spec that requires it. The database is initialised
-- once, and cleared with 'cleanDB' before each test.
withDB :: IO (DBLayer IO) -> SpecWith (DBLayer IO) -> Spec
withDB create = beforeAll create . beforeWith (\db -> cleanDB db $> db)

-- | Set up a DBLayer for testing, with the command context, and the logging
-- variable.
newMemoryDBLayer :: IO Configuration -> IO (DBLayer IO)
newMemoryDBLayer conf = snd . snd <$> (newMemoryDBLayer' conf)

newMemoryDBLayer'
    :: IO Configuration -> IO (TVar [LogObject Text], (SqliteContext, DBLayer IO))
newMemoryDBLayer' testingLogConfig = do
    logConfig <- testingLogConfig
    logVar <- newTVarIO []
    (logVar, ) <$> newDBLayer logConfig (traceInTVarIO logVar) Nothing

properties :: SpecWith (DBLayer IO)
properties = do
    describe "Stake Pool properties" $ do
        it "putPoolProduction . readPoolProduction yields expected results"
            (property . prop_putReadPoolProduction)
        it "putPoolProduction with already put slot yields error"
            (property . prop_putSlotTwicePoolProduction)
        it "Rollback of stake pool production"
            (property . prop_rollbackPools)
        it "readPoolProduction for a given epoch should always give slots \
           \from given epoch"
            (property . prop_readPoolProductionEpochLeak)
        it "readPoolProduction should never give pools with no slots"
            (property . prop_readPoolProductionNoEmptyPools)
        it "readPoolProduction should never give pools with no slots \
           \after rollback - 1 slot rollbacks"
            (property . prop_readPoolProductionNoEmptyPoolsAfterRollback1)
        it "readPoolProduction should never give pools with no slots \
           \after rollback - arbitrary slot rollbacks"
            (property . prop_readPoolProductionNoEmptyPoolsAfterRollback2)

{-------------------------------------------------------------------------------
                                    Properties
-------------------------------------------------------------------------------}

-- | Can read put pool production
prop_putReadPoolProduction
    :: DBLayer IO
    -> StakePoolsFixture
    -> Property
prop_putReadPoolProduction db (StakePoolsFixture pairs _) =
    monadicIO (setup >>= prop)
  where
    setup = liftIO $ do
        cleanDB db
        db' <- MVar.newDBLayer
        cleanDB db'
        pure db'
    prop db' = liftIO $ do
        forM_ pairs $ \(pool, slot) ->
            unsafeRunExceptT $ putPoolProduction db slot pool
        forM_ pairs $ \(pool, slot) ->
            unsafeRunExceptT $ putPoolProduction db' slot pool
        forM_ (uniqueEpochs pairs) $ \epoch -> do
            res' <- readPoolProduction db' epoch
            readPoolProduction db epoch `shouldReturn` res'

-- | Cannot put pool production with already put slot
prop_putSlotTwicePoolProduction
    :: DBLayer IO
    -> StakePoolsFixture
    -> Property
prop_putSlotTwicePoolProduction db (StakePoolsFixture pairs _) =
    monadicIO (setup >>= prop)
  where
    setup = liftIO $ do
        cleanDB db
        db' <- MVar.newDBLayer
        cleanDB db'
        pure db'
    prop db' = liftIO $ do
        forM_ pairs $ \(pool, slot) -> do
            let err = ErrSlotAlreadyExists slot
            runExceptT (putPoolProduction db' slot pool) `shouldReturn` Right ()
            runExceptT (putPoolProduction db' slot pool) `shouldReturn` Left err
            runExceptT (putPoolProduction db slot pool) `shouldReturn` Right ()
            runExceptT (putPoolProduction db slot pool) `shouldReturn` Left err

-- | Rolling back wipes out pool production statistics after the rollback point.
prop_rollbackPools
    :: DBLayer IO
    -> StakePoolsFixture
    -> SlotId
    -> Property
prop_rollbackPools db f@(StakePoolsFixture pairs _) sl =
    monadicIO prop
  where
    prop = do
        (beforeRollback, afterRollback) <- liftIO $ do
            forM_ pairs $ \(pool, slot) ->
                runExceptT $ putPoolProduction db slot pool
            before <- map fst <$> allPoolProduction db f
            rollbackTo db sl
            after <- map fst <$> allPoolProduction db f
            pure (before, after)

        monitor $ counterexample $ unlines
            [ "Rollback point:    " <> showSlot sl
            , "Production before: " <> unwords (map showSlot beforeRollback)
            , "Production after:  " <> unwords (map showSlot afterRollback)
            ]
        monitor $ classify (any (> sl) beforeRollback) "something to roll back"
        monitor $ classify (all (<= sl) beforeRollback) "nothing to roll back"

        assert $ all (<= sl) afterRollback

    showSlot (SlotId epoch slot) = show epoch ++ "." ++ show slot

-- | Can read pool production only for a given epoch
prop_readPoolProductionEpochLeak
    :: DBLayer IO
    -> StakePoolsFixture
    -> Property
prop_readPoolProductionEpochLeak db (StakePoolsFixture pairs _) =
    monadicIO (setup >> prop)
  where
    slotPartition = L.groupBy ((==) `on` epochNumber)
        $ L.sortOn epochNumber
        $ map snd pairs
    epochGroups = L.zip (uniqueEpochs pairs) slotPartition
    setup = liftIO $ cleanDB db
    prop = liftIO $ do
        forM_ pairs $ \(pool, slot) ->
            unsafeRunExceptT $ putPoolProduction db slot pool
        forM_ epochGroups $ \(epoch, slots) -> do
            slots' <-
                (Set.fromList . concat . Map.elems) <$> readPoolProduction db epoch
            slots' `shouldBe` (Set.fromList slots)

-- | Read pool production only gives pools with non-empty slots
prop_readPoolProductionNoEmptyPools
    :: DBLayer IO
    -> StakePoolsFixture
    -> Property
prop_readPoolProductionNoEmptyPools db (StakePoolsFixture pairs _) =
    monadicIO (setup >> prop)
  where
    setup = liftIO $ cleanDB db
    prop = liftIO $ do
        forM_ pairs $ \(pool, slot) ->
            unsafeRunExceptT $ putPoolProduction db slot pool
        forM_ (uniqueEpochs pairs) $ \epoch -> do
            res <- readPoolProduction db epoch
            noEmptyPools res

-- | Read pool production only gives pools with non-empty slots after
-- 1 slot length rollback
prop_readPoolProductionNoEmptyPoolsAfterRollback1
    :: DBLayer IO
    -> StakePoolsFixture
    -> Property
prop_readPoolProductionNoEmptyPoolsAfterRollback1 db (StakePoolsFixture pairs _) =
    monadicIO (setup >> prop)
  where
    setup = liftIO $ cleanDB db
    slots = map snd pairs
    prop = liftIO $ do
        forM_ pairs $ \(pool, slot) ->
            unsafeRunExceptT $ putPoolProduction db slot pool
        forM_ slots $ \slot -> do
            _ <- rollbackTo db slot
            forM_ (uniqueEpochs pairs) $ \epoch -> do
                res <- readPoolProduction db epoch
                noEmptyPools res

-- | Read pool production only gives pools with non-empty slots after
-- arbitrary slot length rollback
prop_readPoolProductionNoEmptyPoolsAfterRollback2
    :: DBLayer IO
    -> StakePoolsFixture
    -> Property
prop_readPoolProductionNoEmptyPoolsAfterRollback2 db (StakePoolsFixture pairs rSlots) =
    (length pairs > 10) ==> monadicIO (setup >> prop)
  where
    setup = liftIO $ cleanDB db
    prop = liftIO $ do
        forM_ pairs $ \(pool, slot) ->
            unsafeRunExceptT $ putPoolProduction db slot pool
        forM_ rSlots $ \slot -> do
            rollbackTo db slot
            forM_ (uniqueEpochs pairs) $ \epoch -> do
                res <- readPoolProduction db epoch
                noEmptyPools res

noEmptyPools :: Map PoolId [SlotId] -> Expectation
noEmptyPools pools = do
    let pools' = Map.filter (not . null) pools
    pools' `shouldBe` pools

uniqueEpochs :: [(PoolId, SlotId)] -> [Word64]
uniqueEpochs = nubOrd . map (epochNumber . snd)

-- | Concatenate stake pool production for all epochs in the test fixture.
allPoolProduction :: DBLayer IO -> StakePoolsFixture -> IO [(SlotId, PoolId)]
allPoolProduction db (StakePoolsFixture pairs _) =
    rearrange <$> mapM (readPoolProduction db) (uniqueEpochs pairs)
  where
    rearrange ms = concat
        [ [ (sl, p) | sl <- sls ] | (p, sls) <- concatMap Map.assocs ms ]
