{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

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
    ( DBLayer (..), ErrPointAlreadyExists (..) )
import Cardano.Pool.DB.Arbitrary
    ( StakePoolsFixture (..) )
import Cardano.Pool.DB.Sqlite
    ( newDBLayer )
import Cardano.Wallet.Primitive.Types
    ( BlockHeader (..), EpochNo, PoolId, SlotId (..) )
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
import Data.Ord
    ( Down (..) )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text )
import Data.Word
    ( Word64 )
import Fmt
    ( pretty )
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
    ( Property, classify, counterexample, property )
import Test.QuickCheck.Monadic
    ( assert, monadicIO, monitor, run )

import qualified Cardano.Pool.DB.MVar as MVar
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T

-- | Provide a DBLayer to a Spec that requires it. The database is initialised
-- once, and cleared with 'cleanDB' before each test.
withDB :: IO (DBLayer IO) -> SpecWith (DBLayer IO) -> Spec
withDB create = beforeAll create . beforeWith
    (\db@DBLayer{cleanDB, atomically}-> atomically $ cleanDB $> db)

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
        it "readPoolProductionCursor should return the last applied blocks"
            (property . prop_readPoolProductionCursorTipIsLast)
        it "readPoolProduction for a given epoch should always give slots \
           \from given epoch"
            (property . prop_readPoolNoEpochLeaks)
        it "readPoolProduction should never give pools with no slots"
            (property . (prop_readPoolCond noEmptyPools))
        it "readPoolProduction should never give pools with no slots \
           \after consecutive 1-slot-depth rollbacks"
            (property . (prop_readPoolCondAfterDeterministicRollbacks noEmptyPools))
        it "readPoolProduction should never give pools with no slots \
           \after rollback - arbitrary N-slot-depth rollbacks"
            (property . (prop_readPoolCondAfterRandomRollbacks noEmptyPools))
        it "readPoolProduction should give pools with descending slots"
            (property . (prop_readPoolCond descSlotsPerPool))
        it "readPoolProduction should give pools with descending slots \
           \after consecutive 1-slot-depth rollbacks"
            (property . (prop_readPoolCondAfterDeterministicRollbacks descSlotsPerPool))
        it "readPoolProduction should never give pools with no slots \
           \after rollback - arbitrary N-slot-depth rollbacks"
            (property . (prop_readPoolCondAfterRandomRollbacks descSlotsPerPool))
        it "readStakeDistribution . putStakeDistribution == pure"
            (property . prop_putStakeReadStake)
        it "readStake . putStake a1 . putStake s0 == pure a1"
            (property . prop_putStakePutStake)

{-------------------------------------------------------------------------------
                                    Properties
-------------------------------------------------------------------------------}

-- | Can read put pool production
prop_putReadPoolProduction
    :: DBLayer IO
    -> StakePoolsFixture
    -> Property
prop_putReadPoolProduction DBLayer{..} (StakePoolsFixture pairs _) =
    monadicIO (setup >>= prop)
  where
    setup = liftIO $ do
        atomically cleanDB
        db'@DBLayer{cleanDB=cleanDB',atomically=atomically'} <- MVar.newDBLayer
        atomically' cleanDB'
        pure db'
    prop
        DBLayer
            { atomically = atomically'
            , putPoolProduction = putPoolProduction'
            , readPoolProduction = readPoolProduction'
            }
        = do
        run . atomically $ forM_ pairs $ \(pool, slot) ->
            unsafeRunExceptT $ putPoolProduction slot pool
        run . atomically' $ forM_ pairs $ \(pool, slot) ->
            unsafeRunExceptT $ putPoolProduction' slot pool
        monitor $ classify (length pairs > 100) "productions > 100"
        monitor $ classify (length pairs > 1000) "productions > 1000"
        run . forM_ (uniqueEpochs pairs) $ \epoch -> do
            res' <- atomically' $ readPoolProduction' epoch
            atomically (readPoolProduction epoch) `shouldReturn` res'

-- | Cannot put pool production with already put slot
prop_putSlotTwicePoolProduction
    :: DBLayer IO
    -> StakePoolsFixture
    -> Property
prop_putSlotTwicePoolProduction DBLayer{..} (StakePoolsFixture pairs _) =
    monadicIO (setup >> prop)
  where
    setup = liftIO $ atomically cleanDB
    prop = liftIO $ do
        forM_ pairs $ \(pool, slot) -> do
            let err = ErrPointAlreadyExists slot
            atomically (runExceptT $ putPoolProduction slot pool) `shouldReturn` Right ()
            atomically (runExceptT $ putPoolProduction slot pool) `shouldReturn` Left err

-- | Rolling back wipes out pool production statistics after the rollback point.
prop_rollbackPools
    :: DBLayer IO
    -> StakePoolsFixture
    -> SlotId
    -> Property
prop_rollbackPools db@DBLayer{..} f@(StakePoolsFixture pairs _) sl =
    monadicIO prop
  where
    prop = do
        (beforeRollback, afterRollback) <- run $ do
            atomically $ forM_ pairs $ \(pool, point) ->
                runExceptT $ putPoolProduction point pool
            before <- map fst <$> allPoolProduction db f
            atomically $ rollbackTo sl
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

    showSlot s = T.unpack $ pretty s

-- | Last element of cursor is the tip
prop_readPoolProductionCursorTipIsLast
    :: DBLayer IO
    -> StakePoolsFixture
    -> Property
prop_readPoolProductionCursorTipIsLast DBLayer{..} (StakePoolsFixture pairs _) =
    monadicIO (setup >> prop)
  where
    setup = liftIO $ atomically cleanDB
    prop = do
        run $ atomically $ forM_ pairs $ \(pool, slot) ->
            unsafeRunExceptT $ putPoolProduction slot pool
        tip <- run $ atomically $ last <$> readPoolProductionCursor 2
        assert $ tip == snd (head pairs)

-- | Can read pool production only for a given epoch
prop_readPoolNoEpochLeaks
    :: DBLayer IO
    -> StakePoolsFixture
    -> Property
prop_readPoolNoEpochLeaks DBLayer{..} (StakePoolsFixture pairs _) =
    monadicIO (setup >> prop)
  where
    slotPartition = L.groupBy ((==) `on` epochNumber)
        $ L.sortOn epochNumber
        $ map (slotId . snd) pairs
    epochGroups = L.zip (uniqueEpochs pairs) slotPartition
    setup = liftIO $ atomically cleanDB
    prop = run $ do
        atomically $ forM_ pairs $ \(pool, slot) ->
            unsafeRunExceptT $ putPoolProduction slot pool
        forM_ epochGroups $ \(epoch, slots) -> do
            slots' <- (Set.fromList . map slotId . concat . Map.elems) <$>
                atomically (readPoolProduction epoch)
            slots' `shouldBe` (Set.fromList slots)

-- | Read pool production satisfies conditions after consecutive
-- 1-slot-depth rollbacks
prop_readPoolCondAfterDeterministicRollbacks
    :: (Map PoolId [BlockHeader] -> Expectation)
    -> DBLayer IO
    -> StakePoolsFixture
    -> Property
prop_readPoolCondAfterDeterministicRollbacks cond DBLayer{..} (StakePoolsFixture pairs _) =
    monadicIO (setup >> prop)
  where
    setup = liftIO $ atomically cleanDB
    slots = map (slotId . snd) pairs
    prop = run $ do
        atomically $ forM_ pairs $ \(pool, point) ->
            unsafeRunExceptT $ putPoolProduction point pool
        forM_ slots $ \slot -> do
            _ <- atomically $ rollbackTo slot
            forM_ (uniqueEpochs pairs) $ \epoch -> do
                res <- atomically $ readPoolProduction epoch
                cond res

-- | Read pool production satisfies conditions after consecutive
-- arbitrary N-slot-depth rollbacks
prop_readPoolCondAfterRandomRollbacks
    :: (Map PoolId [BlockHeader] -> Expectation)
    -> DBLayer IO
    -> StakePoolsFixture
    -> Property
prop_readPoolCondAfterRandomRollbacks cond DBLayer{..} (StakePoolsFixture pairs rSlots) =
    monadicIO (setup >> prop)
  where
    setup = liftIO $ atomically cleanDB
    prop = do
        run $ atomically $ forM_ pairs $ \(pool, slot) ->
            unsafeRunExceptT $ putPoolProduction slot pool
        run $ forM_ rSlots $ \slot -> do
            atomically $ rollbackTo slot
            forM_ (uniqueEpochs pairs) $ \epoch -> do
                res <- atomically $ readPoolProduction epoch
                cond res
        monitor $ classify (length pairs <= 10) "number of slots <= 10"
        monitor $ classify (length pairs > 10) "number of slots > 10"

-- | Read pool production satisfies condition
prop_readPoolCond
    :: (Map PoolId [BlockHeader] -> Expectation)
    -> DBLayer IO
    -> StakePoolsFixture
    -> Property
prop_readPoolCond cond DBLayer{..} (StakePoolsFixture pairs _) =
    monadicIO (setup >> prop)
  where
    setup = liftIO $ atomically cleanDB
    prop = liftIO $ do
        atomically $ forM_ pairs $ \(pool, slot) ->
            unsafeRunExceptT $ putPoolProduction slot pool
        forM_ (uniqueEpochs pairs) $ \epoch -> do
            res <- atomically $ readPoolProduction epoch
            cond res

-- | read . put == pure
prop_putStakeReadStake
    :: DBLayer IO
    -> EpochNo
    -> [(PoolId, Quantity "lovelace" Word64)]
    -> Property
prop_putStakeReadStake DBLayer{..} epoch distribution =
    monadicIO (setup >> prop)
  where
    setup = run $ atomically cleanDB
    prop = do
        run $ atomically $ putStakeDistribution epoch distribution
        distribution' <- run $ atomically $ readStakeDistribution epoch
        monitor $ counterexample $ unlines
            [ "Read from DB: " <> show distribution' ]
        monitor $ classify (null distribution) "Empty distributions"
        assert (L.sort distribution' == L.sort distribution)

-- | read $ put B $ put A == B
prop_putStakePutStake
    :: DBLayer IO
    -> EpochNo
    -> [(PoolId, Quantity "lovelace" Word64)]
    -> [(PoolId, Quantity "lovelace" Word64)]
    -> Property
prop_putStakePutStake DBLayer {..} epoch a b =
    monadicIO (setup >> prop)
  where
    setup = run $ atomically cleanDB
    prop = do
        run . atomically $ putStakeDistribution epoch a
        run . atomically $ putStakeDistribution epoch b
        res <- run . atomically $ readStakeDistribution epoch
        monitor $ counterexample $ unlines
            [ "Read from DB: " <> show res ]
        monitor $ classify (null a) "a is empty"
        monitor $ classify (null b) "b is empty"
        monitor $ classify (null a && null b) "a & b are empty"
        assert (L.sort res == L.sort b)

descSlotsPerPool :: Map PoolId [BlockHeader] -> Expectation
descSlotsPerPool pools = do
    let checkIfDesc slots =
            L.sortOn Down slots == slots
    let pools' = Map.filter checkIfDesc pools
    pools' `shouldBe` pools

noEmptyPools :: Map PoolId [BlockHeader] -> Expectation
noEmptyPools pools = do
    let pools' = Map.filter (not . null) pools
    pools' `shouldBe` pools

uniqueEpochs :: [(PoolId, BlockHeader)] -> [EpochNo]
uniqueEpochs = nubOrd . map (epochNumber . slotId . snd)

-- | Concatenate stake pool production for all epochs in the test fixture.
allPoolProduction :: DBLayer IO -> StakePoolsFixture -> IO [(SlotId, PoolId)]
allPoolProduction DBLayer{..} (StakePoolsFixture pairs _) = atomically $
    rearrange <$> mapM readPoolProduction (uniqueEpochs pairs)
  where
    rearrange ms = concat
        [ [ (slotId h, p) | h <- hs ] | (p, hs) <- concatMap Map.assocs ms ]
