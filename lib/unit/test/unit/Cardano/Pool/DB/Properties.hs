{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Pool.DB.Properties (properties) where

import Cardano.Pool.DB
    ( DBLayer (..)
    , ErrPointAlreadyExists (..)
    , determinePoolLifeCycleStatus
    , readPoolLifeCycleStatus
    )
import Cardano.Pool.DB.Arbitrary
    ( ManyPoolCertificates (..)
    , MultiPoolCertificateSequence (..)
    , SinglePoolCertificateSequence (..)
    , StakePoolsFixture (..)
    , genStakePoolMetadata
    , getMultiPoolCertificateSequence
    , isValidSinglePoolCertificateSequence
    )
import Cardano.Pool.Metadata.Types
    ( StakePoolMetadata (..)
    )
import Cardano.Pool.Types
    ( PoolId (..)
    , StakePoolTicker (..)
    )
import Cardano.Wallet.DummyTarget.Primitive.Types
    ( dummyTimeInterpreter
    )
import Cardano.Wallet.Gen
    ( genBlockHeader
    , genSlotNo
    )
import Cardano.Wallet.Primitive.Slotting
    ( epochOf
    , interpretQuery
    )
import Cardano.Wallet.Primitive.Types
    ( BlockHeader (..)
    , CertificatePublicationTime (..)
    , EpochNo (..)
    , PoolCertificate (..)
    , PoolLifeCycleStatus (..)
    , PoolRegistrationCertificate (..)
    , PoolRetirementCertificate
    , Settings
    , SlotNo (..)
    , defaultSettings
    , getPoolCertificatePoolId
    , getPoolRetirementCertificate
    )
import Cardano.Wallet.Unsafe
    ( unsafeRunExceptT
    )
import Control.Arrow
    ( second
    )
import Control.Monad
    ( forM
    , forM_
    , replicateM
    , void
    )
import Control.Monad.Trans.Except
    ( runExceptT
    )
import Data.Bifunctor
    ( bimap
    )
import Data.Function
    ( on
    , (&)
    )
import Data.Functor.Identity
    ( runIdentity
    )
import Data.Generics.Internal.VL.Lens
    ( set
    , view
    )
import Data.List.Extra
    ( nubOrd
    , nubSortOn
    )
import Data.Map.Strict
    ( Map
    )
import Data.Maybe
    ( catMaybes
    , fromMaybe
    , isJust
    , isNothing
    , listToMaybe
    , mapMaybe
    )
import Data.Ord
    ( Down (..)
    )
import Data.Quantity
    ( Quantity (..)
    )
import Data.Text.Class
    ( toText
    )
import Data.Time.Clock.POSIX
    ( POSIXTime
    )
import Data.Word
    ( Word64
    )
import Fmt
    ( pretty
    )
import Test.Hspec
    ( Expectation
    , Spec
    , anyException
    , describe
    , it
    , shouldBe
    , shouldReturn
    , shouldThrow
    )
import Test.QuickCheck
    ( Arbitrary
    , Confidence (..)
    , NonNegative (..)
    , Positive (..)
    , Property
    , arbitrary
    , checkCoverageWith
    , classify
    , counterexample
    , cover
    , property
    , shrink
    , withMaxSuccess
    , (==>)
    )
import Test.QuickCheck.Monadic
    ( PropertyM
    , assert
    , monadicIO
    , monitor
    , pick
    , run
    )
import UnliftIO.Exception
    ( evaluate
    )
import Prelude

import qualified Cardano.Pool.DB.MVar as MVar
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T

properties :: (forall a. (DBLayer IO -> IO a) -> IO a) -> Spec
properties withDB = do
    describe "Stake Pool properties" $ do
        it
            "putPoolProduction . readPoolProduction yields expected results"
            (property (prop_putReadPoolProduction withDB))
        it
            "putPoolProduction with already put slot yields error"
            (property (prop_putSlotTwicePoolProduction withDB))
        it
            "Rollback of stake pool production"
            (property (prop_rollbackPools withDB))
        it
            "readPoolProductionCursor should return the last applied blocks"
            (property (prop_readPoolProductionCursorTipIsLast withDB))
        it
            "readPoolProduction for a given epoch should always give slots \
            \from given epoch"
            (property (prop_readPoolNoEpochLeaks withDB))
        it
            "readPoolProduction should never give pools with no slots"
            (property (prop_readPoolCond withDB noEmptyPools))
        it
            "readPoolProduction should never give pools with no slots \
            \after consecutive 1-slot-depth rollbacks"
            ( property
                (prop_readPoolCondAfterDeterministicRollbacks withDB noEmptyPools)
            )
        it
            "readPoolProduction should never give pools with no slots \
            \after rollback - arbitrary N-slot-depth rollbacks"
            (property (prop_readPoolCondAfterRandomRollbacks withDB noEmptyPools))
        it
            "readPoolProduction should give pools with descending slots"
            (property (prop_readPoolCond withDB descSlotsPerPool))
        it
            "readPoolProduction should give pools with descending slots \
            \after consecutive 1-slot-depth rollbacks"
            ( property
                ( prop_readPoolCondAfterDeterministicRollbacks
                    withDB
                    descSlotsPerPool
                )
            )
        it
            "readPoolProduction should never give pools with no slots \
            \after rollback - arbitrary N-slot-depth rollbacks"
            ( property
                (prop_readPoolCondAfterRandomRollbacks withDB descSlotsPerPool)
            )
        it
            "readStakeDistribution . putStakeDistribution == pure"
            (property (prop_putStakeReadStake withDB))
        it
            "putPoolRegistration then readPoolRegistration yields expected result"
            (property (prop_poolRegistration withDB))
        it
            "putPoolRetirement then readPoolRetirement yields expected result"
            (property (prop_poolRetirement withDB))
        it
            "prop_multiple_putPoolRegistration_single_readPoolRegistration"
            ( property
                ( prop_multiple_putPoolRegistration_single_readPoolRegistration
                    withDB
                )
            )
        it
            "prop_multiple_putPoolRetirement_single_readPoolRetirement"
            ( property
                ( prop_multiple_putPoolRetirement_single_readPoolRetirement
                    withDB
                )
            )
        it
            "readPoolLifeCycleStatus respects certificate publication order"
            (property (prop_readPoolLifeCycleStatus withDB))
        it
            "rollback of PoolRegistration"
            (property (prop_rollbackRegistration withDB))
        it
            "rollback of PoolRetirement"
            (property (prop_rollbackRetirement withDB))
        it
            "removePools"
            (property (prop_removePools withDB))
        it
            "readStake . putStake a1 . putStake s0 == pure a1"
            (property (prop_putStakePutStake withDB))
        it
            "readSystemSeed is idempotent"
            (property (prop_readSystemSeedIdempotent withDB))
        it
            "putPoolRegistration . listRegisteredPools yield pools"
            (property (prop_listRegisteredPools withDB))
        it
            "prop_listRetiredPools_multiplePools_multipleCerts"
            ( property
                (prop_listRetiredPools_multiplePools_multipleCerts withDB)
            )
        it
            "prop_listPoolLifeCycleData_multiplePools_multipleCerts"
            ( property
                (prop_listPoolLifeCycleData_multiplePools_multipleCerts withDB)
            )
        it
            "putPoolProduction* . readTotalProduction matches expectations"
            (property (prop_readTotalProduction withDB))
        it
            "unfetchedPoolMetadataRefs"
            (property (prop_unfetchedPoolMetadataRefs withDB))
        it
            "unfetchedPoolMetadataRefsIgnoring"
            (property (prop_unfetchedPoolMetadataRefsIgnoring withDB))
        it
            "prop_determinePoolLifeCycleStatus_orderCorrect"
            (property prop_determinePoolLifeCycleStatus_orderCorrect)
        it
            "prop_determinePoolLifeCycleStatus_neverRegistered"
            (property prop_determinePoolLifeCycleStatus_neverRegistered)
        it
            "prop_determinePoolLifeCycleStatus_differentPools"
            (property prop_determinePoolLifeCycleStatus_differentPools)
        it
            "SinglePoolCertificateSequence coverage is adequate"
            (property prop_SinglePoolCertificateSequence_coverage)
        it
            "MultiPoolCertificateSequence coverage is adequate"
            (property prop_MultiPoolCertificateSequence_coverage)
        it
            "forM putHeader headers >> listHeaders == headers"
            (property (prop_putHeaderListHeader withDB))
        it
            "modSettings . readSettings == id"
            (property (prop_modSettingsReadSettings withDB))
        it
            "putLastMetadataGC . readLastMetadataGC == id"
            (property (prop_putLastMetadataGCReadLastMetadataGC withDB))
        it
            "putDelistedPools >> readDelistedPools shows the pool as delisted"
            (property (prop_putDelistedPools withDB))
        it
            "clearing metadata also clears delisted pools"
            (property (prop_removePoolMetadataDelistedPools withDB))

okayConfidence :: Confidence
okayConfidence = Confidence{certainty = 10 ^ (6 :: Int), tolerance = 0.9}

{-------------------------------------------------------------------------------
                                    Properties
-------------------------------------------------------------------------------}

-- | Like 'assert', but allow giving a label / title before running a assertion
assertWith :: String -> Bool -> PropertyM IO ()
assertWith lbl condition = do
    let flag = if condition then "✓" else "✗"
    monitor (counterexample $ lbl <> " " <> flag)
    assert condition

-- | Can read put pool production
prop_putReadPoolProduction
    :: (forall a. (DBLayer IO -> IO a) -> IO a)
    -> StakePoolsFixture
    -> Property
prop_putReadPoolProduction withDB (StakePoolsFixture pairs _) =
    monadicIO $ do
        run $ withDB $ \DBLayer{..} -> do
            DBLayer
                { atomically = atomically'
                , putPoolProduction = putPoolProduction'
                , readPoolProduction = readPoolProduction'
                } <-
                MVar.newDBLayer ti
            atomically $ forM_ pairs $ \(pool, slot) ->
                unsafeRunExceptT $ putPoolProduction slot pool
            atomically' $ forM_ pairs $ \(pool, slot) ->
                unsafeRunExceptT $ putPoolProduction' slot pool
            forM_ (uniqueEpochs pairs) $ \epoch -> do
                res' <- atomically' $ readPoolProduction' epoch
                atomically (readPoolProduction epoch) `shouldReturn` res'
        monitor $ classify (length pairs > 100) "productions > 100"
        monitor $ classify (length pairs > 1000) "productions > 1000"
  where
    ti = dummyTimeInterpreter

prop_readTotalProduction
    :: (forall a. (DBLayer IO -> IO a) -> IO a)
    -> StakePoolsFixture
    -> Property
prop_readTotalProduction withDB (StakePoolsFixture pairs _) =
    monadicIO $ do
        production <- run $ withDB $ \DBLayer{..} -> do
            atomically $ forM_ pairs $ \(pool, slot) ->
                unsafeRunExceptT $ putPoolProduction slot pool
            atomically readTotalProduction
        monitor $ counterexample ("from database: " <> show production)
        let production' =
                Map.map Quantity
                    $ Map.fromListWith (+)
                    $ second (const 1) <$> pairs
        assert (production == production')

-- | Cannot put pool production with already put slot
prop_putSlotTwicePoolProduction
    :: (forall a. (DBLayer IO -> IO a) -> IO a)
    -> StakePoolsFixture
    -> Property
prop_putSlotTwicePoolProduction withDB (StakePoolsFixture pairs _) =
    monadicIO $ run $ withDB $ \DBLayer{..} ->
        forM_ pairs $ \(pool, slot) -> do
            let err = ErrPointAlreadyExists slot
            atomically (runExceptT $ putPoolProduction slot pool)
                `shouldReturn` Right ()
            atomically (runExceptT $ putPoolProduction slot pool)
                `shouldReturn` Left err

-- | Rolling back wipes out pool production statistics after the rollback point.
prop_rollbackPools
    :: (forall a. (DBLayer IO -> IO a) -> IO a)
    -> StakePoolsFixture
    -> SlotNo
    -> Property
prop_rollbackPools withDB f@(StakePoolsFixture pairs _) sl =
    monadicIO $ do
        (beforeRollback, afterRollback) <- run $ withDB $ \db@DBLayer{..} -> do
            atomically $ forM_ pairs $ \(pool, point) ->
                runExceptT $ putPoolProduction point pool
            before <- map fst <$> allPoolProduction db f
            atomically $ rollbackTo sl
            after <- map fst <$> allPoolProduction db f
            pure (before, after)

        monitor
            $ counterexample
            $ unlines
                [ "Rollback point:    " <> showSlot sl
                , "Production before: " <> unwords (map showSlot beforeRollback)
                , "Production after:  " <> unwords (map showSlot afterRollback)
                ]
        monitor
            $ classify (any (> sl) beforeRollback) "something to roll back"
        monitor $ classify (all (<= sl) beforeRollback) "nothing to roll back"

        assert $ all (<= sl) afterRollback
  where
    showSlot s = T.unpack $ pretty s

-- | Last element of cursor is the tip
prop_readPoolProductionCursorTipIsLast
    :: (forall a. (DBLayer IO -> IO a) -> IO a)
    -> StakePoolsFixture
    -> Property
prop_readPoolProductionCursorTipIsLast withDB (StakePoolsFixture pairs _) =
    monadicIO $ do
        tip <- run $ withDB $ \DBLayer{..} -> do
            atomically $ forM_ pairs $ \(pool, slot) ->
                unsafeRunExceptT $ putPoolProduction slot pool
            atomically $ last <$> readPoolProductionCursor 2
        case pairs of
            ((_, firstSlot) : _) -> assert $ tip == firstSlot
            [] -> error "expected non-empty pairs"

-- | Can read pool production only for a given epoch
prop_readPoolNoEpochLeaks
    :: (forall a. (DBLayer IO -> IO a) -> IO a)
    -> StakePoolsFixture
    -> Property
prop_readPoolNoEpochLeaks withDB (StakePoolsFixture pairs _) =
    withMaxSuccess 1000 $ monadicIO $ run $ withDB $ \DBLayer{..} -> do
        atomically $ forM_ pairs $ \(pool, slot) ->
            unsafeRunExceptT $ putPoolProduction slot pool
        forM_ epochGroups $ \(epoch, slots) -> do
            slots' <-
                Set.fromList . map (view #slotNo) . concat . Map.elems
                    <$> atomically (readPoolProduction epoch)
            slots' `shouldBe` Set.fromList slots
  where
    slotPartition =
        L.groupBy ((==) `on` epochOf')
            $ L.sortOn epochOf'
            $ map (view #slotNo . snd) pairs
    epochGroups =
        map
            ( \sls -> case sls of (s : _) -> (epochOf' s, sls); [] -> error "empty group"
            )
            slotPartition
    epochOf' :: SlotNo -> EpochNo
    epochOf' = runIdentity . interpretQuery ti . epochOf
    ti = dummyTimeInterpreter

-- | Read pool production satisfies conditions after consecutive
-- 1-slot-depth rollbacks
prop_readPoolCondAfterDeterministicRollbacks
    :: (forall a. (DBLayer IO -> IO a) -> IO a)
    -> (Map PoolId [BlockHeader] -> Expectation)
    -> StakePoolsFixture
    -> Property
prop_readPoolCondAfterDeterministicRollbacks
    withDB
    cond
    (StakePoolsFixture pairs _) =
        monadicIO $ run $ withDB $ \DBLayer{..} -> do
            atomically $ forM_ pairs $ \(pool, point) ->
                unsafeRunExceptT $ putPoolProduction point pool
            forM_ slots $ \slot -> do
                _ <- atomically $ rollbackTo slot
                forM_ (uniqueEpochs pairs) $ \epoch -> do
                    res <- atomically $ readPoolProduction epoch
                    cond res
      where
        slots = map (view #slotNo . snd) pairs

-- | Read pool production satisfies conditions after consecutive
-- arbitrary N-slot-depth rollbacks
prop_readPoolCondAfterRandomRollbacks
    :: (forall a. (DBLayer IO -> IO a) -> IO a)
    -> (Map PoolId [BlockHeader] -> Expectation)
    -> StakePoolsFixture
    -> Property
prop_readPoolCondAfterRandomRollbacks
    withDB
    cond
    (StakePoolsFixture pairs rSlots) =
        monadicIO $ do
            run $ withDB $ \DBLayer{..} -> do
                atomically $ forM_ pairs $ \(pool, slot) ->
                    unsafeRunExceptT $ putPoolProduction slot pool
                forM_ rSlots $ \slot -> do
                    atomically $ rollbackTo slot
                    forM_ (uniqueEpochs pairs) $ \epoch -> do
                        res <- atomically $ readPoolProduction epoch
                        cond res
            monitor $ classify (length pairs <= 10) "number of slots <= 10"
            monitor $ classify (length pairs > 10) "number of slots > 10"

-- | Read pool production satisfies condition
prop_readPoolCond
    :: (forall a. (DBLayer IO -> IO a) -> IO a)
    -> (Map PoolId [BlockHeader] -> Expectation)
    -> StakePoolsFixture
    -> Property
prop_readPoolCond withDB cond (StakePoolsFixture pairs _) =
    monadicIO $ run $ withDB $ \DBLayer{..} -> do
        atomically $ forM_ pairs $ \(pool, slot) ->
            unsafeRunExceptT $ putPoolProduction slot pool
        forM_ (uniqueEpochs pairs) $ \epoch -> do
            res <- atomically $ readPoolProduction epoch
            cond res

-- | read . put == pure
prop_putStakeReadStake
    :: (forall a. (DBLayer IO -> IO a) -> IO a)
    -> EpochNo
    -> [(PoolId, Quantity "lovelace" Word64)]
    -> Property
prop_putStakeReadStake withDB epoch distribution =
    monadicIO $ do
        distribution' <- run $ withDB $ \DBLayer{..} -> do
            atomically $ putStakeDistribution epoch distribution
            atomically $ readStakeDistribution epoch
        monitor
            $ counterexample
            $ unlines
                ["Read from DB: " <> show distribution']
        monitor $ classify (null distribution) "Empty distributions"
        assert (L.sort distribution' == L.sort distribution)

-- | read $ put B $ put A == B
prop_putStakePutStake
    :: (forall a. (DBLayer IO -> IO a) -> IO a)
    -> EpochNo
    -> [(PoolId, Quantity "lovelace" Word64)]
    -> [(PoolId, Quantity "lovelace" Word64)]
    -> Property
prop_putStakePutStake withDB epoch a b =
    monadicIO $ do
        res <- run $ withDB $ \DBLayer{..} -> do
            atomically $ putStakeDistribution epoch a
            atomically $ putStakeDistribution epoch b
            atomically $ readStakeDistribution epoch
        monitor
            $ counterexample
            $ unlines
                ["Read from DB: " <> show res]
        monitor $ classify (null a) "a is empty"
        monitor $ classify (null b) "b is empty"
        monitor $ classify (null a && null b) "a & b are empty"
        assert (L.sort res == L.sort b)

-- | Heavily relies upon the fact that generated values of 'PoolId' are unique.
prop_poolRegistration
    :: (forall a. (DBLayer IO -> IO a) -> IO a)
    -> ManyPoolCertificates PoolRegistrationCertificate
    -> Property
prop_poolRegistration withDB (ManyPoolCertificates entries) =
    monadicIO $ do
        (entriesOut, poolsMarkedToRetire) <- run $ withDB $ \DBLayer{..} -> do
            atomically $ mapM_ (uncurry putPoolRegistration) entriesIn
            entriesOut <-
                atomically
                    $ L.sort . catMaybes
                        <$> mapM (readPoolRegistration . view #poolId . snd) entries
            poolsMarkedToRetire <-
                atomically $ listRetiredPools $ EpochNo maxBound
            pure (entriesOut, poolsMarkedToRetire)
        monitor
            $ counterexample
            $ unlines
                [ "Written into DB: "
                , show entriesIn
                , "Read from DB: "
                , show entriesOut
                , "All pools that are marked to retire: "
                , unlines (("\n" <>) . show <$> poolsMarkedToRetire)
                ]
        assertWith "entriesIn == entriesOut"
            $ entriesIn == entriesOut
        assertWith "no pools are marked to retire"
            $ null poolsMarkedToRetire
  where
    entriesIn = L.sort entries

-- | Heavily relies upon the fact that generated values of 'PoolId' are unique.
prop_poolRetirement
    :: (forall a. (DBLayer IO -> IO a) -> IO a)
    -> ManyPoolCertificates PoolRetirementCertificate
    -> Property
prop_poolRetirement withDB (ManyPoolCertificates entries) =
    monadicIO $ do
        (entriesOut, poolsMarkedToRetire) <- run $ withDB $ \DBLayer{..} -> do
            atomically $ mapM_ (uncurry putPoolRetirement) entriesIn
            entriesOut <-
                atomically
                    $ L.sort . catMaybes
                        <$> mapM (readPoolRetirement . view #poolId . snd) entries
            poolsMarkedToRetire <-
                atomically $ listRetiredPools $ EpochNo maxBound
            pure (entriesOut, poolsMarkedToRetire)
        monitor
            $ counterexample
            $ unlines
                [ "Written into DB: "
                , show entriesIn
                , "Read from DB: "
                , show entriesOut
                , "All pools that are marked to retire: "
                , unlines (("\n" <>) . show <$> poolsMarkedToRetire)
                ]
        assertWith "entriesIn == entriesOut"
            $ entriesIn == entriesOut
        assertWith "all pools are marked to retire"
            $ (==)
                (Set.fromList $ snd <$> entriesIn)
                (Set.fromList poolsMarkedToRetire)
  where
    entriesIn = L.sort entries

-- For the same pool, write /multiple/ pool registration certificates to the
-- database and then read back the current registration certificate, verifying
-- that the certificate returned is the last one to have been written.
--
prop_multiple_putPoolRegistration_single_readPoolRegistration
    :: (forall a. (DBLayer IO -> IO a) -> IO a)
    -> PoolId
    -> ManyPoolCertificates PoolRegistrationCertificate
    -> Property
prop_multiple_putPoolRegistration_single_readPoolRegistration
    withDB
    sharedPoolId
    (ManyPoolCertificates entries) =
        monadicIO $ do
            (mRetrievedCertificatePublication, poolsMarkedToRetire) <-
                run $ withDB $ \DBLayer{..} -> do
                    atomically
                        $ mapM_
                            (uncurry putPoolRegistration)
                            certificatePublications
                    retrieved <- atomically $ readPoolRegistration sharedPoolId
                    retired <- atomically $ listRetiredPools $ EpochNo maxBound
                    pure (retrieved, retired)
            monitor
                $ counterexample
                $ unlines
                    [ "\nExpected certificate publication: "
                    , show mExpectedCertificatePublication
                    , "\nRetrieved certificate publication: "
                    , show mRetrievedCertificatePublication
                    , "\nNumber of certificate publications: "
                    , show (length certificatePublications)
                    , "\nAll certificate publications: "
                    , unlines (("\n" <>) . show <$> certificatePublications)
                    ]
            assertWith "retrieved certificate matches expectations"
                $ (==)
                    mRetrievedCertificatePublication
                    mExpectedCertificatePublication
            assertWith "pool is not marked to retire"
                $ null poolsMarkedToRetire
      where
        certificatePublications =
            L.nubBy (\(a, _) (b, _) -> view #slotNo a == view #slotNo b)
                $ second (set #poolId sharedPoolId) <$> entries

        mExpectedCertificatePublication =
            certificatePublications
                & L.sortOn (Down . view #slotNo . fst)
                & listToMaybe

-- For the same pool, write /multiple/ pool retirement certificates to the
-- database and then read back the current retirement certificate, verifying
-- that the certificate returned is the last one to have been written.
--
prop_multiple_putPoolRetirement_single_readPoolRetirement
    :: (forall a. (DBLayer IO -> IO a) -> IO a)
    -> PoolId
    -> ManyPoolCertificates PoolRetirementCertificate
    -> Property
prop_multiple_putPoolRetirement_single_readPoolRetirement
    withDB
    sharedPoolId
    (ManyPoolCertificates entries) =
        monadicIO $ do
            (mRetrievedCertificatePublication, poolsMarkedToRetire) <-
                run $ withDB $ \DBLayer{..} -> do
                    atomically
                        $ mapM_
                            (uncurry putPoolRetirement)
                            certificatePublications
                    retrieved <- atomically $ readPoolRetirement sharedPoolId
                    retired <- atomically $ listRetiredPools $ EpochNo maxBound
                    pure (retrieved, retired)
            monitor
                $ counterexample
                $ unlines
                    [ "\nExpected certificate publication: "
                    , show mExpectedCertificatePublication
                    , "\nRetrieved certificate publication: "
                    , show mRetrievedCertificatePublication
                    , "\nNumber of certificate publications: "
                    , show (length certificatePublications)
                    , "\nAll certificate publications: "
                    , unlines (("\n" <>) . show <$> certificatePublications)
                    ]
            assertWith "retrieved certificate matches expectations"
                $ (==)
                    mRetrievedCertificatePublication
                    mExpectedCertificatePublication
            assertWith "pool is marked to retire at the correct epoch"
                $ case mRetrievedCertificatePublication of
                    Nothing ->
                        null poolsMarkedToRetire
                    Just (_publicationTime, retirementCert) ->
                        poolsMarkedToRetire == [retirementCert]
      where
        certificatePublications =
            L.nubBy (\(a, _) (b, _) -> view #slotNo a == view #slotNo b)
                $ second (set #poolId sharedPoolId) <$> entries

        mExpectedCertificatePublication =
            certificatePublications
                & L.sortOn (Down . view #slotNo . fst)
                & listToMaybe

-- After writing an /arbitrary/ sequence of interleaved registration and
-- retirement certificates for the same pool to the database, verify that
-- reading the current lifecycle status returns a result that reflects
-- the correct order of precedence for these certificates.
--
-- Note that this property /assumes/ the correctness of the pure function
-- 'determinePoolLifeCycleStatus', which is tested /elsewhere/ with
-- the @prop_determinePoolLifeCycleStatus@ series of properties.
--
prop_readPoolLifeCycleStatus
    :: (forall a. (DBLayer IO -> IO a) -> IO a)
    -> SinglePoolCertificateSequence
    -> Property
prop_readPoolLifeCycleStatus
    withDB
    (SinglePoolCertificateSequence sharedPoolId certificates) =
        monadicIO $ do
            (actualStatus, poolsMarkedToRetire) <-
                run $ withDB $ \db@DBLayer{..} -> do
                    mapM_
                        (uncurry $ putPoolCertificate db)
                        certificatePublications
                    actual <- atomically $ readPoolLifeCycleStatus sharedPoolId
                    retired <- atomically $ listRetiredPools $ EpochNo maxBound
                    pure (actual, retired)
            monitor
                $ counterexample
                $ unlines
                    [ "\nFinal registration: "
                    , show mFinalRegistration
                    , "\nFinal retirement: "
                    , show mFinalRetirement
                    , "\nExpected status: "
                    , show expectedStatus
                    , "\nActual status: "
                    , show actualStatus
                    , "\nNumber of certificate publications: "
                    , show (length certificatePublications)
                    , "\nAll certificate publications: "
                    , unlines (("\n" <>) . show <$> certificatePublications)
                    , "\nAll pools that are marked to retire: "
                    , unlines (("\n" <>) . show <$> poolsMarkedToRetire)
                    ]
            assertWith
                "actualStatus == expectedStatus"
                (actualStatus == expectedStatus)
            assertWith "pool is marked to retire only when appropriate"
                $ case actualStatus of
                    PoolNotRegistered ->
                        null poolsMarkedToRetire
                    PoolRegistered _regCert ->
                        null poolsMarkedToRetire
                    PoolRegisteredAndRetired _regCert retCert ->
                        poolsMarkedToRetire == [retCert]
      where
        expectedStatus =
            determinePoolLifeCycleStatus
                mFinalRegistration
                mFinalRetirement

        mFinalRegistration = lookupFinalCertificateMatching $ \case
            Registration c -> Just c
            _ -> Nothing

        mFinalRetirement = lookupFinalCertificateMatching $ \case
            Retirement c -> Just c
            _ -> Nothing

        lookupFinalCertificateMatching
            :: (PoolCertificate -> Maybe certificate)
            -> Maybe (CertificatePublicationTime, certificate)
        lookupFinalCertificateMatching match =
            reverse (mapMaybe (traverse match) certificatePublications)
                & listToMaybe

        certificatePublications
            :: [(CertificatePublicationTime, PoolCertificate)]
        certificatePublications = publicationTimes `zip` certificates

        publicationTimes =
            [ CertificatePublicationTime (SlotNo sn) ii
            | sn <- [0 .. 3]
            , ii <- [0 .. 3]
            ]

prop_rollbackRegistration
    :: (forall a. (DBLayer IO -> IO a) -> IO a)
    -> SlotNo
    -> ManyPoolCertificates PoolRegistrationCertificate
    -> Property
prop_rollbackRegistration withDB rollbackPoint (ManyPoolCertificates entries) =
    monadicIO $ do
        pools <- run $ withDB $ \DBLayer{..} -> do
            atomically $ mapM_ (uncurry putPoolRegistration) entries
            atomically $ rollbackTo rollbackPoint
            atomically
                $ L.sort . fmap snd . catMaybes
                    <$> mapM (readPoolRegistration . view #poolId . snd) entries
        monitor $ classify (length pools < length entries) "rolled back some"
        monitor $ classify ownerHasManyPools "owner has many pools"
        monitor
            $ counterexample
            $ unlines
                [ "Read from DB:   " <> show pools
                ]
        assert (all beforeRollback pools)
  where
    beforeRollback pool = do
        case L.find (on (==) (view #poolId) pool . snd) entries of
            Nothing ->
                error "unknown pool?"
            Just (CertificatePublicationTime sl _, pool') ->
                (sl <= rollbackPoint) && (pool == pool')

    ownerHasManyPools =
        let owners = concatMap (poolOwners . snd) entries
        in  L.length owners > L.length (L.nub owners)

-- Verify that retirement certificates are correctly rolled back.
--
prop_rollbackRetirement
    :: (forall a. (DBLayer IO -> IO a) -> IO a)
    -> [PoolRetirementCertificate]
    -> Property
prop_rollbackRetirement withDB certificates =
    checkCoverageWith okayConfidence
        $ cover
            15
            (rollbackPoint == SlotNo 0)
            "rollbackPoint = slotMinBound"
        $ cover
            35
            (rollbackPoint > SlotNo 0)
            "rollbackPoint > slotMinBound"
        $ cover
            2
            (null expectedPublications)
            "length expectedPublications = 0"
        $ cover
            50
            (not (null expectedPublications))
            "length expectedPublications > 0"
        $ cover
            40
            ( (&&)
                (not (null expectedPublications))
                (length expectedPublications < length allPublications)
            )
            "0 < length expectedPublications < length allPublications"
        $ monadicIO
        $ do
            (retrievedPublications, poolsMarkedToRetire) <-
                run $ withDB $ \DBLayer{..} -> do
                    atomically
                        $ mapM_ (uncurry putPoolRetirement) allPublications
                    atomically $ rollbackTo rollbackPoint
                    retrieved <-
                        catMaybes
                            <$> atomically (mapM readPoolRetirement poolIds)
                    retired <- atomically $ listRetiredPools $ EpochNo maxBound
                    pure (retrieved, retired)
            monitor
                $ counterexample
                $ unlines
                    [ "\nRollback point: "
                    , show rollbackPoint
                    , "\nAll certificate publications: "
                    , unlines (("\n" <>) . show <$> allPublications)
                    , "\nExpected certificate publications: "
                    , unlines (("\n" <>) . show <$> expectedPublications)
                    , "\nRetrieved certificate publications: "
                    , unlines (("\n" <>) . show <$> retrievedPublications)
                    , "All pools that are marked to retire: "
                    , unlines (("\n" <>) . show <$> poolsMarkedToRetire)
                    ]
            assertWith "retrieved publications match expectations"
                $ (==)
                    retrievedPublications
                    expectedPublications
            assertWith "only the correct retirements are listed"
                $ (==)
                    (Set.fromList $ snd <$> expectedPublications)
                    (Set.fromList poolsMarkedToRetire)
  where
    poolIds :: [PoolId]
    poolIds = view #poolId <$> certificates

    rollbackPoint :: SlotNo
    rollbackPoint =
        -- Pick a slot that approximately corresponds to the midpoint of the
        -- certificate publication list.
        testCertificatePublicationTimes
            & drop (length certificates `div` 2)
            & fmap (view #slotNo)
            & listToMaybe
            & fromMaybe (SlotNo 0)

    allPublications
        :: [(CertificatePublicationTime, PoolRetirementCertificate)]
    allPublications = testCertificatePublicationTimes `zip` certificates

    expectedPublications
        :: [(CertificatePublicationTime, PoolRetirementCertificate)]
    expectedPublications =
        filter
            ( \(CertificatePublicationTime slotId _, _) ->
                slotId <= rollbackPoint
            )
            allPublications

-- When we remove pools, check that:
--
-- 1. We only remove data relating to the specified pools.
-- 2. We do not remove data relating to other pools.
--
-- Strictly speaking, when removing pools, we should also be able to remove any
-- pool metadata that becomes unreachable as a result.
--
-- However, this is tricky to get right, since it's possible for more than one
-- pool to share the same metadata hash.
--
-- For example, if two pools p and q issue registration certificates that share
-- the same metadata hash h, and then we garbage collect pool p, we have to be
-- careful to not remove the metadata corresponding to h in order to preserve
-- the metadata for pool q.
--
-- One possible solution would be to remove metadata entries only when we can
-- show that they are no longer reachable: when metadata entries are no longer
-- referenced by any pool.
--
-- However, for the moment, we adopt the principle of never removing metadata.
-- We can revise this decision in future if the metadata table grows too large.
--
prop_removePools
    :: (forall a. (DBLayer IO -> IO a) -> IO a)
    -> ManyPoolCertificates PoolCertificate
    -> Property
prop_removePools
    withDB
    (ManyPoolCertificates entries) =
        checkCoverageWith okayConfidence
            $ cover
                50
                (notNull poolsToRemove && notNull poolsToRetain)
                "remove some pools and retain some pools"
            $ cover
                2
                (null poolsToRemove)
                "remove no pools"
            $ cover
                3
                (null poolsToRetain)
                "retain no pools"
            $ cover
                50
                (notNull metadataToRetain)
                "retain some metadata"
            $ cover
                6
                (null metadataToRetain)
                "retain no metadata"
            $ monadicIO
            $ do
                ( poolIdsWithRegCertsAtStart
                    , poolIdsWithRetCertsAtStart
                    , poolMetadataAtStart
                    , poolIdsWithRegCertsAtEnd
                    , poolIdsWithRetCertsAtEnd
                    , poolMetadataAtEnd
                    ) <- run $ withDB $ \DBLayer{..} -> do
                    let poolIdsWithRegCerts =
                            fmap
                                ( Set.fromList
                                    . fmap (view #poolId . snd)
                                    . catMaybes
                                )
                                <$> atomically
                                $ mapM readPoolRegistration
                                $ Set.toList pools
                    let poolIdsWithRetCerts =
                            fmap
                                ( Set.fromList
                                    . fmap (view #poolId . snd)
                                    . catMaybes
                                )
                                <$> atomically
                                $ mapM readPoolRetirement
                                $ Set.toList pools
                    atomically $ forM_ certificatePublications $ \case
                        (publicationTime, Registration cert) -> do
                            putPoolRegistration publicationTime cert
                            forM_
                                (view #poolMetadata cert)
                                $ \(_, metadataHash) ->
                                    putPoolMetadata
                                        metadataHash
                                        mockPoolMetadata
                        (publicationTime, Retirement cert) ->
                            putPoolRetirement publicationTime cert
                    regsAtStart <- poolIdsWithRegCerts
                    retsAtStart <- poolIdsWithRetCerts
                    metadataAtStart <-
                        Map.keysSet <$> atomically readPoolMetadata
                    atomically $ removePools $ Set.toList poolsToRemove
                    regsAtEnd <- poolIdsWithRegCerts
                    retsAtEnd <- poolIdsWithRetCerts
                    metadataAtEnd <-
                        Map.keysSet <$> atomically readPoolMetadata
                    pure
                        ( regsAtStart
                        , retsAtStart
                        , metadataAtStart
                        , regsAtEnd
                        , retsAtEnd
                        , metadataAtEnd
                        )
                monitor
                    $ counterexample
                    $ T.unpack
                    $ T.unlines
                        [ "All pools: "
                        , T.unlines (toText <$> Set.toList pools)
                        , "Pools to remove:"
                        , T.unlines (toText <$> Set.toList poolsToRemove)
                        , "Pools to retain:"
                        , T.unlines (toText <$> Set.toList poolsToRetain)
                        ]
                assertWith "subset rule for registrations"
                    $ poolIdsWithRegCertsAtEnd `Set.isSubsetOf` poolsToRetain
                assertWith "subset rule for retirements"
                    $ poolIdsWithRetCertsAtEnd `Set.isSubsetOf` poolsToRetain
                assertWith "disjoint rule for registrations"
                    $ poolIdsWithRegCertsAtEnd `Set.disjoint` poolsToRemove
                assertWith "disjoint rule for retirements"
                    $ poolIdsWithRetCertsAtEnd `Set.disjoint` poolsToRemove
                assertWith "difference rule for registrations"
                    $ poolIdsWithRegCertsAtStart `Set.difference` poolsToRemove
                        == poolIdsWithRegCertsAtEnd
                assertWith "difference rule for retirements"
                    $ poolIdsWithRetCertsAtStart `Set.difference` poolsToRemove
                        == poolIdsWithRetCertsAtEnd
                assertWith "equality rule #1 for metadata"
                    $ poolMetadataAtEnd == poolMetadataAtStart
                assertWith "equality rule #2 for metadata"
                    $ poolMetadataAtEnd == metadataToRetain
      where
        certificates = snd <$> entries

        notNull = not . null

        -- The complete set of all pools.
        pools = Set.fromList $ getPoolCertificatePoolId <$> certificates

        -- Divide the set of pools into two sets of approximately the same size.
        (poolsToRetain, poolsToRemove) =
            pools
                & Set.toList
                & L.splitAt (length pools `div` 2)
                & bimap Set.fromList Set.fromList

        -- For the moment, we never delete any metadata.
        metadataToRetain =
            certificates
                & mapMaybe toRegistrationCertificate
                & mapMaybe (view #poolMetadata)
                & fmap snd
                & Set.fromList

        toRegistrationCertificate
            :: PoolCertificate -> Maybe PoolRegistrationCertificate
        toRegistrationCertificate = \case
            Registration c -> Just c
            Retirement _ -> Nothing

        certificatePublications
            :: [(CertificatePublicationTime, PoolCertificate)]
        certificatePublications =
            testCertificatePublicationTimes `zip` certificates

        mockPoolMetadata =
            StakePoolMetadata
                { ticker = StakePoolTicker "MOCK"
                , name = "MOCK"
                , description = Nothing
                , homepage = "http://mock.pool/"
                }

prop_listRegisteredPools
    :: (forall a. (DBLayer IO -> IO a) -> IO a)
    -> [PoolRegistrationCertificate]
    -> Property
prop_listRegisteredPools withDB entries =
    monadicIO $ do
        pools <- run $ withDB $ \DBLayer{..} -> do
            let entries' =
                    [ CertificatePublicationTime (SlotNo s) minBound
                    | s <- [0 ..]
                    ]
                        `zip` entries
            atomically $ mapM_ (uncurry putPoolRegistration) entries'
            atomically listRegisteredPools
        monitor
            $ classify
                (any hasDuplicateOwners entries)
                "same owner multiple time in the same certificate"
        monitor
            $ counterexample
            $ unlines
                [ "Read from DB: " <> show pools
                ]
        assertWith
            "pools written == pools retrieved"
            (L.sort pools == L.sort (view #poolId <$> entries))
  where
    hasDuplicateOwners PoolRegistrationCertificate{poolOwners} =
        L.nub poolOwners /= poolOwners

-- | Test that `listRetiredPools` returns the correct set of retirements for
--   any given epoch.
--
-- This property tests `listRetiredPools` in conditions where:
--
--   - there are multiple pools;
--   - there are multiple registrations and retirements for each pool;
--   - certificates affecting different pools are interleaved in time.
prop_listRetiredPools_multiplePools_multipleCerts
    :: (forall a. (DBLayer IO -> IO a) -> IO a)
    -> MultiPoolCertificateSequence
    -> Property
prop_listRetiredPools_multiplePools_multipleCerts
    withDB
    mpcs = monadicIO $ do
        comparisons <- run $ withDB $ \db@DBLayer{..} -> do
            mapM_ (uncurry $ putPoolCertificate db) allPublications
            lifeCycleStatuses <-
                atomically $ mapM readPoolLifeCycleStatus allPoolIds
            let poolsMarkedToRetire =
                    mapMaybe getPoolRetirementCertificate lifeCycleStatuses
            let epochsToTest =
                    EpochNo minBound
                        : EpochNo maxBound
                        : L.nub (view #retirementEpoch <$> poolsMarkedToRetire)
            forM epochsToTest $ \currentEpoch -> do
                retiredPoolsActual <- atomically $ listRetiredPools currentEpoch
                let retiredPoolsExpected =
                        filter
                            ((<= currentEpoch) . view #retirementEpoch)
                            poolsMarkedToRetire
                pure (retiredPoolsActual, retiredPoolsExpected)
        forM_ comparisons $ \(retiredPoolsActual, retiredPoolsExpected) ->
            assert
                $ (==)
                    (Set.fromList retiredPoolsActual)
                    (Set.fromList retiredPoolsExpected)
      where
        allPoolIds :: [PoolId]
        allPoolIds = getSinglePoolId <$> getSinglePoolSequences mpcs

        allPublications :: [(CertificatePublicationTime, PoolCertificate)]
        allPublications =
            testCertificatePublicationTimes
                `zip` getMultiPoolCertificateSequence mpcs

-- | Test `listPoolLifeCycleData` by showing that the following operations are
--   equivalent:
--
--   - Calling `listPoolLifeCycleData` once to fetch lifecycle data for all
--     active pools.
--
--   - Calling `readPoolLifeCycleStatus` multiple times, once for each known
--     pool, and coalescing the results.
--
-- The former operation (calling `listPoolLifeCycleStatus` once) is designed to
-- be efficient, as it delivers its results with only a single database query.
--
-- The latter operation (calling `readPoolLifeCycleStatus` multiple times) is
-- extremely inefficient, but consists of simpler database operations that we
-- already verify with other properties.
--
-- This property tests that both operations give equivalent results, even with
-- complex sequences of pool registration and retirement certificates.
prop_listPoolLifeCycleData_multiplePools_multipleCerts
    :: (forall a. (DBLayer IO -> IO a) -> IO a)
    -> MultiPoolCertificateSequence
    -> Property
prop_listPoolLifeCycleData_multiplePools_multipleCerts
    withDB
    mpcs = monadicIO $ do
        (epochsToTest, poolsMarkedToRetire, comparisons) <-
            run $ withDB $ \db@DBLayer{..} -> do
                mapM_ (uncurry $ putPoolCertificate db) allPublications
                lifeCycleDataReadIndividually <-
                    filter isRegistered
                        <$> atomically
                            (mapM readPoolLifeCycleStatus allPoolIds)
                let poolsMarkedToRetire =
                        mapMaybe
                            getPoolRetirementCertificate
                            lifeCycleDataReadIndividually
                let epochsToTest =
                        EpochNo minBound
                            : EpochNo maxBound
                            : L.nub
                                ( view #retirementEpoch
                                    <$> poolsMarkedToRetire
                                )
                comparisons <- forM epochsToTest $ \currentEpoch -> do
                    lifeCycleDataActual <-
                        Set.fromList
                            <$> atomically
                                (listPoolLifeCycleData currentEpoch)
                    let lifeCycleDataExpected =
                            Set.fromList
                                $ filter
                                    (not . isRetired currentEpoch)
                                    lifeCycleDataReadIndividually
                    pure
                        ( currentEpoch
                        , lifeCycleDataExpected
                        , lifeCycleDataActual
                        )
                pure (epochsToTest, poolsMarkedToRetire, comparisons)
        forM_ comparisons $ \(currentEpoch, expected, actual) -> do
            monitor
                $ counterexample
                $ unlines
                    [ "\nEpochs to test: "
                    , show epochsToTest
                    , "\nCurrent epoch: "
                    , show currentEpoch
                    , "\nPools marked with a retirement epoch: "
                    , show poolsMarkedToRetire
                    , "\nExpected lifecycle data: "
                    , show expected
                    , "\nActual lifecycle data: "
                    , show actual
                    ]
            assert $ expected == actual
      where
        isRegistered :: PoolLifeCycleStatus -> Bool
        isRegistered = \case
            PoolNotRegistered -> False
            PoolRegistered{} -> True
            PoolRegisteredAndRetired{} -> True

        isRetired :: EpochNo -> PoolLifeCycleStatus -> Bool
        isRetired currentEpoch status =
            maybe
                (False)
                ((<= currentEpoch) . view #retirementEpoch)
                (getPoolRetirementCertificate status)

        allPoolIds :: [PoolId]
        allPoolIds = getSinglePoolId <$> getSinglePoolSequences mpcs

        allPublications :: [(CertificatePublicationTime, PoolCertificate)]
        allPublications =
            testCertificatePublicationTimes
                `zip` getMultiPoolCertificateSequence mpcs

prop_unfetchedPoolMetadataRefs
    :: (forall a. (DBLayer IO -> IO a) -> IO a)
    -> [PoolRegistrationCertificate]
    -> Property
prop_unfetchedPoolMetadataRefs withDB entries =
    monadicIO $ do
        generatedMetadata <- forM metas $ \(url, _) -> do
            metadata <- pick $ genStakePoolMetadata url
            pure (url, metadata)
        (refs, interaction) <- run $ withDB $ \DBLayer{..} -> do
            let entries' =
                    [ CertificatePublicationTime (SlotNo s) minBound
                    | s <- [0 ..]
                    ]
                        `zip` entries
            atomically $ mapM_ (uncurry putPoolRegistration) entries'
            refs <- atomically $ unfetchedPoolMetadataRefs 10
            interaction <- case take 1 refs of
                [(_, url, hash)] -> do
                    let metadata = case lookup url generatedMetadata of
                            Just value -> value
                            Nothing -> error "metadata URL not generated"
                    atomically $ putPoolMetadata hash metadata
                    refs' <- atomically $ unfetchedPoolMetadataRefs 10
                    pure $ Just (hash, refs')
                _ -> pure Nothing
            pure (refs, interaction)
        monitor $ classify (length entries > 10) "10+ entries"
        monitor $ classify (length entries > 50) "50+ entries"
        monitor
            $ counterexample
            $ unlines
                [ "Read from DB (" <> show (length refs) <> "): " <> show refs
                ]
        assertWith
            "fewer unfetchedPoolMetadataRefs than registrations"
            (length refs <= length entries)
        assertWith
            "all metadata hashes are indeed known"
            (all ((`elem` hashes) . (\(_, _, c) -> c)) refs)
        assertWith
            "no duplicate"
            (L.nub refs == refs)
        forM_ interaction $ \(hash, refs') -> do
            monitor
                $ counterexample
                $ unlines
                    [ "Read from DB ("
                        <> show (length refs')
                        <> "): "
                        <> show refs'
                    ]
            assertWith
                "fetching metadata removes it from unfetchedPoolMetadataRefs"
                (hash `notElem` ((\(_, _, c) -> c) <$> refs'))
  where
    metas = mapMaybe poolMetadata entries
    hashes = snd <$> metas

prop_unfetchedPoolMetadataRefsIgnoring
    :: (forall a. (DBLayer IO -> IO a) -> IO a)
    -> [PoolRegistrationCertificate]
    -> Property
prop_unfetchedPoolMetadataRefsIgnoring withDB entries =
    length metas >= 2 ==>
        monadicIO $ do
            let recent = case metas of (m : _) -> m; [] -> error "expected metas"
            refs <- run $ withDB $ \DBLayer{..} -> do
                let entries' =
                        [ CertificatePublicationTime (SlotNo s) minBound
                        | s <- [0 ..]
                        ]
                            `zip` entries
                atomically $ mapM_ (uncurry putPoolRegistration) entries'
                atomically $ putFetchAttempt recent
                atomically $ unfetchedPoolMetadataRefs 10
            monitor
                $ counterexample
                $ unlines
                    [ "Read from DB (" <> show (length refs) <> "): " <> show refs
                    ]
            assertWith
                "recently failed URLs are ignored"
                (recent `notElem` ((\(_, b, c) -> (b, c)) <$> refs))
  where
    metas = mapMaybe poolMetadata entries

-- | successive readSystemSeed yield the exact same value
prop_readSystemSeedIdempotent
    :: (forall a. (DBLayer IO -> IO a) -> IO a)
    -> Positive Int
    -> Property
prop_readSystemSeedIdempotent withDB (Positive n) =
    monadicIO $ do
        seeds <- run $ withDB $ \DBLayer{..} ->
            map show <$> replicateM n (atomically readSystemSeed)
        let firstS = case seeds of (s : _) -> s; [] -> error "expected seeds"
        monitor $ counterexample $ show seeds
        monitor $ counterexample $ show $ filter (/= firstS) seeds
        assert (all (== firstS) seeds)

prop_determinePoolLifeCycleStatus_orderCorrect
    :: forall certificatePublicationTime
     . (certificatePublicationTime ~ Int)
    => (certificatePublicationTime, PoolRegistrationCertificate)
    -> (certificatePublicationTime, PoolRetirementCertificate)
    -> Property
prop_determinePoolLifeCycleStatus_orderCorrect regData retData =
    checkCoverageWith okayConfidence
        $ cover
            25
            (regTime > retTime)
            "registration cert time > retirement cert time"
        $ cover
            25
            (regTime < retTime)
            "registration cert time < retirement cert time"
        $ cover
            1
            (regTime == retTime)
            "registration cert time = retirement cert time"
        $ property prop
  where
    prop
        | regTime > retTime =
            -- A re-registration always /supersedes/ a prior retirement.
            result `shouldBe` PoolRegistered regCert
        | regTime < retTime =
            -- A retirement always /augments/ the latest registration.
            result `shouldBe` PoolRegisteredAndRetired regCert retCert
        | otherwise =
            -- If a registration certificate and a retirement certificate
            -- for the same pool appear to have been published at exactly
            -- the same time, this represents a programming error.
            evaluate result `shouldThrow` anyException

    sharedPoolId = view #poolId regCertAnyPool

    (regTime, regCertAnyPool) = regData
    (retTime, retCertAnyPool) = retData

    regCert = set #poolId sharedPoolId regCertAnyPool
    retCert = set #poolId sharedPoolId retCertAnyPool

    result =
        determinePoolLifeCycleStatus
            (pure (regTime, regCert))
            (pure (retTime, retCert))

-- If we've never seen a registration certificate for a given pool, we /always/
-- indicate that the pool was /not registered/, /regardless/ of whether or not
-- we've seen a retirement certificate for that pool.
--
prop_determinePoolLifeCycleStatus_neverRegistered
    :: forall certificatePublicationTime
     . (certificatePublicationTime ~ Int)
    => Maybe (certificatePublicationTime, PoolRetirementCertificate)
    -> Property
prop_determinePoolLifeCycleStatus_neverRegistered maybeRetData =
    checkCoverageWith okayConfidence
        $ cover
            40
            (isJust maybeRetData)
            "with retirement data"
        $ cover
            10
            (isNothing maybeRetData)
            "without retirement data"
        $ property
        $ result
        `shouldBe` PoolNotRegistered
  where
    result = determinePoolLifeCycleStatus Nothing maybeRetData

-- Calling 'determinePoolLifeCycleStatus' with certificates from different
-- pools is a programming error, and should result in an exception.
--
prop_determinePoolLifeCycleStatus_differentPools
    :: forall certificatePublicationTime
     . (certificatePublicationTime ~ Int)
    => (certificatePublicationTime, PoolRegistrationCertificate)
    -> (certificatePublicationTime, PoolRetirementCertificate)
    -> Property
prop_determinePoolLifeCycleStatus_differentPools regData retData =
    property $ (regPoolId /= retPoolId) ==> prop
  where
    prop = evaluate result `shouldThrow` anyException

    regPoolId = view #poolId regCert
    retPoolId = view #poolId retCert

    (regTime, regCert) = regData
    (retTime, retCert) = retData

    result =
        determinePoolLifeCycleStatus
            (pure (regTime, regCert))
            (pure (retTime, retCert))

prop_SinglePoolCertificateSequence_coverage
    :: SinglePoolCertificateSequence
    -> Property
prop_SinglePoolCertificateSequence_coverage
    s@(SinglePoolCertificateSequence _sharedPoolId certificates) =
        checkCoverageWith okayConfidence
            $ cover
                7
                (null certificates)
                "length (all certificates) = 0"
            $ cover
                7
                (length certificates == 1)
                "length (all certificates) = 1"
            $ cover
                40
                (length certificates > 1)
                "length (all certificates) > 1"
            $ cover
                5
                (null registrationCertificates)
                "length (registration certificates) = 0"
            $ cover
                5
                (length registrationCertificates == 1)
                "length (registration certificates) = 1"
            $ cover
                30
                (length registrationCertificates > 1)
                "length (registration certificates) > 1"
            $ cover
                5
                (null retirementCertificates)
                "length (retirement certificates) = 0"
            $ cover
                5
                (length retirementCertificates == 1)
                "length (retirement certificates) = 1"
            $ cover
                30
                (length retirementCertificates > 1)
                "length (retirement certificates) > 1"
            $ cover
                50
                (not (null shrunkenSequences))
                "length (shrunken sequences) > 0"
            $ all isValidSinglePoolCertificateSequence
            $ s : shrunkenSequences
      where
        shrunkenSequences = shrink s

        registrationCertificates =
            mapMaybe getRegistrationCertificate certificates
        retirementCertificates =
            mapMaybe getRetirementCertificate certificates
        getRegistrationCertificate = \case
            Registration cert -> Just cert
            Retirement _ -> Nothing
        getRetirementCertificate = \case
            Registration _ -> Nothing
            Retirement cert -> Just cert

prop_MultiPoolCertificateSequence_coverage
    :: MultiPoolCertificateSequence
    -> Property
prop_MultiPoolCertificateSequence_coverage mpcs =
    checkCoverageWith okayConfidence
        -- Check the number of certificates:
        $ cover
            1
            (certificateCount == 0)
            "number of certificates: = 0"
        $ cover
            3
            (certificateCount > 0 && certificateCount <= 10)
            "number of certificates: > 0 && <= 10"
        $ cover
            20
            (certificateCount > 10 && certificateCount <= 100)
            "number of certificates: > 10 && <= 100"
        -- Check the number of pools:
        $ cover
            3
            (poolCount == 0)
            "number of pools: = 0"
        $ cover
            10
            (poolCount > 0 && poolCount <= 10)
            "number of pools: > 0 && <= 10"
        $ cover
            30
            (poolCount > 10 && poolCount <= 100)
            "number of pools: > 10 && <= 100"
            True
  where
    certificateCount =
        L.sum
            $ L.length . getSinglePoolCertificateSequence <$> certificateSequences
    certificateSequences = getSinglePoolSequences mpcs
    poolCount = length certificateSequences

-- | read . put == pure
prop_putHeaderListHeader
    :: (forall a. (DBLayer IO -> IO a) -> IO a)
    -> [BlockHeader]
    -> NonNegative Int
    -> Property
prop_putHeaderListHeader withDB headers (NonNegative k) =
    monadicIO $ do
        lHeaders <- run $ withDB $ \DBLayer{..} -> do
            void $ atomically $ forM sortedHeaders putHeader
            atomically $ listHeaders k
        monitor
            $ counterexample
            $ unlines
                [ "Read from DB: " <> show lHeaders
                , "Expected: " <> show expected
                , "Input: " <> show sortedHeaders
                , "k: " <> show k
                ]
        monitor $ classify (null lHeaders) "Empty distributions"
        assertWith
            "headers are increasing in block height"
            (strictlyIncreasing . fmap blockHeight $ lHeaders)
        assertWith "read back the headers we expect" (lHeaders == expected)
  where
    -- The generator can't really simulate how we get blocks from the node,
    -- so we need to do some deduplication and sorting.
    sortedHeaders = nubSortOn blockHeight headers
    -- This simulates the limiting of results to the last k elements,
    -- while retaining ascending order.
    expected = limitTo k sortedHeaders
    strictlyIncreasing :: Ord a => [a] -> Bool
    strictlyIncreasing [] = True
    strictlyIncreasing [_] = True
    strictlyIncreasing (x : y : xs) =
        let !b = (x < y)
        in  b && strictlyIncreasing (y : xs)

    limitTo
        :: Int
        -> [a]
        -- \^ input list, sorted in ascending order
        -> [a]
    -- \^ output sorted in ascending order, but limited
    --   to last n elements
    limitTo n
        | n <= 0 = id
        -- this emulates persistent [LimitTo n, Desc ...]
        | otherwise = reverse . take n . reverse

-- | read . put == pure
prop_modSettingsReadSettings
    :: (forall a. (DBLayer IO -> IO a) -> IO a)
    -> Settings
    -> Property
prop_modSettingsReadSettings withDB settings =
    monadicIO $ do
        (defSettings, modSettings') <- run $ withDB $ \DBLayer{..} -> do
            initial <- atomically readSettings
            atomically $ putSettings settings
            modified <- atomically readSettings
            pure (initial, modified)
        assertWith
            "Reading settings from empty table returns default settings"
            (defSettings == defaultSettings)
        assertWith
            "Modifying settings and reading afterwards works"
            (modSettings' == settings)

-- | read . put == id
prop_putLastMetadataGCReadLastMetadataGC
    :: (forall a. (DBLayer IO -> IO a) -> IO a)
    -> POSIXTime
    -> Property
prop_putLastMetadataGCReadLastMetadataGC withDB posixTime =
    monadicIO $ do
        (defGCTime, time) <- run $ withDB $ \DBLayer{..} -> do
            initial <- atomically readLastMetadataGC
            atomically $ putLastMetadataGC posixTime
            modified <- atomically readLastMetadataGC
            pure (initial, modified)
        assertWith
            "Reading sync time from empty db returns Nothing"
            (isNothing defGCTime)
        assertWith
            "Setting sync time and reading afterwards works"
            (time == Just posixTime)

-- Check that removing pool metadata removes delisted pools.
prop_removePoolMetadataDelistedPools
    :: (forall a. (DBLayer IO -> IO a) -> IO a)
    -> Set.Set PoolId
    -> Property
prop_removePoolMetadataDelistedPools withDB pools =
    monadicIO $ do
        (poolsActuallyDelisted, poolsAfter) <-
            run $ withDB $ \DBLayer{..} -> do
                atomically $ putDelistedPools (Set.toList pools)
                actuallyDelisted <-
                    Set.fromList . L.sort <$> atomically readDelistedPools
                atomically removePoolMetadata
                after <- Set.fromList . L.sort <$> atomically readDelistedPools
                pure (actuallyDelisted, after)
        monitor
            $ counterexample
            $ unlines
                [ "Pools to mark as delisted: "
                , pretty $ Set.toList pools
                , "Pools actually delisted: "
                , pretty $ Set.toList poolsActuallyDelisted
                ]
        assertWith "pools == poolsActuallyDelisted"
            $ pools == poolsActuallyDelisted
        assertWith "[] == poolsAfter"
            $ null (Set.toList poolsAfter)

-- Check that 'putDelistedPools' completely overwrites the existing set
-- of delisted pools every time:
--
prop_putDelistedPools
    :: (forall a. (DBLayer IO -> IO a) -> IO a)
    -> [PoolId]
    -> [PoolId]
    -> Property
prop_putDelistedPools withDB pools1 pools2 =
    checkCoverageWith okayConfidence
        $ cover
            2
            (Set.size poolSet1 == 0)
            "number of pools in set #1 = 0"
        $ cover
            2
            (Set.size poolSet1 == 1)
            "number of pools in set #1 = 1"
        $ cover
            40
            (Set.size poolSet1 > 1)
            "number of pools in set #1 > 1"
        $ cover
            2
            (Set.size poolSet2 == 0)
            "number of pools in set #2 = 0"
        $ cover
            2
            (Set.size poolSet2 == 1)
            "number of pools in set #2 = 1"
        $ cover
            40
            (Set.size poolSet2 > 1)
            "number of pools in set #2 > 1"
        $ monadicIO
        $ do
            comparisons <- run $ withDB $ \DBLayer{..} ->
                forM [poolSet1, poolSet2] $ \poolsToMarkAsDelisted -> do
                    atomically
                        $ putDelistedPools
                        $ Set.toList poolsToMarkAsDelisted
                    poolsActuallyDelisted <-
                        Set.fromList . L.sort <$> atomically readDelistedPools
                    pure (poolsToMarkAsDelisted, poolsActuallyDelisted)
            forM_ comparisons $ \(poolsToMarkAsDelisted, poolsActuallyDelisted) -> do
                monitor
                    $ counterexample
                    $ unlines
                        [ "Pools to mark as delisted: "
                        , pretty $ Set.toList poolsToMarkAsDelisted
                        , "Pools actually delisted: "
                        , pretty $ Set.toList poolsActuallyDelisted
                        ]
                assertWith
                    "poolsToMarkAsDelisted == poolsActuallyDelisted"
                    $ poolsToMarkAsDelisted == poolsActuallyDelisted
  where
    poolSet1 = Set.fromList pools1 `Set.difference` Set.fromList pools2
    poolSet2 = Set.fromList pools2 `Set.difference` Set.fromList pools1

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
uniqueEpochs = nubOrd . map (epochOf' . view #slotNo . snd)
  where
    epochOf' = runIdentity . interpretQuery ti . epochOf
    ti = dummyTimeInterpreter

-- | Concatenate stake pool production for all epochs in the test fixture.
allPoolProduction
    :: DBLayer IO -> StakePoolsFixture -> IO [(SlotNo, PoolId)]
allPoolProduction DBLayer{..} (StakePoolsFixture pairs _) =
    atomically
        $ rearrange <$> mapM readPoolProduction (uniqueEpochs pairs)
  where
    rearrange ms =
        concat
            [ [(view #slotNo h, p) | h <- hs]
            | (p, hs) <- concatMap Map.assocs ms
            ]

-- | Write any kind of pool certificate to the database.
putPoolCertificate
    :: DBLayer m
    -> CertificatePublicationTime
    -> PoolCertificate
    -> m ()
putPoolCertificate
    DBLayer{atomically, putPoolRegistration, putPoolRetirement}
    publicationTime =
        atomically . \case
            Registration c ->
                putPoolRegistration publicationTime c
            Retirement c ->
                putPoolRetirement publicationTime c

-- | A sequence of certificate publication times that is useful for testing.
testCertificatePublicationTimes :: [CertificatePublicationTime]
testCertificatePublicationTimes =
    [ CertificatePublicationTime (SlotNo sn) ii
    | sn <- [0 ..]
    , ii <- [0 .. 3]
    ]

instance Arbitrary BlockHeader where
    arbitrary = genSlotNo >>= genBlockHeader

instance Arbitrary POSIXTime where
    arbitrary = do
        NonNegative int <- arbitrary @(NonNegative Int)
        pure (fromIntegral int)
