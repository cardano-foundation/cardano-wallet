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
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Cardano.Pool.DB.Properties (properties) where

import Prelude

import Cardano.Pool.DB
    ( DBLayer (..), ErrPointAlreadyExists (..), determinePoolLifeCycleStatus,
    readPoolLifeCycleStatus )
import Cardano.Pool.DB.Arbitrary
    ( ManyPoolCertificates (..), MultiPoolCertificateSequence (..),
    SinglePoolCertificateSequence (..), StakePoolsFixture (..),
    genStakePoolMetadata, getMultiPoolCertificateSequence,
    isValidSinglePoolCertificateSequence )
import Cardano.Pool.Metadata.Types
    ( StakePoolMetadata (..) )
import Cardano.Pool.Types
    ( PoolId (..), StakePoolTicker (..) )
import Cardano.Wallet.DummyTarget.Primitive.Types
    ( dummyTimeInterpreter )
import Cardano.Wallet.Gen
    ( genBlockHeader, genSlotNo )
import Cardano.Wallet.Primitive.Slotting
    ( epochOf, interpretQuery )
import Cardano.Wallet.Primitive.Types
    ( BlockHeader (..), CertificatePublicationTime (..), EpochNo (..),
    PoolCertificate (..), PoolLifeCycleStatus (..),
    PoolRegistrationCertificate (..), PoolRetirementCertificate, Settings,
    SlotNo (..), defaultSettings, getPoolCertificatePoolId,
    getPoolRetirementCertificate )
import Cardano.Wallet.Unsafe
    ( unsafeRunExceptT )
import Control.Arrow
    ( second )
import Control.Monad
    ( forM, forM_, replicateM, unless, void )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Monad.Trans.Except
    ( runExceptT )
import Data.Bifunctor
    ( bimap )
import Data.Function
    ( on, (&) )
import Data.Functor.Identity
    ( runIdentity )
import Data.Generics.Internal.VL.Lens
    ( set, view )
import Data.List.Extra
    ( nubOrd, nubSortOn )
import Data.Map.Strict
    ( Map )
import Data.Maybe
    ( catMaybes, fromMaybe, isJust, isNothing, listToMaybe, mapMaybe )
import Data.Ord
    ( Down (..) )
import Data.Quantity
    ( Quantity (..) )
import Data.Text.Class
    ( toText )
import Data.Time.Clock.POSIX
    ( POSIXTime )
import Data.Word
    ( Word64 )
import Fmt
    ( pretty )
import Test.Hspec
    ( Expectation, SpecWith, anyException, describe, it, shouldBe, shouldReturn,
    shouldThrow )
import Test.QuickCheck
    ( Arbitrary, Confidence (..), NonNegative (..), Positive (..), Property,
    arbitrary, checkCoverageWith, classify, counterexample, cover, property,
    shrink, withMaxSuccess, (==>) )
import Test.QuickCheck.Monadic
    ( PropertyM, assert, monadicIO, monitor, pick, run )
import UnliftIO.Exception
    ( evaluate )

import qualified Cardano.Pool.DB.MVar as MVar
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T

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
        it "putPoolRegistration then readPoolRegistration yields expected result"
            (property . prop_poolRegistration)
        it "putPoolRetirement then readPoolRetirement yields expected result"
            (property . prop_poolRetirement)
        it "prop_multiple_putPoolRegistration_single_readPoolRegistration"
            (property .  prop_multiple_putPoolRegistration_single_readPoolRegistration)
        it "prop_multiple_putPoolRetirement_single_readPoolRetirement"
            (property .  prop_multiple_putPoolRetirement_single_readPoolRetirement)
        it "readPoolLifeCycleStatus respects certificate publication order"
            (property . prop_readPoolLifeCycleStatus)
        it "rollback of PoolRegistration"
            (property . prop_rollbackRegistration)
        it "rollback of PoolRetirement"
            (property . prop_rollbackRetirement)
        it "removePools"
            (property . prop_removePools)
        it "readStake . putStake a1 . putStake s0 == pure a1"
            (property . prop_putStakePutStake)
        it "readSystemSeed is idempotent"
            (property . prop_readSystemSeedIdempotent)
        it "putPoolRegistration . listRegisteredPools yield pools"
            (property . prop_listRegisteredPools)
        it "prop_listRetiredPools_multiplePools_multipleCerts"
            (property . prop_listRetiredPools_multiplePools_multipleCerts)
        it "prop_listPoolLifeCycleData_multiplePools_multipleCerts"
            (property . prop_listPoolLifeCycleData_multiplePools_multipleCerts)
        it "putPoolProduction* . readTotalProduction matches expectations"
            (property . prop_readTotalProduction)
        it "unfetchedPoolMetadataRefs"
            (property . prop_unfetchedPoolMetadataRefs)
        it "unfetchedPoolMetadataRefsIgnoring"
            (property . prop_unfetchedPoolMetadataRefsIgnoring)
        it "prop_determinePoolLifeCycleStatus_orderCorrect"
            (property . const
                prop_determinePoolLifeCycleStatus_orderCorrect)
        it "prop_determinePoolLifeCycleStatus_neverRegistered"
            (property . const
                prop_determinePoolLifeCycleStatus_neverRegistered)
        it "prop_determinePoolLifeCycleStatus_differentPools"
            (property . const
                prop_determinePoolLifeCycleStatus_differentPools)
        it "SinglePoolCertificateSequence coverage is adequate"
            (property . const prop_SinglePoolCertificateSequence_coverage)
        it "MultiPoolCertificateSequence coverage is adequate"
            (property . const prop_MultiPoolCertificateSequence_coverage)
        it "forM putHeader headers >> listHeaders == headers"
            (property . prop_putHeaderListHeader)
        it "modSettings . readSettings == id"
            (property . prop_modSettingsReadSettings)
        it "putLastMetadataGC . readLastMetadataGC == id"
            (property . prop_putLastMetadataGCReadLastMetadataGC)
        it "putDelistedPools >> readDelistedPools shows the pool as delisted"
            (property . prop_putDelistedPools)
        it "clearing metadata also clears delisted pools"
            (property . prop_removePoolMetadataDelistedPools)

okayConfidence :: Confidence
okayConfidence = Confidence { certainty = 10 ^ (6 :: Int), tolerance = 0.9 }

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
    :: DBLayer IO
    -> StakePoolsFixture
    -> Property
prop_putReadPoolProduction DBLayer{..} (StakePoolsFixture pairs _) =
    monadicIO (setup >>= prop)
  where
    setup = liftIO $ do
        atomically cleanDB
        db'@DBLayer{cleanDB=cleanDB',atomically=atomically'} <- MVar.newDBLayer ti
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
    ti = dummyTimeInterpreter

prop_readTotalProduction
    :: DBLayer IO
    -> StakePoolsFixture
    -> Property
prop_readTotalProduction DBLayer{..} (StakePoolsFixture pairs _) =
    monadicIO (setup >> prop)
  where
    setup = liftIO $ do
        atomically cleanDB
        atomically $ forM_ pairs $ \(pool, slot) ->
            unsafeRunExceptT $ putPoolProduction slot pool
    prop = do
        production <- run $ atomically readTotalProduction
        monitor $ counterexample ("from database: " <> show production)
        let production'
                = Map.map Quantity
                $ Map.fromListWith (+)
                $ second (const 1) <$> pairs
        assert (production == production')

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
    -> SlotNo
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
    withMaxSuccess 1000 $ monadicIO (setup >> prop)
  where
    slotPartition = L.groupBy ((==) `on` epochOf')
        $ L.sortOn epochOf'
        $ map (view #slotNo . snd) pairs
    epochGroups = map (\sls -> (epochOf' $ head sls, sls)) slotPartition
    setup = liftIO $ atomically cleanDB
    prop = do
        run $ do
            atomically $ forM_ pairs $ \(pool, slot) ->
                unsafeRunExceptT $ putPoolProduction slot pool
            forM_ epochGroups $ \(epoch, slots) -> do
                slots' <- Set.fromList . map (view #slotNo) . concat . Map.elems
                    <$> atomically (readPoolProduction epoch)
                slots' `shouldBe` (Set.fromList slots)

    epochOf' :: SlotNo -> EpochNo
    epochOf' = runIdentity . interpretQuery ti . epochOf
    ti = dummyTimeInterpreter

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
    slots = map (view #slotNo . snd) pairs
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

-- | Heavily relies upon the fact that generated values of 'PoolId' are unique.
prop_poolRegistration
    :: DBLayer IO
    -> ManyPoolCertificates PoolRegistrationCertificate
    -> Property
prop_poolRegistration DBLayer {..} (ManyPoolCertificates entries) =
    monadicIO (setup >> prop)
  where
    setup = run $ atomically cleanDB
    entriesIn = L.sort entries
    prop = do
        run $ atomically $
            mapM_ (uncurry putPoolRegistration) entriesIn
        entriesOut <- run . atomically $ L.sort . catMaybes
            <$> mapM (readPoolRegistration . view #poolId . snd) entries
        poolsMarkedToRetire <-
            run $ atomically $ listRetiredPools $ EpochNo maxBound
        monitor $ counterexample $ unlines
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

-- | Heavily relies upon the fact that generated values of 'PoolId' are unique.
prop_poolRetirement
    :: DBLayer IO
    -> ManyPoolCertificates PoolRetirementCertificate
    -> Property
prop_poolRetirement DBLayer {..} (ManyPoolCertificates entries) =
    monadicIO (setup >> prop)
  where
    setup = run $ atomically cleanDB
    entriesIn = L.sort entries
    prop = do
        run $ atomically $
            mapM_ (uncurry putPoolRetirement) entriesIn
        entriesOut <- run . atomically $ L.sort . catMaybes
            <$> mapM (readPoolRetirement . view #poolId . snd) entries
        poolsMarkedToRetire <-
            run $ atomically $ listRetiredPools $ EpochNo maxBound
        monitor $ counterexample $ unlines
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

-- For the same pool, write /multiple/ pool registration certificates to the
-- database and then read back the current registration certificate, verifying
-- that the certificate returned is the last one to have been written.
--
prop_multiple_putPoolRegistration_single_readPoolRegistration
    :: DBLayer IO
    -> PoolId
    -> ManyPoolCertificates PoolRegistrationCertificate
    -> Property
prop_multiple_putPoolRegistration_single_readPoolRegistration
    DBLayer {..} sharedPoolId (ManyPoolCertificates entries) =
        monadicIO (setup >> prop)
  where
    setup = run $ atomically cleanDB

    prop = do
        run $ atomically $
            mapM_ (uncurry putPoolRegistration) certificatePublications
        mRetrievedCertificatePublication <-
            run $ atomically $ readPoolRegistration sharedPoolId
        poolsMarkedToRetire <-
            run $ atomically $ listRetiredPools $ EpochNo maxBound
        monitor $ counterexample $ unlines
            [ "\nExpected certificate publication: "
            , show mExpectedCertificatePublication
            , "\nRetrieved certificate publication: "
            , show mRetrievedCertificatePublication
            , "\nNumber of certificate publications: "
            , show (length certificatePublications)
            , "\nAll certificate publications: "
            , unlines (("\n" <>) . show <$> certificatePublications)
            ]
        assertWith "retrieved certificate matches expectations" $ (==)
            mRetrievedCertificatePublication
            mExpectedCertificatePublication
        assertWith "pool is not marked to retire" $
            null poolsMarkedToRetire

    certificatePublications
        = L.nubBy (\(a,_) (b, _) -> view #slotNo a == view #slotNo b)
        $ second (set #poolId sharedPoolId) <$> entries

    mExpectedCertificatePublication = certificatePublications
        & L.sortOn (Down . view #slotNo . fst)
        & listToMaybe

-- For the same pool, write /multiple/ pool retirement certificates to the
-- database and then read back the current retirement certificate, verifying
-- that the certificate returned is the last one to have been written.
--
prop_multiple_putPoolRetirement_single_readPoolRetirement
    :: DBLayer IO
    -> PoolId
    -> ManyPoolCertificates PoolRetirementCertificate
    -> Property
prop_multiple_putPoolRetirement_single_readPoolRetirement
    DBLayer {..} sharedPoolId (ManyPoolCertificates entries) =
        monadicIO (setup >> prop)
  where
    setup = run $ atomically cleanDB

    prop = do
        run $ atomically $
            mapM_ (uncurry putPoolRetirement) certificatePublications
        mRetrievedCertificatePublication <-
            run $ atomically $ readPoolRetirement sharedPoolId
        poolsMarkedToRetire <-
            run $ atomically $ listRetiredPools $ EpochNo maxBound
        monitor $ counterexample $ unlines
            [ "\nExpected certificate publication: "
            , show mExpectedCertificatePublication
            , "\nRetrieved certificate publication: "
            , show mRetrievedCertificatePublication
            , "\nNumber of certificate publications: "
            , show (length certificatePublications)
            , "\nAll certificate publications: "
            , unlines (("\n" <>) . show <$> certificatePublications)
            ]
        assertWith "retrieved certificate matches expectations" $ (==)
            mRetrievedCertificatePublication
            mExpectedCertificatePublication
        assertWith "pool is marked to retire at the correct epoch" $
            case mRetrievedCertificatePublication of
                Nothing ->
                    null poolsMarkedToRetire
                Just (_publicationTime, retirementCert) ->
                    poolsMarkedToRetire == [retirementCert]

    certificatePublications
        = L.nubBy (\(a,_) (b, _) -> view #slotNo a == view #slotNo b)
        $ second (set #poolId sharedPoolId) <$> entries

    mExpectedCertificatePublication = certificatePublications
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
    :: DBLayer IO
    -> SinglePoolCertificateSequence
    -> Property
prop_readPoolLifeCycleStatus
    db@DBLayer {..} (SinglePoolCertificateSequence sharedPoolId certificates) =
        monadicIO (setup >> prop)
  where
    setup = run $ atomically cleanDB

    prop = do
        actualStatus <- run $ do
            mapM_ (uncurry $ putPoolCertificate db) certificatePublications
            atomically $ readPoolLifeCycleStatus sharedPoolId
        poolsMarkedToRetire <-
            run $ atomically $ listRetiredPools $ EpochNo maxBound
        monitor $ counterexample $ unlines
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
        assertWith "actualStatus == expectedStatus"
            (actualStatus == expectedStatus)
        assertWith "pool is marked to retire only when appropriate" $
            case actualStatus of
                PoolNotRegistered ->
                    null poolsMarkedToRetire
                PoolRegistered _regCert ->
                    null poolsMarkedToRetire
                PoolRegisteredAndRetired _regCert retCert ->
                    poolsMarkedToRetire == [retCert]

    expectedStatus = determinePoolLifeCycleStatus
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
    lookupFinalCertificateMatching match = certificatePublications
        & reverse
        & mapMaybe (traverse match)
        & listToMaybe

    certificatePublications :: [(CertificatePublicationTime, PoolCertificate)]
    certificatePublications = publicationTimes `zip` certificates

    publicationTimes =
        [ CertificatePublicationTime (SlotNo sn) ii
        | sn <- [0 .. 3]
        , ii <- [0 .. 3]
        ]

prop_rollbackRegistration
    :: DBLayer IO
    -> SlotNo
    -> ManyPoolCertificates PoolRegistrationCertificate
    -> Property
prop_rollbackRegistration DBLayer{..} rollbackPoint (ManyPoolCertificates entries) =
    monadicIO (setup >> prop)
  where
    setup = run $ atomically cleanDB

    beforeRollback pool = do
        case L.find (on (==) (view #poolId) pool . snd) entries of
            Nothing ->
                error "unknown pool?"
            Just (CertificatePublicationTime sl _, pool') ->
                (sl <= rollbackPoint) && (pool == pool')

    ownerHasManyPools =
        let owners = concatMap (poolOwners . snd) entries
        in L.length owners > L.length (L.nub owners)

    prop = do
        run . atomically $ mapM_ (uncurry putPoolRegistration) entries
        run . atomically $ rollbackTo rollbackPoint
        pools <- run . atomically $ L.sort . fmap snd . catMaybes
            <$> mapM (readPoolRegistration . (view #poolId) . snd) entries
        monitor $ classify (length pools < length entries) "rolled back some"
        monitor $ classify ownerHasManyPools "owner has many pools"
        monitor $ counterexample $ unlines
            [ "Read from DB:   " <> show pools
            ]
        assert (all beforeRollback pools)

-- Verify that retirement certificates are correctly rolled back.
--
prop_rollbackRetirement
    :: DBLayer IO
    -> [PoolRetirementCertificate]
    -> Property
prop_rollbackRetirement DBLayer{..} certificates =
    checkCoverageWith okayConfidence
        $ cover 15 (rollbackPoint == SlotNo 0)
            "rollbackPoint = slotMinBound"
        $ cover 35 (rollbackPoint > SlotNo 0)
            "rollbackPoint > slotMinBound"
        $ cover 2 (null expectedPublications)
            "length expectedPublications = 0"
        $ cover 50 (not (null expectedPublications))
            "length expectedPublications > 0"
        $ cover 40
            ( (&&)
                (not (null expectedPublications))
                (length expectedPublications < length allPublications)
            )
            "0 < length expectedPublications < length allPublications"
        $ monadicIO (setup >> prop)
  where
    setup = run $ atomically cleanDB

    prop = do
        run $ atomically $
            mapM_ (uncurry putPoolRetirement) allPublications
        run $ atomically $ rollbackTo rollbackPoint
        retrievedPublications <- catMaybes <$>
            run (atomically $ mapM readPoolRetirement poolIds)
        poolsMarkedToRetire <-
            run $ atomically $ listRetiredPools $ EpochNo maxBound
        monitor $ counterexample $ unlines
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
        assertWith "retrieved publications match expectations" $
            (==)
                retrievedPublications
                expectedPublications
        assertWith "only the correct retirements are listed" $
            (==)
                (Set.fromList $ snd <$> expectedPublications)
                (Set.fromList poolsMarkedToRetire)

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
            (\(CertificatePublicationTime slotId _, _) ->
                slotId <= rollbackPoint)
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
    :: DBLayer IO
    -> ManyPoolCertificates PoolCertificate
    -> Property
prop_removePools
    DBLayer {..} (ManyPoolCertificates entries) =
        checkCoverageWith okayConfidence
        $ cover 50 (notNull poolsToRemove && notNull poolsToRetain)
            "remove some pools and retain some pools"
        $ cover 2 (null poolsToRemove)
            "remove no pools"
        $ cover 3 (null poolsToRetain)
            "retain no pools"
        $ cover 50 (notNull metadataToRetain)
            "retain some metadata"
        $ cover 6 (null metadataToRetain)
            "retain no metadata"
        $ monadicIO (setup >> prop)
  where
    setup = run $ atomically cleanDB

    certificates = snd <$> entries

    notNull = not . null

    prop = do
        -- Firstly, publish an arbitrary set of pool certificates:
        run $ atomically $ forM_ certificatePublications $ \case
            (publicationTime, Registration cert) -> do
                -- In the case of a pool registration, we also add an
                -- accompanying mock entry to the metadata table.
                putPoolRegistration publicationTime cert
                forM_ (view #poolMetadata cert) $ \(_, metadataHash) ->
                    putPoolMetadata metadataHash mockPoolMetadata
            (publicationTime, Retirement cert) ->
                putPoolRetirement publicationTime cert
        -- Next, read the latest certificates and metadata for all pools:
        poolIdsWithRegCertsAtStart <- run poolIdsWithRegCerts
        poolIdsWithRetCertsAtStart <- run poolIdsWithRetCerts
        poolMetadataAtStart <- Map.keysSet <$> run (atomically readPoolMetadata)
        -- Next, remove a subset of the pools:
        run $ atomically $ removePools $ Set.toList poolsToRemove
        -- Finally, see which certificates and metadata remain:
        poolIdsWithRegCertsAtEnd <- run poolIdsWithRegCerts
        poolIdsWithRetCertsAtEnd <- run poolIdsWithRetCerts
        poolMetadataAtEnd <- Map.keysSet <$> run (atomically readPoolMetadata)
        monitor $ counterexample $ T.unpack $ T.unlines
            [ "All pools: "
            , T.unlines (toText <$> Set.toList pools)
            , "Pools to remove:"
            , T.unlines (toText <$> Set.toList poolsToRemove)
            , "Pools to retain:"
            , T.unlines (toText <$> Set.toList poolsToRetain)
            ]
        assertWith "subset rule for registrations" $
            poolIdsWithRegCertsAtEnd `Set.isSubsetOf` poolsToRetain
        assertWith "subset rule for retirements" $
            poolIdsWithRetCertsAtEnd `Set.isSubsetOf` poolsToRetain
        assertWith "disjoint rule for registrations" $
            poolIdsWithRegCertsAtEnd `Set.disjoint` poolsToRemove
        assertWith "disjoint rule for retirements" $
            poolIdsWithRetCertsAtEnd `Set.disjoint` poolsToRemove
        assertWith "difference rule for registrations" $
            poolIdsWithRegCertsAtStart `Set.difference` poolsToRemove
                == poolIdsWithRegCertsAtEnd
        assertWith "difference rule for retirements" $
            poolIdsWithRetCertsAtStart `Set.difference` poolsToRemove
                == poolIdsWithRetCertsAtEnd
        -- For the moment, we never delete any metadata.
        assertWith "equality rule #1 for metadata" $
            poolMetadataAtEnd == poolMetadataAtStart
        assertWith "equality rule #2 for metadata" $
            poolMetadataAtEnd == metadataToRetain

    -- The complete set of all pools.
    pools = Set.fromList $ getPoolCertificatePoolId <$> certificates

    -- Divide the set of pools into two sets of approximately the same size.
    (poolsToRetain, poolsToRemove) = pools
        & Set.toList
        & L.splitAt (length pools `div` 2)
        & bimap Set.fromList Set.fromList

    -- For the moment, we never delete any metadata.
    metadataToRetain = certificates
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

    poolIdsWithRegCerts =
        fmap (Set.fromList . fmap (view #poolId . snd) . catMaybes)
            <$> atomically $ mapM readPoolRegistration $ Set.toList pools

    poolIdsWithRetCerts =
        fmap (Set.fromList . fmap (view #poolId . snd) . catMaybes)
            <$> atomically $ mapM readPoolRetirement $ Set.toList pools

    mockPoolMetadata = StakePoolMetadata
        { ticker = StakePoolTicker "MOCK"
        , name = "MOCK"
        , description = Nothing
        , homepage = "http://mock.pool/"
        }

prop_listRegisteredPools
    :: DBLayer IO
    -> [PoolRegistrationCertificate]
    -> Property
prop_listRegisteredPools DBLayer {..} entries =
    monadicIO (setup >> prop)
  where
    setup = run $ atomically cleanDB

    hasDuplicateOwners PoolRegistrationCertificate{poolOwners} =
        L.nub poolOwners /= poolOwners

    prop = do
        let entries' =
                [ CertificatePublicationTime (SlotNo s) minBound
                | s <- [0 ..]
                ]
                `zip` entries
        run . atomically $ mapM_ (uncurry putPoolRegistration) entries'
        pools <- run . atomically $ listRegisteredPools
        monitor $ classify (any hasDuplicateOwners entries)
            "same owner multiple time in the same certificate"
        monitor $ counterexample $ unlines
            [ "Read from DB: " <> show pools
            ]
        assertWith "pools written == pools retrieved"
            (L.sort pools == L.sort (view #poolId <$> entries))

-- | Test that `listRetiredPools` returns the correct set of retirements for
--   any given epoch.
--
-- This property tests `listRetiredPools` in conditions where:
--
--   - there are multiple pools;
--   - there are multiple registrations and retirements for each pool;
--   - certificates affecting different pools are interleaved in time.
--
prop_listRetiredPools_multiplePools_multipleCerts
    :: DBLayer IO
    -> MultiPoolCertificateSequence
    -> Property
prop_listRetiredPools_multiplePools_multipleCerts
    db@DBLayer {..} mpcs = monadicIO (setup >> prop)
  where
    setup = run $ atomically cleanDB

    prop = do
        run $ mapM_ (uncurry $ putPoolCertificate db) allPublications
        lifeCycleStatuses <- run $ atomically $ do
            mapM readPoolLifeCycleStatus allPoolIds
        let poolsMarkedToRetire =
                mapMaybe getPoolRetirementCertificate lifeCycleStatuses
        let epochsToTest =
                EpochNo minBound :
                EpochNo maxBound :
                L.nub (view #retirementEpoch <$> poolsMarkedToRetire)
        forM_ epochsToTest $ \currentEpoch -> do
            let retiredPoolsExpected = filter
                    ((<= currentEpoch) . view #retirementEpoch)
                    (poolsMarkedToRetire)
            retiredPoolsActual <-
                run $ atomically $ listRetiredPools currentEpoch
            assert $ (==)
                (Set.fromList retiredPoolsActual)
                (Set.fromList retiredPoolsExpected)

    allPoolIds :: [PoolId]
    allPoolIds = getSinglePoolId <$> getSinglePoolSequences mpcs

    allPublications :: [(CertificatePublicationTime, PoolCertificate)]
    allPublications =
        testCertificatePublicationTimes
        `zip`
        getMultiPoolCertificateSequence mpcs

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
--
prop_listPoolLifeCycleData_multiplePools_multipleCerts
    :: DBLayer IO
    -> MultiPoolCertificateSequence
    -> Property
prop_listPoolLifeCycleData_multiplePools_multipleCerts
    db@DBLayer {..} mpcs = monadicIO (setup >> prop)
  where
    setup = run $ atomically cleanDB

    prop = do
        run $ mapM_ (uncurry $ putPoolCertificate db) allPublications
        lifeCycleDataReadIndividually <- filter isRegistered <$>
            run (atomically $ mapM readPoolLifeCycleStatus allPoolIds)
        let poolsMarkedToRetire =
                mapMaybe getPoolRetirementCertificate lifeCycleDataReadIndividually
        let epochsToTest =
                EpochNo minBound :
                EpochNo maxBound :
                L.nub (view #retirementEpoch <$> poolsMarkedToRetire)
        forM_ epochsToTest $ \currentEpoch -> do
            let lifeCycleDataExpected = Set.fromList $ filter
                    (not . isRetired currentEpoch)
                    (lifeCycleDataReadIndividually)
            lifeCycleDataActual <- Set.fromList <$> run
                (atomically $ listPoolLifeCycleData currentEpoch)
            monitor $ counterexample $ unlines
                [ "\nEpochs to test: "
                , show epochsToTest
                , "\nCurrent epoch: "
                , show currentEpoch
                , "\nPools marked with a retirement epoch: "
                , show poolsMarkedToRetire
                , "\nExpected lifecycle data: "
                , show lifeCycleDataExpected
                , "\nActual lifecycle data: "
                , show lifeCycleDataActual
                ]
            assert $ (==)
                lifeCycleDataExpected
                lifeCycleDataActual

    isRegistered :: PoolLifeCycleStatus -> Bool
    isRegistered = \case
        PoolNotRegistered -> False
        PoolRegistered {} -> True
        PoolRegisteredAndRetired {} -> True

    isRetired :: EpochNo -> PoolLifeCycleStatus -> Bool
    isRetired currentEpoch status = maybe
        (False)
        ((<= currentEpoch) . view #retirementEpoch)
        (getPoolRetirementCertificate status)

    allPoolIds :: [PoolId]
    allPoolIds = getSinglePoolId <$> getSinglePoolSequences mpcs

    allPublications :: [(CertificatePublicationTime, PoolCertificate)]
    allPublications =
        testCertificatePublicationTimes
        `zip`
        getMultiPoolCertificateSequence mpcs

prop_unfetchedPoolMetadataRefs
    :: DBLayer IO
    -> [PoolRegistrationCertificate]
    -> Property
prop_unfetchedPoolMetadataRefs DBLayer{..} entries =
    monadicIO (setup >> propWellFormedResult >> propInteractionWithPutPoolMetadata)
  where
    setup = do
        run . atomically $ cleanDB
        let entries' =
                [ CertificatePublicationTime (SlotNo s) minBound
                | s <- [0 ..]
                ]
                `zip` entries
        run . atomically $ mapM_ (uncurry putPoolRegistration) entries'
        monitor $ classify (length entries > 10) "10+ entries"
        monitor $ classify (length entries > 50) "50+ entries"

    propWellFormedResult = do
        let hashes = snd <$> mapMaybe poolMetadata entries
        refs <- run . atomically $ unfetchedPoolMetadataRefs 10
        monitor $ counterexample $ unlines
            [ "Read from DB (" <> show (length refs) <> "): " <> show refs
            ]
        assertWith "fewer unfetchedPoolMetadataRefs than registrations"
            (length refs <= length entries)
        assertWith "all metadata hashes are indeed known"
            (all ((`elem` hashes) . (\(_,_,c) -> c)) refs)
        assertWith "no duplicate"
            (L.nub refs == refs)

    propInteractionWithPutPoolMetadata = do
        refs <- run . atomically $ unfetchedPoolMetadataRefs 10
        unless (null refs) $ do
            let [(_, url, hash)] = take 1 refs
            metadata <- pick $ genStakePoolMetadata url
            run . atomically $ putPoolMetadata hash metadata
            refs' <- run . atomically $ unfetchedPoolMetadataRefs 10
            monitor $ counterexample $ unlines
                [ "Read from DB (" <> show (length refs') <> "): " <> show refs'
                ]
            assertWith "fetching metadata removes it from unfetchedPoolMetadataRefs"
                (hash `notElem` ((\(_,_,c) -> c) <$> refs'))

prop_unfetchedPoolMetadataRefsIgnoring
    :: DBLayer IO
    -> [PoolRegistrationCertificate]
    -> Property
prop_unfetchedPoolMetadataRefsIgnoring DBLayer{..} entries =
    length metas >= 2 ==> monadicIO (setup >> propIgnoredMetadataRefs)
  where
    metas = mapMaybe poolMetadata entries

    setup = do
        run . atomically $ cleanDB
        let entries' =
                [ CertificatePublicationTime (SlotNo s) minBound
                | s <- [0 ..]
                ]
                `zip` entries
        run . atomically $ mapM_ (uncurry putPoolRegistration) entries'

    propIgnoredMetadataRefs = do
        let recent = head metas
        run . atomically $ putFetchAttempt recent
        refs <- run . atomically $ unfetchedPoolMetadataRefs 10
        monitor $ counterexample $ unlines
            [ "Read from DB (" <> show (length refs) <> "): " <> show refs
            ]
        assertWith "recently failed URLs are ignored"
            (recent `notElem` ((\(_,b,c) -> (b,c)) <$> refs))

-- | successive readSystemSeed yield the exact same value
prop_readSystemSeedIdempotent
    :: DBLayer IO
    -> Positive Int
    -> Property
prop_readSystemSeedIdempotent DBLayer{..} (Positive n) =
    monadicIO (setup >> prop)
  where
    setup = run $ atomically cleanDB
    prop = do
        seeds <- map show <$> replicateM n (run $ atomically readSystemSeed)
        let firstS = head seeds
        monitor $ counterexample $ show seeds
        monitor $ counterexample $ show $ filter (/= firstS) seeds
        assert (all (== firstS) seeds)

prop_determinePoolLifeCycleStatus_orderCorrect
    :: forall certificatePublicationTime . (certificatePublicationTime ~ Int)
    => (certificatePublicationTime, PoolRegistrationCertificate)
    -> (certificatePublicationTime, PoolRetirementCertificate)
    -> Property
prop_determinePoolLifeCycleStatus_orderCorrect regData retData =
    checkCoverageWith okayConfidence
        $ cover 25 (regTime > retTime)
            "registration cert time > retirement cert time"
        $ cover 25 (regTime < retTime)
            "registration cert time < retirement cert time"
        $ cover 1 (regTime == retTime)
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

    result = determinePoolLifeCycleStatus
        (pure (regTime, regCert))
        (pure (retTime, retCert))

-- If we've never seen a registration certificate for a given pool, we /always/
-- indicate that the pool was /not registered/, /regardless/ of whether or not
-- we've seen a retirement certificate for that pool.
--
prop_determinePoolLifeCycleStatus_neverRegistered
    :: forall certificatePublicationTime . (certificatePublicationTime ~ Int)
    => Maybe (certificatePublicationTime, PoolRetirementCertificate)
    -> Property
prop_determinePoolLifeCycleStatus_neverRegistered maybeRetData =
    checkCoverageWith okayConfidence
        $ cover 40 (isJust maybeRetData)
            "with retirement data"
        $ cover 10 (isNothing maybeRetData)
            "without retirement data"
        $ property
        $ result `shouldBe` PoolNotRegistered
  where
    result = determinePoolLifeCycleStatus Nothing maybeRetData

-- Calling 'determinePoolLifeCycleStatus' with certificates from different
-- pools is a programming error, and should result in an exception.
--
prop_determinePoolLifeCycleStatus_differentPools
    :: forall certificatePublicationTime . (certificatePublicationTime ~ Int)
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

    result = determinePoolLifeCycleStatus
        (pure (regTime, regCert))
        (pure (retTime, retCert))

prop_SinglePoolCertificateSequence_coverage
    :: SinglePoolCertificateSequence
    -> Property
prop_SinglePoolCertificateSequence_coverage
    s@(SinglePoolCertificateSequence _sharedPoolId certificates) =
        checkCoverageWith okayConfidence
            $ cover 7 (null certificates)
                "length (all certificates) = 0"
            $ cover 7 (length certificates == 1)
                "length (all certificates) = 1"
            $ cover 40 (length certificates > 1)
                "length (all certificates) > 1"

            $ cover 5 (null registrationCertificates)
                "length (registration certificates) = 0"
            $ cover 5 (length registrationCertificates == 1)
                "length (registration certificates) = 1"
            $ cover 30 (length registrationCertificates > 1)
                "length (registration certificates) > 1"

            $ cover 5 (null retirementCertificates)
                "length (retirement certificates) = 0"
            $ cover 5 (length retirementCertificates == 1)
                "length (retirement certificates) = 1"
            $ cover 30 (length retirementCertificates > 1)
                "length (retirement certificates) > 1"

            $ cover 50 (not (null shrunkenSequences))
                "length (shrunken sequences) > 0"

            $ all isValidSinglePoolCertificateSequence $ s : shrunkenSequences
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
    $ cover 1 (certificateCount == 0)
        "number of certificates: = 0"
    $ cover 3 (certificateCount > 0 && certificateCount <= 10)
        "number of certificates: > 0 && <= 10"
    $ cover 20 (certificateCount > 10 && certificateCount <= 100)
        "number of certificates: > 10 && <= 100"
    -- Check the number of pools:
    $ cover 3 (poolCount == 0)
        "number of pools: = 0"
    $ cover 10 (poolCount > 0 && poolCount <= 10)
        "number of pools: > 0 && <= 10"
    $ cover 30 (poolCount > 10 && poolCount <= 100)
        "number of pools: > 10 && <= 100"
    True
  where
    certificateCount = L.sum $
        L.length . getSinglePoolCertificateSequence <$> certificateSequences
    certificateSequences = getSinglePoolSequences mpcs
    poolCount = length certificateSequences

-- | read . put == pure
prop_putHeaderListHeader
    :: DBLayer IO
    -> [BlockHeader]
    -> NonNegative Int
    -> Property
prop_putHeaderListHeader DBLayer{..} headers (NonNegative k) =
    monadicIO (setup >> prop)
  where
    -- The generator can't really simulate how we get blocks from the node,
    -- so we need to do some deduplicaton and sorting.
    sortedHeaders = nubSortOn blockHeight headers
    -- This simulates the limiting of results to the last k elements,
    -- while retaining ascending order.
    expected = limitTo k sortedHeaders
    setup = run $ atomically cleanDB
    prop = do
        void $ run $ atomically $ forM sortedHeaders putHeader
        lHeaders <- run $ atomically $ listHeaders k
        monitor $ counterexample $ unlines
            [ "Read from DB: " <> show lHeaders
            , "Expected: " <> show expected
            , "Input: " <> show sortedHeaders
            , "k: " <> show k]
        monitor $ classify (null lHeaders) "Empty distributions"
        -- this is implicit in 'expected', but it's nice to have
        -- as separate assertion
        assertWith "headers are inceraing in block height"
            (strictlyIncreasing . fmap blockHeight $ lHeaders)
        assertWith "read back the headers we expect" (lHeaders == expected)

    strictlyIncreasing :: Ord a => [a] -> Bool
    strictlyIncreasing [] = True
    strictlyIncreasing [_] = True
    strictlyIncreasing (x:y:xs) =
        let !b = (x < y)
        in b && strictlyIncreasing (y:xs)

    limitTo
        :: Int
        -> [a]  -- ^ input list, sorted in ascending order
        -> [a]  -- ^ output sorted in ascending order, but limited
                --   to last n elements
    limitTo n
        | n <= 0 = id
        -- this emulates persistent [LimitTo n, Desc ...]
        | otherwise = reverse . take n . reverse

-- | read . put == pure
prop_modSettingsReadSettings
    :: DBLayer IO
    -> Settings
    -> Property
prop_modSettingsReadSettings DBLayer{..} settings = do
    monadicIO (setup >> prop)
  where
    setup = run $ atomically cleanDB
    prop = do
        defSettings <- run $ atomically readSettings
        assertWith "Reading settings from empty table returns default settings"
            (defSettings == defaultSettings)
        run $ atomically $ putSettings settings
        modSettings' <- run $ atomically readSettings
        assertWith "Modifying settings and reading afterwards works"
            (modSettings' == settings)

-- | read . put == id
prop_putLastMetadataGCReadLastMetadataGC
    :: DBLayer IO
    -> POSIXTime
    -> Property
prop_putLastMetadataGCReadLastMetadataGC DBLayer{..} posixTime = do
    monadicIO (setup >> prop)
  where
    setup = run $ atomically cleanDB
    prop = do
        defGCTime <- run $ atomically readLastMetadataGC
        assertWith
            "Reading sync time from empty db returns Nothing"
            (isNothing defGCTime)
        run $ atomically $ putLastMetadataGC posixTime
        time <- run $ atomically readLastMetadataGC
        assertWith "Setting sync time and reading afterwards works"
            (time == Just posixTime)

-- Check that removing pool metadata removes delisted pools.
prop_removePoolMetadataDelistedPools
    :: DBLayer IO
    -> Set.Set PoolId
    -> Property
prop_removePoolMetadataDelistedPools DBLayer {..} pools =
    monadicIO (setup >> prop)
  where
    setup = run $ atomically cleanDB

    prop = do
        run $ atomically $ putDelistedPools (Set.toList pools)
        poolsActuallyDelisted <- Set.fromList . L.sort <$>
            run (atomically readDelistedPools)
        monitor $ counterexample $ unlines
            [ "Pools to mark as delisted: "
            , pretty $ Set.toList pools
            , "Pools actually delisted: "
            , pretty $ Set.toList poolsActuallyDelisted
            ]
        assertWith "pools == poolsActuallyDelisted"
            $ pools == poolsActuallyDelisted

        -- now should be empty
        run $ atomically removePoolMetadata
        poolsAfter <- Set.fromList . L.sort <$>
            run (atomically readDelistedPools)
        assertWith "[] == poolsAfter"
            $ null (Set.toList poolsAfter)

-- Check that 'putDelistedPools' completely overwrites the existing set
-- of delisted pools every time:
--
prop_putDelistedPools
    :: DBLayer IO
    -> [PoolId]
    -> [PoolId]
    -> Property
prop_putDelistedPools DBLayer {..} pools1 pools2 =
    checkCoverageWith okayConfidence
        $ cover 2 (Set.size poolSet1 == 0)
            "number of pools in set #1 = 0"
        $ cover 2 (Set.size poolSet1 == 1)
            "number of pools in set #1 = 1"
        $ cover 40 (Set.size poolSet1 > 1)
            "number of pools in set #1 > 1"
        $ cover 2 (Set.size poolSet2 == 0)
            "number of pools in set #2 = 0"
        $ cover 2 (Set.size poolSet2 == 1)
            "number of pools in set #2 = 1"
        $ cover 40 (Set.size poolSet2 > 1)
            "number of pools in set #2 > 1"
        $ monadicIO (setup >> prop)
  where
    poolSet1 = Set.fromList pools1 `Set.difference` Set.fromList pools2
    poolSet2 = Set.fromList pools2 `Set.difference` Set.fromList pools1

    setup = run $ atomically cleanDB

    prop = forM_ [poolSet1, poolSet2] $ \poolsToMarkAsDelisted -> do
        run $ atomically $ putDelistedPools $
            Set.toList poolsToMarkAsDelisted
        poolsActuallyDelisted <- Set.fromList . L.sort <$>
            run (atomically readDelistedPools)
        monitor $ counterexample $ unlines
            [ "Pools to mark as delisted: "
            , pretty $ Set.toList poolsToMarkAsDelisted
            , "Pools actually delisted: "
            , pretty $ Set.toList poolsActuallyDelisted
            ]
        assertWith "poolsToMarkAsDelisted == poolsActuallyDelisted"
            $ poolsToMarkAsDelisted == poolsActuallyDelisted

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
allPoolProduction :: DBLayer IO -> StakePoolsFixture -> IO [(SlotNo, PoolId)]
allPoolProduction DBLayer{..} (StakePoolsFixture pairs _) = atomically $
    rearrange <$> mapM readPoolProduction (uniqueEpochs pairs)
  where
    rearrange ms = concat
        [ [ (view #slotNo h, p) | h <- hs ]
        | (p, hs) <- concatMap Map.assocs ms
        ]

-- | Write any kind of pool certificate to the database.
putPoolCertificate
    :: DBLayer m
    -> CertificatePublicationTime
    -> PoolCertificate
    -> m ()
putPoolCertificate
    DBLayer {atomically, putPoolRegistration, putPoolRetirement}
    publicationTime = atomically . \case
        Registration c ->
            putPoolRegistration publicationTime c
        Retirement c ->
            putPoolRetirement publicationTime c

-- | A sequence of certificate publication times that is useful for testing.
--
testCertificatePublicationTimes :: [CertificatePublicationTime]
testCertificatePublicationTimes =
    [ CertificatePublicationTime (SlotNo sn) ii
    | sn <- [0 ..  ]
    , ii <- [0 .. 3]
    ]

instance Arbitrary BlockHeader where
    arbitrary = genSlotNo >>= genBlockHeader

instance Arbitrary POSIXTime where
    arbitrary = do
        NonNegative int <- arbitrary @(NonNegative Int)
        pure (fromIntegral int)
