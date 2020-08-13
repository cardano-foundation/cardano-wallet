{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Pool.DB.Properties
    ( properties
    , withDB
    , newMemoryDBLayer
    ) where

import Prelude

import Cardano.BM.Trace
    ( traceInTVarIO )
import Cardano.DB.Sqlite
    ( DBLog (..), SqliteContext )
import Cardano.Pool.DB
    ( DBLayer (..)
    , ErrPointAlreadyExists (..)
    , determinePoolLifeCycleStatus
    , readPoolLifeCycleStatus
    )
import Cardano.Pool.DB.Arbitrary
    ( ListSerializationMethod
    , SinglePoolCertificateSequence (..)
    , StakePoolsFixture (..)
    , genStakePoolMetadata
    , isValidSinglePoolCertificateSequence
    , serializeLists
    )
import Cardano.Pool.DB.Sqlite
    ( newDBLayer )
import Cardano.Wallet.DummyTarget.Primitive.Types
    ( dummyTimeInterpreter )
import Cardano.Wallet.Primitive.Slotting
    ( epochOf )
import Cardano.Wallet.Primitive.Types
    ( BlockHeader (..)
    , CertificatePublicationTime (..)
    , EpochNo (..)
    , PoolCertificate (..)
    , PoolId
    , PoolLifeCycleStatus (..)
    , PoolRegistrationCertificate (..)
    , PoolRetirementCertificate (..)
    , SlotNo (..)
    , getPoolCertificatePoolId
    , getPoolRetirementCertificate
    )
import Cardano.Wallet.Unsafe
    ( unsafeRunExceptT )
import Control.Arrow
    ( second )
import Control.Exception
    ( evaluate )
import Control.Monad
    ( forM_, replicateM, unless )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Monad.Trans.Except
    ( runExceptT )
import Data.Bifunctor
    ( bimap )
import Data.Function
    ( on, (&) )
import Data.Functor
    ( ($>) )
import Data.Functor.Identity
    ( runIdentity )
import Data.Generics.Internal.VL.Lens
    ( set, view )
import Data.List.Extra
    ( nubOrd )
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
    , anyException
    , beforeAll
    , beforeWith
    , describe
    , it
    , shouldBe
    , shouldReturn
    , shouldThrow
    )
import Test.QuickCheck
    ( Positive (..)
    , Property
    , checkCoverage
    , classify
    , counterexample
    , cover
    , property
    , shrink
    , withMaxSuccess
    , (==>)
    )
import Test.QuickCheck.Monadic
    ( PropertyM, assert, monadicIO, monitor, pick, run )

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
newMemoryDBLayer :: IO (DBLayer IO)
newMemoryDBLayer = snd . snd <$> newMemoryDBLayer'

newMemoryDBLayer' :: IO (TVar [DBLog], (SqliteContext, DBLayer IO))
newMemoryDBLayer' = do
    logVar <- newTVarIO []
    (logVar, ) <$> newDBLayer (traceInTVarIO logVar) Nothing ti
  where
    ti = return . runIdentity . dummyTimeInterpreter

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
            (property .
                prop_multiple_putPoolRegistration_single_readPoolRegistration)
        it "prop_multiple_putPoolRetirement_single_readPoolRetirement"
            (property .
                prop_multiple_putPoolRetirement_single_readPoolRetirement)
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
    withMaxSuccess 10000 $ monadicIO (setup >> prop)
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
    epochOf' = runIdentity . ti . epochOf
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
    -> [(CertificatePublicationTime, PoolRegistrationCertificate)]
    -> Property
prop_poolRegistration DBLayer {..} entries =
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
    -> [(CertificatePublicationTime, PoolRetirementCertificate)]
    -> Property
prop_poolRetirement DBLayer {..} entries =
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
    -> [PoolRegistrationCertificate]
    -> Property
prop_multiple_putPoolRegistration_single_readPoolRegistration
    DBLayer {..} sharedPoolId certificatesManyPoolIds =
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
        :: [(CertificatePublicationTime, PoolRegistrationCertificate)]
    certificatePublications = publicationTimes `zip` certificates

    mExpectedCertificatePublication = certificatePublications
        & reverse
        & listToMaybe

    publicationTimes =
        [ CertificatePublicationTime (SlotNo sn) ii
        | sn <- [0 .. ]
        , ii <- [0 .. 3]
        ]

    certificates = set #poolId sharedPoolId <$> certificatesManyPoolIds

-- For the same pool, write /multiple/ pool retirement certificates to the
-- database and then read back the current retirement certificate, verifying
-- that the certificate returned is the last one to have been written.
--
prop_multiple_putPoolRetirement_single_readPoolRetirement
    :: DBLayer IO
    -> PoolId
    -> [PoolRetirementCertificate]
    -> Property
prop_multiple_putPoolRetirement_single_readPoolRetirement
    DBLayer {..} sharedPoolId certificatesManyPoolIds =
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
        :: [(CertificatePublicationTime, PoolRetirementCertificate)]
    certificatePublications = publicationTimes `zip` certificates

    mExpectedCertificatePublication = certificatePublications
        & reverse
        & listToMaybe

    publicationTimes =
        [ CertificatePublicationTime (SlotNo sn) ii
        | sn <- [0 .. ]
        , ii <- [0 .. 3]
        ]

    certificates = set #poolId sharedPoolId <$> certificatesManyPoolIds

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
    DBLayer {..} (SinglePoolCertificateSequence sharedPoolId certificates) =
        monadicIO (setup >> prop)
  where
    setup = run $ atomically cleanDB

    prop = do
        actualStatus <- run $ atomically $ do
            mapM_ (uncurry putCertificate) certificatePublications
            readPoolLifeCycleStatus sharedPoolId
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
        & fmap (traverse match)
        & catMaybes
        & listToMaybe

    certificatePublications :: [(CertificatePublicationTime, PoolCertificate)]
    certificatePublications = publicationTimes `zip` certificates

    publicationTimes =
        [ CertificatePublicationTime (SlotNo sn) ii
        | sn <- [0 .. 3]
        , ii <- [0 .. 3]
        ]

    putCertificate cpt = \case
        Registration cert ->
            putPoolRegistration cpt cert
        Retirement cert ->
            putPoolRetirement cpt cert

prop_rollbackRegistration
    :: DBLayer IO
    -> SlotNo
    -> [(CertificatePublicationTime, PoolRegistrationCertificate)]
    -> Property
prop_rollbackRegistration DBLayer{..} rollbackPoint entries =
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
    checkCoverage
        $ cover 4 (rollbackPoint == SlotNo 0)
            "rollbackPoint = slotMinBound"
        $ cover 4 (rollbackPoint > SlotNo 0)
            "rollbackPoint > slotMinBound"
        $ cover 2 (null expectedPublications)
            "length expectedPublications = 0"
        $ cover 4 (not (null expectedPublications))
            "length expectedPublications > 0"
        $ cover 4
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
        publicationTimes
            & drop (length certificates `div` 2)
            & fmap (view #slotNo)
            & listToMaybe
            & fromMaybe (SlotNo 0)

    allPublications
        :: [(CertificatePublicationTime, PoolRetirementCertificate)]
    allPublications = publicationTimes `zip` certificates

    expectedPublications
        :: [(CertificatePublicationTime, PoolRetirementCertificate)]
    expectedPublications =
        filter
            (\(CertificatePublicationTime slotId _, _) ->
                slotId <= rollbackPoint)
            allPublications

    publicationTimes :: [CertificatePublicationTime]
    publicationTimes =
        [ CertificatePublicationTime (SlotNo sn) ii
        | sn <- [0 .. 3]
        , ii <- [0 .. 3]
        ]

-- When we remove pools, check that:
--
-- 1. We only remove data relating to the specified pools.
-- 2. We do not remove data relating to other pools.
--
prop_removePools
    :: DBLayer IO
    -> [PoolCertificate]
    -> Property
prop_removePools
    DBLayer {..} certificates =
        monadicIO (setup >> prop)
  where
    setup = run $ atomically cleanDB

    prop = do
        -- Firstly, publish an arbitrary set of pool certificates:
        run $ atomically $ do
            mapM_ (uncurry putCertificate) certificatePublications
        -- Next, read the latest certificates for all pools:
        poolIdsWithRegCertsAtStart <- run poolIdsWithRegCerts
        poolIdsWithRetCertsAtStart <- run poolIdsWithRetCerts
        -- Next, remove a subset of the pools:
        run $ atomically $ removePools $ Set.toList poolsToRemove
        -- Finally, see which certificates remain:
        poolIdsWithRegCertsAtEnd <- run poolIdsWithRegCerts
        poolIdsWithRetCertsAtEnd <- run poolIdsWithRetCerts
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

    -- The complete set of all pools.
    pools = Set.fromList $ getPoolCertificatePoolId <$> certificates

    -- Divide the set of pools into two sets of approximately the same size.
    (poolsToRetain, poolsToRemove) = pools
        & Set.toList
        & L.splitAt (length pools `div` 2)
        & bimap Set.fromList Set.fromList

    certificatePublications
        :: [(CertificatePublicationTime, PoolCertificate)]
    certificatePublications = publicationTimes `zip` certificates

    publicationTimes :: [CertificatePublicationTime]
    publicationTimes =
        [ CertificatePublicationTime (SlotNo sn) ii
        | sn <- [0 .. 3]
        , ii <- [0 .. 3]
        ]

    putCertificate cpt = \case
        Registration cert ->
            putPoolRegistration cpt cert
        Retirement cert ->
            putPoolRetirement cpt cert

    poolIdsWithRegCerts =
        fmap (Set.fromList . fmap (view #poolId . snd) . catMaybes)
            <$> atomically $ mapM readPoolRegistration $ Set.toList pools

    poolIdsWithRetCerts =
        fmap (Set.fromList . fmap (view #poolId . snd) . catMaybes)
            <$> atomically $ mapM readPoolRetirement $ Set.toList pools

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
        assert (pools == (view #poolId <$> reverse entries))

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
    -> [SinglePoolCertificateSequence]
    -> ListSerializationMethod
    -> Property
prop_listRetiredPools_multiplePools_multipleCerts
    DBLayer {..} certificateSequences serializationMethod = checkCoverage
        -- Check the number of certificates:
        $ cover 2 (certificateCount == 0)
            "number of certificates: = 0"
        $ cover 2 (certificateCount > 0 && certificateCount <= 10)
            "number of certificates: > 0 && <= 10"
        $ cover 2 (certificateCount > 10 && certificateCount <= 100)
            "number of certificates: > 10 && <= 100"
        $ cover 2 (certificateCount > 100 && certificateCount <= 1000)
            "number of certificates: > 100 && <= 1000"
        -- Check the number of pools:
        $ cover 2 (poolCount == 0)
            "number of pools: = 0"
        $ cover 2 (poolCount > 0 && poolCount <= 10)
            "number of pools: > 0 && <= 10"
        $ cover 2 (poolCount > 10 && poolCount <= 100)
            "number of pools: > 10 && <= 100"
        $ monadicIO (setup >> prop)
  where
    setup = run $ atomically cleanDB

    prop = do
        run $ atomically $ do
            mapM_ (uncurry putCertificate) allPublicationsSerialized
        lifeCycleStatuses <- run $ atomically $ do
            mapM readPoolLifeCycleStatus allPoolIds
        let poolsMarkedToRetire = catMaybes $
                getPoolRetirementCertificate <$> lifeCycleStatuses
        let epochsToTest =
                EpochNo minBound :
                EpochNo maxBound :
                L.nub (view #retiredIn <$> poolsMarkedToRetire)
        forM_ epochsToTest $ \currentEpoch -> do
            let retiredPoolsExpected = filter
                    ((<= currentEpoch) . view #retiredIn)
                    (poolsMarkedToRetire)
            retiredPoolsActual <-
                run $ atomically $ listRetiredPools currentEpoch
            assert $ (==)
                (Set.fromList retiredPoolsActual)
                (Set.fromList retiredPoolsExpected)

    certificateCount = length allCertificatesSerialized
    poolCount = length certificateSequences

    allCertificatesSerialized :: [PoolCertificate]
    allCertificatesSerialized = serializeLists serializationMethod
        (getSinglePoolCertificateSequence <$> certificateSequences)

    allPublicationsSerialized
        :: [(CertificatePublicationTime, PoolCertificate)]
    allPublicationsSerialized =
        publicationTimes `zip` allCertificatesSerialized

    allPoolIds :: [PoolId]
    allPoolIds = getSinglePoolId <$> certificateSequences

    publicationTimes :: [CertificatePublicationTime]
    publicationTimes =
        [ CertificatePublicationTime (SlotNo sn) ii
        | sn <- [0 .. 3]
        , ii <- [0 .. 3]
        ]

    putCertificate cpt = \case
        Registration cert ->
            putPoolRegistration cpt cert
        Retirement cert ->
            putPoolRetirement cpt cert

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
    checkCoverage
        $ cover 10 (regTime > retTime)
            "registration cert time > retirement cert time"
        $ cover 10 (regTime < retTime)
            "registration cert time < retirement cert time"
        $ cover 2 (regTime == retTime)
            "registration cert time = retirement cert time"
        $ property prop
  where
    prop
        | regTime > retTime =
            -- A re-registration always /supercedes/ a prior retirement.
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
    checkCoverage
        $ cover 10 (isJust maybeRetData)
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
        checkCoverage
            $ cover 2 (null certificates)
                "length (all certificates) = 0"
            $ cover 2 (length certificates == 1)
                "length (all certificates) = 1"
            $ cover 2 (length certificates > 1)
                "length (all certificates) > 1"

            $ cover 2 (null registrationCertificates)
                "length (registration certificates) = 0"
            $ cover 2 (length registrationCertificates == 1)
                "length (registration certificates) = 1"
            $ cover 2 (length registrationCertificates > 1)
                "length (registration certificates) > 1"

            $ cover 2 (null retirementCertificates)
                "length (retirement certificates) = 0"
            $ cover 2 (length retirementCertificates == 1)
                "length (retirement certificates) = 1"
            $ cover 2 (length retirementCertificates > 1)
                "length (retirement certificates) > 1"

            $ cover 50 (not (null shrunkenSequences))
                "length (shrunken sequences) > 0"

            $ all isValidSinglePoolCertificateSequence $ s : shrunkenSequences
  where
    shrunkenSequences = shrink s

    registrationCertificates = catMaybes
        (getRegistrationCertificate <$> certificates)
    retirementCertificates = catMaybes
        (getRetirementCertificate <$> certificates)
    getRegistrationCertificate = \case
        Registration cert -> Just cert
        Retirement _ -> Nothing
    getRetirementCertificate = \case
        Registration _ -> Nothing
        Retirement cert -> Just cert

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
    epochOf' = runIdentity . ti . epochOf
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
