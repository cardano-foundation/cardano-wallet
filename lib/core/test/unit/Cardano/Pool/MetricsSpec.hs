{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Cardano.Pool.MetricsSpec
    ( spec
    , arbitraryChunks
    , RegistrationsTest
    ) where

import Prelude

import Cardano.BM.Data.Tracer
    ( nullTracer )
import Cardano.Pool.DB
    ( DBLayer (..) )
import Cardano.Pool.DB.MVar
    ( newDBLayer )
import Cardano.Pool.Metadata
    ( StakePoolMetadata (..), envVarMetadataRegistry, sameStakePoolMetadata )
import Cardano.Pool.Metrics
    ( Block (..)
    , ErrListStakePools (..)
    , StakePoolLayer (..)
    , StakePoolLog (..)
    , associateMetadata
    , combineMetrics
    , monitorStakePools
    , newStakePoolLayer
    )
import Cardano.Pool.Ranking
    ( EpochConstants (..), unsafeMkNonNegative, unsafeMkPositive )
import Cardano.Wallet.DummyTarget.Primitive.Types
    ( genesisParameters )
import Cardano.Wallet.Network
    ( Cursor
    , ErrGetBlock (..)
    , ErrNetworkUnavailable (..)
    , NetworkLayer (..)
    , NextBlocksResult (..)
    )
import Cardano.Wallet.Primitive.Types
    ( ActiveSlotCoefficient (..)
    , BlockHeader (..)
    , BlockchainParameters (..)
    , Coin (..)
    , EpochLength (..)
    , EpochNo
    , Hash (..)
    , PoolId (..)
    , PoolOwner (..)
    , PoolRegistrationCertificate (..)
    , SlotId (..)
    , flatSlot
    , flatSlot
    , fromFlatSlot
    , slotParams
    , slotSucc
    )
import Cardano.Wallet.Unsafe
    ( unsafeFromText, unsafeRunExceptT )
import Control.Concurrent.Async
    ( race_ )
import Control.Concurrent.MVar
    ( MVar, modifyMVar, newEmptyMVar, newMVar, takeMVar, tryPutMVar )
import Control.Monad
    ( replicateM )
import Control.Monad.Trans.Class
    ( lift )
import Control.Monad.Trans.Except
    ( ExceptT (..), runExceptT )
import Control.Monad.Trans.State.Strict
    ( StateT, evalStateT, get, modify' )
import Data.Functor
    ( ($>) )
import Data.Map.Strict
    ( Map )
import Data.Maybe
    ( catMaybes )
import Data.Quantity
    ( Quantity (..) )
import Data.Text.Class
    ( toText )
import Data.Word
    ( Word32, Word64 )
import System.Environment
    ( setEnv )
import Test.Hspec
    ( Spec, describe, it, shouldBe, shouldContain, shouldSatisfy )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , NonEmptyList (..)
    , Positive (..)
    , Property
    , checkCoverage
    , choose
    , counterexample
    , cover
    , elements
    , frequency
    , property
    , scale
    , vectorOf
    , (===)
    )
import Test.QuickCheck.Arbitrary.Generic
    ( genericArbitrary, genericShrink )
import Test.QuickCheck.Monadic
    ( assert, monadicIO, monitor, run )
import Test.Utils.Trace
    ( captureLogging )

import qualified Data.ByteString.Char8 as B8
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

spec :: Spec
spec = do
    describe "combineMetrics" $ do
        it "pools with no entry for productions are included"
            $ property prop_combineDefaults

        it "it fails if a block-producer is not in the stake distr"
            $ checkCoverage
            $ property prop_combineIsLeftBiased

    describe "monitorStakePools" $ do
        it "records all stake pool registrations in the database"
            $ property prop_trackRegistrations

    describe "listStakePools" $ do
        it "can't list on empty database" test_emptyDatabaseNotSynced
        it "report correct progress when not synced" test_notSyncedProgress

    associateMetadataSpec

{-------------------------------------------------------------------------------
                                Properties
-------------------------------------------------------------------------------}

prop_combineDefaults
    :: Map PoolId (Quantity "lovelace" Word64)
    -> Property
prop_combineDefaults mStake = do
    combineMetrics mStake Map.empty Map.empty
    ===
    Right (Map.map (, Quantity 0, 0) mStake)

-- | it fails if a block-producer or performance is not in the stake distr
prop_combineIsLeftBiased
    :: Map (LowEntropy PoolId) (Quantity "lovelace" Word64)
    -> Map (LowEntropy PoolId) (Quantity "block" Word64)
    -> Map (LowEntropy PoolId) Double
    -> Property
prop_combineIsLeftBiased mStake_ mProd_ mPerf_ =
    let
        shouldLeft = or
            [ not . Map.null $ Map.difference mProd mStake
            , not . Map.null $ Map.difference mPerf mStake
            ]
    in
    cover 10 shouldLeft "A pool without stake produced"
    $ cover 50 (not shouldLeft) "Successfully combined the maps"
    $ case combineMetrics mStake mProd mPerf of
        Left _ ->
            shouldLeft === True
        Right x ->
            Map.map (\(a,_,_) -> a) x === mStake
  where
    mStake = Map.mapKeys getLowEntropy mStake_
    mProd  = Map.mapKeys getLowEntropy mProd_
    mPerf  = Map.mapKeys getLowEntropy mPerf_
{-# HLINT ignore prop_combineIsLeftBiased "Use ||" #-}

-- | A list of chunks of blocks to be served up by the mock network layer.
newtype RegistrationsTest = RegistrationsTest
    { getRegistrationsTest :: [[Block]] }
    deriving (Show, Eq)

-- | Assert that 'monitorStakePools' records all stake pool registrations in the
-- database.
--
-- The idea is to run 'monitorStakePools' with an in-memory database and mock
-- network layer.
--
-- The mock network layer serves up chunks of blocks from the testcase, which
-- contain registration certificates
--
-- It then asserts that the registration info in the database matches that of
-- the blocks of the test case.
prop_trackRegistrations :: RegistrationsTest -> Property
prop_trackRegistrations test = monadicIO $ do
    let expected = getExpected test
    let numRegistrations = getNumRegistrations test

    (logs, registrations) <- run $ captureLogging $ \tr -> do
        done <- newEmptyMVar
        nl <- newMockNetworkLayer done test
        db@DBLayer{..} <- newDBLayer
        race_ (takeMVar done) (monitorStakePools tr nl db)

        let pids = poolId <$> expected
        atomically $ L.sort . catMaybes <$> mapM readPoolRegistration pids

    let numDiscoveryLogs = length (filter isDiscoveryMsg logs)

    monitor $ counterexample $ "Actual registrations:   " <> show registrations
    monitor $ counterexample $ "Expected registrations: " <> show expected
    monitor $ counterexample $ "# Discovery log msgs:   " <> show numDiscoveryLogs
    monitor $ counterexample $ "# Registration certs:   " <> show numRegistrations
    monitor $ counterexample $ "Logs:\n" <>
        unlines (map (("  " ++) . T.unpack . toText) logs)

    assert (registrations == expected)
    assert (numDiscoveryLogs == numRegistrations)
  where
    getExpected :: RegistrationsTest -> [PoolRegistrationCertificate]
    getExpected =
        fmap (\p -> p { poolOwners = L.sortOn toText (poolOwners p) })
        . L.sort
        . concatMap poolRegistrations
        . mconcat
        . getRegistrationsTest

    isDiscoveryMsg :: StakePoolLog -> Bool
    isDiscoveryMsg (MsgStakePoolRegistration _) = True
    isDiscoveryMsg _ = False

    getNumRegistrations :: RegistrationsTest -> Int
    getNumRegistrations =
        sum
        . map (sum . map (length . poolRegistrations))
        . getRegistrationsTest

    newMockNetworkLayer
        :: MVar ()
        -> RegistrationsTest
        -> IO (NetworkLayer IO RegistrationsTest Block)
    newMockNetworkLayer done (RegistrationsTest blocks) = do
        blockVar <- newMVar blocks
        let popChunk = modifyMVar blockVar $ \case
                [] -> pure ([], Nothing)
                (b:bs) -> pure (bs, Just b)
        pure $ mockNetworkLayer
            { nextBlocks = \cursor@(Cursor blkH) -> ExceptT $ popChunk >>= \case
                    Just bs -> pure
                        $ Right
                        $ RollForward cursor blkH bs
                    Nothing -> do
                        tryPutMVar done () $> (Left
                            $ ErrGetBlockNetworkUnreachable
                            $ ErrNetworkInvalid "The test case has finished")
            , initCursor =
                pure . const (Cursor header0)
            , stakeDistribution =
                pure (0, mempty)
            , networkTip =
                pure header0
            -- These params are basically unused and completely arbitrary.
            , staticBlockchainParameters =
                ( block0
                , mockBlockchainParameters { getEpochStability = Quantity 2 }
                )
            }

data instance Cursor RegistrationsTest = Cursor BlockHeader

test_emptyDatabaseNotSynced :: IO ()
test_emptyDatabaseNotSynced = do
    setEnv envVarMetadataRegistry "-"
    db@DBLayer{..} <- newDBLayer
    -- NOTE The directory below isn't use, the test should fail much before
    let spl = newStakePoolLayer nullTracer getEpConsts db nl "/dev/null"
    res <- runExceptT $ listStakePools spl
    case res of
        Left (ErrMetricsIsUnsynced (Quantity p)) -> p `shouldBe` toEnum 0
        _ -> fail $ "got something else than expected: " <> show res
  where
    nl = mockNetworkLayer
        { networkTip =
            pure header0
        , staticBlockchainParameters =
            ( block0
            -- v arbitrary but defined.
            , mockBlockchainParameters { getEpochLength = EpochLength 10 }
            )
        }

test_notSyncedProgress :: IO ()
test_notSyncedProgress = do
    setEnv envVarMetadataRegistry "-"
    db@DBLayer{..} <- newDBLayer
    atomically $ unsafeRunExceptT $
        putPoolProduction prodTip (PoolId "Pool & The Gang")
    -- NOTE The directory below isn't use, the test should fail much before
    let spl = newStakePoolLayer nullTracer getEpConsts db nl "/dev/null"
    res <- runExceptT $ listStakePools spl
    case res of
        Left (ErrMetricsIsUnsynced (Quantity p)) -> p `shouldBe` toEnum 33
        _ -> fail $ "got something else than expected: " <> show res
  where
    nodeTip = header0 { blockHeight = Quantity 42 }
    prodTip = header0 { blockHeight = Quantity 14 }
    nl = mockNetworkLayer
        { networkTip =
            pure nodeTip
        , staticBlockchainParameters =
            ( block0
            -- v arbitrary but defined.
            , mockBlockchainParameters { getEpochLength = EpochLength 10 }
            )
        }

{-------------------------------------------------------------------------------
                                 Mock Data
-------------------------------------------------------------------------------}

-- A mock network layer placeholder that can be re-used across tests. Functions
-- required by a particular test can be stubbed out or mocked as necessary.
mockNetworkLayer :: NetworkLayer m t b
mockNetworkLayer = NetworkLayer
    { nextBlocks =
        \_ -> error "mockNetworkLayer: nextBlocks"
    , findIntersection =
        \_ -> error "mockNetworkLayer: findIntersection"
    , initCursor =
        \_ -> error "mockNetworkLayer: initCursor"
    , cursorSlotId =
        \_ -> error "mockNetworkLayer: cursorSlotId"
    , networkTip =
        error "mockNetworkLayer: networkTip"
    , postTx =
        \_ -> error "mockNetworkLayer: postTx"
    , staticBlockchainParameters =
        ( error "mockNetworkLayer: genesis block"
        , mockBlockchainParameters
        )
    , stakeDistribution =
        error "mockNetworkLayer: stakeDistribution"
    , getAccountBalance =
        \_ -> error "mockNetworkLayer: getAccountBalance"
    }

mockBlockchainParameters :: BlockchainParameters
mockBlockchainParameters = BlockchainParameters
    { getGenesisBlockHash = error "mockBlockchainParameters: getGenesisBlockHash"
    , getGenesisBlockDate = error "mockBlockchainParameters: getGenesisBlockDate"
    , getFeePolicy = error "mockBlockchainParameters: getFeePolicy"
    , getSlotLength = error "mockBlockchainParameters: getSlotLength"
    , getEpochLength = error "mockBlockchainParameters: getEpochLength"
    , getTxMaxSize = error "mockBlockchainParameters: getTxMaxSize"
    , getEpochStability = error "mockBlockchainParameters: getEpochStability"
    , getActiveSlotCoefficient = error "mockBlockchainParameters: getActiveSlotCoefficient"
    }

header0 :: BlockHeader
header0 = BlockHeader
    (SlotId 0 0)
    (Quantity 0)
    (Hash $ B8.replicate 32 '0')
    (Hash $ B8.replicate 32 '0')

block0 :: Block
block0 = Block header0 (PoolId "") []

getEpConsts :: EpochNo -> Quantity "lovelace" Word64 -> EpochConstants
getEpConsts _ _ = EpochConstants
    { leaderStakeInfluence = unsafeMkNonNegative 0
    , desiredNumberOfPools = unsafeMkPositive 100
    , totalRewards = Quantity 1000
    }

{-------------------------------------------------------------------------------
                                 Arbitrary
-------------------------------------------------------------------------------}

instance Arbitrary BlockHeader where
    arbitrary = BlockHeader
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
    shrink = genericShrink

instance Arbitrary SlotId where
    arbitrary = fromFlatSlot epochLength <$> arbitrary
    shrink sl = fromFlatSlot epochLength <$> shrink (flatSlot epochLength sl)

-- | Epoch length used to generate arbitrary @SlotId@
epochLength :: EpochLength
epochLength = EpochLength 50

instance Arbitrary (Hash tag) where
    shrink _  = []
    arbitrary = Hash . B8.pack
        <$> vectorOf 32 (elements (['a'..'f'] ++ ['0'..'9']))

instance Arbitrary Block where
   arbitrary = genericArbitrary
   shrink = genericShrink

instance Arbitrary (Quantity "block" Word32) where
    arbitrary = Quantity . fromIntegral <$> (arbitrary @Word32)
    shrink (Quantity x) = map Quantity $ shrink x

instance Arbitrary (Quantity "block" Word64) where
    arbitrary = Quantity . fromIntegral <$> (arbitrary @Word32)
    shrink (Quantity x) = map Quantity $ shrink x

instance Arbitrary (Quantity "lovelace" Word64) where
    arbitrary = Quantity . fromIntegral . unLovelace <$> (arbitrary @Lovelace)
    shrink (Quantity x) = map Quantity $ shrink x

instance Arbitrary ActiveSlotCoefficient where
    arbitrary = ActiveSlotCoefficient <$> choose (0.001, 1.0)
    shrink (ActiveSlotCoefficient f) = ActiveSlotCoefficient
        <$> filter (\f' -> f' > 0.001 && f' <= 1.0) (shrink f)

-- TODO: Move to a shared location for Arbitrary newtypes
newtype Lovelace = Lovelace { unLovelace :: Word64 }
instance Arbitrary Lovelace where
    shrink (Lovelace x) = map Lovelace $ shrink x
    arbitrary = do
        n <- choose (0, 100)
        Lovelace <$> frequency
            [ (8, return n)
            , (2, choose (minLovelace, maxLovelace))
            ]
      where
        minLovelace = fromIntegral . getCoin $ minBound @Coin
        maxLovelace = fromIntegral . getCoin $ maxBound @Coin

newtype LowEntropy a = LowEntropy { getLowEntropy :: a } deriving (Eq, Show, Ord)

instance Arbitrary (LowEntropy PoolId) where
    shrink _  = []
    arbitrary = LowEntropy . PoolId . B8.pack
        <$> elements [ "ares", "athena", "hades", "hestia", "nemesis" ]

instance Arbitrary PoolId where
    arbitrary = fmap (PoolId . B8.pack) (vectorOf 32 arbitrary)

instance Arbitrary PoolOwner where
    shrink _  = []
    arbitrary = PoolOwner . B8.singleton <$> elements ['a'..'e']

instance Arbitrary PoolRegistrationCertificate where
    shrink (PoolRegistrationCertificate p o m c) =
        (\(p', NonEmpty o') -> PoolRegistrationCertificate p' o' m c)
        <$> shrink (p, NonEmpty o)
    arbitrary = PoolRegistrationCertificate
        <$> arbitrary
        <*> fmap (L.nub . getNonEmpty) (scale (`mod` 3) arbitrary)
        <*> fmap toEnum (choose (0, 100))
        <*> fmap Quantity arbitrary

instance Arbitrary RegistrationsTest where
    shrink (RegistrationsTest xs) = RegistrationsTest <$> shrink xs
    arbitrary = do
        Positive n <- arbitrary
        chunks <- arbitraryChunks =<< evalStateT (replicateM n genBlock) state0
        pure $ RegistrationsTest chunks
      where
        state0 :: (Hash "BlockHeader", SlotId)
        state0 = (headerHash header0, slotId header0)

        genBlock :: StateT (Hash "BlockHeader", SlotId) Gen Block
        genBlock = Block
            <$> genBlockHeader
            <*> lift arbitrary
            <*> lift arbitrary

        genBlockHeader :: StateT (Hash "BlockHeader", SlotId) Gen BlockHeader
        genBlockHeader = do
            (parentHeaderHash, _) <- get
            (headerHash, slotId) <- nextState
            let blockHeight = Quantity $ fromIntegral $ flatSlot ep slotId
            pure BlockHeader
                { slotId
                , blockHeight
                , headerHash
                , parentHeaderHash
                }
          where
            ep = getEpochLength genesisParameters

        nextState :: (s ~ (Hash "BlockHeader", SlotId)) => StateT s Gen s
        nextState = do
            nextHeaderHash <- lift arbitrary
            modify' (\(_, sl) -> (nextHeaderHash, slotSucc sp sl)) >> get
          where
            sp = slotParams genesisParameters

arbitraryChunks :: [a] -> Gen [[a]]
arbitraryChunks [] = pure []
arbitraryChunks xs = do
    n <- choose (1, length xs)
    rest <- arbitraryChunks (drop n xs)
    pure $ take n xs : rest

{-------------------------------------------------------------------------------
                                   Unit tests
-------------------------------------------------------------------------------}

associateMetadataSpec :: Spec
associateMetadataSpec = describe "associateMetadata" $ do
    let mkMetadata owner tckr = StakePoolMetadata
            { owner = owner
            , ticker = unsafeFromText tckr
            , name = tckr
            , description = Just tckr
            , homepage = tckr
            , pledgeAddress = tckr
            }

    it "works in sunny day case" $ do
        let owner = PoolOwner "a"
        let md = mkMetadata owner "AAAA"
        let pid = PoolId "1"

        let res = associateMetadata
                [(pid, [owner])]
                [(owner, Just md)]

        res `shouldBe` [(MsgMetadataUsing pid owner md, Just md)]

    it "missing metadata" $ do
        let res = associateMetadata
                [(PoolId "1", [PoolOwner "a"])]
                [(PoolOwner "a", Nothing)]

        res `shouldBe` [(MsgMetadataMissing (PoolId "1"), Nothing)]

    it "duplicate metadata" $ do
        let mda = mkMetadata (PoolOwner "a") "AAAA"
        let mdb = mkMetadata (PoolOwner "b") "AAAA"
        let [(msg, res)] = associateMetadata
                [(PoolId "1", [PoolOwner "a", PoolOwner "b"])]
                [ (PoolOwner "a", Just mda)
                , (PoolOwner "b", Just mdb)
                ]

        case msg of
            MsgMetadataUsing pid owner md -> do
                pid `shouldBe` PoolId "1"
                [PoolOwner "a", PoolOwner "b"] `shouldContain` [owner]
                md `shouldSatisfy` sameStakePoolMetadata mda
                md `shouldSatisfy` sameStakePoolMetadata mdb
            _ -> fail $ "wrong log message " ++ show msg

        fmap (sameStakePoolMetadata mda) res `shouldBe` Just True

    it "inconsistent metadata" $ do
        let mda = mkMetadata (PoolOwner "a") "AAAA"
        let mdb = mkMetadata (PoolOwner "b") "BBBB"

        let res = associateMetadata
                [(PoolId "1", [PoolOwner "a", PoolOwner "b"])]
                [ (PoolOwner "a", Just mda)
                , (PoolOwner "b", Just mdb)
                ]

        res `shouldBe`
            [( MsgMetadataMultiple (PoolId "1")
                [(PoolOwner "a", mda), (PoolOwner "b", mdb)], Nothing)]
