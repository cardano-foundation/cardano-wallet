{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Cardano.Wallet.DB.Properties
    ( properties
    ) where

import Prelude

import Cardano.Wallet.DB
    ( DBLayer (..)
    , ErrNoSuchWallet (..)
    , ErrWalletAlreadyExists (..)
    , SparseCheckpointsConfig (..)
    , cleanDB
    , defaultSparseCheckpointsConfig
    , gapSize
    , sparseCheckpoints
    )
import Cardano.Wallet.DB.Arbitrary
    ( GenState
    , GenTxHistory (..)
    , InitialCheckpoint (..)
    , KeyValPairs (..)
    , MockChain (..)
    )
import Cardano.Wallet.DB.Pure.Implementation
    ( filterTxHistory )
import Cardano.Wallet.DummyTarget.Primitive.Types
    ( dummyGenesisParameters )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( ShelleyKey (..) )
import Cardano.Wallet.Primitive.Model
    ( Wallet, applyBlock, currentTip )
import Cardano.Wallet.Primitive.Types
    ( BlockHeader (..)
    , ChainPoint (..)
    , GenesisParameters
    , ProtocolParameters
    , Slot
    , SlotId (..)
    , SlotNo (..)
    , SortOrder (..)
    , WalletId (..)
    , WalletMetadata (..)
    , WithOrigin (..)
    , chainPointFromBlockHeader
    , toSlot
    , wholeRange
    )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Cardano.Wallet.Primitive.Types.Tx
    ( Direction (..)
    , TransactionInfo (..)
    , Tx (..)
    , TxMeta (..)
    , TxStatus (..)
    , isPending
    , toTxHistory
    )
import Cardano.Wallet.Unsafe
    ( unsafeRunExceptT )
import Cardano.Wallet.Util
    ( ShowFmt (..) )
import Control.Monad
    ( forM, forM_, void )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Monad.Trans.Class
    ( lift )
import Control.Monad.Trans.Except
    ( ExceptT, mapExceptT, runExceptT )
import Control.Monad.Trans.State.Strict
    ( evalStateT, get, modify' )
import Data.Bifunctor
    ( bimap )
import Data.Function
    ( (&) )
import Data.Functor.Identity
    ( Identity (..) )
import Data.Generics.Internal.VL.Lens
    ( view, (^.) )
import Data.Generics.Labels
    ()
import Data.List
    ( unfoldr )
import Data.Maybe
    ( catMaybes, isNothing, mapMaybe )
import Data.Quantity
    ( Quantity (..) )
import Data.Set
    ( Set )
import Data.Word
    ( Word32, Word64 )
import Fmt
    ( Buildable, blockListF, pretty )
import Test.Hspec
    ( SpecWith, describe, it, shouldBe, shouldReturn )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , Property
    , checkCoverage
    , choose
    , conjoin
    , counterexample
    , cover
    , elements
    , forAll
    , label
    , property
    , suchThat
    , (.&&.)
    , (===)
    , (==>)
    )
import Test.QuickCheck.Monadic
    ( PropertyM, assert, monadicIO, monitor, pick, run )
import UnliftIO.Async
    ( forConcurrently_ )

import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

properties
    :: (GenState s, Eq s)
    => SpecWith (DBLayer IO s ShelleyKey)
properties = do
    describe "Extra Properties about DB initialization" $ do
        it "createWallet . listWallets yields expected results"
            (property . prop_createListWallet)
        it "creating same wallet twice yields an error"
            (property . prop_createWalletTwice)
        it "removing the same wallet twice yields an error"
            (property . prop_removeWalletTwice)

    describe "put . read yields a result" $ do
        it "Checkpoint" $
            property . prop_readAfterPut
                (\DBLayer{..} a0 -> mapExceptT atomically . putCheckpoint a0)
                (\DBLayer{..} -> atomically . readCheckpoint)
        it "Wallet Metadata" $
            property . prop_readAfterPut
                (\DBLayer{..} a0 -> mapExceptT atomically . putWalletMeta a0)
                (\DBLayer{..} -> atomically . readWalletMeta)
        it "Tx History" $
            property . prop_readAfterPut
                putTxHistoryF
                readTxHistoryF
        it "Private Key" $
            property . prop_readAfterPut
                (\DBLayer{..} a0 -> mapExceptT atomically . putPrivateKey a0)
                (\DBLayer{..} -> atomically . readPrivateKey)

    describe "getTx properties" $ do
        it "can read after putting tx history for valid tx id" $
            property . prop_getTxAfterPutValidTxId
        it "cannot read after putting tx history for invalid tx id" $
            property . prop_getTxAfterPutInvalidTxId
        it "cannot read after putting tx history for invalid wallet id" $
            property . prop_getTxAfterPutInvalidWalletId

    describe "can't put before wallet exists" $ do
        it "Checkpoint" $
            property . prop_putBeforeInit
                (\DBLayer{..} a0 -> mapExceptT atomically . putCheckpoint a0)
                (\DBLayer{..} -> atomically . readCheckpoint)
                Nothing
        it "Wallet Metadata" $
            property . prop_putBeforeInit
                (\DBLayer{..} a0 -> mapExceptT atomically . putWalletMeta a0)
                (\DBLayer{..} -> atomically . readWalletMeta)
                Nothing
        it "Tx History" $
            property . prop_putBeforeInit
                putTxHistoryF
                readTxHistoryF
                (pure mempty)
        it "Private Key" $
            property . prop_putBeforeInit
                (\DBLayer{..} a0 -> mapExceptT atomically . putPrivateKey a0)
                (\DBLayer{..} -> atomically . readPrivateKey)
                Nothing

    describe "put doesn't affect other resources" $ do
        it "Checkpoint vs Wallet Metadata & Tx History & Private Key" $
            property . prop_isolation
                (\DBLayer{..} a0 -> mapExceptT atomically . putCheckpoint a0)
                (\DBLayer{..} -> atomically . readWalletMeta)
                readTxHistoryF
                (\DBLayer{..} -> atomically . readPrivateKey)
        it "Wallet Metadata vs Tx History & Checkpoint & Private Key" $
            property . prop_isolation
                (\DBLayer{..} a0 -> mapExceptT atomically . putWalletMeta a0)
                readTxHistoryF
                (\DBLayer{..} -> atomically . readCheckpoint)
                (\DBLayer{..} -> atomically . readPrivateKey)
        it "Tx History vs Checkpoint & Wallet Metadata & Private Key" $
            property . prop_isolation
                putTxHistoryF
                (\DBLayer{..} -> atomically . readCheckpoint)
                (\DBLayer{..} -> atomically . readWalletMeta)
                (\DBLayer{..} -> atomically . readPrivateKey)

    describe "can't read after delete" $ do
        it "Checkpoint" $
            property . prop_readAfterDelete
                (\DBLayer{..} -> atomically . readCheckpoint)
                Nothing
        it "Wallet Metadata" $
            property . prop_readAfterDelete
                (\DBLayer{..} -> atomically . readWalletMeta)
                Nothing
        it "Tx History" $
            property . prop_readAfterDelete
                readTxHistoryF
                (pure mempty)
        it "Private Key" $
            property . prop_readAfterDelete
                (\DBLayer{..} -> atomically . readPrivateKey)
                Nothing

    describe "sequential puts replace values in order" $ do
        it "Checkpoint" $
            checkCoverage . prop_sequentialPut
                (\DBLayer{..} a0 -> mapExceptT atomically . putCheckpoint a0)
                (\DBLayer{..} -> atomically . readCheckpoint)
                lrp
        it "Wallet Metadata" $
            checkCoverage . prop_sequentialPut
                (\DBLayer{..} a0 -> mapExceptT atomically . putWalletMeta a0)
                (\DBLayer{..} -> atomically . readWalletMeta)
                lrp
        it "Tx History" $
            checkCoverage . prop_sequentialPut
                putTxHistoryF
                readTxHistoryF
                sortedUnions
        it "Private Key" $
            checkCoverage . prop_sequentialPut
                (\DBLayer{..} a0 -> mapExceptT atomically . putPrivateKey a0)
                (\DBLayer{..} -> atomically . readPrivateKey)
                lrp

    describe "parallel puts replace values in _any_ order" $ do
        it "Checkpoint" $
            checkCoverage . prop_parallelPut
                (\DBLayer{..} a0 -> mapExceptT atomically . putCheckpoint a0)
                (\DBLayer{..} -> atomically . readCheckpoint)
                (length . lrp @Maybe)
        it "Wallet Metadata" $
            checkCoverage . prop_parallelPut
                (\DBLayer{..} a0 -> mapExceptT atomically . putWalletMeta a0)
                (\DBLayer{..} -> atomically . readWalletMeta)
                (length . lrp @Maybe)
        it "Tx History" $
            checkCoverage . prop_parallelPut
                putTxHistoryF
                readTxHistoryF
                (length . sortedUnions)
        it "Private Key" $
            checkCoverage . prop_parallelPut
                (\DBLayer{..} a0 -> mapExceptT atomically . putPrivateKey a0)
                (\DBLayer{..} -> atomically . readPrivateKey)
                (length . lrp @Maybe)

    describe "rollback" $ do
        it "Can rollback to any arbitrary known checkpoint"
            (property . prop_rollbackCheckpoint)
        it "Correctly re-construct tx history on rollbacks"
            (checkCoverage . prop_rollbackTxHistory)

    describe "sparseCheckpoints" $ do
        it "k=2160, h=42" $ \_ -> do
            let cfg = SparseCheckpointsConfig
                    { edgeSize = 10
                    , epochStability = 2160
                    }
            let h = Quantity 42

            -- First unstable block: 0
            sparseCheckpoints cfg h `shouldBe`
                [ 0
                , 32,33,34,35,36,37,38,39,40,41 -- Short-term checkpoints
                , 42 -- Tip
                ]

        it "k=2160, h=2414" $ \_ -> do
            let cfg = SparseCheckpointsConfig
                    { edgeSize = 10
                    , epochStability = 2160
                    }
            let h = Quantity 2714
            -- First unstable block: 554
            sparseCheckpoints cfg h `shouldBe`
                [ 0
                , 720, 1440, 2160               -- Long-term checkpoints

                , 2704, 2705, 2706, 2707, 2708  -- Short-term checkpoints
                , 2709, 2710, 2711, 2712, 2713  -- edgeSize = 10

                , 2714  -- Tip
                ]

        it "k=2160, h=2414" $ \_ -> do
            let cfg = SparseCheckpointsConfig
                    { edgeSize = 0
                    , epochStability = 2160
                    }
            let h = Quantity 2714
            -- First unstable block: 554
            sparseCheckpoints cfg h `shouldBe`
                [ 0
                , 720, 1440, 2160               -- Long-term checkpoints
                , 2714  -- Tip
                ]


        it "The tip is always a checkpoint" $ \_ ->
            property prop_sparseCheckpointTipAlwaysThere

        it "There's at least (min h edgeSize) checkpoints" $ \_ ->
            property prop_sparseCheckpointMinimum

        it "∀ cfg. sparseCheckpoints (cfg { edgeSize = 0 }) ⊆ sparseCheckpoints cfg" $ \_ ->
            property prop_sparseCheckpointEdgeSize0

        it "Checkpoints are eventually stored in a sparse manner" $ \_ ->
            property prop_checkpointsEventuallyEqual

-- | Wrap the result of 'readTxHistory' in an arbitrary identity Applicative
readTxHistoryF
    :: Functor m
    => DBLayer m s ShelleyKey
    -> WalletId
    -> m (Identity GenTxHistory)
readTxHistoryF DBLayer{..} wid =
    (Identity . GenTxHistory . fmap toTxHistory)
        <$> atomically (readTxHistory wid Nothing Descending wholeRange Nothing)

putTxHistoryF
    :: DBLayer m s ShelleyKey
    -> WalletId
    -> GenTxHistory
    -> ExceptT ErrNoSuchWallet m ()
putTxHistoryF DBLayer{..} wid =
    mapExceptT atomically . putTxHistory wid . unGenTxHistory

{-------------------------------------------------------------------------------
                                       Utils
-------------------------------------------------------------------------------}

-- | Keep only the
-- (L)ast (R)ecently (P)ut entry
lrp :: (Applicative f, Ord k) => [(k, v)] -> [f v]
lrp =
    fmap snd
    . L.sortOn fst
    . Map.toList
    . foldl (\m (k, v) -> Map.insert k (pure v) m) mempty

-- | Keep the unions (right-biaised) of all entry
unions :: (Monoid v, Ord k) => [(k, v)] -> [Identity v]
unions =
    fmap Identity
    . Map.elems
    . foldl (\m (k, v) -> Map.unionWith (<>) (Map.fromList [(k, v)]) m) mempty

-- | Keep the unions (right-biased) of all transactions, and sort them in the
-- default order for readTxHistory.
sortedUnions :: Ord k => [(k, GenTxHistory)] -> [Identity GenTxHistory]
sortedUnions = map (Identity . sort' . runIdentity) . unions
  where
    sort' = GenTxHistory
      . filterTxHistory Nothing Descending wholeRange
      . unGenTxHistory

-- | Execute an action once per key @k@ present in the given list
once :: (Ord k, Monad m) => [(k,v)] -> ((k,v) -> m a) -> m [a]
once xs action = fmap catMaybes $ flip evalStateT mempty $
    forM xs $ \(k, v) -> do
        s <- get
        modify' (Set.insert k)
        if Set.member k s
            then pure Nothing
            else Just <$> lift (action (k, v))

-- | Like 'once', but discards the result
once_ :: (Ord k, Monad m) => [(k,v)] -> ((k,v) -> m a) -> m ()
once_ xs = void . once xs

-- | Filter a transaction list according to the given predicate, returns their
-- ids.
filterTxs
    :: (TxMeta -> Bool)
    -> [(Tx, TxMeta)]
    -> [Hash "Tx"]
filterTxs predicate = mapMaybe fn
  where
    fn (tx, meta) = if predicate meta then Just (txId tx) else Nothing

-- | Pick an arbitrary element from a monadic property, and label it in the
-- counterexample:
--
-- >>> ShowFmt meta <- namedPick "Wallet Metadata" arbitrary
--
-- If failing, the following line will be added to the counter example:
--
-- @
-- Wallet Metadata:
-- squirtle (still restoring (94%)), created at 1963-10-09 06:50:11 UTC, not delegating
-- @
namedPick :: Show a => String -> Gen a -> PropertyM IO a
namedPick lbl gen =
    monitor (counterexample ("\n" <> lbl <> ":")) *> pick gen

-- | Like 'assert', but allow giving a label / title before running a assertion
assertWith :: String -> Bool -> PropertyM IO ()
assertWith lbl condition = do
    let flag = if condition then "✓" else "✗"
    monitor (counterexample $ lbl <> " " <> flag)
    assert condition

{-------------------------------------------------------------------------------
                                    Properties
-------------------------------------------------------------------------------}

-- | Can list created wallets
prop_createListWallet
    :: DBLayer IO s ShelleyKey
    -> KeyValPairs WalletId (InitialCheckpoint s, WalletMetadata)
    -> Property
prop_createListWallet db@DBLayer{..} (KeyValPairs pairs) =
    monadicIO (setup >> prop)
  where
    setup = liftIO (cleanDB db)
    prop = liftIO $ do
        res <- once pairs $ \(k, (InitialCheckpoint cp0, meta)) ->
            atomically $ unsafeRunExceptT $
            initializeWallet k cp0 meta mempty gp
        (length <$> atomically listWallets) `shouldReturn` length res

-- | Trying to create a same wallet twice should yield an error
prop_createWalletTwice
    :: DBLayer IO s ShelleyKey
    -> ( WalletId
       , InitialCheckpoint s
       , WalletMetadata
       )
    -> Property
prop_createWalletTwice db@DBLayer{..} (wid, InitialCheckpoint cp0, meta) =
    monadicIO (setup >> prop)
  where
    setup = liftIO (cleanDB db)
    prop = liftIO $ do
        let err = ErrWalletAlreadyExists wid
        atomically (runExceptT $ initializeWallet wid cp0 meta mempty gp)
            `shouldReturn` Right ()
        atomically (runExceptT $ initializeWallet wid cp0 meta mempty gp)
            `shouldReturn` Left err

-- | Trying to remove a same wallet twice should yield an error
prop_removeWalletTwice
    :: DBLayer IO s ShelleyKey
    -> ( WalletId
       , InitialCheckpoint s
       , WalletMetadata
       )
    -> Property
prop_removeWalletTwice db@DBLayer{..} (wid, InitialCheckpoint cp0, meta) =
    monadicIO (setup >> prop)
  where
    setup = liftIO $ do
        cleanDB db
        atomically $ unsafeRunExceptT $ initializeWallet wid cp0 meta mempty gp
    prop = liftIO $ do
        let err = ErrNoSuchWallet wid
        atomically (runExceptT $ removeWallet wid) `shouldReturn` Right ()
        atomically (runExceptT $ removeWallet wid) `shouldReturn` Left err

-- | Checks that a given resource can be read after having been inserted in DB.
prop_readAfterPut
    :: ( Buildable (f a), Eq (f a), Applicative f, GenState s )
    => (  DBLayer IO s ShelleyKey
       -> WalletId
       -> a
       -> ExceptT ErrNoSuchWallet IO ()
       ) -- ^ Put Operation
    -> (  DBLayer IO s ShelleyKey
       -> WalletId
       -> IO (f a)
       ) -- ^ Read Operation
    -> DBLayer IO s ShelleyKey
    -> (WalletId, a)
        -- ^ Property arguments
    -> Property
prop_readAfterPut putOp readOp db@DBLayer{..} (wid, a) =
    monadicIO (setup >> prop)
  where
    setup = do
        run $ cleanDB db
        (InitialCheckpoint cp0, meta) <- pick arbitrary
        run $ atomically $ unsafeRunExceptT $
            initializeWallet wid cp0 meta mempty gp
    prop = do
        run $ unsafeRunExceptT $ putOp db wid a
        res <- run $ readOp db wid
        let fa = pure a
        monitor $ counterexample $ "\nInserted\n" <> pretty fa
        monitor $ counterexample $ "\nRead\n" <> pretty res
        assertWith "Inserted == Read" (res == fa)

prop_getTxAfterPutValidTxId
    :: GenState s
    => DBLayer IO s ShelleyKey
    -> WalletId
    -> GenTxHistory
    -> Property
prop_getTxAfterPutValidTxId db@DBLayer{..} wid txGen =
    monadicIO (setup >> prop)
  where
    setup = do
        run $ cleanDB db
        (InitialCheckpoint cp0, meta) <- pick arbitrary
        run $ atomically $ unsafeRunExceptT $
            initializeWallet wid cp0 meta mempty gp
    prop = do
        let txs = unGenTxHistory txGen
        run $ unsafeRunExceptT $ mapExceptT atomically $ putTxHistory wid txs
        forM_ txs $ \(Tx {txId}, txMeta) -> do
            (Just (TransactionInfo {txInfoId, txInfoMeta})) <-
                run $ atomically $ unsafeRunExceptT $ getTx wid txId
            monitor $ counterexample $ "\nInserted\n"
                <> pretty txMeta <> " for txId: " <> pretty txId
            monitor $ counterexample $ "\nRead\n"
                <> pretty txInfoMeta <> " for txId: " <> pretty txInfoId
            assertWith "Inserted is included in Read"
                (txMeta == txInfoMeta && txId == txInfoId)

prop_getTxAfterPutInvalidTxId
    :: GenState s
    => DBLayer IO s ShelleyKey
    -> WalletId
    -> GenTxHistory
    -> (Hash "Tx")
    -> Property
prop_getTxAfterPutInvalidTxId db@DBLayer{..} wid txGen txId' =
    monadicIO (setup >> prop)
  where
    setup = do
        run $ cleanDB db
        (InitialCheckpoint cp0, meta) <- pick arbitrary
        run $ atomically $ unsafeRunExceptT $
            initializeWallet wid cp0 meta mempty gp
    prop = do
        let txs = unGenTxHistory txGen
        run $ unsafeRunExceptT $ mapExceptT atomically $ putTxHistory wid txs
        res <- run $ atomically $ unsafeRunExceptT $ getTx wid txId'
        assertWith "Irrespective of Inserted, Read is Nothing for invalid tx id"
            (isNothing res)

prop_getTxAfterPutInvalidWalletId
    :: DBLayer IO s ShelleyKey
    -> ( WalletId
       , InitialCheckpoint s
       , WalletMetadata
       )
    -> GenTxHistory
    -> WalletId
    -> Property
prop_getTxAfterPutInvalidWalletId db@DBLayer{..}
    (wid, InitialCheckpoint cp0, meta) txGen wid'
  = wid /= wid' ==> monadicIO (setup >> prop)
  where
    setup = liftIO $ do
        cleanDB db
        atomically $ unsafeRunExceptT $ initializeWallet wid cp0 meta mempty gp
    prop = liftIO $ do
        let txs = unGenTxHistory txGen
        atomically (runExceptT $ putTxHistory wid txs) `shouldReturn` Right ()
        forM_ txs $ \(Tx {txId}, _) -> do
            let err = ErrNoSuchWallet wid'
            atomically (runExceptT $ getTx wid' txId) `shouldReturn` Left err

-- | Can't put resource before a wallet has been initialized
prop_putBeforeInit
    :: (Buildable (f a), Eq (f a), GenState s)
    => (  DBLayer IO s ShelleyKey
       -> WalletId
       -> a
       -> ExceptT ErrNoSuchWallet IO ()
       ) -- ^ Put Operation
    -> (  DBLayer IO s ShelleyKey
       -> WalletId
       -> IO (f a)
       ) -- ^ Read Operation
    -> f a
        -- ^ An 'empty' value for the 'Applicative' f
    -> DBLayer IO s ShelleyKey
    -> (WalletId, a)
        -- ^ Property arguments
    -> Property
prop_putBeforeInit putOp readOp empty db (wid, a) =
    monadicIO (setup >> prop)
  where
    setup = liftIO (cleanDB db)
    prop = liftIO $ do
        runExceptT (putOp db wid a) >>= \case
            Right _ ->
                fail "expected put operation to fail but it succeeded!"
            Left err ->
                err `shouldBe` ErrNoSuchWallet wid
        (ShowFmt <$> readOp db wid) `shouldReturn` (ShowFmt empty)

-- | Modifying one resource leaves the other untouched
prop_isolation
    :: ( Buildable (f b), Eq (f b)
       , Buildable (g c), Eq (g c)
       , Buildable (h d), Eq (h d)
       , GenState s
       , Show s
       )
    => (  DBLayer IO s ShelleyKey
       -> WalletId
       -> a
       -> ExceptT ErrNoSuchWallet IO ()
       ) -- ^ Put Operation
    -> (  DBLayer IO s ShelleyKey
       -> WalletId
       -> IO (f b)
       ) -- ^ Read Operation for another resource
    -> (  DBLayer IO s ShelleyKey
       -> WalletId
       -> IO (g c)
       ) -- ^ Read Operation for another resource
    -> (  DBLayer IO s ShelleyKey
       -> WalletId
       -> IO (h d)
       ) -- ^ Read Operation for another resource
    -> DBLayer IO s ShelleyKey
    -> (ShowFmt WalletId, ShowFmt a)
        -- ^ Properties arguments
    -> Property
prop_isolation putA readB readC readD db@DBLayer{..} (ShowFmt wid, ShowFmt a) =
    monadicIO (setup >>= prop)
  where
    setup = do
        run $ cleanDB db
        (InitialCheckpoint cp0, meta) <- pick arbitrary
        (GenTxHistory txs) <- pick arbitrary
        run $ atomically $ do
            unsafeRunExceptT $ initializeWallet wid cp0 meta mempty gp
            unsafeRunExceptT $ putTxHistory wid txs
        run $ (,,)
            <$> readB db wid
            <*> readC db wid
            <*> readD db wid

    prop (b, c, d) = liftIO $ do
        unsafeRunExceptT $ putA db wid a
        (ShowFmt <$> readB db wid) `shouldReturn` ShowFmt b
        (ShowFmt <$> readC db wid) `shouldReturn` ShowFmt c
        (ShowFmt <$> readD db wid) `shouldReturn` ShowFmt d

-- | Can't read back data after delete
prop_readAfterDelete
    :: (Buildable (f a), Eq (f a), GenState s)
    => (  DBLayer IO s ShelleyKey
       -> WalletId
       -> IO (f a)
       ) -- ^ Read Operation
    -> f a
        -- ^ An 'empty' value for the 'Applicative' f
    -> DBLayer IO s ShelleyKey
    -> ShowFmt WalletId
    -> Property
prop_readAfterDelete readOp empty db@DBLayer{..} (ShowFmt wid) =
    monadicIO (setup >> prop)
  where
    setup = do
        run $ cleanDB db
        (InitialCheckpoint cp0, meta) <- pick arbitrary
        run $ atomically $ unsafeRunExceptT $
            initializeWallet wid cp0 meta mempty gp
    prop = liftIO $ do
        atomically $ unsafeRunExceptT $ removeWallet wid
        (ShowFmt <$> readOp db wid) `shouldReturn` ShowFmt empty

-- | Check that the DB supports multiple sequential puts for a given resource
prop_sequentialPut
    :: (Buildable (f a), Eq (f a), GenState s)
    => (  DBLayer IO s ShelleyKey
       -> WalletId
       -> a
       -> ExceptT ErrNoSuchWallet IO ()
       ) -- ^ Put Operation
    -> (  DBLayer IO s ShelleyKey
       -> WalletId
       -> IO (f a)
       ) -- ^ Read Operation
    -> (forall k. Ord k => [(k, a)] -> [f a])
        -- ^ How do we expect operations to resolve
    -> DBLayer IO s ShelleyKey
    -> KeyValPairs (ShowFmt WalletId) (ShowFmt a)
        -- ^ Property arguments
    -> Property
prop_sequentialPut putOp readOp resolve db@DBLayer{..} kv =
    cover 25 cond "conflicting db entries" $ monadicIO (setup >> prop)
  where
    pairs = (\(KeyValPairs xs) -> bimap unShowFmt unShowFmt <$> xs) kv
    -- Make sure that we have some conflicting insertion to actually test the
    -- semantic of the DB Layer.
    cond = L.length (L.nub ids) /= L.length ids
      where
        ids = map fst pairs
    setup = do
        run $ cleanDB db
        (InitialCheckpoint cp0, meta) <- pick arbitrary
        run $ atomically $ unsafeRunExceptT $ once_ pairs $ \(k, _) ->
            initializeWallet k cp0 meta mempty gp
    prop = do
        run $ unsafeRunExceptT $ forM_ pairs $ uncurry (putOp db)
        res <- run $ once pairs (readOp db . fst)
        let resolved = resolve pairs
        monitor $ counterexample $ "\nResolved\n" <> pretty resolved
        monitor $ counterexample $ "\nRead\n" <> pretty res
        assertWith "Resolved == Read" (res == resolved)

-- | Check that the DB supports multiple sequential puts for a given resource
prop_parallelPut
    :: (GenState s)
    => (  DBLayer IO s ShelleyKey
       -> WalletId
       -> a
       -> ExceptT ErrNoSuchWallet IO ()
       ) -- ^ Put Operation
    -> (  DBLayer IO s ShelleyKey
       -> WalletId
       -> IO (f a)
       ) -- ^ Read Operation
    -> (forall k. Ord k => [(k, a)] -> Int)
        -- ^ How many entries to we expect in the end
    -> DBLayer IO s ShelleyKey
    -> KeyValPairs WalletId a
        -- ^ Property arguments
    -> Property
prop_parallelPut putOp readOp resolve db@DBLayer{..} (KeyValPairs pairs) =
    cover 25 cond "conflicting db entries" $ monadicIO (setup >> prop)
  where
    -- Make sure that we have some conflicting insertion to actually test the
    -- semantic of the DB Layer.
    cond = L.length (L.nub ids) /= L.length ids
      where
        ids = map fst pairs
    setup = do
        run $ cleanDB db
        (InitialCheckpoint cp0, meta) <- pick arbitrary
        run $ atomically $ unsafeRunExceptT $ once_ pairs $ \(k, _) ->
            initializeWallet k cp0 meta mempty gp
    prop = liftIO $ do
        forConcurrently_ pairs $ unsafeRunExceptT . uncurry (putOp db)
        res <- once pairs (readOp db . fst)
        length res `shouldBe` resolve pairs


-- | Can rollback to any particular checkpoint previously stored
prop_rollbackCheckpoint
    :: forall s k. (GenState s, Eq s)
    => DBLayer IO s k
    -> InitialCheckpoint s
    -> MockChain
    -> Property
prop_rollbackCheckpoint db@DBLayer{..} (InitialCheckpoint cp0) (MockChain chain) = do
    monadicIO $ do
        ShowFmt wid <- namedPick "Wallet ID" arbitrary
        ShowFmt meta <- namedPick "Wallet Metadata" arbitrary
        ShowFmt point <- namedPick "Rollback target" (elements $ ShowFmt <$> cps)
        setup wid meta >> prop wid point
  where
    cps :: [Wallet s]
    cps = flip unfoldr (chain, cp0) $ \case
        ([], _) -> Nothing
        (b:q, cp) -> let cp' = snd . snd $ applyBlock b cp in Just (cp', (q, cp'))

    setup wid meta = run $ do
        cleanDB db
        atomically $ do
            unsafeRunExceptT $ initializeWallet wid cp0 meta mempty gp
            unsafeRunExceptT $ forM_ cps (putCheckpoint wid)

    prop wid point = do
        let tip = currentTip point
        point' <- run $ atomically $ unsafeRunExceptT $
            rollbackTo wid (toSlot $ chainPointFromBlockHeader tip)
        cp <- run $ atomically $ readCheckpoint wid
        let str = maybe "∅" pretty cp
        monitor $ counterexample ("Checkpoint after rollback: \n" <> str)
        assert (ShowFmt cp == ShowFmt (pure point))
        assert (ShowFmt point' == ShowFmt (chainPointFromBlockHeader tip))

-- | Re-schedule pending transaction on rollback, i.e.:
--
-- (PoR = Point of Rollback)
--
-- - There's no transaction beyond the PoR
-- - Any incoming transaction after the PoR is forgotten
-- - Any outgoing transaction after the PoR is back in pending, and have a slot
--   equal to the PoR.
--
-- FIXME LATER: This function only tests slot numbers to roll back to,
-- not Slot. See note [PointSlotNo] for the difference.
-- The reason for this restriction is that the 'rollbackTo' function
-- from the DBLayer currently does not roll the TxHistory back correctly
-- if there is a rollback to genesis.
prop_rollbackTxHistory
    :: forall s k. ()
    => DBLayer IO s k
    -> InitialCheckpoint s
    -> GenTxHistory
    -> Property
prop_rollbackTxHistory db@DBLayer{..} (InitialCheckpoint cp0) (GenTxHistory txs0) = do
    monadicIO $ do
        ShowFmt wid <- namedPick "Wallet ID" arbitrary
        ShowFmt meta <- namedPick "Wallet Metadata" arbitrary
        ShowFmt slot <- namedPick "Requested Rollback slot" arbitrary
        let ixs = rescheduled slot
        monitor $ label ("Outgoing tx after point: " <> show (L.length ixs))
        monitor $ cover 50 (not $ null ixs) "rolling back something"
        setup wid meta >> prop wid slot
  where
    setup wid meta = run $ do
        cleanDB db
        atomically $ do
            unsafeRunExceptT $ initializeWallet wid cp0 meta mempty gp
            unsafeRunExceptT $ putTxHistory wid txs0

    prop wid requestedPoint = do
        point <- run $ unsafeRunExceptT $ mapExceptT atomically $
            rollbackTo wid (At requestedPoint)
        txs <- run $ atomically $ fmap toTxHistory
            <$> readTxHistory wid Nothing Descending wholeRange Nothing

        monitor $ counterexample $ "\n" <> "Actual Rollback Point:\n" <> (pretty point)
        monitor $ counterexample $ "\nOriginal tx history:\n" <> (txsF txs0)
        monitor $ counterexample $ "\nNew tx history:\n" <> (txsF txs)

        let slot = pseudoSlotNo point
        assertWith "Outgoing txs are reschuled" $
            L.sort (rescheduled slot) == L.sort (filterTxs isPending txs)
        assertWith "All other txs are still known" $
            L.sort (knownAfterRollback slot) == L.sort (txId . fst <$> txs)
        assertWith "All txs are now before the point of rollback" $
            all (isBefore slot . snd) txs
      where
        txsF :: [(Tx, TxMeta)] -> String
        txsF =
            L.intercalate "\n"
            . map (\(tx, meta) -> unwords
                [ "- "
                , pretty (txId tx)
                , pretty (meta ^. #slotNo)
                , pretty (meta ^. #status)
                , pretty (meta ^. #direction)
                , pretty (meta ^. #amount)
                ])

    pseudoSlotNo ChainPointAtGenesis = SlotNo 0
    pseudoSlotNo (ChainPoint slot _) = slot

    isBefore :: SlotNo -> TxMeta -> Bool
    isBefore slot meta = (slotNo :: TxMeta -> SlotNo) meta <= slot

    rescheduled :: SlotNo -> [Hash "Tx"]
    rescheduled slot =
        let addedAfter meta =
                direction meta == Outgoing && meta ^. #slotNo > slot
        in filterTxs (\tx -> addedAfter tx || isPending tx) txs0

    knownAfterRollback :: SlotNo -> [Hash "Tx"]
    knownAfterRollback slot =
        rescheduled slot ++ filterTxs (isBefore slot) txs0

-- | No matter what, the current tip is always a checkpoint.
prop_sparseCheckpointTipAlwaysThere
    :: GenSparseCheckpointsArgs
    -> Property
prop_sparseCheckpointTipAlwaysThere (GenSparseCheckpointsArgs cfg h) = prop
    & counterexample ("Checkpoints: " <> show cps)
    & counterexample ("h=" <> show h)
  where
    cps = sparseCheckpoints cfg (Quantity h)

    prop :: Property
    prop = property $ fromIntegral h `elem` cps

-- | Check that sparseCheckpoints always return at least edgeSize checkpoints (or
-- exactly the current height if h < edgeSize).
prop_sparseCheckpointMinimum
    :: GenSparseCheckpointsArgs
    -> Property
prop_sparseCheckpointMinimum (GenSparseCheckpointsArgs cfg h) = prop
    & counterexample ("Checkpoints: " <> show cps)
    & counterexample ("h=" <> show h)
  where
    cps = sparseCheckpoints cfg (Quantity h)

    prop :: Property
    prop = property $ fromIntegral (length cps) >= min e h
      where
        e = fromIntegral $ edgeSize cfg

-- | This property checks that, the checkpoints kept for an edge size of 0 are
-- included in the list with a non-null edge size, all else equals.
prop_sparseCheckpointEdgeSize0
    :: GenSparseCheckpointsArgs
    -> Property
prop_sparseCheckpointEdgeSize0 (GenSparseCheckpointsArgs cfg h) = prop
    & counterexample ("Checkpoints: " <> show cps)
    & counterexample ("h=" <> show h)
  where
    cps  = sparseCheckpoints cfg (Quantity h)
    cps' = sparseCheckpoints (cfg { edgeSize = 0 }) (Quantity h)

    prop :: Property
    prop = property (cps' `L.isSubsequenceOf` cps)

-- | This property shows that, for all possible cuts (i.e. non-null batches) of
-- a sequence of blocks, the following steps:
--
--  For all batch B in sequence:
--
--  - Keep all checkpoints in B yielded by sparseCheckpoint with and
--    edge size of 0.
--
--  - Keep the last checkpoint of the batch regardless
--
--  - Prune all checkpoints not yielded by sparseCheckpoint with a non-null edge
--    size
--
-- are equivalent to calling `sparseCheckpoints` on the flattened batch
-- sequence.
--
-- Note that the batch creation mimics the way blocks are served by the network
-- layer to wallets: first by batches of arbitrary size, and then one by one.
--
-- The property shows that regardless of how batches are served, after
-- 'edgeSize' one-by-one step, the end result is the same as if the entire
-- stream of blocks had been processed in one go.
prop_checkpointsEventuallyEqual
    :: GenSparseCheckpointsArgs
    -> Property
prop_checkpointsEventuallyEqual args@(GenSparseCheckpointsArgs cfg h) =
    h > epochStability cfg ==> forAll (genBatches args) $ \(Batches batches) ->
        let
            tip =
                Quantity $ last $ mconcat batches
            emptyDB =
                SparseCheckpointsDB []
            dbs =
                L.scanl (\db batch -> prune $ step batch db) emptyDB batches
        in
            ( prop_eventuallyReachesExpectedTip tip dbs
              .&&.
              prop_canNeverRollbackMoreThanKPlusGap tip dbs
            )
  where
    prop_eventuallyReachesExpectedTip
        :: Quantity "block" Word32
        -> [SparseCheckpointsDB]
        -> Property
    prop_eventuallyReachesExpectedTip tip dbs =
        last dbs === SparseCheckpointsDB (sparseCheckpoints cfg tip)

    prop_canNeverRollbackMoreThanKPlusGap
        :: Quantity "block" Word32
        -> [SparseCheckpointsDB]
        -> Property
    prop_canNeverRollbackMoreThanKPlusGap (Quantity tip) dbs =
        conjoin (forEachStep <$> L.tail dbs)
      where
        forEachStep (SparseCheckpointsDB db) =
            let
                -- db' contains all the _stable checkpoints_ in the database,
                -- i.e. those that are in the interval [0; network tip - k)
                --
                -- So, if we are asked to rollback for a full k, we'll end up
                -- rolling back to the closest checkpoint from that interval.
                db' = filter (< (tip - epochStability cfg)) db
                farthestRollback = last db - last db'
            in
                property
                    (farthestRollback <= epochStability cfg + gapSize cfg)
                & counterexample
                    ("database: " <> show db)
                & counterexample
                    ("stable checkpoints: " <> show db')

    step :: [Word32] -> SparseCheckpointsDB -> SparseCheckpointsDB
    step cps (SparseCheckpointsDB db) =
        let
            toKeep =
                sparseCheckpoints (cfg { edgeSize = 0 }) (Quantity h)
            cps' =
                last cps : (toKeep `L.intersect` cps)
        in
            SparseCheckpointsDB $ L.sort $ cps' ++ db

    prune :: SparseCheckpointsDB -> SparseCheckpointsDB
    prune (SparseCheckpointsDB db) =
        let
            tip =
                Quantity $ last db
            db' =
                sparseCheckpoints cfg tip `L.intersect` db
        in
            SparseCheckpointsDB db'

newtype Batches = Batches [[Word32]] deriving Show

newtype SparseCheckpointsDB = SparseCheckpointsDB [Word32] deriving (Show, Eq)

gp :: GenesisParameters
gp = dummyGenesisParameters

data GenSparseCheckpointsArgs
    = GenSparseCheckpointsArgs SparseCheckpointsConfig Word32
    deriving Show

instance Arbitrary GenSparseCheckpointsArgs where
    arbitrary = do
        k <- (\x -> 10 + (x `mod` 1000)) <$> arbitrary
        h <- (`mod` 10000) <$> arbitrary
        cfg <- SparseCheckpointsConfig <$> arbitrary <*> pure k
        pure $ GenSparseCheckpointsArgs cfg h

-- This functions generate `h` "block header" (modeled as a Word32), grouped in
-- batches of arbitrary (albeit meaningful) sizes.
--
-- Batches always end with `edgeSize` "block header" in singleton batches, to
-- simulate a fast and slow mode.
genBatches
    :: GenSparseCheckpointsArgs
    -> Gen Batches
genBatches (GenSparseCheckpointsArgs cfg h) = do
    bs <- go [0..h] []
    let e = fromIntegral $ edgeSize cfg
    let oneByOne = pure <$> [h+1..h+e]
    pure (Batches (bs ++ oneByOne))
  where
    go :: [Word32] -> [[Word32]] -> Gen [[Word32]]
    go []     batches = pure $ reverse batches
    go source batches = do
        -- NOTE:
        -- Generate batches that can be larger than the chosen gap size, to make
        -- sure we generate realistic cases.
        n <- fromIntegral <$> choose (1, 3 * gapSize cfg)
        go (drop n source) (take n source : batches)
