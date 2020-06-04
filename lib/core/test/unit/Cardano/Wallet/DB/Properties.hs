{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Cardano.Wallet.DB.Properties
    ( properties
    , withDB
    ) where

import Prelude

import Cardano.Wallet.DB
    ( DBLayer (..)
    , ErrNoSuchWallet (..)
    , ErrWalletAlreadyExists (..)
    , PrimaryKey (..)
    , cleanDB
    , sparseCheckpoints
    )
import Cardano.Wallet.DB.Arbitrary
    ( GenState
    , GenTxHistory (..)
    , InitialCheckpoint (..)
    , KeyValPairs (..)
    , MockChain (..)
    )
import Cardano.Wallet.DB.Model
    ( filterTxHistory )
import Cardano.Wallet.DummyTarget.Primitive.Types
    ( dummyProtocolParameters )
import Cardano.Wallet.Primitive.AddressDerivation.Jormungandr
    ( JormungandrKey (..) )
import Cardano.Wallet.Primitive.Model
    ( Wallet, applyBlock, currentTip )
import Cardano.Wallet.Primitive.Types
    ( BlockHeader (..)
    , Direction (..)
    , Hash (..)
    , ProtocolParameters
    , ShowFmt (..)
    , SlotId (..)
    , SortOrder (..)
    , TransactionInfo (..)
    , Tx (..)
    , TxMeta (..)
    , TxStatus (..)
    , WalletId (..)
    , WalletMetadata (..)
    , isPending
    , toTxHistory
    , wholeRange
    )
import Cardano.Wallet.Unsafe
    ( unsafeRunExceptT )
import Control.Concurrent.Async
    ( forConcurrently_ )
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
import Data.Functor
    ( ($>) )
import Data.Functor.Identity
    ( Identity (..) )
import Data.Generics.Internal.VL.Lens
    ( view, (^.) )
import Data.Generics.Labels
    ()
import Data.List
    ( unfoldr )
import Data.Maybe
    ( catMaybes, mapMaybe )
import Data.Quantity
    ( Quantity (..) )
import Data.Set
    ( Set )
import Data.Word
    ( Word32, Word64 )
import Fmt
    ( Buildable, blockListF, pretty )
import Test.Hspec
    ( Spec
    , SpecWith
    , beforeAll
    , beforeWith
    , describe
    , it
    , shouldBe
    , shouldReturn
    )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , Property
    , checkCoverage
    , counterexample
    , cover
    , elements
    , label
    , property
    , suchThat
    )
import Test.QuickCheck.Monadic
    ( PropertyM, assert, monadicIO, monitor, pick, run )

import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- | Provide a DBLayer to a Spec that requires it. The database is initialised
-- once, and cleared with 'cleanDB' before each test.
withDB :: IO (DBLayer IO s k) -> SpecWith (DBLayer IO s k) -> Spec
withDB create = beforeAll create . beforeWith (\db -> cleanDB db $> db)

properties
    :: (GenState s, Eq s)
    => SpecWith (DBLayer IO s JormungandrKey)
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
            let k = Quantity 2160
            let h = Quantity 42
            -- First unstable block: 0
            sparseCheckpoints k h `shouldBe`
                [0,32,33,34,35,36,37,38,39,40,41,42]

        it "k=2160, h=2414" $ \_ -> do
            let k = Quantity 2160
            let h = Quantity 2714
            -- First unstable block: 554
            sparseCheckpoints k h `shouldBe`
                [ 0    , 500  , 600  , 700  , 800  , 900
                , 1000 , 1100 , 1200 , 1300 , 1400 , 1500
                , 1600 , 1700 , 1800 , 1900 , 2000 , 2100
                , 2200 , 2300 , 2400 , 2500 , 2600 , 2700
                , 2704 , 2705 , 2706 , 2707 , 2708 , 2709
                , 2710 , 2711 , 2712 , 2713 , 2714
                ]

        it "The tip is always a checkpoint" $ \_ ->
            property prop_sparseCheckpointTipAlwaysThere

        it "There's at least (min h 10) checkpoints" $ \_ ->
            property prop_sparseCheckpointMinimum

        it "There's no checkpoint older than k (+/- 100)" $ \_ ->
            property prop_sparseCheckpointNoOlderThanK

-- | Wrap the result of 'readTxHistory' in an arbitrary identity Applicative
readTxHistoryF
    :: Functor m
    => DBLayer m s JormungandrKey
    -> PrimaryKey WalletId
    -> m (Identity GenTxHistory)
readTxHistoryF DBLayer{..} wid =
    (Identity . GenTxHistory . fmap toTxHistory)
        <$> atomically (readTxHistory wid Descending wholeRange Nothing)

putTxHistoryF
    :: DBLayer m s JormungandrKey
    -> PrimaryKey WalletId
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
      . filterTxHistory Descending wholeRange
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
    :: DBLayer IO s JormungandrKey
    -> KeyValPairs (PrimaryKey WalletId) (Wallet s , WalletMetadata)
    -> Property
prop_createListWallet db@DBLayer{..} (KeyValPairs pairs) =
    monadicIO (setup >> prop)
  where
    setup = liftIO (cleanDB db)
    prop = liftIO $ do
        res <- once pairs $ \(k, (cp, meta)) ->
            atomically $ unsafeRunExceptT $
            initializeWallet k cp meta mempty pp
        (length <$> atomically listWallets) `shouldReturn` length res

-- | Trying to create a same wallet twice should yield an error
prop_createWalletTwice
    :: DBLayer IO s JormungandrKey
    -> ( PrimaryKey WalletId
       , Wallet s
       , WalletMetadata
       )
    -> Property
prop_createWalletTwice db@DBLayer{..} (key@(PrimaryKey wid), cp, meta) =
    monadicIO (setup >> prop)
  where
    setup = liftIO (cleanDB db)
    prop = liftIO $ do
        let err = ErrWalletAlreadyExists wid
        atomically (runExceptT $ initializeWallet key cp meta mempty pp)
            `shouldReturn` Right ()
        atomically (runExceptT $ initializeWallet key cp meta mempty pp)
            `shouldReturn` Left err

-- | Trying to remove a same wallet twice should yield an error
prop_removeWalletTwice
    :: DBLayer IO s JormungandrKey
    -> ( PrimaryKey WalletId
       , Wallet s
       , WalletMetadata
       )
    -> Property
prop_removeWalletTwice db@DBLayer{..} (key@(PrimaryKey wid), cp, meta) =
    monadicIO (setup >> prop)
  where
    setup = liftIO $ do
        cleanDB db
        atomically $ unsafeRunExceptT $ initializeWallet key cp meta mempty pp
    prop = liftIO $ do
        let err = ErrNoSuchWallet wid
        atomically (runExceptT $ removeWallet key) `shouldReturn` Right ()
        atomically (runExceptT $ removeWallet key) `shouldReturn` Left err

-- | Checks that a given resource can be read after having been inserted in DB.
prop_readAfterPut
    :: ( Buildable (f a), Eq (f a), Applicative f, GenState s )
    => (  DBLayer IO s JormungandrKey
       -> PrimaryKey WalletId
       -> a
       -> ExceptT ErrNoSuchWallet IO ()
       ) -- ^ Put Operation
    -> (  DBLayer IO s JormungandrKey
       -> PrimaryKey WalletId
       -> IO (f a)
       ) -- ^ Read Operation
    -> DBLayer IO s JormungandrKey
    -> (PrimaryKey WalletId, a)
        -- ^ Property arguments
    -> Property
prop_readAfterPut putOp readOp db@DBLayer{..} (key, a) =
    monadicIO (setup >> prop)
  where
    setup = do
        run $ cleanDB db
        (InitialCheckpoint cp, meta) <- namedPick "Initial Checkpoint" arbitrary
        run $ atomically $ unsafeRunExceptT $
            initializeWallet key cp meta mempty pp
    prop = do
        run $ unsafeRunExceptT $ putOp db key a
        res <- run $ readOp db key
        let fa = pure a
        monitor $ counterexample $ "\nInserted\n" <> pretty fa
        monitor $ counterexample $ "\nRead\n" <> pretty res
        assertWith "Inserted == Read" (res == fa)

-- | Can't put resource before a wallet has been initialized
prop_putBeforeInit
    :: (Buildable (f a), Eq (f a))
    => (  DBLayer IO s JormungandrKey
       -> PrimaryKey WalletId
       -> a
       -> ExceptT ErrNoSuchWallet IO ()
       ) -- ^ Put Operation
    -> (  DBLayer IO s JormungandrKey
       -> PrimaryKey WalletId
       -> IO (f a)
       ) -- ^ Read Operation
    -> f a
        -- ^ An 'empty' value for the 'Applicative' f
    -> DBLayer IO s JormungandrKey
    -> (PrimaryKey WalletId, a)
        -- ^ Property arguments
    -> Property
prop_putBeforeInit putOp readOp empty db@DBLayer{..} (key@(PrimaryKey wid), a) =
    monadicIO (setup >> prop)
  where
    setup = liftIO (cleanDB db)
    prop = liftIO $ do
        runExceptT (putOp db key a) >>= \case
            Right _ ->
                fail "expected put operation to fail but it succeeded!"
            Left err ->
                err `shouldBe` ErrNoSuchWallet wid
        (ShowFmt <$> readOp db key) `shouldReturn` (ShowFmt empty)

-- | Modifying one resource leaves the other untouched
prop_isolation
    :: ( Buildable (f b), Eq (f b)
       , Buildable (g c), Eq (g c)
       , Buildable (h d), Eq (h d)
       , Arbitrary (Wallet s)
       , Show s
       )
    => (  DBLayer IO s JormungandrKey
       -> PrimaryKey WalletId
       -> a
       -> ExceptT ErrNoSuchWallet IO ()
       ) -- ^ Put Operation
    -> (  DBLayer IO s JormungandrKey
       -> PrimaryKey WalletId
       -> IO (f b)
       ) -- ^ Read Operation for another resource
    -> (  DBLayer IO s JormungandrKey
       -> PrimaryKey WalletId
       -> IO (g c)
       ) -- ^ Read Operation for another resource
    -> (  DBLayer IO s JormungandrKey
       -> PrimaryKey WalletId
       -> IO (h d)
       ) -- ^ Read Operation for another resource
    -> DBLayer IO s JormungandrKey
    -> (ShowFmt (PrimaryKey WalletId), ShowFmt a)
        -- ^ Properties arguments
    -> Property
prop_isolation putA readB readC readD db@DBLayer{..} (ShowFmt key, ShowFmt a) =
    monadicIO (setup >>= prop)
  where
    setup = do
        liftIO (cleanDB db)
        (cp, meta, GenTxHistory txs) <- pick arbitrary
        liftIO $ atomically $ do
            unsafeRunExceptT $ initializeWallet key cp meta mempty pp
            unsafeRunExceptT $ putTxHistory key txs
        (b, c, d) <- liftIO $ (,,)
            <$> readB db key
            <*> readC db key
            <*> readD db key
        return (b, c, d)

    prop (b, c, d) = liftIO $ do
        unsafeRunExceptT $ putA db key a
        (ShowFmt <$> readB db key) `shouldReturn` ShowFmt b
        (ShowFmt <$> readC db key) `shouldReturn` ShowFmt c
        (ShowFmt <$> readD db key) `shouldReturn` ShowFmt d

-- | Can't read back data after delete
prop_readAfterDelete
    :: (Buildable (f a), Eq (f a), GenState s)
    => (  DBLayer IO s JormungandrKey
       -> PrimaryKey WalletId
       -> IO (f a)
       ) -- ^ Read Operation
    -> f a
        -- ^ An 'empty' value for the 'Applicative' f
    -> DBLayer IO s JormungandrKey
    -> ShowFmt (PrimaryKey WalletId)
    -> Property
prop_readAfterDelete readOp empty db@DBLayer{..} (ShowFmt key) =
    monadicIO (setup >> prop)
  where
    setup = do
        liftIO (cleanDB db)
        (cp, meta) <- pick arbitrary
        liftIO $ atomically $ unsafeRunExceptT $
            initializeWallet key cp meta mempty pp
    prop = liftIO $ do
        atomically $ unsafeRunExceptT $ removeWallet key
        (ShowFmt <$> readOp db key) `shouldReturn` ShowFmt empty

-- | Check that the DB supports multiple sequential puts for a given resource
prop_sequentialPut
    :: (Buildable (f a), Eq (f a), GenState s)
    => (  DBLayer IO s JormungandrKey
       -> PrimaryKey WalletId
       -> a
       -> ExceptT ErrNoSuchWallet IO ()
       ) -- ^ Put Operation
    -> (  DBLayer IO s JormungandrKey
       -> PrimaryKey WalletId
       -> IO (f a)
       ) -- ^ Read Operation
    -> (forall k. Ord k => [(k, a)] -> [f a])
        -- ^ How do we expect operations to resolve
    -> DBLayer IO s JormungandrKey
    -> KeyValPairs (ShowFmt (PrimaryKey WalletId)) (ShowFmt a)
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
        (InitialCheckpoint cp, meta) <- pick arbitrary
        run $ atomically $ unsafeRunExceptT $ once_ pairs $ \(k, _) ->
            initializeWallet k cp meta mempty pp
    prop = do
        run $ unsafeRunExceptT $ forM_ pairs $ uncurry (putOp db)
        res <- run $ once pairs (readOp db . fst)
        let resolved = resolve pairs
        monitor $ counterexample $ "\nResolved\n" <> pretty resolved
        monitor $ counterexample $ "\nRead\n" <> pretty res
        assertWith "Resolved == Read" (res == resolved)

-- | Check that the DB supports multiple sequential puts for a given resource
prop_parallelPut
    :: (Arbitrary (Wallet s), Show s)
    => (  DBLayer IO s JormungandrKey
       -> PrimaryKey WalletId
       -> a
       -> ExceptT ErrNoSuchWallet IO ()
       ) -- ^ Put Operation
    -> (  DBLayer IO s JormungandrKey
       -> PrimaryKey WalletId
       -> IO (f a)
       ) -- ^ Read Operation
    -> (forall k. Ord k => [(k, a)] -> Int)
        -- ^ How many entries to we expect in the end
    -> DBLayer IO s JormungandrKey
    -> KeyValPairs (PrimaryKey WalletId) a
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
        liftIO (cleanDB db)
        (cp, meta) <- pick arbitrary
        liftIO $ atomically $ unsafeRunExceptT $ once_ pairs $ \(k, _) ->
            initializeWallet k cp meta mempty pp
    prop = liftIO $ do
        forConcurrently_ pairs $ unsafeRunExceptT . uncurry (putOp db)
        res <- once pairs (readOp db . fst)
        length res `shouldBe` resolve pairs


-- | Can rollback to any particular checkpoint previously stored
prop_rollbackCheckpoint
    :: forall s k. (GenState s, Eq s)
    => DBLayer IO s k
    -> Wallet s
    -> MockChain
    -> Property
prop_rollbackCheckpoint db@DBLayer{..} cp0 (MockChain chain) = do
    monadicIO $ do
        ShowFmt wid <- namedPick "Wallet ID" arbitrary
        ShowFmt meta <- namedPick "Wallet Metadata" arbitrary
        ShowFmt point <- namedPick "Rollback target" (elements $ ShowFmt <$> cps)
        setup wid meta >> prop wid point
  where
    cps :: [Wallet s]
    cps = flip unfoldr (chain, cp0) $ \case
        ([], _) -> Nothing
        (b:q, cp) -> let cp' = snd $ applyBlock b cp in Just (cp', (q, cp'))

    setup wid meta = run $ do
        cleanDB db
        atomically $ do
            unsafeRunExceptT $ initializeWallet wid cp0 meta mempty pp
            unsafeRunExceptT $ forM_ cps (putCheckpoint wid)

    prop wid point = do
        let tip = currentTip point
        point' <- run $ atomically $ unsafeRunExceptT $
            rollbackTo wid (tip ^. #slotId)
        cp <- run $ atomically $ readCheckpoint wid
        let str = maybe "∅" pretty cp
        monitor $ counterexample ("Checkpoint after rollback: \n" <> str)
        assert (ShowFmt cp == ShowFmt (pure point))
        assert (ShowFmt point' == ShowFmt (tip ^. #slotId))

-- | Re-schedule pending transaction on rollback, i.e.:
--
-- (PoR = Point of Rollback)
--
-- - There's no transaction beyond the PoR
-- - Any incoming transaction after the PoR is forgotten
-- - Any outgoing transaction after the PoR is back in pending, and have a slot
--   equal to the PoR.
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
        ShowFmt point <- namedPick "Requested Rollback point" arbitrary
        let ixs = rescheduled point
        monitor $ label ("Outgoing tx after point: " <> show (L.length ixs))
        monitor $ cover 50 (not $ null ixs) "rolling back something"
        setup wid meta >> prop wid point
  where
    setup wid meta = run $ do
        cleanDB db
        atomically $ do
            unsafeRunExceptT $ initializeWallet wid cp0 meta mempty pp
            unsafeRunExceptT $ putTxHistory wid txs0

    prop wid requestedPoint = do
        point <- run $ unsafeRunExceptT $ mapExceptT atomically $ rollbackTo wid requestedPoint
        txs <- run $ atomically $ fmap toTxHistory
            <$> readTxHistory wid Descending wholeRange Nothing

        monitor $ counterexample $ "\n" <> "Actual Rollback Point:\n" <> (pretty point)
        monitor $ counterexample $ "\nOriginal tx history:\n" <> (txsF txs0)
        monitor $ counterexample $ "\nNew tx history:\n" <> (txsF txs)

        assertWith "Outgoing txs are reschuled" $
            L.sort (rescheduled point) == L.sort (filterTxs isPending txs)
        assertWith "All other txs are still known" $
            L.sort (knownAfterRollback point) == L.sort (txId . fst <$> txs)
        assertWith "All txs are now before the point of rollback" $
            all (isBefore point . snd) txs
      where
        txsF :: [(Tx, TxMeta)] -> String
        txsF =
            L.intercalate "\n"
            . map (\(tx, meta) -> unwords
                [ "- "
                , pretty (txId tx)
                , pretty (meta ^. #slotId)
                , pretty (meta ^. #status)
                , pretty (meta ^. #direction)
                , show $ getQuantity ((meta ^. #amount))
                ])

    isBefore :: SlotId -> TxMeta -> Bool
    isBefore point meta =
        (slotId :: TxMeta -> SlotId) meta <= point

    rescheduled :: SlotId -> [Hash "Tx"]
    rescheduled point =
        let addedAfter meta = direction meta == Outgoing && meta ^. #slotId > point
        in filterTxs (\tx -> addedAfter tx || isPending tx) txs0

    knownAfterRollback :: SlotId -> [Hash "Tx"]
    knownAfterRollback point =
        rescheduled point ++ filterTxs (isBefore point) txs0

-- | No matter what, the current tip is always a checkpoint.
prop_sparseCheckpointTipAlwaysThere
    :: GenSparseCheckpointsArgs
    -> Property
prop_sparseCheckpointTipAlwaysThere (GenSparseCheckpointsArgs (k, h)) = prop
    & counterexample ("Checkpoints: " <> show cps)
    & counterexample ("h=" <> show h)
    & counterexample ("k=" <> show k)
  where
    cps = sparseCheckpoints (Quantity k) (Quantity h)

    prop :: Property
    prop = property $ fromIntegral h `elem` cps

-- | Check that sparseCheckpoints always return at least 10 checkpoints (or
-- exactly the current height if h < 10).
prop_sparseCheckpointMinimum
    :: GenSparseCheckpointsArgs
    -> Property
prop_sparseCheckpointMinimum (GenSparseCheckpointsArgs (k, h)) = prop
    & counterexample ("Checkpoints: " <> show cps)
    & counterexample ("h=" <> show h)
    & counterexample ("k=" <> show k)
  where
    cps = sparseCheckpoints (Quantity k) (Quantity h)

    prop :: Property
    prop = property $ fromIntegral (length cps) >= min 10 h


-- | Check that sparseCheckpoints always return checkpoints that can cover
-- rollbacks up to `k` in the past. This means that, if the current block height
-- is #3000, and `k=2160`, we should be able to rollback to #840. Since we make
-- checkpoints every 100 blocks, it means that block #800 should be in the list.
--
-- Note: The initial checkpoint at #0 will always be present.
prop_sparseCheckpointNoOlderThanK
    :: GenSparseCheckpointsArgs
    -> Property
prop_sparseCheckpointNoOlderThanK (GenSparseCheckpointsArgs (k, h)) = prop
    & counterexample ("Checkpoints: " <> show ((\cp -> (age cp, cp)) <$> cps))
    & counterexample ("h=" <> show h)
    & counterexample ("k=" <> show k)
  where
    cps = sparseCheckpoints (Quantity k) (Quantity h)

    prop :: Property
    prop = property $ flip all cps $ \cp ->
        cp == 0 || (age cp - 100 <= int k)

    age :: Word32 -> Int
    age cp = int h - int cp

int :: Integral a => a -> Int
int = fromIntegral

pp :: ProtocolParameters
pp = dummyProtocolParameters

newtype GenSparseCheckpointsArgs
    = GenSparseCheckpointsArgs (Word32, Word32)
    deriving newtype Show

instance Arbitrary GenSparseCheckpointsArgs where
    arbitrary = do
        k <- (\x -> 10 + (x `mod` 1000)) <$> arbitrary
        h <- (`mod` 100000) <$> arbitrary
        pure $ GenSparseCheckpointsArgs ( k, h )
