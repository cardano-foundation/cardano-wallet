{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Cardano.Wallet.DB.Properties
    ( properties
    , withDB
    , namedPick
    , assertWith
    ) where

import Prelude

import Cardano.Wallet.DB
    ( DBLayer (..)
    , ErrNoSuchWallet (..)
    , ErrWalletAlreadyExists (..)
    , PrimaryKey (..)
    , cleanDB
    )
import Cardano.Wallet.DB.Arbitrary
    ( GenState, GenTxHistory (..), KeyValPairs (..), MockChain (..) )
import Cardano.Wallet.DB.Model
    ( filterTxHistory )
import Cardano.Wallet.DummyTarget.Primitive.Types
    ( DummyTarget )
import Cardano.Wallet.Primitive.AddressDerivation.Sequential
    ( SeqKey (..) )
import Cardano.Wallet.Primitive.Model
    ( BlockchainParameters (..)
    , Wallet
    , applyBlock
    , blockchainParameters
    , currentTip
    )
import Cardano.Wallet.Primitive.Types
    ( Direction (..)
    , Hash (..)
    , ShowFmt (..)
    , SlotId (..)
    , SortOrder (..)
    , Tx (..)
    , TxMeta (..)
    , TxStatus (..)
    , WalletId (..)
    , WalletMetadata (..)
    , isPending
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
import Control.Monad.Trans.Except
    ( ExceptT, runExceptT )
import Data.Bifunctor
    ( bimap )
import Data.Functor
    ( ($>) )
import Data.Functor.Identity
    ( Identity (..) )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.List
    ( unfoldr )
import Data.Maybe
    ( mapMaybe )
import Data.Quantity
    ( Quantity (..) )
import Data.Set
    ( Set )
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
    , (==>)
    )
import Test.QuickCheck.Monadic
    ( PropertyM, assert, monadicIO, monitor, pick, run )

import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- | Provide a DBLayer to a Spec that requires it. The database is initialised
-- once, and cleared with 'cleanDB' before each test.
withDB :: IO (DBLayer IO s t k) -> SpecWith (DBLayer IO s t k) -> Spec
withDB create = beforeAll create . beforeWith (\db -> cleanDB db $> db)

properties
    :: (GenState s, Eq s)
    => SpecWith (DBLayer IO s DummyTarget SeqKey)
properties = do
    describe "Extra Properties about DB initialization" $ do
        it "createWallet . listWallets yields expected results"
            (property . prop_createListWallet)
        it "creating same wallet twice yields an error"
            (property . prop_createWalletTwice)
        it "removing the same wallet twice yields an error"
            (property . prop_removeWalletTwice)

    describe "put . read yields a result" $ do
        it "Checkpoint"
            (property . (prop_readAfterPut putCheckpoint readCheckpoint))
        it "Wallet Metadata"
            (property . (prop_readAfterPut putWalletMeta readWalletMeta))
        it "Tx History"
            (property . (prop_readAfterPut putTxHistoryF readTxHistoryF))
        it "Private Key"
            (property . (prop_readAfterPut putPrivateKey readPrivateKey))

    describe "can't put before wallet exists" $ do
        it "Checkpoint"
            (property . (prop_putBeforeInit putCheckpoint readCheckpoint Nothing))
        it "Wallet Metadata"
            (property . (prop_putBeforeInit putWalletMeta readWalletMeta Nothing))
        it "Tx History"
            (property . (prop_putBeforeInit putTxHistoryF readTxHistoryF (pure mempty)))
        it "Private Key"
            (property . (prop_putBeforeInit putPrivateKey readPrivateKey Nothing))

    describe "put doesn't affect other resources" $ do
        it "Checkpoint vs Wallet Metadata & Tx History & Private Key"
            (property . (prop_isolation putCheckpoint
                readWalletMeta
                readTxHistoryF
                readPrivateKey)
            )
        it "Wallet Metadata vs Tx History & Checkpoint & Private Key"
            (property . (prop_isolation putWalletMeta
                readTxHistoryF
                readCheckpoint
                readPrivateKey)
            )
        it "Tx History vs Checkpoint & Wallet Metadata & Private Key"
            (property . (prop_isolation putTxHistoryF
                readCheckpoint
                readWalletMeta
                readPrivateKey)
            )

    describe "can't read after delete" $ do
        it "Checkpoint"
            (property . (prop_readAfterDelete readCheckpoint Nothing))
        it "Wallet Metadata"
            (property . (prop_readAfterDelete readWalletMeta Nothing))
        it "Tx History"
            (property . (prop_readAfterDelete readTxHistoryF (pure mempty)))
        it "Private Key"
            (property . (prop_readAfterDelete readPrivateKey Nothing))

    describe "sequential puts replace values in order" $ do
        it "Checkpoint"
            (checkCoverage . (prop_sequentialPut putCheckpoint readCheckpoint lrp))
        it "Wallet Metadata"
            (checkCoverage . (prop_sequentialPut putWalletMeta readWalletMeta lrp))
        it "Tx History"
            (checkCoverage . (prop_sequentialPut putTxHistoryF readTxHistoryF sortedUnions))
        it "Private Key"
            (checkCoverage . (prop_sequentialPut putPrivateKey readPrivateKey lrp))

    describe "parallel puts replace values in _any_ order" $ do
        it "Checkpoint"
            (checkCoverage . (prop_parallelPut putCheckpoint readCheckpoint
                (length . lrp @Maybe)))
        it "Wallet Metadata"
            (checkCoverage . (prop_parallelPut putWalletMeta readWalletMeta
                (length . lrp @Maybe)))
        it "Tx History"
            (checkCoverage . (prop_parallelPut putTxHistoryF readTxHistoryF
                (length . sortedUnions)))
        it "Private Key"
            (checkCoverage . (prop_parallelPut putPrivateKey readPrivateKey
                (length . lrp @Maybe)))


    describe "rollback" $ do
        it "Can rollback to any arbitrary known checkpoint"
            (property . prop_rollbackCheckpoint)
        it "Correctly re-construct tx history on rollbacks"
            (checkCoverage . prop_rollbackTxHistory)

-- | Wrap the result of 'readTxHistory' in an arbitrary identity Applicative
readTxHistoryF
    :: Functor m
    => DBLayer m s DummyTarget SeqKey
    -> PrimaryKey WalletId
    -> m (Identity GenTxHistory)
readTxHistoryF db wid =
    (Identity . GenTxHistory)
        <$> readTxHistory db wid Descending wholeRange Nothing

putTxHistoryF
    :: DBLayer m s DummyTarget SeqKey
    -> PrimaryKey WalletId
    -> GenTxHistory
    -> ExceptT ErrNoSuchWallet m ()
putTxHistoryF db wid =
    putTxHistory db wid . unGenTxHistory

{-------------------------------------------------------------------------------
                                       Utils
-------------------------------------------------------------------------------}

-- | Keep only the
-- (L)ast (R)ecently (P)ut entry
lrp :: (Applicative f, Ord k) => [(k, v)] -> [f v]
lrp = Map.elems . foldl (\m (k, v) -> Map.insert k (pure v) m) mempty

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
      . filterTxHistory @DummyTarget Descending wholeRange
      . unGenTxHistory

-- | Execute an action once per key @k@ present in the given list
once :: (Ord k, Monad m) => [(k,v)] -> ((k,v) -> m a) -> m [a]
once xs = forM (Map.toList (Map.fromList xs))

-- | Like 'once', but discards the result
once_ :: (Ord k, Monad m) => [(k,v)] -> ((k,v) -> m a) -> m ()
once_ xs = void . once xs

-- | Filter a transaction list according to the given predicate, returns their
-- ids.
filterTxs
    :: forall t. (DefineTx t)
    => (TxMeta -> Bool)
    -> [(Tx t, TxMeta)]
    -> [Hash "Tx"]
filterTxs predicate = mapMaybe fn
  where
    fn (tx, meta) = if predicate meta then Just (txId @t tx) else Nothing

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
    :: DBLayer IO s DummyTarget SeqKey
    -> KeyValPairs (PrimaryKey WalletId) (Wallet s DummyTarget, WalletMetadata)
    -> Property
prop_createListWallet db (KeyValPairs pairs) =
    monadicIO (setup >> prop)
  where
    setup = liftIO (cleanDB db)
    prop = liftIO $ do
        res <- once pairs $ \(k, (cp, meta)) ->
            unsafeRunExceptT $ createWallet db k cp meta mempty
        (length <$> listWallets db) `shouldReturn` length res

-- | Trying to create a same wallet twice should yield an error
prop_createWalletTwice
    :: DBLayer IO s DummyTarget SeqKey
    -> ( PrimaryKey WalletId
       , Wallet s DummyTarget
       , WalletMetadata
       )
    -> Property
prop_createWalletTwice db (key@(PrimaryKey wid), cp, meta) =
    monadicIO (setup >> prop)
  where
    setup = liftIO (cleanDB db)
    prop = liftIO $ do
        let err = ErrWalletAlreadyExists wid
        runExceptT (createWallet db key cp meta mempty) `shouldReturn` Right ()
        runExceptT (createWallet db key cp meta mempty) `shouldReturn` Left err

-- | Trying to remove a same wallet twice should yield an error
prop_removeWalletTwice
    :: DBLayer IO s DummyTarget SeqKey
    -> ( PrimaryKey WalletId
       , Wallet s DummyTarget
       , WalletMetadata
       )
    -> Property
prop_removeWalletTwice db (key@(PrimaryKey wid), cp, meta) =
    monadicIO (setup >> prop)
  where
    setup = liftIO $ do
        cleanDB db
        unsafeRunExceptT $ createWallet db key cp meta mempty
    prop = liftIO $ do
        let err = ErrNoSuchWallet wid
        runExceptT (removeWallet db key) `shouldReturn` Right ()
        runExceptT (removeWallet db key) `shouldReturn` Left err

-- | Checks that a given resource can be read after having been inserted in DB.
prop_readAfterPut
    :: ( Buildable (f a), Eq (f a), Applicative f, GenState s )
    => (  DBLayer IO s DummyTarget SeqKey
       -> PrimaryKey WalletId
       -> a
       -> ExceptT ErrNoSuchWallet IO ()
       ) -- ^ Put Operation
    -> (  DBLayer IO s DummyTarget SeqKey
       -> PrimaryKey WalletId
       -> IO (f a)
       ) -- ^ Read Operation
    -> DBLayer IO s DummyTarget SeqKey
    -> (ShowFmt (PrimaryKey WalletId), ShowFmt a)
        -- ^ Property arguments
    -> Property
prop_readAfterPut putOp readOp db (ShowFmt key, ShowFmt a) =
    monadicIO (setup >> prop)
  where
    setup = do
        liftIO (cleanDB db)
        (cp, meta) <- pick arbitrary
        liftIO $ unsafeRunExceptT $ createWallet db key cp meta mempty
    prop = liftIO $ do
        unsafeRunExceptT $ putOp db key a
        res <- readOp db key
        ShowFmt res `shouldBe` ShowFmt (pure a)

-- | Can't put resource before a wallet has been initialized
prop_putBeforeInit
    :: (Buildable (f a), Eq (f a))
    => (  DBLayer IO s DummyTarget SeqKey
       -> PrimaryKey WalletId
       -> a
       -> ExceptT ErrNoSuchWallet IO ()
       ) -- ^ Put Operation
    -> (  DBLayer IO s DummyTarget SeqKey
       -> PrimaryKey WalletId
       -> IO (f a)
       ) -- ^ Read Operation
    -> f a
        -- ^ An 'empty' value for the 'Applicative' f
    -> DBLayer IO s DummyTarget SeqKey
    -> (ShowFmt (PrimaryKey WalletId), ShowFmt a)
        -- ^ Property arguments
    -> Property
prop_putBeforeInit putOp readOp empty db (ShowFmt (key@(PrimaryKey wid)), ShowFmt a) =
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
       , Arbitrary (Wallet s DummyTarget)
       )
    => (  DBLayer IO s DummyTarget SeqKey
       -> PrimaryKey WalletId
       -> a
       -> ExceptT ErrNoSuchWallet IO ()
       ) -- ^ Put Operation
    -> (  DBLayer IO s DummyTarget SeqKey
       -> PrimaryKey WalletId
       -> IO (f b)
       ) -- ^ Read Operation for another resource
    -> (  DBLayer IO s DummyTarget SeqKey
       -> PrimaryKey WalletId
       -> IO (g c)
       ) -- ^ Read Operation for another resource
    -> (  DBLayer IO s DummyTarget SeqKey
       -> PrimaryKey WalletId
       -> IO (h d)
       ) -- ^ Read Operation for another resource
    -> DBLayer IO s DummyTarget SeqKey
    -> (ShowFmt (PrimaryKey WalletId), ShowFmt a)
        -- ^ Properties arguments
    -> Property
prop_isolation putA readB readC readD db (ShowFmt key, ShowFmt a) =
    monadicIO (setup >>= prop)
  where
    setup = do
        liftIO (cleanDB db)
        (cp, meta, GenTxHistory txs) <- pick arbitrary
        liftIO $ unsafeRunExceptT $ createWallet db key cp meta mempty
        liftIO $ unsafeRunExceptT $ putTxHistory db key txs
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
    => (  DBLayer IO s DummyTarget SeqKey
       -> PrimaryKey WalletId
       -> IO (f a)
       ) -- ^ Read Operation
    -> f a
        -- ^ An 'empty' value for the 'Applicative' f
    -> DBLayer IO s DummyTarget SeqKey
    -> ShowFmt (PrimaryKey WalletId)
    -> Property
prop_readAfterDelete readOp empty db (ShowFmt key) =
    monadicIO (setup >> prop)
  where
    setup = do
        liftIO (cleanDB db)
        (cp, meta) <- pick arbitrary
        liftIO $ unsafeRunExceptT $ createWallet db key cp meta mempty
    prop = liftIO $ do
        unsafeRunExceptT $ removeWallet db key
        (ShowFmt <$> readOp db key) `shouldReturn` ShowFmt empty

-- | Check that the DB supports multiple sequential puts for a given resource
prop_sequentialPut
    :: (Buildable (f a), Eq (f a), GenState s)
    => (  DBLayer IO s DummyTarget SeqKey
       -> PrimaryKey WalletId
       -> a
       -> ExceptT ErrNoSuchWallet IO ()
       ) -- ^ Put Operation
    -> (  DBLayer IO s DummyTarget SeqKey
       -> PrimaryKey WalletId
       -> IO (f a)
       ) -- ^ Read Operation
    -> (forall k. Ord k => [(k, a)] -> [f a])
        -- ^ How do we expect operations to resolve
    -> DBLayer IO s DummyTarget SeqKey
    -> KeyValPairs (ShowFmt (PrimaryKey WalletId)) (ShowFmt a)
        -- ^ Property arguments
    -> Property
prop_sequentialPut putOp readOp resolve db kv =
    cover 25 cond "conflicting db entries" $ monadicIO (setup >> prop)
  where
    pairs = (\(KeyValPairs xs) -> bimap unShowFmt unShowFmt <$> xs) kv
    -- Make sure that we have some conflicting insertion to actually test the
    -- semantic of the DB Layer.
    cond = L.length (L.nub ids) /= L.length ids
      where
        ids = map fst pairs
    setup = do
        liftIO (cleanDB db)
        (cp, meta) <- pick arbitrary
        liftIO $ unsafeRunExceptT $ once_ pairs $ \(k, _) ->
            createWallet db k cp meta mempty
    prop = liftIO $ do
        unsafeRunExceptT $ forM_ pairs $ uncurry (putOp db)
        res <- once pairs (readOp db . fst)
        (ShowFmt <$> res) `shouldBe` (ShowFmt <$> resolve pairs)

-- | Check that the DB supports multiple sequential puts for a given resource
prop_parallelPut
    :: (Arbitrary (Wallet s DummyTarget))
    => (  DBLayer IO s DummyTarget SeqKey
       -> PrimaryKey WalletId
       -> a
       -> ExceptT ErrNoSuchWallet IO ()
       ) -- ^ Put Operation
    -> (  DBLayer IO s DummyTarget SeqKey
       -> PrimaryKey WalletId
       -> IO (f a)
       ) -- ^ Read Operation
    -> (forall k. Ord k => [(k, a)] -> Int)
        -- ^ How many entries to we expect in the end
    -> DBLayer IO s DummyTarget SeqKey
    -> KeyValPairs (PrimaryKey WalletId) a
        -- ^ Property arguments
    -> Property
prop_parallelPut putOp readOp resolve db (KeyValPairs pairs) =
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
        liftIO $ unsafeRunExceptT $ once_ pairs $ \(k, _) ->
            createWallet db k cp meta mempty
    prop = liftIO $ do
        forConcurrently_ pairs $ unsafeRunExceptT . uncurry (putOp db)
        res <- once pairs (readOp db . fst)
        length res `shouldBe` resolve pairs


-- | Can rollback to any particular checkpoint previously stored (within the
-- unstable zone. We can't rollback for more than `k=epochStability`)
prop_rollbackCheckpoint
    :: forall s t k. (t ~ DummyTarget, GenState s, Eq s)
    => DBLayer IO s t k
    -> ShowFmt (Wallet s t)
    -> ShowFmt MockChain
    -> Property
prop_rollbackCheckpoint db (ShowFmt cp0) (ShowFmt (MockChain chain)) = do
    L.length chain < fromIntegral k ==> monadicIO $ do
        ShowFmt wid <- namedPick "Wallet ID" arbitrary
        ShowFmt meta <- namedPick "Wallet Metadata" arbitrary
        ShowFmt point <- namedPick "Rollback target" (elements $ ShowFmt <$> cps)
        setup wid meta >> prop wid point
  where
    (Quantity k) = getEpochStability (blockchainParameters cp0)

    cps :: [Wallet s t]
    cps = flip unfoldr (chain, cp0) $ \case
        ([], _) -> Nothing
        (b:q, cp) -> let cp' = snd $ applyBlock b cp in Just (cp', (q, cp'))

    setup wid meta = run $ do
        cleanDB db
        unsafeRunExceptT $ createWallet db wid cp0 meta mempty
        unsafeRunExceptT $ forM_ cps (putCheckpoint db wid)

    prop wid point = do
        let tip = currentTip point
        run $ unsafeRunExceptT $ rollbackTo db wid (tip ^. #slotId)
        cp <- run $ readCheckpoint db wid
        let str = maybe "∅" pretty cp
        monitor $ counterexample ("Checkpoint after rollback: \n" <> str)
        assert (ShowFmt cp == ShowFmt (pure point))

-- | Re-schedule pending transaction on rollback, i.e.:
--
-- (PoR = Point of Rollback)
--
-- - There's no transaction beyond the PoR
-- - Any incoming transaction after the PoR is forgotten
-- - Any outgoing transaction after the PoR is back in pending, and have a slot
--   equal to the PoR.
prop_rollbackTxHistory
    :: forall s t k. (t ~ DummyTarget)
    => DBLayer IO s t k
    -> ShowFmt (Wallet s t)
    -> ShowFmt GenTxHistory
    -> Property
prop_rollbackTxHistory db (ShowFmt cp0) (ShowFmt (GenTxHistory txs0)) = do
    monadicIO $ do
        ShowFmt wid <- namedPick "Wallet ID" arbitrary
        ShowFmt meta <- namedPick "Wallet Metadata" arbitrary
        ShowFmt point <- namedPick "Rollback point" arbitrary
        let ixs = rescheduled point
        monitor $ label ("Outgoing tx after point: " <> show (L.length ixs))
        monitor $ cover 50 (not $ null ixs) "rolling back something"
        setup wid meta >> prop wid point
  where
    setup wid meta = run $ do
        cleanDB db
        unsafeRunExceptT $ createWallet db wid cp0 meta mempty
        unsafeRunExceptT $ putTxHistory db wid txs0

    prop wid point = do
        run $ unsafeRunExceptT $ rollbackTo db wid point
        txs <- run $ readTxHistory db wid Descending wholeRange Nothing
        monitor $ counterexample $ "\nTx history after rollback: \n" <> fmt txs

        assertWith "Outgoing txs are reschuled" $
            L.sort (rescheduled point) == L.sort (filterTxs @t isPending txs)
        assertWith "All other txs are still known" $
            L.sort (knownAfterRollback point) == L.sort (txId @t . fst <$> txs)
        assertWith "All txs are now before the point of rollback" $
            all (isBefore point . snd) txs
      where
        fmt = pretty . GenTxHistory

    isBefore :: SlotId -> TxMeta -> Bool
    isBefore point meta =
        (slotId :: TxMeta -> SlotId) meta <= point

    rescheduled :: SlotId -> [Hash "Tx"]
    rescheduled point =
        let addedAfter meta = direction meta == Outgoing && slotId meta > point
        in filterTxs @t (\tx -> addedAfter tx || isPending tx) txs0

    knownAfterRollback :: SlotId -> [Hash "Tx"]
    knownAfterRollback point =
        rescheduled point ++ filterTxs @t (isBefore point) txs0
