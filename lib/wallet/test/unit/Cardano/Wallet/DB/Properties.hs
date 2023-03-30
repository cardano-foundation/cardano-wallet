{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
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
{-# LANGUAGE TypeFamilies #-}

-- TODO: https://input-output.atlassian.net/browse/ADP-2841
#if __GLASGOW_HASKELL__ >= 902
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}
#endif

module Cardano.Wallet.DB.Properties
    ( properties
    ) where

import Prelude

import Cardano.Wallet.DB
    ( DBLayer (..)
    , ErrWalletAlreadyInitialized (ErrWalletAlreadyInitialized)
    , ErrWalletNotInitialized (ErrWalletNotInitialized)
    )
import Cardano.Wallet.DB.Arbitrary
    ( GenState, GenTxHistory (..), InitialCheckpoint (..), MockChain (..) )
import Cardano.Wallet.DB.Layer
    ( PersistAddressBook )
import Cardano.Wallet.DB.Pure.Implementation
    ( filterTxHistory )
import Cardano.Wallet.DummyTarget.Primitive.Types
    ( dummyGenesisParameters )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( ShelleyKey (..) )
import Cardano.Wallet.Primitive.Model
    ( Wallet (currentTip), applyBlock, currentTip )
import Cardano.Wallet.Primitive.Types
    ( ChainPoint (..)
    , GenesisParameters
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
    ( TransactionInfo (..), Tx (..), TxMeta (..), toTxHistory )
import Cardano.Wallet.Unsafe
    ( unsafeRunExceptT )
import Cardano.Wallet.Util
    ( ShowFmt (..) )
import Control.Monad
    ( forM_, when )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Monad.Trans.Except
    ( ExceptT, mapExceptT, runExceptT, withExceptT )
import Data.Foldable
    ( fold )
import Data.Functor.Identity
    ( Identity (..) )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.Generics.Labels
    ()
import Data.List
    ( sortOn, unfoldr )
import Data.Maybe
    ( isNothing, mapMaybe )
import Fmt
    ( Buildable, pretty )
import Test.Hspec
    ( SpecWith, describe, it, shouldBe, shouldReturn )
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
    )
import Test.QuickCheck.Monadic
    ( PropertyM, assert, monadicIO, monitor, pick, run )

import qualified Data.List as L

-- | How to boot a fresh database.
type WithFreshDB s =
    (DBLayer IO s ShelleyKey -> PropertyM IO ())
    -> PropertyM IO ()

withFreshWallet
    :: GenState s
    => WithFreshDB s
    -> (DBLayer IO s ShelleyKey -> WalletId -> PropertyM IO ())
    -> PropertyM IO ()
withFreshWallet withFreshDB f = withFreshDB $ \db@DBLayer {..} -> do
    (InitialCheckpoint cp0, meta, wid) <- pick arbitrary
    run
        $ atomically
        $ unsafeRunExceptT
        $ initializeWallet wid cp0 meta mempty gp
    f db wid

type TestOnLayer s =
    ( DBLayer IO s ShelleyKey
      -> WalletId
      -> PropertyM IO ()
    )
    -> Property

-- | Wallet properties.
properties
    :: (GenState s, PersistAddressBook s)
    => WithFreshDB s
    -> SpecWith ()
properties withFreshDB = describe "DB.Properties" $ do
    let testOnLayer = monadicIO . withFreshWallet withFreshDB

    describe "Extra Properties about DB initialization" $ do
        it "creating same wallet twice yields an error"
            $ property
            $ prop_createWalletTwice withFreshDB

    describe "put . read yields a result" $ do
        it "Checkpoint"
            $ property
            $ prop_readAfterPut
                testOnLayer
                (\DBLayer{..} wid -> mapExceptT atomically . putCheckpoint wid)
                (\DBLayer{..} -> atomically . readCheckpoint)
        it "Wallet Metadata"
            $ property
            $ prop_readAfterPut
                testOnLayer
                (\DBLayer{..} wid -> mapExceptT atomically . putWalletMeta wid)
                (\DBLayer{..} -> atomically . fmap (fmap fst) . readWalletMeta)
        it "Tx History"
            $ property
            $ prop_readAfterPut
                testOnLayer
                putTxHistory_
                readTxHistory_
        it "Private Key"
            $ property
            $ prop_readAfterPut
                testOnLayer
                (\DBLayer{..} wid -> mapExceptT atomically . putPrivateKey wid)
                (\DBLayer{..} -> atomically . readPrivateKey)

    describe "getTx properties" $ do
        it "can read after putting tx history for valid tx id"
            $ property
            $ prop_getTxAfterPutValidTxId testOnLayer
        it "cannot read after putting tx history for invalid tx id"
            $ property
            $ prop_getTxAfterPutInvalidTxId testOnLayer
        it "cannot read after putting tx history for invalid wallet id"
            $ property
            $ prop_getTxAfterPutInvalidWalletId testOnLayer

    describe "can't put before wallet exists" $ do
        it "Checkpoint"
            $ property
            $ prop_putBeforeInit
                withFreshDB
                (\DBLayer{..} wid -> mapExceptT atomically . putCheckpoint wid)
                (\DBLayer{..} -> atomically . readCheckpoint)
                Nothing
        it "Wallet Metadata"
            $ property
            $ prop_putBeforeInit
                withFreshDB
                (\DBLayer{..} wid -> mapExceptT atomically . putWalletMeta wid)
                (\DBLayer{..} -> atomically . fmap (fmap fst) . readWalletMeta)
                Nothing
        it "Tx History"
            $ property
            $ prop_putBeforeInit
                withFreshDB
                putTxHistory_
                readTxHistory_
                (pure mempty)
        it "Private Key"
            $ property
            $ prop_putBeforeInit
                withFreshDB
                (\DBLayer{..} wid -> mapExceptT atomically . putPrivateKey wid)
                (\DBLayer{..} -> atomically . readPrivateKey)
                Nothing

    describe "put doesn't affect other resources" $ do
        it "Checkpoint vs Wallet Metadata & Tx History & Private Key"
            $ property
            $ prop_isolation
                testOnLayer
                (\DBLayer{..} wid -> mapExceptT atomically . putCheckpoint wid)
                (\DBLayer{..} -> atomically . readWalletMeta)
                readTxHistory_
                (\DBLayer{..} -> atomically . readPrivateKey)
        it "Wallet Metadata vs Tx History & Checkpoint & Private Key"
            $ property
            $ prop_isolation
                testOnLayer
                (\DBLayer{..} wid -> mapExceptT atomically . putWalletMeta wid)
                readTxHistory_
                (\DBLayer{..} -> atomically . readCheckpoint)
                (\DBLayer{..} -> atomically . readPrivateKey)
        it "Tx History vs Checkpoint & Wallet Metadata & Private Key"
            $ property
            $ prop_isolation
                testOnLayer
                putTxHistory_
                (\DBLayer{..} -> atomically . readCheckpoint)
                (\DBLayer{..} -> atomically . readWalletMeta)
                (\DBLayer{..} -> atomically . readPrivateKey)

    let lastMay [] = Nothing
        lastMay xs = Just (last xs)
    describe "sequential puts replace values in order" $ do
        it "Checkpoint"
            $ checkCoverage
            $ prop_sequentialPut
                testOnLayer
                (\DBLayer{..} wid -> mapExceptT atomically . putCheckpoint wid)
                (\DBLayer{..} -> atomically . readCheckpoint)
                lastMay
                . sortOn (currentTip . unShowFmt)
        it "Wallet Metadata"
            $ checkCoverage
            $ prop_sequentialPut
                testOnLayer
                (\DBLayer{..} wid -> mapExceptT atomically . putWalletMeta wid)
                (\DBLayer{..} -> atomically . fmap (fmap fst) . readWalletMeta)
                lastMay
        it "Tx History"
            $ checkCoverage
            $ prop_sequentialPut
                testOnLayer
                putTxHistory_
                readTxHistory_
                ( let sort' =
                        GenTxHistory
                            . filterTxHistory Nothing Descending wholeRange
                            . unGenTxHistory
                  in  Identity . sort' . fold
                )
        it "Private Key"
            $ checkCoverage
            $ prop_sequentialPut
                testOnLayer
                (\DBLayer{..} wid -> mapExceptT atomically . putPrivateKey wid)
                (\DBLayer{..} -> atomically . readPrivateKey)
                lastMay

    describe "rollback" $ do
        it
            "Can rollback to any arbitrary known checkpoint"
            (property $ prop_rollbackCheckpoint withFreshDB)
        it
            "Correctly re-construct tx history on rollbacks"
            (checkCoverage $ prop_rollbackTxHistory withFreshDB)

-- | Wrap the result of 'readTransactions' in an arbitrary identity Applicative
readTxHistory_
    :: Functor m
    => DBLayer m s ShelleyKey
    -> WalletId
    -> m (Identity GenTxHistory)
readTxHistory_ DBLayer {..} wid =
    (Identity . GenTxHistory . fmap toTxHistory)
        <$> atomically
            (readTransactions wid Nothing Descending wholeRange Nothing Nothing)

putTxHistory_
    :: Functor m
    => DBLayer m s ShelleyKey
    -> WalletId
    -> GenTxHistory
    -> ExceptT ErrWalletNotInitialized m ()
putTxHistory_ DBLayer {..} wid =
    withExceptT (const ErrWalletNotInitialized)
        . mapExceptT atomically
        . putTxHistory wid
        . unGenTxHistory

{-------------------------------------------------------------------------------
                                       Utils
-------------------------------------------------------------------------------}

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

-- | Trying to create a same wallet twice should yield an error
prop_createWalletTwice
    :: WithFreshDB s
    -> ( WalletId
       , InitialCheckpoint s
       , WalletMetadata
       )
    -> Property
prop_createWalletTwice test (wid, InitialCheckpoint cp0, meta) = monadicIO
    $ test
    $ \DBLayer {..} -> do
        liftIO $ do
            let err = ErrWalletAlreadyInitialized
            atomically (runExceptT $ initializeWallet wid cp0 meta mempty gp)
                `shouldReturn` Right ()
            atomically (runExceptT $ initializeWallet wid cp0 meta mempty gp)
                `shouldReturn` Left err

-- | Checks that a given resource can be read after having been inserted in DB.
prop_readAfterPut
    :: (Buildable (f a), Eq (f a), Applicative f, GenState s)
    => TestOnLayer s
    -> ( DBLayer IO s ShelleyKey
         -> WalletId
         -> a
         -> ExceptT ErrWalletNotInitialized IO ()
       )
    -- ^ Put Operation
    -> ( DBLayer IO s ShelleyKey
         -> WalletId
         -> IO (f a)
       )
    -- ^ Read Operation
    -> a
    -- ^ Property arguments
    -> Property
prop_readAfterPut test putOp readOp a = test $ \db wid -> do
    run $ unsafeRunExceptT $ putOp db wid a
    res <- run $ readOp db wid
    let fa = pure a
    monitor $ counterexample $ "\nInserted\n" <> pretty fa
    monitor $ counterexample $ "\nRead\n" <> pretty res
    assertWith "Inserted == Read" (res == fa)

prop_getTxAfterPutValidTxId
    :: GenState s
    => TestOnLayer s
    -> GenTxHistory
    -> Property
prop_getTxAfterPutValidTxId test txGen = test $ \DBLayer {..} wid -> do
    let txs = unGenTxHistory txGen
    run $ unsafeRunExceptT $ mapExceptT atomically $ putTxHistory wid txs
    forM_ txs $ \(Tx {txId}, txMeta) -> do
        (Just (TransactionInfo {txInfoId, txInfoMeta})) <-
            run $ atomically $ unsafeRunExceptT $ getTx wid txId
        monitor
            $ counterexample
            $ "\nInserted\n"
                <> pretty txMeta
                <> " for txId: "
                <> pretty txId
        monitor
            $ counterexample
            $ "\nRead\n"
                <> pretty txInfoMeta
                <> " for txId: "
                <> pretty txInfoId
        assertWith
            "Inserted is included in Read"
            (txMeta == txInfoMeta && txId == txInfoId)

prop_getTxAfterPutInvalidTxId
    :: GenState s
    => TestOnLayer s
    -> GenTxHistory
    -> (Hash "Tx")
    -> Property
prop_getTxAfterPutInvalidTxId test txGen txId' = test $ \DBLayer {..} wid -> do
    let txs = unGenTxHistory txGen
    run $ unsafeRunExceptT $ mapExceptT atomically $ putTxHistory wid txs
    res <- run $ atomically $ unsafeRunExceptT $ getTx wid txId'
    assertWith
        "Irrespective of Inserted, Read is Nothing for invalid tx id"
        (isNothing res)

prop_getTxAfterPutInvalidWalletId
    :: TestOnLayer s
    -> WalletId
    -> GenTxHistory
    -> Property
prop_getTxAfterPutInvalidWalletId test wid txGen =
    test $ \DBLayer {..} wid' -> liftIO $ do
        let txs = unGenTxHistory txGen
        atomically (runExceptT $ putTxHistory wid' txs) `shouldReturn` Right ()
        forM_ txs $ \(Tx {txId}, _) -> do
            let err = ErrWalletNotInitialized
            when (wid /= wid')
                $ atomically (runExceptT $ getTx wid txId)
                `shouldReturn` Left err

-- | Can't put resource before a wallet has been initialized
prop_putBeforeInit
    :: (Buildable (f a), Eq (f a), GenState s)
    => WithFreshDB s
    -> ( DBLayer IO s ShelleyKey
         -> WalletId
         -> a
         -> ExceptT ErrWalletNotInitialized IO ()
       )
    -- ^ Put Operation
    -> ( DBLayer IO s ShelleyKey
         -> WalletId
         -> IO (f a)
       )
    -- ^ Read Operation
    -> f a
    -- ^ An 'empty' value for the 'Applicative' f
    -> (WalletId, a)
    -- ^ Property arguments
    -> Property
prop_putBeforeInit test putOp readOp empty (wid, a) = monadicIO $ test $ \db ->
    liftIO $ do
        runExceptT (putOp db wid a) >>= \case
            Right _ ->
                fail "expected put operation to fail but it succeeded!"
            Left err ->
                err `shouldBe` ErrWalletNotInitialized
        (ShowFmt <$> readOp db wid) `shouldReturn` (ShowFmt empty)

-- | Modifying one resource leaves the other untouched
prop_isolation
    :: ( Buildable (f b)
       , Eq (f b)
       , Buildable (g c)
       , Eq (g c)
       , Buildable (h d)
       , Eq (h d)
       , GenState s
       , Show s
       , PersistAddressBook s
       )
    => TestOnLayer s
    -> ( DBLayer IO s ShelleyKey
         -> WalletId
         -> a
         -> ExceptT ErrWalletNotInitialized IO ()
       )
    -- ^ Put Operation
    -> ( DBLayer IO s ShelleyKey
         -> WalletId
         -> IO (f b)
       )
    -- ^ Read Operation for another resource
    -> ( DBLayer IO s ShelleyKey
         -> WalletId
         -> IO (g c)
       )
    -- ^ Read Operation for another resource
    -> ( DBLayer IO s ShelleyKey
         -> WalletId
         -> IO (h d)
       )
    -- ^ Read Operation for another resource
    -> ShowFmt a
    -- ^ Properties arguments
    -> Property
prop_isolation test putA readB readC readD (ShowFmt a) =
    test $ \db@DBLayer {..} wid -> do
        (GenTxHistory txs) <- pick arbitrary
        run $ atomically $ do
            unsafeRunExceptT $ putTxHistory wid txs
        (b, c, d) <-
            run
                $ (,,)
                    <$> readB db wid
                    <*> readC db wid
                    <*> readD db wid
        liftIO $ do
            unsafeRunExceptT $ putA db wid a
            (ShowFmt <$> readB db wid) `shouldReturn` ShowFmt b
            (ShowFmt <$> readC db wid) `shouldReturn` ShowFmt c
            (ShowFmt <$> readD db wid) `shouldReturn` ShowFmt d

-- | Check that the DB supports multiple sequential puts for a given resource
prop_sequentialPut
    :: (Buildable (f a), Eq (f a), GenState s, PersistAddressBook s)
    => TestOnLayer s
    -> ( DBLayer IO s ShelleyKey
         -> WalletId
         -> a
         -> ExceptT ErrWalletNotInitialized IO ()
       )
    -- ^ Put Operation
    -> ( DBLayer IO s ShelleyKey
         -> WalletId
         -> IO (f a)
       )
    -- ^ Read Operation
    -> ([a] -> f a)
    -- ^ How do we expect operations to resolve
    -> [ShowFmt a]
    -- ^ Property arguments
    -> Property
prop_sequentialPut _ _ _ _ [] = property True
prop_sequentialPut test putOp readOp resolve as =
    test $ \db wid -> do
        let ops = unShowFmt <$> as
        run $ unsafeRunExceptT $ forM_ ops $ putOp db wid
        res <- run $ readOp db wid
        let resolved = resolve ops
        monitor $ counterexample $ "\nResolved\n" <> pretty resolved
        monitor $ counterexample $ "\nRead\n" <> pretty res
        assertWith "Resolved == Read" (res == resolved)

-- | Can rollback to any particular checkpoint previously stored
prop_rollbackCheckpoint
    :: forall s
     . (GenState s, Eq s)
    => WithFreshDB s
    -> InitialCheckpoint s
    -> MockChain
    -> Property
prop_rollbackCheckpoint test (InitialCheckpoint cp0) (MockChain chain) = monadicIO
    $ test
    $ \DBLayer {..} -> do
        let cps :: [Wallet s]
            cps = flip unfoldr (chain, cp0) $ \case
                ([], _) -> Nothing
                (b : q, cp) ->
                    let cp' = snd . snd $ applyBlock b cp in Just (cp', (q, cp'))
        ShowFmt wid <- namedPick "Wallet ID" arbitrary
        ShowFmt meta <- namedPick "Wallet Metadata" arbitrary
        ShowFmt point <- namedPick "Rollback target" (elements $ ShowFmt <$> cps)
        run $ atomically $ do
            unsafeRunExceptT $ initializeWallet wid cp0 meta mempty gp
            unsafeRunExceptT $ forM_ cps (putCheckpoint wid)
        let tip = currentTip point
        point' <-
            run
                $ atomically
                $ unsafeRunExceptT
                $ rollbackTo wid (toSlot $ chainPointFromBlockHeader tip)
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
-- - Any transaction after the PoR is forgotten
--
-- FIXME LATER: This function only tests slot numbers to roll back to,
-- not Slot. See note [PointSlotNo] for the difference.
-- The reason for this restriction is that the 'rollbackTo' function
-- from the DBLayer currently does not roll the TxHistory back correctly
-- if there is a rollback to genesis.
prop_rollbackTxHistory
    :: forall s
     . WithFreshDB s
    -> InitialCheckpoint s
    -> GenTxHistory
    -> Property
prop_rollbackTxHistory test (InitialCheckpoint cp0) (GenTxHistory txs0) = do
    monadicIO $ test $ \DBLayer {..} -> do
        ShowFmt wid <- namedPick "Wallet ID" arbitrary
        ShowFmt meta <- namedPick "Wallet Metadata" arbitrary
        ShowFmt requestedPoint <- namedPick "Requested Rollback slot" arbitrary
        let ixs = forgotten requestedPoint
        monitor $ label ("Forgotten tx after point: " <> show (L.length ixs))
        monitor $ cover 50 (not $ null ixs) "rolling back something"
        run $ atomically $ do
            unsafeRunExceptT $ initializeWallet wid cp0 meta mempty gp
            unsafeRunExceptT $ putTxHistory wid txs0
        point <-
            run
                $ unsafeRunExceptT
                $ mapExceptT atomically
                $ rollbackTo wid (At requestedPoint)
        txs <-
            run
                $ atomically
                $ fmap toTxHistory
                    <$> readTransactions
                        wid
                        Nothing
                        Descending
                        wholeRange
                        Nothing
                        Nothing

        monitor $ counterexample $ "\n" <> "Actual Rollback Point:\n" <> (pretty point)
        monitor $ counterexample $ "\nOriginal tx history:\n" <> (txsF txs0)
        monitor $ counterexample $ "\nNew tx history:\n" <> (txsF txs)

        let slot = pseudoSlotNo point
        assertWith "All txs before are still known"
            $ L.sort (knownAfterRollback slot) == L.sort (txId . fst <$> txs)
        assertWith "All txs are now before the point of rollback"
            $ all (isBefore slot . snd) txs
  where
    txsF :: [(Tx, TxMeta)] -> String
    txsF =
        L.intercalate "\n"
            . map
                ( \(tx, meta) ->
                    unwords
                        [ "- "
                        , pretty (txId tx)
                        , pretty (meta ^. #slotNo)
                        , pretty (meta ^. #status)
                        , pretty (meta ^. #direction)
                        , pretty (meta ^. #amount)
                        ]
                )

    pseudoSlotNo ChainPointAtGenesis = SlotNo 0
    pseudoSlotNo (ChainPoint slot _) = slot

    isBefore :: SlotNo -> TxMeta -> Bool
    isBefore slot meta = (slotNo :: TxMeta -> SlotNo) meta <= slot

    forgotten :: SlotNo -> [Hash "Tx"]
    forgotten slot =
        let isAfter meta = meta ^. #slotNo > slot
        in  filterTxs isAfter txs0

    knownAfterRollback :: SlotNo -> [Hash "Tx"]
    knownAfterRollback slot =
        filterTxs (isBefore slot) txs0

gp :: GenesisParameters
gp = dummyGenesisParameters
