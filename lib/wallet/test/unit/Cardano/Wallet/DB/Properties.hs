{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- TODO: https://cardanofoundation.atlassian.net/browse/ADP-2841
#if __GLASGOW_HASKELL__ >= 902
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}
#endif

module Cardano.Wallet.DB.Properties
    ( properties
    ) where

import Prelude

import Cardano.Wallet.DB
    ( DBFresh (..)
    , DBLayer (..)
    , DBLayerParams (..)
    , ErrWalletAlreadyInitialized (ErrWalletAlreadyInitialized)
    , ErrWalletNotInitialized
    )
import Cardano.Wallet.DB.Arbitrary
    ( GenState
    , GenTxHistory (..)
    , InitialCheckpoint (..)
    )
import Cardano.Wallet.DB.Pure.Implementation
    ( filterTxHistory
    )
import Cardano.Wallet.DummyTarget.Primitive.Types
    ( dummyGenesisParameters
    )
import Cardano.Wallet.Primitive.Types
    ( ChainPoint (..)
    , GenesisParameters
    , SlotNo (..)
    , SortOrder (..)
    , WalletId (..)
    , WalletMetadata (..)
    , WithOrigin (..)
    , wholeRange
    )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..)
    )
import Cardano.Wallet.Primitive.Types.Tx
    ( Tx (..)
    )
import Cardano.Wallet.Primitive.Types.Tx.TransactionInfo
    ( TransactionInfo (..)
    , toTxHistory
    )
import Cardano.Wallet.Primitive.Types.Tx.TxMeta
    ( TxMeta (slotNo)
    )
import Cardano.Wallet.Unsafe
    ( unsafeRunExceptT
    )
import Cardano.Wallet.Util
    ( ShowFmt (..)
    )
import Control.Monad
    ( forM_
    , void
    )
import Control.Monad.IO.Class
    ( liftIO
    )
import Control.Monad.Trans
    ( lift
    )
import Control.Monad.Trans.Except
    ( ExceptT
    , runExceptT
    )
import Crypto.Hash
    ( hash
    )
import Data.ByteString
    ( ByteString
    )
import Data.Foldable
    ( fold
    )
import Data.Functor.Identity
    ( Identity (..)
    )
import Data.Generics.Internal.VL.Lens
    ( (^.)
    )
import Data.Generics.Labels
    ()
import Data.Maybe
    ( isNothing
    , mapMaybe
    )
import Fmt
    ( Buildable
    , pretty
    )
import Test.Hspec
    ( SpecWith
    , describe
    , it
    , shouldReturn
    )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , Property
    , checkCoverage
    , counterexample
    , cover
    , label
    , property
    )
import Test.QuickCheck.Monadic
    ( PropertyM
    , assert
    , monadicIO
    , monitor
    , pick
    , run
    )

import qualified Data.List as L

-- | How to boot a fresh database.
type WithDBFresh s
    = WalletId
    -> (DBFresh IO s -> PropertyM IO ())
    -> PropertyM IO ()


withFreshWallet
    :: GenState s
    => WalletId
    -> WithDBFresh s
    -> (DBLayer IO s -> WalletId -> PropertyM IO ())
    -> PropertyM IO ()
withFreshWallet wid withFreshDB f = do
    withFreshDB wid $ \DBFresh {bootDBLayer} -> do
        (InitialCheckpoint cp0, meta) <- pick arbitrary
        db <- run
            $ unsafeRunExceptT
            $ bootDBLayer
            $ DBLayerParams cp0 meta mempty gp
        f db wid

type TestOnLayer s =
    ( DBLayer IO s
      -> WalletId
      -> PropertyM IO ()
    )
    -> Property

testWid :: WalletId
testWid = WalletId (hash ("test" :: ByteString))

-- | Wallet properties.
properties
    :: GenState s
    => WithDBFresh s
    -> SpecWith ()
properties withFreshDB = describe "DB.Properties" $ do

    let testOnLayer = monadicIO . withFreshWallet testWid withFreshDB

    describe "Extra Properties about DB initialization" $ do
        it "creating same wallet twice yields an error"
            $ property
            $ prop_createWalletTwice withFreshDB

    describe "put . read yields a result" $ do
        it "Tx History"
            $ property
            $ prop_readAfterPut
                testOnLayer
                (\db _ -> lift . putTxHistory_ db)
                (\db _ -> readTxHistory_ db)

    describe "getTx properties" $ do
        it "can read after putting tx history for valid tx id"
            $ property
            $ prop_getTxAfterPutValidTxId testOnLayer
        it "cannot read after putting tx history for invalid tx id"
            $ property
            $ prop_getTxAfterPutInvalidTxId testOnLayer

    describe "sequential puts replace values in order" $ do
        it "Tx History"
            $ checkCoverage
            $ prop_sequentialPut
                testOnLayer
                (\db _ -> lift . putTxHistory_ db)
                (\db _ -> readTxHistory_ db)
                ( let sort' =
                        GenTxHistory
                            . filterTxHistory Nothing Descending wholeRange Nothing
                            . unGenTxHistory
                  in  Identity . sort' . fold
                )

    describe "rollback" $ do
        it
            "Correctly re-construct tx history on rollbacks"
            (checkCoverage $ prop_rollbackTxHistory withFreshDB)

-- | Wrap the result of 'readTransactions' in an arbitrary identity Applicative
readTxHistory_
    :: Functor m
    => DBLayer m s
    ->  m (Identity GenTxHistory)
readTxHistory_ DBLayer {..} =
    (Identity . GenTxHistory . fmap toTxHistory)
        <$> atomically
            (readTransactions Nothing Descending wholeRange Nothing Nothing Nothing)

putTxHistory_
    :: DBLayer m s
    -> GenTxHistory
    -> m ()
putTxHistory_ DBLayer {..} = atomically . putTxHistory . unGenTxHistory

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
    :: WithDBFresh s
    -> ( WalletId
       , InitialCheckpoint s
       , WalletMetadata
       )
    -> Property
prop_createWalletTwice test (wid, InitialCheckpoint cp0, meta) = monadicIO
    $ test wid
    $ \DBFresh {..} -> do
        liftIO $ do
            let err = ErrWalletAlreadyInitialized
                bootData = DBLayerParams cp0 meta mempty gp
            runExceptT (void $ bootDBLayer bootData)
                `shouldReturn` Right ()
            runExceptT (void $ bootDBLayer bootData)
                `shouldReturn` Left err

-- | Checks that a given resource can be read after having been inserted in DB.
prop_readAfterPut
    :: (Buildable (f a), Eq (f a), Applicative f)
    => TestOnLayer s
    -> ( DBLayer IO s
         -> WalletId
         -> a
         -> ExceptT ErrWalletNotInitialized IO ()
       )
    -- ^ Put Operation
    -> ( DBLayer IO s
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
    :: TestOnLayer s
    -> GenTxHistory
    -> Property
prop_getTxAfterPutValidTxId test txGen = test $ \DBLayer {..} _ -> do
    let txs = unGenTxHistory txGen
    run $ atomically $ putTxHistory txs
    forM_ txs $ \(Tx {txId}, txMeta) -> do
        (Just (TransactionInfo {txInfoId, txInfoMeta})) <-
            run $ atomically $ getTx txId
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
    :: TestOnLayer s
    -> GenTxHistory
    -> (Hash "Tx")
    -> Property
prop_getTxAfterPutInvalidTxId test txGen txId' = test $ \DBLayer {..} _ -> do
    let txs = unGenTxHistory txGen
    run $ atomically $ putTxHistory txs
    res <- run $ atomically $ getTx txId'
    assertWith
        "Irrespective of Inserted, Read is Nothing for invalid tx id"
        (isNothing res)

-- | Check that the DB supports multiple sequential puts for a given resource
prop_sequentialPut
    :: (Buildable (f a), Eq (f a))
    => TestOnLayer s
    -> ( DBLayer IO s
         -> WalletId
         -> a
         -> ExceptT ErrWalletNotInitialized IO ()
       )
    -- ^ Put Operation
    -> ( DBLayer IO s
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
     . WithDBFresh s
    -> InitialCheckpoint s
    -> GenTxHistory
    -> Property
prop_rollbackTxHistory test (InitialCheckpoint cp0) (GenTxHistory txs0) = do
    monadicIO $ test testWid $ \DBFresh{..} -> do
        ShowFmt meta <- namedPick "Wallet Metadata" arbitrary
        ShowFmt requestedPoint <- namedPick "Requested Rollback slot" arbitrary
        let ixs = forgotten requestedPoint
        monitor $ label ("Forgotten tx after point: " <> show (L.length ixs))
        monitor $ cover 50 (not $ null ixs) "rolling back something"
        (point, txs) <- run $ do
            DBLayer{..} <-
                unsafeRunExceptT
                    $ bootDBLayer
                    $ DBLayerParams cp0 meta mempty gp
            atomically $ putTxHistory txs0
            point <- atomically $ rollbackTo (At requestedPoint)
            txs <-
                atomically
                    $ fmap toTxHistory
                        <$> readTransactions
                            Nothing
                            Descending
                            wholeRange
                            Nothing
                            Nothing
                            Nothing
            pure (point, txs)

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
