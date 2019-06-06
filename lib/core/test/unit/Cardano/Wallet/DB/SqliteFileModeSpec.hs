{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.DB.SqliteFileModeSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet
    ( unsafeRunExceptT )
import Cardano.Wallet.DB
    ( DBLayer (..)
    , ErrNoSuchWallet (..)
    , ErrWalletAlreadyExists (..)
    , PrimaryKey (..)
    , cleanDB
    )
import Cardano.Wallet.DB.Sqlite
    ( PersistState, newDBLayer )
import Cardano.Wallet.DBSpec
    ( DummyTarget, KeyValPairs (..) )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..)
    , Key
    , Passphrase (..)
    , XPrv
    , encryptPassphrase
    , generateKeyFromSeed
    )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( IsOurs, SeqState (..), defaultAddressPoolGap, mkSeqState )
import Cardano.Wallet.Primitive.Mnemonic
    ( EntropySize, entropyToBytes, genEntropy )
import Cardano.Wallet.Primitive.Model
    ( Wallet, initWallet )
import Cardano.Wallet.Primitive.Types
    ( Address (..)
    , Coin (..)
    , Direction (..)
    , Hash (..)
    , SlotId (..)
    , Tx (..)
    , TxId
    , TxIn (..)
    , TxMeta (TxMeta)
    , TxOut (..)
    , TxStatus (..)
    , WalletDelegation (..)
    , WalletId (..)
    , WalletMetadata (..)
    , WalletName (..)
    , WalletPassphraseInfo (..)
    , WalletState (..)
    )
import Control.DeepSeq
    ( NFData )
import Control.Monad
    ( forM_, replicateM_ )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Monad.Trans.Except
    ( ExceptT, runExceptT )
import Crypto.Hash
    ( hash )
import Data.ByteString
    ( ByteString )
import Data.Coerce
    ( coerce )
import Data.Functor
    ( ($>) )
import Data.Quantity
    ( Quantity (..) )
import Data.Text.Class
    ( FromText (..) )
import Data.Time.Clock
    ( getCurrentTime )
import Database.Persist.Sql
    ( close' )
import Database.Persist.Sqlite
    ( SqlBackend )
import System.IO.Unsafe
    ( unsafePerformIO )
import System.Random
    ( randomRIO )
import Test.Hspec
    ( Expectation, Spec, before, describe, it, shouldReturn )
import Test.QuickCheck
    ( Property, property, (==>) )
import Test.QuickCheck.Monadic
    ( monadicIO )

import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

spec :: Spec
spec =  do
    describe "Check db opening/closing" $ do
        it "Opening and closing of db works" $ do
            replicateM_ 25 $
                fileDBLayer @(SeqState DummyTarget) @DummyTarget >>= close' . fst

    before (fileDBLayer >>= cleanDB') $
        describe "Check db reading/writing from/to file and cleaning" $ do

        it "create and list wallet works" $ \(conn, db) -> do
            unsafeRunExceptT $ createWallet db testPk testCp testMetadata
            close' conn
            testOpeningCleaning listWallets [testPk] []

        it "create and get meta works" $ \(conn, db) -> do
            now <- getCurrentTime
            let meta = testMetadata
                   { passphraseInfo = Just $ WalletPassphraseInfo now }
            unsafeRunExceptT $ createWallet db testPk testCp meta
            close' conn
            testOpeningCleaning (`readWalletMeta` testPk) (Just meta) Nothing

        it "create twice is handled" $ \(conn, db) -> do
            let create' = createWallet db testPk testCp testMetadata
            runExceptT create' `shouldReturn` (Right ())
            runExceptT create' `shouldReturn`
                (Left (ErrWalletAlreadyExists testWid))
            close' conn
            testOpeningCleaning
                (`readWalletMeta` testPk) (Just testMetadata) Nothing

        it "create and remove" $ \(conn, db) -> do
            unsafeRunExceptT $ createWallet db testPk testCp testMetadata
            unsafeRunExceptT $ putTxHistory db testPk testTxs
            unsafeRunExceptT $ removeWallet db testPk

            readCheckpoint db testPk `shouldReturn` Nothing
            readWalletMeta db testPk `shouldReturn` Nothing
            readTxHistory db testPk `shouldReturn` Map.empty
            readPrivateKey db testPk `shouldReturn` Nothing
            listWallets db `shouldReturn` []

            runExceptT (putCheckpoint db testPk testCp) `shouldReturn`
                (Left (ErrNoSuchWallet testWid))
            runExceptT (putWalletMeta db testPk testMetadata) `shouldReturn`
                (Left (ErrNoSuchWallet testWid))
            runExceptT (putTxHistory db testPk testTxs) `shouldReturn`
                (Left (ErrNoSuchWallet testWid))

            let Right pwd = fromText "aaaaaaaaaa"
            let k = generateKeyFromSeed (coerce pwd, coerce pwd) pwd
            h <- liftIO $ encryptPassphrase pwd
            runExceptT (putPrivateKey db testPk (k, h)) `shouldReturn`
                (Left (ErrNoSuchWallet testWid))
            close' conn
            testOpeningCleaning (`readWalletMeta` testPk) Nothing Nothing

        it "create and get private key" $ \(conn, db) -> do
            unsafeRunExceptT $ createWallet db testPk testCp testMetadata
            (k, h) <- unsafeRunExceptT $ attachPrivateKey db testPk
            close' conn
            testOpeningCleaning (`readPrivateKey` testPk) (Just (k, h)) Nothing

        it "put and read metadata" $ \(conn, db) -> do
            unsafeRunExceptT $ createWallet db testPk testCp testMetadata
            let md = testMetadata
                    { name = WalletName "test wallet updated now" }
            runExceptT (putWalletMeta db testPk md) `shouldReturn` Right ()
            close' conn
            testOpeningCleaning (`readWalletMeta` testPk) (Just md) Nothing

        it "put and read tx history" $ \(conn, db) -> do
            unsafeRunExceptT $ createWallet db testPk testCp testMetadata
            unsafeRunExceptT $ putTxHistory db testPk testTxs
            close' conn
            testOpeningCleaning (`readTxHistory` testPk) testTxs Map.empty

        it "put and read tx history - regression case" $ \(conn, db) -> do
            unsafeRunExceptT $ createWallet db testPk testCp testMetadata
            unsafeRunExceptT $ createWallet db testPk1 testCp testMetadata
            runExceptT (putTxHistory db testPk1 testTxs) `shouldReturn` Right ()
            runExceptT (putTxHistory db testPk testTxs) `shouldReturn` Right ()
            runExceptT (removeWallet db testPk) `shouldReturn` Right ()
            readTxHistory db testPk1 `shouldReturn` testTxs
            readTxHistory db testPk `shouldReturn` (Map.empty)
            close' conn
            testOpeningCleaning (`readTxHistory` testPk1) testTxs Map.empty

        it "put and read checkpoint" $ \(conn, db) -> do
            unsafeRunExceptT $ createWallet db testPk testCp testMetadata
            unsafeRunExceptT $ putCheckpoint db testPk testCp
            close' conn
            testOpeningCleaning (`readCheckpoint` testPk) (Just testCp) Nothing

    describe "random operation chunks property" $ do
        it "realize a random batch of operations upon one db open"
            (property $ prop_randomOpChunks @(SeqState DummyTarget) @DummyTarget)

{-------------------------------------------------------------------------------
                              Tests and Properties
-------------------------------------------------------------------------------}

-- This property checks that executing series of wallet operations in a single
-- SQLite session has the same effect as executing the same operations over
-- multiple sessions.
prop_randomOpChunks
    :: (Eq s, IsOurs s, NFData s, Show s, PersistState s, TxId t)
    => KeyValPairs (PrimaryKey WalletId) (Wallet s t, WalletMetadata)
    -> Property
prop_randomOpChunks (KeyValPairs pairs) =
    not (null pairs) ==> monadicIO (liftIO prop)
  where
    prop = do
        (connF, dbF) <- fileDBLayer >>= cleanDB'
        (connM, dbM) <- inMemoryDBLayer >>= cleanDB'
        forM_ pairs (insertPair dbM)
        cutRandomly pairs >>= mapM_ (\chunk -> do
            (conn, db) <- fileDBLayer
            forM_ chunk (insertPair db)
            close' conn)
        dbF `shouldBeConsistentWith` dbM
        close' connF *> close' connM

    insertPair
        :: DBLayer IO s t
        -> (PrimaryKey WalletId, (Wallet s t, WalletMetadata))
        -> IO ()
    insertPair db (k, (cp, meta)) = do
        keys <- listWallets db
        if k `elem` keys then do
            unsafeRunExceptT $ putCheckpoint db k cp
            unsafeRunExceptT $ putWalletMeta db k meta
        else do
            unsafeRunExceptT $ createWallet db k cp meta
            Set.fromList <$> listWallets db `shouldReturn` Set.fromList (k:keys)

    shouldBeConsistentWith :: (Eq s) => DBLayer IO s t -> DBLayer IO s t -> IO ()
    shouldBeConsistentWith db1 db2 = do
        expectedWalIds <- Set.fromList <$> listWallets db1
        Set.fromList <$> listWallets db2
            `shouldReturn` expectedWalIds
        forM_ expectedWalIds $ \walId -> do
            expectedCps <- readCheckpoint db1 walId
            readCheckpoint db2 walId
                `shouldReturn` expectedCps
        forM_ expectedWalIds $ \walId -> do
            expectedMetas <- readWalletMeta db1 walId
            readWalletMeta db2 walId
                `shouldReturn` expectedMetas


-- | Test that data is preserved between open / close of the same database and
-- that cleaning up happens as expected.
testOpeningCleaning
    :: (Show s, Eq s)
    => (DBLayer IO (SeqState DummyTarget) DummyTarget -> IO s)
    -> s
    -> s
    -> Expectation
testOpeningCleaning call expectedAfterOpen expectedAfterClean = do
    (conn1, db1) <- fileDBLayer
    call db1 `shouldReturn` expectedAfterOpen
    _ <- cleanDB db1
    call db1 `shouldReturn` expectedAfterClean
    close' conn1
    (conn2,db2) <- fileDBLayer
    call db2 `shouldReturn` expectedAfterClean
    close' conn2

{-------------------------------------------------------------------------------
                                 Helpers
-------------------------------------------------------------------------------}

inMemoryDBLayer
    :: (IsOurs s, NFData s, Show s, PersistState s, TxId t)
    => IO (SqlBackend, DBLayer IO s t)
inMemoryDBLayer = newDBLayer Nothing

fileDBLayer
    :: (IsOurs s, NFData s, Show s, PersistState s, TxId t)
    => IO (SqlBackend, DBLayer IO s t)
fileDBLayer = newDBLayer (Just "backup/test.db")

-- | Clean the database
cleanDB'
    :: Monad m
    => (SqlBackend, DBLayer m s t)
    -> m (SqlBackend, DBLayer m s t)
cleanDB' (conn, db) =
    cleanDB db $> (conn, db)

-- | Attach an arbitrary private key to a wallet
attachPrivateKey
    :: DBLayer IO s t
    -> PrimaryKey WalletId
    -> ExceptT ErrNoSuchWallet IO (Key 'RootK XPrv, Hash "encryption")
attachPrivateKey db wid = do
    let Right pwd = fromText "simplephrase"
    let k = generateKeyFromSeed (coerce pwd, coerce pwd) pwd
    h <- liftIO $ encryptPassphrase pwd
    putPrivateKey db wid (k, h)
    return (k, h)

cutRandomly :: [a] -> IO [[a]]
cutRandomly = iter []
  where
    iter acc rest
        | L.length rest <= 1 =
            pure $ L.reverse (rest:acc)
        | otherwise = do
            chunksNum <- randomRIO (1, L.length rest)
            let chunk = L.take chunksNum rest
            iter (chunk:acc) (L.drop chunksNum rest)

{-------------------------------------------------------------------------------
                           arbitrary / dummy test data
-------------------------------------------------------------------------------}

testCp :: Wallet (SeqState DummyTarget) DummyTarget
testCp = initWallet initDummyState
  where
    initDummyState :: SeqState DummyTarget
    initDummyState = mkSeqState (xprv, mempty) defaultAddressPoolGap
      where
        bytes = entropyToBytes <$> unsafePerformIO $ genEntropy @(EntropySize 15)
        xprv = generateKeyFromSeed (Passphrase bytes, mempty) mempty

testMetadata :: WalletMetadata
testMetadata = WalletMetadata
    { name = WalletName "test wallet"
    , creationTime = unsafePerformIO getCurrentTime
    , passphraseInfo = Nothing
    , status = Ready
    , delegation = NotDelegating
    }

testWid :: WalletId
testWid = WalletId (hash @ByteString "test")

testPk :: PrimaryKey WalletId
testPk = PrimaryKey testWid

testPk1 :: PrimaryKey WalletId
testPk1 = PrimaryKey (WalletId (hash @ByteString "test1"))

testTxs :: Map.Map (Hash "Tx") (Tx, TxMeta)
testTxs = Map.fromList
    [ (Hash "tx2", (Tx [TxIn (Hash "tx1") 0] [TxOut (Address "addr") (Coin 1)]
      , TxMeta InLedger Incoming (SlotId 14 0) (Quantity 1337144))
      )
    ]
