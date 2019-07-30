{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.DB.SqliteFileModeSpec
    ( spec
    ) where

import Prelude

import Cardano.BM.Data.Tracer
    ( nullTracer )
import Cardano.Wallet.DB
    ( DBLayer (..), ErrNoSuchWallet (..), PrimaryKey (..), cleanDB )
import Cardano.Wallet.DB.Sqlite
    ( PersistState, PersistTx, SqliteContext, destroyDBLayer, newDBLayer )
import Cardano.Wallet.DBSpec
    ( KeyValPairs (..) )
import Cardano.Wallet.DummyTarget.Primitive.Types
    ( DummyTarget, Tx (..), block0 )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..), Key, Passphrase (..), XPrv, encryptPassphrase )
import Cardano.Wallet.Primitive.AddressDerivation.Sequential
    ( generateKeyFromSeed )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( IsOurs )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( SeqState (..), defaultAddressPoolGap, mkSeqState )
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
    , SortOrder (..)
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
    , wholeRange
    )
import Cardano.Wallet.Unsafe
    ( unsafeRunExceptT )
import Control.DeepSeq
    ( NFData )
import Control.Monad
    ( forM_, replicateM_ )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Monad.Trans.Except
    ( ExceptT )
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
import System.IO.Temp
    ( emptySystemTempFile )
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

import qualified Cardano.BM.Configuration.Model as CM
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

spec :: Spec
spec =  do
    describe "Check db opening/closing" $ do
        it "Opening and closing of db works" $ do
            replicateM_ 25 $ do
                db <- Just <$> temporaryDBFile
                (ctx, _) <- newDBLayer' @(SeqState DummyTarget) @DummyTarget db
                destroyDBLayer ctx

    before temporaryDBFile $
        describe "Check db reading/writing from/to file and cleaning" $ do

        it "create and list wallet works" $ \f -> do
            (ctx, db) <- newDBLayer' (Just f)
            unsafeRunExceptT $ createWallet db testWid testCp testMetadata
            destroyDBLayer ctx
            testOpeningCleaning f listWallets [testWid] []

        it "create and get meta works" $ \f -> do
            (ctx, db) <- newDBLayer' (Just f)
            now <- getCurrentTime
            let meta = testMetadata
                   { passphraseInfo = Just $ WalletPassphraseInfo now }
            unsafeRunExceptT $ createWallet db testWid testCp meta
            destroyDBLayer ctx
            testOpeningCleaning f (`readWalletMeta` testWid) (Just meta) Nothing

        it "create and get private key" $ \f-> do
            (ctx, db) <- newDBLayer' (Just f)
            unsafeRunExceptT $ createWallet db testWid testCp testMetadata
            (k, h) <- unsafeRunExceptT $ attachPrivateKey db testWid
            destroyDBLayer ctx
            testOpeningCleaning f (`readPrivateKey` testWid) (Just (k, h)) Nothing

        it "put and read tx history (Ascending and Descending)" $ \f -> do
            (ctx, db) <- newDBLayer' (Just f)
            unsafeRunExceptT $ createWallet db testWid testCp testMetadata
            unsafeRunExceptT $ putTxHistory db testWid (Map.fromList testTxs)
            destroyDBLayer ctx
            let testWith order = testOpeningCleaning
                    f
                    (\db' -> readTxHistory db' testWid order wholeRange)
                    testTxs
                    mempty
            testWith Ascending
            testWith Descending

        it "put and read checkpoint" $ \f -> do
            (ctx, db) <- newDBLayer' (Just f)
            unsafeRunExceptT $ createWallet db testWid testCp testMetadata
            unsafeRunExceptT $ putCheckpoint db testWid testCp
            destroyDBLayer ctx
            testOpeningCleaning f (`readCheckpoint` testWid) (Just testCp) Nothing

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
    :: (Eq s, IsOurs s, NFData s, Show s, PersistState s, PersistTx t)
    => KeyValPairs (PrimaryKey WalletId) (Wallet s t, WalletMetadata)
    -> Property
prop_randomOpChunks (KeyValPairs pairs) =
    not (null pairs) ==> monadicIO (liftIO prop)
  where
    prop = do
        filepath <- temporaryDBFile
        (ctxF, dbF) <- newDBLayer' (Just filepath) >>= cleanDB'
        (ctxM, dbM) <- inMemoryDBLayer >>= cleanDB'
        forM_ pairs (insertPair dbM)
        cutRandomly pairs >>= mapM_ (\chunk -> do
            (ctx, db) <- newDBLayer' (Just filepath)
            forM_ chunk (insertPair db)
            destroyDBLayer ctx)
        dbF `shouldBeConsistentWith` dbM
        destroyDBLayer ctxF *> destroyDBLayer ctxM

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
    => FilePath
    -> (DBLayer IO (SeqState DummyTarget) DummyTarget -> IO s)
    -> s
    -> s
    -> Expectation
testOpeningCleaning filepath call expectedAfterOpen expectedAfterClean = do
    (ctx1, db1) <- newDBLayer' (Just filepath)
    call db1 `shouldReturn` expectedAfterOpen
    _ <- cleanDB db1
    call db1 `shouldReturn` expectedAfterClean
    destroyDBLayer ctx1
    (ctx2,db2) <- newDBLayer' (Just filepath)
    call db2 `shouldReturn` expectedAfterClean
    destroyDBLayer ctx2

{-------------------------------------------------------------------------------
                                 Helpers
-------------------------------------------------------------------------------}

inMemoryDBLayer
    :: (IsOurs s, NFData s, Show s, PersistState s, PersistTx t)
    => IO (SqliteContext, DBLayer IO s t)
inMemoryDBLayer = newDBLayer' Nothing

temporaryDBFile :: IO FilePath
temporaryDBFile = emptySystemTempFile "cardano-wallet-SqliteFileMode"

newDBLayer'
    :: (IsOurs s, NFData s, Show s, PersistState s, PersistTx t)
    => Maybe FilePath
    -> IO (SqliteContext, DBLayer IO s t)
newDBLayer' fp = do
    logConfig <- CM.empty
    newDBLayer logConfig nullTracer fp

-- | Clean the database
cleanDB'
    :: Monad m
    => (SqliteContext, DBLayer m s t)
    -> m (SqliteContext, DBLayer m s t)
cleanDB' (ctx, db) =
    cleanDB db $> (ctx, db)

-- | Attach an arbitrary private key to a wallet
attachPrivateKey
    :: DBLayer IO s t
    -> PrimaryKey WalletId
    -> ExceptT ErrNoSuchWallet IO (Key 'RootK XPrv, Hash "encryption")
attachPrivateKey db wid = do
    let Right pwd = fromText "simplevalidphrase"
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
testCp = initWallet block0 initDummyState
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

testWid :: PrimaryKey WalletId
testWid = PrimaryKey (WalletId (hash @ByteString "test"))

testTxs :: [(Hash "Tx", (Tx, TxMeta))]
testTxs =
    [ (Hash "tx2", (Tx [TxIn (Hash "tx1") 0] [TxOut (Address "addr") (Coin 1)]
      , TxMeta InLedger Incoming (SlotId 14 0) (Quantity 1337144))
      )
    ]
