{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.DB.SqliteCorruptionSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet
    ( unsafeRunExceptT )
import Cardano.Wallet.DB
    ( DBLayer (..), ErrWalletAlreadyExists (..), PrimaryKey (..) )
import Cardano.Wallet.DB.Sqlite
    ( newDBLayer )
import Cardano.Wallet.DBSpec
    ( DummyTarget, KeyValPairs (..), cleanDB, withDB )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Passphrase (..)
    , encryptPassphrase
    , generateKeyFromSeed
    , unsafeGenerateKeyFromSeed
    )
import Cardano.Wallet.Primitive.AddressDiscovery
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
    , Tx (..)
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
import Control.Monad
    ( forM_ )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Monad.Trans.Except
    ( runExceptT )
import Crypto.Hash
    ( hash )
import Data.ByteString
    ( ByteString )
import Data.Coerce
    ( coerce )
import Data.Quantity
    ( Quantity (..) )
import Data.Text.Class
    ( FromText (..) )
import Data.Time.Clock
    ( getCurrentTime )
import Database.Sqlite
    ( Connection )
import System.IO.Unsafe
    ( unsafePerformIO )
import Test.Hspec
    ( Expectation, Spec, before, describe, it, shouldReturn )
import Test.QuickCheck
    ( Property, choose, generate, property )
import Test.QuickCheck.Monadic
    ( monadicIO )

import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

spec :: Spec
spec =  do
    before ((snd <$> fileDBLayer) >>= cleanDB) $
        describe "Check db reading/writing from/to file and cleaning" $ do

        it "create and list wallet works" $ \db -> do
            unsafeRunExceptT $ createWallet db testPk testCp testMetadata
            listWallets db `shouldReturn` [testPk]
            (_, db1) <- fileDBLayer
            runExceptT (createWallet db1 testPk testCp testMetadata)
                `shouldReturn` (Left (ErrWalletAlreadyExists testWid))
            ( testOpeningCleaning
                listWallets
                [testPk]
                [] )

        it "create and get meta works" $ \db -> do
            now <- getCurrentTime
            let md = testMetadata { passphraseInfo = Just $ WalletPassphraseInfo now }
            unsafeRunExceptT $ createWallet db testPk testCp md
            readWalletMeta db testPk `shouldReturn` Just md
            ( testOpeningCleaning
                (`readWalletMeta` testPk)
                (Just md)
                Nothing )

        it "create and get private key" $ \db -> do
            unsafeRunExceptT $ createWallet db testPk testCp testMetadata
            readPrivateKey db testPk `shouldReturn` Nothing
            let Right phr = fromText "simplephrase"
                k = unsafeGenerateKeyFromSeed (coerce phr, coerce phr) phr
            h <- encryptPassphrase phr
            unsafeRunExceptT (putPrivateKey db testPk (k, h))
            readPrivateKey db testPk `shouldReturn` Just (k, h)
            ( testOpeningCleaning
                (`readPrivateKey` testPk)
                (Just (k, h))
                Nothing )

        it "put and read tx history" $ \db -> do
            unsafeRunExceptT $ createWallet db testPk testCp testMetadata
            runExceptT (putTxHistory db testPk testTxs) `shouldReturn` Right ()
            readTxHistory db testPk `shouldReturn` testTxs
            ( testOpeningCleaning
                (`readTxHistory` testPk)
                testTxs
                Map.empty )

        it "put and read checkpoint" $ \db -> do
            unsafeRunExceptT $ createWallet db testPk testCp testMetadata
            runExceptT (putCheckpoint db testPk testCp) `shouldReturn` Right ()
            readCheckpoint db testPk `shouldReturn` Just testCp
            ( testOpeningCleaning
                (`readCheckpoint` testPk)
                (Just testCp)
                Nothing )

    withDB inMemoryDBLayer $
        describe "random operation chunks property when writing to/reading from file" $ do
        it "realize a random batch of operations upon one db open"
            (property . prop_randomOpChunks)

    where
        testOpeningCleaning
            :: (Show s, Eq s)
            => (DBLayer IO (SeqState DummyTarget) DummyTarget -> IO s)
            -> s
            -> s
            -> Expectation
        testOpeningCleaning call expectedAfterOpen expectedAfterClean = do
            (_, db1) <- fileDBLayer
            call db1 `shouldReturn` expectedAfterOpen
            _ <- cleanDB db1
            call db1 `shouldReturn` expectedAfterClean
            (_,db2) <- fileDBLayer
            call db2 `shouldReturn` expectedAfterClean


prop_randomOpChunks
    :: DBLayer IO (SeqState DummyTarget) DummyTarget
    -> KeyValPairs (PrimaryKey WalletId) (Wallet (SeqState DummyTarget) DummyTarget , WalletMetadata)
    -> Property
prop_randomOpChunks inMemoryDB (KeyValPairs pairs) =
    monadicIO (pure inMemoryDB >>= prop)
  where
    prop dbM = liftIO $ do
        (_, dbF) <- fileDBLayer
        _ <- cleanDB dbF

        forM_ pairs (updateDB dbM)
        chunks <- cutRandomly [] pairs
        forM_ chunks handleChunks

        verify dbM dbF

    cutRandomly :: [[a]] -> [a] -> IO [[a]]
    cutRandomly acc rest =
        if L.length rest > 1 then do
            chunksNum <- generate $ choose (1, L.length rest)
            let chunk = L.take chunksNum rest
            cutRandomly (chunk:acc) (L.drop chunksNum rest)
        else
            pure $ L.reverse (rest:acc)
    handleChunks chunk = do
        (_, db) <- fileDBLayer
        forM_ chunk (updateDB db)
    updateDB
        :: DBLayer IO s t
        -> (PrimaryKey WalletId, (Wallet s t, WalletMetadata))
        -> IO ()
    updateDB db (k, (cp, meta)) = do
        keys <- listWallets db
        if k `elem` keys then do
            runExceptT (putCheckpoint db k cp) `shouldReturn` Right ()
            runExceptT (putWalletMeta db k meta) `shouldReturn` Right ()
        else do
            unsafeRunExceptT $ createWallet db k cp meta
            Set.fromList <$> listWallets db `shouldReturn` Set.fromList (k:keys)
    verify :: (Eq s) => DBLayer IO s t -> DBLayer IO s t -> IO ()
    verify db1 db2 = do
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


fileDBLayer :: IO (Connection, DBLayer IO (SeqState DummyTarget) DummyTarget)
fileDBLayer = newDBLayer (Just "backup/test.db")

inMemoryDBLayer :: IO (DBLayer IO (SeqState DummyTarget) DummyTarget)
inMemoryDBLayer = do
    (_, db) <- newDBLayer Nothing
    pure db

testCp :: Wallet (SeqState DummyTarget) DummyTarget
testCp = initWallet initDummyState

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
testWid = WalletId (hash ("test" :: ByteString))

testPk :: PrimaryKey WalletId
testPk = PrimaryKey testWid

testTxs :: Map.Map (Hash "Tx") (Tx, TxMeta)
testTxs = Map.fromList
    [ (Hash "tx2"
      , (Tx [TxIn (Hash "tx1") 0] [TxOut (Address "addr") (Coin 1)]
        , TxMeta InLedger Incoming (SlotId 14 0) (Quantity 1337144))) ]
