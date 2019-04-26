{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.DB.MVarSpec
    ( spec
    ) where

import Prelude

import Cardano.Crypto.Wallet
    ( unXPrv )
import Cardano.Wallet
    ( unsafeRunExceptT )
import Cardano.Wallet.DB
    ( DBLayer (..)
    , ErrNoSuchWallet (..)
    , ErrWalletAlreadyExists (..)
    , PrimaryKey (..)
    )
import Cardano.Wallet.DB.MVar
    ( newDBLayer )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..), Key, Passphrase (..), XPrv, generateKeyFromSeed )
import Cardano.Wallet.Primitive.Model
    ( Wallet, initWallet )
import Cardano.Wallet.Primitive.Types
    ( Direction (..)
    , Hash (..)
    , IsOurs (..)
    , SlotId (..)
    , Tx (..)
    , TxMeta (..)
    , TxStatus (..)
    , WalletDelegation (..)
    , WalletId (..)
    , WalletMetadata (..)
    , WalletName (..)
    , WalletPassphraseInfo (..)
    , WalletState (..)
    )
import Control.Concurrent.Async
    ( forConcurrently_ )
import Control.DeepSeq
    ( NFData )
import Control.Monad
    ( forM, forM_, void )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Monad.Trans.Except
    ( ExceptT, runExceptT )
import Crypto.Hash
    ( hash )
import Data.Functor.Identity
    ( Identity (..) )
import Data.Map.Strict
    ( Map )
import Data.Quantity
    ( Quantity (..) )
import Data.Word
    ( Word32 )
import GHC.Generics
    ( Generic )
import Test.Hspec
    ( Spec, describe, it, shouldBe, shouldReturn )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , InfiniteList (..)
    , Property
    , arbitraryBoundedEnum
    , checkCoverage
    , choose
    , cover
    , elements
    , genericShrink
    , oneof
    , property
    , vectorOf
    )
import Test.QuickCheck.Instances
    ()
import Test.QuickCheck.Monadic
    ( monadicIO, pick )

import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.List as L
import qualified Data.Map.Strict as Map


spec :: Spec
spec = do
    describe "Extra Properties about DB initialization" $ do
        it "createWallet . listWallets yields expected results"
            (property prop_createListWallet)
        it "creating same wallet twice yields an error"
            (property prop_createWalletTwice)
        it "removing the same wallet twice yields an error"
            (property prop_removeWalletTwice)

    describe "put . read yields a result" $ do
        it "Checkpoint"
            (property $ prop_readAfterPut putCheckpoint readCheckpoint)
        it "Wallet Metadata"
            (property $ prop_readAfterPut putWalletMeta readWalletMeta)
        it "Tx History"
            (property $ prop_readAfterPut putTxHistory readTxHistoryF)

    describe "can't put before wallet exists" $ do
        it "Checkpoint"
            (property $ prop_putBeforeInit putCheckpoint readCheckpoint Nothing)
        it "Wallet Metadata"
            (property $ prop_putBeforeInit putWalletMeta readWalletMeta Nothing)
        it "Tx History"
            (property $ prop_putBeforeInit putTxHistory readTxHistoryF (pure mempty))

    describe "put doesn't affect other resources" $ do
        it "Checkpoint vs Wallet Metadata & Tx History & Private Key"
            (property $ prop_isolation putCheckpoint
                readWalletMeta
                readTxHistoryF
                readPrivateKey
            )
        it "Wallet Metadata vs Tx History & Checkpoint & Private Key"
            (property $ prop_isolation putWalletMeta
                readTxHistoryF
                readCheckpoint
                readPrivateKey
            )
        it "Tx History vs Checkpoint & Wallet Metadata & Private Key"
            (property $ prop_isolation putTxHistory
                readCheckpoint
                readWalletMeta
                readPrivateKey
            )

    describe "can't read after delete" $ do
        it "Checkpoint"
            (property $ prop_readAfterDelete readCheckpoint Nothing)
        it "Wallet Metadata"
            (property $ prop_readAfterDelete readWalletMeta Nothing)
        it "Tx History"
            (property $ prop_readAfterDelete readTxHistoryF (pure mempty))
        it "Private Key"
            (property $ prop_readAfterDelete readPrivateKey Nothing)

    describe "sequential puts replace values in order" $ do
        it "Checkpoint"
            (checkCoverage $ prop_sequentialPut putCheckpoint readCheckpoint lrp)
        it "Wallet Metadata"
            (checkCoverage $ prop_sequentialPut putWalletMeta readWalletMeta lrp)
        it "Tx History"
            (checkCoverage $ prop_sequentialPut putTxHistory readTxHistoryF unions)

    describe "parallel puts replace values in _any_ order" $ do
        it "Checkpoint"
            (checkCoverage $ prop_parallelPut putCheckpoint readCheckpoint
                (length . lrp @Maybe))
        it "Wallet Metadata"
            (checkCoverage $ prop_parallelPut putWalletMeta readWalletMeta
                (length . lrp @Maybe))
        it "Tx History"
            (checkCoverage $ prop_parallelPut putTxHistory readTxHistoryF
                (length . unions @(Map (Hash "Tx") (Tx, TxMeta))))
  where
    -- | Wrap the result of 'readTxHistory' in an arbitrary identity Applicative
    readTxHistoryF
        :: Monad m
        => DBLayer m s
        -> PrimaryKey WalletId
        -> m (Identity (Map (Hash "Tx") (Tx, TxMeta)))
    readTxHistoryF db = fmap Identity . readTxHistory db

    -- | Keep only the (L)ast (R)ecently (P)ut entry
    lrp :: (Applicative f, Ord k) => [(k, v)] -> [f v]
    lrp = Map.elems . foldl (\m (k, v) -> Map.insert k (pure v) m) mempty

    -- | Keep the unions (right-biaised) of all entry
    unions :: (Monoid v, Ord k) => [(k, v)] -> [Identity v]
    unions =
        fmap Identity
        . Map.elems
        . foldl (\m (k, v) -> Map.unionWith (<>) (Map.fromList [(k, v)]) m) mempty

{-------------------------------------------------------------------------------
                                    Properties
-------------------------------------------------------------------------------}

-- | Checks that a given resource can be read after having been inserted in DB.
prop_readAfterPut
    :: (Show (f a), Eq (f a), Applicative f)
    => (  DBLayer IO DummyState
       -> PrimaryKey WalletId
       -> a
       -> ExceptT ErrNoSuchWallet IO ()
       ) -- ^ Put Operation
    -> (  DBLayer IO DummyState
       -> PrimaryKey WalletId
       -> IO (f a)
       ) -- ^ Read Operation
    -> (PrimaryKey WalletId, a)
        -- ^ Property arguments
    -> Property
prop_readAfterPut putOp readOp (key, a) =
    monadicIO (setup >>= prop)
  where
    setup = do
        db <- liftIO newDBLayer
        (cp, meta) <- pick arbitrary
        liftIO $ unsafeRunExceptT $ createWallet db key cp meta
        return db
    prop db = liftIO $ do
        unsafeRunExceptT $ putOp db key a
        res <- readOp db key
        res `shouldBe` pure a

-- | Can't put resource before a wallet has been initialized
prop_putBeforeInit
    :: (Show (f a), Eq (f a), Applicative f)
    => (  DBLayer IO DummyState
       -> PrimaryKey WalletId
       -> a
       -> ExceptT ErrNoSuchWallet IO ()
       ) -- ^ Put Operation
    -> (  DBLayer IO DummyState
       -> PrimaryKey WalletId
       -> IO (f a)
       ) -- ^ Read Operation
    -> f a
        -- ^ An 'empty' value for the 'Applicative' f
    -> (PrimaryKey WalletId, a)
        -- ^ Property arguments
    -> Property
prop_putBeforeInit putOp readOp empty (key@(PrimaryKey wid), a) =
    monadicIO (setup >>= prop)
  where
    setup = liftIO newDBLayer
    prop db = liftIO $ do
        runExceptT (putOp db key a) >>= \case
            Right _ ->
                fail "expected put operation to fail but it succeeded!"
            Left err ->
                err `shouldBe` ErrNoSuchWallet wid
        readOp db key `shouldReturn` empty

-- | Can't read back data after delete
prop_readAfterDelete
    :: (Show (f a), Eq (f a), Applicative f)
    => (  DBLayer IO DummyState
       -> PrimaryKey WalletId
       -> IO (f a)
       ) -- ^ Read Operation
    -> f a
        -- ^ An 'empty' value for the 'Applicative' f
    -> PrimaryKey WalletId
    -> Property
prop_readAfterDelete readOp empty key =
    monadicIO (setup >>= prop)
  where
    setup = do
        db <- liftIO newDBLayer
        (cp, meta) <- pick arbitrary
        liftIO $ unsafeRunExceptT $ createWallet db key cp meta
        return db
    prop db = liftIO $ do
        unsafeRunExceptT $ removeWallet db key
        readOp db key `shouldReturn` empty

-- | Modifying one resource leaves the other untouched
prop_isolation
    :: ( Applicative f, Show (f b), Eq (f b)
       , Applicative g, Show (g c), Eq (g c)
       , Applicative h, Show (h d), Eq (h d)
       )
    => (  DBLayer IO DummyState
       -> PrimaryKey WalletId
       -> a
       -> ExceptT ErrNoSuchWallet IO ()
       ) -- ^ Put Operation
    -> (  DBLayer IO DummyState
       -> PrimaryKey WalletId
       -> IO (f b)
       ) -- ^ Read Operation for another resource
    -> (  DBLayer IO DummyState
       -> PrimaryKey WalletId
       -> IO (g c)
       ) -- ^ Read Operation for another resource
    -> (  DBLayer IO DummyState
       -> PrimaryKey WalletId
       -> IO (h d)
       ) -- ^ Read Operation for another resource
    -> (PrimaryKey WalletId, a)
        -- ^ Properties arguments
    -> Property
prop_isolation putA readB readC readD (key, a) =
    monadicIO (setup >>= prop)
  where
    setup = do
        db <- liftIO newDBLayer
        (cp, meta, txs) <- pick arbitrary
        liftIO $ unsafeRunExceptT $ createWallet db key cp meta
        liftIO $ unsafeRunExceptT $ putTxHistory db key txs
        (b, c, d) <- liftIO $ (,,)
            <$> readB db key
            <*> readC db key
            <*> readD db key
        return (db, (b, c, d))

    prop (db, (b, c, d)) = liftIO $ do
        unsafeRunExceptT $ putA db key a
        readB db key `shouldReturn` b
        readC db key `shouldReturn` c
        readD db key `shouldReturn` d

-- | Check that the DB supports multiple sequential puts for a given resource
prop_sequentialPut
    :: (Show (f a), Eq (f a), Applicative f)
    => (  DBLayer IO DummyState
       -> PrimaryKey WalletId
       -> a
       -> ExceptT ErrNoSuchWallet IO ()
       ) -- ^ Put Operation
    -> (  DBLayer IO DummyState
       -> PrimaryKey WalletId
       -> IO (f a)
       ) -- ^ Read Operation
    -> (forall k. Ord k => [(k, a)] -> [f a])
        -- ^ How do we expect operations to resolve
    -> KeyValPairs (PrimaryKey WalletId) a
        -- ^ Property arguments
    -> Property
prop_sequentialPut putOp readOp resolve (KeyValPairs pairs) =
    cover 90 cond "conflicting db entries" $ monadicIO (setup >>= prop)
  where
    -- Make sure that we have some conflicting insertion to actually test the
    -- semantic of the DB Layer.
    cond = L.length (L.nub ids) /= L.length ids
      where
        ids = map fst pairs
    setup = do
        db <- liftIO newDBLayer
        (cp, meta) <- pick arbitrary
        liftIO $ unsafeRunExceptT $ once_ pairs $ \(k, _) ->
            createWallet db k cp meta
        return db
    prop db = liftIO $ do
        unsafeRunExceptT $ forM_ pairs $ uncurry (putOp db)
        res <- once pairs (readOp db . fst)
        res `shouldBe` resolve pairs

-- | Check that the DB supports multiple sequential puts for a given resource
prop_parallelPut
    :: (Show (f a), Eq (f a), Applicative f)
    => (  DBLayer IO DummyState
       -> PrimaryKey WalletId
       -> a
       -> ExceptT ErrNoSuchWallet IO ()
       ) -- ^ Put Operation
    -> (  DBLayer IO DummyState
       -> PrimaryKey WalletId
       -> IO (f a)
       ) -- ^ Read Operation
    -> (forall k. Ord k => [(k, a)] -> Int)
        -- ^ How many entries to we expect in the end
    -> KeyValPairs (PrimaryKey WalletId) a
        -- ^ Property arguments
    -> Property
prop_parallelPut putOp readOp resolve (KeyValPairs pairs) =
    cover 90 cond "conflicting db entries" $ monadicIO (setup >>= prop)
  where
    -- Make sure that we have some conflicting insertion to actually test the
    -- semantic of the DB Layer.
    cond = L.length (L.nub ids) /= L.length ids
      where
        ids = map fst pairs
    setup = do
        db <- liftIO newDBLayer
        (cp, meta) <- pick arbitrary
        liftIO $ unsafeRunExceptT $ once_ pairs $ \(k, _) ->
            createWallet db k cp meta
        return db
    prop db = liftIO $ do
        forConcurrently_ pairs $ unsafeRunExceptT . uncurry (putOp db)
        res <- once pairs (readOp db . fst)
        length res `shouldBe` resolve pairs

-- | Can list created wallets
prop_createListWallet
    :: KeyValPairs (PrimaryKey WalletId) (Wallet DummyState, WalletMetadata)
    -> Property
prop_createListWallet (KeyValPairs pairs) =
    monadicIO (setup >>= prop)
  where
    setup = liftIO newDBLayer
    prop db = liftIO $ do
        res <- once pairs $ \(k, (cp, meta)) ->
            unsafeRunExceptT $ createWallet db k cp meta
        (length <$> listWallets db) `shouldReturn` length res

-- | Trying to create a same wallet twice should yield an error
prop_createWalletTwice
    :: ( PrimaryKey WalletId
       , Wallet DummyState
       , WalletMetadata
       )
    -> Property
prop_createWalletTwice (key@(PrimaryKey wid), cp, meta) =
    monadicIO (setup >>= prop)
  where
    setup = liftIO newDBLayer
    prop db = liftIO $ do
        let err = ErrWalletAlreadyExists wid
        runExceptT (createWallet db key cp meta) `shouldReturn` Right ()
        runExceptT (createWallet db key cp meta) `shouldReturn` Left err

-- | Trying to remove a same wallet twice should yield an error
prop_removeWalletTwice
    :: ( PrimaryKey WalletId
       , Wallet DummyState
       , WalletMetadata
       )
    -> Property
prop_removeWalletTwice (key@(PrimaryKey wid), cp, meta) =
    monadicIO (setup >>= prop)
  where
    setup = liftIO $ do
        db <- newDBLayer
        unsafeRunExceptT $ createWallet db key cp meta
        return db
    prop db = liftIO $ do
        let err = ErrNoSuchWallet wid
        runExceptT (removeWallet db key) `shouldReturn` Right ()
        runExceptT (removeWallet db key) `shouldReturn` Left err

{-------------------------------------------------------------------------------
                      Tests machinery, Arbitrary instances
-------------------------------------------------------------------------------}

-- | Execute an action once per key @k@ present in the given list
once :: (Ord k, Monad m) => [(k,v)] -> ((k,v) -> m a) -> m [a]
once xs = forM (Map.toList (Map.fromList xs))

-- | Like 'once', but discards the result
once_ :: (Ord k, Monad m) => [(k,v)] -> ((k,v) -> m a) -> m ()
once_ xs = void . once xs

newtype KeyValPairs k v = KeyValPairs [(k, v)]
    deriving (Generic, Show, Eq)

instance (Arbitrary k, Arbitrary v) => Arbitrary (KeyValPairs k v) where
    shrink = genericShrink
    arbitrary = do
        pairs <- choose (10, 50) >>= flip vectorOf arbitrary
        pure $ KeyValPairs pairs

newtype DummyState = DummyState Int
    deriving (Show, Eq)

instance Arbitrary DummyState where
    shrink _ = []
    arbitrary = DummyState <$> arbitrary

deriving instance NFData DummyState

instance IsOurs DummyState where
    isOurs _ num = (True, num)

instance Arbitrary (Wallet DummyState) where
    shrink _ = []
    arbitrary = initWallet <$> arbitrary

deriving instance Show (PrimaryKey WalletId)

instance Arbitrary (PrimaryKey WalletId) where
    shrink _ = []
    arbitrary = do
        bytes <- B8.pack . pure <$> elements ['a'..'k']
        return $ PrimaryKey $ WalletId $ hash bytes

instance Arbitrary (Hash "Tx") where
    shrink _ = []
    arbitrary = do
        k <- choose (0, 10)
        return $ Hash (B8.pack ("TX" <> show @Int k))

instance Arbitrary Tx where
    shrink _ = []
    arbitrary = return $ Tx mempty mempty

instance Arbitrary TxMeta where
    shrink _ = []
    arbitrary = TxMeta
        <$> elements [Pending, InLedger, Invalidated]
        <*> elements [Incoming, Outgoing]
        <*> (SlotId <$> arbitrary <*> choose (0, 21600))
        <*> fmap (Quantity . fromIntegral) (arbitrary @Word32)

instance Arbitrary WalletMetadata where
    shrink _ = []
    arbitrary = WalletMetadata
        <$> (WalletName <$> elements ["bulbazaur", "charmander", "squirtle"])
        <*> (WalletPassphraseInfo <$> arbitrary)
        <*> oneof [pure Ready, Restoring . Quantity <$> arbitraryBoundedEnum]
        <*> pure NotDelegating

instance Arbitrary (Key 'RootK XPrv) where
    shrink _ = []
    arbitrary = do
        (s, g, e) <- (,,)
            <$> genPassphrase @"seed" (0, 32)
            <*> genPassphrase @"generation" (0, 16)
            <*> genPassphrase @"encryption" (0, 16)
        return $ generateKeyFromSeed (s, g) e
      where
        genPassphrase :: (Int, Int) -> Gen (Passphrase purpose)
        genPassphrase range = do
            n <- choose range
            InfiniteList bytes _ <- arbitrary
            return $ Passphrase $ BA.convert $ BS.pack $ take n bytes

instance Arbitrary (Hash "encryption") where
    shrink _ = []
    arbitrary = do
        InfiniteList bytes _ <- arbitrary
        return $ Hash $ BS.pack $ take 32 bytes

-- Necessary unsound Show instance for QuickCheck failure reporting
instance Show XPrv where
    show = show . unXPrv

-- Necessary unsound Eq instance for QuickCheck properties
instance Eq XPrv where
    a == b = unXPrv a == unXPrv b
