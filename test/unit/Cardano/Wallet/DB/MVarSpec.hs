{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.DB.MVarSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet
    ( unsafeRunExceptT )
import Cardano.Wallet.DB
    ( DBLayer (..), ErrNoSuchWallet (..), PrimaryKey (..) )
import Cardano.Wallet.DB.MVar
    ( newDBLayer )
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
    ( Spec, before, describe, it, shouldBe, shouldReturn )
import Test.QuickCheck
    ( Arbitrary (..)
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

import qualified Data.ByteString.Char8 as B8
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set


spec :: Spec
spec = do
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
        it "Checkpoint vs Wallet Metadata & Tx History"
            (property $ prop_isolation putCheckpoint readWalletMeta readTxHistoryF)
        it "Wallet Metadata vs Tx History & Checkpoint"
            (property $ prop_isolation putWalletMeta readTxHistoryF readCheckpoint)
        it "Tx History vs Checkpoint & Wallet Metadata"
            (property $ prop_isolation putTxHistory readCheckpoint readWalletMeta)

    describe "sequential puts replace values in order" $ do
        it "Checkpoint"
            (checkCoverage $ prop_sequentialPut putCheckpoint readCheckpoint lrp)
        it "Wallet Metadata"
            (checkCoverage $ prop_sequentialPut putWalletMeta readWalletMeta lrp)
        it "Tx History"
            (checkCoverage $ prop_sequentialPut putTxHistory readTxHistoryF unions)

    describe "DB works as expected" $ before newDBLayer $ do
        it "replacement of checkpoint returns last checkpoint that was put"
            (property . dbReplaceCheckpointsProp)
        it "multiple parallel putCheckpoint work properly"
            (property . dbMultiplePutsParProp)
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
    unions :: (Ord k, Monoid v) => [(k, v)] -> [Identity v]
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
       -> ExceptT (ErrNoSuchWallet e) IO ()
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
       -> ExceptT (ErrNoSuchWallet e) IO ()
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
                err `shouldBe` (ErrNoSuchWallet wid :: ErrNoSuchWallet e)
        readOp db key `shouldReturn` empty

-- | Modifying one resouce leaves the other untouched
prop_isolation
    :: (Show (f b), Eq (f b), Show (g c), Eq (g c), Applicative f, Applicative g)
    => (  DBLayer IO DummyState
       -> PrimaryKey WalletId
       -> a
       -> ExceptT (ErrNoSuchWallet e) IO ()
       ) -- ^ Put Operation
    -> (  DBLayer IO DummyState
       -> PrimaryKey WalletId
       -> IO (f b)
       ) -- ^ Read Operation for another resource
    -> (  DBLayer IO DummyState
       -> PrimaryKey WalletId
       -> IO (g c)
       ) -- ^ Read Operation for another resource
    -> (PrimaryKey WalletId, a)
        -- ^ Properties arguments
    -> Property
prop_isolation putA readB readC (key, a) =
    monadicIO (setup >>= prop)
  where
    setup = do
        db <- liftIO newDBLayer
        (cp, meta, txs) <- pick arbitrary
        liftIO $ unsafeRunExceptT $ createWallet db key cp meta
        liftIO $ unsafeRunExceptT $ putTxHistory db key txs
        (b, c) <- liftIO $ (,) <$> readB db key <*> readC db key
        return (db, (b,c))

    prop (db, (b, c)) = liftIO $ do
        unsafeRunExceptT $ putA db key a
        readB db key `shouldReturn` b
        readC db key `shouldReturn` c

-- | Check that the DB supports multiple sequential puts for a given resource
prop_sequentialPut
    :: (Show (f a), Eq (f a), Applicative f)
    => (  DBLayer IO DummyState
       -> PrimaryKey WalletId
       -> a
       -> ExceptT (ErrNoSuchWallet e) IO ()
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
        liftIO $ unsafeRunExceptT $ once_ pairs $ \k ->
            createWallet db k cp meta
        return db
    prop db = liftIO $ do
        unsafeRunExceptT $ forM_ pairs $ uncurry (putOp db)
        res <- once pairs (readOp db)
        res `shouldBe` resolve pairs

dbReplaceCheckpointsProp
    :: DBLayer IO DummyState
    -> (PrimaryKey WalletId, Wallet DummyState, Wallet DummyState)
    -> Property
dbReplaceCheckpointsProp db (key, cp1, cp2)  = monadicIO $ do
    meta <- pick arbitrary
    liftIO $ do
        unsafeRunExceptT $ createWallet db key cp1 meta
        unsafeRunExceptT $ putCheckpoint db key cp2
        resFromDb <- readCheckpoint db key
        resFromDb `shouldBe` Just cp2

dbMultiplePutsParProp
    :: DBLayer IO DummyState
    -> KeyValPairs (PrimaryKey WalletId) (Wallet DummyState)
    -> Property
dbMultiplePutsParProp db (KeyValPairs keyValPairs) = monadicIO $ do
    meta <- pick arbitrary
    cp <- pick arbitrary
    liftIO $ do
        unsafeRunExceptT $ once_ keyValPairs $ \k -> createWallet db k cp meta
        forConcurrently_ keyValPairs
            (\(k, cp') -> unsafeRunExceptT $ putCheckpoint db k cp')
        resFromDb <- Set.fromList <$> listWallets db
        resFromDb `shouldBe` (Set.fromList (map fst keyValPairs))

{-------------------------------------------------------------------------------
                      Tests machinery, Arbitrary instances
-------------------------------------------------------------------------------}

once :: (Ord k, Monad m) => [(k,v)] -> (k -> m a) -> m [a]
once xs = forM (Map.keys (Map.fromList xs))

once_ :: (Ord k, Monad m) => [(k,v)] -> (k -> m a) -> m ()
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
