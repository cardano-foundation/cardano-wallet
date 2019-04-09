{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
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
    ( forM_ )
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
    describe "Checkpoints" $ before newDBLayer $ do
        it "put . read yield a result"
            (property . prop_readAfterPut putCheckpoint readCheckpoint)
        it "can't put before wallet exists"
            (property . prop_putBeforeInit putCheckpoint readCheckpoint Nothing)
        it "update doesn't affect other resources"
            (property . prop_isolation putCheckpoint readWalletMeta readTxHistoryF)

    describe "Wallet Metadata" $ before newDBLayer $ do
        it "put . read yield a result"
            (property . prop_readAfterPut putWalletMeta readWalletMeta)
        it "can't put before wallet exists"
            (property . prop_putBeforeInit putWalletMeta readWalletMeta Nothing)
        it "update doesn't affect other resources"
            (property . prop_isolation putWalletMeta readTxHistoryF readCheckpoint)

    describe "Tx History" $ before newDBLayer $ do
        it "put . read yield a result"
            (property . prop_readAfterPut putTxHistory readTxHistoryF)
        it "can't put before wallet exists"
            (property . prop_putBeforeInit putTxHistory readTxHistoryF (pure mempty))
        it "update doesn't affect other resources"
            (property . prop_isolation putTxHistory readCheckpoint readWalletMeta)

    describe "DB works as expected" $ before newDBLayer $ do
        it "replacement of checkpoint returns last checkpoint that was put"
            (property . dbReplaceCheckpointsProp)
        it "multiple sequential putCheckpoint work properly"
            (property . dbMultiplePutsSeqProp)
        it "multiple parallel putCheckpoint work properly"
            (property . dbMultiplePutsParProp)
        it "readTxHistory . putTxHistory yields inserted merged history"
            (checkCoverage . dbMergeTxHistoryProp)
  where
    -- | Wrap the result of 'readTxHistory' in an arbitrary identity Applicative
    readTxHistoryF
        :: Monad m
        => DBLayer m s
        -> PrimaryKey WalletId
        -> m (Identity (Map (Hash "Tx") (Tx, TxMeta)))
    readTxHistoryF db = fmap Identity . readTxHistory db

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
    -> DBLayer IO DummyState
        -- ^ DB Layer
    -> (PrimaryKey WalletId, a)
        -- ^ Property arguments
    -> Property
prop_readAfterPut putOp readOp db (key, a) =
    monadicIO (setup *> prop)
  where
    setup = do
        (cp, meta) <- pick arbitrary
        liftIO $ unsafeRunExceptT $ createWallet db key cp meta
    prop = liftIO $ do
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
    -> DBLayer IO DummyState
        -- ^ DB Layer
    -> (PrimaryKey WalletId, a)
        -- ^ Property arguments
    -> Property
prop_putBeforeInit putOp readOp empty db (key@(PrimaryKey wid), a) =
    monadicIO (setup *> prop)
  where
    setup = return ()
    prop = liftIO $ do
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
    -> DBLayer IO DummyState
        -- ^ DB Layer
    -> (PrimaryKey WalletId, a)
        -- ^ Properties arguments
    -> Property
prop_isolation putA readB readC db (key, a) =
    monadicIO (setup >>= prop)
  where
    setup = do
        (cp, meta, txs) <- pick arbitrary
        liftIO $ unsafeRunExceptT $ createWallet db key cp meta
        liftIO $ unsafeRunExceptT $ putTxHistory db key txs
        liftIO $ (,) <$> readB db key <*> readC db key

    prop (b, c) = liftIO $ do
        unsafeRunExceptT $ putA db key a
        readB db key `shouldReturn` b
        readC db key `shouldReturn` c

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

dbMultiplePutsSeqProp
    :: DBLayer IO DummyState
    -> KeyValPairs (PrimaryKey WalletId) (Wallet DummyState)
    -> Property
dbMultiplePutsSeqProp db (KeyValPairs keyValPairs) = monadicIO $ do
    meta <- pick arbitrary
    liftIO $ do
        unsafeRunExceptT $ once keyValPairs $ \(k, cp) -> createWallet db k cp meta
        unsafeRunExceptT $ forM_ keyValPairs $ uncurry (putCheckpoint db)
        resFromDb <- Set.fromList <$> listWallets db
        resFromDb `shouldBe` (Set.fromList (map fst keyValPairs))

dbMultiplePutsParProp
    :: DBLayer IO DummyState
    -> KeyValPairs (PrimaryKey WalletId) (Wallet DummyState)
    -> Property
dbMultiplePutsParProp db (KeyValPairs keyValPairs) = monadicIO $ do
    meta <- pick arbitrary
    liftIO $ do
        unsafeRunExceptT $ once keyValPairs $ \(k, cp) -> createWallet db k cp meta
        forConcurrently_ keyValPairs
            (\(k, cp) -> unsafeRunExceptT $ putCheckpoint db k cp)
        resFromDb <- Set.fromList <$> listWallets db
        resFromDb `shouldBe` (Set.fromList (map fst keyValPairs))

dbMergeTxHistoryProp
    :: DBLayer IO DummyState
    -> KeyValPairs (PrimaryKey WalletId) (Map (Hash "Tx") (Tx, TxMeta))
    -> Property
dbMergeTxHistoryProp db (KeyValPairs keyValPairs) =
    cover 90 cond "conflicting tx histories" prop
  where
    restrictTo k = filter ((== k) . fst)
    -- Make sure that we have some conflicting insertion to actually test the
    -- semantic of the DB Layer.
    cond = L.length (L.nub ids) /= L.length ids
      where
        ids = concatMap (\(w, m) -> (w,) <$> Map.keys m) keyValPairs
    prop = monadicIO $ do
        cp <- pick arbitrary
        meta <- pick arbitrary
        liftIO $ do
            unsafeRunExceptT $ once keyValPairs $ \(k, _) -> createWallet db k cp meta
            unsafeRunExceptT $ forM_ keyValPairs $ uncurry (putTxHistory db)
            forM_ keyValPairs $ \(key, _) -> do
                res <- readTxHistory db key
                res `shouldBe` (Map.unions (snd <$> restrictTo key keyValPairs))

{-------------------------------------------------------------------------------
                      Tests machinery, Arbitrary instances
-------------------------------------------------------------------------------}

once :: (Ord k, Monad m) => [(k,v)] -> ((k,v) -> m a) -> m ()
once xs = forM_ (Map.toList (Map.fromList xs))

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
