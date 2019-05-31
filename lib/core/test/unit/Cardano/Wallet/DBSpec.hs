{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.DBSpec
    ( spec
    , dbPropertyTests
    , withDB
    , DummyTarget
    , SkipTests(..)
    ) where

import Prelude

import Cardano.Crypto.Wallet
    ( unXPrv, unXPub )
import Cardano.Wallet
    ( unsafeRunExceptT )
import Cardano.Wallet.DB
    ( DBLayer (..)
    , ErrNoSuchWallet (..)
    , ErrWalletAlreadyExists (..)
    , PrimaryKey (..)
    , cleanDB
    )
import Cardano.Wallet.Primitive.AddressDerivation
    ( ChangeChain (..)
    , Depth (..)
    , Key
    , KeyToAddress (..)
    , Passphrase (..)
    , XPrv
    , XPub
    , deriveAddressPublicKey
    , generateKeyFromSeed
    , getKey
    , publicKey
    , unsafeGenerateKeyFromSeed
    )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( AddressPool
    , AddressPoolGap (..)
    , IsOurs (..)
    , SeqState (..)
    , accountPubKey
    , addresses
    , changeChain
    , emptyPendingIxs
    , gap
    , mkAddressPool
    , mkAddressPoolGap
    )
import Cardano.Wallet.Primitive.Model
    ( Wallet, initWallet )
import Cardano.Wallet.Primitive.Types
    ( Address (..)
    , Coin (..)
    , Direction (..)
    , Hash (..)
    , SlotId (..)
    , Tx (..)
    , TxId (..)
    , TxIn (..)
    , TxMeta (..)
    , TxOut (..)
    , TxStatus (..)
    , UTxO (..)
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
    ( forM, forM_, void, when )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Monad.Trans.Except
    ( ExceptT, runExceptT )
import Crypto.Hash
    ( hash )
import Data.Functor
    ( ($>) )
import Data.Functor.Identity
    ( Identity (..) )
import Data.Map.Strict
    ( Map )
import Data.Quantity
    ( Percentage, Quantity (..), mkPercentage )
import Data.Typeable
    ( Typeable )
import Data.Word
    ( Word32 )
import GHC.Generics
    ( Generic )
import System.IO.Unsafe
    ( unsafePerformIO )
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
    , InfiniteList (..)
    , NonEmptyList (..)
    , Positive (..)
    , Property
    , arbitraryBoundedEnum
    , checkCoverage
    , choose
    , cover
    , elements
    , generate
    , genericShrink
    , oneof
    , property
    , scale
    , suchThat
    , vectorOf
    )
import Test.QuickCheck.Instances.Time
    ()
import Test.QuickCheck.Monadic
    ( monadicIO, pick )

import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.List as L
import qualified Data.Map.Strict as Map

spec :: Spec
spec = return ()

{-------------------------------------------------------------------------------
                    Cross DB Specs Shared Arbitrary Instances
-------------------------------------------------------------------------------}
-- | Keep only the (L)ast (R)ecently (P)ut entry
lrp :: (Applicative f, Ord k) => [(k, v)] -> [f v]
lrp = Map.elems . foldl (\m (k, v) -> Map.insert k (pure v) m) mempty

-- | Keep the unions (right-biaised) of all entry
unions :: (Monoid v, Ord k) => [(k, v)] -> [Identity v]
unions =
    fmap Identity
    . Map.elems
    . foldl (\m (k, v) -> Map.unionWith (<>) (Map.fromList [(k, v)]) m) mempty

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

data DummyTarget

instance KeyToAddress DummyTarget where
    keyToAddress = Address . unXPub . getKey

deriving instance Eq (SeqState DummyTarget)

instance Arbitrary (Wallet (SeqState DummyTarget) DummyTarget) where
    shrink _ = []
    arbitrary = initWallet <$> arbitrary

instance TxId DummyTarget where
    txId = Hash . B8.pack . show

instance Arbitrary (PrimaryKey WalletId) where
    shrink _ = []
    arbitrary = do
        bytes <- B8.pack . pure <$> elements ['a'..'k']
        return $ PrimaryKey $ WalletId $ hash bytes

deriving instance Show (PrimaryKey WalletId)

instance Arbitrary Address where
    -- No Shrinking
    arbitrary = oneof
        [ pure $ Address "ADDR01"
        , pure $ Address "ADDR02"
        , pure $ Address "ADDR03"
        , pure $ Address "ADDR04"
        , pure $ Address "ADDR05"
        , pure $ Address "ADDR06"
        , pure $ Address "ADDR07"
        , pure $ Address "ADDR08"
        , pure $ Address "ADDR09"
        , pure $ Address "ADDR10"
        ]

instance Arbitrary (SeqState DummyTarget) where
    shrink (SeqState intPool extPool ixs) =
        (\(i, e) -> SeqState i e ixs) <$> shrink (intPool, extPool)
    arbitrary = do
        intPool <- arbitrary
        extPool <- arbitrary
        return $ SeqState intPool extPool emptyPendingIxs

instance Typeable chain => Arbitrary (AddressPool DummyTarget chain) where
    shrink pool =
        let
            key = accountPubKey pool
            g = gap pool
            addrs = addresses pool
        in case length addrs of
            k | k == fromEnum g && g == minBound ->
                []
            k | k == fromEnum g && g > minBound ->
                [ mkAddressPool key minBound [] ]
            k ->
                [ mkAddressPool key minBound []
                , mkAddressPool key g []
                , mkAddressPool key g (take (k - (fromEnum g `div` 5)) addrs)
                ]
    arbitrary = do
        g <- unsafeMkAddressPoolGap <$> choose
            (getAddressPoolGap minBound, 2 * getAddressPoolGap minBound)
        n <- choose (0, 2 * fromEnum g)
        let addrs = take n (ourAddresses (changeChain @chain))
        return $ mkAddressPool ourAccount g addrs

unsafeMkAddressPoolGap :: (Integral a, Show a) => a -> AddressPoolGap
unsafeMkAddressPoolGap g = case (mkAddressPoolGap $ fromIntegral g) of
    Right a -> a
    Left _ -> error $ "unsafeMkAddressPoolGap: bad argument: " <> show g

ourAccount
    :: Key 'AccountK XPub
ourAccount = publicKey $ unsafeGenerateKeyFromSeed (seed, mempty) mempty
  where
    seed = Passphrase $ BA.convert $ BS.replicate 32 0

ourAddresses
    :: ChangeChain
    -> [Address]
ourAddresses cc =
    keyToAddress @DummyTarget . deriveAddressPublicKey ourAccount cc
        <$> [minBound..maxBound]

instance Arbitrary (Hash "Tx") where
    shrink _ = [] -- no way to shrink a hash
    arbitrary = do
        k <- choose (0, 10)
        return $ Hash (B8.pack ("TX" <> show @Int k))

instance Arbitrary Tx where
    shrink (Tx ins outs) =
        [Tx ins' outs | ins' <- shrinkList ins ] ++
        [Tx ins outs' | outs' <- shrinkList outs ]
      where
        shrinkList xs  = filter (not . null)
            [ take n xs | Positive n <- shrink (Positive $ length xs) ]

    arbitrary = Tx <$> fmap (L.nub . getNonEmpty) arbitrary
                   <*> fmap getNonEmpty arbitrary

instance Arbitrary TxMeta where
    shrink _ = []
    arbitrary = TxMeta
        <$> elements [Pending, InLedger, Invalidated]
        <*> elements [Incoming, Outgoing]
        <*> (SlotId <$> choose (0, 1000) <*> choose (0, 21599))
        <*> fmap (Quantity . fromIntegral) (arbitrary @Word32)

customizedGen :: Gen Percentage
customizedGen = do
    let (Right upperBound) = mkPercentage @Int 100
    arbitraryBoundedEnum `suchThat` (/= upperBound)

instance Arbitrary WalletMetadata where
    shrink _ = []
    arbitrary =  WalletMetadata
        <$> (WalletName <$> elements ["bulbazaur", "charmander", "squirtle"])
        <*> arbitrary
        <*> (fmap WalletPassphraseInfo <$> arbitrary)
        <*> oneof [pure Ready, Restoring . Quantity <$> customizedGen]
        <*> pure NotDelegating

instance Arbitrary Coin where
    -- No Shrinking
    arbitrary = Coin <$> choose (1, 100000)

instance Arbitrary TxIn where
    -- No Shrinking
    arbitrary = TxIn
        <$> arbitrary
        <*> scale (`mod` 3) arbitrary -- No need for a high indexes

instance Arbitrary TxOut where
    -- No Shrinking
    arbitrary = TxOut
        <$> arbitrary
        <*> arbitrary

instance Arbitrary UTxO where
    shrink (UTxO utxo) = UTxO <$> shrink utxo
    arbitrary = do
        n <- choose (1, 100)
        utxo <- zip
            <$> vectorOf n arbitrary
            <*> vectorOf n arbitrary
        return $ UTxO $ Map.fromList utxo

instance Arbitrary (Key 'RootK XPrv) where
    shrink _ = []
    arbitrary = elements rootKeys

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

genRootKeys :: Gen (Key 'RootK XPrv)
genRootKeys = do
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

-- Properties above are quite heavy on the generation of values, althrough for
-- private keys, it isn't particularly useful / relevant to generate many of
-- them as they're really treated as an opaque type.
-- Instead, we generate them once, and picks from the list.
rootKeys :: [Key 'RootK XPrv]
rootKeys = unsafePerformIO $ generate (vectorOf 10 genRootKeys)
{-# NOINLINE rootKeys #-}

-- | Wrap the result of 'readTxHistory' in an arbitrary identity Applicative
readTxHistoryF
    :: (Monad m, IsOurs s, NFData s, Show s, TxId t)
    => DBLayer m s t
    -> PrimaryKey WalletId
    -> m (Identity (Map (Hash "Tx") (Tx, TxMeta)))
readTxHistoryF db = fmap Identity . readTxHistory db


{-------------------------------------------------------------------------------
                                    Properties
-------------------------------------------------------------------------------}

-- | Can list created wallets
prop_createListWallet
    :: (Show s, Eq s, IsOurs s, NFData s)
    => DBLayer IO s DummyTarget
    -> KeyValPairs (PrimaryKey WalletId) (Wallet s DummyTarget, WalletMetadata)
    -> Property
prop_createListWallet dbLayer (KeyValPairs pairs) =
    monadicIO (pure dbLayer >>= prop)
  where
    prop db = liftIO $ do
        res <- once pairs $ \(k, (cp, meta)) ->
            unsafeRunExceptT $ createWallet db k cp meta
        (length <$> listWallets db) `shouldReturn` length res

-- | Trying to create a same wallet twice should yield an error
prop_createWalletTwice
    :: (Show s, Eq s, IsOurs s, NFData s)
    => DBLayer IO s DummyTarget
    -> ( PrimaryKey WalletId
       , Wallet s DummyTarget
       , WalletMetadata
       )
    -> Property
prop_createWalletTwice dbLayer (key@(PrimaryKey wid), cp, meta) =
    monadicIO (pure dbLayer >>= prop)
  where
    prop db = liftIO $ do
        let err = ErrWalletAlreadyExists wid
        runExceptT (createWallet db key cp meta) `shouldReturn` Right ()
        runExceptT (createWallet db key cp meta) `shouldReturn` Left err

-- | Trying to remove a same wallet twice should yield an error
prop_removeWalletTwice
    :: (Show s, Eq s, IsOurs s, NFData s)
    => DBLayer IO s DummyTarget
    -> ( PrimaryKey WalletId
       , Wallet s DummyTarget
       , WalletMetadata
       )
    -> Property
prop_removeWalletTwice dbLayer (key@(PrimaryKey wid), cp, meta) =
    monadicIO (setup >>= prop)
  where
    setup = liftIO $ do
        unsafeRunExceptT $ createWallet dbLayer key cp meta
        return dbLayer
    prop db = liftIO $ do
        let err = ErrNoSuchWallet wid
        runExceptT (removeWallet db key) `shouldReturn` Right ()
        runExceptT (removeWallet db key) `shouldReturn` Left err

-- | Checks that a given resource can be read after having been inserted in DB.
prop_readAfterPut
    :: ( Show (f a), Eq (f a), Applicative f
       , Show s, Eq s, IsOurs s, NFData s, Arbitrary (Wallet s DummyTarget))
    => (  DBLayer IO s DummyTarget
       -> PrimaryKey WalletId
       -> a
       -> ExceptT ErrNoSuchWallet IO ()
       ) -- ^ Put Operation
    -> (  DBLayer IO s DummyTarget
       -> PrimaryKey WalletId
       -> IO (f a)
       ) -- ^ Read Operation
    -> DBLayer IO s DummyTarget
    -> (PrimaryKey WalletId, a)
        -- ^ Property arguments
    -> Property
prop_readAfterPut putOp readOp dbLayer (key, a) =
    monadicIO (setup >>= prop)
  where
    setup = do
        (cp, meta) <- pick arbitrary
        liftIO $ unsafeRunExceptT $ createWallet dbLayer key cp meta
        return dbLayer
    prop db = liftIO $ do
        unsafeRunExceptT $ putOp db key a
        res <- readOp db key
        res `shouldBe` pure a

-- | Can't put resource before a wallet has been initialized
prop_putBeforeInit
    :: (Show (f a), Eq (f a), Applicative f, Show s, Eq s, IsOurs s, NFData s)
    => (  DBLayer IO s DummyTarget
       -> PrimaryKey WalletId
       -> a
       -> ExceptT ErrNoSuchWallet IO ()
       ) -- ^ Put Operation
    -> (  DBLayer IO s DummyTarget
       -> PrimaryKey WalletId
       -> IO (f a)
       ) -- ^ Read Operation
    -> f a
        -- ^ An 'empty' value for the 'Applicative' f
    -> DBLayer IO s DummyTarget
    -> (PrimaryKey WalletId, a)
        -- ^ Property arguments
    -> Property
prop_putBeforeInit putOp readOp empty dbLayer (key@(PrimaryKey wid), a) =
    monadicIO (pure dbLayer >>= prop)
  where
    prop db = liftIO $ do
        runExceptT (putOp db key a) >>= \case
            Right _ ->
                fail "expected put operation to fail but it succeeded!"
            Left err ->
                err `shouldBe` ErrNoSuchWallet wid
        readOp db key `shouldReturn` empty

-- | Modifying one resource leaves the other untouched
prop_isolation
    :: ( Applicative f, Show (f b), Eq (f b)
       , Applicative g, Show (g c), Eq (g c)
       , Applicative h, Show (h d), Eq (h d)
       , Show s, Eq s, IsOurs s, NFData s
       , Arbitrary (Wallet s DummyTarget)
       )
    => (  DBLayer IO s DummyTarget
       -> PrimaryKey WalletId
       -> a
       -> ExceptT ErrNoSuchWallet IO ()
       ) -- ^ Put Operation
    -> (  DBLayer IO s DummyTarget
       -> PrimaryKey WalletId
       -> IO (f b)
       ) -- ^ Read Operation for another resource
    -> (  DBLayer IO s DummyTarget
       -> PrimaryKey WalletId
       -> IO (g c)
       ) -- ^ Read Operation for another resource
    -> (  DBLayer IO s DummyTarget
       -> PrimaryKey WalletId
       -> IO (h d)
       ) -- ^ Read Operation for another resource
    -> DBLayer IO s DummyTarget
    -> (PrimaryKey WalletId, a)
        -- ^ Properties arguments
    -> Property
prop_isolation putA readB readC readD dbLayer (key, a) =
    monadicIO (setup >>= prop)
  where
    setup = do
        (cp, meta, txs) <- pick arbitrary
        liftIO $ unsafeRunExceptT $ createWallet dbLayer key cp meta
        liftIO $ unsafeRunExceptT $ putTxHistory dbLayer key txs
        (b, c, d) <- liftIO $ (,,)
            <$> readB dbLayer key
            <*> readC dbLayer key
            <*> readD dbLayer key
        return (dbLayer, (b, c, d))

    prop (db, (b, c, d)) = liftIO $ do
        unsafeRunExceptT $ putA db key a
        readB db key `shouldReturn` b
        readC db key `shouldReturn` c
        readD db key `shouldReturn` d

-- | Can't read back data after delete
prop_readAfterDelete
    :: ( Show (f a), Eq (f a), Applicative f
       , Show s, Eq s, IsOurs s, NFData s
       , Arbitrary (Wallet s DummyTarget)
       )
    => (  DBLayer IO s DummyTarget
       -> PrimaryKey WalletId
       -> IO (f a)
       ) -- ^ Read Operation
    -> f a
        -- ^ An 'empty' value for the 'Applicative' f
    -> DBLayer IO s DummyTarget
    -> PrimaryKey WalletId
    -> Property
prop_readAfterDelete readOp empty dbLayer key =
    monadicIO (setup >>= prop)
  where
    setup = do
        (cp, meta) <- pick arbitrary
        liftIO $ unsafeRunExceptT $ createWallet dbLayer key cp meta
        return dbLayer
    prop db = liftIO $ do
        unsafeRunExceptT $ removeWallet db key
        readOp db key `shouldReturn` empty


-- | Check that the DB supports multiple sequential puts for a given resource
prop_sequentialPut
    :: ( Show (f a), Eq (f a), Applicative f
       , Show s, Eq s, IsOurs s, NFData s
       , Arbitrary (Wallet s DummyTarget)
       )
    => (  DBLayer IO s DummyTarget
       -> PrimaryKey WalletId
       -> a
       -> ExceptT ErrNoSuchWallet IO ()
       ) -- ^ Put Operation
    -> (  DBLayer IO s DummyTarget
       -> PrimaryKey WalletId
       -> IO (f a)
       ) -- ^ Read Operation
    -> (forall k. Ord k => [(k, a)] -> [f a])
        -- ^ How do we expect operations to resolve
    -> DBLayer IO s DummyTarget
    -> KeyValPairs (PrimaryKey WalletId) a
        -- ^ Property arguments
    -> Property
prop_sequentialPut putOp readOp resolve dbLayer (KeyValPairs pairs) =
    cover 90 cond "conflicting db entries" $ monadicIO (setup >>= prop)
  where
    -- Make sure that we have some conflicting insertion to actually test the
    -- semantic of the DB Layer.
    cond = L.length (L.nub ids) /= L.length ids
      where
        ids = map fst pairs
    setup = do
        (cp, meta) <- pick arbitrary
        liftIO $ unsafeRunExceptT $ once_ pairs $ \(k, _) ->
            createWallet dbLayer k cp meta
        return dbLayer
    prop db = liftIO $ do
        unsafeRunExceptT $ forM_ pairs $ uncurry (putOp db)
        res <- once pairs (readOp db . fst)
        res `shouldBe` resolve pairs


-- | Check that the DB supports multiple sequential puts for a given resource
prop_parallelPut
    :: ( Show (f a), Eq (f a), Applicative f
       , Show s, Eq s, IsOurs s, NFData s
       , Arbitrary (Wallet s DummyTarget)
       )
    => (  DBLayer IO s DummyTarget
       -> PrimaryKey WalletId
       -> a
       -> ExceptT ErrNoSuchWallet IO ()
       ) -- ^ Put Operation
    -> (  DBLayer IO s DummyTarget
       -> PrimaryKey WalletId
       -> IO (f a)
       ) -- ^ Read Operation
    -> (forall k. Ord k => [(k, a)] -> Int)
        -- ^ How many entries to we expect in the end
    -> DBLayer IO s DummyTarget
    -> KeyValPairs (PrimaryKey WalletId) a
        -- ^ Property arguments
    -> Property
prop_parallelPut putOp readOp resolve dbLayer (KeyValPairs pairs) =
    cover 90 cond "conflicting db entries" $ monadicIO (setup >>= prop)
  where
    -- Make sure that we have some conflicting insertion to actually test the
    -- semantic of the DB Layer.
    cond = L.length (L.nub ids) /= L.length ids
      where
        ids = map fst pairs
    setup = do
        (cp, meta) <- pick arbitrary
        liftIO $ unsafeRunExceptT $ once_ pairs $ \(k, _) ->
            createWallet dbLayer k cp meta
        return dbLayer
    prop db = liftIO $ do
        forConcurrently_ pairs $ unsafeRunExceptT . uncurry (putOp db)
        res <- once pairs (readOp db . fst)
        length res `shouldBe` resolve pairs

data SkipTests
    = RunAllTests
    | SkipTxHistoryReplaceTest
    deriving (Show, Eq)

dbPropertyTests
    :: (Arbitrary (Wallet s DummyTarget), Show s, Eq s, IsOurs s, NFData s)
    => SkipTests
    -> SpecWith (DBLayer IO s DummyTarget)
dbPropertyTests skip = do
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
            (property . (prop_readAfterPut putTxHistory readTxHistoryF))
        it "Private Key"
            (property . (prop_readAfterPut putPrivateKey readPrivateKey))

    describe "can't put before wallet exists" $ do
        it "Checkpoint"
            (property . (prop_putBeforeInit putCheckpoint readCheckpoint Nothing))
        it "Wallet Metadata"
            (property . (prop_putBeforeInit putWalletMeta readWalletMeta Nothing))
        it "Tx History"
            (property . (prop_putBeforeInit putTxHistory readTxHistoryF (pure mempty)))
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
            (property . (prop_isolation putTxHistory
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
        when (skip == RunAllTests) $ it "Tx History"
            (checkCoverage . (prop_sequentialPut putTxHistory readTxHistoryF unions))
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
            (checkCoverage . (prop_parallelPut putTxHistory readTxHistoryF
                (length . unions @(Map (Hash "Tx") (Tx, TxMeta)))))
        it "Private Key"
            (checkCoverage . (prop_parallelPut putPrivateKey readPrivateKey
                (length . lrp @Maybe)))

-- | Provide a DBLayer to a Spec that requires it. The database is initialised
-- once, and cleared with 'cleanDB' before each test.
withDB :: IO (DBLayer IO s t) -> SpecWith (DBLayer IO s t) -> Spec
withDB create = beforeAll create . beforeWith (\db -> cleanDB db $> db)
