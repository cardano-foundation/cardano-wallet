{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.DBSpec
    ( spec
    , dbPropertyTests
    , withDB
    , GenTxHistory (..)
    , KeyValPairs (..)
    , TxHistory
    , filterTxHistory
    ) where

import Prelude

import Cardano.Crypto.Wallet
    ( unXPrv )
import Cardano.Wallet.DB
    ( DBLayer (..)
    , ErrNoSuchWallet (..)
    , ErrWalletAlreadyExists (..)
    , PrimaryKey (..)
    , cleanDB
    )
import Cardano.Wallet.DB.Sqlite
    ( PersistTx (..) )
import Cardano.Wallet.DummyTarget.Primitive.Types
    ( DummyTarget, Tx (..), block0 )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..), Key, Passphrase (..), XPrv, XPub, keyToAddress, publicKey )
import Cardano.Wallet.Primitive.AddressDerivation.Sequential
    ( ChangeChain (..)
    , deriveAddressPublicKey
    , generateKeyFromSeed
    , unsafeGenerateKeyFromSeed
    )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( AddressPool
    , AddressPoolGap (..)
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
    , Range (..)
    , SlotId (..)
    , SortOrder (..)
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
    , defaultTxSortOrder
    , isPending
    , isWithinRange
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
import Crypto.Hash
    ( hash )
import Data.Functor
    ( ($>) )
import Data.Functor.Identity
    ( Identity (..) )
import Data.Ord
    ( Down (..) )
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
    , shrinkList
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

-- | Keep the unions (right-biased) of all transactions, and sort them in the
-- default order for readTxHistory.
sortedUnions :: Ord k => [(k, GenTxHistory)] -> [Identity GenTxHistory]
sortedUnions = map (Identity . sort' . runIdentity) . unions
  where
    sort' = GenTxHistory
      . filterTxHistory defaultTxSortOrder wholeRange
      . unGenTxHistory

-- | Execute an action once per key @k@ present in the given list
once :: (Ord k, Monad m) => [(k,v)] -> ((k,v) -> m a) -> m [a]
once xs = forM (Map.toList (Map.fromList xs))

-- | Like 'once', but discards the result
once_ :: (Ord k, Monad m) => [(k,v)] -> ((k,v) -> m a) -> m ()
once_ xs = void . once xs

-- | Shorthand for the readTxHistory result type.
type TxHistory = [(Hash "Tx", (Tx, TxMeta))]

-- | Apply optional filters on slotId and sort using the default sort order
-- (first time/slotId, then by TxId) to a 'TxHistory'.
filterTxHistory :: SortOrder -> Range SlotId -> TxHistory -> TxHistory
filterTxHistory order range =
    filter (isWithinRange range . slotId . snd . snd)
    . (case order of
        Ascending -> reverse
        Descending -> id)
    . sortBySlot
    . sortByTxId
  where
    sortBySlot = L.sortOn (Down . slotId . snd . snd)
    sortByTxId = L.sortOn fst

newtype KeyValPairs k v = KeyValPairs [(k, v)]
    deriving (Generic, Show, Eq)

instance (Arbitrary k, Arbitrary v) => Arbitrary (KeyValPairs k v) where
    shrink = genericShrink
    arbitrary = do
        pairs <- choose (1, 10) >>= flip vectorOf arbitrary
        pure $ KeyValPairs pairs

instance Arbitrary (PrimaryKey WalletId) where
    shrink _ = []
    arbitrary = do
        bytes <- B8.pack . pure <$> elements ['a'..'k']
        return $ PrimaryKey $ WalletId $ hash bytes

instance Arbitrary (Wallet (SeqState DummyTarget) DummyTarget) where
    shrink _ = []
    arbitrary = initWallet block0 <$> arbitrary

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

instance PersistTx DummyTarget where
    resolvedInputs = flip zip (repeat Nothing) . inputs
    mkTx inps = Tx (fst <$> inps)

instance Arbitrary Tx where
    shrink (Tx ins outs) =
        [Tx ins' outs | ins' <- shrinkList' ins ] ++
        [Tx ins outs' | outs' <- shrinkList' outs ]
      where
        shrinkList' xs  = filter (not . null)
            [ take n xs | Positive n <- shrink (Positive $ length xs) ]

    arbitrary = Tx
        <$> fmap (L.nub . L.take 5 . getNonEmpty) arbitrary
        <*> fmap (L.take 5 . getNonEmpty) arbitrary

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
        <$> (Hash . B8.pack <$> vectorOf 32 arbitrary)
        <*> scale (`mod` 3) arbitrary -- No need for a high indexes

instance Arbitrary TxOut where
    -- No Shrinking
    arbitrary = TxOut
        <$> arbitrary
        <*> arbitrary

newtype GenTxHistory = GenTxHistory { unGenTxHistory :: TxHistory }
    deriving stock (Show, Eq)
    deriving newtype (Semigroup, Monoid)

instance Arbitrary GenTxHistory where
    shrink (GenTxHistory h) = map GenTxHistory (shrinkList shrinkOneTx h)
      where
        shrinkOneTx :: (Hash "Tx", (Tx, TxMeta)) -> [(Hash "Tx", (Tx, TxMeta))]
        shrinkOneTx (txid, (tx, meta)) =
            [(txid, (tx', meta)) | tx' <- shrink tx]

    -- Ensure unique transaction IDs within a given batch of transactions to add
    -- to the history.
    arbitrary = GenTxHistory . sortTxHistory <$> do
        -- NOTE
        -- We discard pending transaction from any 'GenTxHistory since,
        -- inserting a pending transaction actually has an effect on the
        -- checkpoint's pending transactions of the same wallet.
        txs <- filter (not . isPending . snd) <$> arbitrary
        return $ (\(tx, meta) -> (mockTxId tx, (tx, meta))) <$> txs
      where
        mockTxId :: Tx -> Hash "Tx"
        mockTxId = Hash . B8.pack . show

        sortTxHistory = filterTxHistory defaultTxSortOrder wholeRange

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
        <$> genPassphrase @"seed" (16, 32)
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
    :: Functor m
    => DBLayer m s DummyTarget
    -> PrimaryKey WalletId
    -> m (Identity GenTxHistory)
readTxHistoryF db wid =
    (Identity . GenTxHistory)
    <$> readTxHistory db wid defaultTxSortOrder wholeRange

putTxHistoryF
    :: DBLayer m s DummyTarget
    -> PrimaryKey WalletId
    -> GenTxHistory
    -> ExceptT ErrNoSuchWallet m ()
putTxHistoryF db wid =
    putTxHistory db wid . Map.fromList . unGenTxHistory


{-------------------------------------------------------------------------------
                                    Properties
-------------------------------------------------------------------------------}

-- | Can list created wallets
prop_createListWallet
    :: DBLayer IO s DummyTarget
    -> KeyValPairs (PrimaryKey WalletId) (Wallet s DummyTarget, WalletMetadata)
    -> Property
prop_createListWallet db (KeyValPairs pairs) =
    monadicIO (setup >> prop)
  where
    setup = liftIO (cleanDB db)
    prop = liftIO $ do
        res <- once pairs $ \(k, (cp, meta)) ->
            unsafeRunExceptT $ createWallet db k cp meta
        (length <$> listWallets db) `shouldReturn` length res

-- | Trying to create a same wallet twice should yield an error
prop_createWalletTwice
    :: DBLayer IO s DummyTarget
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
        runExceptT (createWallet db key cp meta) `shouldReturn` Right ()
        runExceptT (createWallet db key cp meta) `shouldReturn` Left err

-- | Trying to remove a same wallet twice should yield an error
prop_removeWalletTwice
    :: DBLayer IO s DummyTarget
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
        unsafeRunExceptT $ createWallet db key cp meta
    prop = liftIO $ do
        let err = ErrNoSuchWallet wid
        runExceptT (removeWallet db key) `shouldReturn` Right ()
        runExceptT (removeWallet db key) `shouldReturn` Left err

-- | Checks that a given resource can be read after having been inserted in DB.
prop_readAfterPut
    :: ( Show (f a), Eq (f a), Applicative f
       , Arbitrary (Wallet s DummyTarget))
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
prop_readAfterPut putOp readOp db (key, a) =
    monadicIO (setup >> prop)
  where
    setup = do
        liftIO (cleanDB db)
        (cp, meta) <- pick arbitrary
        liftIO $ unsafeRunExceptT $ createWallet db key cp meta
    prop = liftIO $ do
        unsafeRunExceptT $ putOp db key a
        res <- readOp db key
        res `shouldBe` pure a

-- | Can't put resource before a wallet has been initialized
prop_putBeforeInit
    :: (Show (f a), Eq (f a))
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
prop_putBeforeInit putOp readOp empty db (key@(PrimaryKey wid), a) =
    monadicIO (setup >> prop)
  where
    setup = liftIO (cleanDB db)
    prop = liftIO $ do
        runExceptT (putOp db key a) >>= \case
            Right _ ->
                fail "expected put operation to fail but it succeeded!"
            Left err ->
                err `shouldBe` ErrNoSuchWallet wid
        readOp db key `shouldReturn` empty

-- | Modifying one resource leaves the other untouched
prop_isolation
    :: ( Show (f b), Eq (f b)
       , Show (g c), Eq (g c)
       , Show (h d), Eq (h d)
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
prop_isolation putA readB readC readD db (key, a) =
    monadicIO (setup >>= prop)
  where
    setup = do
        liftIO (cleanDB db)
        (cp, meta, GenTxHistory txs) <- pick arbitrary
        liftIO $ unsafeRunExceptT $ createWallet db key cp meta
        liftIO $ unsafeRunExceptT $ putTxHistory db key (Map.fromList txs)
        (b, c, d) <- liftIO $ (,,)
            <$> readB db key
            <*> readC db key
            <*> readD db key
        return (b, c, d)

    prop (b, c, d) = liftIO $ do
        unsafeRunExceptT $ putA db key a
        readB db key `shouldReturn` b
        readC db key `shouldReturn` c
        readD db key `shouldReturn` d

-- | Can't read back data after delete
prop_readAfterDelete
    :: (Show (f a), Eq (f a), Arbitrary (Wallet s DummyTarget))
    => (  DBLayer IO s DummyTarget
       -> PrimaryKey WalletId
       -> IO (f a)
       ) -- ^ Read Operation
    -> f a
        -- ^ An 'empty' value for the 'Applicative' f
    -> DBLayer IO s DummyTarget
    -> PrimaryKey WalletId
    -> Property
prop_readAfterDelete readOp empty db key =
    monadicIO (setup >> prop)
  where
    setup = do
        liftIO (cleanDB db)
        (cp, meta) <- pick arbitrary
        liftIO $ unsafeRunExceptT $ createWallet db key cp meta
    prop = liftIO $ do
        unsafeRunExceptT $ removeWallet db key
        readOp db key `shouldReturn` empty

-- | Check that the DB supports multiple sequential puts for a given resource
prop_sequentialPut
    :: (Show (f a), Eq (f a), Arbitrary (Wallet s DummyTarget))
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
prop_sequentialPut putOp readOp resolve db (KeyValPairs pairs) =
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
            createWallet db k cp meta
    prop = liftIO $ do
        unsafeRunExceptT $ forM_ pairs $ uncurry (putOp db)
        res <- once pairs (readOp db . fst)
        res `shouldBe` resolve pairs

-- | Check that the DB supports multiple sequential puts for a given resource
prop_parallelPut
    :: (Arbitrary (Wallet s DummyTarget))
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
            createWallet db k cp meta
    prop = liftIO $ do
        forConcurrently_ pairs $ unsafeRunExceptT . uncurry (putOp db)
        res <- once pairs (readOp db . fst)
        length res `shouldBe` resolve pairs

dbPropertyTests
    :: (Arbitrary (Wallet s DummyTarget), Eq s)
    => SpecWith (DBLayer IO s DummyTarget)
dbPropertyTests = do
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

-- | Provide a DBLayer to a Spec that requires it. The database is initialised
-- once, and cleared with 'cleanDB' before each test.
withDB :: IO (DBLayer IO s t) -> SpecWith (DBLayer IO s t) -> Spec
withDB create = beforeAll create . beforeWith (\db -> cleanDB db $> db)
