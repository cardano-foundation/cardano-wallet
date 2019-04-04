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
    ( DBLayer (..), ErrPutTxHistory (..), PrimaryKey (..) )
import Cardano.Wallet.DB.MVar
    ( newDBLayer )
import Cardano.Wallet.Primitive.Model
    ( initWallet )
import Cardano.Wallet.Primitive.Types
    ( Direction (..)
    , Hash (..)
    , IsOurs (..)
    , SlotId (..)
    , Tx (..)
    , TxMeta (..)
    , TxStatus (..)
    , WalletId (..)
    )
import Control.Concurrent.Async
    ( mapConcurrently_ )
import Control.DeepSeq
    ( NFData )
import Control.Monad
    ( forM_, void )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Monad.Trans.Except
    ( runExceptT )
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
    , checkCoverage
    , choose
    , cover
    , elements
    , genericShrink
    , property
    , vectorOf
    )
import Test.QuickCheck.Monadic
    ( monadicIO )

import qualified Data.ByteString.Char8 as B8
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.UUID.Types as UUID


spec :: Spec
spec = do
    describe "DB works as expected" $ before newDBLayer $ do
        it "readCheckpoint . putCheckpoint yields inserted checkpoints"
            (property . dbReadCheckpointProp)
        it "replacement of values returns last value that was put"
            (property . dbReplaceValsProp)
        it "multiple sequential putCheckpoint work properly"
            (property . dbMultiplePutsSeqProp)
        it "multiple parallel putCheckpoint work properly"
            (property . dbMultiplePutsParProp)
        it "readTxHistory . putTxHistory yields inserted merged history"
            (checkCoverage . dbMergeTxHistoryProp)
        it "can't read Tx history if there's no checkpoint"
            (property . dbPutTxHistoryBeforeCheckpointProp)
        it "putTxHistory leaves the wallet state untouched"
            (property . dbPutTxHistoryNoEffectOnWallet)
        it "putCheckpoint leaves the tx history untouched"
            (property . dbPutCheckpointNoEffectOnHistory)

{-------------------------------------------------------------------------------
                                    Properties
-------------------------------------------------------------------------------}


dbReadCheckpointProp
    :: DBLayer IO DummyState
    -> (PrimaryKey WalletId, DummyState)
    -> Property
dbReadCheckpointProp db (key, val)  = monadicIO $ liftIO $ do
    putCheckpoint db key (initWallet val)
    resFromDb <- readCheckpoint db key
    resFromDb `shouldBe` (Just $ initWallet val)

dbReplaceValsProp
    :: DBLayer IO DummyState
    -> (PrimaryKey WalletId, DummyState, DummyState)
    -> Property
dbReplaceValsProp db (key, val1, val2)  = monadicIO $ liftIO $ do
    putCheckpoint db key (initWallet val1)
    putCheckpoint db key (initWallet val2)
    resFromDb <- readCheckpoint db key
    resFromDb `shouldBe` (Just $ initWallet val2)

dbMultiplePutsSeqProp
    :: DBLayer IO DummyState
    -> KeyValPairs (PrimaryKey WalletId) DummyState
    -> Property
dbMultiplePutsSeqProp db (KeyValPairs keyValPairs) = monadicIO $ liftIO $ do
    mapM_ (\(key, val) -> putCheckpoint db key (initWallet val)) keyValPairs
    resFromDb <- Set.fromList <$> readWallets db
    resFromDb `shouldBe` (Set.fromList (map fst keyValPairs))

dbMultiplePutsParProp
    :: DBLayer IO DummyState
    -> KeyValPairs (PrimaryKey WalletId) DummyState
    -> Property
dbMultiplePutsParProp db (KeyValPairs keyValPairs) = monadicIO $ liftIO $ do
    mapConcurrently_
        (\(key, val) -> putCheckpoint db key (initWallet val))
        keyValPairs
    resFromDb <- Set.fromList <$> readWallets db
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
    prop = monadicIO $ liftIO $ do
        forM_ keyValPairs $ \(key, val) -> do
            putCheckpoint db key (initWallet $ DummyState 0)
            unsafeRunExceptT $ putTxHistory db key val
        forM_ keyValPairs $ \(key, _) -> do
            res <- readTxHistory db key
            res `shouldBe` (Map.unions (snd <$> restrictTo key keyValPairs))

dbPutTxHistoryBeforeCheckpointProp
    :: DBLayer IO DummyState
    -> PrimaryKey WalletId
    -> Property
dbPutTxHistoryBeforeCheckpointProp db key@(PrimaryKey wid) = monadicIO $ liftIO $ do
    runExceptT (putTxHistory db key mempty) >>= \case
        Right _ -> fail "expected insertion to fail but it succeeded?"
        Left err -> err `shouldBe` ErrNoSuchWallet wid
    readTxHistory db key `shouldReturn` mempty

dbPutTxHistoryNoEffectOnWallet
    :: DBLayer IO DummyState
    -> (PrimaryKey WalletId, DummyState, Map (Hash "Tx") (Tx, TxMeta))
    -> Property
dbPutTxHistoryNoEffectOnWallet db (key, s, txs) = monadicIO $ liftIO $ do
    let cp = initWallet s
    putCheckpoint db key cp
    void $ runExceptT (putTxHistory db key txs)
    cp' <- readCheckpoint db key
    cp' `shouldBe` Just cp

dbPutCheckpointNoEffectOnHistory
    :: DBLayer IO DummyState
    -> (PrimaryKey WalletId, Map (Hash "Tx") (Tx, TxMeta))
    -> Property
dbPutCheckpointNoEffectOnHistory db (key, txs) = monadicIO $ liftIO $ do
    let cp0 = initWallet (DummyState 0)
    let cp1 = initWallet (DummyState 14)
    putCheckpoint db key cp0
    void $ runExceptT (putTxHistory db key txs)
    putCheckpoint db key cp1
    txs' <- readTxHistory db key
    txs' `shouldBe` txs

{-------------------------------------------------------------------------------
                      Tests machinery, Arbitrary instances
-------------------------------------------------------------------------------}

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
    -- No shrinking
    arbitrary = DummyState <$> arbitrary

deriving instance NFData DummyState

instance IsOurs DummyState where
    isOurs _ num = (True, num)

instance Semigroup DummyState where
    (DummyState num1) <> (DummyState num2)
        = DummyState (num1 + num2)

deriving instance Show (PrimaryKey WalletId)

instance Arbitrary (PrimaryKey WalletId) where
    shrink _ = []
    arbitrary = do
        k <- choose (0, 10)
        return $ PrimaryKey $ WalletId $ UUID.fromWords k 0 0 0

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
