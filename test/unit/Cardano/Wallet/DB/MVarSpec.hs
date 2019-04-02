{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.DB.MVarSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.DB
    ( DBLayer (..), PrimaryKey (..) )
import Cardano.Wallet.DB.MVar
    ( newDBLayer )
import Cardano.Wallet.Primitive.Model
    ( initWallet )
import Cardano.Wallet.Primitive.Types
    ( IsOurs (..), WalletId (..) )
import Control.Concurrent.Async
    ( mapConcurrently_ )
import Control.DeepSeq
    ( NFData )
import Control.Monad.IO.Class
    ( liftIO )
import Test.Hspec
    ( Spec, before, describe, it, shouldBe )
import Test.QuickCheck
    ( Arbitrary (..), Property, choose, property, vectorOf )
import Test.QuickCheck.Gen
    ( chooseAny )
import Test.QuickCheck.Monadic
    ( monadicIO )

import qualified Data.Set as Set


spec :: Spec
spec = do
    describe "DB works as expected" $ before newDBLayer $ do
        it "readCheckpoint . putCheckpoint yields inserted checkpoints" $
            \db -> (property $ dbReadCheckpointProp db)
        it "replacement of values returns last value that was put" $
            \db -> (property $ dbReplaceValsProp db)
        it "multiple sequential putCheckpoint work properly" $
            \db -> (property $ dbMultiplePutsSeqProp db)
        it "multiple parallel putCheckpoint work properly" $
            \db -> (property $ dbMultiplePutsParProp db)

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
    -> KeyValPairs
    -> Property
dbMultiplePutsSeqProp db (KeyValPairs keyValPairs) = monadicIO $ liftIO $ do
    mapM_ (\(key, val) -> putCheckpoint db key (initWallet val)) keyValPairs
    resFromDb <- Set.fromList <$> readWallets db

    resFromDb `shouldBe` (Set.fromList (map fst keyValPairs))

dbMultiplePutsParProp
    :: DBLayer IO DummyState
    -> KeyValPairs
    -> Property
dbMultiplePutsParProp db (KeyValPairs keyValPairs) = monadicIO $ liftIO $ do
    mapConcurrently_
        (\(key, val) -> putCheckpoint db key (initWallet val))
        keyValPairs
    resFromDb <- Set.fromList <$> readWallets db

    resFromDb `shouldBe` (Set.fromList (map fst keyValPairs))

{-------------------------------------------------------------------------------
                      Tests machinery, Arbitrary instances
-------------------------------------------------------------------------------}


newtype KeyValPairs = KeyValPairs [(PrimaryKey WalletId, DummyState)]
    deriving (Show, Eq)

instance Arbitrary KeyValPairs where
    -- No shrinking
    arbitrary = do
        pairs <- choose (10, 50) >>= flip vectorOf arbitrary
        KeyValPairs <$> pure pairs

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
    -- No shrinking
    arbitrary = PrimaryKey . WalletId <$> chooseAny
