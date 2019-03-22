{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.DBLayer.MVarSpec
    ( spec
    ) where

import Prelude

import Cardano.DBLayer
    ( DBLayer (..), PrimaryKey (..) )
import Cardano.DBLayer.MVar
    ( newDBLayer )
import Cardano.Wallet
    ( Wallet, WalletId (..), initWallet )
import Cardano.Wallet.Primitive
    ( IsOurs (..) )
import Control.Concurrent.Async
    ( mapConcurrently_ )
import Control.DeepSeq
    ( NFData )
import Control.Monad.IO.Class
    ( liftIO )
import Data.List.NonEmpty
    ( NonEmpty ((:|)) )
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
        it "readCheckpoints . putCheckpoints yields inserted checkpoints" $
            \db -> (property $ dbReadCheckpointsProp db)
        it "replacement of values returns last value that was put" $
            \db -> (property $ dbReplaceValsProp db)
        it "multiple sequential putCheckpoints work properly" $
            \db -> (property $ dbMultiplePutsSeqProp db)
        it "multiple parallel putCheckpoints work properly" $
            \db -> (property $ dbMultiplePutsParProp db)

{-------------------------------------------------------------------------------
                                    Properties
-------------------------------------------------------------------------------}


dbReadCheckpointsProp
    :: DBLayer IO DummyState
    -> (PrimaryKey WalletId, DummyState)
    -> Property
dbReadCheckpointsProp db (key, val)  = monadicIO $ liftIO $ do
    putCheckpoints db key (toWalletState val)
    resFromDb <- readCheckpoints db key

    resFromDb `shouldBe` (Just $ toWalletState val)

dbReplaceValsProp
    :: DBLayer IO DummyState
    -> (PrimaryKey WalletId, DummyState, DummyState)
    -> Property
dbReplaceValsProp db (key, val1, val2)  = monadicIO $ liftIO $ do
    putCheckpoints db key (toWalletState val1)
    putCheckpoints db key (toWalletState val2)
    resFromDb <- readCheckpoints db key

    resFromDb `shouldBe` (Just $ toWalletState val2)

dbMultiplePutsSeqProp
    :: DBLayer IO DummyState
    -> KeyValPairs
    -> Property
dbMultiplePutsSeqProp db (KeyValPairs keyValPairs) = monadicIO $ liftIO $ do
    mapM_ (\(key, val) -> putCheckpoints db key (toWalletState val)) keyValPairs
    resFromDb <- Set.fromList <$> readWallets db

    resFromDb `shouldBe` (Set.fromList (map fst keyValPairs))

dbMultiplePutsParProp
    :: DBLayer IO DummyState
    -> KeyValPairs
    -> Property
dbMultiplePutsParProp db (KeyValPairs keyValPairs) = monadicIO $ liftIO $ do
    mapConcurrently_ (\(key, val) -> putCheckpoints db key (toWalletState val)) keyValPairs
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

toWalletState
    :: (IsOurs s, Semigroup s, NFData s, Show s) => s
    -> NonEmpty (Wallet s)
toWalletState val = initWallet val :| []
