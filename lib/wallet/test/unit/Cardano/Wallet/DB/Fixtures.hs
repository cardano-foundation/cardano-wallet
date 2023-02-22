{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.DB.Fixtures
    ( withDBInMemory
    , initializeWallet
    , assertWith
    , RunQuery
    , unsafeLoadS
    , unsafeUpdateS
    , logScale
    , logScale'
    , coverM
    , frequencySuchThat
    , withInitializedWalletProp
    , WalletProperty
    , withStoreProp
    , StoreProperty
    , elementsOrArbitrary
    ) where

import Prelude

import Cardano.DB.Sqlite
    ( ForeignKeysSetting, SqliteContext, newInMemorySqliteContext, runQuery )
import Cardano.Wallet.DB.Sqlite.Schema
    ( Wallet (..), migrateAll )
import Cardano.Wallet.DB.Sqlite.Types
    ( BlockId (..) )
import Cardano.Wallet.Primitive.Types
    ( WalletId (..) )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Cardano.Wallet.Unsafe
    ( unsafeFromHex )
import Control.Tracer
    ( nullTracer )
import Data.Bifunctor
    ( second )
import Data.DBVar
    ( Store (loadS, updateS) )
import Data.Delta
    ( Delta (Base) )
import Data.Either
    ( fromRight )
import Data.Time.Clock
    ( UTCTime )
import Data.Time.Clock.POSIX
    ( posixSecondsToUTCTime )
import Database.Persist.Sql
    ( deleteWhere, insert_ )
import Database.Persist.Sqlite
    ( SqlPersistT, (==.) )
import Test.QuickCheck
    ( Arbitrary
    , Gen
    , Property
    , Testable
    , arbitrary
    , counterexample
    , cover
    , elements
    , frequency
    , scale
    , suchThat
    )
import Test.QuickCheck.Monadic
    ( PropertyM, assert, monadicIO, monitor, run )
import UnliftIO.Exception
    ( bracket )

import qualified Cardano.Wallet.DB.Sqlite.Schema as TH

{-------------------------------------------------------------------------------
    DB setup
-------------------------------------------------------------------------------}
withDBInMemory :: ForeignKeysSetting -> (SqliteContext -> IO a) -> IO a
withDBInMemory disableFK action = bracket (newDBInMemory disableFK) fst (action . snd)

newDBInMemory :: ForeignKeysSetting -> IO (IO (), SqliteContext)
newDBInMemory = newInMemorySqliteContext nullTracer [] migrateAll

initializeWallet :: WalletId -> SqlPersistT IO ()
initializeWallet wid = do
    deleteWhere [TH.WalId ==. wid] -- triggers delete cascade
    insertWalletTable wid

-- | Insert a wallet table in order to satisfy  FOREIGN PRIMARY constraints
insertWalletTable :: WalletId -> SqlPersistT IO ()
insertWalletTable wid = insert_ $ Wallet
    { walId = wid
    , walName = "Stores"
    , walCreationTime = dummyUTCTime
    , walPassphraseLastUpdatedAt = Nothing
    , walPassphraseScheme = Nothing
    , walGenesisHash = BlockId dummyHash
    , walGenesisStart = dummyUTCTime
    }

{-------------------------------------------------------------------------------
    Arbitrary
-------------------------------------------------------------------------------}
dummyUTCTime :: UTCTime
dummyUTCTime = posixSecondsToUTCTime 1506203091

dummyHash :: Hash "BlockHeader"
dummyHash = Hash $ unsafeFromHex
    "5f20df933584822601f9e3f8c024eb5eb252fe8cefb24d1317dc3d432e940ebb"

{-------------------------------------------------------------------------------
    QuickCheck utilities
-------------------------------------------------------------------------------}
-- | Like 'assert', but allow giving a label / title before running a assertion
assertWith :: String -> Bool -> PropertyM IO ()
assertWith lbl condition = do
    let flag = if condition then "✓" else "✗"
    monitor (counterexample $ lbl <> " " <> flag)
    assert condition

-- | Adjust the QuickCheck @size@ parameter,
-- by replacing it with its logarithm with respect to some given basis.
logScale' :: Double -> Gen a -> Gen a
logScale' b = scale (floor @Double . logBase b . fromIntegral . succ)

-- | Adjust the QuickCheck @size@ parameter,
-- by replacing it with its logarithm.
logScale :: Gen a -> Gen a
logScale = logScale' $ exp 1

-- | 'cover', lifted with 'fmap'.
coverM :: (Functor f, Testable prop) => Double -> Bool -> String -> f prop -> f Property
coverM n c t = fmap $ cover n c t

-- | Like 'frequency' but use only one generator with different filters.
frequencySuchThat :: Gen a -> [(Int, a -> Bool)] -> Gen a
frequencySuchThat g fs = frequency $ second (suchThat g) <$> fs

-- | Pick an element from a list (or a default if the list is empty)
elementsOrArbitrary :: Arbitrary a => (a -> b) -> [b] -> Gen b
elementsOrArbitrary f [] = f <$> arbitrary
elementsOrArbitrary _ xs = elements xs

-- | A way to embed any 'SqlPersistT IO' action into 'PropertyM IO'.
-- (This type synonym is useful for avoiding errors about impredicative types.)
type RunQuery =
    forall a. SqlPersistT IO a -> PropertyM IO a

-- | Minimalistic type signature for a 'Store' property.
type StoreProperty = SqliteContext -> Property

-- | Initialize a wallet, and pass control to the rest of the property.
withStoreProp
    :: Testable a
    => (RunQuery -> PropertyM IO a)
    -> StoreProperty
withStoreProp prop db = monadicIO $ do
    prop $ run . runQuery db

-- | Minimalistic type signature for a property specific to a 'WalletId'.
type WalletProperty = SqliteContext -> WalletId -> Property

-- | Initialize a wallet, and pass control to the rest of the property.
withInitializedWalletProp
    :: Testable a
    => (WalletId -> RunQuery -> PropertyM IO a)
    -> WalletProperty
withInitializedWalletProp prop db wid = monadicIO $ do
    let runQ = run .runQuery db
    runQ $ initializeWallet wid
    prop wid runQ

-- store unsafe ops

-- | Bomb on failing 'loadS'.
unsafeLoadS :: Functor f => Store f da -> f (Base da)
unsafeLoadS s = fromRight (error "store law is broken") <$> loadS s

-- | A simpler interface for 'updateS' in tests, using 'unsafeLoadS'.
-- Natural for use with 'foldM'.
unsafeUpdateS :: Applicative m => Store m da -> Base da -> da -> m (Base da)
unsafeUpdateS store ba da = updateS store (Just ba) da *> unsafeLoadS store
