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
    )
    where

import Prelude

import Cardano.DB.Sqlite
    ( SqliteContext, newInMemorySqliteContext, runQuery )
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
    ( Gen
    , Property
    , Testable
    , counterexample
    , cover
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
withDBInMemory :: (SqliteContext -> IO a) -> IO a
withDBInMemory action = bracket newDBInMemory fst (action . snd)

newDBInMemory :: IO (IO (), SqliteContext)
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

-- | rescale logaritmically, any base
logScale' :: Double -> Gen a -> Gen a
logScale' b = scale (floor @Double . logBase b . fromIntegral . succ)

-- | rescale logaritmically
logScale :: Gen a -> Gen a
logScale = logScale' $ exp 1

-- | cover with use of ($)
coverM :: (Functor f, Testable prop) => Double -> Bool -> String -> f prop -> f Property
coverM n c t =fmap $ cover n c t

-- | like frequency but use only one generator with different filters
frequencySuchThat :: Gen a -> [(Int, a -> Bool)] -> Gen a
frequencySuchThat g fs = frequency $ second (suchThat g) <$> fs

-- wallet property writing support

-- | a wrapper just to avoid impredicative errors
type RunQuery =
    forall a. SqlPersistT IO a -> PropertyM IO a

-- | minimalistic signature for a store property definition
type StoreProperty = SqliteContext ->  Property

-- | initialize a wallet an pass control to the rest of the property
withStoreProp
    :: Testable a
    => (RunQuery -> PropertyM IO a)
    -> StoreProperty
withStoreProp prop db = monadicIO $ do
    prop $ run .runQuery db
-- | minimalistic signature for a wallet property definition
type WalletProperty = SqliteContext -> WalletId -> Property

-- | initialize a wallet an pass control to the rest of the property
withInitializedWalletProp
    :: Testable a
    => (WalletId -> RunQuery -> PropertyM IO a)
    -> WalletProperty
withInitializedWalletProp prop db wid = monadicIO $ do
    let runQ = run .runQuery db
    runQ $ initializeWallet wid
    prop wid runQ

-- store unsafe ops

-- | bomb on Left
unsafeLoadS :: Functor f => Store f da -> f (Base da)
unsafeLoadS s = fromRight (error "store law is broken") <$> loadS s

-- | a better interface for update in tests, using unsafeLoadS
-- natural to fold onto
unsafeUpdateS :: Applicative m => Store m da -> Base da -> da -> m (Base da)
unsafeUpdateS store da ba = updateS store da ba *> unsafeLoadS store
