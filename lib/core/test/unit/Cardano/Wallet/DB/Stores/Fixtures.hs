{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.DB.Stores.Fixtures
    ( withDBInMemory
    , initializeWallet
    , assertWith
    , runWalletProp
    , RunQuery (..)
    , unsafeLoadS
    , unsafeUpdateS
    , logScale)
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
    ( Gen, Property, Testable, counterexample, scale )
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

logScale :: Gen a -> Gen a
logScale = scale (floor @Double . log . fromIntegral . succ)

newtype RunQuery = RunQuery
    { _runQueryA :: forall a. SqlPersistT IO a -> IO a}

runWalletProp
    :: Testable a
    => (WalletId -> RunQuery -> PropertyM IO a)
    -> SqliteContext -> WalletId -> Property

runWalletProp prop db wid = monadicIO $ do
    run . runQuery db $ initializeWallet wid
    prop wid (RunQuery $ runQuery db)

unsafeLoadS :: Functor f => Store f da -> f (Base da)
unsafeLoadS s = fromRight (error "store law is broken") <$> loadS s

unsafeUpdateS :: Monad f => Store f da -> Base da -> da -> f (Base da)
unsafeUpdateS store da ba = updateS store da ba >> unsafeLoadS store
