{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- |
-- Copyright: © 2023 IOHK
-- License: Apache-2.0
--
-- Small library for database migrations.
-- Features: Typed versions, version checks, and backups.
module Cardano.Wallet.DB.Migration
    ( -- * Build migrations
      Migration
    , VersionT
    , mkMigration
    , hoistMigration

      -- * Run migrations
    , Version (..)
    , getTargetVersion
    , MigrationInterface (..)
    , runMigrations
    , ErrWrongVersion (..)
    ) where

import Prelude hiding
    ( (.)
    )

import Control.Category
    ( Category (..)
    )
import Control.Exception
    ( Exception
    )
import Control.Monad
    ( forM_
    )
import Control.Monad.Class.MonadThrow
    ( MonadThrow (throwIO)
    )
import Control.Monad.Reader
    ( ReaderT (runReaderT)
    )
import Data.Proxy
    ( Proxy (..)
    )
import Fmt
    ( Buildable (..)
    )
import GHC.Natural
    ( Natural
    )
import GHC.TypeNats
    ( KnownNat
    , Nat
    , natVal
    , type (+)
    )

--------------------------------------------------------------------------------
-------  public ----------------------------------------------------------------
--------------------------------------------------------------------------------

-- | A version number at type level.
type VersionT = Nat

-- | A version number at value level.
newtype Version = Version Natural
    deriving newtype (Show, Eq, Enum, Num, Ord)

instance Buildable Version where
    build = build . show

-- | A migration path between two database versions.
--
-- This path contains migration steps between any two consecutive
-- versions in the range @from@ @to@.
newtype Migration m (from :: VersionT) (to :: VersionT) = Migration [m ()]

hoistMigration
    :: (forall x. m x -> n x)
    -> Migration m from to
    -> Migration n from to
hoistMigration f (Migration steps) = Migration (fmap f steps)

-- | A migration path between two consecutive versions.
mkMigration :: forall v m. m () -> Migration m v (v + 1)
mkMigration m = Migration [m]

instance Category (Migration m) where
    id = Migration []
    Migration s2 . Migration s1 = Migration (s1 <> s2)

-- | Target version of a 'Migration', on the value level.
getTargetVersion
    :: forall m vmin vtarget
     . KnownNat vtarget
    => Migration m vmin vtarget
    -> Version
getTargetVersion _ = Version $ natVal (Proxy :: Proxy vtarget)

-- | Functions to interact with the database.
data MigrationInterface m handle = MigrationInterface
    { backupDatabaseFile :: FilePath -> Version -> m ()
        -- ^ Back up the (closed) database file.
        -- The 'Version' argument can be used to find a new file path
        -- for the backup copy.
    , withDatabaseFile :: forall r. FilePath -> (handle -> m r) -> m r
        -- ^ Open a database file and close it after use or on exception.
    , getVersion :: handle -> m Version
        -- ^ Get the current version.
    , setVersion :: handle -> Version -> m ()
        -- ^ Set the current version.
    }

-- | Migrate the given database file.
--
-- Throw 'ErrWrongVersion' if the version of the database file
-- is too old or too new for the given migration path.
runMigrations
    :: forall m vmin vtarget handle
     . (MonadThrow m, KnownNat vmin, KnownNat vtarget)
    => MigrationInterface m handle
        -- ^ Functions to interact with the database.
    -> FilePath
        -- ^ File path of database to run migrations on.
    -> Migration (ReaderT handle m) vmin vtarget
        -- ^ Migration path to run.
        --
        -- @vmin@ is the minimum version that a database can be
        -- migrated from.
        --
        -- @vtarget@ is the version to which the database is migrated.
    -> m ()
runMigrations interface filepath (Migration steps) = do
    let nfrom = Version $ natVal (Proxy :: Proxy vmin)
        nto = Version $ natVal (Proxy :: Proxy vtarget)
    forM_ (zip [nfrom .. nto] steps)
        $ uncurry (runMigrationStep interface filepath)

-- | Error if the database version is not the expected one.
data ErrWrongVersion = ErrWrongVersion
    { expectedVersion :: Version
    , actualVersion :: Version
    }
    deriving (Show, Eq, Exception)

instance Buildable ErrWrongVersion where
    build (ErrWrongVersion expected actual) = "Expected database version "
        <> build expected
        <> ", but found "
        <> build actual
        <> "."
--------------------------------------------------------------------------------
-------  internal --------------------------------------------------------------
--------------------------------------------------------------------------------

runMigrationStep
    :: MonadThrow m
    => MigrationInterface m handle
    -> FilePath
    -> Version
    -> ReaderT handle m ()
    -> m ()
runMigrationStep MigrationInterface{..} filepath migrateVersion step = do
    actualVersion <- withDatabaseFile filepath getVersion
    case actualVersion `compare` migrateVersion of
        LT -> throwIO $ ErrWrongVersion migrateVersion actualVersion
        GT -> skip
        EQ -> do
            backupDatabaseFile filepath actualVersion
            withDatabaseFile filepath $ \handle -> do
                runReaderT step handle
                setVersion handle $ actualVersion + 1
  where
    skip = pure ()
