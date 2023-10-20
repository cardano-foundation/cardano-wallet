{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Copyright: © 2023 IOHK
-- License: Apache-2.0
--
-- Tests for database migration library.
module Cardano.Wallet.DB.MigrationSpec where

import Prelude hiding
    ( (.)
    )

import Cardano.Wallet.DB.Migration
    ( ErrWrongVersion (ErrWrongVersion)
    , Migration
    , MigrationInterface (..)
    , Version (..)
    , mkMigration
    , runMigrations
    )
import Control.Category
    ( (.)
    )
import Control.Monad.Class.MonadThrow
    ( MonadThrow (..)
    , SomeException
    , toException
    )
import Control.Monad.Trans.Class
    ( lift
    )
import Control.Monad.Trans.Except
    ( ExceptT (..)
    , runExceptT
    )
import Control.Monad.Trans.Reader
    ( ReaderT
    )
import Control.Monad.Trans.State
    ( State
    , get
    , modify
    , runState
    )
import GHC.Natural
    ( Natural
    )
import GHC.TypeNats
    ( KnownNat
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldReturn
    , shouldThrow
    )

data Database = Database
    { version :: Version
    , backups :: [Version]
    , state :: [Int]
    }
    deriving (Eq, Show)

{-----------------------------------------------------------------------------
    Tests
------------------------------------------------------------------------------}
spec :: Spec
spec = do
    describe "database migration library" $ do
        it "can do one migration"
            $ runTestMigrations 0 m1
            `shouldReturn` Database
                { version = 1
                , backups = [0]
                , state = [1]
                }

        it "can do 2 migrations"
            $ runTestMigrations 0 m12
            `shouldReturn` Database
                { version = 2
                , backups = [0, 1]
                , state = [1, 2]
                }

        it "can skip migrations already done"
            $ runTestMigrations 1 m12
            `shouldReturn` Database
                { version = 2
                , backups = [1]
                , state = [2]
                }

        it "will fail applying future migrations"
            $ let failure e = case e of
                    ErrWrongVersion 1 0 -> True
                    _ -> False
              in  runTestMigrations 0 m2
                    `shouldThrow` failure

m1 :: Migration (ReaderT Handle MonadDatabase) 0 1
m1 = mkMigration $ migrate [1]

m2 :: Migration (ReaderT Handle MonadDatabase) 1 2
m2 = mkMigration $ migrate [2]

m12 :: Migration (ReaderT Handle MonadDatabase) 0 2
m12 = m2 . m1

{-----------------------------------------------------------------------------
    Test harness
------------------------------------------------------------------------------}
newtype MonadDatabase a = MonadDatabase
    { unMonadDatabase :: ExceptT SomeException (State Database) a }
    deriving (Functor, Applicative, Monad)

instance MonadThrow MonadDatabase where
    throwIO = MonadDatabase . ExceptT . pure . Left . toException
    bracket = error "not defined"

liftState :: State Database a -> MonadDatabase a
liftState = MonadDatabase . lift

runMonadDatabase :: Database -> MonadDatabase a -> IO Database
runMonadDatabase db0 (MonadDatabase action) = do
    case runState (runExceptT action) db0 of
        (Left e, _) -> throwIO e
        (_, db1) -> pure db1

type Handle = ()

mkMigrationInterface :: MigrationInterface MonadDatabase Handle
mkMigrationInterface = MigrationInterface
    { backupDatabaseFile = \_ v ->
        liftState . modify $ \db -> db{ backups = backups db <> [v] }
    , withDatabaseFile =
        \_ action -> action ()
    , getVersion = \_ ->
        version <$> liftState get
    , setVersion = \_ v ->
        liftState . modify $ \db -> db{ version = v }
    }

migrate :: [Int] -> ReaderT Handle MonadDatabase ()
migrate xs = lift . liftState . modify $ \db -> db{ state = state db <> xs }

runTestMigrations
    :: (KnownNat from, KnownNat to)
    => Natural
    -> Migration (ReaderT Handle MonadDatabase) from to
    -> IO Database
runTestMigrations v0 =
    runMonadDatabase initialDatabase
    . runMigrations mkMigrationInterface "filepath"
  where
    initialDatabase = Database (Version v0) [] []
