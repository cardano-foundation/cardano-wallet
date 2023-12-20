{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright: © 2018-2023 IOHK
-- License: Apache-2.0
--
-- Auto-generated Sqlite & Persistent machinery via Template-Haskell. This has
-- been moved into a separate file so that we can treat it slightly differently
-- when computing code-coverage.
--
-- More than 6K lines end-up being generated from the instructions below! As a
-- result, we're going to ignore code-coverage on the following module and, no
-- hand-written functions should be written in this module!

module Cardano.Wallet.DB.Store.Delegations.Schema
    ( Delegations (..)
    , EntityField (DelegationSlot)
    , Key (DelegationsKey)
    , resetDelegationTable
    )
where

import Prelude

import Cardano.Pool.Types
    ( PoolId
    )
import Cardano.Slotting.Slot
    ( SlotNo
    )
import Cardano.Wallet.DB.Sqlite.Types
    ( DelegationStatusEnum (..)
    , sqlSettings'
    )
import Cardano.Wallet.Delegation.Model
    ( VoteAction
    )
import Control.Monad
    ( void
    )
import Control.Monad.IO.Class
    ( MonadIO
    )
import Data.Proxy
    ( Proxy (..)
    )
import Database.Persist
    ( PersistEntity (EntityField, Key, entityDef)
    )
import Database.Persist.Sql
    ( Migration
    , SqlPersistT
    , rawExecute
    , runMigrationUnsafeQuiet
    )
import Database.Persist.TH
    ( migrateModels
    , mkPersist
    , persistLowerCase
    )
import GHC.Generics
    ( Generic (..)
    )

import qualified Cardano.Wallet.Primitive.Types as W

mkPersist sqlSettings'
    [persistLowerCase|
        Delegations                                     sql=delegations
            delegationSlot      SlotNo                  sql=slot
            delegationStatus    DelegationStatusEnum    sql=status
            delegationPool      PoolId Maybe            sql=pool
            delegationVote      VoteAction Maybe        sql=vote

            Primary delegationSlot
            deriving Show Generic Eq

    |]

delegationMigration :: Migration
delegationMigration = migrateModels [entityDef (Proxy :: Proxy Delegations)]

migrateDelegations :: MonadIO m => SqlPersistT m ()
migrateDelegations = void $ runMigrationUnsafeQuiet delegationMigration

dropDelegationTable :: MonadIO m => SqlPersistT m ()
dropDelegationTable = rawExecute "DROP TABLE IF EXISTS \"delegations\";" []

resetDelegationTable :: MonadIO m => SqlPersistT m ()
resetDelegationTable = do
    dropDelegationTable
    migrateDelegations
