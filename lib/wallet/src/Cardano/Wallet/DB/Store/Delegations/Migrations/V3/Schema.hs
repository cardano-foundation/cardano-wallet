{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Wallet.DB.Store.Delegations.Migrations.V3.Schema
    ( Delegations (..)
    , EntityField (DelegationSlot)
    , Key (DelegationsKey)
    , resetDelegationTable
    , DelegationStatusEnum (..)
    )
where

import Prelude

import Cardano.Pool.Types
    ( PoolId
    )
import Cardano.Wallet.Primitive.Slotting
    ( SlotNo (..)
    )
import Control.Monad
    ( void
    , (>=>)
    )
import Control.Monad.IO.Class
    ( MonadIO
    )
import Data.Proxy
    ( Proxy (..)
    )
import Data.Text
    ( Text
    )
import Data.Text.Class
    ( ToText (..)
    )
import Data.Word
    ( Word64
    )
import Database.Persist
    ( PersistEntity (EntityField, Key, entityDef)
    , PersistField (..)
    , PersistValue
    )
import Database.Persist.PersistValue.Extended
    ( fromPersistValueFromText
    )
import Database.Persist.Sql
    ( Migration
    , PersistFieldSql (..)
    , SqlPersistT
    , rawExecute
    , runMigrationUnsafeQuiet
    )
import Database.Persist.TH
    ( MkPersistSettings (mpsPrefixFields)
    , migrateModels
    , mkPersist
    , persistLowerCase
    , sqlSettings
    )
import GHC.Generics
    ( Generic (..)
    )
import Web.HttpApiData
    ( FromHttpApiData (..)
    , ToHttpApiData (..)
    )
import Web.PathPieces
    ( PathPiece (..)
    )

instance PersistField PoolId where
    toPersistValue :: PoolId -> PersistValue
    toPersistValue = toPersistValue . toText
    fromPersistValue = fromPersistValueFromText

instance PersistFieldSql PoolId where
    sqlType _ = sqlType (Proxy @Text)

persistSlotNo :: SlotNo -> PersistValue
persistSlotNo = toPersistValue . unSlotNo

unPersistSlotNo :: PersistValue -> Either Text SlotNo
unPersistSlotNo = fmap SlotNo . fromPersistValue

instance PersistField SlotNo where
    toPersistValue = persistSlotNo
    fromPersistValue = unPersistSlotNo

instance PersistFieldSql SlotNo where
    sqlType _ = sqlType (Proxy @Word64)

instance Read SlotNo where
    readsPrec _ = error "readsPrec stub needed for persistent"

instance ToHttpApiData SlotNo where
    toUrlPiece = error "toUrlPiece stub needed for persistent"
instance FromHttpApiData SlotNo where
    parseUrlPiece = error "parseUrlPiece stub needed for persistent"
instance PathPiece SlotNo where
    toPathPiece = error "toPathPiece stub needed for persistent"
    fromPathPiece = error "fromPathPiece stub needed for persistent"

mkPersist (sqlSettings { mpsPrefixFields = False })
    [persistLowerCase|
        Delegations                                     sql=delegations
            delegationSlot      SlotNo                  sql=slot
            delegationStatus    DelegationStatusEnum    sql=status
            delegationPool      PoolId Maybe            sql=pool

            Primary delegationSlot
            deriving Show Generic Eq

    |]

data DelegationStatusEnum = InactiveE | RegisteredE | ActiveE
    deriving (Eq, Show, Enum, Generic)

instance PersistField DelegationStatusEnum where
    toPersistValue = toPersistValue . \case
        InactiveE -> "inactive" :: Text
        RegisteredE -> "registered"
        ActiveE -> "active"
    fromPersistValue = fromPersistValue >=> readDelegationStatus

readDelegationStatus :: Text -> Either Text DelegationStatusEnum
readDelegationStatus "inactive" = Right InactiveE
readDelegationStatus "registered" = Right RegisteredE
readDelegationStatus "active" = Right ActiveE
readDelegationStatus other = Left $ "Invalid delegation status: " <> other

instance PersistFieldSql DelegationStatusEnum where
    sqlType _ = sqlType (Proxy @Text)

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
