{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.DB.Store.Delegations.Migrations.V3.Migration
    ( migrateDelegations
    ) where

import Prelude

import Cardano.DB.Sqlite
    ( ReadDBHandle
    , dbBackend
    , dbConn
    )
import Cardano.DB.Sqlite.Migration.Old
    ( DBField (..)
    )
import Cardano.Pool.Types
    ( PoolId (..)
    )
import Cardano.Wallet.DB.Migration
    ( Migration
    , mkMigration
    )
import Cardano.Wallet.DB.Sqlite.Migration.Old
    ( SqlColumnStatus (..)
    , isFieldPresent
    )
import Cardano.Wallet.DB.Store.Delegations.Migrations.V2.Schema
    ( DelegationCertificate (..)
    , EntityField (..)
    , StakeKeyCertificate (..)
    , WStakeKeyCertificate (..)
    )
import Cardano.Wallet.DB.Store.Delegations.Migrations.V3.Model
    ( History
    , Operation (Delegate, Deregister, Register)
    , Status (..)
    )
import Cardano.Wallet.DB.Store.Delegations.Migrations.V3.Schema
    ( DelegationStatusEnum (..)
    , Delegations (..)
    , resetDelegationTable
    )
import Cardano.Wallet.Primitive.Types
    ( SlotNo (..)
    )
import Control.Monad.Reader
    ( asks
    , liftIO
    , withReaderT
    )
import Data.Delta
    ( Delta (Base, apply)
    )
import Data.Map.Strict
    ( Map
    )
import Data.These
    ( These (..)
    )
import Database.Persist.Sql
    ( Entity (Entity)
    , SqlPersistT
    , insertMany_
    , rawExecute
    , selectList
    )

import qualified Data.Map as Map
import qualified Data.Map.Merge.Strict as Map

readOldEncoding
    :: SqlPersistT IO (Base (Operation SlotNo PoolId))
readOldEncoding = do
    skcs <- selectList [] []
    dcs <- selectList [] []
    pure $ Map.foldlWithKey' applyChange mempty (slotMap skcs dcs)
  where
    applyChange
        :: History SlotNo PoolId
        -> SlotNo
        -> These WStakeKeyCertificate (Maybe PoolId)
        -> History SlotNo PoolId
    applyChange h s = \case
        This StakeKeyDeregistration -> apply (Deregister s) h
        This StakeKeyRegistration -> apply (Register s) h
        That Nothing -> h
        That (Just p) -> apply (Delegate p s) h
        These StakeKeyDeregistration _ -> apply (Deregister s) h
        These StakeKeyRegistration (Just p) ->
            apply (Delegate p s) $ apply (Register s) h
        These StakeKeyRegistration Nothing ->
            apply (Register s) h

    slotMap
        :: [Entity StakeKeyCertificate]
        -> [Entity DelegationCertificate]
        -> Map SlotNo (These WStakeKeyCertificate (Maybe PoolId))
    slotMap skcs dcs =
        mapMergeThese
            ( Map.fromList
                [ (slot, type')
                | Entity _ (StakeKeyCertificate _ slot type') <- skcs
                ]
            )
            ( Map.fromList
                [ (slot, type')
                | Entity _ (DelegationCertificate _ slot type') <- dcs
                ]
            )

    mapMergeThese :: Ord k => Map k a -> Map k b -> Map k (These a b)
    mapMergeThese =
        Map.merge
            (Map.mapMaybeMissing $ \_ -> Just . This)
            (Map.mapMaybeMissing $ \_ -> Just . That)
            (Map.zipWithMaybeMatched $ \_ x y -> Just $ These x y)

migration
    :: ReadDBHandle IO ()
migration = do
    conn <- asks dbConn
    r <- liftIO $ isFieldPresent conn $ DBField StakeKeyCertSlot
    case r of
        TableMissing ->
            fail
                $ unwords
                    [ "Database migration from version 2 to version 3 failed:"
                    , "Expected TABLE stake_key_certificate"
                    , "to exist in database_schema_version 2"
                    ]
        ColumnMissing ->
            fail
                $ unwords
                    [ "Database migration from version 2 to version 3 failed:"
                    , "Expected COLUMN slot of TABLE stake_key_certificate"
                    , "to exist in database_schema_version 2"
                    ]
        ColumnPresent -> withReaderT dbBackend $ do
            old <- readOldEncoding
            write old
            rawExecute "DROP TABLE stake_key_certificate"  []
            rawExecute "DROP TABLE delegation_certificate"  []

migrateDelegations :: Migration (ReadDBHandle IO) 2 3
migrateDelegations = mkMigration migration

write :: History SlotNo PoolId -> SqlPersistT IO ()
write h = do
    resetDelegationTable
    insertMany_ [encodeStatus slot x | (slot, x) <- Map.assocs h]

encodeStatus :: SlotNo -> Status PoolId -> Delegations
encodeStatus slot = \case
    Inactive -> Delegations slot InactiveE Nothing
    Registered -> Delegations slot RegisteredE Nothing
    Active pi' -> Delegations slot ActiveE (Just pi')
