{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.DB.Store.Delegations.Migration
    ( migrateDelegations
    ) where

import Prelude

import Cardano.DB.Sqlite
    ( DBField (..), ReadDBHandle, dbBackend, dbConn )
import Cardano.Pool.Types
    ( PoolId )
import Cardano.Wallet.DB.Migration
    ( Migration, mkMigration )
import Cardano.Wallet.DB.Sqlite.Migration.Old
    ( SqlColumnStatus (..), isFieldPresent )
import Cardano.Wallet.DB.Store.Delegations.Migration.Schema
    ( DelegationCertificate (..), EntityField (..), StakeKeyCertificate (..),
    WStakeKeyCertificate (..) )
import Cardano.Wallet.DB.Store.Delegations.Store
    ( mkStoreDelegations )
import Cardano.Wallet.Delegation.Model
    ( History, Operation (Delegate, Deregister, Register) )
import Cardano.Wallet.Primitive.Types
    ( SlotNo )
import Control.Monad.Reader
    ( asks, liftIO, withReaderT )
import Data.Delta
    ( Delta (Base, apply) )
import Data.Map.Strict
    ( Map )
import Data.Store
    ( Store (..), UpdateStore )
import Data.These
    ( These (..) )
import Database.Persist.Sql
    ( Entity (Entity), SqlPersistT, rawExecute, selectList )

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
    :: UpdateStore (SqlPersistT IO) (Operation SlotNo PoolId)
    -> ReadDBHandle IO ()
migration store = do
    conn <- asks dbConn
    r <- liftIO $ isFieldPresent conn $ DBField StakeKeyCertSlot
    case r of
        TableMissing -> return ()
        ColumnMissing -> return ()
        ColumnPresent -> withReaderT dbBackend $ do
            old <- readOldEncoding
            writeS store old
            rawExecute "DROP TABLE stake_key_certificate"  []
            rawExecute "DROP TABLE delegation_certificate"  []

migrateDelegations :: Migration (ReadDBHandle IO) 2 3
migrateDelegations = mkMigration $ migration mkStoreDelegations
