{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Copyright: © 2022–2023 IOHK
-- License: Apache-2.0
--
-- Delegations-history store and migration from old db tables.
module Cardano.Wallet.DB.Store.Delegations.Store
    ( mkStoreDelegations
    , encodeStatus
    )
    where

import Prelude

import Cardano.Pool.Types
    ( PoolId )
import Cardano.Slotting.Slot
    ( SlotNo )
import Cardano.Wallet.DB.Sqlite.Types
    ( DelegationStatusEnum (..) )
import Cardano.Wallet.DB.Store.Delegations.Schema
    ( Delegations (..)
    , EntityField (DelegationSlot)
    , Key (DelegationsKey)
    , resetDelegationTable
    )
import Cardano.Wallet.Delegation.Model
    ( History, Operation (..), Status (..), slotOf )
import Control.Exception
    ( Exception, SomeException (SomeException) )
import Control.Monad
    ( when )
import Control.Monad.Class.MonadThrow
    ( throwIO )
import Data.Delta
    ( Delta (apply) )
import Data.Store
    ( UpdateStore, mkUpdateStore, updateLoad )
import Database.Persist
    ( Entity (..)
    , PersistQueryWrite (deleteWhere)
    , repsert
    , selectList
    , (==.)
    , (>.)
    )
import Database.Persist.Sql
    ( SqlPersistT, insertMany_ )

import qualified Data.Map.Strict as Map

mkStoreDelegations :: UpdateStore (SqlPersistT IO) (Operation SlotNo PoolId)
mkStoreDelegations = mkUpdateStore loadS' writeS' updateS'

loadS'
    :: SqlPersistT IO (Either SomeException (History SlotNo PoolId))
loadS' = do
    xs <- selectList [] []
    pure $ Map.fromList <$> sequence [decodeStatus x | Entity _ x <- xs]

data DecodeDelegationError
    = ActiveDelegationWithoutAPool
    | UnknownDelegationStatus Int
    deriving (Show, Eq, Exception)

decodeStatus :: Delegations -> (Either SomeException (SlotNo, Status PoolId))
decodeStatus (Delegations sn n m_pi) = case n of
    InactiveE -> Right (sn, Inactive)
    RegisteredE -> Right (sn, Registered)
    ActiveE -> case m_pi of
        Nothing -> Left $ SomeException ActiveDelegationWithoutAPool
        Just pi' -> Right (sn, Active pi')

updateS'
    :: Maybe (History SlotNo PoolId)
    -> Operation SlotNo PoolId
    -> SqlPersistT IO ()
updateS' = updateLoad loadS' throwIO $ \h op -> do
    let slot = slotOf op
        h' = apply op h
        v' = Map.lookup slot h'
        v = Map.lookup slot h
    deleteWhere [DelegationSlot >. slotOf op]
    when (v /= v')
        $ case v' of
            Nothing ->
                deleteWhere
                    [DelegationSlot ==. slotOf op]
            Just w ->
                repsert (DelegationsKey slot)
                    $ encodeStatus slot w

encodeStatus :: SlotNo -> Status PoolId -> Delegations
encodeStatus slot = \case
    Inactive -> Delegations slot InactiveE Nothing
    Registered -> Delegations slot RegisteredE Nothing
    Active pi' -> Delegations slot ActiveE (Just pi')

writeS' :: History SlotNo PoolId -> SqlPersistT IO ()
writeS' h = do
    resetDelegationTable
    insertMany_ [encodeStatus slot x | (slot, x) <- Map.assocs h]
