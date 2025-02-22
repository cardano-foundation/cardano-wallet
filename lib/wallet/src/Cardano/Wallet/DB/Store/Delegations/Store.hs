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
    ( PoolId
    )
import Cardano.Slotting.Slot
    ( SlotNo
    )
import Cardano.Wallet.DB.Store.Delegations.Schema
    ( Delegations (..)
    , EntityField (DelegationSlot)
    , Key (DelegationsKey)
    , resetDelegationTable
    )
import Cardano.Wallet.DB.Store.Delegations.Types
    ( DelegationStatusEnum (..)
    )
import Cardano.Wallet.Delegation.Model
    ( History
    , Operation (..)
    , Status (..)
    , slotOf
    )
import Cardano.Wallet.Primitive.Types.DRep
    ( DRep
    )
import Control.Exception
    ( Exception
    , SomeException (SomeException)
    )
import Control.Monad
    ( when
    )
import Control.Monad.Class.MonadThrow
    ( throwIO
    )
import Data.Delta
    ( Delta (apply)
    )
import Data.Store
    ( UpdateStore
    , mkUpdateStore
    , updateLoad
    )
import Database.Persist
    ( Entity (..)
    , PersistQueryWrite (deleteWhere)
    , repsert
    , selectList
    , (==.)
    , (>.)
    )
import Database.Persist.Sql
    ( SqlPersistT
    , insertMany_
    )

import qualified Data.Map.Strict as Map

mkStoreDelegations :: UpdateStore (SqlPersistT IO) (Operation SlotNo DRep PoolId)
mkStoreDelegations = mkUpdateStore loadS' writeS' updateS'

loadS'
    :: SqlPersistT IO (Either SomeException (History SlotNo DRep PoolId))
loadS' = do
    xs <- selectList [] []
    pure $ Map.fromList <$> sequence [decodeStatus x | Entity _ x <- xs]

data DecodeDelegationError
    = ActiveDelegationWithoutAPool
    | UnknownDelegationStatus Int
    | VotingWithoutADRep
    | ActiveVotingWithoutADRep
    | ActiveVotingWithoutAPool
    | ActiveVotingWithoutPoolAndDRep
    deriving (Show, Eq, Exception)

decodeStatus :: Delegations -> (Either SomeException (SlotNo, Status DRep PoolId))
decodeStatus (Delegations sn n m_pi m_v) = case n of
    InactiveE -> Right (sn, Inactive)
    RegisteredE -> Right (sn, Active Nothing Nothing)
    ActiveE -> case m_pi of
        Nothing -> Left $ SomeException ActiveDelegationWithoutAPool
        Just pi' -> Right (sn, Active Nothing (Just pi'))
    VotedE -> case m_v of
        Nothing -> Left $ SomeException VotingWithoutADRep
        Just v' -> Right (sn, Active (Just v') Nothing)
    ActiveAndVotedE -> case (m_pi, m_v) of
        (Just _, Nothing) -> Left $ SomeException ActiveVotingWithoutADRep
        (Nothing, Just _) -> Left $ SomeException ActiveVotingWithoutAPool
        (Nothing, Nothing) -> Left $ SomeException ActiveVotingWithoutPoolAndDRep
        (Just pi', Just v') -> Right (sn, Active (Just v') (Just pi'))

updateS'
    :: Maybe (History SlotNo DRep PoolId)
    -> Operation SlotNo DRep PoolId
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

encodeStatus :: SlotNo -> Status DRep PoolId -> Delegations
encodeStatus slot = \case
    Inactive -> Delegations slot InactiveE Nothing Nothing
    Active Nothing Nothing -> Delegations slot RegisteredE Nothing Nothing
    Active Nothing (Just pi') -> Delegations slot ActiveE (Just pi') Nothing
    Active (Just v') Nothing -> Delegations slot VotedE Nothing (Just v')
    Active v' pi' -> Delegations slot ActiveAndVotedE pi' v'

writeS' :: History SlotNo DRep PoolId -> SqlPersistT IO ()
writeS' h = do
    resetDelegationTable
    insertMany_ [encodeStatus slot x | (slot, x) <- Map.assocs h]
