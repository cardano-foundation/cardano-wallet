{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2022–2023 IOHK
-- License: Apache-2.0
--
-- Delegations-history store and migration from old db tables.
module Cardano.Wallet.DB.Store.Delegations
    ( mkStoreDelegations
    , migration
    )
    where

import Prelude

import Cardano.Pool.Types
    ( PoolId )
import Cardano.Slotting.Slot
    ( SlotNo )
import Cardano.Wallet.DB.Sqlite.Schema
    ( DelegationCertificate (DelegationCertificate)
    , Delegations (..)
    , EntityField (..)
    , Key (DelegationsKey)
    , StakeKeyCertificate (StakeKeyCertificate)
    )
import Cardano.Wallet.DB.Sqlite.Types
    ( DelegationStatusEnum (..) )
import Cardano.Wallet.Delegation.Model
    ( History, Operation (..), Status (..), slotOf )
import Cardano.Wallet.Primitive.Types
    ( WalletId )
import Control.Exception
    ( Exception, SomeException (SomeException) )
import Control.Monad
    ( when )
import Control.Monad.Class.MonadThrow
    ( throwIO )
import Data.Delta
    ( Delta (Base, apply) )
import Data.Map.Strict
    ( Map )
import Data.Store
    ( UpdateStore, mkUpdateStore, updateLoad )
import Data.These
    ( These (..) )
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

import qualified Cardano.Wallet.Primitive.Types as W
import qualified Data.Map.Merge.Strict as Map
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
    deleteWhere @_ @_ @Delegations []
    insertMany_ [encodeStatus slot x | (slot, x) <- Map.assocs h]

readOldEncoding
    :: WalletId
    -> SqlPersistT IO (Base (Operation SlotNo PoolId))
readOldEncoding wid = do
    skcs <- selectList [StakeKeyCertWalletId ==. wid] []
    dcs <- selectList [CertWalletId ==. wid] []
    pure $ Map.foldlWithKey' applyChange mempty (slotMap skcs dcs)
  where
    applyChange
        :: History SlotNo PoolId
        -> SlotNo
        -> These W.StakeKeyCertificate (Maybe PoolId)
        -> History SlotNo PoolId
    applyChange h s = \case
        This W.StakeKeyDeregistration -> apply (Deregister s) h
        This W.StakeKeyRegistration -> apply (Register s) h
        That Nothing -> h
        That (Just p) -> apply (Delegate p s) h
        These W.StakeKeyDeregistration _ -> apply (Deregister s) h
        These W.StakeKeyRegistration (Just p) ->
            apply (Delegate p s) $ apply (Register s) h
        These W.StakeKeyRegistration Nothing ->
            apply (Register s) h

    slotMap
        :: [Entity StakeKeyCertificate]
        -> [Entity DelegationCertificate]
        -> Map SlotNo (These W.StakeKeyCertificate (Maybe PoolId))
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

migration :: WalletId -> SqlPersistT IO ()
migration wid = do
    old <- readOldEncoding wid
    writeS' old
    deleteWhere [StakeKeyCertWalletId ==. wid]
    deleteWhere [CertWalletId ==. wid]
