{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- |
-- Copyright: © 2022–2023 IOHK
-- License: Apache-2.0
--
-- Rewards-history store and migration from old db tables.
module Cardano.Wallet.DB.Store.Rewards.Store
  ( mkStoreRewards
  )
where

import Cardano.Wallet.DB.Sqlite.Schema
  ( DelegationReward (..)
  )
import Cardano.Wallet.Primitive.Types qualified as W
import Cardano.Wallet.Primitive.Types.Coin qualified as W
import Control.Exception
  ( SomeException (SomeException)
  )
import Data.Delta
  ( Replace
  )
import Data.Store
  ( UpdateStore
  , mkSimpleStore
  )
import Database.Persist.Sql
  ( Entity (..)
  , Filter
  , SqlPersistT
  , deleteWhere
  , insert_
  , selectList
  )
import Prelude

mkStoreRewards :: W.WalletId -> UpdateStore (SqlPersistT IO) (Replace W.Coin)
mkStoreRewards wid = mkSimpleStore loadS' (writeS' wid)

writeS' :: W.WalletId -> W.Coin -> SqlPersistT IO ()
writeS' wid x = do
  deleteWhere ([] :: [Filter DelegationReward])
  insert_ $ DelegationReward wid $ W.unsafeToWord64 x

loadS' :: SqlPersistT IO (Either SomeException W.Coin)
loadS' = do
  xs <- selectList [] []
  case xs of
    [Entity _ (DelegationReward _ y)] -> pure $ Right $ W.fromWord64 y
    [] -> pure $ Right $ W.Coin 0
    _multiple ->
      pure
        $ Left
        $ SomeException
        $ userError "Multiple rewards found"
