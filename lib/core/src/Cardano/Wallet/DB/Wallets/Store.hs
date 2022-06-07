{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Copyright: © 2022 IOHK
-- License: Apache-2.0
--
-- 'Store' implementations that can store various wallet types
-- in an SQLite database using `persistent`.
--
-- FIXME LATER during ADP-1043:
--
-- * Use 'Table' and 'Embedding' to construct the relevant 'Store'
--   rather than implementing 'loadS', 'writeS', 'updateS' in
--   a monadic fashion.
--   Hide the new implementation behind a feature flag,
--   i.e. "Cardano.Wallet.DB.Sqlite.StoresNew".

module Cardano.Wallet.DB.Wallets.Store
    ( mkStoreWallets
    , PersistAddressBook (..)
    , blockHeaderFromEntity
    -- * Testing
    , mkStoreWallet
    )
    where

import Prelude

import Cardano.Wallet.DB
    ( ErrBadFormat (ErrBadFormatAddressPrologue) )
import Cardano.Wallet.DB.Checkpoints.Store
    ( PersistAddressBook (..), blockHeaderFromEntity, mkStoreCheckpoints )
import Cardano.Wallet.DB.Sqlite.Schema
    ( EntityField (CheckpointWalletId), Wallet )
import Cardano.Wallet.DB.Wallets.State
    ( DeltaWalletState
    , DeltaWalletState1 (ReplacePrologue, UpdateCheckpoints)
    , WalletState (WalletState)
    )
import Control.Exception
    ( toException )
import Control.Monad
    ( forM )
import Control.Monad.Except
    ( ExceptT (ExceptT), runExceptT )
import Data.DBVar
    ( Store (Store, loadS, updateS, writeS) )
import Data.DeltaMap
    ( DeltaMap (Adjust, Delete, Insert) )
import Data.Generics.Internal.VL
    ( view, (^.) )
import Database.Persist.Sql
    ( Entity, SqlPersistT, deleteWhere, entityVal, selectList, (==.) )

import qualified Cardano.Wallet.Primitive.Types as W
import qualified Data.Map.Strict as Map

{-------------------------------------------------------------------------------
    WalletState Store
-------------------------------------------------------------------------------}
-- | Store for 'WalletState' of multiple different wallets.
mkStoreWallets
    :: forall s key. (PersistAddressBook s, key ~ W.WalletId)
    => Store (SqlPersistT IO)
        (DeltaMap key (DeltaWalletState s))
mkStoreWallets = Store{loadS=load,writeS=write,updateS=update}
  where
    write = error "mkStoreWalletsCheckpoints: not implemented"

    update _ (Insert wid a) =
        writeS (mkStoreWallet wid) a
    update _ (Delete wid) = do
        -- FIXME LATER during ADP-1043:
        --  Deleting an entry in the Checkpoint table
        --  will trigger a delete cascade. We want this cascade
        --  to be explicit in our code.
        deleteWhere [CheckpointWalletId ==. wid]
    update _ (Adjust wid da) =
        updateS (mkStoreWallet wid) undefined da
        -- FIXME LATER during ADP-1043:
        --   Remove 'undefined'.
        --   Probably needs a change to 'Data.DBVar.updateS'
        --   to take a 'Maybe a' as parameter instead of an 'a'.

    load = do
        wids <- fmap (view #walId . entityVal) <$> selectAll
        runExceptT $ do
            xs <- forM wids $ ExceptT . loadS . mkStoreWallet
            pure $ Map.fromList (zip wids xs)
      where
        selectAll :: SqlPersistT IO [Entity Wallet]
        selectAll = selectList [] []

-- | Store for 'WalletState' of a single wallet.
mkStoreWallet
    :: forall s. PersistAddressBook s
    => W.WalletId
    -> Store (SqlPersistT IO) (DeltaWalletState s)
mkStoreWallet wid =
    Store{ loadS = load, writeS = write, updateS = \_ -> update }
  where
    storeCheckpoints = mkStoreCheckpoints wid

    load = do
        eprologue <- maybe
            (Left $ toException ErrBadFormatAddressPrologue) Right
                <$> loadPrologue wid
        echeckpoints <- loadS storeCheckpoints
        pure $ WalletState <$> eprologue <*> echeckpoints -- <*> etransactions

    write wallet = do
        insertPrologue wid (wallet ^. #prologue)
        writeS storeCheckpoints (wallet ^. #checkpoints)

    update =
         -- first update in list is last to be applied!
        mapM_ update1 . reverse
    update1 (ReplacePrologue prologue) =
        insertPrologue wid prologue
    update1 (UpdateCheckpoints delta) =
        -- FIXME LATER during ADP-1043: remove 'undefined'
        updateS storeCheckpoints undefined delta
