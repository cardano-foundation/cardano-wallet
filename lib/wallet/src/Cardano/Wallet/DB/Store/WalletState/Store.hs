{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}

-- |
-- Copyright: © 2021–2023 IOHK
-- License: Apache-2.0
--
-- 'Store' for 'WalletState'.
module Cardano.Wallet.DB.Store.WalletState.Store
    ( mkStoreWallet
    )
    where

import Prelude

import Cardano.Wallet.DB.Errors
    ( ErrBadFormat (..) )
import Cardano.Wallet.DB.Store.Checkpoints.Store
    ( PersistAddressBook (..), mkStoreCheckpoints )
import Cardano.Wallet.DB.Store.Info.Store
    ( mkStoreInfo )
import Cardano.Wallet.DB.Store.PrivateKey.Store
    ( mkStorePrivateKey )
import Cardano.Wallet.DB.Store.Submissions.Operations
    ( mkStoreSubmissions )
import Cardano.Wallet.DB.WalletState
    ( DeltaWalletState, DeltaWalletState1 (..), WalletState (..) )
import Cardano.Wallet.Flavor
    ( WalletFlavorS, keyOfWallet )
import Control.Monad.Class.MonadThrow
    ( throwIO )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.Store
    ( Store (..), UpdateStore, mkUpdateStore, updateLoad, updateSequence )
import Database.Persist.Sqlite
    ( SqlPersistT )
import UnliftIO.Exception
    ( toException )

import qualified Cardano.Wallet.Primitive.Types as W

{-------------------------------------------------------------------------------
    WalletState Store
-------------------------------------------------------------------------------}

-- | Store for 'WalletState' of a single wallet.
mkStoreWallet
    :: PersistAddressBook s
    => WalletFlavorS s
    -> W.WalletId
    -> UpdateStore (SqlPersistT IO) (DeltaWalletState s)
mkStoreWallet wF wid = mkUpdateStore load write update
  where
    checkpointsStore = mkStoreCheckpoints wid
    submissionsStore = mkStoreSubmissions wid
    infoStore = mkStoreInfo
    pkStore = mkStorePrivateKey (keyOfWallet wF) wid

    load = do
        eprologue <-
            maybe (Left $ toException ErrBadFormatAddressPrologue) Right
                <$> loadPrologue wid
        echeckpoints <- loadS checkpointsStore
        esubmissions <- loadS submissionsStore
        einfo <- loadS infoStore
        ecredentials <- loadS pkStore
        pure
            $ WalletState
                <$> eprologue
                <*> echeckpoints
                <*> esubmissions
                <*> einfo
                <*> ecredentials

    write wallet = do
        writeS infoStore (wallet ^. #info)
        insertPrologue wid (wallet ^. #prologue)
        writeS checkpointsStore (wallet ^. #checkpoints)
        writeS submissionsStore (wallet ^. #submissions)
        writeS pkStore (wallet ^. #credentials)

    update = updateLoad load throwIO $ updateSequence update1
      where
        update1 _ (ReplacePrologue prologue') = insertPrologue wid prologue'
        update1 s (UpdateCheckpoints delta) =
            updateS checkpointsStore (Just $ checkpoints s) delta
        update1 s (UpdateSubmissions deltas) =
            updateSequence
                (updateS submissionsStore . Just)
                (submissions s)
                deltas
        update1 _ (UpdateInfo delta) = updateS infoStore Nothing delta
        update1 _ (UpdateCredentials delta) = do
            updateS pkStore Nothing delta
