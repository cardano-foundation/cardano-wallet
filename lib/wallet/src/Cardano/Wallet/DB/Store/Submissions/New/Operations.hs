{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{- |
Copyright: 2022 IOHK
License: Apache-2.0

Implementation of a 'Store' for 'TxSubmissions' based on 'DeltaTxSubmissions' delta.

-}
module Cardano.Wallet.DB.Store.Submissions.New.Operations
    ( TxSubmissions
    , mkTransactions
    , syncSubmissions
    , mkStoreSubmissions
    , DeltaTxSubmissions
    ) where

import Prelude

import Cardano.Wallet.DB.Sqlite.Schema
    ( EntityField (SubmissionWallet, SubmissionsSlotsWallet)
    , Key (SubmissionsKey, SubmissionsSlotsKey)
    , Submissions (Submissions)
    , SubmissionsSlots (SubmissionsSlots)
    )
import Cardano.Wallet.DB.Sqlite.Types
    ( TxId, TxSubmissionStatusEnum (..) )
import Cardano.Wallet.Primitive.Types
    ( SlotNo (..), WalletId )
import Cardano.Wallet.Submissions.Operations
    ( applyOperations )
import Cardano.Wallet.Submissions.Submissions
    ( finality, tip, transactions )
import Control.Exception
    ( Exception, SomeException (..) )
import Control.Monad
    ( forM_ )
import Data.DBVar
    ( Store (..) )
import Data.Delta
    ( Delta (..) )
import Data.Map.Strict
    ( Map )
import Database.Persist
    ( Entity (Entity), PersistStoreWrite (delete, repsert), selectList, (==.) )
import Database.Persist.Sql
    ( SqlPersistT )

import qualified Cardano.Wallet.Primitive.Types.Tx as W
import qualified Cardano.Wallet.Submissions.Operations as Sbm
import qualified Cardano.Wallet.Submissions.Submissions as Sbm
import qualified Cardano.Wallet.Submissions.TxStatus as Sbm
import qualified Data.Map.Strict as Map

type TxSubmissions = Sbm.Submissions SlotNo (TxId, W.SealedTx)
type TxSubmissionsStatus = Sbm.TxStatus SlotNo (TxId, W.SealedTx)
type DeltaTxSubmissions = Sbm.Operation SlotNo (TxId, W.SealedTx)


syncSubmissions :: WalletId -> TxSubmissions -> TxSubmissions -> SqlPersistT IO ()
syncSubmissions wid old new = do

    let deletes = transactions old `Map.difference` transactions new
    forM_ (Map.keys deletes) $ \k -> delete (SubmissionsKey k)

    let repserts = transactions new
    forM_ (Map.assocs repserts) $ \(iden, status) -> do
        let result = case status of
                Sbm.Expired expiring (_, sealed)
                    -> Just (sealed, expiring, Nothing, ExpiredE)
                Sbm.InSubmission expiring (_, sealed)
                    -> Just (sealed, expiring, Nothing, InSubmissionE)
                Sbm.InLedger expiring acceptance (_, sealed)
                    -> Just (sealed, expiring, Just acceptance, InLedgerE)
                Sbm.Unknown -> Nothing
        case result of
            Just (sealed, expiring, acceptance, statusNumber) -> repsert
                (SubmissionsKey iden)
                (Submissions iden sealed expiring acceptance wid statusNumber)
            Nothing -> pure ()
    repsert
        (SubmissionsSlotsKey wid)
        $ SubmissionsSlots (finality new) (tip new) wid

instance Sbm.HasTxId (TxId, W.SealedTx) where
    type TxId (TxId, W.SealedTx) = TxId
    txId (iden,_) = iden

data ErrSubmissions
    = ErrSubmissionsSlotsMissingForWallet WalletId
    | ErrMoreThanOneSubmissionsSlotsDefinedForWallet WalletId
    deriving (Show, Eq, Exception)

mkStoreAnySubmissions
    :: (Base d ~ TxSubmissions, Delta d)
    => WalletId
    -> Store (SqlPersistT IO) d
mkStoreAnySubmissions wid =
    Store
    { loadS = do
        slots <- selectList [SubmissionsSlotsWallet ==. wid] []
        txs <- selectList [SubmissionWallet ==. wid ] []
        pure $ case slots of
            [] -> Left $ SomeException $ ErrSubmissionsSlotsMissingForWallet wid
            [Entity _ (SubmissionsSlots finality' tip' _)] -> Right
                $ Sbm.Submissions (mkTransactions txs) finality' tip'
                -- Note: We don't try very hard to detect whether the database
                -- contains messed-up data.
            _ -> Left $ SomeException
                    $ ErrMoreThanOneSubmissionsSlotsDefinedForWallet wid
    , writeS = syncSubmissions wid (Sbm.Submissions mempty 0 0)
    , updateS = \base delta -> syncSubmissions wid base $ apply delta base
    }

mkTransactions :: [Entity Submissions] -> Map TxId TxSubmissionsStatus
mkTransactions xs = Map.fromList $ do
    Entity _  (Submissions iden sealed expiration acceptance _ status) <- xs
    pure (iden, mkStatus iden sealed expiration acceptance status)

mkStatus
    :: TxId -> W.SealedTx -> SlotNo -> Maybe SlotNo
    -> TxSubmissionStatusEnum -> TxSubmissionsStatus
mkStatus iden sealed expiring (Just acceptance) InLedgerE
    = Sbm.InLedger expiring acceptance (iden, sealed)
mkStatus iden sealed expiring Nothing InSubmissionE
    = Sbm.InSubmission expiring (iden, sealed)
mkStatus iden sealed expiring Nothing ExpiredE
    = Sbm.Expired expiring (iden, sealed)
mkStatus _ _ _ _ _
    = Sbm.Unknown

instance Delta DeltaTxSubmissions where
  type Base DeltaTxSubmissions = TxSubmissions
  apply = applyOperations

mkStoreSubmissions :: WalletId -> Store (SqlPersistT IO) DeltaTxSubmissions
mkStoreSubmissions = mkStoreAnySubmissions
