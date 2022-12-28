{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-orphans #-}

{- |
Copyright: 2022 IOHK
License: Apache-2.0

Implementation of a 'Store' for 'Submissions' based on
    'DeltaSubmissions' delta.

-}
module Cardano.Wallet.DB.Store.Submissions.New.Operations
    ( TxSubmissions
    , mkTransactions
    , syncSubmissions
    , mkStoreSubmissions
    , DeltaTxSubmissions
    , SubmissionMeta (..)
    , WalletSubmissions
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
    ( TxStatusMeta (..), finality, tip, transactions, transactionsL )
import Control.Exception
    ( Exception, SomeException (..) )
import Control.Lens
    ( (^.) )
import Control.Monad
    ( forM_ )
import Data.DBVar
    ( Store (..) )
import Data.Delta
    ( Delta (..) )
import Data.Map.Strict
    ( Map )
import Data.Quantity
import Data.Word
import Database.Persist
    ( Entity (Entity), PersistStoreWrite (delete, repsert), selectList, (==.) )
import Database.Persist.Sql
    ( SqlPersistT )

import qualified Cardano.Wallet.Primitive.Types.Coin as W
import qualified Cardano.Wallet.Primitive.Types.Tx as W
import qualified Cardano.Wallet.Submissions.Operations as Sbm
import qualified Cardano.Wallet.Submissions.Submissions as Sbm
import qualified Cardano.Wallet.Submissions.TxStatus as Sbm
import qualified Data.Map.Strict as Map

data SubmissionMeta  = SubmissionMeta
    { submissionMetaDirection :: W.Direction
    , submissionMetaSlot :: SlotNo
    , submissionMetaHeight :: Quantity "block" Word32
    , submissionMetaAmount :: W.Coin
    } deriving (Show, Eq)

type TxSubmissions
    = Sbm.Submissions SubmissionMeta SlotNo (TxId, W.SealedTx)
type TxSubmissionsStatus
    = Sbm.TxStatusMeta SubmissionMeta SlotNo (TxId, W.SealedTx)
type DeltaTxSubmissions
    = Sbm.Operation SubmissionMeta SlotNo (TxId, W.SealedTx)


syncSubmissions :: WalletId -> TxSubmissions -> TxSubmissions -> SqlPersistT IO ()
syncSubmissions wid old new = do

    let deletes = transactions old `Map.difference` transactions new
    forM_ (Map.keys deletes) $ \k -> delete (SubmissionsKey k)

    let repserts = new ^. transactionsL
    forM_ (Map.assocs repserts) $
        \(iden, TxStatusMeta status SubmissionMeta{..} ) -> do
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
                    (Submissions iden sealed expiring
                        acceptance wid statusNumber
                        submissionMetaDirection
                        submissionMetaSlot
                        submissionMetaHeight
                        submissionMetaAmount
                    )
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
    Entity _
        (Submissions iden sealed expiration acceptance _ status
            direction slot height amount)
            <- xs
    pure
        ( iden
        , mkStatusMeta
            (SubmissionMeta direction slot height amount)
                iden sealed expiration acceptance status
        )

mkStatusMeta
    :: SubmissionMeta
    -> TxId
    -> W.SealedTx
    -> SlotNo
    -> Maybe SlotNo
    -> TxSubmissionStatusEnum
    -> TxSubmissionsStatus
mkStatusMeta meta iden sealed expiring acceptance n
    = (`TxStatusMeta` meta) $ mkStatus iden sealed expiring acceptance n

mkStatus :: TxId -> W.SealedTx -> SlotNo -> Maybe SlotNo
    -> TxSubmissionStatusEnum -> (Sbm.TxStatus SlotNo (TxId, W.SealedTx))
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

type WalletSubmissions = Map WalletId TxSubmissions
