{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Copyright: 2022 IOHK
-- License: Apache-2.0
--
-- Implementation of a 'Store' for 'Submissions' based on
--     'DeltaSubmissions' delta.
module Cardano.Wallet.DB.Store.Submissions.Operations
    ( TxSubmissions
    , TxSubmissionsStatus
    , DeltaTxSubmissions
    , mkStoreSubmissions
    , SubmissionMeta (..)
    , submissionMetaFromTxMeta
    , DurableSubmission (..)
    , isRollbackConflict
    , rollbackConflictCode
    , DurableSubmissionInput (..)
    , DurableSubmissionInsert (..)
    , insertOrClassifyDurableSubmission
    , updateDurableSubmission
    , compareAndUpdateDurableSubmission
    , claimDurableSubmissionAttempt
    , releaseDurableSubmissionClaims
    , restoreDurableSubmissionClaims
    , readDurableSubmissions
    )
where

import Cardano.Wallet.DB.Sqlite.Schema
    ( DappSubmission (..)
    , DappSubmissionInput (..)
    , EntityField (..)
    , Key (DappSubmissionKey, SubmissionsKey, SubmissionsSlotsKey)
    , Submissions (Submissions)
    , SubmissionsSlots (SubmissionsSlots)
    )
import Cardano.Wallet.DB.Sqlite.Types
    ( DappSubmissionInputRole (..)
    , DappSubmissionStatusEnum (..)
    , TxId
    , TxSubmissionStatusEnum (..)
    )
import Cardano.Wallet.Primitive.Types
    ( SlotNo (..)
    , WalletId
    )
import Cardano.Wallet.Primitive.Types.Tx.TxMeta
    ( TxMeta (..)
    )
import Cardano.Wallet.Submissions.Operations
    ( applyOperations
    )
import Cardano.Wallet.Submissions.Submissions
    ( TxStatusMeta (..)
    , finality
    , tip
    , transactions
    , transactionsL
    )
import Control.Exception
    ( Exception
    , SomeException (..)
    )
import Control.Lens
    ( (^.)
    )
import Control.Monad
    ( forM_
    )
import Control.Monad.Class.MonadThrow
    ( throwIO
    )
import Data.Delta
    ( Delta (..)
    )
import Data.List
    ( nub
    , sort
    )
import Data.Map.Strict
    ( Map
    )
import Data.Quantity
    ( Quantity
    )
import Data.Store
    ( UpdateStore
    , mkUpdateStore
    , updateLoad
    )
import Data.Text
    ( Text
    )
import Data.Time.Clock
    ( UTCTime
    )
import Data.Word
    ( Word32
    , Word64
    )
import Database.Persist
    ( Entity (..)
    , PersistStoreRead (get)
    , PersistStoreWrite (delete, insert, repsert, update)
    , selectList
    , (=.)
    , (==.)
    )
import Database.Persist.Sql
    ( PersistQueryWrite (updateWhere)
    , SqlPersistT
    )
import Prelude

import qualified Cardano.Wallet.Primitive.Types.Coin as W
import qualified Cardano.Wallet.Primitive.Types.Tx as W
import qualified Cardano.Wallet.Primitive.Types.Tx.TxMeta as W
import qualified Cardano.Wallet.Submissions.Operations as Sbm
import qualified Cardano.Wallet.Submissions.Submissions as Sbm
import qualified Cardano.Wallet.Submissions.TxStatus as Sbm
import qualified Data.Map.Strict as Map

{-----------------------------------------------------------------------------
    Data types
------------------------------------------------------------------------------}
type TxSubmissions =
    Sbm.Submissions SubmissionMeta SlotNo (TxId, W.SealedTx)
type TxSubmissionsStatus =
    Sbm.TxStatusMeta SubmissionMeta SlotNo (TxId, W.SealedTx)
type DeltaTxSubmissions1 =
    Sbm.Operation SubmissionMeta SlotNo (TxId, W.SealedTx)
type DeltaTxSubmissions =
    [DeltaTxSubmissions1]

instance Delta DeltaTxSubmissions1 where
    type Base DeltaTxSubmissions1 = TxSubmissions
    apply = applyOperations

{-----------------------------------------------------------------------------
    Data types
------------------------------------------------------------------------------}
data SubmissionMeta = SubmissionMeta
    { submissionMetaSlot :: SlotNo
    , submissionMetaHeight :: Quantity "block" Word32
    , submissionMetaAmount :: W.Coin
    , submissionMetaDirection :: W.Direction
    , submissionMetaResubmitted :: SlotNo
    }
    deriving (Show, Eq)

submissionMetaFromTxMeta :: TxMeta -> SlotNo -> SubmissionMeta
submissionMetaFromTxMeta TxMeta{direction, blockHeight, slotNo, amount} resub =
    SubmissionMeta
        { submissionMetaSlot = slotNo
        , submissionMetaHeight = blockHeight
        , submissionMetaAmount = amount
        , submissionMetaDirection = direction
        , submissionMetaResubmitted = resub
        }

-- | Exact evidence and its state for a wallet-scoped external submission.
-- The sealed envelope is compared byte-for-byte by 'SealedTx''s 'Eq'
-- instance; a body hash alone is deliberately insufficient.
data DurableSubmission = DurableSubmission
    { durableWalletId :: WalletId
    , durableTxId :: TxId
    , durableSealedTx :: W.SealedTx
    , durableExpiration :: Maybe SlotNo
    , durableAuthorized :: Bool
    , durableStatus :: DappSubmissionStatusEnum
    , durableAttemptGeneration :: Word64
    , durableBroadcastGeneration :: Maybe Word64
    , durableBroadcastStarted :: Maybe UTCTime
    , durableAcceptance :: Maybe SlotNo
    , durableRejectionCode :: Maybe Text
    }
    deriving (Eq, Show)

-- The existing nullable rejection-code column records an internal rollback
-- conflict while keeping revision-1 submission statuses unchanged. Unlike
-- migrated V5 outcome-unknown rows, these rows do not own active claims.
rollbackConflictCode :: Text
rollbackConflictCode = "rollback_conflict"

isRollbackConflict :: DurableSubmission -> Bool
isRollbackConflict DurableSubmission{durableStatus, durableRejectionCode} =
    durableStatus == OutcomeUnknownE
        && durableRejectionCode == Just rollbackConflictCode


data DurableSubmissionInput = DurableSubmissionInput
    { durableInputTxId :: TxId
    , durableInputIndex :: Word32
    , durableInputRole :: DappSubmissionInputRole
    }
    deriving (Eq, Ord, Show)

data DurableSubmissionInsert
    = DurableSubmissionAuthorized
    | DurableSubmissionReplay DurableSubmission
    | DurableSubmissionIdentityConflict
    | DurableSubmissionInputConflict
    deriving (Eq, Show)

-- | Insert exact evidence and all non-reference claims, or classify a replay.
-- The caller executes this action inside its wallet DB transaction, so the
-- submission row and every claim are committed together before node I/O.
insertOrClassifyDurableSubmission
    :: DurableSubmission
    -> [DurableSubmissionInput]
    -> SqlPersistT IO DurableSubmissionInsert
insertOrClassifyDurableSubmission submission@DurableSubmission{..} inputs = do
    let requestedInputs = sort inputs
        -- One active lock per outpoint; exact roles remain in the sealed transaction.
        claims =
            Map.elems
                $ Map.fromListWith
                    min
                    [ ((durableInputTxId input, durableInputIndex input), input)
                    | input <- requestedInputs
                    ]
    existing <-
        selectList
            [ DappSubmissionWallet ==. durableWalletId
            , DappSubmissionTxId ==. durableTxId
            ]
            []
    case existing of
        [Entity _ stored] -> do
            storedInputs <- readDurableInputs durableWalletId durableTxId
            pure
                $ if sameDurableIdentity stored submission
                    && requestedInputs == nub requestedInputs
                    && sort storedInputs == claims
                    then DurableSubmissionReplay (durableFromRow stored)
                    else DurableSubmissionIdentityConflict
        []
            | length requestedInputs /= length (nub requestedInputs) ->
                pure DurableSubmissionInputConflict
            | otherwise -> do
                claimed <- fmap concat $ mapM claimOwners claims
                if null claimed
                    then do
                        _ <-
                            insert
                                $ DappSubmission
                                    durableWalletId
                                    durableTxId
                                    durableSealedTx
                                    durableExpiration
                                    durableAuthorized
                                    durableStatus
                                    durableAttemptGeneration
                                    durableBroadcastGeneration
                                    durableBroadcastStarted
                                    durableAcceptance
                                    durableRejectionCode
                        forM_ claims $ \DurableSubmissionInput{..} -> do
                            _ <-
                                insert
                                    $ DappSubmissionInput
                                        durableWalletId
                                        durableTxId
                                        durableInputTxId
                                        durableInputIndex
                                        durableInputRole
                                        True
                            pure ()
                        pure DurableSubmissionAuthorized
                    else pure DurableSubmissionInputConflict
        _ -> pure DurableSubmissionIdentityConflict
  where
    claimOwners DurableSubmissionInput{..} =
        selectList
            [ DappSubmissionInputWallet ==. durableWalletId
            , DappSubmissionInputSourceTxId ==. durableInputTxId
            , DappSubmissionInputSourceIndex ==. durableInputIndex
            , DappSubmissionInputActive ==. True
            ]
            []

-- | Atomically acquire the only permitted broadcast attempt for an authorized
-- submission. A concurrent exact replay observes the durable state but cannot
-- dispatch a second network request.
claimDurableSubmissionAttempt
    :: WalletId
    -> TxId
    -> Word64
    -> UTCTime
    -> SqlPersistT IO (Maybe DurableSubmission)
claimDurableSubmissionAttempt walletId txId generation started = do
    rows <-
        selectList
            [ DappSubmissionWallet ==. walletId
            , DappSubmissionTxId ==. txId
            ]
            []
    case rows of
        [Entity _ row]
            | dappSubmissionAuthorized row
                && dappSubmissionStatus row == AuthorizedE
                && dappSubmissionAttemptGeneration row == generation -> do
                let broadcasting =
                        (durableFromRow row)
                            { durableStatus = BroadcastingE
                            , durableBroadcastGeneration = Just generation
                            , durableBroadcastStarted = Just started
                            }
                update
                    (DappSubmissionKey walletId txId)
                    [ DappSubmissionStatus =. BroadcastingE
                    , DappSubmissionBroadcastGeneration =. Just generation
                    , DappSubmissionBroadcastStarted =. Just started
                    ]
                pure $ Just broadcasting
        _ -> pure Nothing

-- | Compare the entire predecessor before changing state or claims. The
-- caller's database transaction serializes the comparison and update.
-- Equal expected/replacement arguments only read the actual current row.
compareAndUpdateDurableSubmission
    :: DurableSubmission
    -> DurableSubmission
    -> SqlPersistT IO (Maybe DurableSubmission)
compareAndUpdateDurableSubmission expected replacement = do
    current <- get key
    case current of
        Just row
            | expected == replacement -> pure $ Just $ durableFromRow row
            | durableFromRow row == expected
                && sameDurableIdentity row replacement
                && durableExpiration expected == durableExpiration replacement -> do
                updateDurableSubmission replacement
                fmap durableFromRow <$> get key
        unchanged -> pure $ durableFromRow <$> unchanged
  where
    key = DappSubmissionKey (durableWalletId expected) (durableTxId expected)

-- | Unconditional state-and-claims transition for transaction-local work.
updateDurableSubmission :: DurableSubmission -> SqlPersistT IO ()
updateDurableSubmission DurableSubmission{..} = do
    prior <-
        selectList
            [ DappSubmissionWallet ==. durableWalletId
            , DappSubmissionTxId ==. durableTxId
            ]
            []
    -- A rollback may race a newer submission that claimed the released input.
    -- Keep the older row unclaimed and non-broadcastable until that claim is
    -- released, rather than throwing from the wallet's reconciliation worker.
    restored <- case durableStatus of
        SubmittedE
            | any
                (\(Entity _ row) ->
                    dappSubmissionStatus row == InLedgerDappE
                        || dappSubmissionStatus row == OutcomeUnknownE
                            && dappSubmissionRejectionCode row
                                == Just rollbackConflictCode
                )
                prior ->
                restoreDurableSubmissionClaims durableWalletId durableTxId
        _ -> pure True
    let finalStatus = if restored then durableStatus else OutcomeUnknownE
        finalCode
            | not restored = Just rollbackConflictCode
            | durableRejectionCode == Just rollbackConflictCode = Nothing
            | otherwise = durableRejectionCode
    update
        (DappSubmissionKey durableWalletId durableTxId)
        [ DappSubmissionAuthorized =. (restored && durableAuthorized)
        , DappSubmissionStatus =. finalStatus
        , DappSubmissionAttemptGeneration =. durableAttemptGeneration
        , DappSubmissionBroadcastGeneration =. durableBroadcastGeneration
        , DappSubmissionBroadcastStarted =. durableBroadcastStarted
        , DappSubmissionAcceptance =. durableAcceptance
        , DappSubmissionRejectionCode =. finalCode
        ]
    case durableStatus of
        RejectedE -> releaseDurableSubmissionClaims durableWalletId durableTxId
        ExpiredDappE -> releaseDurableSubmissionClaims durableWalletId durableTxId
        InLedgerDappE -> releaseDurableSubmissionClaims durableWalletId durableTxId
        _ -> pure ()

-- | Release normal and collateral claims while retaining identity evidence.
releaseDurableSubmissionClaims
    :: WalletId -> TxId -> SqlPersistT IO ()
releaseDurableSubmissionClaims walletId txId =
    updateWhere
        [ DappSubmissionInputWallet ==. walletId
        , DappSubmissionInputTxId ==. txId
        ]
        [DappSubmissionInputActive =. False]

-- | Re-activate preserved claims on rollback only when no other active
-- submission owns the same outpoint.
restoreDurableSubmissionClaims
    :: WalletId -> TxId -> SqlPersistT IO Bool
restoreDurableSubmissionClaims walletId txId = do
    claims <- readDurableInputs walletId txId
    owners <- fmap concat $ mapM activeOwners claims
    if null owners
        then do
            updateWhere
                [ DappSubmissionInputWallet ==. walletId
                , DappSubmissionInputTxId ==. txId
                ]
                [DappSubmissionInputActive =. True]
            pure True
        else pure False
  where
    activeOwners DurableSubmissionInput{..} =
        selectList
            [ DappSubmissionInputWallet ==. walletId
            , DappSubmissionInputSourceTxId ==. durableInputTxId
            , DappSubmissionInputSourceIndex ==. durableInputIndex
            , DappSubmissionInputActive ==. True
            ]
            []

readDurableSubmissions
    :: WalletId -> SqlPersistT IO [DurableSubmission]
readDurableSubmissions walletId =
    fmap (durableFromRow . entityVal)
        <$> selectList [DappSubmissionWallet ==. walletId] []

readDurableInputs
    :: WalletId -> TxId -> SqlPersistT IO [DurableSubmissionInput]
readDurableInputs walletId txId =
    fmap (toInput . entityVal)
        <$> selectList
            [ DappSubmissionInputWallet ==. walletId
            , DappSubmissionInputTxId ==. txId
            ]
            []
  where
    toInput DappSubmissionInput{..} =
        DurableSubmissionInput
            dappSubmissionInputSourceTxId
            dappSubmissionInputSourceIndex
            dappSubmissionInputRole

durableFromRow :: DappSubmission -> DurableSubmission
durableFromRow DappSubmission{..} =
    DurableSubmission
        dappSubmissionWallet
        dappSubmissionTxId
        dappSubmissionTx
        dappSubmissionExpiration
        dappSubmissionAuthorized
        dappSubmissionStatus
        dappSubmissionAttemptGeneration
        dappSubmissionBroadcastGeneration
        dappSubmissionBroadcastStarted
        dappSubmissionAcceptance
        dappSubmissionRejectionCode

sameDurableIdentity :: DappSubmission -> DurableSubmission -> Bool
sameDurableIdentity
    DappSubmission
        { dappSubmissionWallet
        , dappSubmissionTxId
        , dappSubmissionTx
        }
    DurableSubmission{..} =
        dappSubmissionWallet == durableWalletId
            && dappSubmissionTxId == durableTxId
            && dappSubmissionTx == durableSealedTx

{-----------------------------------------------------------------------------
    Store for a single wallet
------------------------------------------------------------------------------}
syncSubmissions
    :: WalletId -> TxSubmissions -> TxSubmissions -> SqlPersistT IO ()
syncSubmissions wid old new = do
    let deletes = transactions old `Map.difference` transactions new
    forM_ (Map.keys deletes) $ \k -> delete (SubmissionsKey k)

    let repserts = new ^. transactionsL
    forM_ (Map.assocs repserts)
        $ \(iden, TxStatusMeta status SubmissionMeta{..}) -> do
            let result = case status of
                    Sbm.Expired expiring (_, sealed) ->
                        Just (sealed, expiring, Nothing, ExpiredE)
                    Sbm.InSubmission expiring (_, sealed) ->
                        Just (sealed, expiring, Nothing, InSubmissionE)
                    Sbm.InLedger expiring acceptance (_, sealed) ->
                        Just (sealed, expiring, Just acceptance, InLedgerE)
                    Sbm.Unknown -> Nothing
            case result of
                Just (sealed, expiring, acceptance, statusNumber) ->
                    repsert
                        (SubmissionsKey iden)
                        ( Submissions
                            iden
                            sealed
                            expiring
                            acceptance
                            wid
                            statusNumber
                            submissionMetaSlot
                            submissionMetaHeight
                            submissionMetaAmount
                            submissionMetaDirection
                            submissionMetaResubmitted
                        )
                Nothing -> pure ()
    repsert
        (SubmissionsSlotsKey wid)
        $ SubmissionsSlots (finality new) (tip new) wid

instance Sbm.HasTxId (TxId, W.SealedTx) where
    type TxId (TxId, W.SealedTx) = TxId
    txId (iden, _) = iden

data ErrSubmissions
    = ErrSubmissionsSlotsMissingForWallet WalletId
    | ErrMoreThanOneSubmissionsSlotsDefinedForWallet WalletId
    deriving (Show, Eq, Exception)

mkStoreAnySubmissions
    :: (Base d ~ TxSubmissions, Delta d)
    => WalletId
    -> UpdateStore (SqlPersistT IO) d
mkStoreAnySubmissions wid =
    mkUpdateStore load write updateStore
  where
    load = do
        slots <- selectList [SubmissionsSlotsWallet ==. wid] []
        txs <- selectList [SubmissionWallet ==. wid] []
        pure $ case slots of
            [] -> Left $ SomeException $ ErrSubmissionsSlotsMissingForWallet wid
            [Entity _ (SubmissionsSlots finality' tip' _)] ->
                Right
                    $ Sbm.Submissions (mkTransactions txs) finality' tip'
            -- Note: We don't try very hard to detect whether the database
            -- contains messed-up data.
            _ ->
                Left
                    $ SomeException
                    $ ErrMoreThanOneSubmissionsSlotsDefinedForWallet wid
    write = syncSubmissions wid (Sbm.Submissions mempty 0 0)
    updateStore = updateLoad load throwIO $ \base delta ->
        syncSubmissions wid base $ apply delta base

mkTransactions :: [Entity Submissions] -> Map TxId TxSubmissionsStatus
mkTransactions xs = Map.fromList $ do
    Entity
        _
        ( Submissions
                iden
                sealed
                expiration
                acceptance
                _
                status
                slot
                height
                amount
                direction
                resubmitted
            ) <-
        xs
    pure
        ( iden
        , mkStatusMeta
            (SubmissionMeta slot height amount direction resubmitted)
            iden
            sealed
            expiration
            acceptance
            status
        )

mkStatusMeta
    :: SubmissionMeta
    -> TxId
    -> W.SealedTx
    -> SlotNo
    -> Maybe SlotNo
    -> TxSubmissionStatusEnum
    -> TxSubmissionsStatus
mkStatusMeta meta iden sealed expiring acceptance n =
    (`TxStatusMeta` meta) $ mkStatus iden sealed expiring acceptance n

mkStatus
    :: TxId
    -> W.SealedTx
    -> SlotNo
    -> Maybe SlotNo
    -> TxSubmissionStatusEnum
    -> (Sbm.TxStatus SlotNo (TxId, W.SealedTx))
mkStatus iden sealed expiring (Just acceptance) InLedgerE =
    Sbm.InLedger expiring acceptance (iden, sealed)
mkStatus iden sealed expiring Nothing InSubmissionE =
    Sbm.InSubmission expiring (iden, sealed)
mkStatus iden sealed expiring Nothing ExpiredE =
    Sbm.Expired expiring (iden, sealed)
mkStatus _ _ _ _ _ =
    Sbm.Unknown

mkStoreSubmissions
    :: WalletId
    -> UpdateStore (SqlPersistT IO) DeltaTxSubmissions1
mkStoreSubmissions = mkStoreAnySubmissions
