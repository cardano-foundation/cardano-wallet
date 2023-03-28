{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}

-- TODO: https://input-output.atlassian.net/browse/ADP-2841
{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 902
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}
#endif

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- An implementation of the wallet database using only pure functions.
--
-- These functions and types model the behaviour of the SQLite database backend,
-- and are used for QuickCheck state machine testing, and the MVar database
-- backend.

module Cardano.Wallet.DB.Pure.Implementation
    (
    -- * Model Types
      Database (..)
    , WalletDatabase (..)
    , emptyDatabase
    , TxHistory
    , TxHistoryMap
    , filterTxHistory

    -- * Model Operation Types
    , ModelOp
    , Err (..)

    -- * Model database functions
    , mCleanDB
    , mInitializeWallet
    , mGetWalletId
    , mPutCheckpoint
    , mReadCheckpoint
    , mListCheckpoints
    , mRollbackTo
    , mPutWalletMeta
    , mReadWalletMeta
    , mPutDelegationCertificate
    , mIsStakeKeyRegistered
    , mPutTxHistory
    , mReadTxHistory
    , mPutLocalTxSubmission
    , mReadLocalTxSubmissionPending
    , mUpdatePendingTxForExpiry
    , mRemovePendingOrExpiredTx
    , mPutPrivateKey
    , mReadPrivateKey
    , mReadGenesisParameters
    , mPutDelegationRewardBalance
    , mReadDelegationRewardBalance
    , mCheckWallet
    ) where

import Prelude

import Cardano.Pool.Types
    ( PoolId )
import Cardano.Wallet.Primitive.Model
    ( Wallet, currentTip )
import Cardano.Wallet.Primitive.Slotting
    ( TimeInterpreter, epochOf, interpretQuery, slotToUTCTime )
import Cardano.Wallet.Primitive.Types
    ( BlockHeader (blockHeight, slotNo)
    , ChainPoint
    , DelegationCertificate (..)
    , EpochNo (..)
    , GenesisParameters (..)
    , Range (..)
    , Slot
    , SlotNo (..)
    , SortOrder (..)
    , StakeKeyCertificate (..)
    , WalletDelegation (..)
    , WalletDelegationNext (..)
    , WalletDelegationStatus (..)
    , WalletMetadata (..)
    , chainPointFromBlockHeader
    , dlgCertPoolId
    , isWithinRange
    , toSlot
    )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Cardano.Wallet.Primitive.Types.Tx
    ( LocalTxSubmissionStatus (..)
    , SealedTx (..)
    , TransactionInfo (..)
    , Tx (..)
    , TxMeta (..)
    , TxStatus (..)
    )
import Control.DeepSeq
    ( NFData )
import Control.Monad
    ( join )
import Data.Bifunctor
    ( first )
import Data.Function
    ( (&) )
import Data.Functor.Identity
    ( Identity (..) )
import Data.Generics.Internal.VL.Lens
    ( view, (^.) )
import Data.List
    ( sort, sortOn )
import Data.Map.Strict
    ( Map )
import Data.Maybe
    ( catMaybes, fromMaybe, isJust, mapMaybe )
import Data.Ord
    ( Down (..) )
import Data.Quantity
    ( Quantity (..) )
import Data.Word
    ( Word32 )
import GHC.Generics
    ( Generic )

import qualified Data.Map.Strict as Map

{-------------------------------------------------------------------------------
                            Model Database wid Types
-------------------------------------------------------------------------------}

-- | Model database, parameterised by the wallet ID type, the wallet AD state
-- type, the target backend, and the private key type.
--
-- Tne type parameters exist so that simpler mock types can be used in place of
-- actual wallet types.
data Database wid s xprv = Database
    { wallet :: Maybe (wid, WalletDatabase s xprv)
    -- ^ Wallet-related information.
    , txs :: !(Map (Hash "Tx") Tx)
    -- ^ In the database, transactions are global and not associated with any
    -- particular wallet.
    }
    deriving (Generic, NFData)

deriving instance (Show wid, Show s, Show xprv) => Show (Database wid s xprv)
deriving instance (Eq wid, Eq xprv, Eq s) => Eq (Database wid s xprv)

-- | Model database record for a single wallet.
data WalletDatabase s xprv = WalletDatabase
    { checkpoints :: !(Map SlotNo (Wallet s))
    , certificates :: !(Map SlotNo (Maybe PoolId))
    , stakeKeys :: !(Map SlotNo StakeKeyCertificate)
    , metadata :: !WalletMetadata
    , txHistory :: !(Map (Hash "Tx") TxMeta)
    , xprv :: !(Maybe xprv)
    , genesisParameters :: !GenesisParameters
    , rewardAccountBalance :: !Coin
    , submittedTxs :: !(Map (Hash "Tx") (SealedTx, SlotNo))
    }
    deriving (Show, Eq, Generic, NFData)

-- | Shorthand for the putTxHistory argument type.
type TxHistoryMap = Map (Hash "Tx") (Tx, TxMeta)

-- | Shorthand for the readTransactions result type.
type TxHistory = [(Tx, TxMeta)]

-- | Produces an empty model database.
emptyDatabase :: Database wid s xprv
emptyDatabase = Database Nothing mempty

{-------------------------------------------------------------------------------
                                  Model Operation Types
-------------------------------------------------------------------------------}

-- | A database model operation, which is a function that takes a database and
-- returns:
--  * a value, which is a query of the database, or an error; and
--  * a (possibly) modified database.
type ModelOp wid s xprv a =
    Database wid s xprv -> (Either Err a, Database wid s xprv)

fmapModelOp :: (a -> b) -> ModelOp wid s xprv a -> ModelOp wid s xprv b
fmapModelOp f op db = first (fmap f) (op db)

-- | All of the possible errors that any of the model database functions might
-- return.
data Err
    = WalletAlreadyInitialized
    | WalletNotInitialized
    | NoSuchTx (Hash "Tx")
    | CantRemoveTxInLedger (Hash "Tx")
    deriving (Show, Eq)

{-------------------------------------------------------------------------------
                            Model Database wid Functions
-------------------------------------------------------------------------------}

mCleanDB :: ModelOp wid s xprv ()
mCleanDB _ = (Right (), emptyDatabase)

mInitializeWallet
    :: forall wid s xprv
    .  wid
    -> Wallet s
    -> WalletMetadata
    -> TxHistory
    -> GenesisParameters
    -> ModelOp wid s xprv ()
mInitializeWallet wid cp meta txs0 gp db@Database {wallet,txs}
    | isJust wallet = (Left WalletAlreadyInitialized, db)
    | otherwise =
        let
            wal = WalletDatabase
                { checkpoints = Map.singleton (tip cp) cp
                , stakeKeys = mempty
                , certificates = mempty
                , metadata = meta
                , txHistory = history
                , xprv = Nothing
                , genesisParameters = gp
                , rewardAccountBalance = Coin 0
                , submittedTxs = mempty
                }
            txs' = Map.fromList $ (\(tx, _) -> (view #txId tx, tx)) <$> txs0
            history = Map.fromList $ first (view #txId) <$> txs0
        in
            (Right (), Database (Just (wid, wal)) (txs <> txs'))

mCheckWallet :: ModelOp wid s xprv ()
mCheckWallet db@Database {wallet}
    | isJust wallet =
        (Right (), db)
    | otherwise = (Left WalletNotInitialized, db)

mGetWalletId :: ModelOp wid s xprv wid
mGetWalletId db@(Database wallet _) = case wallet of
    Just (wid, _) -> (Right wid, db)
    Nothing -> (Left WalletNotInitialized, db)

mPutCheckpoint
    :: Wallet s -> ModelOp wid s xprv ()
mPutCheckpoint cp = alterModelNoTxs' $ \wal
    -> wal { checkpoints = Map.insert (tip cp) cp (checkpoints wal) }

mReadCheckpoint
    :: ModelOp wid s xprv (Maybe (Wallet s))
mReadCheckpoint db@(Database wallet _) =
    case wallet of
        Just (_, wal) -> (Right (mostRecentCheckpoint wal), db)
        Nothing -> (Left WalletNotInitialized, db)

mostRecentCheckpoint :: WalletDatabase s xprv -> Maybe (Wallet s)
mostRecentCheckpoint = fmap snd . Map.lookupMax . checkpoints

mListCheckpoints
    :: ModelOp wid s xprv [ChainPoint]
mListCheckpoints db@(Database wallet _) =
    case wallet of
        Nothing -> (Left WalletNotInitialized, db)
        Just (_, wal) -> (Right $ sort $ tips wal, db)
  where
    tips =
        map (chainPointFromBlockHeader . currentTip)
            . Map.elems
            . checkpoints

mUpdatePendingTxForExpiry :: SlotNo -> ModelOp wid s xprv ()
mUpdatePendingTxForExpiry tipSlot = alterModelNoTxs' $ \wal ->
    wal
        { txHistory = setExpired <$> txHistory wal
        , submittedTxs =
            Map.withoutKeys (submittedTxs wal)
                $ Map.keysSet
                $ Map.filter isExpired (txHistory wal)
        }
  where
    setExpired :: TxMeta -> TxMeta
    setExpired txMeta
        | isExpired txMeta = txMeta {status = Expired}
        | otherwise = txMeta
    isExpired :: TxMeta -> Bool
    isExpired TxMeta {status, expiry} = case (status, expiry) of
        (Pending, Just txExp) | txExp <= tipSlot -> True
        _ -> False

mRemovePendingOrExpiredTx :: Hash "Tx" -> ModelOp wid s xprv ()
mRemovePendingOrExpiredTx tid = alterModelErr $ \wal txs ->
    case Map.lookup tid (txHistory wal) of
        Nothing -> Left (NoSuchTx tid)
        Just txMeta
            | txMeta ^. #status == InLedger ->
                Left (CantRemoveTxInLedger tid)
        Just _ ->
            Right
                ( ()
                , wal
                    { txHistory = Map.delete tid (txHistory wal)
                    , submittedTxs = Map.delete tid (submittedTxs wal)
                    }
                , txs
                )

mRollbackTo :: Slot -> ModelOp wid s xprv ChainPoint
mRollbackTo requested db@(Database wallet txs) = case wallet of
    Nothing ->
        ( Left WalletNotInitialized, db )
    Just (wid, wal) ->
        case findNearestPoint (Map.elems $ checkpoints wal) of
            Nothing -> (Left WalletNotInitialized, db)
            Just point ->
                let
                    wal' = wal
                        { checkpoints =
                            Map.filter ((<= point) . tip) (checkpoints wal)
                        , certificates =
                            Map.filterWithKey (\k _ -> k <= point) (certificates wal)
                        , txHistory =
                            Map.mapMaybe (keepOrForget point) (txHistory wal)
                        }
                in
                    ( Right
                        $ chainPointFromBlockHeader
                        $ view #currentTip
                        $ checkpoints wal Map.! point
                    , Database (Just (wid, wal')) txs
                    )
  where
    -- | Removes all transaction beyond the rollback point.
    keepOrForget :: SlotNo -> TxMeta -> Maybe TxMeta
    keepOrForget point meta
        | isAfter = Nothing
        | otherwise = Just meta
      where
        isAfter = (slotNo :: TxMeta -> SlotNo) meta > point

    -- | Find nearest checkpoint's slot before or equal to 'requested'.
    findNearestPoint :: [Wallet s] -> Maybe SlotNo
    findNearestPoint = safeHead . sortOn Down . mapMaybe fn
      where
        fn :: Wallet s -> Maybe SlotNo
        fn cp = if stip cp <= requested then Just (tip cp) else Nothing
          where
            stip = toSlot . chainPointFromBlockHeader . currentTip

    safeHead :: [a] -> Maybe a
    safeHead [] = Nothing
    safeHead (h:_) = Just h

mPutWalletMeta :: WalletMetadata -> ModelOp wid s xprv ()
mPutWalletMeta meta = alterModelNoTxs $ \wal -> ((), wal { metadata = meta })

mReadWalletMeta
    :: TimeInterpreter Identity
    -> ModelOp wid s xprv (Maybe (WalletMetadata, WalletDelegation))
mReadWalletMeta ti db@(Database wallet _) =
    case wallet of
        Nothing -> (Left WalletNotInitialized, db)
        Just (_, wal) -> (Right (mkMetadata wal), db)
  where
    epochOf' = runIdentity . interpretQuery ti . epochOf
    mkMetadata
        :: WalletDatabase s xprv
        -> Maybe (WalletMetadata, WalletDelegation)
    mkMetadata WalletDatabase{checkpoints,certificates,metadata} = do
        (slot, _) <- Map.lookupMax checkpoints
        let currentEpoch = epochOf' slot
        pure (metadata, readWalletDelegation certificates currentEpoch)

    readWalletDelegation :: Map SlotNo (Maybe PoolId) -> EpochNo -> WalletDelegation
    readWalletDelegation certificates currentEpoch
        | currentEpoch == 0 = WalletDelegation NotDelegating []
        | otherwise =
            let active = certificates
                    & Map.filterWithKey (\sl _ -> (epochOf' sl) < currentEpoch - 1)
                    & Map.lookupMax
                    & (snd =<<)
                    & maybe NotDelegating Delegating
                next1 = certificates
                    & Map.filterWithKey (\sl _ -> let ep = epochOf' sl in
                        ep >= currentEpoch - 1 && ep < currentEpoch)
                    & Map.lookupMax
                    & maybe [] (mkDelegationNext (currentEpoch + 1) . snd)
                next2 = certificates
                    & Map.filterWithKey (\sl _ -> epochOf' sl >= currentEpoch)
                    & Map.lookupMax
                    & maybe [] (mkDelegationNext (currentEpoch + 2) . snd)
            in
                WalletDelegation active (next1 ++ next2)

    mkDelegationNext :: EpochNo -> Maybe PoolId -> [WalletDelegationNext]
    mkDelegationNext ep = pure . \case
        Nothing -> WalletDelegationNext ep NotDelegating
        Just pid -> WalletDelegationNext ep (Delegating pid)

mPutDelegationCertificate
    :: DelegationCertificate
    -> SlotNo
    -> ModelOp wid s xprv ()
mPutDelegationCertificate cert slot = alterModelNoTxs'
    $ \wal@WalletDatabase {certificates, stakeKeys} ->
        wal
            { certificates = Map.insert slot (dlgCertPoolId cert) certificates
            , stakeKeys = case cert of
                CertDelegateNone {} ->
                    Map.insert slot StakeKeyDeregistration stakeKeys
                CertDelegateFull {} -> stakeKeys
                CertRegisterKey {} ->
                    Map.insert slot StakeKeyRegistration stakeKeys
            }


mIsStakeKeyRegistered
    ::  ModelOp wid s xprv Bool
mIsStakeKeyRegistered = alterModelNoTxs $ \wal@WalletDatabase{stakeKeys} ->
    ( maybe False ((== StakeKeyRegistration) . snd) (Map.lookupMax stakeKeys)
    , wal
    )

mPutTxHistory
    :: forall wid s xprv
     . TxHistory
    -> ModelOp wid s xprv ()
mPutTxHistory txList = alterModel_ $ \wal txs ->
    let
        wal' = wal {txHistory = txHistory wal <> txHistory'}
        txHistory' = Map.fromList $ first (view #txId) <$> txList
        txs' = Map.fromList $ (\(tx, _) -> (view #txId tx, tx)) <$> txList
    in
        (wal', txs <> txs')

mReadTxHistory
    :: forall wid s xprv
    . TimeInterpreter Identity
    -> Maybe Coin
    -> SortOrder
    -> Range SlotNo
    -> Maybe TxStatus
    -> ModelOp wid s xprv [TransactionInfo]
mReadTxHistory ti minWithdrawal order range mstatus db@(Database wallet txs) =
    (Right res, db)
  where
    slotStartTime' = runIdentity . interpretQuery ti . slotToUTCTime
    res = fromMaybe mempty $ do
        (_, wal) <- wallet
        (_, cp) <- Map.lookupMax (checkpoints wal)
        pure $ getTxs cp (txHistory wal)

    getTxs cp history
            = fmap (mkTransactionInfo cp)
            $ filterTxHistory minWithdrawal order range
            $ catMaybes
            [ fmap (, meta) (Map.lookup tid txs)
            | (tid, meta) <- Map.toList history
            , case mstatus of
                Nothing -> True
                Just s -> (status :: TxMeta -> TxStatus) meta == s
            ]

    mkTransactionInfo cp (tx, meta) = TransactionInfo
        { txInfoId =
            view #txId tx
        , txInfoCBOR =
            view #txCBOR tx
        , txInfoFee =
            fee tx
        , txInfoInputs =
            resolvedInputs tx
        , txInfoCollateralInputs =
            resolvedCollateralInputs tx
        , txInfoOutputs =
            outputs tx
        , txInfoCollateralOutput =
            collateralOutput tx
        , txInfoWithdrawals =
            withdrawals tx
        , txInfoMeta =
            meta
        , txInfoDepth =
            Quantity $ fromIntegral $ if tipH > txH then tipH - txH else 0
        , txInfoTime =
            slotStartTime' (meta ^. #slotNo)
        , txInfoMetadata =
            (tx ^. #metadata)
        , txInfoScriptValidity =
            (tx ^. #scriptValidity)
        }
      where
        txH  = getQuantity
             $ (blockHeight :: TxMeta -> Quantity "block" Word32)
             meta
        tipH = getQuantity
             $ (blockHeight :: BlockHeader -> Quantity "block" Word32)
             $ currentTip cp

mPutPrivateKey :: xprv -> ModelOp wid s xprv ()
mPutPrivateKey pk = alterModelNoTxs' $ \wal -> wal { xprv = Just pk }

mReadPrivateKey ::  ModelOp wid s xprv (Maybe xprv)
mReadPrivateKey = fmapModelOp join $ readWalletModelMaybe $ \wal -> xprv wal

mReadGenesisParameters
    :: ModelOp wid s xprv (Maybe GenesisParameters)
mReadGenesisParameters = readWalletModelMaybe $ \wal -> genesisParameters wal


mPutDelegationRewardBalance
    :: Coin -> ModelOp wid s xprv ()
mPutDelegationRewardBalance amt = alterModelNoTxs' $ \wal ->
    wal { rewardAccountBalance = amt }

mReadDelegationRewardBalance
    :: ModelOp wid s xprv Coin
mReadDelegationRewardBalance  =
    fmapModelOp (fromMaybe (Coin 0)) $ readWalletModelMaybe $ \wal ->
        rewardAccountBalance wal
    -- (Right (maybe (Coin 0) rewardAccountBalance $ Map.lookup wallet), db)

mPutLocalTxSubmission ::
    Hash "Tx" -> SealedTx -> SlotNo -> ModelOp wid s xprv ()
mPutLocalTxSubmission tid tx sl = alterModelErr $ \wal txs ->
    case Map.lookup tid (txHistory wal) of
        Nothing -> Left $ NoSuchTx tid
        Just _ -> Right ((), insertSubmittedTx wal, txs)
  where
    insertSubmittedTx wal = wal { submittedTxs = putTx (submittedTxs wal) }
    putTx = Map.insertWith upsert tid (tx, sl)
    upsert (_, newSl) (origTx, _) = (origTx, newSl)

mReadLocalTxSubmissionPending
    ::  ModelOp wid s xprv [LocalTxSubmissionStatus SealedTx]
mReadLocalTxSubmissionPending = readWalletModel $ \wal ->
    sortOn (view #txId) $ mapMaybe (getSubmission wal) (pendings wal)
  where
    pendings = mapMaybe getPending . Map.toList . txHistory

    getPending :: (Hash "Tx", TxMeta) -> Maybe (Hash "Tx", SlotNo)
    getPending (txid, TxMeta{status,slotNo})
        | status == Pending = Just (txid, slotNo)
        | otherwise = Nothing

    getSubmission wal (tid, _sl0) = make <$> Map.lookup tid (submittedTxs wal)
      where
        make (tx, sl1) = LocalTxSubmissionStatus tid tx sl1

{-------------------------------------------------------------------------------
                             Model function helpers
-------------------------------------------------------------------------------}

alterModelErr
    :: ( WalletDatabase s xprv
         -> Map (Hash "Tx") Tx
         -> Either Err (x, WalletDatabase s xprv, Map (Hash "Tx") Tx)
       )
    -> Database wid s xprv
    -> (Either Err x, Database wid s xprv)
alterModelErr f (Database wallet txs) = case wallet of
    Nothing -> (Left WalletNotInitialized, Database wallet txs)
    Just (wid, wal) ->
        case f wal txs of
            Left err -> (Left err, Database wallet txs)
            Right (x, wal', txs') ->
                (Right x, Database (Just (wid, wal')) txs')

alterModel
    :: ( WalletDatabase s xprv
         -> Map (Hash "Tx") Tx
         -> (x, WalletDatabase s xprv, Map (Hash "Tx") Tx)
       )
    -> Database wid s xprv
    -> (Either Err x, Database wid s xprv)
alterModel f (Database wallet txs) = alterModelErr f' (Database wallet txs)
  where
    f' wal txs' = Right $ f wal txs'

alterModel_
    :: ( WalletDatabase s xprv
         -> Map (Hash "Tx") Tx
         -> (WalletDatabase s xprv, Map (Hash "Tx") Tx)
       )
    -> Database wid s xprv
    -> (Either Err (), Database wid s xprv)
alterModel_ f = alterModel $ \wal txs ->
    let (wal', txs') = f wal txs
    in  ((), wal', txs')

alterModelNoTxs
    :: (WalletDatabase s xprv -> (x, WalletDatabase s xprv))
    -> Database wid s xprv
    -> (Either Err x, Database wid s xprv)
alterModelNoTxs f = alterModel $ \wal txs ->
    let (x, wal') = f wal
    in (x, wal', txs)

alterModelNoTxs' :: (WalletDatabase s xprv -> WalletDatabase s xprv)
    -> Database wid s xprv
    -> (Either Err (), Database wid s xprv)
alterModelNoTxs' f = alterModelNoTxs $ \wal -> ((), f wal)

-- | Create a 'ModelOp wid' for a specific wallet which reads but does not alter the
-- database.
readWalletModel
    ::  (WalletDatabase s xprv -> a)
    -> ModelOp wid s xprv a
readWalletModel f db@(Database wallet _) = case wallet of
    Nothing -> (Left WalletNotInitialized, db)
    Just (_, wal) -> (Right (f wal), db)

readWalletModelMaybe :: (WalletDatabase s xprv -> a)
    -> Database wid s xprv
    -> (Either err (Maybe a), Database wid s xprv)
readWalletModelMaybe f db@(Database wallet _) = case wallet of
    Nothing -> (Right Nothing, db)
    Just (_, wal) -> (Right (Just (f wal)), db)

-- | Apply optional filters on slotNo and sort using the default sort order
-- (first time/slotNo, then by TxId) to a 'TxHistory'.
filterTxHistory
    :: Maybe Coin
    -> SortOrder
    -> Range SlotNo
    -> TxHistory
    -> TxHistory
filterTxHistory minWithdrawal order range =
    filter (filterWithdrawals minWithdrawal)
    . filter ((`isWithinRange` range) . (slotNo :: TxMeta -> SlotNo) . snd)
    . (case order of
        Ascending -> reverse
        Descending -> id)
    . sortBySlot
    . sortByTxId
  where
    sortBySlot = sortOn (Down . (slotNo :: TxMeta -> SlotNo) . snd)
    sortByTxId = sortOn (view #txId . fst)
    atLeast inf = not . Map.null . Map.filter (>= inf)
    filterWithdrawals = maybe
        (const True)
        (\inf -> atLeast inf . withdrawals . fst)

tip :: Wallet s -> SlotNo
tip = (slotNo :: BlockHeader -> SlotNo) . currentTip
