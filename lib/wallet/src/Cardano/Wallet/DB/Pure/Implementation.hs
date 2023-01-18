{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
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
    , mRemoveWallet
    , mListWallets
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
    ( Direction (..)
    , LocalTxSubmissionStatus (..)
    , SealedTx (..)
    , TransactionInfo (..)
    , Tx (..)
    , TxMeta (..)
    , TxStatus (..)
    )
import Control.DeepSeq
    ( NFData )
import Control.Monad
    ( when )
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
    ( catMaybes, fromMaybe, mapMaybe )
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
                            Model Database Types
-------------------------------------------------------------------------------}

-- | Model database, parameterised by the wallet ID type, the wallet AD state
-- type, the target backend, and the private key type.
--
-- Tne type parameters exist so that simpler mock types can be used in place of
-- actual wallet types.
data Database wid s xprv = Database
    { wallets :: !(Map wid (WalletDatabase s xprv))
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

-- | Shorthand for the readTxHistory result type.
type TxHistory = [(Tx, TxMeta)]

-- | Produces an empty model database.
emptyDatabase :: Ord wid => Database wid s xprv
emptyDatabase = Database mempty mempty

{-------------------------------------------------------------------------------
                                  Model Operation Types
-------------------------------------------------------------------------------}

-- | A database model operation, which is a function that takes a database and
-- returns:
--  * a value, which is a query of the database, or an error; and
--  * a (possibly) modified database.
type ModelOp wid s xprv a =
    Database wid s xprv -> (Either (Err wid) a, Database wid s xprv)

-- | All of the possible errors that any of the model database functions might
-- return.
data Err wid
    = NoSuchWallet wid
    | WalletAlreadyExists wid
    | NoSuchTx wid (Hash "Tx")
    | CantRemoveTxInLedger wid (Hash "Tx")
    deriving (Show, Eq, Functor, Foldable, Traversable)

{-------------------------------------------------------------------------------
                            Model Database Functions
-------------------------------------------------------------------------------}

mCleanDB :: Ord wid => ModelOp wid s xprv ()
mCleanDB _ = (Right (), emptyDatabase)

mInitializeWallet
    :: forall wid s xprv. Ord wid
    => wid
    -> Wallet s
    -> WalletMetadata
    -> TxHistory
    -> GenesisParameters
    -> ModelOp wid s xprv ()
mInitializeWallet wid cp meta txs0 gp db@Database{wallets,txs}
    | wid `Map.member` wallets = (Left (WalletAlreadyExists wid), db)
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
            (Right (), Database (Map.insert wid wal wallets) (txs <> txs'))

mRemoveWallet :: Ord wid => wid -> ModelOp wid s xprv ()
mRemoveWallet wid db@Database{wallets,txs}
    | wid `Map.member` wallets =
        (Right (), Database (Map.delete wid wallets) txs)
    | otherwise = (Left (NoSuchWallet wid), db)

mCheckWallet :: Ord wid => wid -> ModelOp wid s xprv ()
mCheckWallet wid db@Database{wallets}
    | wid `Map.member` wallets =
        (Right (), db)
    | otherwise = (Left (NoSuchWallet wid), db)

mListWallets :: Ord wid => ModelOp wid s xprv [wid]
mListWallets db@(Database wallets _) = (Right (sort $ Map.keys wallets), db)

mPutCheckpoint
    :: Ord wid => wid -> Wallet s -> ModelOp wid s xprv ()
mPutCheckpoint wid cp = alterModel wid $ \wal ->
    ((), wal { checkpoints = Map.insert (tip cp) cp (checkpoints wal) })

mReadCheckpoint
    :: Ord wid => wid -> ModelOp wid s xprv (Maybe (Wallet s))
mReadCheckpoint wid db@(Database wallets _) =
    (Right (Map.lookup wid wallets >>= mostRecentCheckpoint), db)

mostRecentCheckpoint :: WalletDatabase s xprv -> Maybe (Wallet s)
mostRecentCheckpoint = fmap snd . Map.lookupMax . checkpoints

mListCheckpoints
    :: Ord wid => wid -> ModelOp wid s xprv [ChainPoint]
mListCheckpoints wid db@(Database wallets _) =
    (Right $ sort $ maybe [] tips (Map.lookup wid wallets), db)
  where
    tips = map (chainPointFromBlockHeader . currentTip) . Map.elems . checkpoints

mUpdatePendingTxForExpiry :: Ord wid => wid -> SlotNo -> ModelOp wid s xprv ()
mUpdatePendingTxForExpiry wid tipSlot = alterModel wid $ ((),) . updatePending
  where
    updatePending wal = wal
        { txHistory = setExpired <$> txHistory wal
        , submittedTxs = Map.withoutKeys (submittedTxs wal) $
            Map.keysSet $ Map.filter isExpired (txHistory wal)
        }

    setExpired :: TxMeta -> TxMeta
    setExpired txMeta
        | isExpired txMeta = txMeta { status = Expired }
        | otherwise = txMeta

    isExpired :: TxMeta -> Bool
    isExpired TxMeta{status,expiry} = case (status, expiry) of
        (Pending, Just txExp) | txExp <= tipSlot -> True
        _ -> False

mRemovePendingOrExpiredTx :: Ord wid => wid -> Hash "Tx" -> ModelOp wid s xprv ()
mRemovePendingOrExpiredTx wid tid = alterModelErr wid $ \wal ->
    case Map.lookup tid (txHistory wal) of
        Nothing ->
            ( Left (NoSuchTx wid tid), wal )
        Just txMeta | txMeta ^. #status == InLedger ->
            ( Left (CantRemoveTxInLedger wid tid), wal )
        Just _ ->
            ( Right (), wal
                { txHistory = Map.delete tid (txHistory wal)
                , submittedTxs = Map.delete tid (submittedTxs wal)
                } )

mRollbackTo :: Ord wid => wid -> Slot -> ModelOp wid s xprv ChainPoint
mRollbackTo wid requested db@(Database wallets txs) = case Map.lookup wid wallets of
    Nothing ->
        ( Left (NoSuchWallet wid), db )
    Just wal ->
        case findNearestPoint (Map.elems $ checkpoints wal) of
            Nothing -> (Left (NoSuchWallet wid), db)
            Just point ->
                let
                    wal' = wal
                        { checkpoints =
                            Map.filter ((<= point) . tip) (checkpoints wal)
                        , certificates =
                            Map.filterWithKey (\k _ -> k <= point) (certificates wal)
                        , txHistory =
                            Map.mapMaybe (rescheduleOrForget point) (txHistory wal)
                        }
                in
                    ( Right
                        $ chainPointFromBlockHeader
                        $ view #currentTip
                        $ checkpoints wal Map.! point
                    , Database (Map.insert wid wal' wallets) txs
                    )
  where
    -- | Removes 'Incoming' transaction beyond the rollback point, and
    -- reschedule as 'Pending' the 'Outgoing' one beyond the rollback point.
    rescheduleOrForget :: SlotNo -> TxMeta -> Maybe TxMeta
    rescheduleOrForget point meta = do
        let isAfter = (slotNo :: TxMeta -> SlotNo) meta > point
        let isIncoming = direction meta == Incoming
        when (isIncoming && isAfter) Nothing
        pure $ if isAfter
            then meta { slotNo = point , status = Pending }
            else meta

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

mPutWalletMeta :: Ord wid => wid -> WalletMetadata -> ModelOp wid s xprv ()
mPutWalletMeta wid meta = alterModel wid $ \wal ->
    ((), wal { metadata = meta })

mReadWalletMeta
    :: Ord wid
    => TimeInterpreter Identity
    -> wid
    -> ModelOp wid s xprv (Maybe (WalletMetadata, WalletDelegation))
mReadWalletMeta ti wid db@(Database wallets _) =
    (Right (mkMetadata =<< Map.lookup wid wallets), db)
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
    :: Ord wid
    => wid
    -> DelegationCertificate
    -> SlotNo
    -> ModelOp wid s xprv ()
mPutDelegationCertificate wid cert slot = alterModel wid
    $ \wal@WalletDatabase{certificates,stakeKeys} ->
        ( ()
        , wal
            { certificates = Map.insert slot (dlgCertPoolId cert) certificates
            , stakeKeys = case cert of
                CertDelegateNone{} -> Map.insert slot StakeKeyDeregistration stakeKeys
                CertDelegateFull{} -> stakeKeys
                CertRegisterKey{}  -> Map.insert slot StakeKeyRegistration stakeKeys
            }
        )

mIsStakeKeyRegistered
    :: Ord wid
    => wid
    -> ModelOp wid s xprv Bool
mIsStakeKeyRegistered wid = alterModel wid $ \wal@WalletDatabase{stakeKeys} ->
    ( maybe False ((== StakeKeyRegistration) . snd) (Map.lookupMax stakeKeys)
    , wal
    )

mPutTxHistory
    :: forall wid s xprv. Ord wid
    => wid
    -> TxHistory
    -> ModelOp wid s xprv ()
mPutTxHistory wid txList db@Database{wallets,txs} =
    case Map.lookup wid wallets of
        Just wal ->
            ( Right ()
            , Database (Map.insert wid wal' wallets) (txs <> txs')
            )
          where
            wal' = wal { txHistory = txHistory wal <> txHistory' }
            txHistory' = Map.fromList $ first (view #txId) <$> txList
            txs' = Map.fromList $ (\(tx, _) -> (view #txId tx, tx)) <$> txList
        Nothing -> (Left (NoSuchWallet wid), db)

mReadTxHistory
    :: forall wid s xprv . Ord wid
    => TimeInterpreter Identity
    -> wid
    -> Maybe Coin
    -> SortOrder
    -> Range SlotNo
    -> Maybe TxStatus
    -> ModelOp wid s xprv [TransactionInfo]
mReadTxHistory ti wid minWithdrawal order range mstatus db@(Database wallets txs) =
    (Right res, db)
  where
    slotStartTime' = runIdentity . interpretQuery ti . slotToUTCTime
    res = fromMaybe mempty $ do
        wal <- Map.lookup wid wallets
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

mPutPrivateKey :: Ord wid => wid -> xprv -> ModelOp wid s xprv ()
mPutPrivateKey wid pk = alterModel wid $ \wal ->
    ((), wal { xprv = Just pk })

mReadPrivateKey :: Ord wid => wid -> ModelOp wid s xprv (Maybe xprv)
mReadPrivateKey wid db@(Database wallets _) =
    (Right (Map.lookup wid wallets >>= xprv), db)

mReadGenesisParameters
    :: Ord wid => wid -> ModelOp wid s xprv (Maybe GenesisParameters)
mReadGenesisParameters wid db@(Database wallets _) =
    (Right (genesisParameters <$> Map.lookup wid wallets), db)

mPutDelegationRewardBalance
    :: Ord wid => wid -> Coin -> ModelOp wid s xprv ()
mPutDelegationRewardBalance wid amt = alterModel wid $ \wal ->
    ((), wal { rewardAccountBalance = amt })

mReadDelegationRewardBalance
    :: Ord wid => wid -> ModelOp wid s xprv Coin
mReadDelegationRewardBalance wid db@(Database wallets _) =
    (Right (maybe (Coin 0) rewardAccountBalance $ Map.lookup wid wallets), db)

mPutLocalTxSubmission ::
    Ord wid => wid -> Hash "Tx" -> SealedTx -> SlotNo -> ModelOp wid s xprv ()
mPutLocalTxSubmission wid tid tx sl = alterModelErr wid $ \wal ->
    case Map.lookup tid (txHistory wal) of
        Nothing -> (Left (NoSuchTx wid tid), wal)
        Just _ -> (Right (), insertSubmittedTx wal)
  where
    insertSubmittedTx wal = wal { submittedTxs = putTx (submittedTxs wal) }
    putTx = Map.insertWith upsert tid (tx, sl)
    upsert (_, newSl) (origTx, _) = (origTx, newSl)

mReadLocalTxSubmissionPending
    :: Ord wid
    => wid
    -> ModelOp wid s xprv [LocalTxSubmissionStatus SealedTx]
mReadLocalTxSubmissionPending wid = readWalletModel wid $ \wal ->
    sortOn (view #txId) $ mapMaybe (getSubmission wal) (pendings wal)
  where
    pendings = mapMaybe getPending . Map.toList . txHistory

    getPending :: (Hash "Tx", TxMeta) -> Maybe (Hash "Tx", SlotNo)
    getPending (txid, TxMeta{status,slotNo})
        | status == Pending = Just (txid, slotNo)
        | otherwise = Nothing

    getSubmission wal (tid, sl0) = make <$> Map.lookup tid (submittedTxs wal)
      where
        make (tx, sl1) = LocalTxSubmissionStatus tid tx sl0 sl1

{-------------------------------------------------------------------------------
                             Model function helpers
-------------------------------------------------------------------------------}

-- | Create a 'ModelOp' which mutates the database for a certain wallet id.
--
-- The given function returns a value and a modified wallet database.
alterModel
    :: Ord wid
    => wid
    -> (WalletDatabase s xprv -> (a, WalletDatabase s xprv))
    -> ModelOp wid s xprv a
alterModel wid f = alterModelErr wid (first Right . f)

-- | Create a 'ModelOp' which mutates the database for a certain wallet id.
--
-- The given function returns a either a value or error, and a modified wallet
-- database.
alterModelErr
    :: Ord wid
    => wid
    -> (WalletDatabase s xprv -> (Either (Err wid) a, WalletDatabase s xprv))
    -> ModelOp wid s xprv a
alterModelErr wid f db@Database{wallets,txs} =
    case f <$> Map.lookup wid wallets of
        Just (a, wal) -> (a, Database (Map.insert wid wal wallets) txs)
        Nothing -> (Left (NoSuchWallet wid), db)

-- | Create a 'ModelOp' for a specific wallet which reads but does not alter the
-- database.
readWalletModelMaybe
    :: Ord wid
    => wid
    -> (WalletDatabase s xprv -> a)
    -> ModelOp wid s xprv (Maybe a)
readWalletModelMaybe wid f db = (,db) $ Right $ f <$> Map.lookup wid (wallets db)

readWalletModel
    :: (Ord wid, Monoid a)
    => wid
    -> (WalletDatabase s xprv -> a)
    -> ModelOp wid s xprv a
readWalletModel wid f = first (fmap (fromMaybe mempty)) . readWalletModelMaybe wid f

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
