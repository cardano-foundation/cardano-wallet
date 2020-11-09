{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
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

module Cardano.Wallet.DB.Model
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
    , mUpdatePendingTxForExpiry
    , mRemovePendingOrExpiredTx
    , mPutPrivateKey
    , mReadPrivateKey
    , mPutProtocolParameters
    , mReadProtocolParameters
    , mPutDelegationRewardBalance
    , mReadDelegationRewardBalance
    , mCheckWallet
    ) where

import Prelude

import Cardano.Wallet.Primitive.Model
    ( Wallet, currentTip, utxo )
import Cardano.Wallet.Primitive.Slotting
    ( TimeInterpreter, epochOf, startTime )
import Cardano.Wallet.Primitive.Types
    ( BlockHeader (blockHeight, slotNo)
    , Coin (..)
    , DelegationCertificate (..)
    , Direction (..)
    , EpochNo (..)
    , PoolId
    , ProtocolParameters (..)
    , Range (..)
    , SlotNo (..)
    , SortOrder (..)
    , StakeKeyCertificate (..)
    , TransactionInfo (..)
    , Tx (..)
    , TxMeta (..)
    , TxStatus (..)
    , UTxO (..)
    , WalletDelegation (..)
    , WalletDelegationNext (..)
    , WalletDelegationStatus (..)
    , WalletMetadata (..)
    , dlgCertPoolId
    , isWithinRange
    )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Control.Monad
    ( when )
import Data.Bifunctor
    ( first )
import Data.Function
    ( (&) )
import Data.Functor.Identity
    ( Identity (..) )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
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
    ( Word32, Word64 )
import GHC.Generics
    ( Generic )
import Numeric.Natural
    ( Natural )

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
    , txs :: Map (Hash "Tx") Tx
    -- ^ In the database, transactions are global and not associated with any
    -- particular wallet.
    } deriving (Generic)

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
    , protocolParameters :: !ProtocolParameters
    , rewardAccountBalance :: !(Quantity "lovelace" Word64)
    } deriving (Show, Eq, Generic)

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
    -> ProtocolParameters
    -> ModelOp wid s xprv ()
mInitializeWallet wid cp meta txs0 pp db@Database{wallets,txs}
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
                , protocolParameters = pp
                , rewardAccountBalance = minBound
                }
            txs' = Map.fromList $ (\(tx, _) -> (txId tx, tx)) <$> txs0
            history = Map.fromList $ first txId <$> txs0
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
  where
    mostRecentCheckpoint :: WalletDatabase s xprv -> Maybe (Wallet s)
    mostRecentCheckpoint = fmap snd . Map.lookupMax . checkpoints

mListCheckpoints
    :: Ord wid => wid -> ModelOp wid s xprv [BlockHeader]
mListCheckpoints wid db@(Database wallets _) =
    (Right $ sort $ maybe [] tips (Map.lookup wid wallets), db)
  where
    tips = map currentTip . Map.elems . checkpoints

mUpdatePendingTxForExpiry :: Ord wid => wid -> SlotNo -> ModelOp wid s xprv ()
mUpdatePendingTxForExpiry wid currentTip = alterModel wid $ \wal ->
    ((), wal { txHistory = setExpired <$> txHistory wal })
  where
    setExpired :: TxMeta -> TxMeta
    setExpired txMeta@TxMeta{status,expiry} = case (status, expiry) of
        (Pending, Just txExp) | txExp <= currentTip ->
            txMeta { status = Expired }
        _ ->
            txMeta

mRemovePendingOrExpiredTx :: Ord wid => wid -> Hash "Tx" -> ModelOp wid s xprv ()
mRemovePendingOrExpiredTx wid tid = alterModelErr wid $ \wal ->
    case Map.lookup tid (txHistory wal) of
        Nothing ->
            ( Left (NoSuchTx wid tid), wal )
        Just txMeta | txMeta ^. #status == InLedger ->
            ( Left (CantRemoveTxInLedger wid tid), wal )
        Just _ ->
            ( Right (), wal { txHistory = Map.delete tid (txHistory wal) } )

mRollbackTo :: Ord wid => wid -> SlotNo -> ModelOp wid s xprv SlotNo
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
                    ( Right point
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
        fn cp = if (tip cp <= requested) then Just (tip cp) else Nothing

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
    -> ModelOp wid s xprv (Maybe WalletMetadata)
mReadWalletMeta interpretTime wid db@(Database wallets _) =
    (Right (mkMetadata =<< Map.lookup wid wallets), db)
  where
    epochOf' = runIdentity . interpretTime . epochOf
    mkMetadata :: WalletDatabase s xprv -> Maybe WalletMetadata
    mkMetadata WalletDatabase{checkpoints,certificates,metadata} = do
        (slot, _) <- Map.lookupMax checkpoints
        let currentEpoch = epochOf' slot
        pure $ metadata { delegation = readWalletDelegation certificates currentEpoch }

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
            txHistory' = Map.fromList $ first txId <$> txList
            txs' = Map.fromList $ (\(tx, _) -> (txId tx, tx)) <$> txList
        Nothing -> (Left (NoSuchWallet wid), db)

mReadTxHistory
    :: forall wid s xprv. Ord wid
    => TimeInterpreter Identity
    -> wid
    -> Maybe (Quantity "lovelace" Natural)
    -> SortOrder
    -> Range SlotNo
    -> Maybe TxStatus
    -> ModelOp wid s xprv [TransactionInfo]
mReadTxHistory ti wid minWithdrawal order range mstatus db@(Database wallets txs) =
    (Right res, db)
  where
    slotStartTime' = runIdentity . ti . startTime
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
            txId tx
        , txInfoInputs =
            (\(inp, amt) -> (inp, amt, Map.lookup inp $ getUTxO $ utxo cp))
                <$> resolvedInputs tx
        , txInfoOutputs =
            outputs tx
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

mPutProtocolParameters
    :: Ord wid => wid -> ProtocolParameters -> ModelOp wid s xprv ()
mPutProtocolParameters wid pp = alterModel wid $ \wal ->
    ((), wal { protocolParameters = pp })

mReadProtocolParameters
    :: Ord wid => wid -> ModelOp wid s xprv (Maybe ProtocolParameters)
mReadProtocolParameters wid db@(Database wallets _) =
    (Right (protocolParameters <$> Map.lookup wid wallets), db)

mPutDelegationRewardBalance
    :: Ord wid => wid -> Quantity "lovelace" Word64 -> ModelOp wid s xprv ()
mPutDelegationRewardBalance wid amt = alterModel wid $ \wal ->
    ((), wal { rewardAccountBalance = amt })

mReadDelegationRewardBalance
    :: Ord wid => wid -> ModelOp wid s xprv (Quantity "lovelace" Word64)
mReadDelegationRewardBalance wid db@(Database wallets _) =
    (Right (maybe minBound rewardAccountBalance $ Map.lookup wid wallets), db)

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

-- | Apply optional filters on slotNo and sort using the default sort order
-- (first time/slotNo, then by TxId) to a 'TxHistory'.
filterTxHistory
    :: Maybe (Quantity "lovelace" Natural)
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
    sortByTxId = sortOn (txId . fst)
    atLeast (Quantity inf) =
        not . Map.null . Map.filter (>= Coin (fromIntegral inf))
    filterWithdrawals = maybe
        (const True)
        (\inf -> atLeast inf . withdrawals . fst)


tip :: Wallet s -> SlotNo
tip = (slotNo :: BlockHeader -> SlotNo) . currentTip
