{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
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
    , TxHistory
    , TxHistoryMap
    , filterTxHistory

    -- * Model Operation Types
    , ModelOp
    , Err (..)

    -- * Model database functions
    , mInitializeWallet
    , mGetWalletId
    , mReadCheckpoint
    , mListCheckpoints
    , mRollbackTo
    , mPutWalletMeta
    , mPutDelegationCertificate
    , mIsStakeKeyRegistered
    , mPutTxHistory
    , mReadTxHistory
    , mPutLocalTxSubmission
    , mUpdatePendingTxForExpiry
    , mPutPrivateKey
    , mReadPrivateKey
    , mReadGenesisParameters
    , mPutDelegationRewardBalance
    , mReadDelegationRewardBalance
    ) where

import Prelude

import Cardano.Pool.Types
    ( PoolId )
import Cardano.Wallet.DB
    ( DBLayerParams (..) )
import Cardano.Wallet.Primitive.Model
    ( Wallet, currentTip )
import Cardano.Wallet.Primitive.Slotting
    ( TimeInterpreter, interpretQuery, slotToUTCTime )
import Cardano.Wallet.Primitive.Types
    ( BlockHeader (blockHeight, slotNo)
    , ChainPoint
    , DelegationCertificate (..)
    , GenesisParameters (..)
    , Range (..)
    , Slot
    , SlotNo (..)
    , SortOrder (..)
    , StakeKeyCertificate (..)
    , WalletMetadata (..)
    , chainPointFromBlockHeader
    , dlgCertPoolId
    , isWithinRange
    , toSlot
    )
import Cardano.Wallet.Primitive.Types.Address
    ( Address )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Cardano.Wallet.Primitive.Types.Tx
    ( SealedTx (..)
    , TransactionInfo (..)
    , Tx (..)
    , TxMeta (..)
    , TxStatus (..)
    )
import Cardano.Wallet.Primitive.Types.Tx.TxOut
    ( TxOut (..) )
import Control.DeepSeq
    ( NFData )
import Control.Monad
    ( join )
import Data.Bifunctor
    ( first )
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
                            Model Database wid Types
-------------------------------------------------------------------------------}

-- | Model database, parameterised by the wallet ID type, the wallet AD state
-- type, the target backend, and the private key type.
--
-- Tne type parameters exist so that simpler mock types can be used in place of
-- actual wallet types.
data Database wid s xprv = Database
    { walletId :: wid
    , wallet :: WalletDatabase s xprv
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
    = WalletNotInitialized
    | NoSuchTx (Hash "Tx")
    | CantRemoveTxInLedger (Hash "Tx")
    deriving (Show, Eq)

{-------------------------------------------------------------------------------
                            Model Database wid Functions
-------------------------------------------------------------------------------}

mInitializeWallet
    :: forall wid s xprv
     . wid
    -> DBLayerParams s
    -> Database wid s xprv
mInitializeWallet wid (DBLayerParams cp meta txs0 gp) =
    Database wid wal txs0'
  where
    wal = WalletDatabase
        { checkpoints = Map.singleton (tip cp) cp
        , stakeKeys = mempty
        , certificates = mempty
        , metadata = meta
        , txHistory = Map.fromList $ first (view #txId) <$> txs0
        , xprv = Nothing
        , genesisParameters = gp
        , rewardAccountBalance = Coin 0
        , submittedTxs = mempty
        }
    txs0' = Map.fromList $ (\(tx, _) -> (view #txId tx, tx)) <$> txs0

mGetWalletId :: ModelOp wid s xprv wid
mGetWalletId db@(Database wid _wallet _) = (Right wid, db)


mReadCheckpoint
    :: ModelOp wid s xprv (Wallet s)
mReadCheckpoint db@(Database _ wallet _)
    = (Right (mostRecentCheckpoint wallet), db)

mostRecentCheckpoint :: WalletDatabase s xprv -> Wallet s
mostRecentCheckpoint = snd . Map.findMax . checkpoints

mListCheckpoints
    :: ModelOp wid s xprv [ChainPoint]
mListCheckpoints db@(Database _ wallet _) = (Right $ sort $ tips wallet, db)
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

mRollbackTo :: Slot -> ModelOp wid s xprv ChainPoint
mRollbackTo requested (Database wid wal txs) =
    let point = findNearestPoint (Map.elems $ checkpoints wal)
        wal' =
            wal
                { checkpoints =
                    Map.filter ((<= point) . tip) (checkpoints wal)
                , certificates =
                    Map.filterWithKey
                        (\k _ -> k <= point)
                        (certificates wal)
                , txHistory =
                    Map.mapMaybe (keepOrForget point) (txHistory wal)
                }
    in  ( Right
            $ chainPointFromBlockHeader
            $ view #currentTip
            $ checkpoints wal Map.! point
        , Database wid wal' txs
        )
  where
    -- \| Removes all transaction beyond the rollback point.
    keepOrForget :: SlotNo -> TxMeta -> Maybe TxMeta
    keepOrForget point meta
        | isAfter = Nothing
        | otherwise = Just meta
      where
        isAfter = (slotNo :: TxMeta -> SlotNo) meta > point

    -- \| Find nearest checkpoint's slot before or equal to 'requested'.
    findNearestPoint :: [Wallet s] -> SlotNo
    findNearestPoint = head . sortOn Down . mapMaybe fn
      where
        fn :: Wallet s -> Maybe SlotNo
        fn cp = if stip cp <= requested then Just (tip cp) else Nothing
          where
            stip = toSlot . chainPointFromBlockHeader . currentTip

mPutWalletMeta :: WalletMetadata -> ModelOp wid s xprv ()
mPutWalletMeta meta = alterModelNoTxs $ \wal -> ((), wal { metadata = meta })

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
    -> Maybe Address
    -> ModelOp wid s xprv [TransactionInfo]
mReadTxHistory ti minWithdrawal order range mstatus maddress db@(Database _ wallet txs) =
    (Right res, db)
  where
    slotStartTime' = runIdentity . interpretQuery ti . slotToUTCTime
    res = fromMaybe mempty $ do
        (_, cp) <- Map.lookupMax (checkpoints wallet)
        pure $ getTxs cp (txHistory wallet)

    getTxs cp history
            = fmap (mkTransactionInfo cp)
            $ filterTxHistory minWithdrawal order range maddress
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
alterModelErr f (Database wid wallet txs) =
    case f wallet txs of
        Left err -> (Left err, Database wid wallet txs)
        Right (x, wal', txs') ->
            (Right x, Database wid wal' txs')

alterModel
    :: ( WalletDatabase s xprv
         -> Map (Hash "Tx") Tx
         -> (x, WalletDatabase s xprv, Map (Hash "Tx") Tx)
       )
    -> Database wid s xprv
    -> (Either Err x, Database wid s xprv)
alterModel f (Database wid wallet txs)
    = alterModelErr f' (Database wid wallet txs)
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
readWalletModelMaybe :: (WalletDatabase s xprv -> a)
    -> Database wid s xprv
    -> (Either err (Maybe a), Database wid s xprv)
readWalletModelMaybe f db@(Database _ wallet _) = (Right (Just (f wallet)), db)

-- | Apply optional filters on slotNo and sort using the default sort order
-- (first time/slotNo, then by TxId) to a 'TxHistory'.
filterTxHistory
    :: Maybe Coin
    -> SortOrder
    -> Range SlotNo
    -> Maybe Address
    -> TxHistory
    -> TxHistory
filterTxHistory minWithdrawal order range address =
    filter (filterAddress address)
    . filter (filterWithdrawals minWithdrawal)
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
    addressInTxOut addr = any (\(TxOut addr' _) -> addr' == addr)
    filterAddress = maybe
        (const True)
        (\addr -> addressInTxOut addr . outputs . fst)

tip :: Wallet s -> SlotNo
tip = (slotNo :: BlockHeader -> SlotNo) . currentTip
