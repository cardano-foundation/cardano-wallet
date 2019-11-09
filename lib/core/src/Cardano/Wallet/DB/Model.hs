{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright: © 2018-2019 IOHK
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
    , ErrErasePendingTx (..)
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
    , mPutTxHistory
    , mReadTxHistory
    , mPutPrivateKey
    , mReadPrivateKey
    , mRemovePendingTx
    ) where

import Prelude

import Cardano.Wallet.Primitive.Model
    ( Wallet, currentTip )
import Cardano.Wallet.Primitive.Types
    ( BlockHeader (slotId)
    , Direction (..)
    , Hash
    , PoolId (..)
    , Range (..)
    , SlotId (..)
    , SortOrder (..)
    , Tx (..)
    , TxMeta (..)
    , TxStatus (..)
    , WalletDelegation (..)
    , WalletMetadata (..)
    , isWithinRange
    )
import Control.Monad
    ( when )
import Data.Bifunctor
    ( first )
import Data.List
    ( sort, sortOn )
import Data.Map.Strict
    ( Map )
import Data.Maybe
    ( catMaybes, mapMaybe )
import Data.Ord
    ( Down (..) )
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
    , txs :: Map (Hash "Tx") Tx
    -- ^ In the database, transactions are global and not associated with any
    -- particular wallet.
    } deriving (Generic)

deriving instance (Show wid, Show xprv) => Show (Database wid s xprv)
deriving instance (Eq wid, Eq xprv, Eq s) => Eq (Database wid s xprv)

-- | Model database record for a single wallet.
data WalletDatabase s xprv = WalletDatabase
    { checkpoints :: !(Map SlotId (Wallet s))
    , certificates :: !(Map SlotId PoolId)
    , metadata :: !WalletMetadata
    , txHistory :: !(Map (Hash "Tx") TxMeta)
    , xprv :: !(Maybe xprv)
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
    | CannotRemovePendingTx (ErrErasePendingTx wid)
    deriving (Show, Eq, Functor, Foldable, Traversable)

data ErrErasePendingTx wid
    = ErrErasePendingTxNoSuchWallet wid
    | ErrErasePendingTxNoTx (Hash "Tx")
    | ErrErasePendingTxNoPendingTx (Hash "Tx")
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
    -> [(Tx, TxMeta)]
    -> ModelOp wid s xprv ()
mInitializeWallet wid cp meta txs0 db@Database{wallets,txs}
    | wid `Map.member` wallets = (Left (WalletAlreadyExists wid), db)
    | otherwise =
        let
            wal = WalletDatabase
                { checkpoints = Map.singleton (tip cp) cp
                , certificates = mempty
                , metadata = meta
                , txHistory = history
                , xprv = Nothing
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

mRemovePendingTx :: Ord wid => wid -> (Hash "Tx") -> ModelOp wid s xprv ()
mRemovePendingTx wid tid db@(Database wallets txs) = case Map.lookup wid wallets of
    Nothing ->
        ( Left (CannotRemovePendingTx (ErrErasePendingTxNoSuchWallet wid)), db )
    Just wal -> case Map.lookup tid (txHistory wal) of
        Nothing ->
            ( Left (CannotRemovePendingTx (ErrErasePendingTxNoTx tid)), db )
        Just txMeta ->
            if status txMeta == Pending then
                ( Right (), Database updateWallets txs )
            else ( Left (CannotRemovePendingTx (ErrErasePendingTxNoPendingTx tid)), db )
    where
        updateWallets = Map.adjust changeTxMeta wid wallets
        changeTxMeta meta = meta { txHistory = Map.delete tid (txHistory meta) }

mRollbackTo :: Ord wid => wid -> SlotId -> ModelOp wid s xprv ()
mRollbackTo wid point db@(Database wallets txs) = case Map.lookup wid wallets of
    Nothing ->
        ( Left (NoSuchWallet wid), db )
    Just wal ->
        case findNearestPoint (Map.elems $ checkpoints wal) of
            Nothing -> (Left (NoSuchWallet wid), db)
            Just nearest ->
                let
                    wal' = wal
                        { checkpoints =
                            Map.filter ((<= point) . tip) (checkpoints wal)
                        , certificates =
                            Map.filterWithKey (\k _ -> k <= point) (certificates wal)
                        , txHistory =
                            Map.mapMaybe (rescheduleOrForget nearest) (txHistory wal)
                        }
                in
                    ( Right ()
                    , Database (Map.insert wid wal' wallets) txs
                    )
  where
    -- | Removes 'Incoming' transaction beyond the rollback point, and
    -- reschedule as 'Pending' the 'Outgoing' one beyond the rollback point.
    rescheduleOrForget :: SlotId -> TxMeta -> Maybe TxMeta
    rescheduleOrForget nearest meta = do
        let isAfter = (slotId :: TxMeta -> SlotId) meta > point
        let isIncoming = direction meta == Incoming
        when (isIncoming && isAfter) Nothing
        pure $ if isAfter
            then meta { slotId = nearest, status = Pending }
            else meta

    -- | Find nearest checkpoint's slot before or equal to 'point'.
    findNearestPoint :: [Wallet s] -> Maybe SlotId
    findNearestPoint = safeHead . sortOn Down . mapMaybe fn
      where
        fn :: Wallet s -> Maybe SlotId
        fn cp = if (tip cp <= point) then Just (tip cp) else Nothing

    safeHead :: [a] -> Maybe a
    safeHead [] = Nothing
    safeHead (h:_) = Just h

mPutWalletMeta :: Ord wid => wid -> WalletMetadata -> ModelOp wid s xprv ()
mPutWalletMeta wid meta = alterModel wid $ \wal ->
    ((), wal { metadata = meta })

mReadWalletMeta :: Ord wid => wid -> ModelOp wid s xprv (Maybe WalletMetadata)
mReadWalletMeta wid db@(Database wallets _) =
    (Right (mkMetadata <$> Map.lookup wid wallets), db)
  where
    mkMetadata :: WalletDatabase s xprv -> WalletMetadata
    mkMetadata WalletDatabase{certificates,metadata} =
        case Map.lookupMax certificates of
            Nothing ->
                metadata { delegation = NotDelegating }
            Just (_, pool) ->
                metadata { delegation = Delegating pool }

mPutDelegationCertificate
    :: Ord wid
    => wid
    -> PoolId
    -> SlotId
    -> ModelOp wid s xprv ()
mPutDelegationCertificate wid pool slot = alterModel wid $ \wal ->
    ((), wal { certificates = Map.insert slot pool (certificates wal) })

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
    => wid
    -> SortOrder
    -> Range SlotId
    -> Maybe TxStatus
    -> ModelOp wid s xprv TxHistory
mReadTxHistory wid order range mstatus db@(Database wallets txs) = (Right res, db)
  where
    res = maybe mempty getTxs $ Map.lookup wid wallets
    getTxs wal = filterTxHistory order range $ catMaybes
            [ fmap (, meta) (Map.lookup tid txs)
            | (tid, meta) <- Map.toList (txHistory wal)
            , case mstatus of
                Nothing -> True
                Just s -> status meta == s
            ]

mPutPrivateKey :: Ord wid => wid -> xprv -> ModelOp wid s xprv ()
mPutPrivateKey wid pk = alterModel wid $ \wal ->
    ((), wal { xprv = Just pk })

mReadPrivateKey :: Ord wid => wid -> ModelOp wid s xprv (Maybe xprv)
mReadPrivateKey wid db@(Database wallets _) =
    (Right (Map.lookup wid wallets >>= xprv), db)

{-------------------------------------------------------------------------------
                             Model function helpers
-------------------------------------------------------------------------------}

alterModel
    :: Ord wid
    => wid
    -> (WalletDatabase s xprv -> (a, WalletDatabase s xprv))
    -> ModelOp wid s xprv a
alterModel wid f db@Database{wallets,txs} = case f <$> Map.lookup wid wallets of
    Just (a, wal) -> (Right a, Database (Map.insert wid wal wallets) txs)
    Nothing -> (Left (NoSuchWallet wid), db)

-- | Apply optional filters on slotId and sort using the default sort order
-- (first time/slotId, then by TxId) to a 'TxHistory'.
filterTxHistory
    :: SortOrder
    -> Range SlotId
    -> TxHistory
    -> TxHistory
filterTxHistory order range =
    filter ((`isWithinRange` range) . (slotId :: TxMeta -> SlotId) . snd)
    . (case order of
        Ascending -> reverse
        Descending -> id)
    . sortBySlot
    . sortByTxId
  where
    sortBySlot = sortOn (Down . (slotId :: TxMeta -> SlotId) . snd)
    sortByTxId = sortOn (txId . fst)

tip :: Wallet s -> SlotId
tip = (slotId :: BlockHeader -> SlotId) . currentTip
