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
{-# LANGUAGE TypeApplications #-}
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
    -- * Model database functions
    , mCleanDB
    , mCreateWallet
    , mRemoveWallet
    , mListWallets
    , mPutCheckpoint
    , mReadCheckpoint
    , mRollbackTo
    , mPutWalletMeta
    , mReadWalletMeta
    , mPutTxHistory
    , mReadTxHistory
    , mPutPrivateKey
    , mReadPrivateKey
    ) where

import Prelude

import Cardano.Wallet.Primitive.Model
    ( Wallet, currentTip )
import Cardano.Wallet.Primitive.Types
    ( BlockHeader (slotId)
    , DefineTx (..)
    , Direction (..)
    , Hash
    , Range (..)
    , SlotId (..)
    , SortOrder (..)
    , Tx
    , TxMeta (..)
    , TxStatus (..)
    , WalletMetadata
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
data Database wid s t xprv = Database
    { wallets :: !(Map wid (WalletDatabase s t xprv))
    -- ^ Wallet-related information.
    , txs :: (Map (Hash "Tx") (Tx t))
    -- ^ In the database, transactions are global and not associated with any
    -- particular wallet.
    } deriving (Generic)

deriving instance (Show (Tx t), Show wid, Show xprv) => Show (Database wid s t xprv)
deriving instance (Eq (Tx t), Eq wid, Eq xprv, Eq s) => Eq (Database wid s t xprv)

-- | Model database record for a single wallet.
data WalletDatabase s t xprv = WalletDatabase
    { checkpoints :: !(Map SlotId (Wallet s t))
    , metadata :: !WalletMetadata
    , txHistory :: !(Map (Hash "Tx") TxMeta)
    , xprv :: !(Maybe xprv)
    } deriving (Show, Eq, Generic)

-- | Shorthand for the putTxHistory argument type.
type TxHistoryMap t = Map (Hash "Tx") (Tx t, TxMeta)

-- | Shorthand for the readTxHistory result type.
type TxHistory t = [(Tx t, TxMeta)]

-- | Produces an empty model database.
emptyDatabase :: Ord wid => Database wid s t xprv
emptyDatabase = Database mempty mempty

{-------------------------------------------------------------------------------
                                  Model Operation Types
-------------------------------------------------------------------------------}

-- | A database model operation, which is a function that takes a database and
-- returns:
--  * a value, which is a query of the database, or an error; and
--  * a (possibly) modified database.
type ModelOp wid s t xprv a =
    Database wid s t xprv -> (Either (Err wid) a, Database wid s t xprv)

-- | All of the possible errors that any of the model database functions might
-- return.
data Err wid
    = NoSuchWallet wid
    | WalletAlreadyExists wid
    deriving (Show, Eq, Functor, Foldable, Traversable)

{-------------------------------------------------------------------------------
                            Model Database Functions
-------------------------------------------------------------------------------}

mCleanDB :: Ord wid => ModelOp wid s t xprv ()
mCleanDB _ = (Right (), emptyDatabase)

mCreateWallet
    :: forall wid s t xprv. (Ord wid, DefineTx t)
    => wid
    -> Wallet s t
    -> WalletMetadata
    -> [(Tx t, TxMeta)]
    -> ModelOp wid s t xprv ()
mCreateWallet wid cp meta txs0 db@Database{wallets,txs}
    | wid `Map.member` wallets = (Left (WalletAlreadyExists wid), db)
    | otherwise =
        let
            wal = WalletDatabase (Map.singleton (tip cp) cp) meta history Nothing
            txs' = Map.fromList $ (\(tx, _) -> (txId @t tx, tx)) <$> txs0
            history = Map.fromList $ first (txId @t) <$> txs0
        in
            (Right (), Database (Map.insert wid wal wallets) (txs <> txs'))

mRemoveWallet :: Ord wid => wid -> ModelOp wid s t xprv ()
mRemoveWallet wid db@Database{wallets,txs}
    | wid `Map.member` wallets =
        (Right (), Database (Map.delete wid wallets) txs)
    | otherwise = (Left (NoSuchWallet wid), db)

mListWallets :: Ord wid => ModelOp wid s t xprv [wid]
mListWallets db@(Database wallets _) = (Right (sort $ Map.keys wallets), db)

mPutCheckpoint
    :: Ord wid => wid -> Wallet s t -> ModelOp wid s t xprv ()
mPutCheckpoint wid cp = alterModel wid $ \wal ->
    ((), wal { checkpoints = Map.insert (tip cp) cp (checkpoints wal) })

mReadCheckpoint
    :: Ord wid => wid -> ModelOp wid s t xprv (Maybe (Wallet s t))
mReadCheckpoint wid db@(Database wallets _) =
    (Right (Map.lookup wid wallets >>= mostRecentCheckpoint), db)
  where
    mostRecentCheckpoint :: WalletDatabase s t xprv -> Maybe (Wallet s t)
    mostRecentCheckpoint = fmap snd . Map.lookupMax . checkpoints

mRollbackTo :: Ord wid => wid -> SlotId -> ModelOp wid s t xprv ()
mRollbackTo wid point db = flip (alterModel wid) db $ \wal ->
    let
        nearest = findNearestPoint (Map.elems $ checkpoints wal)
    in
        ( ()
        , wal
            { checkpoints =
                Map.filter ((<= point) . tip) (checkpoints wal)
            , txHistory =
                Map.mapMaybe (rescheduleOrForget nearest) (txHistory wal)
            }
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

    -- | Find nearest checkpoint's slot before or equal to 'point'
    findNearestPoint :: [Wallet s t] -> SlotId
    findNearestPoint = head . sortOn Down . mapMaybe fn
      where
        fn :: Wallet s t -> Maybe SlotId
        fn cp = if (tip cp <= point) then Just (tip cp) else Nothing

mPutWalletMeta :: Ord wid => wid -> WalletMetadata -> ModelOp wid s t xprv ()
mPutWalletMeta wid meta = alterModel wid $ \wal ->
    ((), wal { metadata = meta })

mReadWalletMeta :: Ord wid => wid -> ModelOp wid s t xprv (Maybe WalletMetadata)
mReadWalletMeta wid db@(Database wallets _) =
    (Right (metadata <$> Map.lookup wid wallets), db)

mPutTxHistory
    :: forall wid s t xprv. (Ord wid, DefineTx t)
    => wid
    -> TxHistory t
    -> ModelOp wid s t xprv ()
mPutTxHistory wid txList db@Database{wallets,txs} =
    case Map.lookup wid wallets of
        Just wal ->
            ( Right ()
            , Database (Map.insert wid wal' wallets) (txs <> txs')
            )
          where
            wal' = wal { txHistory = txHistory wal <> txHistory' }
            txHistory' = Map.fromList $ first (txId @t) <$> txList
            txs' = Map.fromList $ (\(tx, _) -> (txId @t tx, tx)) <$> txList
        Nothing -> (Left (NoSuchWallet wid), db)

mReadTxHistory
    :: forall wid s t xprv. (Ord wid, DefineTx t)
    => wid
    -> SortOrder
    -> Range SlotId
    -> Maybe TxStatus
    -> ModelOp wid s t xprv (TxHistory t)
mReadTxHistory wid order range mstatus db@Database{wallets,txs} = (Right res, db)
  where
    res = maybe mempty getTxs $ Map.lookup wid wallets
    getTxs wal = filterTxHistory @t order range $ catMaybes
            [ fmap (, meta) (Map.lookup tid txs)
            | (tid, meta) <- Map.toList (txHistory wal)
            , case mstatus of
                Nothing -> True
                Just s -> status meta == s
            ]

mPutPrivateKey :: Ord wid => wid -> xprv -> ModelOp wid s t xprv ()
mPutPrivateKey wid pk = alterModel wid $ \wal ->
    ((), wal { xprv = Just pk })

mReadPrivateKey :: Ord wid => wid -> ModelOp wid s t xprv (Maybe xprv)
mReadPrivateKey wid db@(Database wallets _) =
    (Right (Map.lookup wid wallets >>= xprv), db)

{-------------------------------------------------------------------------------
                             Model function helpers
-------------------------------------------------------------------------------}

alterModel
    :: Ord wid
    => wid
    -> (WalletDatabase s t xprv -> (a, WalletDatabase s t xprv))
    -> ModelOp wid s t xprv a
alterModel wid f db@Database{wallets,txs} = case f <$> Map.lookup wid wallets of
    Just (a, wal) -> (Right a, Database (Map.insert wid wal wallets) txs)
    Nothing -> (Left (NoSuchWallet wid), db)

-- | Apply optional filters on slotId and sort using the default sort order
-- (first time/slotId, then by TxId) to a 'TxHistory'.
filterTxHistory
    :: forall t. DefineTx t
    => SortOrder
    -> Range SlotId
    -> TxHistory t
    -> TxHistory t
filterTxHistory order range =
    filter ((`isWithinRange` range) . (slotId :: TxMeta -> SlotId) . snd)
    . (case order of
        Ascending -> reverse
        Descending -> id)
    . sortBySlot
    . sortByTxId
  where
    sortBySlot = sortOn (Down . (slotId :: TxMeta -> SlotId) . snd)
    sortByTxId = sortOn (txId @t . fst)

tip :: Wallet s t -> SlotId
tip = (slotId :: BlockHeader -> SlotId) . currentTip
