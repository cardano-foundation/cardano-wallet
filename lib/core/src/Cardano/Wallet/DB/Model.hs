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
    , Hash
    , Range (..)
    , SlotId (..)
    , SortOrder (..)
    , Tx
    , TxMeta (slotId)
    , WalletMetadata
    , isWithinRange
    )
import Data.List
    ( sort, sortOn )
import Data.Map.Strict
    ( Map )
import Data.Maybe
    ( catMaybes, listToMaybe )
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
    { checkpoints :: ![Wallet s t]
    , metadata :: !WalletMetadata
    , txHistory :: !(Map (Hash "Tx") TxMeta)
    , xprv :: !(Maybe xprv)
    } deriving (Show, Eq, Generic)

-- | Shorthand for the putTxHistory argument type.
type TxHistoryMap t = Map (Hash "Tx") (Tx t, TxMeta)

-- | Shorthand for the readTxHistory result type.
type TxHistory t = [(Hash "Tx", (Tx t, TxMeta))]

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

mCreateWallet :: Ord wid => wid -> Wallet s t -> WalletMetadata -> ModelOp wid s t xprv ()
mCreateWallet wid cp meta db@Database{wallets,txs}
    | wid `Map.member` wallets = (Left (WalletAlreadyExists wid), db)
    | otherwise =
        ( Right ()
        , Database (Map.insert wid (WalletDatabase [cp] meta mempty Nothing) wallets) txs
        )

mRemoveWallet :: Ord wid => wid -> ModelOp wid s t xprv ()
mRemoveWallet wid db@Database{wallets,txs}
    | wid `Map.member` wallets =
        (Right (), Database (Map.delete wid wallets) txs)
    | otherwise = (Left (NoSuchWallet wid), db)

mListWallets :: Ord wid => ModelOp wid s t xprv [wid]
mListWallets db@(Database wallets _) = (Right (sort $ Map.keys wallets), db)

mPutCheckpoint :: Ord wid => wid -> Wallet s t -> ModelOp wid s t xprv ()
mPutCheckpoint wid cp = alterModel wid $ \wal ->
    ((), wal { checkpoints = (cp:checkpoints wal) })

mReadCheckpoint :: Ord wid => wid -> ModelOp wid s t xprv (Maybe (Wallet s t))
mReadCheckpoint wid db@(Database wallets _) =
    (Right (checkpoints <$> Map.lookup wid wallets >>= listToMaybe), db)

mRollbackTo :: Ord wid => wid -> SlotId -> ModelOp wid s t xprv ()
mRollbackTo wid pt = alterModel wid $ \wal ->
    ((), wal { checkpoints = filter keepBeforePoint (checkpoints wal) })
  where
    keepBeforePoint cp = (slotId :: BlockHeader -> SlotId) (currentTip cp) <= pt

mPutWalletMeta :: Ord wid => wid -> WalletMetadata -> ModelOp wid s t xprv ()
mPutWalletMeta wid meta = alterModel wid $ \wal ->
    ((), wal { metadata = meta })

mReadWalletMeta :: Ord wid => wid -> ModelOp wid s t xprv (Maybe WalletMetadata)
mReadWalletMeta wid db@(Database wallets _) =
    (Right (metadata <$> Map.lookup wid wallets), db)

mPutTxHistory :: Ord wid => wid -> TxHistoryMap t -> ModelOp wid s t xprv ()
mPutTxHistory wid txList db@Database{wallets,txs} =
    case Map.lookup wid wallets of
        Just wal -> (Right (), Database (Map.insert wid wal' wallets) txs')
          where
            wal' = wal { txHistory = txHistory wal <> Map.map snd txList }
            txs' = txs <> Map.map fst txList
        Nothing -> (Left (NoSuchWallet wid), db)

mReadTxHistory
    :: forall wid s t xprv.
       (Ord wid)
    => wid
    -> SortOrder
    -> Range SlotId
    -> ModelOp wid s t xprv (TxHistory t)
mReadTxHistory wid order range db@Database{wallets,txs} = (Right res, db)
  where
    res = maybe mempty getTxs $ Map.lookup wid wallets
    getTxs wal = filterTxHistory @t order range $ catMaybes
            [ fmap (\tx -> (txId, (tx, meta))) (Map.lookup txId txs)
            | (txId, meta) <- Map.toList (txHistory wal) ]

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
filterTxHistory :: SortOrder -> Range SlotId -> TxHistory t -> TxHistory t
filterTxHistory order range =
    filter ((`isWithinRange` range) . (slotId :: TxMeta -> SlotId) . snd . snd)
    . (case order of
        Ascending -> reverse
        Descending -> id)
    . sortBySlot
    . sortByTxId
  where
    sortBySlot = sortOn (Down . (slotId :: TxMeta -> SlotId) . snd . snd)
    sortByTxId = sortOn fst
