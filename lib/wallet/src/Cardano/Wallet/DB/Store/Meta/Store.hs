
{-# LANGUAGE LambdaCase #-}

{- |
 Copyright: © 2018-2022 IOHK
 License: Apache-2.0

Low level 'Store' for a collection of meta-transactions,
i.e. additional data ('TxMeta') that the wallet stores for each transaction.
Meta-transactions are specific to a wallet.

-}
module Cardano.Wallet.DB.Store.Meta.Store ( mkStoreMetaTransactions ) where

import Prelude

import Cardano.Wallet.DB.Sqlite.Schema
    ( EntityField (..), TxMeta (..) )
import Cardano.Wallet.DB.Store.Meta.Model
    ( DeltaTxMetaHistory (..)
    , ManipulateTxMetaHistory (..)
    , TxMetaHistory (..)
    )
import Cardano.Wallet.Primitive.Types
    ( WalletId )
import Control.Arrow
    ( (&&&) )
import Control.Exception
    ( SomeException )
import Control.Monad
    ( void )
import Data.DBVar
    ( Store (Store, loadS, updateS, writeS) )
import Data.Foldable
    ( Foldable (toList) )
import Data.List.Split
    ( chunksOf )
import Data.Maybe
    ( fromJust )
import Database.Persist.Sql
    ( Entity (entityVal)
    , PersistEntity (keyFromRecordM)
    , PersistQueryRead (selectFirst)
    , SqlPersistT
    , deleteWhere
    , deleteWhereCount
    , repsertMany
    , selectList
    , updateWhere
    , (<-.)
    , (<=.)
    , (=.)
    , (==.)
    , (>.)
    )

import qualified Cardano.Wallet.Primitive.Types.Tx as W
import qualified Data.Map.Strict as Map

-- | Create an SQL store to hold meta transactions for a wallet.
mkStoreMetaTransactions :: WalletId
    -> Store (SqlPersistT IO) DeltaTxMetaHistory
mkStoreMetaTransactions
    wid = Store { loadS = load wid, writeS = write wid, updateS = update wid }

update :: WalletId
    -> Maybe TxMetaHistory
    -> DeltaTxMetaHistory
    -> SqlPersistT IO ()
update wid _ change = case change of
    Expand txs -> putMetas txs
    Manipulate (PruneTxMetaHistory tid) -> do
        let filt = [TxMetaWalletId ==. wid, TxMetaTxId ==. tid]
        selectFirst ((TxMetaStatus ==. W.InLedger) : filt) [] >>= \case
            Just _ -> pure () -- marked in ledger - refuse to delete
            Nothing -> void
                $ deleteWhereCount
                $ (TxMetaStatus <-. [W.Pending, W.Expired]) : filt
    Manipulate (AgeTxMetaHistory tip) -> updateWhere
        [ TxMetaWalletId ==. wid
        , TxMetaStatus ==. W.Pending
        , TxMetaSlotExpires <=. Just tip
        ]
        [TxMetaStatus =. W.Expired]
    Manipulate (RollBackTxMetaHistory point) -> do
        let isAfter = TxMetaSlot >. point
        deleteWhere
            [ TxMetaWalletId ==. wid
            , isAfter
            ]

write :: WalletId -> TxMetaHistory -> SqlPersistT IO ()
write wid txs = do
    deleteWhere [TxMetaWalletId ==. wid]
    putMetas txs

load :: WalletId
    -> SqlPersistT IO (Either SomeException TxMetaHistory)
load wid =
    Right
    . TxMetaHistory
    . Map.fromList
    . fmap ((txMetaTxId &&& id) . entityVal)
    <$> selectList [TxMetaWalletId ==. wid] []

-- | Insert multiple meta-transactions, overwriting the previous version in
-- case of the same transaction index.
-- Only one meta-transaction can be stored per transaction for a given wallet.
putMetas :: TxMetaHistory -> SqlPersistT IO ()
putMetas (TxMetaHistory metas) =
    chunked repsertMany [(fromJust keyFromRecordM x, x) | x <- toList metas]
    where
        -- needed to submit large numberot transactions
        chunked f xs = mapM_ f (chunksOf 1000 xs)
