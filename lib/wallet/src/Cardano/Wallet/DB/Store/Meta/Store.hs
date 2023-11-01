{-# LANGUAGE TypeApplications #-}

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
    ( EntityField (..)
    , TxMeta (..)
    )
import Cardano.Wallet.DB.Store.Meta.Model
    ( DeltaTxMetaHistory (..)
    , TxMetaHistory (..)
    )
import Control.Arrow
    ( (&&&)
    )
import Control.Exception
    ( SomeException
    )
import Data.Foldable
    ( Foldable (toList)
    )
import Data.List.Split
    ( chunksOf
    )
import Data.Maybe
    ( fromJust
    )
import Data.Store
    ( UpdateStore
    , mkUpdateStore
    )
import Database.Persist.Sql
    ( Entity (entityVal)
    , PersistEntity (keyFromRecordM)
    , SqlPersistT
    , deleteWhere
    , repsertMany
    , selectList
    , (>.)
    )

import qualified Data.Map.Strict as Map

-- | Create an SQL store to hold meta transactions for a wallet.
mkStoreMetaTransactions
    :: UpdateStore (SqlPersistT IO) DeltaTxMetaHistory
mkStoreMetaTransactions
    = mkUpdateStore load write update

update
    :: Maybe TxMetaHistory
    -> DeltaTxMetaHistory
    -> SqlPersistT IO ()
update _ change = case change of
    Expand txs -> putMetas txs
    Rollback point -> do
        let isAfter = TxMetaSlot >. point
        deleteWhere
            [ isAfter
            ]

write :: TxMetaHistory -> SqlPersistT IO ()
write txs = do
    deleteWhere @_ @_ @TxMeta []
    putMetas txs

load ::SqlPersistT IO (Either SomeException TxMetaHistory)
load =
    Right
    . TxMetaHistory
    . Map.fromList
    . fmap ((txMetaTxId &&& id) . entityVal)
    <$> selectList [] []

-- | Insert multiple meta-transactions, overwriting the previous version in
-- case of the same transaction index.
-- Only one meta-transaction can be stored per transaction for a given wallet.
putMetas :: TxMetaHistory -> SqlPersistT IO ()
putMetas (TxMetaHistory metas) =
    chunked repsertMany [(fromJust keyFromRecordM x, x) | x <- toList metas]
    where
        -- needed to submit large numberot transactions
        chunked f xs = mapM_ f (chunksOf 1000 xs)
