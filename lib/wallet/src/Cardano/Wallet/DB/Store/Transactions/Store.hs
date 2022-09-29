{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Copyright: 2022 IOHK
License: Apache-2.0

Implementation of a 'Store' for 'TxHistory'.

-}
module Cardano.Wallet.DB.Store.Transactions.Store
    ( selectTxHistory
    , putTxHistory
    , mkStoreTransactions ) where

import Prelude

import Cardano.Wallet.DB.Sqlite.Schema
    ( EntityField (..)
    , TxCollateral (..)
    , TxCollateralOut (..)
    , TxCollateralOutToken (..)
    , TxIn (..)
    , TxOut (..)
    , TxOutToken (..)
    , TxWithdrawal (..)
    )
import Cardano.Wallet.DB.Sqlite.Types
    ( TxId )
import Cardano.Wallet.DB.Store.Transactions.Model
    ( DeltaTxHistory (..)
    , TxHistory (..)
    , TxRelation (..)
    , tokenCollateralOrd
    , tokenOutOrd
    )
import Control.Arrow
    ( Arrow ((&&&)) )
import Data.DBVar
    ( Store (..) )
import Data.Foldable
    ( fold, forM_, toList )
import Data.List
    ( sortOn )
import Data.List.Split
    ( chunksOf )
import Data.Map.Strict
    ( Map )
import Data.Maybe
    ( maybeToList )
import Data.Monoid
    ( getFirst )
import Database.Persist.Sql
    ( Entity
    , SqlPersistT
    , deleteWhere
    , entityVal
    , keyFromRecordM
    , repsertMany
    , selectList
    , (==.)
    )

import qualified Data.Map.Strict as Map

mkStoreTransactions
    :: Store (SqlPersistT IO) DeltaTxHistory
mkStoreTransactions =
    Store
    { loadS = Right <$> selectTxHistory
    , writeS = write
    , updateS = update
    }

update :: TxHistory -> DeltaTxHistory -> SqlPersistT IO ()
update _ change = case change of
    Append txs -> putTxHistory txs
    DeleteTx tid -> do
        deleteWhere [TxInputTxId ==. tid ]
        deleteWhere [TxCollateralTxId ==. tid ]
        deleteWhere [TxOutTokenTxId ==. tid ]
        deleteWhere [TxOutputTxId ==. tid ]
        deleteWhere [TxCollateralOutTokenTxId ==. tid ]
        deleteWhere [TxCollateralOutTxId ==. tid ]
        deleteWhere [TxWithdrawalTxId ==. tid ]

write :: TxHistory -> SqlPersistT IO ()
write txs = do
    deleteWhere @_ @_ @TxIn mempty
    deleteWhere @_ @_ @TxCollateral mempty
    deleteWhere @_ @_ @TxOutToken mempty
    deleteWhere @_ @_ @TxOut mempty
    deleteWhere @_ @_ @TxCollateralOutToken mempty
    deleteWhere @_ @_ @TxCollateralOut mempty
    deleteWhere @_ @_ @TxWithdrawal mempty
    putTxHistory txs


-- | Insert multiple transactions
putTxHistory :: TxHistory -> SqlPersistT IO ()
putTxHistory (TxHistory tx_map) = forM_ tx_map $ \TxRelation {..} -> do
    repsertMany' ins
    repsertMany' collateralIns
    repsertMany' $ fst <$> outs
    repsertMany' $ outs >>= snd
    repsertMany' $ maybeToList $ fst <$> collateralOuts
    repsertMany' $ maybeToList (collateralOuts) >>= snd
    repsertMany' withdrawals

    where
        repsertMany' xs = let
            Just f = keyFromRecordM
            in chunked repsertMany [(f x, x) | x <- xs]
        -- needed to submit large numberot transactions
        chunked f xs = mapM_ f (chunksOf 1000 xs)

-- | Select transactions history from the database
selectTxHistory :: SqlPersistT IO TxHistory
selectTxHistory = TxHistory <$> select
  where
    selectListAll = selectList [] []
    select :: SqlPersistT IO (Map TxId TxRelation)
    select = do
        inputs <- mkMap txInputTxId selectListAll
        collaterals <- mkMap txCollateralTxId selectListAll
        outputs <- mkMap txOutputTxId selectListAll
        collateralOutputs
            <- fmap getFirst <$> mkMap txCollateralOutTxId selectListAll
        withdrawals <- mkMap txWithdrawalTxId selectListAll
        outTokens <- mkMap txOutTokenTxId selectListAll
        collateralTokens <- mkMap txCollateralOutTokenTxId selectListAll
        let ids =
                fold
                    [Map.keysSet inputs
                   , Map.keysSet collaterals
                   , Map.keysSet outputs
                   , Map.keysSet collateralOutputs
                   , Map.keysSet withdrawals
                   , Map.keysSet outTokens
                   , Map.keysSet collateralTokens
                    ]
            selectOutTokens :: TxId -> TxOut -> [TxOutToken]
            selectOutTokens txId txOut =
                filter
                    (\token -> txOutTokenTxIndex token == txOutputIndex txOut)
                $ Map.findWithDefault [] txId outTokens
            selectCollateralTokens
                :: TxId -> TxCollateralOut -> [TxCollateralOutToken]
            selectCollateralTokens txId _ =
                Map.findWithDefault [] txId collateralTokens
        pure $ mconcat $ do
            k <- toList ids
            pure
                $ Map.singleton k
                $ TxRelation
                { ins = sortOn txInputOrder $ Map.findWithDefault [] k inputs
                , collateralIns = sortOn txCollateralOrder
                      $ Map.findWithDefault [] k collaterals
                , outs = fmap (fmap $ sortOn tokenOutOrd)
                      $ sortOn (txOutputIndex . fst)
                      $ (id &&& selectOutTokens k)
                      <$> Map.findWithDefault [] k outputs
                , collateralOuts = fmap (fmap $ sortOn tokenCollateralOrd)
                      $ (id &&& selectCollateralTokens k)
                      <$> Map.findWithDefault Nothing k collateralOutputs
                , withdrawals = sortOn txWithdrawalAccount
                      $ Map.findWithDefault [] k withdrawals
                }

mkMap :: (Ord k, Functor f, Applicative g, Semigroup (g b))
    => (b -> k)
    -> f [Entity b]
    -> f (Map k (g b))
mkMap k v =
    Map.fromListWith (<>)
    . fmap ((k &&& pure) . entityVal)
    <$> v
