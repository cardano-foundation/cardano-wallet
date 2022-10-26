{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Copyright: 2022 IOHK
License: Apache-2.0

Implementation of a 'Store' for 'TxSet'.

-}
module Cardano.Wallet.DB.Store.Transactions.Store
    ( selectTxSet
    , putTxSet
    , mkStoreTransactions
    ) where

import Prelude

import Cardano.Wallet.DB.Sqlite.Schema
    ( CBOR (..)
    , EntityField (..)
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
    ( DeltaTxSet (..)
    , TxRelation (..)
    , TxSet (..)
    , tokenCollateralOrd
    , tokenOutOrd
    )
import Control.Arrow
    ( Arrow ((&&&)) )
import Data.DBVar
    ( Store (..) )
import Data.Foldable
    ( fold, toList )
import Data.Functor
    ( (<&>) )
import Data.List
    ( sortOn )
import Data.List.Split
    ( chunksOf )
import Data.Map.Strict
    ( Map )
import Data.Maybe
    ( maybeToList )
import Data.Monoid
    ( First (..), getFirst )
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
    :: Store (SqlPersistT IO) DeltaTxSet
mkStoreTransactions =
    Store
    { loadS = Right <$> selectTxSet
    , writeS = write
    , updateS = update
    }

update :: TxSet -> DeltaTxSet -> SqlPersistT IO ()
update _ change = case change of
    Append txs -> putTxSet txs
    DeleteTx tid -> do
        deleteWhere [TxInputTxId ==. tid ]
        deleteWhere [TxCollateralTxId ==. tid ]
        deleteWhere [TxOutTokenTxId ==. tid ]
        deleteWhere [TxOutputTxId ==. tid ]
        deleteWhere [TxCollateralOutTokenTxId ==. tid ]
        deleteWhere [TxCollateralOutTxId ==. tid ]
        deleteWhere [TxWithdrawalTxId ==. tid ]
        deleteWhere [CborTxId ==. tid ]

write :: TxSet -> SqlPersistT IO ()
write txs = do
    deleteWhere @_ @_ @TxIn mempty
    deleteWhere @_ @_ @TxCollateral mempty
    deleteWhere @_ @_ @TxOutToken mempty
    deleteWhere @_ @_ @TxOut mempty
    deleteWhere @_ @_ @TxCollateralOutToken mempty
    deleteWhere @_ @_ @TxCollateralOut mempty
    deleteWhere @_ @_ @TxWithdrawal mempty
    deleteWhere @_ @_ @CBOR mempty
    putTxSet txs

-- | Insert multiple transactions.
putTxSet:: TxSet -> SqlPersistT IO ()
putTxSet (TxSet tx_map) = do
    repsertMany' $ tx_list >>= ins
    repsertMany' $ tx_list >>= collateralIns
    repsertMany' $ (tx_list >>= outs) <&> fst
    repsertMany' $ tx_list >>= outs >>= snd
    repsertMany' $ (tx_list >>= maybeToList . collateralOuts) <&> fst
    repsertMany' $ (tx_list >>= maybeToList . collateralOuts) >>= snd
    repsertMany' $ tx_list >>= withdrawals
    repsertMany' $ tx_list >>= maybeToList . cbor

  where
    tx_list = toList tx_map
    repsertMany' xs = let
        Just f = keyFromRecordM
        in chunked repsertMany [(f x, x) | x <- xs]
    -- needed to submit large number of elements
    chunked f xs = mapM_ f (chunksOf 1000 xs)

-- | Select transactions from the database.
selectTxSet :: SqlPersistT IO TxSet
selectTxSet = TxSet <$> select
  where
    selectListAll = selectList [] []
    orEmpty = Map.findWithDefault []
    select :: SqlPersistT IO (Map TxId TxRelation)
    select = do
        inputs <- mkMap txInputTxId selectListAll
        collaterals <- mkMap txCollateralTxId selectListAll
        outputs <- mkMap txOutputTxId selectListAll
        collateralOutputs
            <- fmap getFirst <$> mkMap txCollateralOutTxId selectListAll
        widths <- mkMap txWithdrawalTxId selectListAll
        outTokens <- mkMap txOutTokenTxId selectListAll
        collateralTokens <- mkMap txCollateralOutTokenTxId selectListAll
        cbors :: Map TxId (First  CBOR) <- mkMap cborTxId selectListAll
        let ids =
                fold
                    [Map.keysSet inputs
                   , Map.keysSet collaterals
                   , Map.keysSet outputs
                   , Map.keysSet collateralOutputs
                   , Map.keysSet widths
                   , Map.keysSet outTokens
                   , Map.keysSet collateralTokens
                    ]
            selectOutTokens :: TxId -> TxOut -> [TxOutToken]
            selectOutTokens txId txOut = filter
                (\token -> txOutTokenTxIndex token == txOutputIndex txOut)
                $ orEmpty txId outTokens
            selectCollateralTokens
                :: TxId -> TxCollateralOut -> [TxCollateralOutToken]
            selectCollateralTokens txId _ = orEmpty txId collateralTokens
        pure $ mconcat $ do
            k <- toList ids
            pure
                $ Map.singleton k
                $ TxRelation
                { ins = sortOn txInputOrder $ orEmpty k inputs
                , collateralIns = sortOn txCollateralOrder
                      $ orEmpty k collaterals
                , outs = fmap (fmap $ sortOn tokenOutOrd)
                      $ sortOn (txOutputIndex . fst)
                      $ (id &&& selectOutTokens k)
                      <$> orEmpty k outputs
                , collateralOuts = fmap (fmap $ sortOn tokenCollateralOrd)
                      $ (id &&& selectCollateralTokens k)
                      <$> Map.findWithDefault Nothing k collateralOutputs
                , withdrawals = sortOn txWithdrawalAccount $ orEmpty k widths
                , cbor = getFirst $ Map.findWithDefault (First Nothing) k cbors
                }

mkMap :: (Ord k, Functor f, Applicative g, Semigroup (g b))
    => (b -> k)
    -> f [Entity b]
    -> f (Map k (g b))
mkMap k v =
    Map.fromListWith (<>)
    . fmap ((k &&& pure) . entityVal)
    <$> v
