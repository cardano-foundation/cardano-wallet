{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
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
    , updateTxSet
    , mkStoreTransactions

    , selectTx
    , selectTxOut
    , selectTxCollateralOut
    ) where

import Prelude

import Cardano.Wallet.DB.Sqlite.Schema
    ( CBOR (..)
    , CBOR (..)
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
    , DeltaTxSet
    , TxRelation (..)
    , TxSet (..)
    , tokenCollateralOrd
    , tokenOutOrd
    )
import Control.Arrow
    ( Arrow ((&&&)) )
import Control.Monad.Reader
    ( MonadIO, ReaderT )
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
    ( listToMaybe, maybeToList )
import Data.Monoid
    ( First (..), getFirst )
import Data.Store
    ( UpdateStore, mkUpdateStore )
import Data.Word
    ( Word32 )
import Database.Persist.Sql
    ( BaseBackend
    , Entity
    , PersistEntity (PersistEntityBackend)
    , PersistQueryRead
    , SqlPersistT
    , count
    , deleteWhere
    , entityVal
    , keyFromRecordM
    , repsertMany
    , selectFirst
    , selectList
    , (<-.)
    , (==.)
    )

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

mkStoreTransactions
    :: UpdateStore (SqlPersistT IO) DeltaTxSet
mkStoreTransactions =
    mkUpdateStore load write update
  where
    load = Right <$> selectTxSet
    update = const updateTxSet

updateTxSet :: DeltaTxSet -> SqlPersistT IO ()
updateTxSet change = case change of
    Append txs -> putTxSet txs
    DeleteTxs tidSet -> do
        let tids = Set.toList tidSet
        deleteWhere [TxInputTxId <-. tids ]
        deleteWhere [TxCollateralTxId <-. tids ]
        deleteWhere [TxOutTokenTxId <-. tids ]
        deleteWhere [TxOutputTxId <-. tids ]
        deleteWhere [TxCollateralOutTokenTxId <-. tids ]
        deleteWhere [TxCollateralOutTxId <-. tids ]
        deleteWhere [TxWithdrawalTxId <-. tids ]
        deleteWhere [CborTxId <-. tids ]

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
    repsertMany' xs =
        case keyFromRecordM of
            Just f -> chunked repsertMany [(f x, x) | x <- xs]
            Nothing -> error "keyFromRecordM is Nothing"

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
        withs <- mkMap txWithdrawalTxId selectListAll
        outTokens <- mkMap txOutTokenTxId selectListAll
        collateralTokens <- mkMap txCollateralOutTokenTxId selectListAll
        cbors <- mkMap cborTxId selectListAll
        let ids =
                fold
                    [Map.keysSet inputs
                   , Map.keysSet collaterals
                   , Map.keysSet outputs
                   , Map.keysSet collateralOutputs
                   , Map.keysSet withs
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
                , withdrawals = sortOn txWithdrawalAccount $ orEmpty k withs
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

-- | Select one transaction from the database.
selectTx :: TxId -> SqlPersistT IO (Maybe (Either TxRelation CBOR))
selectTx k = select
  where
    selectK
        :: (MonadIO m, PersistEntityBackend record ~ BaseBackend backend
            , PersistQueryRead backend, PersistEntity record)
        => EntityField record TxId
        -> ReaderT backend m [record]
    selectK f = fmap entityVal <$> selectList [f ==. k] []
    select :: SqlPersistT IO (Maybe (Either TxRelation CBOR))
    select = do
        mcbor <- selectK CborTxId
        case mcbor of
            [] -> fmap (fmap Left) $ do
                inputs <- selectK TxInputTxId
                collaterals <- selectK TxCollateralTxId
                outputs <- selectK TxOutputTxId
                collateralOutputs <-  selectK TxCollateralOutTxId
                withds <- selectK TxWithdrawalTxId
                outTokens <- selectK TxOutTokenTxId
                collateralTokens <- selectK TxCollateralOutTokenTxId
                let
                    selectOutTokens :: TxOut -> [TxOutToken]
                    selectOutTokens txOut = filter
                        (\token -> txOutTokenTxIndex token == txOutputIndex txOut)
                        outTokens
                pure $ Just $ TxRelation
                        { ins = sortOn txInputOrder inputs
                        , collateralIns = sortOn txCollateralOrder collaterals
                        , outs = fmap (fmap $ sortOn tokenOutOrd)
                            $ sortOn (txOutputIndex . fst)
                            $ (id &&& selectOutTokens)
                            <$> outputs
                        , collateralOuts = listToMaybe collateralOutputs
                            <&> (, sortOn tokenCollateralOrd collateralTokens)
                        , withdrawals = sortOn txWithdrawalAccount withds
                        , cbor = listToMaybe mcbor
                        }
            cbor' : _ -> pure $ Just $ Right cbor'

-- | Select one regular output from the database.
selectTxOut :: (TxId, Word32) -> SqlPersistT IO (Maybe (TxOut, [TxOutToken]))
selectTxOut (txid, index) = do
    moutput <- fmap entityVal <$> selectFirst
        [TxOutputTxId ==. txid, TxOutputIndex ==. index] []
    case moutput of
        Nothing -> pure Nothing
        Just output -> do
            outTokens <- map entityVal <$> selectList
                [TxOutTokenTxId ==. txid, TxOutTokenTxIndex ==. index] []
            pure $ Just (output, outTokens)

-- | Select the collateral output of a transaction from the database,
-- if it has the correct index.
selectTxCollateralOut
    :: (TxId, Word32)
    -> SqlPersistT IO (Maybe (TxCollateralOut, [TxCollateralOutToken]))
selectTxCollateralOut (txid, index) = do
    noutputs <- count [TxOutputTxId ==. txid]
    let collateralOutputIndex = toEnum noutputs
    if index /= collateralOutputIndex -- Babbage ledger spec
    then pure Nothing
    else do
        mcollateralOutput <- fmap entityVal <$> selectFirst
            [TxCollateralOutTxId ==. txid] []
        case mcollateralOutput of
            Nothing -> pure Nothing
            Just collateralOutput -> do
                collateralTokens <- fmap entityVal <$> selectList
                    [TxCollateralOutTokenTxId ==. txid] []
                pure $ Just (collateralOutput,collateralTokens)
