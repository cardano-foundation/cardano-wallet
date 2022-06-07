
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Wallet.DB.Transactions.Transaction.Store where

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
import Cardano.Wallet.DB.Transactions.Transaction.Model
    ( DeltaTxHistory (..)
    , TxHistory
    , TxHistoryF (TxHistoryF)
    , TxRelationF (..)
    , tokenCollateralOrd
    , tokenOutOrd
    )
import Control.Arrow
    ( Arrow ((&&&)) )
import Control.Exception
    ( SomeException )
import Data.DBVar
    ( Store (Store, loadS, updateS, writeS) )
import Data.Foldable
    ( fold, forM_, toList )
import Data.Functor.Identity
    ( Identity (Identity, runIdentity) )
import Data.List
    ( sortOn )
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
    , insertMany_
    , selectList
    , (==.)
    )

import qualified Data.Map.Strict as Map

mkStoreTransactions :: Store (SqlPersistT IO) DeltaTxHistory
mkStoreTransactions =
    Store
        { loadS = load,
          writeS = write,
          updateS = update
        }

update :: TxHistory -> DeltaTxHistory -> SqlPersistT IO ()
update _ change = case change of
    ExpandTxHistory txs -> putTxHistory txs
    DeleteTxHistory tid -> do
        deleteWhere [TxInputTxId ==. tid]
        deleteWhere [TxCollateralTxId ==. tid]
        deleteWhere [TxOutTokenTxId  ==. tid]
        deleteWhere [TxOutputTxId ==. tid]
        deleteWhere [TxCollateralOutTokenTxId ==. tid]
        deleteWhere [TxCollateralOutTxId ==. tid]
        deleteWhere [TxWithdrawalTxId ==. tid]

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

load :: SqlPersistT IO (Either SomeException TxHistory)
load = Right <$> selectTxHistory

-- | Insert multiple transactions
putTxHistory :: TxHistory -> SqlPersistT IO ()
putTxHistory (TxHistoryF tx_map) = forM_ tx_map $ \TxRelationF{..} -> do
    insertMany_ $ runIdentity <$> txRelation_ins
    insertMany_ $ runIdentity <$> txRelation_colls
    insertMany_ $ fst <$> txRelation_outs
    insertMany_ $ txRelation_outs >>= snd
    insertMany_ $ maybeToList $ fst <$> txRelation_collouts
    insertMany_ $ maybeToList (txRelation_collouts) >>= snd
    insertMany_   txRelation_withdraws


-- This relies on available information from the database to reconstruct coin
-- selection information for __outgoing__ payments. We can't however guarantee
-- that we have such information for __incoming__ payments (we usually don't
-- have it).
--
-- To reliably provide this information for incoming payments, it should be
-- looked up when applying blocks from the global ledger, but that is future
-- work.
--
-- See also: issue #573.

-- select a txRelation starting from the list of meta
selectTxHistory ::
    SqlPersistT IO TxHistory
selectTxHistory = TxHistoryF  <$> select
  where
    select :: SqlPersistT IO (Map TxId (TxRelationF Identity))
    select = do
        inputs <- mkMap txInputTxId $ selectList [] []
        collaterals <- mkMap txCollateralTxId $ selectList [] []
        outputs <- mkMap txOutputTxId $ selectList [] []
        collateralOutputs <- fmap (fmap getFirst) 
            $ mkMap txCollateralOutTxId $ selectList [] []
        withdrawals <- mkMap txWithdrawalTxId $ selectList [] []
        outTokens <- mkMap txOutTokenTxId $ selectList [ ] []
        collateralTokens <- mkMap txCollateralOutTokenTxId $ selectList [] []
        let ids = fold
                [ Map.keysSet inputs
                , Map.keysSet collaterals
                , Map.keysSet outputs
                , Map.keysSet collateralOutputs
                , Map.keysSet withdrawals
                , Map.keysSet outTokens
                , Map.keysSet collateralTokens
                ]
            selectOutTokens :: TxId -> TxOut -> [TxOutToken]
            selectOutTokens txId txOut =
                filter (\token -> txOutTokenTxIndex token == txOutputIndex txOut )
                $ Map.findWithDefault [] txId outTokens
            selectCollateralTokens
                :: TxId
                -> TxCollateralOut
                -> [TxCollateralOutToken]
            selectCollateralTokens txId _
                = Map.findWithDefault [] txId collateralTokens
        pure $ mconcat $ do
            k <- toList ids
            pure $ Map.singleton k $ TxRelationF
                { txRelation_ins = fmap Identity
                    $ sortOn txInputOrder
                    $ Map.findWithDefault [] k inputs
                , txRelation_colls = fmap Identity
                    $ sortOn txCollateralOrder
                    $ Map.findWithDefault [] k collaterals
                , txRelation_outs = fmap (fmap $ sortOn tokenOutOrd)
                    $ sortOn (txOutputIndex . fst)
                    $ (id &&& selectOutTokens k)
                    <$> Map.findWithDefault [] k outputs
                , txRelation_collouts = fmap (fmap $ sortOn tokenCollateralOrd)
                    $ (id &&& selectCollateralTokens k)
                    <$> Map.findWithDefault Nothing k collateralOutputs
                , txRelation_withdraws = sortOn txWithdrawalAccount  
                    $ Map.findWithDefault [] k withdrawals
                }

mkMap :: (Ord k, Functor f, Applicative g, Semigroup (g b))
    => (b -> k)
    -> f [Entity b]
    -> f (Map k (g b) )
mkMap k v = Map.fromListWith (<>) . fmap ((k &&& pure ) . entityVal ) <$> v
