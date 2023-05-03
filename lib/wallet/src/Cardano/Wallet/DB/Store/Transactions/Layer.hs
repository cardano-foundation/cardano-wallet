{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Copyright: © 2022 IOHK
License: Apache-2.0

Implementation of a 'QueryStore' for 'TxSet'.

-}
module Cardano.Wallet.DB.Store.Transactions.Layer
    ( QueryTxSet (..)
    , mkQueryStoreTxSet
    ) where

import Prelude

import Cardano.Wallet.DB.Sqlite.Schema
    ( CBOR )
import Cardano.Wallet.DB.Sqlite.Types
    ( TxId )
import Cardano.Wallet.DB.Store.Transactions.Model
    ( DeltaTxSet, TxRelation (..), TxSet (..), fromTxCollateralOut, fromTxOut )
import Control.Applicative
    ( (<|>) )
import Data.Maybe
    ( maybeToList )
import Data.Store
    ( Query (..), Store, mkQueryStore )
import Data.Word
    ( Word32 )
import Database.Persist.Sql
    ( SqlPersistT )
import Safe
    ( atMay )

import qualified Cardano.Wallet.DB.Store.Transactions.Store as TxSet
import qualified Cardano.Wallet.Primitive.Types.Tx.TxOut as W
import qualified Data.Map.Strict as Map

{-----------------------------------------------------------------------------
    DB for 'TxSet'
------------------------------------------------------------------------------}
data QueryTxSet b where
    GetByTxId :: TxId -> QueryTxSet (Maybe (Either TxRelation CBOR))
    GetTxOut :: (TxId, Word32) -> QueryTxSet (Maybe W.TxOut)

-- | Implementation of a 'QueryStore' for 'TxSet'.
mkQueryStoreTxSet :: Store (SqlPersistT IO) QueryTxSet DeltaTxSet
mkQueryStoreTxSet =
    mkQueryStore query' TxSet.mkStoreTransactions
  where
    query' :: forall b. QueryTxSet b -> (SqlPersistT IO) b
    query' = \case
        GetByTxId txid -> TxSet.selectTx txid
        GetTxOut key -> do
            mout <- TxSet.selectTxOut key
            mcollateralOut <- TxSet.selectTxCollateralOut key
            pure $
                    (fromTxOut <$> mout)
                <|> (fromTxCollateralOut <$> mcollateralOut)

instance Query QueryTxSet where
    type World QueryTxSet = TxSet
    query = flip runQuery

runQuery :: TxSet -> QueryTxSet b -> b
runQuery (TxSet txs) = \case
    GetByTxId txid -> Left <$> Map.lookup txid txs
    GetTxOut (txid,index) -> do
        tx <- Map.lookup txid txs
        let outputs
                = (fromTxOut <$> outs tx)
                <> maybeToList (fromTxCollateralOut <$> collateralOuts tx)
                -- Babbage spec:
                -- The collateral output has index (length outs)
        outputs `atMay` (fromEnum index)
