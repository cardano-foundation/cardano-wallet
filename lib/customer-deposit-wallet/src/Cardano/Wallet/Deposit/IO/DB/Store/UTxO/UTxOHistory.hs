{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -Wno-orphans #-}

{- |
Copyright: © 2024 Cardano Foundation
License: Apache-2.0

-}
module Cardano.Wallet.Deposit.IO.DB.Store.UTxO.UTxOHistory
    ( mkStoreUTxOHistory
    )
    where

import Prelude

import Cardano.Wallet.Deposit.Pure.UTxO.UTxO
    ( UTxO
    )
import Cardano.Wallet.Deposit.Pure.UTxO.UTxOHistory
    ( DeltaUTxOHistory (..)
    , Pruned (..)
    , UTxOHistory (..)
    )
import "customer-deposit-wallet-pure" Cardano.Wallet.Deposit.Read
    ( Slot
    , SlotNo
    , TxId
    , TxIn
    , TxOut (..)
    )
import "customer-deposit-wallet-pure" Cardano.Wallet.Deposit.Read.Value
    ( injectCoin
    )
import Control.Exception
    ( Exception
    , SomeException
    )
import Data.ByteString
    ( ByteString
    )
import Data.Delta
    ( Delta (..)
    )
import Data.List
    ( foldl'
    )
import Data.Maybe
    ( fromJust
    )
import Data.Proxy
    ( Proxy (..)
    )
import Data.Store
    ( SimpleStore
    , UpdateStore
    , loadS
    , loadWhenNothing
    , mkSimpleStore
    , mkUpdateStore
    , writeS
    )
import Database.Table
    ( Col (..)
    , IsColumnName
    , Row
    , Table
    , (:.)
    )

import qualified Cardano.Wallet.Deposit.Pure.UTxO.DeltaUTxO as DeltaUTxO
import qualified Cardano.Wallet.Deposit.Pure.UTxO.UTxO as UTxO
import qualified "customer-deposit-wallet-pure" Cardano.Wallet.Deposit.Read as Read
import qualified Control.Exception as E
import qualified Data.Map.Strict as Map
import qualified Data.Maps.Timeline as Timeline
import qualified Data.Set as Set
import qualified Database.SQLite.Simple.FromField as Sqlite
import qualified Database.SQLite.Simple.ToField as Sqlite
import qualified Database.Table.SQLite.Simple as Sql

{-----------------------------------------------------------------------------
    Helper types
    TODO: Move out
------------------------------------------------------------------------------}

type TxIx = Read.Ix

mkTxIn :: TxId -> TxIx -> Read.TxIn
mkTxIn a b = (a,b)

type TxOutCBOR = Int

deserializeTxOut :: TxOutCBOR -> TxOut
deserializeTxOut _ = TxOutC mempty (injectCoin 10)

serializeTxOut :: TxOut -> TxOutCBOR
serializeTxOut _ = 10

instance Sql.IsColumn TxOut where
    getSqlType _ = Sql.getSqlType (Proxy :: Proxy ByteString)

instance Sqlite.ToField TxOut where
    toField = Sqlite.toField . serializeTxOut . undefined

instance Sqlite.FromField TxOut where
    fromField = (undefined . deserializeTxOut <$>) . Sqlite.fromField

instance Sql.IsColumn (Maybe SlotNo) where
    getSqlType _ = Sql.getSqlType (Proxy :: Proxy (Maybe Int))

instance Sqlite.ToField SlotNo where
    toField = Sqlite.toField . fromEnum

instance Sqlite.FromField SlotNo where
    fromField = (toEnum <$>) . Sqlite.fromField

instance Sql.IsColumn (Read.WithOrigin SlotNo) where
    getSqlType _ = Sql.getSqlType (Proxy :: Proxy Int)

instance Sqlite.ToField (Read.WithOrigin SlotNo) where
    toField = Sqlite.toField . toInt
      where
        toInt Read.Origin = -1
        toInt (Read.At slotNo) = fromEnum slotNo

instance Sqlite.FromField (Read.WithOrigin SlotNo) where
    fromField = (fromInt <$>) . Sqlite.fromField
      where
        fromInt (-1) = Read.Origin
        fromInt slotNo = Read.At (toEnum slotNo)

instance Sql.IsColumn Pruned where
    getSqlType _ = Sql.getSqlType (Proxy :: Proxy Int)

instance Sqlite.ToField Pruned where
    toField = Sqlite.toField . toInt
      where
        toInt NotPruned = -2
        toInt (PrunedUpTo slotNo) = fromEnum slotNo

instance Sqlite.FromField Pruned where
    fromField = (fromInt <$>) . Sqlite.fromField
      where
        fromInt (-2) = NotPruned
        fromInt slotNo = PrunedUpTo (toEnum slotNo)

{-----------------------------------------------------------------------------
    Store wrapper
    TODO: Move out
------------------------------------------------------------------------------}

{- | Catch exceptions in the 'SqlM' monad that are related to failures
of reading from or writing to the database.

This includes:

* Exceptions that are specific to using SQLite:
    'Sqlite.FormatErrorSource',
    'Sqlite.ResultError',
    'Sqlite.SQLError'.
* Exceptions that may arise when trying to convert data:
    'E.ArithException',
    'E.AssertionFailed',
    'E.ErrorCall',
    'E.PatternMatchFail',
    'E.Void'.
-}
catchSqlM :: forall a. Sql.SqlM a -> (SomeException -> Sql.SqlM a) -> Sql.SqlM a
catchSqlM action handler = action
    {-
    E.catches action
        [ E.Handler (\(e :: Sqlite.FormatErrorSource) -> handle e)
        , E.Handler (\(e :: Sqlite.ResultError) -> handle e)
        , E.Handler (\(e :: Sqlite.SQLError) -> handle e)
        , E.Handler (\(e :: E.ArithException) -> handle e)
        , E.Handler (\(e :: E.AssertionFailed) -> handle e)
        , E.Handler (\(e :: E.ErrorCall) -> handle e)
        , E.Handler (\(e :: E.PatternMatchFail) -> handle e)
        , E.Handler (\(e :: E.Void) -> handle e)
        ]
    -}
  where
    handle :: Exception e => e -> Sql.SqlM a
    handle = undefined . handler . E.toException

trySqlM :: Sql.SqlM a -> Sql.SqlM (Either SomeException a)
trySqlM action = (Right <$> action) `catchSqlM` (pure . Left)

unTrySqlM :: Either SomeException a -> Sql.SqlM a
unTrySqlM (Left e) = undefined e
unTrySqlM (Right a) = pure a

{-----------------------------------------------------------------------------
    Delta type
    TODO: Move out
------------------------------------------------------------------------------}

instance Delta DeltaUTxOHistory where
    type Base DeltaUTxOHistory = UTxOHistory
    apply _ = id

{-----------------------------------------------------------------------------
    Database schema
------------------------------------------------------------------------------}

type TableUTxOHistory =
    Table "utxo_history_slots"
    :. Col "tip" Slot
    :. Col "finality" Pruned

type TableTimelineUTxO =
    Table "utxo_history_timeline"
    :. Col "txin_id" TxId
    :. Col "txin_ix" TxIx
    :. Col "txout" TxOut
    :. Col "created" Slot
    :. Col "spent" (Maybe SlotNo)

colCreated :: Col "created" Slot
colCreated = Col

colSpent :: Col "spent" (Maybe SlotNo)
colSpent = Col

type TableUTxO name =
    Table name
    :. Col "txin_id" TxId
    :. Col "txin_ix" TxIx
    :. Col "txout" TxOut

type TableBoot =
    TableUTxO "utxo_history_boot"

tableUTxOHistory :: Proxy TableUTxOHistory
tableUTxOHistory = Proxy

tableTimelineUTxO :: Proxy TableTimelineUTxO
tableTimelineUTxO = Proxy

tableBoot :: Proxy TableBoot
tableBoot = Proxy

{-----------------------------------------------------------------------------
    Store
    UTxO
------------------------------------------------------------------------------}
mkStoreUTxO
    :: IsColumnName name
    => proxy (TableUTxO name) -> SimpleStore Sql.SqlM UTxO
mkStoreUTxO t = mkSimpleStore (loadUTxO t) (writeUTxO t)

loadUTxO
    :: IsColumnName name
    => proxy (TableUTxO name) -> Sql.SqlM (Either SomeException UTxO)
loadUTxO tableUTxO =
    trySqlM (Map.fromList . map toPair <$> Sql.selectAll tableUTxO)
  where
    toPair (txid,ix,out) = (mkTxIn txid ix, out)

writeUTxO
    :: IsColumnName name
    => proxy (TableUTxO name) -> UTxO -> Sql.SqlM ()
writeUTxO tableUTxO utxo = do
    Sql.deleteAll tableUTxO
    Sql.insertMany (map fromPair $ Map.toList utxo) tableUTxO
  where
    fromPair ((txid, txix), txout) = (txid, txix, txout)

{-----------------------------------------------------------------------------
    Store
    UTxOHistory
------------------------------------------------------------------------------}

mkStoreUTxOHistory :: UpdateStore Sql.SqlM DeltaUTxOHistory
mkStoreUTxOHistory = mkUpdateStore loadS' writeS' updateS'

data ErrStoreUTxOHistory
    = ErrStoreUTxOHistory
    deriving Show

instance Exception ErrStoreUTxOHistory

loadS' :: Sql.SqlM (Either SomeException UTxOHistory)
loadS' = trySqlM $ do
    [(tip, finality)] <- Sql.selectAll tableUTxOHistory
    boot <- unTrySqlM =<< loadS (mkStoreUTxO tableBoot)
    rows <- Sql.selectAll tableTimelineUTxO
    let utxo0 = UTxOHistory
            { history = mempty
            , created = Timeline.empty
            , spent = Timeline.empty
            , tip
            , finality
            , boot
            }
    pure $ foldl' insertRow utxo0 rows

insertRow :: UTxOHistory -> Row TableTimelineUTxO -> UTxOHistory
insertRow u (txid, txix, txout, create, spend) = u
    { history = Map.insert txin txout (history u)
    , spent = case spend of
        Nothing -> spent u
        Just slotNo -> Timeline.insert slotNo txin (spent u)
    , created = Timeline.insert create txin (created u)
    }
  where
    txin = mkTxIn txid txix

writeS' :: UTxOHistory -> Sql.SqlM ()
writeS' u@UTxOHistory{boot,tip,finality} = do
    Sql.deleteAll tableTimelineUTxO
    let txins = Map.keys (history u)
    Sql.insertMany (map (mkRow u) txins) tableTimelineUTxO
    Sql.deleteAll tableUTxOHistory
    Sql.insertOne (tip, finality) tableUTxOHistory
    writeS (mkStoreUTxO tableBoot) boot

mkRow :: UTxOHistory -> TxIn -> Row TableTimelineUTxO
mkRow UTxOHistory{history,created,spent} txin@(txid, txix) =
    (txid, txix, txout, create, spend)
  where
    txout =
        fromJust (error "mkRow: txin should exist")
        $ Map.lookup txin history
    create =
        fromJust (error "mkRow: create slot should exist")
        $ Timeline.lookupByItem txin created
    spend =
        Timeline.lookupByItem txin spent

updateS'
    :: Maybe UTxOHistory
    -> DeltaUTxOHistory
    -> Sql.SqlM ()
updateS' mu0 du@(AppendBlock _ deltaUTxO) = do
    u0 <- loadWhenNothing mu0 mkStoreUTxOHistory
    let u1 = apply du u0

    let txins =
            UTxO.dom (DeltaUTxO.received deltaUTxO)
            <> DeltaUTxO.excluded deltaUTxO
    Sql.insertMany
        (map (mkRow u1) (Set.toList txins))
        tableTimelineUTxO

    Sql.deleteAll tableUTxOHistory
    Sql.insertOne (tip u1, finality u1) tableUTxOHistory

updateS' mu0 du@(Rollback slot) = do
    -- FIXME: Finality
    u0 <- loadWhenNothing mu0 mkStoreUTxOHistory
    let u1 = apply du u0

    Sql.deleteWhere
        (colCreated Sql.>. slot)
        tableTimelineUTxO
    case slot of
        Read.Origin ->
            Sql.updateWhere
                Sql.true
                [colSpent Sql.=. Nothing]
                tableTimelineUTxO
        Read.At slotNo ->
            Sql.updateWhere
                (colSpent Sql.>. Just slotNo)
                [colSpent Sql.=. Nothing]
                tableTimelineUTxO

    Sql.deleteAll tableUTxOHistory
    Sql.insertOne (tip u1, finality u1) tableUTxOHistory

updateS' mu0 du@(Prune newFinality) = do
    u0 <- loadWhenNothing mu0 mkStoreUTxOHistory
    let u1 = apply du u0

    Sql.deleteWhere
        (colCreated Sql.<=. Read.At newFinality
            Sql.&&. colSpent Sql.<=. Just newFinality
            Sql.&&. colSpent Sql./=. Nothing
        )
        tableTimelineUTxO

    Sql.deleteAll tableUTxOHistory
    Sql.insertOne (tip u1, finality u1) tableUTxOHistory
