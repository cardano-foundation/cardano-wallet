{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.DB.Store.UTxOHistory.Store
    ( mkStoreUTxOHistory
    ) where

import Prelude

import Cardano.Wallet.DB.Sqlite.Schema
    ( DeltaUTxOSlots (..)
    , DeltaUTxOValue (..)
    , EntityField (..)
    , Key (..)
    )
import Cardano.Wallet.DB.Sqlite.Types
    ( TxId (..)
    , getTxId
    )
import Cardano.Wallet.DB.Store.UTxOHistory.Model
    ( DeltaUTxOHistory (..)
    , Pruned (..)
    , Spent (..)
    , constrainingAppendBlock
    , constrainingPrune
    , constrainingRollback
    , empty
    , reverseMapOfSets
    )
import Cardano.Wallet.DB.Store.UTxOHistory.Model.Internal
    ( UTxOHistory (..)
    )
import Cardano.Wallet.DB.Store.UTxOHistory.TxOutCBOR
    ( deserializeTxOut
    , serializeTxOut
    )
import Cardano.Wallet.Primitive.Types
    ( WalletId
    , WithOrigin (..)
    )
import Cardano.Wallet.Primitive.Types.Tx.TxIn
    ( TxIn (..)
    )
import Cardano.Wallet.Primitive.Types.Tx.TxOut
    ( TxOut
    )
import Cardano.Wallet.Primitive.Types.UTxO
    ( DeltaUTxO (..)
    , UTxO (..)
    )
import Control.Lens
    ( lazy
    , strict
    , view
    , (<&>)
    )
import Control.Monad.Class.MonadThrow
    ( throwIO
    )
import Data.ByteString
    ( ByteString
    )
import Data.Either.Extra
import Data.Foldable
    ( foldl'
    , forM_
    )
import Data.Maybe
    ( maybeToList
    )
import Data.Store
    ( UpdateStore
    , mkUpdateStore
    , updateLoad
    )
import Database.Persist.Sql
    ( PersistQueryWrite (deleteWhere)
    , SqlPersistT
    , entityVal
    , insertMany_
    , insert_
    , selectFirst
    , selectList
    , updateWhere
    , (!=.)
    , (<=.)
    , (=.)
    , (==.)
    , (>.)
    )
import GHC.Exception
    ( Exception
    , SomeException
    )
import GHC.Exception.Type
    ( toException
    )

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Database.Persist as Sql

-- | Create a 'Store' for 'UTxOHistory' using the given 'WalletId' as a key.
mkStoreUTxOHistory
    :: WalletId
    -> UpdateStore (SqlPersistT IO) DeltaUTxOHistory
mkStoreUTxOHistory wid = mkUpdateStore
    (load wid)
    (write wid)
    (update wid)

-- | Issues with UTxOHistory operations.
data UTxOHistoryError
    = UTxOHistoryNotFound WalletId
    | UTxOHistoryNotDeserializationError WalletId
    deriving (Show, Eq, Exception)

load :: WalletId -> SqlPersistT IO (Either SomeException UTxOHistory)
load wid = do
    slots :: Maybe DeltaUTxOSlots <-
        fmap entityVal
            <$> selectFirst [DeltaUTxOSlotsWallet ==. wid] []
    case slots of
        Nothing -> pure $ Left $ toException $ UTxOHistoryNotFound wid
        Just DeltaUTxOSlots {deltaUTxOSlotsTip, deltaUTxOSlotsFinality} -> do
            xs <-
                fmap entityVal
                    <$> selectList [DeltaUTxOValueWalletId ==. wid] []
            pure
                $ maybeToEither
                    (toException $ UTxOHistoryNotDeserializationError wid)
                $ foldl'
                    patchByRow
                    ( Just $
                        UTxOHistory
                            mempty
                            mempty
                            mempty
                            mempty
                            mempty
                            deltaUTxOSlotsTip
                            deltaUTxOSlotsFinality
                            mempty
                    )
                    xs

patchByRow :: Maybe UTxOHistory -> DeltaUTxOValue -> Maybe UTxOHistory
patchByRow Nothing _ = Nothing
patchByRow
    ( Just
            UTxOHistory
                { history
                , creationSlots
                , creationTxIns
                , spentSlots
                , spentTxIns
                , finality
                , tip
                , boot
                }
        )
    DeltaUTxOValue
        { deltaUTxOValueTxInTx
        , deltaUTxOValueTxInIx
        , deltaUTxOValueTxOut
        , deltaUTxOValueCreation
        , deltaUTxOValueSpent
        , deltaUTxOValueBoot
        } =
        decodeTxOut deltaUTxOValueTxOut <&> \txOut ->
            UTxOHistory
                { history =
                    onNotBoot
                        history
                        (<> (UTxO $ Map.singleton txIn txOut))
                , creationSlots =
                    onNotBoot creationSlots $
                        Map.insertWith (<>) deltaUTxOValueCreation txInSingleton
                , creationTxIns =
                    onNotBoot creationTxIns $
                        Map.insert txIn deltaUTxOValueCreation
                , spentSlots = case deltaUTxOValueSpent of
                    Unspent -> spentSlots
                    Spent slot ->
                        onNotBoot spentSlots $
                            Map.insertWith (<>) slot txInSingleton
                , spentTxIns = case deltaUTxOValueSpent of
                    Unspent -> spentTxIns
                    Spent slot ->
                        onNotBoot spentTxIns $
                            Map.insert txIn slot
                , tip
                , finality
                , boot =
                    boot
                        <> if deltaUTxOValueBoot
                            then UTxO $ Map.singleton txIn txOut
                            else mempty
                }
      where
        txIn = TxIn (getTxId deltaUTxOValueTxInTx) deltaUTxOValueTxInIx
        txInSingleton = Set.singleton txIn
        decodeTxOut = eitherToMaybe . deserializeTxOut . view lazy
        onNotBoot y f = if deltaUTxOValueBoot then y else f y

write :: WalletId -> UTxOHistory -> SqlPersistT IO ()
write
    wid
    ( UTxOHistory
            { history
            , creationSlots
            , spentSlots
            , tip
            , finality
            , boot
            }
        ) = do
        deleteWhere [DeltaUTxOSlotsWallet ==. wid]
        insert_ $ DeltaUTxOSlots wid finality tip
        deleteWhere [DeltaUTxOValueWalletId ==. wid]
        insertMany_ $ do
            (txIn@TxIn {inputId, inputIx}, txOut) <-
                Map.assocs $ unUTxO history
            creation <-
                maybeToList $
                    Map.lookup txIn $
                        reverseMapOfSets creationSlots
            let
                spent = Map.lookup txIn $ reverseMapOfSets spentSlots
            pure $
                DeltaUTxOValue
                    wid
                    creation
                    (maybe Unspent Spent spent)
                    (TxId inputId)
                    inputIx
                    (encodeTxOut txOut)
                    False
        insertMany_ $ do
            (TxIn {inputId, inputIx}, txOut) <-
                Map.assocs $
                    unUTxO boot
            pure $
                DeltaUTxOValue
                    wid
                    Origin
                    Unspent
                    (TxId inputId)
                    inputIx
                    (encodeTxOut txOut)
                    True

encodeTxOut :: TxOut -> ByteString
encodeTxOut = view strict . serializeTxOut

update :: WalletId -> Maybe UTxOHistory -> DeltaUTxOHistory -> SqlPersistT IO ()
update wid = updateLoad (load wid) throwIO updateJust
  where
    updateJust :: UTxOHistory -> DeltaUTxOHistory -> SqlPersistT IO ()
    updateJust old =
        \case
            AppendBlock newTip delta ->
                constrainingAppendBlock (pure ()) old newTip $ do
                    let
                        new = received delta
                        spent = excluded delta
                    insertMany_ $ do
                        (TxIn {inputId, inputIx}, txOut) <-
                            Map.assocs $
                                unUTxO new
                        pure $
                            DeltaUTxOValue
                                wid
                                (At newTip)
                                Unspent
                                (TxId inputId)
                                inputIx
                                (encodeTxOut txOut)
                                False
                    forM_ spent $ \(TxIn txId txIx) ->
                        Sql.update
                            (DeltaUTxOValueKey wid (TxId txId) txIx False)
                            [DeltaUTxOValueSpent =. Spent newTip]
                    updateWhere
                        [DeltaUTxOSlotsWallet ==. wid]
                        [DeltaUTxOSlotsTip =. At newTip]

            Rollback slot -> constrainingRollback (pure ()) old slot $ \case
                Just newTip -> do
                    deleteWhere
                        [ DeltaUTxOValueWalletId ==. wid
                        , DeltaUTxOValueCreation >. newTip
                        , DeltaUTxOValueBoot ==. False
                        ]
                    -- TODO: Add indices to the database schema for slots.
                    let
                        spentFilter = case slot of
                            Origin -> [] -- remove everything
                            At slotNo ->
                                [ DeltaUTxOValueSpent >. Spent slotNo
                                , DeltaUTxOValueSpent !=. Unspent
                                ]
                    updateWhere
                        ( [DeltaUTxOValueBoot ==. False]
                            <> spentFilter
                        )
                        [DeltaUTxOValueSpent =. Unspent]
                    updateWhere
                        [DeltaUTxOSlotsWallet ==. wid]
                        [DeltaUTxOSlotsTip =. newTip]
                Nothing -> do
                    write wid $ empty $ boot old
            Prune slot -> constrainingPrune (pure ()) old slot $ \newFinality ->
                do
                    -- TODO: Add indices to the database schema for slots.
                    deleteWhere
                        [ DeltaUTxOValueWalletId ==. wid
                        , DeltaUTxOValueSpent <=. Spent newFinality
                        , -- , DeltaUTxOValueSpent !=. Unspent
                          DeltaUTxOValueBoot ==. False
                        ]
                    updateWhere
                        [DeltaUTxOSlotsWallet ==. wid]
                        [DeltaUTxOSlotsFinality =. PrunedUpTo newFinality]
