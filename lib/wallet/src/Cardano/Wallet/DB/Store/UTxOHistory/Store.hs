{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.DB.Store.UTxOHistory.Store
    ( mkStoreUTxOHistory
    ) where

import Prelude

import Cardano.Wallet.DB.Sqlite.Schema
    ( DeltaUTxOSlots (..), DeltaUTxOValue (..), EntityField (..) )
import Cardano.Wallet.DB.Sqlite.Types
    ( TxId (..), getTxId )
import Cardano.Wallet.DB.Store.UTxOHistory.Model
    ( DeltaUTxOHistory (..), Spent (..), reverseMapOfSets )
import Cardano.Wallet.DB.Store.UTxOHistory.Model.Internal
    ( UTxOHistory (..) )
import Cardano.Wallet.DB.Store.UTxOHistory.TxOutCBOR
    ( deserializeTxOut, serializeTxOut )
import Cardano.Wallet.Primitive.Types
    ( WalletId, WithOrigin (..) )
import Cardano.Wallet.Primitive.Types.Tx.TxIn
    ( TxIn (..) )
import Cardano.Wallet.Primitive.Types.Tx.TxOut
    ( TxOut )
import Cardano.Wallet.Primitive.Types.UTxO
    ( UTxO (..) )
import Control.Lens
    ( lazy, strict, view, (<&>) )
import Data.ByteString
    ( ByteString )
import Data.Either.Extra
import Data.Foldable
    ( foldl' )
import qualified Data.Map.Strict as Map
import Data.Maybe
    ( maybeToList )
import qualified Data.Set as Set
import Data.Store
    ( UpdateStore, mkUpdateStore )
import Database.Persist.Sql
    ( PersistQueryWrite (deleteWhere)
    , SqlPersistT
    , entityVal
    , insertMany_
    , insert_
    , selectFirst
    , selectList
    , (==.)
    )
import GHC.Exception
    ( Exception, SomeException )
import GHC.Exception.Type
    ( toException )

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
update _wid = undefined
