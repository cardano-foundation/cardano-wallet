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
    ( getTxId )
import Cardano.Wallet.DB.Store.UTxOHistory.Model
    ( DeltaUTxOHistory (..), Spent (..) )
import Cardano.Wallet.DB.Store.UTxOHistory.Model.Internal
    ( UTxOHistory (..) )
import Cardano.Wallet.DB.Store.UTxOHistory.TxOutCBOR
    ( deserializeTxOut )
import Cardano.Wallet.Primitive.Types
    ( WalletId )
import Cardano.Wallet.Primitive.Types.Tx.TxIn
    ( TxIn (TxIn) )
import Cardano.Wallet.Primitive.Types.UTxO
    ( UTxO (..) )
import Control.Lens
    ( lazy, view, (<&>) )
import Data.Either.Extra
    ( eitherToMaybe, maybeToEither )
import Data.Foldable
    ( foldl' )
import Data.Store
    ( UpdateStore, mkUpdateStore )
import Database.Persist.Sql
    ( SqlPersistT, entityVal, selectFirst, selectList, (==.) )
import GHC.Exception
    ( Exception, SomeException )
import GHC.Exception.Type
    ( toException )

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

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
write _wid = undefined

update :: WalletId -> Maybe UTxOHistory -> DeltaUTxOHistory -> SqlPersistT IO ()
update _wid = undefined
