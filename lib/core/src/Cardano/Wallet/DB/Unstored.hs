{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}


-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- An implementation of the DBLayer which uses Persistent and SQLite.
module Cardano.Wallet.DB.Unstored
  ( ErrInitializeGenesisAbsent (..)
  , ErrRollbackTo (..)
  , deleteTxMetas
  , updateTxMetas
  , deleteLooseTransactions
  , selectWallet
  , deleteDelegationCertificates
  , pruneLocalTxSubmission
  , updatePendingTxForExpiryQuery
  , deleteStakeKeyCerts
  , localTxSubmissionFromEntity
  , listPendingLocalTxSubmissionQuery
  , selectPrivateKey
  , selectGenesisParameters
  , mkPrivateKeyEntity
  , mkWalletMetadataUpdate
  , mkWalletEntity
  , readWalletDelegation
  , readWalletMetadata
  )
where

import Prelude

import Cardano.Address.Derivation
    ( XPrv )
import Cardano.DB.Sqlite
    ( dbChunkedFor )
import Cardano.Wallet.DB
    ( ErrNoSuchWallet (ErrNoSuchWallet) )
import Cardano.Wallet.DB.Sqlite.Schema
    ( DelegationCertificate (..)
    , EntityField (..)
    , LocalTxSubmission (..)
    , PrivateKey (..)
    , StakeKeyCertificate (..)
    , TxMeta (..)
    , Wallet (..)
    )
import Cardano.Wallet.DB.Sqlite.Types
    ( BlockId (..), TxId (..) )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..), PersistPrivateKey (..) )
import Cardano.Wallet.Primitive.Passphrase
    ( PassphraseHash )
import Cardano.Wallet.Primitive.Slotting
    ( TimeInterpreter, firstSlotInEpoch, interpretQuery )
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Cardano.Wallet.Primitive.Types.Hash as W
import qualified Cardano.Wallet.Primitive.Types.Tx as W
import Control.Exception
    ( Exception (toException), throw )
import Control.Monad.Exception.Unchecked
    ( Unchecked (Unchecked) )
import Control.Monad.IO.Class
    ( MonadIO (..) )
import Data.Coerce
    ( coerce )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.Maybe
    ( catMaybes )
import Data.Quantity
    ( Quantity (..) )
import Data.Word
    ( Word32 )
import Database.Persist.Class
    ( toPersistValue )
import Database.Persist.Sql
    ( Entity (..)
    , Filter
    , SelectOpt (..)
    , Single (..)
    , SqlPersistT
    , Update (..)
    , deleteWhere
    , rawExecute
    , rawSql
    , selectFirst
    , selectList
    , updateWhere
    , (<-.)
    , (<.)
    , (<=.)
    , (=.)
    , (==.)
    , (>=.)
    )

import qualified Cardano.Wallet.Primitive.Passphrase as W
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Cardano.Wallet.Primitive.Types.Hash as W
import qualified Cardano.Wallet.Primitive.Types.Tx as W

readWalletMetadata
    :: W.WalletId
    -> W.WalletDelegation
    -> SqlPersistT IO (Maybe W.WalletMetadata)
readWalletMetadata wid walDel =
     fmap (metadataFromEntity walDel . entityVal)
        <$> selectFirst [WalId ==. wid] []

readWalletDelegation
    :: TimeInterpreter IO
    -> W.WalletId
    -> W.EpochNo
    -> SqlPersistT IO W.WalletDelegation
readWalletDelegation ti wid epoch
    | epoch == 0 = pure $ W.WalletDelegation W.NotDelegating []
    | otherwise = do
        (eMinus1, e) <- liftIO $ interpretQuery ti $
            (,) <$> firstSlotInEpoch (epoch - 1) <*> firstSlotInEpoch epoch
        active <- maybe W.NotDelegating toWalletDelegationStatus
            <$> readDelegationCertificate wid
                [ CertSlot <. eMinus1
                ]

        next <- catMaybes <$> sequence
            [ fmap (W.WalletDelegationNext (epoch + 1) . toWalletDelegationStatus)
                <$> readDelegationCertificate wid
                    [ CertSlot >=. eMinus1
                    , CertSlot <. e
                    ]
            , fmap (W.WalletDelegationNext (epoch + 2) . toWalletDelegationStatus)
                <$> readDelegationCertificate wid
                    [ CertSlot >=. e
                    ]
            ]

        pure $ W.WalletDelegation active next

readDelegationCertificate
    :: W.WalletId
    -> [Filter DelegationCertificate]
    -> SqlPersistT IO (Maybe DelegationCertificate)
readDelegationCertificate wid filters = fmap entityVal
    <$> selectFirst ((CertWalletId ==. wid) : filters) [Desc CertSlot]

{-------------------------------------------------------------------------------
    Conversion between types
        from the `persistent` database (Cardano.Wallet.DB.Sqlite.TH)
        and from the wallet core ( Cardano.Wallet.Primitive.Types.*)
-------------------------------------------------------------------------------}

toWalletDelegationStatus
    :: DelegationCertificate
    -> W.WalletDelegationStatus
toWalletDelegationStatus = \case
    DelegationCertificate _ _ Nothing ->
        W.NotDelegating
    DelegationCertificate _ _ (Just pool) ->
        W.Delegating pool

mkWalletEntity
    :: W.WalletId
    -> W.WalletMetadata
    -> W.GenesisParameters
    -> Wallet
mkWalletEntity wid meta gp = Wallet
    { walId = wid
    , walName = meta ^. #name . coerce
    , walCreationTime = meta ^. #creationTime
    , walPassphraseLastUpdatedAt = W.lastUpdatedAt <$> meta ^. #passphraseInfo
    , walPassphraseScheme = W.passphraseScheme <$> meta ^. #passphraseInfo
    , walGenesisHash = BlockId (coerce (gp ^. #getGenesisBlockHash))
    , walGenesisStart = coerce (gp ^. #getGenesisBlockDate)
    }

mkWalletMetadataUpdate :: W.WalletMetadata -> [Update Wallet]
mkWalletMetadataUpdate meta =
    [ WalName =. meta ^. #name . coerce
    , WalCreationTime =. meta ^. #creationTime
    , WalPassphraseLastUpdatedAt =.
        W.lastUpdatedAt <$> meta ^. #passphraseInfo
    , WalPassphraseScheme =.
        W.passphraseScheme <$> meta ^. #passphraseInfo
    ]

metadataFromEntity :: W.WalletDelegation -> Wallet -> W.WalletMetadata
metadataFromEntity walDelegation wal = W.WalletMetadata
    { name = W.WalletName (walName wal)
    , creationTime = walCreationTime wal
    , passphraseInfo = W.WalletPassphraseInfo
        <$> walPassphraseLastUpdatedAt wal
        <*> walPassphraseScheme wal
    , delegation = walDelegation
    }

mkPrivateKeyEntity
    :: PersistPrivateKey (k 'RootK)
    => W.WalletId
    -> (k 'RootK XPrv, W.PassphraseHash)
    -> PrivateKey
mkPrivateKeyEntity wid kh = PrivateKey
    { privateKeyWalletId = wid
    , privateKeyRootKey = root
    , privateKeyHash = hash
    }
  where
    (root, hash) = serializeXPrv kh

privateKeyFromEntity
    :: PersistPrivateKey (k 'RootK)
    => PrivateKey
    -> (k 'RootK XPrv, PassphraseHash)
privateKeyFromEntity (PrivateKey _ k h) =
    unsafeDeserializeXPrv (k, h)


genesisParametersFromEntity
    :: Wallet
    -> W.GenesisParameters
genesisParametersFromEntity (Wallet _ _ _ _ _ hash startTime) =
    W.GenesisParameters
        { W.getGenesisBlockHash = coerce (getBlockId hash)
        , W.getGenesisBlockDate = W.StartTime startTime
        }

{-------------------------------------------------------------------------------
    SQLite database operations
-------------------------------------------------------------------------------}

overWallet
    :: MonadIO m
    => W.WalletId
    -> (Wallet -> SqlPersistT m b)
    -> SqlPersistT m b
overWallet wid f = selectWallet wid >>= \case
        Nothing -> throw
            $ toException
            $ Unchecked
            $ ErrNoSuchWallet wid
        Just x -> f x

selectWallet :: MonadIO m => W.WalletId -> SqlPersistT m (Maybe Wallet)
selectWallet wid =
    fmap entityVal <$> selectFirst [WalId ==. wid] []

-- | Delete TxMeta values for a wallet.
deleteTxMetas
    :: W.WalletId
    -> [Filter TxMeta]
    -> SqlPersistT IO ()
deleteTxMetas wid filters =
    deleteWhere ((TxMetaWalletId ==. wid) : filters)

-- | Delete stake key certificates for a wallet.
deleteStakeKeyCerts
    :: W.WalletId
    -> [Filter StakeKeyCertificate]
    -> SqlPersistT IO ()
deleteStakeKeyCerts wid filters =
    deleteWhere ((StakeKeyCertWalletId ==. wid) : filters)

updateTxMetas
    :: W.WalletId
    -> [Filter TxMeta]
    -> [Update TxMeta]
    -> SqlPersistT IO ()
updateTxMetas wid filters =
    updateWhere ((TxMetaWalletId ==. wid) : filters)


-- | Delete transactions that aren't referred to by TxMeta of any wallet.
deleteLooseTransactions :: SqlPersistT IO ()
deleteLooseTransactions = do
    deleteLoose "tx_in"
    deleteLoose "tx_out"
    deleteLoose "tx_withdrawal"
  where
    -- Deletes all TxIn/TxOuts/TxWithdrawal returned by the sub-select.
    -- The sub-select outer joins TxMeta with TxIn/TxOut/TxWithdrawal.
    -- All rows of the join table TxMeta as NULL are loose (unreferenced)
    -- transactions.
    deleteLoose t = flip rawExecute [] $
        "DELETE FROM "<> t <>" WHERE tx_id IN (" <>
            "SELECT "<> t <>".tx_id FROM "<> t <>" " <>
            "LEFT OUTER JOIN tx_meta ON tx_meta.tx_id = "<> t <>".tx_id " <>
            "WHERE (tx_meta.tx_id IS NULL))"

-- | Delete all delegation certificates matching the given filter
deleteDelegationCertificates
    :: W.WalletId
    -> [Filter DelegationCertificate]
    -> SqlPersistT IO ()
deleteDelegationCertificates wid filters = do
    deleteWhere ((CertWalletId ==. wid) : filters)




-- | Returns the initial submission slot and submission record for all pending
-- transactions in the wallet.
listPendingLocalTxSubmissionQuery
    :: W.WalletId
    -> SqlPersistT IO [(W.SlotNo, LocalTxSubmission)]
listPendingLocalTxSubmissionQuery wid = fmap unRaw <$> rawSql query params
  where
    -- fixme: sort results
    query =
        "SELECT tx_meta.slot,?? " <>
        "FROM tx_meta INNER JOIN local_tx_submission " <>
        "ON tx_meta.wallet_id=local_tx_submission.wallet_id " <>
        "    AND tx_meta.tx_id=local_tx_submission.tx_id " <>
        "WHERE tx_meta.wallet_id=? AND tx_meta.status=? " <>
        "ORDER BY local_tx_submission.wallet_id, local_tx_submission.tx_id"
    params = [toPersistValue wid, toPersistValue W.Pending]
    unRaw (Single sl, Entity _ tx) = (sl, tx)

localTxSubmissionFromEntity
    :: (W.SlotNo, LocalTxSubmission)
    -> W.LocalTxSubmissionStatus W.SealedTx
localTxSubmissionFromEntity (sl0, LocalTxSubmission (TxId txid) _ sl tx) =
    W.LocalTxSubmissionStatus txid tx sl0 sl

-- | Remove transactions from the local submission pool once they can no longer
-- be rolled back.
pruneLocalTxSubmission
    :: W.WalletId
    -> Quantity "block" Word32
    -> W.BlockHeader
    -> SqlPersistT IO ()
pruneLocalTxSubmission wid (Quantity epochStability) tip =
    rawExecute query params
  where
    query =
        "DELETE FROM local_tx_submission " <>
        "WHERE wallet_id=? AND tx_id IN " <>
        "( SELECT tx_id FROM tx_meta WHERE tx_meta.block_height < ? )"
    params = [toPersistValue wid, toPersistValue stableHeight]
    stableHeight = getQuantity (tip ^. #blockHeight) - epochStability

-- | Mutates all pending transaction entries which have exceeded their TTL so
-- that their status becomes expired. Then it removes these transactions from
-- the local tx submission pool.
--
-- Transaction expiry is not something which can be rolled back.
updatePendingTxForExpiryQuery
    :: W.WalletId
    -> W.SlotNo
    -> SqlPersistT IO ()
updatePendingTxForExpiryQuery wid tip = do
    txIds <- fmap (txMetaTxId . entityVal) <$> selectList isExpired []
    updateWhere isExpired [TxMetaStatus =. W.Expired]
    -- Remove these transactions from the wallet's local submission pool.
    dbChunkedFor @LocalTxSubmission (\batch -> deleteWhere
        [ LocalTxSubmissionWalletId ==. wid
        , LocalTxSubmissionTxId <-. batch
        ]) txIds
  where
    isExpired =
        [ TxMetaWalletId ==. wid
        , TxMetaStatus ==. W.Pending
        , TxMetaSlotExpires <=. Just tip ]

deleteTxLocal
    :: W.WalletId
    -> [TxId]
    -> SqlPersistT IO ()
deleteTxLocal wid = do
    dbChunkedFor @LocalTxSubmission (\batch -> deleteWhere
        [ LocalTxSubmissionWalletId ==. wid
        , LocalTxSubmissionTxId <-. batch
        ])
selectPrivateKey
    :: (MonadIO m, PersistPrivateKey (k 'RootK))
    => W.WalletId
    -> SqlPersistT m (Maybe (k 'RootK XPrv, PassphraseHash))
selectPrivateKey wid = do
    keys <- selectFirst [PrivateKeyWalletId ==. wid] []
    pure $ (privateKeyFromEntity . entityVal) <$> keys

selectGenesisParameters
    :: MonadIO m
    => W.WalletId
    -> SqlPersistT m (Maybe W.GenesisParameters)
selectGenesisParameters wid = do
    gp <- selectFirst [WalId ==. wid] []
    pure $ (genesisParametersFromEntity . entityVal) <$> gp

{-------------------------------------------------------------------------------
    Internal errors
-------------------------------------------------------------------------------}
-- | A fatal exception thrown when trying to rollback but, there's no checkpoint
-- to rollback to. The database maintain the invariant that there's always at
-- least one checkpoint (the first one made for genesis) present in the
-- database.
--
-- If we don't find any checkpoint, it means that this invariant has been
-- violated.
data ErrRollbackTo = ErrNoOlderCheckpoint W.WalletId W.Slot deriving (Show)
instance Exception ErrRollbackTo

-- | Can't initialize a wallet because the given 'BlockHeader' is not genesis.
data ErrInitializeGenesisAbsent
    = ErrInitializeGenesisAbsent W.WalletId W.BlockHeader deriving (Eq, Show)

instance Exception ErrInitializeGenesisAbsent
