{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

{- HLINT ignore "Redundant flip" -}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- An implementation of the DBLayer which uses Persistent and SQLite.

module Cardano.Wallet.DB.Sqlite
    ( newDBLayer
    , newDBFactory
    , findDatabases
    , withDBLayer

    -- * Interfaces
    , PersistState (..)

    -- * Migration Support
    , DefaultFieldValues (..)
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPrv, XPub )
import Cardano.Address.Script
    ( KeyHash, ScriptHash (..) )
import Cardano.DB.Sqlite
    ( DBField (..)
    , DBLog (..)
    , ManualMigration (..)
    , SqliteContext (..)
    , chunkSize
    , dbChunked
    , destroyDBLayer
    , fieldName
    , fieldType
    , handleConstraint
    , startSqliteBackend
    , tableName
    )
import Cardano.DB.Sqlite.Delete
    ( deleteSqliteDatabase, newRefCount, waitForFree, withRef )
import Cardano.Wallet.DB
    ( DBFactory (..)
    , DBLayer (..)
    , ErrNoSuchWallet (..)
    , ErrRemoveTx (..)
    , ErrWalletAlreadyExists (..)
    , PrimaryKey (..)
    , defaultSparseCheckpointsConfig
    , sparseCheckpoints
    )
import Cardano.Wallet.DB.Sqlite.TH
    ( Checkpoint (..)
    , DelegationCertificate (..)
    , DelegationReward (..)
    , EntityField (..)
    , Key (..)
    , PrivateKey (..)
    , ProtocolParameters (..)
    , RndState (..)
    , RndStateAddress (..)
    , RndStatePendingAddress (..)
    , SeqState (..)
    , SeqStateAddress (..)
    , SeqStateKeyHash (..)
    , SeqStatePendingIx (..)
    , SeqStateScriptHash (..)
    , StakeKeyCertificate (..)
    , TxIn (..)
    , TxMeta (..)
    , TxOut (..)
    , TxOutToken (..)
    , TxWithdrawal (..)
    , UTxO (..)
    , UTxOToken (..)
    , Wallet (..)
    , migrateAll
    , unWalletKey
    )
import Cardano.Wallet.DB.Sqlite.Types
    ( BlockId (..), HDPassphrase (..), TxId (..) )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..)
    , DerivationType (..)
    , Index (..)
    , MkKeyFingerprint (..)
    , NetworkDiscriminant (..)
    , PaymentAddress (..)
    , PersistPrivateKey (..)
    , PersistPublicKey (..)
    , SoftDerivation (..)
    , WalletKey (..)
    )
import Cardano.Wallet.Primitive.AddressDerivation.Icarus
    ( IcarusKey )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( ShelleyKey (..) )
import Cardano.Wallet.Primitive.Slotting
    ( TimeInterpreter
    , epochOf
    , firstSlotInEpoch
    , interpretQuery
    , slotToUTCTime
    )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( AssetId (..) )
import Control.Monad
    ( forM, unless, void, when )
import Control.Monad.Extra
    ( concatMapM )
import Control.Monad.IO.Class
    ( MonadIO (..) )
import Control.Monad.Trans.Class
    ( lift )
import Control.Monad.Trans.Except
    ( ExceptT (..) )
import Control.Monad.Trans.Maybe
    ( MaybeT (..) )
import Control.Tracer
    ( Tracer, contramap, traceWith )
import Data.Coerce
    ( coerce )
import Data.Either
    ( isRight )
import Data.Generics.Internal.VL.Lens
    ( view, (^.) )
import Data.IORef
    ( modifyIORef', newIORef, readIORef )
import Data.List
    ( nub, sortOn, unzip3 )
import Data.List.Split
    ( chunksOf )
import Data.Map.Strict
    ( Map )
import Data.Maybe
    ( catMaybes, mapMaybe )
import Data.Ord
    ( Down (..) )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( ToText (..), fromText )
import Data.Typeable
    ( Typeable )
import Data.Word
    ( Word16, Word32 )
import Database.Persist.Class
    ( toPersistValue )
import Database.Persist.Sql
    ( Entity (..)
    , Filter
    , SelectOpt (..)
    , Update (..)
    , deleteCascadeWhere
    , deleteWhere
    , deleteWhereCount
    , insertMany_
    , insert_
    , rawExecute
    , repsert
    , repsertMany
    , selectFirst
    , selectKeysList
    , selectList
    , updateWhere
    , (/<-.)
    , (<-.)
    , (<.)
    , (<=.)
    , (=.)
    , (==.)
    , (>.)
    , (>=.)
    )
import Database.Persist.Sqlite
    ( SqlPersistT )
import Database.Persist.Types
    ( PersistValue (..), fromPersistValueText )
import Fmt
    ( pretty )
import Numeric.Natural
    ( Natural )
import System.Directory
    ( doesFileExist, listDirectory )
import System.FilePath
    ( (</>) )
import UnliftIO.Exception
    ( Exception, bracket, throwIO )
import UnliftIO.MVar
    ( modifyMVar, modifyMVar_, newMVar, readMVar )

import qualified Cardano.Wallet.Primitive.AddressDerivation as W
import qualified Cardano.Wallet.Primitive.AddressDiscovery.Random as Rnd
import qualified Cardano.Wallet.Primitive.AddressDiscovery.Sequential as Seq
import qualified Cardano.Wallet.Primitive.Model as W
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Cardano.Wallet.Primitive.Types.Address as W
import qualified Cardano.Wallet.Primitive.Types.Coin as W
import qualified Cardano.Wallet.Primitive.Types.Hash as W
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.Tx as W
import qualified Cardano.Wallet.Primitive.Types.UTxO as W
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Database.Sqlite as Sqlite

-- | Runs an action with a connection to the SQLite database.
--
-- Database migrations are run to create tables if necessary.
--
-- If the given file path does not exist, it will be created by the sqlite
-- library.
withDBLayer
    :: forall s k a.
        ( PersistState s
        , PersistPrivateKey (k 'RootK)
        , WalletKey k
        )
    => Tracer IO DBLog
       -- ^ Logging object
    -> DefaultFieldValues
       -- ^ Default database field values, used during migration.
    -> Maybe FilePath
       -- ^ Path to database directory, or Nothing for in-memory database
    -> TimeInterpreter IO
    -> ((SqliteContext, DBLayer IO s k) -> IO a)
       -- ^ Action to run.
    -> IO a
withDBLayer trace defaultFieldValues mDatabaseDir ti =
    bracket before after
  where
    before = newDBLayer trace defaultFieldValues mDatabaseDir ti
    after = destroyDBLayer . fst

-- | Instantiate a 'DBFactory' from a given directory
newDBFactory
    :: forall s k.
        ( PersistState s
        , PersistPrivateKey (k 'RootK)
        , WalletKey k
        )
    => Tracer IO DBLog
       -- ^ Logging object
    -> DefaultFieldValues
       -- ^ Default database field values, used during migration.
    -> TimeInterpreter IO

    -> Maybe FilePath
       -- ^ Path to database directory, or Nothing for in-memory database
    -> IO (DBFactory IO s k)
newDBFactory tr defaultFieldValues ti = \case
    Nothing -> do
        -- NOTE
        -- For the in-memory database, we do actually preserve the database
        -- after the 'action' is done. This allows for calling 'withDatabase'
        -- several times within the same execution and get back the same
        -- database. The memory is only cleaned up when calling
        -- 'removeDatabase', to mimic the way the file database works!
        mvar <- newMVar mempty
        pure DBFactory
            { withDatabase = \wid action -> do
                db <- modifyMVar mvar $ \m -> case Map.lookup wid m of
                    Just (_, db) -> pure (m, db)
                    Nothing -> do
                        (ctx, db) <-
                            newDBLayer tr defaultFieldValues Nothing ti
                        pure (Map.insert wid (ctx, db) m, db)
                action db
            , removeDatabase = \wid -> do
                traceWith tr $ MsgRemoving (pretty wid)
                modifyMVar_ mvar (pure . Map.delete wid)

            , listDatabases =
                Map.keys <$> readMVar mvar
            }

    Just databaseDir -> do
        refs <- newRefCount
        pure DBFactory
            { withDatabase = \wid action -> withRef refs wid $ withDBLayer
                tr
                defaultFieldValues
                (Just $ databaseFile wid)
                ti
                (action . snd)
            , removeDatabase = \wid -> do
                let widp = pretty wid
                -- try to wait for all 'withDatabase' calls to finish before
                -- deleting database file.
                let trWait = contramap (MsgWaitingForDatabase widp) tr
                waitForFree trWait refs wid $ \inUse -> do
                    unless (inUse == 0) $
                        traceWith tr $ MsgRemovingInUse widp inUse
                    traceWith tr $ MsgRemoving widp
                    let trDel = contramap (MsgRemovingDatabaseFile widp) tr
                    deleteSqliteDatabase trDel (databaseFile wid)
            , listDatabases =
                findDatabases @k tr databaseDir
            }
      where
        databaseFilePrefix = keyTypeDescriptor $ Proxy @k
        databaseFile wid =
            databaseDir </>
            databaseFilePrefix <> "." <>
            T.unpack (toText wid) <> ".sqlite"

-- | Return all wallet databases that match the specified key type within the
--   specified directory.
findDatabases
    :: forall k. WalletKey k
    => Tracer IO DBLog
    -> FilePath
    -> IO [W.WalletId]
findDatabases tr dir = do
    files <- listDirectory dir
    fmap catMaybes $ forM files $ \file -> do
        isFile <- doesFileExist (dir </> file)
        case (isFile, T.splitOn "." $ T.pack file) of
            (True, prefix : basename : ["sqlite"]) | prefix == expectedPrefix ->
                case fromText basename of
                    Right wid -> do
                        traceWith tr $ MsgFoundDatabase (dir </> file) (toText wid)
                        return (Just wid)
                    _ -> do
                        traceWith tr $ MsgUnknownDBFile file
                        return Nothing
            _ -> return Nothing
  where
    expectedPrefix = T.pack $ keyTypeDescriptor $ Proxy @k

-- | A data-type for capturing column status. Used to be represented as a
-- 'Maybe Bool' which is somewhat confusing to interpret.
data SqlColumnStatus
    = TableMissing
    | ColumnMissing
    | ColumnPresent
    deriving Eq

-- | Executes any manual database migration steps that may be required on
--   startup.
--
migrateManually
    :: WalletKey k
    => Tracer IO DBLog
    -> Proxy k
    -> DefaultFieldValues
    -> ManualMigration
migrateManually tr proxy defaultFieldValues =
    ManualMigration $ \conn -> do
        cleanupCheckpointTable conn

        assignDefaultPassphraseScheme conn

        addDesiredPoolNumberIfMissing conn

        addMinimumUTxOValueIfMissing conn

        addHardforkEpochIfMissing conn

        -- FIXME
        -- Temporary migration to fix Daedalus flight wallets. This should
        -- really be removed as soon as we have a fix for the cardano-sl:wallet
        -- currently in production.
        removeSoftRndAddresses conn

        removeOldTxParametersTable conn

        addAddressStateIfMissing conn

        addSeqStateDerivationPrefixIfMissing conn

        renameRoleColumn conn

        renameRoleFields conn

        addScriptAddressGapIfMissing conn

        updateFeeValueAndAddKeyDeposit conn
  where
    -- NOTE
    -- We originally stored protocol parameters in the 'Checkpoint' table, and
    -- later moved them to a new dedicatd table. However, removing a column is
    -- not something straightforward in SQLite, so we initially simply marked
    -- most parameters as _unused. Later, we did rework how genesis and protocol
    -- parameters were stored and shared between wallets and completely removed
    -- them from the database. At the same time, we also introduced
    -- 'genesis_hash' and 'genesis_start' in the 'Wallet' table which we use is
    -- as a discriminator for the migration.
    cleanupCheckpointTable :: Sqlite.Connection -> IO ()
    cleanupCheckpointTable conn = do
        let field = DBField WalGenesisHash
        isFieldPresent conn field >>= \case
            TableMissing ->
                traceWith tr $ MsgManualMigrationNotNeeded field

            ColumnPresent -> do
                traceWith tr $ MsgManualMigrationNotNeeded field

            ColumnMissing -> do
                let orig = "checkpoint"
                let tmp = orig <> "_tmp"

                -- 1. Add genesis_hash and genesis_start to the 'wallet' table.
                [defaults] <- runSql conn $ select ["genesis_hash", "genesis_start"] orig
                let [PersistText genesisHash, PersistText genesisStart] = defaults
                addColumn_ conn True (DBField WalGenesisHash) (quotes genesisHash)
                addColumn_ conn True (DBField WalGenesisStart) (quotes genesisStart)

                -- 2. Drop columns from the 'checkpoint' table
                info <- runSql conn $ getTableInfo orig
                let filtered = mapMaybe (filterColumn excluding) info
                      where
                        excluding =
                            [ "genesis_hash", "genesis_start", "fee_policy"
                            , "slot_length", "epoch_length", "tx_max_size"
                            , "epoch_stability", "active_slot_coeff"
                            ]
                _ <- runSql conn $ createTable tmp filtered
                _ <- runSql conn $ copyTable orig tmp filtered
                _ <- runSql conn $ dropTable orig
                _ <- runSql conn $ renameTable tmp orig

                return ()
      where
        select fields table = mconcat
            [ "SELECT ", T.intercalate ", " fields
            , " FROM ", table
            , " ORDER BY slot ASC LIMIT 1;"
            ]

        getTableInfo table = mconcat
            [ "PRAGMA table_info(", table, ");"
            ]

        createTable table cols = mconcat
            [ "CREATE TABLE ", table, " ("
            , T.intercalate ", " (mapMaybe createColumn cols)
            , ");"
            ]

        copyTable source destination cols = mconcat
            [ "INSERT INTO ", destination, " SELECT "
            , T.intercalate ", " (mapMaybe selectColumn cols)
            , " FROM ", source
            , ";"
            ]

        dropTable table = mconcat
            [ "DROP TABLE " <> table <> ";"
            ]

        renameTable from to = mconcat
            [ "ALTER TABLE ", from, " RENAME TO ", to, ";" ]

        filterColumn :: [Text] -> [PersistValue] -> Maybe [PersistValue]
        filterColumn excluding = \case
            [ _, PersistText colName, PersistText colType, colNull, _, _] ->
                if colName `elem` excluding then
                    Nothing
                else
                    Just [PersistText colName, PersistText colType, colNull]
            _ ->
                Nothing

        selectColumn :: [PersistValue] -> Maybe Text
        selectColumn = \case
            [ PersistText colName, _ , _ ] ->
                Just colName
            _ ->
                Nothing

        createColumn :: [PersistValue] -> Maybe Text
        createColumn = \case
            [ PersistText colName, PersistText colType, PersistInt64 1 ] ->
                Just $ T.unwords [ colName, colType, "NOT NULL" ]
            [ PersistText colName, PersistText colType, _ ] ->
                Just $ T.unwords [ colName, colType ]
            _ ->
                Nothing

    -- NOTE
    -- Wallets created before the 'PassphraseScheme' was introduced have no
    -- passphrase scheme set in the database. Yet, their passphrase is known
    -- to use the default / new scheme (i.e. PBKDF2) and, it is impossible
    -- to have a wallet with a scheme but no last update. Either they should
    -- have both, or they should have none.
    --
    --     Creation Method               | Scheme | Last Update
    --     ---                           | ---    | ---
    --     Byron, from mnemonic          | ✓      | ✓
    --     Byron, from xprv              | ✓      | ✓
    --     Shelley, from mnemonic        | ✓      | ✓
    --     Shelley, from account pub key | ø      | ø
    assignDefaultPassphraseScheme :: Sqlite.Connection -> IO ()
    assignDefaultPassphraseScheme conn = do
        isFieldPresent conn passphraseScheme >>= \case
            TableMissing -> do
                traceWith tr $ MsgManualMigrationNotNeeded passphraseScheme
            ColumnMissing -> do
                traceWith tr $ MsgManualMigrationNotNeeded passphraseScheme
                query <- Sqlite.prepare conn $ T.unwords
                    [ "ALTER TABLE", tableName passphraseScheme
                    , "ADD COLUMN", fieldName passphraseScheme
                    , fieldType passphraseScheme, " NULL"
                    , ";"
                    ]
                Sqlite.step query *> Sqlite.finalize query
                assignDefaultPassphraseScheme conn -- loop to apply case below
            ColumnPresent  -> do
                value <- either (fail . show) (\x -> pure $ "\"" <> x <> "\"") $
                    fromPersistValueText (toPersistValue W.EncryptWithPBKDF2)
                traceWith tr $ MsgManualMigrationNeeded passphraseScheme value
                query <- Sqlite.prepare conn $ T.unwords
                    [ "UPDATE", tableName passphraseScheme
                    , "SET", fieldName passphraseScheme, "=", value
                    , "WHERE", fieldName passphraseScheme, "IS NULL"
                    , "AND", fieldName passphraseLastUpdatedAt, "IS NOT NULL"
                    , ";"
                    ]
                Sqlite.step query *> Sqlite.finalize query
      where
        passphraseScheme = DBField WalPassphraseScheme
        passphraseLastUpdatedAt = DBField WalPassphraseLastUpdatedAt

    -- | Remove any addresses that were wrongly generated in previous releases.
    -- See comment below in 'selectState' from 'RndState'.
    --
    -- Important: this _may_ remove USED addresses from the discovered set which
    -- is _okay-ish_ for two reasons:
    --
    --     1. Address will still be discovered in UTxOs and this won't affect
    --     users' balance. But the address won't show up when in the listing.
    --     This is a wanted behavior.
    --
    --     2. The discovered list of address is really used internally to avoid
    --     index clash when generating new change addresses. Since we'll
    --     generate addresses from a completely different part of the HD tree
    --     ANYWAY, there's no risk of clash.
    removeSoftRndAddresses :: Sqlite.Connection -> IO ()
    removeSoftRndAddresses conn = do
        isFieldPresent conn rndAccountIx >>= \case
            TableMissing -> do
                traceWith tr $ MsgManualMigrationNotNeeded rndAccountIx
            ColumnMissing -> do
                traceWith tr $ MsgManualMigrationNotNeeded rndAccountIx
            ColumnPresent -> do
                traceWith tr $ MsgManualMigrationNeeded rndAccountIx hardLowerBound
                stmt <- Sqlite.prepare conn $ T.unwords
                    [ "DELETE FROM", tableName rndAccountIx
                    , "WHERE", fieldName rndAccountIx, "<", hardLowerBound
                    , ";"
                    ]
                _ <- Sqlite.step stmt
                Sqlite.finalize stmt
      where
        hardLowerBound = toText $ fromEnum $ minBound @(Index 'Hardened _)
        rndAccountIx   = DBField RndStateAddressAccountIndex

    -- | Adds an 'desired_pool_number' column to the 'protocol_parameters'
    -- table if it is missing.
    --
    addDesiredPoolNumberIfMissing :: Sqlite.Connection -> IO ()
    addDesiredPoolNumberIfMissing conn = do
        addColumn_ conn True (DBField ProtocolParametersDesiredNumberOfPools) value
      where
        value = T.pack $ show $ defaultDesiredNumberOfPool defaultFieldValues

    -- | Adds an 'minimum_utxo_value' column to the 'protocol_parameters'
    -- table if it is missing.
    --
    addMinimumUTxOValueIfMissing :: Sqlite.Connection -> IO ()
    addMinimumUTxOValueIfMissing conn = do
        addColumn_ conn True (DBField ProtocolParametersMinimumUtxoValue) value
      where
        value = T.pack $ show $ W.unCoin $ defaultMinimumUTxOValue defaultFieldValues

    -- | Adds an 'hardfork_epoch' column to the 'protocol_parameters'
    -- table if it is missing.
    --
    addHardforkEpochIfMissing :: Sqlite.Connection -> IO ()
    addHardforkEpochIfMissing conn = do
        addColumn_ conn False (DBField ProtocolParametersHardforkEpoch) value
      where
        value = case defaultHardforkEpoch defaultFieldValues of
            Nothing -> "NULL"
            Just v -> T.pack $ show $ W.unEpochNo v

    -- | Adds a 'key_deposit column to the 'protocol_parameters' table if it is
    -- missing.
    --
    addKeyDepositIfMissing :: Sqlite.Connection -> Text -> IO ()
    addKeyDepositIfMissing conn =
        addColumn_ conn True (DBField ProtocolParametersKeyDeposit)

    -- | This table became @protocol_parameters@.
    removeOldTxParametersTable :: Sqlite.Connection -> IO ()
    removeOldTxParametersTable conn = do
        dropTable <- Sqlite.prepare conn "DROP TABLE IF EXISTS tx_parameters;"
        void $ Sqlite.stepConn conn dropTable
        Sqlite.finalize dropTable

    -- | In order to make listing addresses bearable for large wallet, we
    -- altered the discovery process to mark addresses as used as they are
    -- discovered. Existing databases don't have that pre-computed field.
    addAddressStateIfMissing :: Sqlite.Connection -> IO ()
    addAddressStateIfMissing conn = do
        _  <- addColumn conn False (DBField SeqStateAddressStatus) (toText W.Unused)
        st <- addColumn conn False (DBField RndStateAddressStatus) (toText W.Unused)
        when (st == ColumnMissing) $ do
            markAddressesAsUsed (DBField SeqStateAddressStatus)
            markAddressesAsUsed (DBField RndStateAddressStatus)
      where
        markAddressesAsUsed field = do
            query <- Sqlite.prepare conn $ T.unwords
                [ "UPDATE", tableName field
                , "SET status = '" <> toText W.Used <> "'"
                , "WHERE", tableName field <> ".address", "IN"
                , "(SELECT DISTINCT(address) FROM tx_out)"
                ]
            _ <- Sqlite.step query
            Sqlite.finalize query

    addSeqStateDerivationPrefixIfMissing :: Sqlite.Connection -> IO ()
    addSeqStateDerivationPrefixIfMissing conn
        | isIcarusDatabase = do
            addColumn_ conn True (DBField SeqStateDerivationPrefix) icarusPrefix

        | isShelleyDatabase = do
            addColumn_ conn True (DBField SeqStateDerivationPrefix) shelleyPrefix

        | otherwise =
            return ()
      where
        isIcarusDatabase =
            keyTypeDescriptor proxy == keyTypeDescriptor (Proxy @IcarusKey)
        icarusPrefix = T.pack $ show $ toText
            $ Seq.DerivationPrefix (Seq.purposeBIP44, Seq.coinTypeAda, minBound)

        isShelleyDatabase =
            keyTypeDescriptor proxy == keyTypeDescriptor (Proxy @ShelleyKey)
        shelleyPrefix = T.pack $ show $ toText
            $ Seq.DerivationPrefix (Seq.purposeCIP1852, Seq.coinTypeAda, minBound)

    --
    --   - UTxOInternal
    --   - UTxOExternal
    --
    -- (notice the mixed case here) and were serialized to text as:
    --
    --   - u_tx_o_internal
    --   - u_tx_o_external
    --
    -- which is pretty lame. This was changed later on, but already
    -- serialized data may subsist on for quite a while. Hence this little
    -- pirouette here.
    renameRoleFields :: Sqlite.Connection -> IO ()
    renameRoleFields conn = do
        renameColumnField conn (DBField SeqStateAddressRole)
            "u_tx_o_internal" "utxo_internal"
        renameColumnField conn (DBField SeqStateAddressRole)
            "u_tx_o_external" "utxo_external"

    -- | Rename column table of SeqStateAddress from 'accounting_style' to `role`
    -- if needed.
    renameRoleColumn :: Sqlite.Connection -> IO ()
    renameRoleColumn conn =
        isFieldPresent conn roleField >>= \case
            TableMissing ->
                traceWith tr $ MsgManualMigrationNotNeeded roleField
            ColumnMissing -> do
                traceWith tr $ MsgManualMigrationNeeded roleField "accounting_style"
                query <- Sqlite.prepare conn $ T.unwords
                    [ "ALTER TABLE", tableName roleField
                    , "RENAME COLUMN accounting_style TO"
                    , fieldName roleField
                    , ";"
                    ]
                Sqlite.step query *> Sqlite.finalize query
            ColumnPresent ->
                traceWith tr $ MsgManualMigrationNotNeeded roleField
      where
        roleField = DBField SeqStateAddressRole

    -- | Adds an 'script_gap' column to the 'SeqState'
    -- table if it is missing.
    --
    addScriptAddressGapIfMissing :: Sqlite.Connection -> IO ()
    addScriptAddressGapIfMissing conn =
        addColumn_ conn True (DBField SeqStateScriptGap) value
     where
        value = T.pack $ show $ Seq.getAddressPoolGap Seq.defaultAddressPoolGap

--    addFeeToTransaction :: Sqlite.Connection -> IO ()
--    addFeeToTransaction conn = do
--        isFieldPresent conn fieldFee >>= \case
--            ColumnMissing -> do
--                addColumn_ conn True fieldFee "0"
--                rows <- runSql conn $ mconcat
--                    [ "SELECT tx_id, totalIn + totalWdrl - totalOut FROM tx"
--                    , " JOIN (SELECT tx_id, SUM(amount) AS totalIn   FROM tx_in  GROUP_BY tx_id)"
--                    , " JOIN (SELECT tx_id, SUM(amount) AS totalOut  FROM tx_out GROUP_BY tx_id)"
--                    , " JOIN (SELECT tx_id, SUM(AMOUNT) AS totalWdrl FROM tx_withdrawal GROUP_BY tx_id)"
--                    , ";"
--                    ]
--
--                    SELECT tx.tx_id, totalIn - totalOut FROM tx
--                        JOIN (SELECT tx_id, SUM(amount) AS totalIn  FROM txin  GROUP BY tx_id) USING (tx_id)
--                        JOIN (SELECT tx_id, SUM(amount) AS totalOut FROM txout GROUP BY tx_id) USING (tx_id)
--                    ;
--
--            _ ->
--                return ()
--      where
--        fieldFee = DBField TxMetaFee

    -- | Since key deposit and fee value are intertwined, we migrate them both
    -- here.
    updateFeeValueAndAddKeyDeposit :: Sqlite.Connection -> IO ()
    updateFeeValueAndAddKeyDeposit conn = do
        isFieldPresent conn fieldKeyDeposit >>= \case
            ColumnMissing -> do
                -- If the key deposit is missing, we need to add it, but also
                -- and first, we also need to update the fee policy and drop
                -- the third component of the fee policy which is now captured
                -- by the stake key deposit.
                feePolicyInfo <- Sqlite.prepare conn $ T.unwords
                    [ "SELECT", fieldName fieldFeePolicy
                    , "FROM", tableName fieldFeePolicy
                    , ";"
                    ]
                row <- Sqlite.step feePolicyInfo >> Sqlite.columns feePolicyInfo
                Sqlite.finalize feePolicyInfo

                case filter (/= PersistNull) row of
                    [PersistText t] -> case T.splitOn " + " t of
                        [a,b,c] -> do
                            traceWith tr $ MsgManualMigrationNeeded fieldFeePolicy t
                            -- update fee policy
                            let newVal = a <> " + " <> b
                            query <- Sqlite.prepare conn $ T.unwords
                                [ "UPDATE", tableName fieldFeePolicy
                                , "SET", fieldName fieldFeePolicy, "= '" <> newVal <> "'"
                                , ";"
                                ]
                            Sqlite.step query *> Sqlite.finalize query
                            let (Right stakeKeyVal) = W.Coin . round <$> fromText @Double (T.dropEnd 1 c)
                            addKeyDepositIfMissing conn (toText stakeKeyVal)
                        _ ->
                            fail ("Unexpected row result when querying fee value: " <> T.unpack t)
                    _ ->
                        return ()

            -- If the protocol_parameters table is missing, or if if the key
            -- deposit exists, there's nothing to do in this migration.
            _ -> do
                traceWith tr $ MsgManualMigrationNotNeeded fieldFeePolicy
                traceWith tr $ MsgManualMigrationNotNeeded fieldKeyDeposit
      where
        fieldFeePolicy  = DBField ProtocolParametersFeePolicy
        fieldKeyDeposit = DBField ProtocolParametersKeyDeposit

    -- | Determines whether a field is present in its parent table.
    isFieldPresent :: Sqlite.Connection -> DBField -> IO SqlColumnStatus
    isFieldPresent conn field = do
        getTableInfo <- Sqlite.prepare conn $ mconcat
            [ "SELECT sql FROM sqlite_master "
            , "WHERE type = 'table' "
            , "AND name = '" <> tableName field <> "';"
            ]
        row <- Sqlite.step getTableInfo
            >> Sqlite.columns getTableInfo
        Sqlite.finalize getTableInfo
        pure $ case row of
            [PersistText t]
                | fieldName field `T.isInfixOf` t -> ColumnPresent
                | otherwise                       -> ColumnMissing
            _ -> TableMissing

    addColumn_
        :: Sqlite.Connection
        -> Bool
        -> DBField
        -> Text
        -> IO ()
    addColumn_ a b c =
        void . addColumn a b c

    -- | A migration for adding a non-existing column to a table. Factor out as
    -- it's a common use-case.
    addColumn
        :: Sqlite.Connection
        -> Bool
        -> DBField
        -> Text
        -> IO SqlColumnStatus
    addColumn conn notNull field value = do
        isFieldPresent conn field >>= \st -> st <$ case st of
            TableMissing ->
                traceWith tr $ MsgManualMigrationNotNeeded field
            ColumnMissing -> do
                traceWith tr $ MsgManualMigrationNeeded field value
                query <- Sqlite.prepare conn $ T.unwords
                    [ "ALTER TABLE", tableName field
                    , "ADD COLUMN", fieldName field
                    , fieldType field, if notNull then "NOT NULL" else ""
                    , "DEFAULT", value
                    , ";"
                    ]
                _ <- Sqlite.step query
                Sqlite.finalize query
            ColumnPresent ->
                traceWith tr $ MsgManualMigrationNotNeeded field

    renameColumnField
        :: Sqlite.Connection
        -> DBField
        -> Text -- Old Value
        -> Text -- New Value
        -> IO ()
    renameColumnField conn field old new = do
        isFieldPresent conn field >>= \case
            TableMissing ->
                traceWith tr $ MsgManualMigrationNotNeeded field
            ColumnMissing -> do
                traceWith tr $ MsgManualMigrationNotNeeded field
            ColumnPresent -> do
                query <- Sqlite.prepare conn $ T.unwords
                    [ "UPDATE", tableName field
                    , "SET", fieldName field, "=", quotes new
                    , "WHERE", fieldName field, "=", quotes old
                    ]
                _ <- Sqlite.step query
                changes <- Sqlite.changes conn
                traceWith tr $ if changes > 0
                    then MsgManualMigrationNeeded field old
                    else MsgManualMigrationNotNeeded field
                Sqlite.finalize query

    quotes :: Text -> Text
    quotes x = "\"" <> x <> "\""

-- | Unsafe, execute a raw SQLite query. Used only in migration when really
-- needed.
runSql :: Sqlite.Connection -> Text -> IO [[PersistValue]]
runSql conn raw = do
    query <- Sqlite.prepare conn raw
    result <- collect query []
    Sqlite.finalize query
    return result
  where
    collect query acc = do
        step <- Sqlite.step query
        case step of
            Sqlite.Row -> do
                result <- Sqlite.columns query
                collect query (result : acc)
            Sqlite.Done -> do
                return (reverse acc)

-- | A set of default field values that can be consulted when performing a
--   database migration.
--
data DefaultFieldValues = DefaultFieldValues
    { defaultActiveSlotCoefficient :: W.ActiveSlotCoefficient
    , defaultDesiredNumberOfPool :: Word16
    , defaultMinimumUTxOValue :: W.Coin
    , defaultHardforkEpoch :: Maybe W.EpochNo
    , defaultKeyDeposit :: W.Coin
    }

-- | Sets up a connection to the SQLite database.
--
-- Database migrations are run to create tables if necessary.
--
-- If the given file path does not exist, it will be created by the sqlite
-- library.
--
-- 'getDBLayer' will provide the actual 'DBLayer' implementation. The database
-- should be closed with 'destroyDBLayer'. If you use 'withDBLayer' then both of
-- these things will be handled for you.
newDBLayer
    :: forall s k.
        ( PersistState s
        , PersistPrivateKey (k 'RootK)
        , WalletKey k
        )
    => Tracer IO DBLog
       -- ^ Logging object
    -> DefaultFieldValues
       -- ^ Default database field values, used during migration.
    -> Maybe FilePath
       -- ^ Path to database file, or Nothing for in-memory database
    -> TimeInterpreter IO
    -> IO (SqliteContext, DBLayer IO s k)
newDBLayer trace defaultFieldValues mDatabaseFile ti = do
    ctx@SqliteContext{runQuery} <-
        either throwIO pure =<<
        startSqliteBackend
            (migrateManually trace (Proxy @k) defaultFieldValues)
            migrateAll
            trace
            mDatabaseFile

    -- NOTE1
    -- We cache the latest checkpoint for read operation such that we prevent
    -- needless marshalling and unmarshalling with the database. Many handlers
    -- dealing with the database are actually in the form of:
    --
    --   - read latest CP
    --   - write latest CP
    --
    -- When chaining them, we end up paying the cost of unmarshalling data from
    -- the database one extra time. That doesn't matter much for small wallets
    -- because the time needed to unmarshall data is relatively negligible. For
    -- large wallets however, this has a massive performance impact.
    --
    -- Instead, the cache now retains the Haskell data-types in-memory to
    -- short-circuit the most frequent database lookups.
    --
    -- NOTE2
    -- We use an IORef here without fearing race-conditions because every
    -- database query can only be run within calls to `atomically` which
    -- enforces that there's only a single thread executing a given
    -- `SqlPersistT`.
    cache <- newIORef Map.empty

    let readCache :: W.WalletId -> SqlPersistT IO (Maybe (W.Wallet s))
        readCache wid = Map.lookup wid <$> liftIO (readIORef cache)

    let writeCache :: W.WalletId -> Maybe (W.Wallet s) -> SqlPersistT IO ()
        writeCache wid = \case
            Nothing ->
                liftIO $ modifyIORef' cache $ Map.delete wid
            Just cp -> do
                let tip = cp ^. #currentTip . #blockHeight
                let alter = \case
                        Just old | tip < old ^. #currentTip .  #blockHeight ->
                            Just old
                        _ ->
                            Just cp
                liftIO $ modifyIORef' cache $ Map.alter alter wid

    let selectLatestCheckpoint
            :: W.WalletId
            -> SqlPersistT IO (Maybe (W.Wallet s))
        selectLatestCheckpoint wid = do
            readCache wid >>= maybe fromDatabase (pure . Just)
          where
            fromDatabase = do
                mcp <- fmap entityVal <$> selectFirst
                    [ CheckpointWalletId ==. wid ]
                    [ LimitTo 1, Desc CheckpointSlot ]
                case mcp of
                    Nothing -> pure Nothing
                    Just cp -> do
                        utxo <- selectUTxO cp
                        s <- selectState (checkpointId cp)
                        pure (checkpointFromEntity @s cp utxo <$> s)

    let invalidateCache :: W.WalletId -> SqlPersistT IO ()
        invalidateCache wid = do
            writeCache wid Nothing
            selectLatestCheckpoint wid >>= writeCache wid

    return (ctx, DBLayer

        {-----------------------------------------------------------------------
                                      Wallets
        -----------------------------------------------------------------------}

        { initializeWallet = \(PrimaryKey wid) cp meta txs gp pp -> ExceptT $ do
            res <- handleConstraint (ErrWalletAlreadyExists wid) $
                insert_ (mkWalletEntity wid meta gp)
            when (isRight res) $ do
                insertCheckpoint wid cp <* writeCache wid (Just cp)
                let (metas, txins, txouts, txoutTokens, ws) =
                        mkTxHistory wid txs
                putTxs metas txins txouts txoutTokens ws
                insert_ (mkProtocolParametersEntity wid pp)
            pure res

        , removeWallet = \(PrimaryKey wid) -> ExceptT $ do
            selectWallet wid >>= \case
                Nothing -> pure $ Left $ ErrNoSuchWallet wid
                Just _  -> Right <$> do
                    deleteCascadeWhere [WalId ==. wid]
                    deleteLooseTransactions
                    invalidateCache wid

        , listWallets =
            map (PrimaryKey . unWalletKey) <$> selectKeysList [] [Asc WalId]

        {-----------------------------------------------------------------------
                                    Checkpoints
        -----------------------------------------------------------------------}

        , putCheckpoint = \(PrimaryKey wid) cp -> ExceptT $ do
            selectWallet wid >>= \case
                Nothing ->
                    pure $ Left $ ErrNoSuchWallet wid
                Just _  ->
                    Right <$> (insertCheckpoint wid cp <* writeCache wid (Just cp))

        , readCheckpoint = \(PrimaryKey wid) -> do
            selectLatestCheckpoint wid

        , listCheckpoints = \(PrimaryKey wid) -> do
            map (blockHeaderFromEntity . entityVal) <$> selectList
                [ CheckpointWalletId ==. wid ]
                [ Asc CheckpointSlot ]

        , rollbackTo = \(PrimaryKey wid) requestedPoint -> ExceptT $ do
            findNearestPoint wid requestedPoint >>= \case
                Nothing -> selectWallet wid >>= \case
                    Nothing ->
                        pure $ Left $ ErrNoSuchWallet wid
                    Just _  ->
                        lift $ throwIO (ErrNoOlderCheckpoint wid requestedPoint)
                Just nearestPoint -> do
                    deleteCheckpoints wid
                        [ CheckpointSlot >. nearestPoint
                        ]
                    deleteDelegationCertificates wid
                        [ CertSlot >. nearestPoint
                        ]
                    updateTxMetas wid
                        [ TxMetaDirection ==. W.Outgoing
                        , TxMetaSlot >. nearestPoint
                        ]
                        [ TxMetaStatus =. W.Pending
                        , TxMetaSlot =. nearestPoint
                        ]
                    deleteTxMetas wid
                        [ TxMetaDirection ==. W.Incoming
                        , TxMetaSlot >. nearestPoint
                        ]
                    deleteStakeKeyCerts wid
                        [ StakeKeyCertSlot >. nearestPoint
                        ]
                    invalidateCache wid
                    pure (Right nearestPoint)

        , prune = \(PrimaryKey wid) epochStability -> ExceptT $ do
            selectLatestCheckpoint wid >>= \case
                Nothing -> pure $ Left $ ErrNoSuchWallet wid
                Just cp -> Right <$> do
                    pruneCheckpoints wid epochStability cp
                    deleteLooseTransactions

        {-----------------------------------------------------------------------
                                   Wallet Metadata
        -----------------------------------------------------------------------}

        , putWalletMeta = \(PrimaryKey wid) meta -> ExceptT $ do
            selectWallet wid >>= \case
                Nothing -> pure $ Left $ ErrNoSuchWallet wid
                Just _ -> do
                    updateWhere [WalId ==. wid]
                        (mkWalletMetadataUpdate meta)
                    pure $ Right ()

        , readWalletMeta = \(PrimaryKey wid) -> do
            selectLatestCheckpoint wid >>= \case
                Nothing -> pure Nothing
                Just cp -> do
                    currentEpoch <- liftIO $
                        interpretQuery ti (epochOf $ cp ^. #currentTip . #slotNo)
                    readWalletDelegation ti wid currentEpoch
                        >>= readWalletMetadata wid

        , putDelegationCertificate = \(PrimaryKey wid) cert sl -> ExceptT $ do
            selectWallet wid >>= \case
                Nothing -> pure $ Left $ ErrNoSuchWallet wid
                Just _  -> case cert of
                    W.CertDelegateNone _ -> do
                        repsert
                            (DelegationCertificateKey wid sl)
                            (DelegationCertificate wid sl Nothing)
                        pure <$> repsert
                            (StakeKeyCertificateKey wid sl)
                            (StakeKeyCertificate wid sl W.StakeKeyDeregistration)
                    W.CertDelegateFull _ pool ->
                        pure <$> repsert
                            (DelegationCertificateKey wid sl)
                            (DelegationCertificate wid sl (Just pool))
                    W.CertRegisterKey _ ->
                        pure <$> repsert
                            (StakeKeyCertificateKey wid sl)
                            (StakeKeyCertificate wid sl W.StakeKeyRegistration)

        , isStakeKeyRegistered = \(PrimaryKey wid) -> ExceptT $ do
              selectWallet wid >>= \case
                Nothing -> pure $ Left $ ErrNoSuchWallet wid
                Just{} -> do
                    val <- fmap entityVal <$> selectFirst
                        [StakeKeyCertWalletId ==. wid]
                        [Desc StakeKeyCertSlot]
                    return $ case val of
                        Nothing -> Right False
                        Just (StakeKeyCertificate _ _ status) ->
                            Right (status == W.StakeKeyRegistration)

        {-----------------------------------------------------------------------
                                     Tx History
        -----------------------------------------------------------------------}

        , putTxHistory = \(PrimaryKey wid) txs -> ExceptT $ do
            selectWallet wid >>= \case
                Nothing -> pure $ Left $ ErrNoSuchWallet wid
                Just _ -> do
                    let (metas, txins, txouts, txoutTokens, ws) =
                            mkTxHistory wid txs
                    putTxs metas txins txouts txoutTokens ws
                    pure $ Right ()

        , readTxHistory = \(PrimaryKey wid) minWithdrawal order range status -> do
            selectLatestCheckpoint wid >>= \case
                Nothing -> pure []
                Just cp -> selectTxHistory cp
                    ti wid minWithdrawal order $ catMaybes
                    [ (TxMetaSlot >=.) <$> W.inclusiveLowerBound range
                    , (TxMetaSlot <=.) <$> W.inclusiveUpperBound range
                    , (TxMetaStatus ==.) <$> status
                    ]

        , updatePendingTxForExpiry = \(PrimaryKey wid) tip -> ExceptT $ do
            selectWallet wid >>= \case
                Nothing -> pure $ Left $ ErrNoSuchWallet wid
                Just _ -> do
                  updatePendingTxForExpiryQuery wid tip
                  pure $ Right ()

        , removePendingOrExpiredTx = \(PrimaryKey wid) tid -> ExceptT $ do
            let errNoSuchWallet =
                    Left $ ErrRemoveTxNoSuchWallet $ ErrNoSuchWallet wid
            let errNoMorePending =
                    Left $ ErrRemoveTxAlreadyInLedger tid
            let errNoSuchTransaction =
                    Left $ ErrRemoveTxNoSuchTransaction tid
            selectWallet wid >>= \case
                Nothing -> pure errNoSuchWallet
                Just _  -> selectTxMeta wid tid >>= \case
                    Nothing -> pure errNoSuchTransaction
                    Just _ -> do
                        count <- deletePendingOrExpiredTx wid tid
                        pure $ if count == 0
                            then errNoMorePending
                            else Right ()

        , getTx = \(PrimaryKey wid) tid -> ExceptT $ do
            selectLatestCheckpoint wid >>= \case
                Nothing -> pure $ Left $ ErrNoSuchWallet wid
                Just cp -> do
                    metas <- selectTxHistory cp
                        ti wid Nothing W.Descending
                            [ TxMetaTxId ==. TxId tid ]
                    case metas of
                        [] -> pure (Right Nothing)
                        meta:_ -> pure (Right $ Just meta)

        {-----------------------------------------------------------------------
                                       Keystore
        -----------------------------------------------------------------------}

        , putPrivateKey = \(PrimaryKey wid) key -> ExceptT $ do
            selectWallet wid >>= \case
                Nothing -> pure $ Left $ ErrNoSuchWallet wid
                Just _ -> Right <$> do
                    deleteWhere [PrivateKeyWalletId ==. wid]
                    insert_ (mkPrivateKeyEntity wid key)

        , readPrivateKey = \(PrimaryKey wid) -> selectPrivateKey wid

        {-----------------------------------------------------------------------
                                 Blockchain Parameters
        -----------------------------------------------------------------------}

        , putProtocolParameters = \(PrimaryKey wid) pp -> ExceptT $ do
            selectWallet wid >>= \case
                Nothing -> pure $ Left $ ErrNoSuchWallet wid
                Just _  -> Right <$> repsert
                    (ProtocolParametersKey wid)
                    (mkProtocolParametersEntity wid pp)

        , readProtocolParameters =
            \(PrimaryKey wid) -> selectProtocolParameters wid

        , readGenesisParameters =
            \(PrimaryKey wid) -> selectGenesisParameters wid

        {-----------------------------------------------------------------------
                                 Delegation Rewards
        -----------------------------------------------------------------------}

        , putDelegationRewardBalance =
            \(PrimaryKey wid) (Quantity amt) -> ExceptT $ do
            selectWallet wid >>= \case
                Nothing -> pure $ Left $ ErrNoSuchWallet wid
                Just _  -> Right <$> repsert
                    (DelegationRewardKey wid)
                    (DelegationReward wid amt)

        , readDelegationRewardBalance =
            \(PrimaryKey wid) ->
                maybe minBound (Quantity . rewardAccountBalance . entityVal) <$>
                selectFirst [RewardWalletId ==. wid] []

        {-----------------------------------------------------------------------
                                     ACID Execution
        -----------------------------------------------------------------------}

        , atomically = runQuery

        })

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

toWalletDelegationStatus
    :: DelegationCertificate
    -> W.WalletDelegationStatus
toWalletDelegationStatus = \case
    DelegationCertificate _ _ Nothing ->
        W.NotDelegating
    DelegationCertificate _ _ (Just pool) ->
        W.Delegating pool

mkWalletEntity :: W.WalletId -> W.WalletMetadata -> W.GenesisParameters -> Wallet
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

blockHeaderFromEntity :: Checkpoint -> W.BlockHeader
blockHeaderFromEntity cp = W.BlockHeader
    { slotNo = checkpointSlot cp
    , blockHeight = Quantity (checkpointBlockHeight cp)
    , headerHash = getBlockId (checkpointHeaderHash cp)
    , parentHeaderHash = getBlockId (checkpointParentHash cp)
    }

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
    -> (k 'RootK XPrv, W.Hash "encryption")
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
    -> (k 'RootK XPrv, W.Hash "encryption")
privateKeyFromEntity (PrivateKey _ k h) =
    unsafeDeserializeXPrv (k, h)

mkCheckpointEntity
    :: W.WalletId
    -> W.Wallet s
    -> (Checkpoint, [UTxO], [UTxOToken])
mkCheckpointEntity wid wal =
    (cp, utxo, utxoTokens)
  where
    header = W.currentTip wal
    sl = header ^. #slotNo
    (Quantity bh) = header ^. #blockHeight
    cp = Checkpoint
        { checkpointWalletId = wid
        , checkpointSlot = sl
        , checkpointParentHash = BlockId (header ^. #parentHeaderHash)
        , checkpointHeaderHash = BlockId (header ^. #headerHash)
        , checkpointBlockHeight = bh
        }
    utxo =
        [ UTxO wid sl (TxId input) ix addr (TokenBundle.getCoin tokens)
        | (W.TxIn input ix, W.TxOut addr tokens) <- utxoMap
        ]
    utxoTokens =
        [ UTxOToken wid sl (TxId input) ix policy token quantity
        | (W.TxIn input ix, W.TxOut {tokens}) <- utxoMap
        , let tokenList = snd (TokenBundle.toFlatList tokens)
        , (AssetId policy token, quantity) <- tokenList
        ]
    utxoMap = Map.assocs (W.getUTxO (W.utxo wal))

-- note: TxIn records must already be sorted by order
-- and TxOut records must already by sorted by index.
checkpointFromEntity
    :: Checkpoint
    -> [(UTxO, [UTxOToken])]
    -> s
    -> W.Wallet s
checkpointFromEntity cp utxo =
    W.unsafeInitWallet utxo' header
  where
    (Checkpoint
        _walletId
        slot
        (BlockId headerHash)
        (BlockId parentHeaderHash)
        bh
        ) = cp
    header = (W.BlockHeader slot (Quantity bh) headerHash parentHeaderHash)
    utxo' = W.UTxO . Map.fromList $
        [ (W.TxIn input ix, W.TxOut addr (mkTokenBundle coin tokens))
        | (UTxO _ _ (TxId input) ix addr coin, tokens) <- utxo
        ]
    mkTokenBundle coin tokens =
        TokenBundle.fromFlatList coin (mkTokenEntry <$> tokens)
    mkTokenEntry token =
        ( AssetId (utxoTokenPolicyId token) (utxoTokenName token)
        , utxoTokenQuantity token
        )

mkTxHistory
    :: W.WalletId
    -> [(W.Tx, W.TxMeta)]
    -> ([TxMeta], [TxIn], [TxOut], [TxOutToken], [TxWithdrawal])
mkTxHistory wid txs = flatTxHistory
    [ ( mkTxMetaEntity wid txid (W.fee tx) (W.metadata tx) derived
      , mkTxInputsOutputs (txid, tx)
      , mkTxWithdrawals (txid, tx)
      )
    | (tx, derived) <- txs
    , let txid = W.txId tx
    ]
  where
    -- | Make flat lists of entities from the result of 'mkTxHistory'.
    flatTxHistory
        :: [(TxMeta, ([TxIn], [(TxOut, [TxOutToken])]), [TxWithdrawal])]
        -> ([TxMeta], [TxIn], [TxOut], [TxOutToken], [TxWithdrawal])
    flatTxHistory entities =
        ( map (\(a,_,_) -> a) entities
        , concatMap (fst . (\(_,b,_) -> b)) entities
        , fst <$> concatMap (snd . (\(_,b,_) -> b)) entities
        , snd =<< concatMap (snd . (\(_,b,_) -> b)) entities
        , concatMap (\(_,_,c) -> c) entities
        )

mkTxInputsOutputs
    :: (W.Hash "Tx", W.Tx)
    -> ([TxIn], [(TxOut, [TxOutToken])])
mkTxInputsOutputs tx =
    ( (dist mkTxIn . ordered W.resolvedInputs) tx
    , (dist mkTxOut . ordered W.outputs) tx )
  where
    mkTxIn tid (ix, (txIn, amt)) = TxIn
        { txInputTxId = TxId tid
        , txInputOrder = ix
        , txInputSourceTxId = TxId (W.inputId txIn)
        , txInputSourceIndex = W.inputIx txIn
        , txInputSourceAmount = amt
        }
    mkTxOut tid (ix, txOut) = (out, tokens)
      where
        out = TxOut
            { txOutputTxId = TxId tid
            , txOutputIndex = ix
            , txOutputAddress = view #address txOut
            , txOutputAmount = W.txOutCoin txOut
            }
        tokens = mkTxOutToken tid ix <$>
            snd (TokenBundle.toFlatList $ view #tokens txOut)
    mkTxOutToken tid ix (AssetId policy token, quantity) = TxOutToken
        { txOutTokenTxId = TxId tid
        , txOutTokenTxIndex = ix
        , txOutTokenPolicyId = policy
        , txOutTokenName = token
        , txOutTokenQuantity = quantity
        }
    ordered f = fmap (zip [0..] . f)
    -- | Distribute `a` accross many `b`s using the given function.
    -- >>> dist TxOut (addr, [Coin 1, Coin 42, Coin 14])
    -- [TxOut addr (Coin 1), TxOut addr (Coin 42), TxOut addr (Coin 14)]
    dist :: (a -> b -> c) -> (a, [b]) -> [c]
    dist f (a, bs) = [f a b | b <- bs]

mkTxWithdrawals
    :: (W.Hash "Tx", W.Tx)
    -> [TxWithdrawal]
mkTxWithdrawals (txid, tx) =
    mkTxWithdrawal <$> Map.toList (tx ^. #withdrawals)
  where
    txWithdrawalTxId = TxId txid
    mkTxWithdrawal (txWithdrawalAccount, txWithdrawalAmount) =
        TxWithdrawal
            { txWithdrawalTxId
            , txWithdrawalAccount
            , txWithdrawalAmount
            }

mkTxMetaEntity
    :: W.WalletId
    -> W.Hash "Tx"
    -> Maybe W.Coin
    -> Maybe W.TxMetadata
    -> W.TxMeta
    -> TxMeta
mkTxMetaEntity wid txid mfee meta derived = TxMeta
    { txMetaTxId = TxId txid
    , txMetaWalletId = wid
    , txMetaStatus = derived ^. #status
    , txMetaDirection = derived ^. #direction
    , txMetaSlot = derived ^. #slotNo
    , txMetaBlockHeight = getQuantity (derived ^. #blockHeight)
    , txMetaAmount = getQuantity (derived ^. #amount)
    , txMetaFee = fromIntegral . W.getCoin <$> mfee
    , txMetaSlotExpires = derived ^. #expiry
    , txMetaData = meta
    }

-- note: TxIn records must already be sorted by order
-- and TxOut records must already be sorted by index
txHistoryFromEntity
    :: Monad m
    => TimeInterpreter m
    -> W.BlockHeader
    -> [TxMeta]
    -> [(TxIn, Maybe (TxOut, [TxOutToken]))]
    -> [(TxOut, [TxOutToken])]
    -> [TxWithdrawal]
    -> m [W.TransactionInfo]
txHistoryFromEntity ti tip metas ins outs ws =
    mapM mkItem metas
  where
    startTime' = interpretQuery ti . slotToUTCTime
    mkItem m = mkTxWith (txMetaTxId m) (txMetaFee m) (txMetaData m) (mkTxDerived m)
    mkTxWith txid mfee meta derived = do
        t <- startTime' (derived ^. #slotNo)
        return $ W.TransactionInfo
            { W.txInfoId =
                getTxId txid
            , W.txInfoFee =
                W.Coin . fromIntegral <$> mfee
            , W.txInfoInputs =
                map mkTxIn $ filter ((== txid) . txInputTxId . fst) ins
            , W.txInfoOutputs =
                map mkTxOut $ filter ((== txid) . txOutputTxId . fst) outs
            , W.txInfoWithdrawals =
                Map.fromList
                    $ map mkTxWithdrawal
                    $ filter ((== txid) . txWithdrawalTxId) ws
            , W.txInfoMeta =
                derived
            , W.txInfoMetadata =
                meta
            , W.txInfoDepth =
                Quantity $ fromIntegral $ if tipH > txH then tipH - txH else 0
            , W.txInfoTime =
                t
            }
      where
        txH  = getQuantity (derived ^. #blockHeight)
        tipH = getQuantity (tip ^. #blockHeight)
    mkTxIn (tx, out) =
        ( W.TxIn
            { W.inputId = getTxId (txInputSourceTxId tx)
            , W.inputIx = txInputSourceIndex tx
            }
        , txInputSourceAmount tx
        , mkTxOut <$> out
        )
    mkTxOut (out, tokens) = W.TxOut
        { W.address = txOutputAddress out
        , W.tokens = TokenBundle.fromFlatList
            (txOutputAmount out)
            (mkTxOutToken <$> tokens)
        }
    mkTxOutToken token =
        ( AssetId (txOutTokenPolicyId token) (txOutTokenName token)
        , txOutTokenQuantity token
        )
    mkTxWithdrawal w =
        ( txWithdrawalAccount w
        , txWithdrawalAmount w
        )
    mkTxDerived m = W.TxMeta
        { W.status = txMetaStatus m
        , W.direction = txMetaDirection m
        , W.slotNo = txMetaSlot m
        , W.blockHeight = Quantity (txMetaBlockHeight m)
        , W.amount = Quantity (txMetaAmount m)
        , W.expiry = txMetaSlotExpires m
        }

mkProtocolParametersEntity
    :: W.WalletId
    -> W.ProtocolParameters
    -> ProtocolParameters
mkProtocolParametersEntity wid pp =
    ProtocolParameters wid fp (getQuantity mx) dl desiredPoolNum minUTxO keyDep epochNo
  where
    (W.ProtocolParameters
        (W.DecentralizationLevel dl)
        (W.TxParameters fp mx)
        desiredPoolNum
        minUTxO
        epochNo
        keyDep
        ) = pp

protocolParametersFromEntity
    :: ProtocolParameters
    -> W.ProtocolParameters
protocolParametersFromEntity (ProtocolParameters _ fp mx dl poolNum minUTxO keyDep epochNo) =
    W.ProtocolParameters
        (W.DecentralizationLevel dl)
        (W.TxParameters fp (Quantity mx))
        poolNum
        minUTxO
        epochNo
        keyDep

genesisParametersFromEntity
    :: Wallet
    -> W.GenesisParameters
genesisParametersFromEntity (Wallet _ _ _ _ _ hash startTime) =
    W.GenesisParameters
        { W.getGenesisBlockHash = coerce (getBlockId hash)
        , W.getGenesisBlockDate = W.StartTime startTime
        }

{-------------------------------------------------------------------------------
                                   DB Queries
-------------------------------------------------------------------------------}

selectWallet :: MonadIO m => W.WalletId -> SqlPersistT m (Maybe Wallet)
selectWallet wid =
    fmap entityVal <$> selectFirst [WalId ==. wid] []

insertCheckpoint
    :: forall s. (PersistState s)
    => W.WalletId
    -> W.Wallet s
    -> SqlPersistT IO ()
insertCheckpoint wid wallet = do
    let (cp, utxo, utxoTokens) = mkCheckpointEntity wid wallet
    let sl = (W.currentTip wallet) ^. #slotNo
    deleteCheckpoints wid [CheckpointSlot ==. sl]
    insert_ cp
    dbChunked insertMany_ utxo
    dbChunked insertMany_ utxoTokens
    insertState (wid, sl) (W.getState wallet)

-- | Delete one or all checkpoints associated with a wallet.
deleteCheckpoints
    :: W.WalletId
    -> [Filter Checkpoint]
    -> SqlPersistT IO ()
deleteCheckpoints wid filters = do
    deleteCascadeWhere ((CheckpointWalletId ==. wid) : filters)

-- | Prune checkpoints in the database to keep it tidy
pruneCheckpoints
    :: W.WalletId
    -> Quantity "block" Word32
    -> W.Wallet s
    -> SqlPersistT IO ()
pruneCheckpoints wid epochStability cp = do
    let height = cp ^. #currentTip . #blockHeight
    let cfg = defaultSparseCheckpointsConfig epochStability
    let cps = sparseCheckpoints cfg height
    deleteCheckpoints wid [ CheckpointBlockHeight /<-. cps ]

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

-- | Insert multiple transactions, removing old instances first.
putTxs
    :: [TxMeta]
    -> [TxIn]
    -> [TxOut]
    -> [TxOutToken]
    -> [TxWithdrawal]
    -> SqlPersistT IO ()
putTxs metas txins txouts txoutTokens ws = do
    dbChunked repsertMany
        [ (TxMetaKey txMetaTxId txMetaWalletId, m)
        | m@TxMeta{..} <- metas]
    dbChunked repsertMany
        [ (TxInKey txInputTxId txInputSourceTxId txInputSourceIndex, i)
        | i@TxIn{..} <- txins ]
    dbChunked repsertMany
        [ (TxOutKey txOutputTxId txOutputIndex, o)
        | o@TxOut{..} <- txouts ]
    dbChunked repsertMany
        [ ( TxOutTokenKey
            txOutTokenTxId
            txOutTokenTxIndex
            txOutTokenPolicyId
            txOutTokenName
          , o
          )
        | o@TxOutToken{..} <- txoutTokens ]
    dbChunked repsertMany
        [ (TxWithdrawalKey txWithdrawalTxId txWithdrawalAccount, w)
        | w@TxWithdrawal{..} <- ws ]

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
    deleteCascadeWhere ((CertWalletId ==. wid) : filters)

selectUTxO
    :: Checkpoint
    -> SqlPersistT IO [(UTxO, [UTxOToken])]
selectUTxO cp = do
    utxos <- fmap entityVal <$>
        selectList
            [ UtxoWalletId ==. checkpointWalletId cp
            , UtxoSlot ==. checkpointSlot cp
            ] []
    forM utxos $ \utxo -> do
        (utxo, ) . fmap entityVal <$> selectList
            [ UtxoTokenWalletId ==. utxoWalletId utxo
            , UtxoTokenSlot ==. utxoSlot utxo
            , UtxoTokenTxId ==. utxoInputId utxo
            , UtxoTokenTxIndex ==. utxoInputIndex utxo
            ] []

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
selectTxs
    :: [TxId]
    -> SqlPersistT IO
        ( [(TxIn, Maybe (TxOut, [TxOutToken]))]
        , [(TxOut, [TxOutToken])]
        , [TxWithdrawal]
        )
selectTxs = fmap concatUnzip . mapM select . chunksOf chunkSize
  where
    select txids = do
        inputs <- fmap entityVal <$> selectList
            [TxInputTxId <-. txids]
            [Asc TxInputTxId, Asc TxInputOrder]

        resolvedInputs <- fmap toOutputMap $
            combineChunked inputs $ \inputsChunk ->
                traverse readTxOutTokens . fmap entityVal =<<
                    selectList
                        [TxOutputTxId <-. (txInputSourceTxId <$> inputsChunk)]
                        [Asc TxOutputTxId, Asc TxOutputIndex]

        outputs <- traverse readTxOutTokens . fmap entityVal =<<
            selectList
                [TxOutputTxId <-. txids]
                [Asc TxOutputTxId, Asc TxOutputIndex]

        withdrawals <- fmap entityVal <$> selectList
            [TxWithdrawalTxId <-. txids]
            []

        pure
            ( inputs `resolveWith` resolvedInputs
            , outputs
            , withdrawals
            )

    -- Fetch the complete set of tokens associated with a TxOut.
    --
    readTxOutTokens :: TxOut -> SqlPersistT IO (TxOut, [TxOutToken])
    readTxOutTokens out = (out,) . fmap entityVal <$> selectList
        [ TxOutTokenTxId ==. txOutputTxId out
        , TxOutTokenTxIndex ==. txOutputIndex out
        ]
        []

    toOutputMap
        :: [(TxOut, [TxOutToken])]
        -> Map (TxId, Word32) (TxOut, [TxOutToken])
    toOutputMap = Map.fromList . fmap toEntry
      where
        toEntry (out, tokens) = (key, (out, tokens))
          where
            key = (txOutputTxId out, txOutputIndex out)

    resolveWith :: [TxIn] -> Map (TxId, Word32) txOut -> [(TxIn, Maybe txOut)]
    resolveWith inputs resolvedInputs =
        [ (i, Map.lookup key resolvedInputs)
        | i <- inputs
        , let key = (txInputSourceTxId i, txInputSourceIndex i)
        ]

    concatUnzip :: [([a], [b], [c])] -> ([a], [b], [c])
    concatUnzip =
        (\(a, b, c) -> (concat a, concat b, concat c))
        . unzip3

-- | Split a query's input values into chunks, run multiple smaller queries,
-- and then concatenate the results afterwards. Used to avoid "too many SQL
-- variables" errors for "SELECT IN" queries.
combineChunked :: [a] -> ([a] -> SqlPersistT IO [b]) -> SqlPersistT IO [b]
combineChunked xs f = concatMapM f $ chunksOf chunkSize xs

selectTxHistory
    :: W.Wallet s
    -> TimeInterpreter IO
    -> W.WalletId
    -> Maybe (Quantity "lovelace" Natural)
    -> W.SortOrder
    -> [Filter TxMeta]
    -> SqlPersistT IO [W.TransactionInfo]
selectTxHistory cp ti wid minWithdrawal order conditions = do
    let txMetaFilter = (TxMetaWalletId ==. wid):conditions
    metas <- case minWithdrawal of
        Nothing -> fmap entityVal <$> selectList txMetaFilter sortOpt
        Just inf -> do
            let coin = W.Coin $ fromIntegral $ getQuantity inf
            txids <- fmap (txWithdrawalTxId . entityVal)
                <$> selectList [ TxWithdrawalAmount >=. coin ] []
            ms <- combineChunked (nub txids) (\chunk -> selectList
              ((TxMetaTxId <-. chunk):txMetaFilter) [])
            let sortTxId = case order of
                    W.Ascending -> sortOn (Down . txMetaTxId)
                    W.Descending -> sortOn txMetaTxId
            let sortSlot = case order of
                    W.Ascending -> sortOn txMetaSlot
                    W.Descending -> sortOn (Down . txMetaSlot)
            pure $ sortSlot $ sortTxId $ fmap entityVal ms

    let txids = map txMetaTxId metas
    (ins, outs, ws) <- selectTxs txids

    let tip = W.currentTip cp

    liftIO $ txHistoryFromEntity ti tip metas ins outs ws
  where
    -- Note: there are sorted indices on these columns.
    -- The secondary sort by TxId is to make the ordering stable
    -- so that testing with random data always works.
    sortOpt = case order of
        W.Ascending -> [Asc TxMetaSlot, Desc TxMetaTxId]
        W.Descending -> [Desc TxMetaSlot, Asc TxMetaTxId]

selectTxMeta
    :: W.WalletId
    -> W.Hash "Tx"
    -> SqlPersistT IO (Maybe TxMeta)
selectTxMeta wid tid =
    fmap entityVal <$> selectFirst
        [ TxMetaWalletId ==. wid, TxMetaTxId ==. (TxId tid)]
        [ Desc TxMetaSlot ]

-- | Delete the transaction, but only if it's not in ledger.
-- Returns non-zero if this was a success.
deletePendingOrExpiredTx
    :: W.WalletId
    -> W.Hash "Tx"
    -> SqlPersistT IO Int
deletePendingOrExpiredTx wid tid = do
    let filt = [ TxMetaWalletId ==. wid, TxMetaTxId ==. (TxId tid) ]
    selectFirst ((TxMetaStatus ==. W.InLedger):filt) [] >>= \case
        Just _ -> pure 0  -- marked in ledger - refuse to delete
        Nothing -> fromIntegral <$> deleteWhereCount
            ((TxMetaStatus <-. [W.Pending, W.Expired]):filt)

-- | Mutates all pending transaction entries which have exceeded their TTL so
-- that their status becomes expired. Transaction expiry is not something which
-- can be rolled back.
updatePendingTxForExpiryQuery
    :: W.WalletId
    -> W.SlotNo
    -> SqlPersistT IO ()
updatePendingTxForExpiryQuery wid tip =
    updateWhere isExpired [TxMetaStatus =. W.Expired]
  where
    isExpired =
        [ TxMetaWalletId ==. wid
        , TxMetaStatus ==. W.Pending
        , TxMetaSlotExpires <=. Just tip ]

selectPrivateKey
    :: (MonadIO m, PersistPrivateKey (k 'RootK))
    => W.WalletId
    -> SqlPersistT m (Maybe (k 'RootK XPrv, W.Hash "encryption"))
selectPrivateKey wid = do
    keys <- selectFirst [PrivateKeyWalletId ==. wid] []
    pure $ (privateKeyFromEntity . entityVal) <$> keys

selectProtocolParameters
    :: MonadIO m
    => W.WalletId
    -> SqlPersistT m (Maybe W.ProtocolParameters)
selectProtocolParameters wid = do
    pp <- selectFirst [ProtocolParametersWalletId ==. wid] []
    pure $ (protocolParametersFromEntity . entityVal) <$> pp

selectGenesisParameters
    :: MonadIO m
    => W.WalletId
    -> SqlPersistT m (Maybe W.GenesisParameters)
selectGenesisParameters wid = do
    gp <- selectFirst [WalId ==. wid] []
    pure $ (genesisParametersFromEntity . entityVal) <$> gp

-- | Find the nearest 'Checkpoint' that is either at the given point or before.
findNearestPoint
    :: W.WalletId
    -> W.SlotNo
    -> SqlPersistT IO (Maybe W.SlotNo)
findNearestPoint wid sl =
    fmap (checkpointSlot . entityVal) <$> selectFirst
        [CheckpointWalletId ==. wid, CheckpointSlot <=. sl]
        [Desc CheckpointSlot]

-- | A fatal exception thrown when trying to rollback but, there's no checkpoint
-- to rollback to. The database maintain the invariant that there's always at
-- least one checkpoint (the first one made for genesis) present in the
-- database.
--
-- If we don't find any checkpoint, it means that this invariant has been
-- violated.
data ErrRollbackTo = ErrNoOlderCheckpoint W.WalletId W.SlotNo deriving (Show)
instance Exception ErrRollbackTo

{-------------------------------------------------------------------------------
                     DB queries for address discovery state
-------------------------------------------------------------------------------}

-- | Get a @(WalletId, SlotNo)@ pair from the checkpoint table, for use with
-- 'insertState' and 'selectState'.
checkpointId :: Checkpoint -> (W.WalletId, W.SlotNo)
checkpointId cp = (checkpointWalletId cp, checkpointSlot cp)

-- | Functions for saving/loading the wallet's address discovery state into
-- SQLite.
class PersistState s where
    -- | Store the state for a checkpoint.
    insertState :: (W.WalletId, W.SlotNo) -> s -> SqlPersistT IO ()
    -- | Load the state for a checkpoint.
    selectState :: (W.WalletId, W.SlotNo) -> SqlPersistT IO (Maybe s)

{-------------------------------------------------------------------------------
                          Sequential address discovery
-------------------------------------------------------------------------------}

-- piggy-back on SeqState existing instance, to simulate the same behavior.
instance PersistState (Seq.SeqState n k) => PersistState (Seq.SeqAnyState n k p)
  where
    insertState (wid, sl) = insertState (wid, sl) . Seq.innerState
    selectState (wid, sl) = fmap Seq.SeqAnyState <$> selectState (wid, sl)

instance
    ( Eq (k 'AccountK XPub)
    , PersistPublicKey (k 'AccountK)
    , PersistPublicKey (k 'AddressK)
    , MkKeyFingerprint k (Proxy n, k 'AddressK XPub)
    , PaymentAddress n k
    , SoftDerivation k
    ) => PersistState (Seq.SeqState n k) where
    insertState (wid, sl) st = do
        let (intPool, extPool, sPool) =
                (Seq.internalPool st, Seq.externalPool st, Seq.scriptPool st)
        let (accountXPub, _) = W.invariant
                "Internal & External pool use different account public keys!"
                (Seq.accountPubKey intPool, Seq.accountPubKey extPool)
                (uncurry (==))
        let eGap = Seq.gap extPool
        let iGap = Seq.gap intPool
        let sGap = Seq.verPoolGap sPool
        repsert (SeqStateKey wid) $ SeqState
            { seqStateWalletId = wid
            , seqStateExternalGap = eGap
            , seqStateInternalGap = iGap
            , seqStateAccountXPub = serializeXPub accountXPub
            , seqStateRewardXPub = serializeXPub (Seq.rewardAccountKey st)
            , seqStateDerivationPrefix = Seq.derivationPrefix st
            , seqStateScriptGap = sGap
            }
        insertAddressPool @n wid sl intPool
        insertAddressPool @n wid sl extPool
        insertScriptPool wid sl sPool
        deleteWhere [SeqStatePendingWalletId ==. wid]
        dbChunked
            insertMany_
            (mkSeqStatePendingIxs wid $ Seq.pendingChangeIxs st)

    selectState (wid, sl) = runMaybeT $ do
        st <- MaybeT $ selectFirst [SeqStateWalletId ==. wid] []
        let SeqState _ eGap iGap accountBytes rewardBytes prefix sGap = entityVal st
        let accountXPub = unsafeDeserializeXPub accountBytes
        let rewardXPub = unsafeDeserializeXPub rewardBytes
        intPool <- lift $ selectAddressPool @n wid sl iGap accountXPub
        extPool <- lift $ selectAddressPool @n wid sl eGap accountXPub
        sPool <- lift $ selectScriptPool wid sl sGap accountXPub
        pendingChangeIxs <- lift $ selectSeqStatePendingIxs wid
        pure $ Seq.SeqState intPool extPool pendingChangeIxs rewardXPub prefix sPool

insertAddressPool
    :: forall n k c. (PaymentAddress n k, Typeable c)
    => W.WalletId
    -> W.SlotNo
    -> Seq.AddressPool c k
    -> SqlPersistT IO ()
insertAddressPool wid sl pool =
    void $ dbChunked insertMany_
        [ SeqStateAddress wid sl addr ix (Seq.role @c) state
        | (ix, (addr, state))
        <- zip [0..] (Seq.addresses (liftPaymentAddress @n) pool)
        ]

selectAddressPool
    :: forall (n :: NetworkDiscriminant) k c.
        ( Typeable c
        , SoftDerivation k
        , MkKeyFingerprint k (Proxy n, k 'AddressK XPub)
        , MkKeyFingerprint k W.Address
        )
    => W.WalletId
    -> W.SlotNo
    -> Seq.AddressPoolGap
    -> k 'AccountK XPub
    -> SqlPersistT IO (Seq.AddressPool c k)
selectAddressPool wid sl gap xpub = do
    addrs <- fmap entityVal <$> selectList
        [ SeqStateAddressWalletId ==. wid
        , SeqStateAddressSlot ==. sl
        , SeqStateAddressRole ==. Seq.role @c
        ] [Asc SeqStateAddressIndex]
    pure $ addressPoolFromEntity addrs
  where
    addressPoolFromEntity
        :: [SeqStateAddress]
        -> Seq.AddressPool c k
    addressPoolFromEntity addrs
        = Seq.mkAddressPool @n @c @k xpub gap
        $ map (\x -> (seqStateAddressAddress x, seqStateAddressStatus x)) addrs

mkSeqStatePendingIxs :: W.WalletId -> Seq.PendingIxs -> [SeqStatePendingIx]
mkSeqStatePendingIxs wid =
    fmap (SeqStatePendingIx wid . W.getIndex) . Seq.pendingIxsToList

selectSeqStatePendingIxs :: W.WalletId -> SqlPersistT IO Seq.PendingIxs
selectSeqStatePendingIxs wid =
    Seq.pendingIxsFromList . fromRes <$> selectList
        [SeqStatePendingWalletId ==. wid]
        [Desc SeqStatePendingIxIndex]
  where
    fromRes = fmap (W.Index . seqStatePendingIxIndex . entityVal)

insertScriptPool
    :: W.WalletId
    -> W.SlotNo
    -> Seq.VerificationKeyPool k
    -> SqlPersistT IO ()
insertScriptPool wid sl pool = do
    void $ dbChunked insertMany_ $
        Map.foldMapWithKey toSeqStateKeyHash (Seq.verPoolIndexedKeys pool)
    void $ dbChunked insertMany_ $
        concatMap toDB $ Map.toList (Seq.verPoolKnownScripts pool)
  where
    toSeqStateKeyHash keyHash (ix, state) =
        [SeqStateKeyHash wid sl keyHash (getIndex ix) state]
    toDB (scriptHash, verKeyIxs) =
        zipWith (SeqStateScriptHash wid sl scriptHash . getIndex) verKeyIxs [0..]

instance Ord ScriptHash where
    compare (ScriptHash sh1) (ScriptHash sh2) = compare sh1 sh2

selectScriptPool
    :: forall k . W.WalletId
    -> W.SlotNo
    -> Seq.AddressPoolGap
    -> k 'AccountK XPub
    -> SqlPersistT IO (Seq.VerificationKeyPool k)
selectScriptPool wid sl gap xpub = do
    verKeys <- fmap entityVal <$> selectList
        [ SeqStateKeyHashWalletId ==. wid
        , SeqStateKeyHashSlot ==. sl
        ] [Asc SeqStateKeyHashIndex]
    scripts <- fmap entityVal <$> selectList
        [ SeqStateScriptHashWalletId ==. wid
        , SeqStateScriptHashSlot ==. sl
        ] [Asc SeqStateScriptHashScriptHash, Desc SeqStateScriptHashKeyIndexInArray]
    pure $ scriptPoolFromEntities verKeys scripts
  where
    updateVerKeyMap
        :: SeqStateKeyHash
        -> Map KeyHash (Index 'Soft 'ScriptK, W.AddressState)
        -> Map KeyHash (Index 'Soft 'ScriptK, W.AddressState)
    updateVerKeyMap x = Map.insert (seqStateKeyHashKeyHash x)
        (W.Index $ seqStateKeyHashIndex x, seqStateKeyHashStatus x)
    verKeyMap =
        L.foldr updateVerKeyMap Map.empty
    updateKnowScript
        :: SeqStateScriptHash
        -> Map ScriptHash [Index 'Soft 'ScriptK]
        -> Map ScriptHash [Index 'Soft 'ScriptK]
    updateKnowScript =
        (\(sh, keyIndex) -> Map.insertWith (++) sh [keyIndex] ) .
        (\scriptH -> (seqStateScriptHashScriptHash scriptH
        , W.Index $ seqStateScriptHashVerificationKeyIndex scriptH) )
    knownScripts =
        L.foldr updateKnowScript Map.empty
    scriptPoolFromEntities
        :: [SeqStateKeyHash]
        -> [SeqStateScriptHash]
        -> Seq.VerificationKeyPool k
    scriptPoolFromEntities verKeys scripts
        = Seq.unsafeVerificationKeyPool xpub gap
          (verKeyMap verKeys) (knownScripts scripts)

{-------------------------------------------------------------------------------
                          HD Random address discovery
-------------------------------------------------------------------------------}

-- piggy-back on RndState existing instance, to simulate the same behavior.
instance PersistState (Rnd.RndAnyState n p) where
    insertState (wid, sl) = insertState (wid, sl) . Rnd.innerState
    selectState (wid, sl) = fmap Rnd.RndAnyState <$> selectState (wid, sl)

-- Persisting 'RndState' requires that the wallet root key has already been
-- added to the database with 'putPrivateKey'. Unlike sequential AD, random
-- address discovery requires a root key to recognize addresses.
instance PersistState (Rnd.RndState t) where
    insertState (wid, sl) st = do
        let ix = W.getIndex (st ^. #accountIndex)
        let gen = st ^. #gen
        let pwd = st ^. #hdPassphrase
        repsert (RndStateKey wid) (RndState wid ix gen (HDPassphrase pwd))
        insertRndStateAddresses wid sl (Rnd.addresses st)
        insertRndStatePending wid (Rnd.pendingAddresses st)

    selectState (wid, sl) = runMaybeT $ do
        st <- MaybeT $ selectFirst
            [ RndStateWalletId ==. wid
            ] []
        let (RndState _ ix gen (HDPassphrase pwd)) = entityVal st
        addresses <- lift $ selectRndStateAddresses wid sl
        pendingAddresses <- lift $ selectRndStatePending wid
        pure $ Rnd.RndState
            { hdPassphrase = pwd
            , accountIndex = W.Index ix
            , addresses = addresses
            , pendingAddresses = pendingAddresses
            , gen = gen
            }

insertRndStateAddresses
    :: W.WalletId
    -> W.SlotNo
    -> Map Rnd.DerivationPath (W.Address, W.AddressState)
    -> SqlPersistT IO ()
insertRndStateAddresses wid sl addresses = do
    dbChunked insertMany_
        [ RndStateAddress wid sl accIx addrIx addr st
        | ((W.Index accIx, W.Index addrIx), (addr, st)) <- Map.assocs addresses
        ]

insertRndStatePending
    :: W.WalletId
    -> Map Rnd.DerivationPath W.Address
    -> SqlPersistT IO ()
insertRndStatePending wid addresses = do
    deleteWhere [RndStatePendingAddressWalletId ==. wid]
    dbChunked insertMany_
        [ RndStatePendingAddress wid accIx addrIx addr
        | ((W.Index accIx, W.Index addrIx), addr) <- Map.assocs addresses
        ]

selectRndStateAddresses
    :: W.WalletId
    -> W.SlotNo
    -> SqlPersistT IO (Map Rnd.DerivationPath (W.Address, W.AddressState))
selectRndStateAddresses wid sl = do
    addrs <- fmap entityVal <$> selectList
        [ RndStateAddressWalletId ==. wid
        , RndStateAddressSlot ==. sl
        ] []
    pure $ Map.fromList $ map assocFromEntity addrs
  where
    assocFromEntity (RndStateAddress _ _ accIx addrIx addr st) =
        ((W.Index accIx, W.Index addrIx), (addr, st))

selectRndStatePending
    :: W.WalletId
    -> SqlPersistT IO (Map Rnd.DerivationPath W.Address)
selectRndStatePending wid = do
    addrs <- fmap entityVal <$> selectList
        [ RndStatePendingAddressWalletId ==. wid
        ] []
    pure $ Map.fromList $ map assocFromEntity addrs
  where
    assocFromEntity (RndStatePendingAddress _ accIx addrIx addr) =
        ((W.Index accIx, W.Index addrIx), addr)
