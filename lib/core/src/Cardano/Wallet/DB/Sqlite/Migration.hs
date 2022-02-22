{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Old-style manual migrations of the SQLlite database.
-- These migrations are soon to be removed in favor of
-- a file format with version number.

module Cardano.Wallet.DB.Sqlite.Migration
    ( DefaultFieldValues (..)
    , migrateManually
    , SchemaVersion (..)
    , currentSchemaVersion
    , InvalidDatabaseSchemaVersion (..)
    )
    where

import Prelude

import Cardano.DB.Sqlite
    ( DBField (..)
    , DBLog (..)
    , ManualMigration (..)
    , fieldName
    , fieldType
    , tableName
    )
import Cardano.Wallet.DB.Sqlite.TH
    ( EntityField (..) )
import Cardano.Wallet.Primitive.AddressDerivation.Icarus
    ( IcarusKey )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( ShelleyKey (..) )
import Control.Monad
    ( forM_, void, when )
import Control.Tracer
    ( Tracer, traceWith )
import Data.Functor
    ( (<&>) )
import Data.Maybe
    ( mapMaybe )
import Data.Proxy
    ( Proxy (..) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( ToText (..), fromText )
import Data.Word
    ( Word16 )
import Database.Persist.Class
    ( toPersistValue )
import Database.Persist.Types
    ( PersistValue (..), fromPersistValueText )
import Numeric.Natural
    ( Natural )
import UnliftIO.Exception
    ( Exception, throwIO, throwString )

import qualified Cardano.Wallet.Primitive.AddressDerivation as W
import qualified Cardano.Wallet.Primitive.AddressDiscovery.Sequential as Seq
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Cardano.Wallet.Primitive.Types.Address as W
import qualified Cardano.Wallet.Primitive.Types.Coin as W
import qualified Data.Text as T
import qualified Database.Sqlite as Sqlite

{-------------------------------------------------------------------------------
    Database Migrations
-------------------------------------------------------------------------------}

-- | A set of default field values that can be consulted when performing a
-- database migration.
data DefaultFieldValues = DefaultFieldValues
    { defaultActiveSlotCoefficient :: W.ActiveSlotCoefficient
    , defaultDesiredNumberOfPool :: Word16
    , defaultMinimumUTxOValue :: W.Coin
    , defaultHardforkEpoch :: Maybe W.EpochNo
    , defaultKeyDeposit :: W.Coin
    }

-- | A data-type for capturing column status. Used to be represented as a
-- 'Maybe Bool' which is somewhat confusing to interpret.
data SqlColumnStatus
    = TableMissing
    | ColumnMissing
    | ColumnPresent
    deriving Eq

data TableCreationResult
    = TableCreated
    | TableExisted

newtype SchemaVersion = SchemaVersion Natural
  deriving newtype (Eq, Ord, Read, Show )

data InvalidDatabaseSchemaVersion
    = InvalidDatabaseSchemaVersion
    { expectedVersion :: SchemaVersion
    , actualVersion :: SchemaVersion
    }
    deriving (Show, Eq, Exception)

currentSchemaVersion :: SchemaVersion
currentSchemaVersion = SchemaVersion 1

-- | Executes any manual database migration steps that may be required on
-- startup.
migrateManually
    :: W.WalletKey k
    => Tracer IO DBLog
    -> Proxy k
    -> DefaultFieldValues
    -> [ManualMigration]
migrateManually tr proxy defaultFieldValues =
    ManualMigration <$>
    [ initializeSchemaVersionTable
    , cleanupCheckpointTable
    , assignDefaultPassphraseScheme
    , addDesiredPoolNumberIfMissing
    , addMinimumUTxOValueIfMissing
    , addHardforkEpochIfMissing

      -- FIXME
      -- Temporary migration to fix Daedalus flight wallets. This should
      -- really be removed as soon as we have a fix for the cardano-sl:wallet
      -- currently in production.
    , removeSoftRndAddresses

    , removeOldTxParametersTable
    , addAddressStateIfMissing
    , addSeqStateDerivationPrefixIfMissing
    , renameRoleColumn
    , renameRoleFields
    , updateFeeValueAndAddKeyDeposit
    , addFeeToTransaction
    , moveRndUnusedAddresses
    , cleanupSeqStateTable
    , addPolicyXPubIfMissing
    ]
  where
    initializeSchemaVersionTable :: Sqlite.Connection -> IO ()
    initializeSchemaVersionTable conn =
        createSchemaVersionTableIfMissing conn >>= \case
            TableCreated -> putSchemaVersion conn currentSchemaVersion
            TableExisted -> do
                schemaVersion <- getSchemaVersion conn
                case compare schemaVersion currentSchemaVersion of
                    GT -> throwIO InvalidDatabaseSchemaVersion
                        { expectedVersion = currentSchemaVersion
                        , actualVersion = schemaVersion
                        }
                    LT -> putSchemaVersion conn currentSchemaVersion
                    EQ -> pure ()

    createSchemaVersionTableIfMissing ::
        Sqlite.Connection -> IO TableCreationResult
    createSchemaVersionTableIfMissing conn = do
        res <- runSql conn
            "SELECT name FROM sqlite_master \
            \WHERE type='table' AND name='database_schema_version'"
        case res of
           [] -> TableCreated <$ runSql conn
            "CREATE TABLE database_schema_version\
            \( name TEXT PRIMARY KEY \
            \, version INTEGER NOT NULL \
            \)"
           _ -> pure TableExisted

    putSchemaVersion :: Sqlite.Connection -> SchemaVersion -> IO ()
    putSchemaVersion conn schemaVersion = void $ runSql conn $ T.unwords
        [ "INSERT INTO database_schema_version (name, version)"
        , "VALUES ('schema',"
        , version
        , ") ON CONFLICT (name) DO UPDATE SET version ="
        , version
        ]
      where
        version = T.pack $ show schemaVersion

    getSchemaVersion :: Sqlite.Connection -> IO SchemaVersion
    getSchemaVersion conn =
        runSql conn "SELECT version FROM database_schema_version" >>= \case
            [[PersistInt64 i]] | i >= 0 -> pure $ SchemaVersion $ fromIntegral i
            _ -> throwString "Database metadata table is corrupt"

    -- NOTE
    -- We originally stored script pool gap inside sequential state in the 'SeqState' table,
    -- represented by 'seqStateScriptGap' field. We introduce separate shared wallet state
    -- and want to get rid of this. Also we had two supporting tables which we will drop,
    -- 'SeqStateKeyHash' and 'SeqStateScriptHash'.
    cleanupSeqStateTable :: Sqlite.Connection -> IO ()
    cleanupSeqStateTable conn = do
        let orig = "seq_state"

        -- 1. Drop column from the 'seq_state' table
        isFieldPresentByName conn "seq_state" "script_gap" >>= \case
            ColumnPresent -> do
                let tmp = orig <> "_tmp"

                info <- runSql conn $ getTableInfo orig
                let excluding = ["script_gap"]
                let filtered = mapMaybe (filterColumn excluding) info
                dropColumnOp conn orig tmp filtered

            _ -> return ()

        -- 2. Drop supplementrary tables
        _ <- runSql conn $ dropTable "seq_state_key_hash"
        _ <- runSql conn $ dropTable "seq_state_script_hash"

        return ()

    dropTable :: Text -> Text
    dropTable table = "DROP TABLE IF EXISTS " <> table <> ";"

    getTableInfo :: Text -> Text
    getTableInfo table = "PRAGMA table_info(" <> table <> ");"

    filterColumn :: [Text] -> [PersistValue] -> Maybe [PersistValue]
    filterColumn excluding = \case
        [ _, PersistText colName, PersistText colType, colNull, _, _] ->
            if colName `elem` excluding then
                Nothing
            else
                Just [PersistText colName, PersistText colType, colNull]
        _ ->
            Nothing

    dropColumnOp
        :: Sqlite.Connection
        -> Text
        -> Text
        -> [[PersistValue]]
        -> IO ()
    dropColumnOp conn orig tmp filtered = do
        _ <- runSql conn $ dropTable tmp
        _ <- runSql conn $ createTable tmp filtered
        _ <- runSql conn $ copyTable orig tmp filtered
        _ <- runSql conn $ dropTable orig
        _ <- runSql conn $ renameTable tmp orig

        return ()
      where
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
        renameTable from to = mconcat
            [ "ALTER TABLE ", from, " RENAME TO ", to, ";" ]

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
        let orig = "checkpoint"

        -- 1. Add genesis_hash and genesis_start to the 'wallet' table.
        let field = DBField WalGenesisHash
        isFieldPresent conn field >>= \case
            TableMissing ->
                traceWith tr $ MsgManualMigrationNotNeeded field

            ColumnPresent -> do
                traceWith tr $ MsgManualMigrationNotNeeded field

            ColumnMissing -> do
                [defaults] <- runSql conn $ select ["genesis_hash", "genesis_start"] orig
                let [PersistText genesisHash, PersistText genesisStart] = defaults
                addColumn_ conn True (DBField WalGenesisHash) (quotes genesisHash)
                addColumn_ conn True (DBField WalGenesisStart) (quotes genesisStart)

        -- 2. Drop columns from the 'checkpoint' table
        isFieldPresentByName conn "checkpoint" "genesis_hash" >>= \case
            ColumnPresent -> do
                let tmp = orig <> "_tmp"

                info <- runSql conn $ getTableInfo orig
                let filtered = mapMaybe (filterColumn excluding) info
                      where
                        excluding =
                            [ "genesis_hash", "genesis_start", "fee_policy"
                            , "slot_length", "epoch_length", "tx_max_size"
                            , "epoch_stability", "active_slot_coeff"
                            ]
                dropColumnOp conn orig tmp filtered
            _ -> return ()

      where
        select fields table = mconcat
            [ "SELECT ", T.intercalate ", " fields
            , " FROM ", table
            , " ORDER BY slot ASC LIMIT 1;"
            ]

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
                traceWith tr . MsgExpectedMigration
                    $ MsgManualMigrationNeeded passphraseScheme value
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
                traceWith tr . MsgExpectedMigration
                    $ MsgManualMigrationNeeded rndAccountIx hardLowerBound
                stmt <- Sqlite.prepare conn $ T.unwords
                    [ "DELETE FROM", tableName rndAccountIx
                    , "WHERE", fieldName rndAccountIx, "<", hardLowerBound
                    , ";"
                    ]
                _ <- Sqlite.step stmt
                Sqlite.finalize stmt
      where
        hardLowerBound = toText $ fromEnum $ minBound @(W.Index 'W.Hardened _)
        rndAccountIx   = DBField RndStateAddressAccountIndex

    -- | When we implemented the 'importAddress' and 'createAddress' features,
    -- we mistakenly added all imported addresses in the discovered section and
    -- table of the RndState. This makes them affected by rollbacks, which is
    -- very much an issue. While fixing this, we can also take the opportunity
    -- to move all existing 'unused' addresses from the 'RndStateAddress' to the
    -- 'RndStatePendingAddress' table.
    --
    -- Arguably, the 'status' column is redundant on the 'RndStateAddress' table
    -- because any address in that table must be 'Used', by construction.
    moveRndUnusedAddresses :: Sqlite.Connection -> IO ()
    moveRndUnusedAddresses conn = do
        isFieldPresent conn rndStateAddressStatus >>= \case
            TableMissing -> do
                traceWith tr $ MsgManualMigrationNotNeeded rndStateAddressStatus
            ColumnMissing -> do
                traceWith tr $ MsgManualMigrationNotNeeded rndStateAddressStatus
            ColumnPresent -> do
                let unused = quotes $ toText W.Unused

                [[PersistInt64 n]] <- runSql conn $ T.unwords
                    [ "SELECT COUNT(*)"
                    , "FROM", tableName rndStateAddressStatus
                    , "WHERE", fieldName rndStateAddressStatus, "=", unused
                    , ";"
                    ]

                if n > 0 then do
                    traceWith tr $ MsgManualMigrationNeeded rndStateAddressStatus "-"

                    void $ runSql conn $ T.unwords
                        [ "INSERT INTO", rndStatePendingTable
                        , "(wallet_id, account_ix, address_ix, address)"
                        , "SELECT wallet_id, account_ix, address_ix, address"
                        , "FROM", rndStateDiscoveredTable
                        , "WHERE", fieldName rndStateAddressStatus, "=", unused
                        , ";"
                        ]

                    void $ runSql conn $ T.unwords
                        [ "DELETE FROM", rndStateDiscoveredTable
                        , "WHERE", fieldName rndStateAddressStatus, "=", unused
                        , ";"
                        ]
                else do
                    traceWith tr $ MsgManualMigrationNotNeeded rndStateAddressStatus
      where
        rndStateAddressStatus = DBField RndStateAddressStatus
        rndStateDiscoveredTable = tableName $ DBField RndStateAddressWalletId
        rndStatePendingTable  = tableName $ DBField RndStatePendingAddressWalletId

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
        dropTable' <- Sqlite.prepare conn "DROP TABLE IF EXISTS tx_parameters;"
        void $ Sqlite.stepConn conn dropTable'
        Sqlite.finalize dropTable'

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
            W.keyTypeDescriptor proxy == W.keyTypeDescriptor (Proxy @IcarusKey)
        icarusPrefix = T.pack $ show $ toText
            $ Seq.DerivationPrefix (Seq.purposeBIP44, Seq.coinTypeAda, minBound)

        isShelleyDatabase =
            W.keyTypeDescriptor proxy == W.keyTypeDescriptor (Proxy @ShelleyKey)
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

    -- This migration is rather delicate. Indeed, we need to introduce an
    -- explicit 'fee' on known transactions, so only do we need to add the new
    -- column (easy), but we also need to find the right value for that new
    -- column (delicate).
    --
    -- Note that it is not possible to recover explicit fees on incoming
    -- transactions without having access to the entire ledger (we do not know
    -- the _amount_ from inputs of incoming transactions). Therefore, by
    -- convention it has been decided that incoming transactions will have fee
    -- equals to 0.
    --
    -- For outgoing transaction, it is possible to recalculate fees by
    -- calculating the delta between the total input value minus the total
    -- output value. The delta (inputs - output) is necessarily positive
    -- (by definition of 'outgoing' transactions) and comprised of:
    --
    -- - Fees
    -- - Total deposits if any
    --
    -- To subtract deposit values from fees, we consider that any transaction
    -- that has one or less output and fees greater than the key deposit (or min
    -- utxo value) is a key registration transaction and the key deposit value
    -- can be subtracted from the delta to deduce the fees.
    --
    -- Note that ideally, we would do this in a single `UPDATE ... FROM` query
    -- but the `FROM` syntax is only supported in SQLite >= 3.33 which is only
    -- supported in the latest version of persistent-sqlite (2.11.0.0). So
    -- instead, we query all transactions which require an update in memory,
    -- and update them one by one. This may be quite long on some database but
    -- it is in the end a one-time cost paid on start-up.
    addFeeToTransaction :: Sqlite.Connection -> IO ()
    addFeeToTransaction conn = do
        isFieldPresent conn fieldFee >>= \case
            TableMissing  ->
                traceWith tr $ MsgManualMigrationNotNeeded fieldFee
            ColumnPresent ->
                traceWith tr $ MsgManualMigrationNotNeeded fieldFee
            ColumnMissing -> do
                traceWith tr $ MsgManualMigrationNeeded fieldFee "NULL"

                rows <- fmap unwrapRows (mkQuery >>= runSql conn)

                _ <- runSql conn $ T.unwords
                    [ "ALTER TABLE", tableName fieldFee
                    , "ADD COLUMN", fieldName fieldFee
                    , fieldType fieldFee
                    , ";"
                    ]

                forM_ rows $ \(txid, nOuts, delta) -> do
                    let fee = T.pack $ show $
                            if isKeyRegistration nOuts delta
                            then delta - keyDepositValue
                            else delta

                    runSql conn $ T.unwords
                        [ "UPDATE", tableName fieldFee
                        , "SET", fieldName fieldFee, "=", quotes fee
                        , "WHERE", fieldName fieldTxId, "=", quotes txid
                        , ";"
                        ]
      where
        fieldFee  = DBField TxMetaFee
        fieldTxId = DBField TxMetaTxId

        unwrapRows = fmap $ \[PersistText txid, PersistInt64 nOuts, PersistInt64 delta] ->
            (txid, nOuts, delta)

        isKeyRegistration nOuts delta =
            nOuts <= 1 && delta > max keyDepositValue minUtxoValue

        minUtxoValue
            = fromIntegral
            $ W.unCoin
            $ defaultMinimumUTxOValue defaultFieldValues

        keyDepositValue
            = fromIntegral
            $ W.unCoin
            $ defaultKeyDeposit defaultFieldValues

        mkQuery = isFieldPresent conn (DBField TxWithdrawalTxId) <&> \case
            -- On rather old databases, the tx_withdrawal table doesn't even exists.
            TableMissing -> T.unwords
                [ "SELECT tx_id, num_out, total_in - total_out FROM tx_meta"
                , "JOIN (" <> resolvedInputsQuery <> ") USING (tx_id)"
                , "JOIN (" <> outputsQuery <> ") USING (tx_id)"
                , "WHERE direction = 0"
                , ";"
                ]

            _ -> T.unwords
                [ "SELECT tx_id, num_out, total_in + IFNULL(total_wdrl, 0) - total_out FROM tx_meta"
                , "JOIN (" <> resolvedInputsQuery <> ") USING (tx_id)"
                , "LEFT JOIN (" <> withdrawalsQuery <> ") USING (tx_id)"
                , "JOIN (" <> outputsQuery <> ") USING (tx_id)"
                , "WHERE direction = 0"
                , ";"
                ]

        resolvedInputsQuery = T.unwords
            [ "SELECT tx_in.tx_id, SUM(tx_out.amount) AS total_in FROM tx_in"
            , "JOIN tx_out ON tx_out.tx_id = tx_in.source_tx_id AND tx_out.'index' = tx_in.source_index"
            , "GROUP BY tx_in.tx_id"
            ]

        withdrawalsQuery = T.unwords
            [ "SELECT tx_id, SUM(amount) AS total_wdrl FROM tx_withdrawal"
            , "GROUP BY tx_id"
            ]

        outputsQuery = T.unwords
            [ "SELECT tx_id, SUM(amount) AS total_out, COUNT(*) AS num_out FROM tx_out"
            , "GROUP BY tx_id"
            ]

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

    -- | Adds an 'policy_xpub' column to the 'seq_state'
    -- table if it is missing.
    --
    addPolicyXPubIfMissing :: Sqlite.Connection -> IO ()
    addPolicyXPubIfMissing conn = do
        addColumn_ conn False (DBField SeqStatePolicyXPub) value
      where
        value = "NULL"

    -- | Determines whether a field is present in its parent table.
    isFieldPresent :: Sqlite.Connection -> DBField -> IO SqlColumnStatus
    isFieldPresent conn field =
        isFieldPresentByName conn (tableName field) (fieldName field)

    isFieldPresentByName :: Sqlite.Connection -> Text -> Text -> IO SqlColumnStatus
    isFieldPresentByName conn table field = do
        getTableInfo' <- Sqlite.prepare conn $ mconcat
            [ "SELECT sql FROM sqlite_master "
            , "WHERE type = 'table' "
            , "AND name = '" <> table <> "';"
            ]
        row <- Sqlite.step getTableInfo'
            >> Sqlite.columns getTableInfo'
        Sqlite.finalize getTableInfo'
        pure $ case row of
            [PersistText t]
                | field `T.isInfixOf` t -> ColumnPresent
                | otherwise             -> ColumnMissing
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
