{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Wallet.DB.Store.Submissions.Migrations.V6.Migration
    ( migrateSubmissions
    ) where

import Cardano.DB.Sqlite
    ( ReadDBHandle
    , dbConn
    )
import Cardano.Wallet.DB.Migration
    ( Migration
    , mkMigration
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.TxExtended
    ( getTxExtended
    )
import Cardano.Wallet.Primitive.Types.Tx
    ( Tx (..)
    , sealedTxFromBytes
    , unsafeReadTx
    )
import Cardano.Wallet.Primitive.Types.Tx.TxExtended
    ( TxExtended (walletTx)
    )
import Cardano.Wallet.Primitive.Types.Tx.TxIn
    ( TxIn (..)
    )
import Control.Exception
    ( onException
    )
import Control.Monad
    ( forM_
    , void
    )
import Control.Monad.Reader
    ( ReaderT (..)
    )
import Data.Text
    ( Text
    )
import Data.Text.Class
    ( ToText (..)
    )
import Database.Persist.Types
    ( PersistValue (..)
    )
import Prelude
import qualified Cardano.Wallet.Read as Read
import qualified Data.Text as T
import qualified Data.ByteString as BS

import qualified Database.Sqlite as Sqlite

-- | Introduce exact wallet-scoped submission state beside legacy pool records.
-- Live V5 rows are decoded before their exact active claims are written. Any
-- malformed body, identity mismatch, or conflicting claim aborts the one SQL
-- transaction, allowing the migration framework to restore its backup.
migrateSubmissions :: Migration (ReadDBHandle IO) 5 6
migrateSubmissions = mkMigration $ ReaderT $ \db -> do
    let conn = dbConn db
    execute conn "BEGIN IMMEDIATE"
    let migration = do
            execute conn
                "CREATE TABLE dapp_submission (\
                \wallet_id BLOB NOT NULL, tx_id BLOB NOT NULL, tx BLOB NOT NULL, \
                \expiration INTEGER NULL, authorized INTEGER NOT NULL, status INTEGER NOT NULL, \
                \attempt_generation INTEGER NOT NULL, broadcast_generation INTEGER NULL, \
                \broadcast_started TEXT NULL, acceptance INTEGER NULL, rejection_code TEXT NULL, \
                \PRIMARY KEY (wallet_id, tx_id), \
                \FOREIGN KEY (wallet_id) REFERENCES wallet(wallet_id) ON DELETE CASCADE)"
            execute conn
                "CREATE TABLE dapp_submission_input (\
                \wallet_id BLOB NOT NULL, tx_id BLOB NOT NULL, source_tx_id BLOB NOT NULL, \
                \source_index INTEGER NOT NULL, role INTEGER NOT NULL, active INTEGER NOT NULL, \
                \PRIMARY KEY (wallet_id, tx_id, source_tx_id, source_index, role), \
                \FOREIGN KEY (wallet_id, tx_id) REFERENCES dapp_submission(wallet_id, tx_id) ON DELETE CASCADE)"
            execute conn
                "CREATE UNIQUE INDEX dapp_submission_claim ON dapp_submission_input \
                \(wallet_id, source_tx_id, source_index) WHERE active = 1"
            execute conn
                "INSERT INTO dapp_submission \
                \(wallet_id, tx_id, tx, expiration, authorized, status, attempt_generation, \
                \broadcast_generation, broadcast_started, acceptance, rejection_code) \
                \SELECT wallet_id, tx_id, tx, expiration, 0, \
                \CASE status WHEN 0 THEN 4 WHEN 1 THEN 5 ELSE 6 END, \
                \0, NULL, NULL, acceptance, NULL FROM submissions"
            liveRows <- query conn "SELECT wallet_id, tx_id, tx FROM submissions WHERE status = 0"
            forM_ liveRows $ \case
                [PersistText walletId, PersistText submissionId, PersistByteString bytes] ->
                    insertLiveClaims conn walletId submissionId bytes
                row ->
                    fail
                        $ "invalid V5 live submission row during V6 migration: "
                        <> show row
            execute conn
                "UPDATE database_schema_version SET version = 6 WHERE name = 'schema'"
            execute conn "COMMIT"
    migration `onException` execute conn "ROLLBACK"

insertLiveClaims :: Sqlite.Connection -> Text -> Text -> BS.ByteString -> IO ()
insertLiveClaims conn walletId submissionId bytes = do
    sealed <- either (fail . show) pure $ sealedTxFromBytes bytes
    let tx = case unsafeReadTx sealed of
            Read.EraValue readTx -> walletTx $ getTxExtended readTx
    if toText (txId tx) /= submissionId
        then fail "V5 submission transaction id does not match its sealed body"
        else do
            forM_ (resolvedInputs tx) $ \(txIn, _) ->
                insertClaim conn walletId submissionId 0 txIn
            forM_ (resolvedCollateralInputs tx) $ \(txIn, _) ->
                insertClaim conn walletId submissionId 1 txIn

insertClaim :: Sqlite.Connection -> Text -> Text -> Int -> TxIn -> IO ()
insertClaim conn walletId submissionId role TxIn{inputId, inputIx} =
    execute conn
        $ T.unpack
        $ T.concat
            [ "INSERT INTO dapp_submission_input \
              \(wallet_id, tx_id, source_tx_id, source_index, role, active) VALUES ('"
            , sqlHex walletId
            , "','"
            , sqlHex submissionId
            , "','"
            , sqlHex (toText inputId)
            , "',"
            , T.pack $ show inputIx
            , ","
            , T.pack $ show role
            , ",1)"
            ]

-- All persisted identifiers are hexadecimal. Reject any corrupt value rather
-- than interpolating it into migration SQL.
sqlHex :: Text -> Text
sqlHex value
    | T.all isHex value = value
    | otherwise = error "non-hexadecimal persisted identifier in V6 migration"
  where
    isHex c =
        ('0' <= c && c <= '9')
            || ('a' <= c && c <= 'f')
            || ('A' <= c && c <= 'F')

query :: Sqlite.Connection -> Text -> IO [[PersistValue]]
query conn sql = do
    statement <- Sqlite.prepare conn sql
    let collect rows = Sqlite.step statement >>= \case
            Sqlite.Row -> Sqlite.columns statement >>= \row -> collect (row : rows)
            Sqlite.Done -> pure $ reverse rows
    rows <- collect []
    Sqlite.finalize statement
    pure rows

execute :: Sqlite.Connection -> String -> IO ()
execute conn sql = do
    statement <- Sqlite.prepare conn (T.pack sql)
    void $ Sqlite.step statement
    Sqlite.finalize statement
