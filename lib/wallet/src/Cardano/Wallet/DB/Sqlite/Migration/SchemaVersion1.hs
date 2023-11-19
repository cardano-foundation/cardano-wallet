{-# LANGUAGE QuasiQuotes #-}

-- |
-- Copyright: © 2023 Cardano Foundation
-- License: Apache-2.0
--
-- Historical record of SchemaVersion 1.
-- Necessary for migrations.

module Cardano.Wallet.DB.Sqlite.Migration.SchemaVersion1
    ( sqlCreateSchemaVersion1TablesIfMissing
    ) where

import Prelude

import Data.String.Interpolate
    ( i
    )
import Data.Text
    ( Text
    , split
    )

-- | List of raw SQL queries that
-- create any missing tables from SchemaVersion 1.
sqlCreateSchemaVersion1TablesIfMissing :: [Text]
sqlCreateSchemaVersion1TablesIfMissing =
  split (== ';')
    [i|
CREATE TABLE IF NOT EXISTS database_schema_version
  (
    name    TEXT PRIMARY KEY,
    version INTEGER NOT NULL
  );

CREATE TABLE IF NOT EXISTS "wallet"
  (
    "wallet_id"                  VARCHAR NOT NULL,
    "creation_time"              TIMESTAMP NOT NULL,
    "name"                       VARCHAR NOT NULL,
    "passphrase_last_updated_at" TIMESTAMP NULL,
    "passphrase_scheme"          VARCHAR NULL,
    "genesis_hash"               VARCHAR NOT NULL,
    "genesis_start"              TIMESTAMP NOT NULL,
    PRIMARY KEY ("wallet_id")
  );

CREATE TABLE IF NOT EXISTS "private_key"
  (
    "wallet_id" VARCHAR NOT NULL,
    "root"      BLOB NOT NULL,
    "hash"      BLOB NOT NULL,
    PRIMARY KEY ("wallet_id"),
    CONSTRAINT "private_keyfk_wallet_private_key"
        FOREIGN KEY("wallet_id")
        REFERENCES "wallet"("wallet_id")
        ON DELETE CASCADE
  );

CREATE TABLE IF NOT EXISTS "tx_meta"
  (
    "tx_id"           VARCHAR NOT NULL,
    "wallet_id"       VARCHAR NOT NULL,
    "status"          VARCHAR NOT NULL,
    "direction"       BOOLEAN NOT NULL,
    "slot"            INTEGER NOT NULL,
    "block_height"    INTEGER NOT NULL,
    "amount"          INTEGER NOT NULL,
    "data"            VARCHAR NULL,
    "slot_expires"    INTEGER NULL,
    "fee"             INTEGER NULL,
    "script_validity" BOOLEAN NULL,
    PRIMARY KEY ("tx_id", "wallet_id"),
    CONSTRAINT "tx_metafk_wallet_tx_meta"
        FOREIGN KEY("wallet_id")
        REFERENCES "wallet"("wallet_id")
        ON DELETE CASCADE
  );

CREATE TABLE IF NOT EXISTS "tx_in"
(
    "tx_id"         VARCHAR NOT NULL,
    "order"         INTEGER NOT NULL,
    "source_tx_id"  VARCHAR NOT NULL,
    "source_index"  INTEGER NOT NULL,
    "source_amount" INTEGER NOT NULL,
    PRIMARY KEY ("tx_id", "source_tx_id", "source_index")
);

CREATE TABLE IF NOT EXISTS "tx_collateral"
(
    "tx_id"         VARCHAR NOT NULL,
    "order"         INTEGER NOT NULL,
    "source_tx_id"  VARCHAR NOT NULL,
    "source_index"  INTEGER NOT NULL,
    "source_amount" INTEGER NOT NULL,
    PRIMARY KEY ("tx_id", "source_tx_id", "source_index")
);

CREATE TABLE IF NOT EXISTS "tx_out"
(
    "tx_id"   VARCHAR NOT NULL,
    "index"   INTEGER NOT NULL,
    "address" VARCHAR NOT NULL,
    "amount"  INTEGER NOT NULL,
    PRIMARY KEY ("tx_id", "index")
);

CREATE TABLE IF NOT EXISTS "tx_out_token"
(
    "tx_id"           VARCHAR NOT NULL,
    "tx_index"        INTEGER NOT NULL,
    "token_policy_id" VARCHAR NOT NULL,
    "token_name"      VARCHAR NOT NULL,
    "token_quantity"  VARCHAR NOT NULL,
    PRIMARY KEY ("tx_id", "tx_index", "token_policy_id", "token_name"),
    CONSTRAINT "tx_out_tokentx_out"
        FOREIGN KEY("tx_id", "tx_index")
        REFERENCES "tx_out"("tx_id", "index")
        ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS "tx_collateral_out"
(
    "tx_id"   VARCHAR NOT NULL,
    "address" VARCHAR NOT NULL,
    "amount"  INTEGER NOT NULL,
    PRIMARY KEY ("tx_id")
);

CREATE TABLE IF NOT EXISTS "tx_collateral_out_token"
(
    "tx_id"           VARCHAR NOT NULL,
    "token_policy_id" VARCHAR NOT NULL,
    "token_name"      VARCHAR NOT NULL,
    "token_quantity"  VARCHAR NOT NULL,
    PRIMARY KEY ("tx_id", "token_policy_id", "token_name"),
    CONSTRAINT "tx_collateral_out_tokentx_collateral_out"
        FOREIGN KEY("tx_id")
        REFERENCES "tx_collateral_out"("tx_id")
        ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS "tx_withdrawal"
  (
    "tx_id"   VARCHAR NOT NULL,
    "amount"  INTEGER NOT NULL,
    "account" VARCHAR NOT NULL,
    PRIMARY KEY ("tx_id", "account")
  );

CREATE TABLE IF NOT EXISTS "local_tx_submission"
  (
     "tx_id"     VARCHAR NOT NULL,
     "wallet_id" VARCHAR NOT NULL,
     "last_slot" INTEGER NOT NULL,
     "tx"        BLOB NOT NULL,
     PRIMARY KEY ("tx_id", "wallet_id"),
     CONSTRAINT "unique_local_tx_submission"
        UNIQUE ("tx_id", "wallet_id"),
     CONSTRAINT "local_tx_submissionfk_tx_meta"
        FOREIGN KEY("tx_id", "wallet_id")
        REFERENCES "tx_meta"("tx_id", "wallet_id")
        ON DELETE CASCADE
  );

CREATE TABLE IF NOT EXISTS "checkpoint"
  (
     "wallet_id"          VARCHAR NOT NULL,
     "slot"               INTEGER NOT NULL,
     "header_hash"        VARCHAR NOT NULL,
     "parent_header_hash" VARCHAR NOT NULL,
     "block_height"       INTEGER NOT NULL,
     PRIMARY KEY ("wallet_id", "slot"),
     CONSTRAINT "checkpointcheckpoint"
        FOREIGN KEY("wallet_id")
        REFERENCES "wallet"("wallet_id")
        ON DELETE CASCADE
  );

CREATE TABLE IF NOT EXISTS "protocol_parameters"
  (
     "wallet_id"              VARCHAR NOT NULL,
     "fee_policy"             VARCHAR NOT NULL,
     "tx_max_size"            INTEGER NOT NULL,
     "decentralization_level" NUMERIC(32, 20) NOT NULL,
     "desired_pool_number"    INTEGER NOT NULL,
     "minimum_utxo_value"     INTEGER NOT NULL,
     "hardfork_epoch"         INTEGER NULL,
     "key_deposit"            INTEGER NOT NULL,
     PRIMARY KEY ("wallet_id"),
     CONSTRAINT "protocol_parametersfk_wallet_protocol_parameters"
        FOREIGN KEY("wallet_id")
        REFERENCES "wallet"("wallet_id")
        ON DELETE CASCADE
  );

CREATE TABLE IF NOT EXISTS "stake_key_certificate"
  (
     "wallet_id" VARCHAR NOT NULL,
     "slot"      INTEGER NOT NULL,
     "type"      VARCHAR NOT NULL,
     PRIMARY KEY ("wallet_id", "slot"),
     CONSTRAINT "stake_key_certificatestake_key_registration"
        FOREIGN KEY("wallet_id")
        REFERENCES "wallet"("wallet_id")
        ON DELETE CASCADE
  );

CREATE TABLE IF NOT EXISTS "delegation_certificate"
  (
     "wallet_id"  VARCHAR NOT NULL,
     "slot"       INTEGER NOT NULL,
     "delegation" VARCHAR NULL,
     PRIMARY KEY ("wallet_id", "slot"),
     CONSTRAINT "delegation_certificatedelegation_certificate"
        FOREIGN KEY("wallet_id")
        REFERENCES "wallet"("wallet_id")
        ON DELETE CASCADE
  );

CREATE TABLE IF NOT EXISTS "delegation_reward"
  (
     "wallet_id"       VARCHAR NOT NULL,
     "account_balance" INTEGER NOT NULL,
     PRIMARY KEY ("wallet_id"),
     CONSTRAINT "delegation_rewarddelegation_reward"
        FOREIGN KEY("wallet_id")
        REFERENCES "wallet"("wallet_id")
        ON DELETE CASCADE
  );

CREATE TABLE IF NOT EXISTS "utxo"
  (
     "wallet_id"      VARCHAR NOT NULL,
     "slot"           INTEGER NOT NULL,
     "input_tx_id"    VARCHAR NOT NULL,
     "input_index"    INTEGER NOT NULL,
     "output_address" VARCHAR NOT NULL,
     "output_coin"    INTEGER NOT NULL,
     PRIMARY KEY ("wallet_id", "slot", "input_tx_id", "input_index"),
     CONSTRAINT "u_tx_outxo"
        FOREIGN KEY("wallet_id", "slot")
        REFERENCES "checkpoint"("wallet_id", "slot")
        ON DELETE CASCADE
  );

CREATE TABLE IF NOT EXISTS "utxo_token"
  (
     "id"              INTEGER PRIMARY KEY,
     "wallet_id"       VARCHAR NOT NULL,
     "slot"            INTEGER NOT NULL,
     "tx_id"           VARCHAR NOT NULL,
     "tx_index"        INTEGER NOT NULL,
     "token_policy_id" VARCHAR NOT NULL,
     "token_name"      VARCHAR NOT NULL,
     "token_quantity"  VARCHAR NOT NULL,
     CONSTRAINT "u_tx_o_tokenutxot"
        FOREIGN KEY("wallet_id", "slot") REFERENCES
         "checkpoint"("wallet_id", "slot") ON DELETE CASCADE
  );

CREATE TABLE IF NOT EXISTS "seq_state"
  (
     "wallet_id"         VARCHAR NOT NULL,
     "external_gap"      INTEGER NOT NULL,
     "internal_gap"      INTEGER NOT NULL,
     "account_xpub"      BLOB NOT NULL,
     "policy_xpub"       BLOB NULL,
     "reward_xpub"       BLOB NOT NULL,
     "derivation_prefix" VARCHAR NOT NULL,
     PRIMARY KEY ("wallet_id"),
     CONSTRAINT "seq_stateseq_state"
        FOREIGN KEY("wallet_id")
        REFERENCES "wallet"("wallet_id")
        ON DELETE CASCADE
  );

CREATE TABLE IF NOT EXISTS "seq_state_address"
  (
     "id"         INTEGER PRIMARY KEY,
     "wallet_id"  VARCHAR NOT NULL,
     "slot"       INTEGER NOT NULL,
     "address"    VARCHAR NOT NULL,
     "address_ix" INTEGER NOT NULL,
     "role"       VARCHAR NOT NULL,
     "status"     VARCHAR NOT NULL,
     CONSTRAINT "seq_state_addressseq_state_address"
        FOREIGN KEY("wallet_id", "slot")
        REFERENCES "checkpoint"("wallet_id", "slot")
        ON DELETE CASCADE
  );

CREATE TABLE IF NOT EXISTS "seq_state_pending"
  (
     "wallet_id"  VARCHAR NOT NULL,
     "pending_ix" INTEGER NOT NULL,
     PRIMARY KEY ("wallet_id", "pending_ix"),
     CONSTRAINT "seq_state_pending_ixseq_state_address_pending"
        FOREIGN KEY("wallet_id")
        REFERENCES "wallet"("wallet_id")
        ON DELETE CASCADE
  );

CREATE TABLE IF NOT EXISTS "rnd_state"
  (
     "wallet_id"     VARCHAR NOT NULL,
     "account_ix"    INTEGER NOT NULL,
     "gen"           VARCHAR NOT NULL,
     "hd_passphrase" BLOB NOT NULL,
     PRIMARY KEY ("wallet_id"),
     CONSTRAINT "rnd_staternd_state"
        FOREIGN KEY("wallet_id")
        REFERENCES "wallet"("wallet_id")
        ON DELETE CASCADE
  );

CREATE TABLE IF NOT EXISTS "rnd_state_address"
  (
     "id"         INTEGER PRIMARY KEY,
     "wallet_id"  VARCHAR NOT NULL,
     "slot"       INTEGER NOT NULL,
     "account_ix" INTEGER NOT NULL,
     "address_ix" INTEGER NOT NULL,
     "address"    VARCHAR NOT NULL,
     "status"     VARCHAR NOT NULL,
     CONSTRAINT "rnd_state_addressrnd_state_address"
        FOREIGN KEY("wallet_id", "slot")
        REFERENCES "checkpoint"("wallet_id", "slot")
        ON DELETE CASCADE
  );

CREATE TABLE IF NOT EXISTS "rnd_state_pending_address"
  (
     "id"         INTEGER PRIMARY KEY,
     "wallet_id"  VARCHAR NOT NULL,
     "account_ix" INTEGER NOT NULL,
     "address_ix" INTEGER NOT NULL,
     "address"    VARCHAR NOT NULL,
     CONSTRAINT "rnd_state_pending_addressrnd_state_pending_address"
        FOREIGN KEY ("wallet_id")
        REFERENCES "wallet"("wallet_id")
        ON DELETE CASCADE
  );

CREATE TABLE IF NOT EXISTS "shared_state"
  (
     "wallet_id"         VARCHAR NOT NULL,
     "account_xpub"      BLOB NOT NULL,
     "pool_gap"          INTEGER NOT NULL,
     "payment_script"    VARCHAR NOT NULL,
     "delegation_script" VARCHAR NULL,
     "derivation_prefix" VARCHAR NOT NULL,
     PRIMARY KEY ("wallet_id"),
     CONSTRAINT "shared_stateshared_state"
        FOREIGN KEY("wallet_id")
        REFERENCES "wallet"("wallet_id")
        ON DELETE CASCADE
  );

CREATE TABLE IF NOT EXISTS "shared_state_pending"
  (
     "wallet_id"  VARCHAR NOT NULL,
     "pending_ix" INTEGER NOT NULL,
     PRIMARY KEY ("wallet_id", "pending_ix"),
     CONSTRAINT "shared_state_pending_ixshared_state_address_pending"
        FOREIGN KEY("wallet_id")
        REFERENCES "wallet"("wallet_id")
        ON DELETE CASCADE
  );

CREATE TABLE IF NOT EXISTS "cosigner_key"
  (
     "id"             INTEGER PRIMARY KEY,
     "wallet_id"      VARCHAR NOT NULL,
     "credential"     VARCHAR NOT NULL,
     "account_xpub"   BLOB NOT NULL,
     "cosigner_index" INTEGER NOT NULL,
     CONSTRAINT "cosigner_keycosigner_key"
        FOREIGN KEY("wallet_id")
        REFERENCES "wallet"("wallet_id")
        ON DELETE CASCADE
  );

CREATE TABLE IF NOT EXISTS "c_b_o_r"
  (
     "tx_id"   VARCHAR NOT NULL,
     "tx_cbor" BLOB NOT NULL,
     "tx_era"  INTEGER NOT NULL,
     PRIMARY KEY ("tx_id")
  )
|]
-- Don't put a semicolon ';' after the last "CREATE" statement,
-- or the list created by 'split' will contain an empty statement.
