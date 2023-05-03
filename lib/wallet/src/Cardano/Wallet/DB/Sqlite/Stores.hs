-- |
-- Copyright: © 2022 IOHK
-- License: Apache-2.0
--
-- 'Store' implementations that can store various wallet types
-- in an SQLite database using `persistent`.
--
-- FIXME LATER during ADP-1043:
--
-- * Inline the contents of "Cardano.Wallet.DB.Sqlite.CheckpointsOld"
--   into this module
-- * Use 'Table' and 'Embedding' to construct the relevant 'Store'
--   rather than implementing 'loadS', 'writeS', 'updateS' in
--   a monadic fashion.
--   Hide the new implementation behind a feature flag,
--   i.e. "Cardano.Wallet.DB.Sqlite.StoresNew".

module Cardano.Wallet.DB.Sqlite.Stores
    ( PersistAddressBook (..)
    , blockHeaderFromEntity
    -- * Testing
    , mkStoreWallet
    )
    where

import Cardano.Wallet.DB.Store.Checkpoints
