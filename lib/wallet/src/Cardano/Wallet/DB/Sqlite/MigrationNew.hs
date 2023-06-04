module Cardano.Wallet.DB.Sqlite.MigrationNew
    (
    ) where


import Cardano.Wallet.DB.Migration
    ( Version )

import qualified Cardano.Wallet.DB.Sqlite.MigrationOld as MigrateOld

oldToNewSchemaVersion :: MigrateOld.SchemaVersion -> Version
oldToNewSchemaVersion (MigrateOld.SchemaVersion v) = v

newToOldSchemaVersion :: Version -> MigrateOld.SchemaVersion
newToOldSchemaVersion = MigrateOld.SchemaVersion
