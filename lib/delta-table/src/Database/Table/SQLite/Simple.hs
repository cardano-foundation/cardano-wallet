{- |
Copyright: © 2024 Cardano Foundation
License: Apache-2.0

Storing 'Table' in SQLite databases.

This module is meant to be imported qualified, e.g.

@
import qualified Database.Table.SQLite.Simple as Sqlite
@
-}
module Database.Table.SQLite.Simple
    (
      module Database.Table.SQL.Table
    , module Database.Table.SQL.Expr
    , module Database.Table.SQLite.Simple.Exec
    , module Database.Table.SQL.Column
    ) where

import Database.Table.SQL.Column
import Database.Table.SQL.Expr
import Database.Table.SQL.Table
import Database.Table.SQLite.Simple.Exec
