## Overview

At the time of this writing, the package contains
an early prototype of a `Table` type that represents
database tables, and an example usage can be found in `Data.Chain`.

The main modules are:

* [Data.Table](src/Data/Table.hs) — database
    * Contains the `Table` dataype, which models a table as you would find it in a database.
* [Data.Chain](src/Data/Delta.hs) — (block)chain
    * Contains the `Chain` datatype, which is more specific
      to the problem domain of storing a linear sequence
      of checkpoints. Useful in a blockchain setting.

In addition, this package contains
* [Database.Persist.Delta](src/Database/Persist/Delta.hs) — database
    * Implements a `Store` in an SQLite database for the `Entity` types in the `persistent` package.
    * Implements a `Store` using raw SQL commands.
* [Database.Schema](src/Database/Schema.hs) — database
    * Contains the `IsRow` type class for typed database rows, very similar to the style of the [servant](https://hackage.haskell.org/package/servant) package.
    * Example: `type PersonRow = Table "person" :. Col "name" Text :. Col "birth" Int :. Col "id" Primary`.
