## Overview

This package provides a notion of *delta encodings* for data types
via a `Delta` type class.
This package also provides a `DBVar` abstraction that represents
a mutable variable whose value is written using delta encodings.

At the time of this writing, the package is still a preview.
In the future, we may want to split off the delta encoding
abstraction from the code that is more specific to databases.

The main modules are:

* [Data.Delta](src/Data/Delta.hs) — delta encodings
    * Contains the `Delta` class. Each member type `da` represents
      a delta encoding for a base type `Base da`.
      A few basic delta encodings are implemented.
    * Contains the `Embedding` type which represents an embedding
      of one type and its delta encoding into another type.
      `Embedding` can be composed and form a category, but for technical
      reasons, they only implement the `Semigroupoid` class.
* [Data.DBVar](src/Data/DBVar.hs) — delta encodings
    * Contains the `DBVar` mutable variable abstraction.
      The intention is that these variables are stored in-memory
    * Contains the `Store` mutable storage abstraction,
      which can store values and their delta encodings.
      Unlike `DBVar`, a `Store` need not be stored fully in memory.
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
