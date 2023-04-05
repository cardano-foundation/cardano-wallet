## Overview

This package provides a `Store` abstraction that represents
a facility for storing one value of a given type.

This package also provides a `DBVar` abstraction that represents
a mutable variable whose value is written using delta types.

## Modules

* [Data.DBVar](src/Data/DBVar.hs)
    * Contains the `DBVar` mutable variable abstraction.
      These variables store a Haskell value in memory,
      but updating the variables is done with a delta encoding
      and also has the side effect of writing the value to a `Store`,
      which typically represents a file or database on the hard disk.
* [Data.Store](src/Data/Store.hs)
    * Contains the `Store` mutable storage abstraction,
      which can store values and their delta encodings.
      Unlike `DBVar`, a `Store` need not be stored fully in memory.
