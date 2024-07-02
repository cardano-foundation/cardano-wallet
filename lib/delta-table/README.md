## Overview

This package provides a data type `Table` that represents
database tables. In addition, it provides delta types
such as `DeltaTable` and `DeltaDB` that represent typical
update operations on such tables.

At the time of this writing, the package is still a preview.

The main modules are:

* [Data.Table](src/Data/Table.hs) — database
    * Contains the `Table` dataype, which models a table as you would find it in a database.
