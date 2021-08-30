{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Table (
    -- * Synopsis
    -- | 'Table' models a database table.
    -- It corresponds to a collection of rows.
    -- Each row has a unique ID, but this is transparent to the API user.
    --
    -- 'Supply' is a supply of unique IDs.

    -- * Table
    Table
    , empty, fromList, toList
    , selectWhere, insertMany, deleteWhere, updateWhere
    , DeltaTable (..)
    , DeltaDB (..)
    , tableIntoDatabase

    -- * Supply
    , Supply
    , abundance, fresh
    ) where

import Prelude

import Data.Delta
    ( Delta (..)
    , Embedding (..)
    )
import Data.IntMap.Strict
    ( IntMap )

import qualified Data.IntMap.Strict as Map

{-------------------------------------------------------------------------------
    Table
-------------------------------------------------------------------------------}
-- | A 'Table' is a collection of rows.
data Table row = Table
    { rows :: IntMap row
    , uids :: Supply Int
    }

instance Functor Table where
    fmap f table@Table{rows} = table{ rows = Map.map f rows }

-- | The empty 'Table', containing no rows.
empty :: Table row
empty = Table{ rows = Map.empty, uids = abundance }

-- | List all rows satisfying the predicate.
selectWhere :: (row -> Bool) -> Table row -> [row]
selectWhere p = filter p . Map.elems . rows

-- | Insert rows into the table.
insertMany :: [row] -> Table row -> Table row
insertMany rs table = ($ table) . foldr (.) id $ map insertRow rs
  where
    insertRow row Table{rows,uids} =
        Table{ rows = Map.insert uid row rows, uids = uids2 }
      where (uid, uids2) = fresh uids

-- | Construct a 'Table' from a list of rows
fromList :: [row] -> Table row
fromList rows = insertMany rows empty

-- | List of rows contained in the 'Table'.
toList :: Table row -> [row]
toList = Map.elems . rows

-- | Delete all rows satisfying the predicate.
deleteWhere :: (row -> Bool) -> Table row -> Table row
deleteWhere p table@Table{rows} = table{ rows = Map.filter (not . p) rows }

-- | Update all rows satisfying the predicate
updateWhere :: (row -> Bool) -> (row -> row) -> Table row -> Table row
updateWhere p f table@Table{rows} = table{ rows = Map.map g rows }
  where g row = if p row then f row else row

-- | Delta encoding for changes to a 'Table'.
data DeltaTable row
    = InsertMany [row]
    | DeleteWhere (row -> Bool)
    | UpdateWhere (row -> Bool) (row -> row)

instance Delta (DeltaTable row) where
    type instance Base (DeltaTable row) = Table row
    apply (InsertMany rows) = insertMany rows
    apply (DeleteWhere p) = deleteWhere p
    apply (UpdateWhere p f) = updateWhere p f

-- | Delta encoding for changes to a database table with uniqe IDs.
data DeltaDB key row
    = InsertManyDB [row]
    | DeleteManyDB [key]
    | UpdateManyDB [(key, row)]

instance Functor (DeltaDB key) where
    fmap f (InsertManyDB rs) = InsertManyDB (fmap f rs)
    fmap _ (DeleteManyDB ks) = DeleteManyDB ks
    fmap f (UpdateManyDB krs) = UpdateManyDB [ (k, f r) | (k,r) <- krs ]

tableIntoDatabase :: Embedding
    (Table row) [DeltaTable row]
    (Table row) [DeltaDB Int row]
tableIntoDatabase = Embedding{ load, write, update = fmap . update1 }
  where
    load = Just . id
    write = id
    update1 _ (InsertMany rs)
        = InsertManyDB rs
    update1 Table{rows} (DeleteWhere p)
        = DeleteManyDB [ key | (key,row) <- Map.toList rows, p row ]
    update1 Table{rows} (UpdateWhere p f)
        = UpdateManyDB [ (key, f row) | (key,row) <- Map.toList rows, p row ]

{-------------------------------------------------------------------------------
    Supply
-------------------------------------------------------------------------------}
-- | A supply of unique IDs.
data Supply uid = Supply
    { now  :: !uid
    , next :: uid -> uid
    }

-- | Fresh supply of unique IDs.
abundance :: Enum uid => Supply uid
abundance = Supply
    { now = toEnum 0
    , next = succ
    }

-- | Retrieve a fresh unique ID.
fresh :: Supply uid -> (uid, Supply uid)
fresh supply@Supply{now=old,next} =
    let new = next old in new `seq` (new, supply{now=new})
