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
    , abundance, fresh, consume
    ) where

import Prelude

import Control.Monad
    ( forM )
import Control.Monad.Trans.State.Strict
    ( state
    , evalState
    )
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
    , uids :: Supply
    } deriving (Show)

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

instance Show row => Show (DeltaTable row) where
    showsPrec d delta = showParen (d > app_prec) $ case delta of
        InsertMany rs -> showString "InsertMany " . showsPrec (app_prec+1) rs
        DeleteWhere _ -> showString "DeleteWhere (..)"
        UpdateWhere _ _ -> showString "UpdateWhere (..)"
      where app_prec = 10

instance Delta (DeltaTable row) where
    type instance Base (DeltaTable row) = Table row
    apply (InsertMany rows) = insertMany rows
    apply (DeleteWhere p)   = deleteWhere p
    apply (UpdateWhere p f) = updateWhere p f

-- | Delta encoding for changes to a database table with uniqe IDs.
data DeltaDB key row
    = InsertManyDB [(key, row)]
    | DeleteManyDB [key]
    | UpdateManyDB [(key, row)]
    deriving (Eq, Show)

instance Functor (DeltaDB key) where
    fmap f (InsertManyDB zs) = InsertManyDB [ (k, f r) | (k,r) <- zs ]
    fmap _ (DeleteManyDB ks) = DeleteManyDB ks
    fmap f (UpdateManyDB zs) = UpdateManyDB [ (k, f r) | (k,r) <- zs ]

instance (key ~ Int) => Delta (DeltaDB key row) where
    type instance Base (DeltaDB key row) = Table row
    apply (InsertManyDB zs) table@Table{rows,uids} = table
        { rows = foldr (.) id [ Map.insert k r | (k,r) <- zs ] rows
        , uids = consume (map fst zs) uids
        }
    apply (DeleteManyDB ks) table@Table{rows} =
        table{ rows = foldr (.) id [ Map.delete k | k <- ks ] rows }
    apply (UpdateManyDB zs) table@Table{rows} =
        table{ rows = foldr (.) id [ Map.adjust (const r) k | (k,r) <- zs ] rows }
-- FIXME: Enlarge UID supply as necessary for InsertManyDB!

tableIntoDatabase :: Embedding
    (Table row) [DeltaTable row]
    (Table row) [DeltaDB Int row]
tableIntoDatabase = Embedding{ load, write, update = fmap . update1 }
  where
    load = Just . id
    write = id
    update1 Table{uids} (InsertMany rs) = InsertManyDB (zip keys rs)
      where
        keys = flip evalState uids $ forM (reverse rs) $ \_ -> state fresh
    update1 Table{rows} (DeleteWhere p)
        = DeleteManyDB [ key | (key,row) <- Map.toList rows, p row ]
    update1 Table{rows} (UpdateWhere p f)
        = UpdateManyDB [ (key, f row) | (key,row) <- Map.toList rows, p row ]
-- FIXME! Be careful about the order of updates here.

{-------------------------------------------------------------------------------
    Supply
-------------------------------------------------------------------------------}
-- | A supply of unique IDs.
data Supply = Supply { now  :: Int }

instance Show Supply where
    showsPrec d (Supply{now}) = showParen (d > app_prec) $
        showString "Supply {now = " . showsPrec 0 now . showString "} "
      where app_prec = 10

-- | Fresh supply of unique IDs.
abundance :: Supply
abundance = Supply{ now = 0 }

-- | Retrieve a fresh unique ID.
fresh :: Supply -> (Int, Supply)
fresh supply@Supply{now=old} = new `seq` (new, supply{now=new})
  where new = succ old

-- | Remove a list of unique IDs from the 'Supply' if necessary.
consume :: [Int] -> Supply -> Supply
consume xs supply@Supply{now=old} = new `seq` supply{now=new}
  where new = old `max` (succ $ maximum xs)
