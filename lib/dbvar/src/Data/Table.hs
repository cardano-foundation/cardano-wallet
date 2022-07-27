{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Table (
    -- * Synopsis
    -- | 'Table' models a database table.
    -- It corresponds to a collection of rows.
    -- Each row has a unique ID, but this is transparent to the API user.
    --
    -- 'Pile' models a set of values.
    -- Unlike 'Set', it is represented as a lightweight list.
    -- This is used to highlight that the ordering of rows
    -- in a 'Table' is /not/ deterministic.
    --
    -- 'Supply' is a supply of unique IDs.

    -- * Table
    Table (..)
    , empty, fromRows, fromList, toPile, toRows
    , selectWhere, insertMany, deleteWhere, updateWhere
    , DeltaTable (..)
    , DeltaDB (..)
    , tableIntoDatabase

    -- * Pile
    , Pile (..)
    , fromSet
    , deltaListToPile, deltaListFromPile
    , deltaSetToPile, deltaSetFromPile

    -- * Supply
    , Supply
    , abundance, fresh, consume
    ) where

import Prelude

import Control.Monad
    ( forM )
import Control.Monad.Trans.State.Strict
    ( evalState, state )
import Data.Delta
    ( Delta (..)
    , DeltaList (..)
    , DeltaSet
    , DeltaSet1 (..)
    , Embedding
    , Embedding' (..)
    , mkEmbedding
    )
import Data.IntMap.Strict
    ( IntMap )
import Data.List
    ( sort, sortOn )
import Data.Ord
    ( Down (..) )
import Data.Set
    ( Set )

import qualified Data.Delta as Delta
import qualified Data.IntMap.Strict as Map
import qualified Data.Set as Set

{-------------------------------------------------------------------------------
    Table
-------------------------------------------------------------------------------}
-- | A 'Table' is a collection of rows.
data Table row = Table
    { rows :: IntMap row
    -- ^ Rows indexed by unique ID.
    , uids :: Supply
    -- ^ Unique ID supply.
    -- WARNING: This is an internal part of the structure.
    -- Changing it may lead to an inconsistent state.
    } deriving (Show)

instance Functor Table where
    fmap f table@Table{rows} = table{ rows = Map.map f rows }

-- | The empty 'Table', containing no rows.
empty :: Table row
empty = Table{ rows = Map.empty, uids = abundance }

-- | List all rows satisfying the predicate.
selectWhere :: (row -> Bool) -> Table row -> Pile row
selectWhere p = Pile . filter p . Map.elems . rows

-- | Insert rows into the table.
insertMany :: [row] -> Table row -> Table row
insertMany rs table = foldr insertRow table rs
  where
    insertRow row Table{rows,uids} =
        Table{ rows = Map.insert uid row rows, uids = uids2 }
      where (uid, uids2) = fresh uids

-- | Construct a 'Table' from a list of rows
fromList :: [row] -> Table row
fromList rows = insertMany rows empty

-- | Construct a 'Table' from a list of rows with unique IDs.
fromRows :: [(Int, row)] -> Table row
fromRows rows = Table
    { rows = Map.fromList rows
    , uids = consume keys abundance
    }
  where keys = map fst rows

-- | Pile of rows contained in the 'Table'.
toPile :: Table row -> Pile row
toPile = Pile . Map.elems . rows

-- | Pile of rows with unique IDs contained in the 'Table'.
toRows :: Table row -> Pile (Int,row)
toRows = Pile . Map.toList . rows

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
    type Base (DeltaTable row) = Table row
    apply (InsertMany rows) = insertMany rows
    apply (DeleteWhere p)   = deleteWhere p
    apply (UpdateWhere p f) = updateWhere p f

-- | Delta encoding for changes to a database table with unique IDs.
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
    type Base (DeltaDB key row) = Table row
    apply (InsertManyDB zs) table@Table{rows,uids} = table
        { rows = foldr ($) rows [ Map.insert k r | (k,r) <- zs ]
        , uids = consume (map fst zs) uids
        }
    apply (DeleteManyDB ks) table@Table{rows} =
        table{ rows = foldr ($) rows [ Map.delete k | k <- ks ] }
    apply (UpdateManyDB zs) table@Table{rows} =
        table{ rows = foldr ($) rows [ Map.adjust (const r) k | (k,r) <- zs ] }

tableIntoDatabase :: Embedding [DeltaTable row] [DeltaDB Int row]
tableIntoDatabase = mkEmbedding Embedding'
    { load, write, update = \_ b -> map (update1 b) }
  where
    load = Right . id
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
    Pile
-------------------------------------------------------------------------------}
-- | A 'Pile' is a set of values.
-- Unlike 'Set', it is represented as a list, and avoids the 'Ord' constraint.
--
-- This type is useful for highlighting that a collection of values
-- has no specific order, even though it is not represented as a 'Set'.
newtype Pile a = Pile { getPile :: [a] }
    deriving Show

instance Ord a => Eq (Pile a) where
    (Pile x) == (Pile y) = sort x == sort y

fromSet :: Set a -> Pile a
fromSet = Pile . Set.toList

-- | Randomly permute the objects in a 'Pile'. Useful for stress testing.
--
-- Every function @f :: Pile A -> B@ should satisfy
--
-- > forall g.  f . permute g = f
--
-- permute :: RandomGen g => g -> Pile a -> Pile a
-- permute = undefined
--      let (index, g2) = randomR (1,n) g1

-- | Map a 'DeltaSet' to a 'Pile' of single element insertions and deltions.
deltaSetToPile :: DeltaSet a -> Pile (DeltaSet1 a)
deltaSetToPile = Pile . Delta.deltaSetToList

-- | Restore a 'DeltaSet' from a 'Pile' of single element
-- insertions and deletions.
--
-- > deltaSetFromPile . deltaSetToPile = id
deltaSetFromPile :: Ord a => Pile (DeltaSet1 a) -> DeltaSet a
deltaSetFromPile = Delta.deltaSetFromList . getPile

-- | Map a 'DeltaList' to a 'Pile' of indexed single element concatenations.
-- Higher indices are prepended later.
deltaListToPile :: DeltaList a -> Pile (Int, a)
deltaListToPile (Append xs) = Pile $ zip [0..] (reverse xs)

-- | Restore a 'DeltaList' from a 'Pile'.
--
-- > deltaListFromPile . deltaListToPile = id
deltaListFromPile :: Pile (Int, a) -> DeltaList a
deltaListFromPile = Append . map snd . sortOn (Down . fst) . getPile

{-------------------------------------------------------------------------------
    Supply
-------------------------------------------------------------------------------}
-- | A supply of unique IDs.
newtype Supply = Supply
    { now  :: Int -- ^ Largest unique ID that is *in use*.
    }

instance Show Supply where
    showsPrec d (Supply{now}) = showParen (d > app_prec) $
        showString "Supply {now = " . shows now . showString "} "
      where app_prec = 10

-- | Fresh supply of unique IDs.
abundance :: Supply
abundance = Supply{ now = 0 }

-- | Retrieve a fresh unique ID.
fresh :: Supply -> (Int, Supply)
fresh supply@Supply{now=old} = new `seq` (new, supply{now=new})
  where new = succ old -- smallest unused unique ID

-- | Remove a list of unique IDs from the 'Supply' if necessary.
consume :: [Int] -> Supply -> Supply
consume xs supply@Supply{now=old} = new `seq` supply{now=new}
  where new = old `max` maximum xs
