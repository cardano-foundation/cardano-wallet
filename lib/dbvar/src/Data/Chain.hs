{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Chain (
    -- * Synopsis
    -- | 'Chain'@ node edge@ is a linear chain of nodes with directed
    -- edges.

    -- * Chain
      Chain
    , member, ChainContext, lookup
    --, singleton
    , fromEdge, fromEdges
    , edges, toEdges, summary

    -- * DeltaChain
    , DeltaChain (..)
    , appendTip, collapseNode, rollbackTo
    , chainIntoTable

    -- * Edge
    , Edge (..), flattenEdge
    ) where

import Prelude hiding (lookup)

import Control.Monad
    ( (<=<)
    , guard
    , join
    )
import Data.Delta
    ( Delta (..)
    , Embedding (..)
    )
import Data.List
    ( unfoldr )
import Data.Map.Strict
    ( Map )
import Data.Maybe
    ( fromMaybe )
import Data.Semigroupoid
    ( o )
import Data.Table
    ( Table , DeltaTable (..) )

import qualified Data.Table as Table
import qualified Data.Map as Map

{-------------------------------------------------------------------------------
    Chain
-------------------------------------------------------------------------------}
-- | A linear chain of nodes.
-- Edges between nodes are labeled by a 'Monoid' @edge@.
--
-- @
--   n_tip  <--e_tip-- … <--e1-- n1 <--e0-- n0
-- @
data Chain node edge = Chain
    { next :: Map node (edge, node)
    , prev :: Map node (Maybe node)
    , tip  :: node
    } deriving (Eq, Show)

instance Functor (Chain node) where
    fmap f chain = chain{ next = fmap (\(e,n) -> (f e, n)) (next chain) }

-- | Test whether a node is contained in the chain.
member :: Ord node => node -> Chain node edge -> Bool
member node Chain{prev} = node `Map.member` prev

-- | Context (incoming and outgoing edges) for a @node@ in a 'Chain'.
type ChainContext node edge = Edge (Maybe (edge,node)) node

-- | Look up the 'Context' of a node in a 'Chain'.
lookup :: Ord node => node -> Chain node edge -> Maybe (ChainContext node edge)
lookup node Chain{next,prev} =
    case (Map.lookup node next, Map.lookup node prev) of
        (_, Nothing) ->
            Nothing
        (after, Just Nothing) ->
            Just Edge{ via=node, to=after, from=Nothing }
        (after, Just (Just before)) -> let adjust (e,_) = (e,before) in
            Just Edge{ via=node, to=after, from=adjust <$> Map.lookup before next }

{-
-- | Chain with a single node and no edges.
--
-- FIXME: This cannot be represented in a database that only stores edges.
singleton :: Ord node => node -> Chain node edge
singleton node = Chain
    { next = Map.empty
    , prev = Map.fromList [(node, Nothing)]
    , tip  = node
    }
-}

-- | Construct a chain from a single 'Edge'.
fromEdge :: Ord node => Edge node edge -> Chain node edge
fromEdge Edge{from,to,via} = Chain
    { next = Map.fromList [(from, (via,to))]
    , prev = Map.fromList [(to, Just from), (from, Nothing)]
    , tip  = to
    }

-- | Construct a chain from a collection of edges.
-- Fails if the edges do not fit together.
--
-- FIXME: Order of @edge@ labels? This is important.
-- We probably need to model the edges in the table as a set.
--
-- FIXME: Edges in the table correspond to NonEmpty list.
-- Need to deal with that properly.
fromEdges :: Ord node => [Edge node edge] -> Maybe (Chain node [edge])
fromEdges []     = Nothing
fromEdges (e:es) = ($ fromEdge' e) . foldr (<=<) Just $ map addEdge es
  where fromEdge' = fmap (:[]) . fromEdge

-- | List all edges in the 'Chain'.
--
-- The edge that points to the tip is listed /first/,
-- and the edge that starts at the beginning is listed /last/.
edges :: Ord node => Chain node edge -> [edge]
edges Chain{prev,next,tip} = unfoldr backwards tip
  where
    backwards now = do
        before <- join $ Map.lookup now prev
        (e,_)  <- Map.lookup before next
        pure (e,before)

-- | Convert a 'Chain' into a list of 'Edge'.
--
-- FIXME: Order?
toEdges :: Chain node [edge] -> [Edge node edge]
toEdges Chain{next} =
    [Edge{from,to,via} | (from, (vias,to)) <- Map.toList next, via <- vias]

-- | Combine all the edges in the 'Chain'.
-- The summary is invariant under 'collapseNode'.
--
-- > summary = mconcat . edges
summary :: (Ord node, Monoid edge) => Chain node edge -> edge
summary = mconcat . edges

{-------------------------------------------------------------------------------
    DeltaChain
-------------------------------------------------------------------------------}
-- | Changes to a 'Chain'.
data DeltaChain node edge
    = AppendTip node edge
    -- ^ See 'appendTip'.
    | CollapseNode node
    -- ^ See 'collapseNode'.
    | RollbackTo node
    -- ^ See 'rollbackTo'.

instance (Ord node, Monoid edge) => Delta (DeltaChain node edge) where
    type instance Base (DeltaChain node edge) = Chain node edge
    apply (AppendTip n e) = appendTip n e
    apply (CollapseNode n) = collapseNode n
    apply (RollbackTo n ) = rollbackTo n

-- | Append a new tip to the chain.
appendTip :: Ord node => node -> edge -> Chain node edge -> Chain node edge
appendTip new edge Chain{next,prev,tip=old} = Chain
    { next = Map.insert old (edge, new) next
    , prev = Map.insert new (Just old) prev
    , tip  = new
    }

-- | Remove the given @node@ and combine the incoming and outgoing edges.
-- Do nothing if the node is at the tip, or at the bottom,
-- or not in the chain at all.
collapseNode
    :: (Ord node, Monoid edge)
    => node -> Chain node edge -> Chain node edge
collapseNode node chain@Chain{next,prev} =
    case (join (Map.lookup node prev), Map.lookup node next) of
        (Just before, Just (e1, after)) ->
            let (e2,_) = fromMaybe err (Map.lookup before next)
            in chain
                { next
                    = Map.insert before (e1 <> e2, after)
                    $ Map.delete node next
                , prev
                    = Map.insert after (Just before)
                    $ Map.delete node prev 
                }
        _ -> chain
  where
    err = error "collapseNode: impossible case"

-- | Remove the tip and more nodes from the chain until
-- the given node is the tip.
-- 
-- Do nothing if the node is not in the chain.
rollbackTo :: Ord node => node -> Chain node edge -> Chain node edge
rollbackTo new chain@Chain{next,prev,tip}
    | new `member` chain = Chain
        { next = deleteAll (new:deletions) next
        , prev = deleteAll deletions prev
        , tip  = new
        }
    | otherwise = chain
  where
    deleteAll = foldr (.) id . map Map.delete
    deletions = unfoldr backwards tip
    backwards now = do
        guard $ new /= now
        x <- join $ Map.lookup now prev
        return (now,x)

-- | Helper: Add a single edge to a 'Chain' if possible.
-- The chain may contain gaps while adding edges.
addEdge :: Ord node => Edge node edge -> Chain node [edge] -> Maybe (Chain node [edge])
addEdge Edge{from,to,via} chain@Chain{next,prev,tip} =
    case Map.lookup from next of
        -- A connection from->to' already exists,
        -- add the edge if this has the same destination.
        Just (es,to') -> do
            guard $ to == to'
            pure $ chain { next = Map.insert from (via:es,to) next }
        -- No connection exists, create one.
        Nothing -> pure $ chain
            { next = Map.insert from ([via], to) next
            , prev
                = Map.insert to (Just from)
                . Map.insertWith (\_ old -> old) from Nothing
                $ prev
            , tip = if from == tip then to else tip
            }

-- | Embed a 'Chain' into a table of 'Edge'.
chainIntoTable
    :: (Ord edge, Ord node, e ~ Edge node edge)
    => Embedding (DeltaChain node [edge]) [DeltaTable e]
chainIntoTable = Embedding {load,write,update}
  where
    load  = fromEdges . Table.toList
    write = Table.fromList . toEdges
    update Chain{tip=from} _ (AppendTip to vias) =
        [InsertMany [Edge{from,to,via} | via <- vias]]
    update Chain{tip,prev} _ (RollbackTo node) =
        [DeleteWhere $ \Edge{to} -> to `elem` deletions]
      where
        deletions = unfoldr backwards tip
        backwards now = do
            guard $ node /= now
            x <- join $ Map.lookup now prev
            return (now,x)
    update chain _ (CollapseNode now) = case lookup now chain of
        Nothing -> []
        Just Edge{to,from} ->
            maybe [] (\(_,new) -> updateTo now new) to
            <> maybe [] (\(_,new) -> updateFrom now new) from
    updateTo old new = [UpdateWhere (\Edge{to} -> to == old) (\e -> e{to=new})]
    updateFrom old new = [UpdateWhere (\Edge{from} -> from == old) (\e -> e{from=new})]
        -- Wait. If we are at the beginning of the chain,
        -- I have to delete the entries, not just update them!

{-------------------------------------------------------------------------------
    Tests
-------------------------------------------------------------------------------}
test :: (Table (Edge Int Char), [[Table.DeltaDB Int (Edge Int Char)]])
test = liftUpdates (Table.tableIntoDatabase `o` chainIntoTable)
    [CollapseNode 1, AppendTip 3 "DC", AppendTip 2 "B"]
    (fromEdge Edge{from=0,to=1,via="A"})

liftUpdates
    :: (Delta da, Delta da)
    => Embedding da db
    -> [da] -> Base da -> (Base db, [db])
liftUpdates Embedding{load,write,update} ds = go ds . write
  where
    go []       bin = (bin, [])
    go (da:das) bin = case load bout of
        Nothing -> (bout, dbs)
        Just a  -> let db = update a bout da in (apply db bout, db : dbs)
      where
        (bout, dbs) = go das bin

{-------------------------------------------------------------------------------
    Edge
-------------------------------------------------------------------------------}
-- | Utility type that represents an 'Edge' in a graph:
-- it connects two @node@ via an @edge@ label.
data Edge node edge = Edge
    { from :: node
    , to   :: node
    , via  :: edge
    } deriving (Eq, Ord, Show)

instance Functor (Edge node) where
    fmap f e@Edge{via} = e{ via = f via }

-- | Flatten a list of edges 
flattenEdge :: Edge node [edge] -> [Edge node edge]
flattenEdge Edge{to,from,via} = Edge to from <$> via
