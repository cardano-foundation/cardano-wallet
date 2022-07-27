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
    , edges, nodes, toEdges, summary

    -- * DeltaChain
    , DeltaChain (..)
    , appendTip, collapseNode, rollbackTo
    , chainIntoTable
    , ErrMalformedChainTable (..)

    -- * Edge
    , Edge (..), flattenEdge

    -- * Testing
    , testChain
    ) where

import Prelude hiding
    ( lookup )

import Control.Exception
    ( Exception, toException )
import Control.Monad
    ( guard, join, (<=<) )
import Data.Bifunctor
    ( first )
import Data.Delta
    ( Delta (..), Embedding, Embedding' (..), liftUpdates, mkEmbedding )
import Data.List
    ( unfoldr )
import Data.Map.Strict
    ( Map )
import Data.Semigroupoid
    ( o )
import Data.Table
    ( DeltaTable (..), Pile (..), Table )

import qualified Data.Map.Strict as Map
import qualified Data.Table as Table

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
    fmap f chain = chain{ next = fmap (first f) (next chain) }

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

{- HLINT ignore fromEdges "Fuse foldr/map" -}
-- | Construct a chain from a collection of edges.
-- Fails if the edges do not fit together.
--
-- The ordering of edge labels of a single edge in the chain will
-- be the same as the ordering of the edge labels as they
-- appear in the list.
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

-- | List all nodes in the 'Chain'.
-- The tip is listed /first/.
nodes :: Ord node => Chain node edge -> [node]
nodes Chain{prev,next,tip} = tip : unfoldr backwards tip
  where
    backwards now = do
        before <- join $ Map.lookup now prev
        (_,_)  <- Map.lookup before next
        pure (before,before)

-- | Convert a 'Chain' into a list of 'Edge'.
toEdges :: Chain node edge -> [Edge node edge]
toEdges Chain{next} =
    [ Edge{from,to,via} | (from, (via,to)) <- Map.toList next ]

-- | Combine all the edges in the 'Chain'.
-- The summary is invariant under 'collapseNode'.
--
-- > summary = mconcat . edges
summary :: (Ord node, Monoid edge) => Chain node edge -> edge
summary = mconcat . edges
-- FIXME: If a Chain without edges does not exist, then
-- we can go to Semigroup here.

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

instance (Ord node, Semigroup edge) => Delta (DeltaChain node edge) where
    type Base (DeltaChain node edge) = Chain node edge
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
    :: (Ord node, Semigroup edge)
    => node -> Chain node edge -> Chain node edge
collapseNode now chain@Chain{next,prev} =
    case lookup now chain of
        -- Chain:   nto <--eto-- now <--efrom-- nfrom
        Just Edge{to = Just (eto,nto), from = Just (efrom,nfrom)} -> chain
            { next
                = Map.insert nfrom (eto <> efrom, nto)
                $ Map.delete now next
            , prev
                = Map.insert nto (Just nfrom)
                $ Map.delete now prev
            }
        _ -> chain

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
    deleteAll dels m = foldr Map.delete m dels
    deletions = unfoldr backwards tip
    backwards now = do
        guard $ new /= now
        x <- join $ Map.lookup now prev
        return (now,x)

-- | Helper: Add a single edge to a 'Chain' if possible.
-- The chain may contain gaps while adding edges.
addEdge
    :: Ord node
    => Edge node edge -> Chain node [edge] -> Maybe (Chain node [edge])
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
                . Map.insertWith (\_new old -> old) from Nothing
                $ prev
            , tip = if from == tip then to else tip
            }

-- | Embed a 'Chain' into a table of 'Edge'.
--
-- The first and second argument specify how the edge labels
-- are to be mapped to and from sets of table rows.
-- Importantly, we may not assume that the table stores
-- the rows in any particular order.
chainIntoTable
    :: (Ord node, Semigroup edge)
    => (edge -> Pile e) -> (Pile e -> edge)
    -> Embedding (DeltaChain node edge) [DeltaTable (Edge node e)]
chainIntoTable toPile fromPile = mkEmbedding Embedding'{load,write,update}
  where
    load = toEither . fmap (fmap $ fromPile . Pile)
        . fromEdges . getPile . Table.toPile
      where
        toEither = maybe (Left $ toException ErrMalformedChainTable) Right

    write = Table.fromList
        . concatMap (flattenEdge . fmap (getPile . toPile)) . toEdges

    update Chain{tip=from} _ (AppendTip to vias) =
        [InsertMany [Edge{from,to,via} | via <- getPile $ toPile vias]]
    update Chain{tip,prev} _ (RollbackTo node) =
        [DeleteWhere $ \Edge{to} -> to `elem` deletions]
      where
        deletions = unfoldr backwards tip
        backwards now = do
            guard $ node /= now
            x <- join $ Map.lookup now prev
            return (now,x)
    update chain _ (CollapseNode now) = case lookup now chain of
        Just Edge{to=Just (eto,nto), from=Just (efrom,nfrom)} ->
            -- insert new edges
            [ InsertMany
                [ Edge{to=nto,from=nfrom,via}
                | via <- getPile $ toPile (eto <> efrom)
                ]
            -- delete old edges
            , DeleteWhere (\Edge{to,from} -> to == now || from == now)
            ]
        _ -> []

data ErrMalformedChainTable = ErrMalformedChainTable deriving (Eq, Show)
instance Exception ErrMalformedChainTable

{-------------------------------------------------------------------------------
    Tests
-------------------------------------------------------------------------------}
testChain :: (Table (Edge Int Char), [[Table.DeltaDB Int (Edge Int Char)]])
testChain = liftUpdates (Table.tableIntoDatabase `o` chainIntoTable Pile getPile)
    [CollapseNode 1, CollapseNode 2, AppendTip 3 "c", AppendTip 2 "b"]
    $ fromEdge Edge{from=0,to=1,via="a"}

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
flattenEdge Edge{to,from,via} = [ Edge{to,from,via=v} | v <- via ]
