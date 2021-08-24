{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Checkpoints (
    -- * Synopsis
    -- | 'Checkpoints'@ a@ provides a collection of checkpoints for values of type @a@
    -- with an efficient delta encoding 'DeltaCheckpoints'.
    --
    -- 'MultiGraph' is a graph type used to implement 'Checkpoints'.

    -- * Checkpoints
    demoCheckpoints,
    Checkpoints, focus, singleton
    , DeltaCheckpoints (..)
    , checkpointsInMultiGraph
    , newStoreCheckpoints
    
    -- * MultiGraph
    , Edge(..), MultiGraph, emptyMultiGraph, getEdges
    , fromLinearChain
    , DeltaMultiGraph (..)
    , graphIntoTable
    ) where

import Prelude

import Control.Monad
    ( forM_ )
import Control.Monad.Class.MonadSTM
    ( MonadSTM )
import Data.DBVar
    ( -- DBVar
      Store (..)
    , embedStore
    , newStore
    , newTable
    , pairStores
    , initDBVar
    , readDBVar
    , updateDBVar
    )
import Data.Delta
    ( Delta (..)
    , DeltaList (..)
    , DeltaSet (..)
    , Embedding (..)
    , NoChange (..)
    )
import Data.Map.Strict
    ( Map )
import Data.Set
    ( Set )
import Safe
    ( lastMay )

import qualified Data.Set as Set
import qualified Data.Map as Map

-- | /Small demo for developers./
demoCheckpoints :: IO ()
demoCheckpoints = do
    store <- newStoreCheckpoints
    let store_
            :: (delta ~ DeltaList Char)
            => Store IO
                (DeltaCheckpoints Int delta)
                (Checkpoints Int delta String)
        store_ = store
    db    <- initDBVar store $ singleton ""
    forM_ (reverse "Hello" :: String) $ \c ->
        updateDBVar db $ AppendToFocus [Cons c]
    print =<< readDBVar db
    print =<< loadS store_

{-------------------------------------------------------------------------------
    Checkpoints
-------------------------------------------------------------------------------}
-- | A collection of checkpoints with an efficient delta encoding.
data Checkpoints id delta a = Checkpoints
    { focus   :: (id, a)
    -- ^ The latest checkpoint id and value.
    , root    :: (id, a)
    , deltas  :: MultiGraph id delta
    } deriving (Eq, Ord, Show)

-- | A single checkpoint.
singleton :: Enum id => a -> Checkpoints id delta a
singleton a = Checkpoints
    { focus  = (toEnum 0, a)
    , root   = (toEnum 0, a)
    , deltas = emptyMultiGraph
    }

data DeltaCheckpoints id delta = AppendToFocus [delta]

nextFocus :: Enum id => Checkpoints id delta a -> id
nextFocus Checkpoints{focus} = succ $ fst focus

instance (Enum id, Ord id, Delta delta) => Delta (DeltaCheckpoints id delta) where
    type instance Base (DeltaCheckpoints id delta) = Checkpoints id delta (Base delta)
    apply (AppendToFocus ds) cp@Checkpoints{focus=(old,olda),root,deltas} = Checkpoints
        { focus  = (new, apply ds olda)
        , root   = root
        , deltas = apply (AddEdges Edge{from=old,to=new,via=ds}) deltas
        }
      where new = nextFocus cp

-- | Embed a 'Checkpoints' structure into a 'MultiGraph'.
checkpointsInMultiGraph
    :: (Enum id, Ord id, Delta delta, v ~ Base delta, root ~ (id, v))
    => Embedding
        (Checkpoints id delta v) (DeltaCheckpoints id delta)
        (root, MultiGraph id delta) (NoChange root, DeltaMultiGraph id delta)
checkpointsInMultiGraph = Embedding{load,write,update}
  where
    write Checkpoints{root,deltas} = (root, deltas)
    update cp@Checkpoints{focus=(old,_)} (AppendToFocus deltas) = 
        (NoChange, AddEdges Edge{from=old,to=new,via=deltas})
      where new = nextFocus cp

    load ((rootid,rootv),deltas) = go <$> fromLinearChain rootid deltas
      where
        go chain = Checkpoints{focus=(nowid,nowv),root=(rootid,rootv),deltas=deltas}
          where
            nowid = maybe rootid fst (lastMay chain)
            nowv  = apply (concatMap snd chain) rootv

-- | Create a 'Store' for 'Checkpoints'
newStoreCheckpoints
    :: (Enum id, Ord id, Ord delta, MonadSTM m, Delta delta, v ~ Base delta)
    => m (Store m (DeltaCheckpoints id delta) (Checkpoints id delta v))
newStoreCheckpoints = do
    store <- pairStores <$>
        newStore <*> (embedStore graphIntoTable <$> newTable)
    pure $ embedStore checkpointsInMultiGraph store

{-------------------------------------------------------------------------------
    Multigraphs
-------------------------------------------------------------------------------}
-- | An 'Edge' in a graph.
data Edge node edge = Edge
    { from :: node
    , to   :: node
    , via  :: edge
    } deriving (Eq, Ord, Show)

-- | A 'MultiGraph' is a collection of nodes
-- and a collection of edges between those nodes.
--
-- The precise rules of this graph variant are:
--
-- * Nodes are identified by the type @node@.
-- * Edges are labeled, but not identified by the type @edge@.
-- * Edges are have a direction.
-- * Loops are allowed. A loop is an edge that connect a node to itself.
data MultiGraph node edge = MultiGraph
    { edges :: Map node (Map node [edge])
    } deriving (Eq, Ord, Show)

-- | The graph without nodes and edges
emptyMultiGraph :: MultiGraph node edge
emptyMultiGraph = MultiGraph Map.empty

-- | Get all edges with labels in the graph
--
-- FIXME: The output should be an 'Set' without order?
getEdges :: MultiGraph node edge -> [Edge node edge]
getEdges MultiGraph{edges} =
    [ Edge{from,to,via}
    | (from,out) <- Map.toList edges
    , (to ,vias) <- Map.toList out
    , via        <- vias
    ]

-- | Check whether a given 'MultiGraph' contains an isolated
-- chain that starts from a given node.
fromLinearChain :: Ord node => node -> MultiGraph node edge -> Maybe [(node, [edge])]
fromLinearChain from g =
    case Map.toList <$> Map.lookup from (edges g) of
        -- We are at the end of the chain
        Nothing          -> Just []
        Just []          -> Just []
        -- The chain continues with a single node.
        Just [(to,vias)] -> ((to,vias):) <$> fromLinearChain to g
        -- The graph diverges, the node is not part of a linear chain
        _                -> Nothing

-- | Changes to a 'MultiGraph'.
data DeltaMultiGraph node edge
    = AddEdges (Edge node [edge])
    -- ^ Add several edges between two given nodes.
    | CollapseNode node
    -- ^ Collapse a node and connect its neighbors.
    -- FIXME: Not implemented yet!
    deriving (Eq, Ord, Show)

instance Ord node => Delta (DeltaMultiGraph node edge) where
    type instance Base (DeltaMultiGraph node edge) = MultiGraph node edge
    apply (AddEdges Edge{from,to,via}) =
        MultiGraph . Map.insertWith Map.union from (Map.singleton to via) . edges
    apply (CollapseNode node) =
        error "FIXME: apply (CollapseNode node) is  undefined"

-- | Embed a 'MultiGraph' into a store for 'Edge'.
graphIntoTable
    :: (Ord edge, Ord node, e ~ Edge node edge)
    => Embedding
        (MultiGraph node edge) (DeltaMultiGraph node edge)
        (Set e) [DeltaSet e]
graphIntoTable = Embedding {load,write,update}
  where
    load = Just . ($ emptyMultiGraph) . foldr (.) id . map addEdge . Set.toList
      where addEdge Edge{from,to,via} = apply $ AddEdges Edge{from,to,via=[via]}
    write = Set.fromList . getEdges
    update _ (AddEdges e) = map Insert $ flattenEdges e
    update _ (CollapseNode node) =
        error "FIXME: update (CollapseNode node) is  undefined"

flattenEdges :: Edge node [edge] -> [Edge node edge]
flattenEdges Edge{to,from,via} = Edge to from <$> via