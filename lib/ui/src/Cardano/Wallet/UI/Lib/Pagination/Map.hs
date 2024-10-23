module Cardano.Wallet.UI.Lib.Pagination.Map
    ( mkStrictMapPaginate
    , Paginate (..)
    )
where

import Prelude

import Cardano.Wallet.UI.Lib.Pagination.Type
    ( MkPaginatePure
    , Paginate (..)
    )
import Data.Map.Strict
    ( Map
    )

import qualified Data.Map.Strict as Map

-- | Compute the next key in the given map after given number of elements.
next :: Ord k => Int -> Map k a -> k -> Maybe k
next n m start' =
    fmap fst
        $ Map.lookupMin
        $ Map.drop n
        $ Map.dropWhileAntitone (< start') m

dropEnd :: Int -> Map k a -> Map k a
dropEnd n xs = Map.take (Map.size xs - n) xs

-- | Compute the previous key in the given map after given number of elements.
previous :: Ord k => Int -> Map k a -> k -> Maybe k
previous n m start' =
    fmap fst
        $ Map.lookupMax
        $ dropEnd n
        $ Map.takeWhileAntitone (<= start') m

-- | Extract a page of elements from the given map starting from the given key.
nextPage :: Ord k => Int -> k -> Map k a -> Maybe (Int, Map k a)
nextPage n start' s =
    let
        r = Map.take n . Map.dropWhileAntitone (< start') $ s
    in
        case length r of
            0 -> Nothing
            l -> Just (l, r)

-- | Compute the minimum key in the given map.
minKey :: Map k a -> Maybe k
minKey = fmap fst . Map.lookupMin

-- | Make a 'Paginate' for a strict map.
mkStrictMapPaginate :: Ord k => MkPaginatePure k (Map k a)
mkStrictMapPaginate pageSize a =
    Paginate
        { nextIndex = next pageSize a
        , previousIndex = previous pageSize a
        , pageAtIndex = \k -> nextPage pageSize k a
        , minIndex = minKey a
        }
