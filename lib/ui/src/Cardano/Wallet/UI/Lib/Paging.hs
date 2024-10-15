module Cardano.Wallet.UI.Lib.Paging
where

import Prelude

import Data.Map.Strict
    ( Map
    )
import qualified Data.Map.Strict as Map

-- | Compute the next key in the given map after given number of elements.
next :: Ord k => Int -> Map k a -> k -> Maybe k
next n m start =
    fmap fst
        $ Map.lookupMin
        $ Map.drop n
        $ Map.dropWhileAntitone (< start) m

dropEnd :: Int -> Map k a -> Map k a
dropEnd n xs = Map.take (Map.size xs - n) xs

takeEnd :: Int -> Map k a -> Map k a
takeEnd n xs = Map.drop (Map.size xs - n) xs

-- | Compute the previous key in the given map after given number of elements.
previous :: Ord k => Int -> Map k a -> k -> Maybe k
previous n m start =
    fmap fst
        $ Map.lookupMax
        $ dropEnd n
        $ Map.takeWhileAntitone (<= start) m

-- | Extract a page of elements from the given map starting from the given key.
nextPage :: Ord k => Int -> k -> Map k a -> Map k a
nextPage n start = Map.take n . Map.dropWhileAntitone (< start)

-- | Extract a page of elements from the given map starting from the given key
-- and going backwards.
previousPage :: Ord k => Int -> k -> Map k a -> Map k a
previousPage n start = takeEnd n . Map.takeWhileAntitone (<= start)
