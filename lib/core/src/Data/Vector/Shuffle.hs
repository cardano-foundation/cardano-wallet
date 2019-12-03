{-# LANGUAGE RankNTypes #-}

module Data.Vector.Shuffle
    ( shuffle
    ) where

import Prelude

import Control.Monad
    ( forM_ )
import Control.Monad.Trans.Class
    ( lift )
import Control.Monad.Trans.State.Strict
    ( evalStateT, state )
import Data.Vector.Mutable
    ( IOVector )
import System.Random
    ( RandomGen, newStdGen, randomR )

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

-- | Shuffles a list of elements.
--
-- >>> shuffle (outputs coinSel)
-- [...]
shuffle :: [a] -> IO [a]
shuffle xs = newStdGen >>= flip shuffleWith xs

-- | Like 'shuffle', but from a given seed. 'shuffle' will use a randomly
-- generate seed using 'newStdGen' from @System.Random@.
shuffleWith :: RandomGen g => g -> [a] -> IO [a]
shuffleWith seed = modifyInPlace $ \v -> flip evalStateT seed $ do
    let (lo, hi) = (0, MV.length v - 1)
    forM_ [lo .. hi] $ \i -> do
      j <- fromInteger <$> state (randomR (fromIntegral lo, fromIntegral hi))
      lift $ swapElems v i j
  where
    swapElems :: IOVector a -> Int -> Int -> IO ()
    swapElems v i j = do
        x <- MV.read v i
        y <- MV.read v j
        MV.write v i y
        MV.write v j x

    modifyInPlace :: forall a. (IOVector a -> IO ()) -> [a] -> IO [a]
    modifyInPlace f xs = do
        v' <- V.thaw $ V.fromList xs
        f v'
        V.toList <$> V.freeze v'
