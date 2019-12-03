{-# LANGUAGE RankNTypes #-}

module Data.Vector.Shuffle
    ( shuffle
    ) where

import Prelude

import Control.Monad
    ( forM_ )
import Data.Vector.Mutable
    ( IOVector )
import System.Random
    ( randomRIO )

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

-- | Shuffles a list of elements.
--
-- >>> shuffle (outputs coinSel)
-- [...]
shuffle :: [a] -> IO [a]
shuffle = modifyInPlace $ \v -> do
    let (lo, hi) = (0, MV.length v - 1)
    forM_ [lo .. hi] $ \i -> do
      j <- fromInteger <$> randomRIO (fromIntegral lo, fromIntegral hi)
      swapElems v i j
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
