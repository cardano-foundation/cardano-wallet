{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Data.Vector.Shuffle
    ( -- * Simple
      shuffle

      -- * Advanced
    , mkSeed
    , shuffleWith
    ) where

import Prelude

import Control.Monad
    ( forM_ )
import Control.Monad.Trans.Class
    ( lift )
import Control.Monad.Trans.State.Strict
    ( evalStateT, state )
import Crypto.Hash
    ( hash )
import Crypto.Hash.Algorithms
    ( MD5 )
import Data.Text
    ( Text )
import Data.Vector.Mutable
    ( IOVector )
import Data.Word
    ( Word8 )
import System.Random
    ( RandomGen, StdGen, mkStdGen, newStdGen, randomR )

import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV


-- | Generate a random generator seed from a text string
mkSeed :: Text -> StdGen
mkSeed = mkStdGen . toInt . quickHash . T.encodeUtf16LE
  where
    quickHash = BA.convert . hash @_ @MD5
    toInt = snd . BS.foldl' exponentiation (0,0)
      where
        exponentiation :: (Int, Int) -> Word8 -> (Int, Int)
        exponentiation (e, n) i = (e+1, n + fromIntegral i*2^e)

-- | Shuffles a list of elements.
--
-- >>> shuffle (outputs coinSel)
-- [...]
shuffle :: [a] -> IO [a]
shuffle xs = newStdGen >>= flip shuffleWith xs

-- | Like 'shuffle', but from a given seed. 'shuffle' will use a randomly
-- generate seed using 'newStdGen' from @System.Random@.
--
-- __Properties:__
--
-- - @shuffleWith g es == shuffleWith g es@
-- - @∃Δ> 1. g ≠g', length es > Δ⇒ shuffleWith g es ≠shuffleWith g' es@
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
