{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2021-2022 IOHK
-- License: Apache-2.0
--
-- This module provides the 'StdGenSeed' type and related functions.
module System.Random.StdGenSeed
    ( -- * Random number generator seeds
      StdGenSeed (..)
    , stdGenSeed
    , stdGenFromSeed
    , stdGenToSeed
    ) where

import Prelude

import Control.Monad.Random.Class
    ( MonadRandom (..)
    )
import Data.Bits
    ( (.|.)
    )
import Data.Word
    ( Word64
    )
import Data.Word.Odd
    ( Lit
    , OddWord
    )
import GHC.Generics
    ( Generic
    )
import Quiet
    ( Quiet (..)
    )
import System.Random.Internal
    ( StdGen (..)
    )
import System.Random.SplitMix
    ( seedSMGen'
    , unseedSMGen
    )

import qualified Data.Bits as Bits

--------------------------------------------------------------------------------
-- Random number generator seeds
--------------------------------------------------------------------------------

-- | A seed for the standard random number generator.
--
-- This type is equivalent to the internal state of a 'StdGen', but provides a
-- representation that is more convenient for construction and serialization.
--
-- The number of possible seeds is identical to the number of valid states of
-- the 'StdGen' type, but unlike the 'StdGen' type, whose state has an internal
-- invariant that must not be broken, values of the 'StdGenSeed' type are
-- correct by construction.
newtype StdGenSeed = StdGenSeed
    { unStdGenSeed :: Word127
    }
    deriving (Eq, Bounded, Generic, Ord)
    deriving (Show) via (Quiet StdGenSeed)

type Word127 = OddWord Integer (Lit 127)

-- | Creates a new 'StdGenSeed' from within a random monadic context.
stdGenSeed :: (MonadRandom m) => m StdGenSeed
stdGenSeed = do
    hi <- getRandom
    lo <- getRandom
    pure
        $ StdGenSeed
        $ (.|.)
            (fromIntegral @Word64 @Word127 hi `Bits.shiftL` 63)
            (fromIntegral @Word64 @Word127 lo)

-- | Converts a 'StdGenSeed' value to a 'StdGen' value.
--
-- This function satisfies the following properties:
--
-- >>> stdGenFromSeed . stdGenToSeed == id
-- >>> stdGenToSeed . stdGenFromSeed == id
stdGenFromSeed :: StdGenSeed -> StdGen
stdGenFromSeed =
    StdGen
        . seedSMGen'
        . ( \s ->
                (,)
                    (fromIntegral @Word127 @Word64 (s `Bits.shiftR` 63))
                    (fromIntegral @Word127 @Word64 (s `Bits.shiftL` 1))
          )
        . unStdGenSeed

-- | Converts a 'StdGen' value to a 'StdGenSeed' value.
--
-- This function satisfies the following properties:
--
-- >>> stdGenFromSeed . stdGenToSeed == id
-- >>> stdGenToSeed . stdGenFromSeed == id
stdGenToSeed :: StdGen -> StdGenSeed
stdGenToSeed =
    StdGenSeed
        . ( \(a, b) ->
                (.|.)
                    (fromIntegral @Word64 @Word127 a `Bits.shiftL` 63)
                    (fromIntegral @Word64 @Word127 b `Bits.shiftR` 1)
          )
        . unseedSMGen
        . unStdGen
