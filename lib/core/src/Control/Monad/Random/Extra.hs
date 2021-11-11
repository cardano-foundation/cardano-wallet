{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2021 IOHK
-- License: Apache-2.0
--
-- This module provides functions and types that extend those provided by
-- the 'Control.Monad.Random' module hierarchy.
--
module Control.Monad.Random.Extra
    (
    -- * Random number generator states
      MonadRandomState (..)

    -- * Random number generator seeds
    , StdGenSeed (..)
    , stdGenFromSeed
    , stdGenToSeed

    -- * Non-random contexts
    , NonRandom (..)

    ) where

import Prelude

import Control.Applicative
    ( Applicative (..) )
import Control.Monad.Random.Class
    ( MonadRandom (..) )
import Data.Aeson
    ( FromJSON (..), ToJSON (..), Value (Number) )
import Data.Aeson.Extra
    ( parseBoundedIntegral )
import Data.Bits
    ( (.|.) )
import Data.Coerce
    ( coerce )
import Data.WideWord.Word128
    ( Word128 (..) )
import Data.Word
    ( Word64 )
import Data.Word.Odd
    ( Lit, OddWord )
import GHC.Generics
    ( Generic )
import Quiet
    ( Quiet (..) )
import System.Random
    ( Random (..), RandomGen (..), getStdGen, setStdGen )
import System.Random.Internal
    ( StdGen (..) )
import System.Random.SplitMix
    ( seedSMGen', unseedSMGen )

import qualified Data.Bits as Bits

--------------------------------------------------------------------------------
-- Random number generator states
--------------------------------------------------------------------------------

-- | Provides support for manipulating the state of the random number generator
--   associated with a 'MonadRandom' context.
--
-- Instances of this class should satisfy the following law:
--
-- >>> (setRandomSeed s >> getRandomSeed) == pure s
--
class MonadRandom m => MonadRandomState m where
    type RandomSeed m
    getRandomSeed :: m (RandomSeed m)
    setRandomSeed :: RandomSeed m -> m ()

instance MonadRandomState IO where
    type RandomSeed IO = StdGenSeed
    getRandomSeed = stdGenToSeed <$> getStdGen
    setRandomSeed = setStdGen . stdGenFromSeed

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
--
newtype StdGenSeed = StdGenSeed
    { unStdGenSeed :: Word127
    }
    deriving (Eq, Bounded, Generic, Ord)
    deriving Show via (Quiet StdGenSeed)

type Word127 = OddWord Word128 (Lit 127)

instance ToJSON StdGenSeed where
    toJSON = toJSON . Number . fromIntegral . unStdGenSeed

instance FromJSON StdGenSeed where
    parseJSON = fmap StdGenSeed . parseBoundedIntegral "StdGenSeed"

-- | Converts a 'StdGenSeed' value to a 'StdGen' value.
--
-- This function satisfies the following properties:
--
-- >>> stdGenFromSeed . stdGenToSeed == id
-- >>> stdGenToSeed . stdGenFromSeed == id
--
stdGenFromSeed :: StdGenSeed -> StdGen
stdGenFromSeed
    = StdGen
    . seedSMGen'
    . (\s -> (,)
        (fromIntegral @Word127 @Word64 (s `Bits.shiftR` 63))
        (fromIntegral @Word127 @Word64 (s `Bits.shiftL` 1)))
    . unStdGenSeed

-- | Converts a 'StdGen' value to a 'StdGenSeed' value.
--
-- This function satisfies the following properties:
--
-- >>> stdGenFromSeed . stdGenToSeed == id
-- >>> stdGenToSeed . stdGenFromSeed == id
--
stdGenToSeed :: StdGen -> StdGenSeed
stdGenToSeed
    = StdGenSeed
    . (\(a, b) -> (.|.)
        (fromIntegral @Word64 @Word127 a `Bits.shiftL` 63)
        (fromIntegral @Word64 @Word127 b `Bits.shiftR` 1))
    . unseedSMGen
    . unStdGen

--------------------------------------------------------------------------------
-- Non-random contexts
--------------------------------------------------------------------------------

-- | Provides a stateless context for computations that must be non-random.
--
-- This type is useful for testing functions that require a 'MonadRandom'
-- context, but when actual randomness is not required or even desired.
--
newtype NonRandom a = NonRandom
    { runNonRandom :: a }
    deriving (Eq, Generic, Ord, Show)

instance Functor NonRandom where
    fmap = coerce

instance Applicative NonRandom where
    liftA2 = coerce
    pure = NonRandom
    (<*>) = coerce

instance Monad NonRandom where
    m >>= k = k (runNonRandom m)

instance MonadRandom NonRandom where
    getRandom = pure $ fst $ random NonRandomGen
    getRandomR r = pure $ fst $ randomR r NonRandomGen
    getRandomRs r = pure $ randomRs r NonRandomGen
    getRandoms = pure $ randoms NonRandomGen

instance MonadRandomState NonRandom where
    type RandomSeed NonRandom = ()
    getRandomSeed = pure ()
    setRandomSeed () = pure ()

-- | Provides a stateless and non-random implementation of 'RandomGen'
--
data NonRandomGen = NonRandomGen

instance RandomGen NonRandomGen where
    genRange NonRandomGen = (minBound, maxBound)
    next NonRandomGen = (0, NonRandomGen)
    split NonRandomGen = (NonRandomGen, NonRandomGen)
