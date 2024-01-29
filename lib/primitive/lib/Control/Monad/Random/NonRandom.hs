{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2021-2022 IOHK
-- License: Apache-2.0
--
-- This module provides the 'NonRandom' type and related instances.
--
module Control.Monad.Random.NonRandom
    (
    -- * Non-random contexts
      NonRandom (..)

    ) where

import Prelude

import Control.Monad.Random.Class
    ( MonadRandom (..)
    )
import Data.Coerce
    ( coerce
    )
import GHC.Generics
    ( Generic
    )
import System.Random
    ( Random (..)
    , RandomGen (..)
    )

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

-- | Provides a stateless and non-random implementation of 'RandomGen'
--
data NonRandomGen = NonRandomGen

instance RandomGen NonRandomGen where
    genRange NonRandomGen = (minBound, maxBound)
    next NonRandomGen = (0, NonRandomGen)
    split NonRandomGen = (NonRandomGen, NonRandomGen)
