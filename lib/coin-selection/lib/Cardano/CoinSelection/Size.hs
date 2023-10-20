{-# LANGUAGE DeriveGeneric #-}

-- |
-- Copyright: © 2023 Cardano Foundation
-- License: Apache-2.0
--
-- Types and functions relating to the coin selection algorithm's abstract size
-- model.
--
module Cardano.CoinSelection.Size
    ( TokenBundleSizeAssessor (..)
    , TokenBundleSizeAssessment (..)
    )
    where

import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle (..)
    )
import GHC.Generics
    ( Generic
    )

import Prelude

-- | A function capable of assessing the size of a token bundle relative to the
--   upper limit of what can be included in a single transaction output.
--
-- In general, a token bundle size assessment function 'f' should satisfy the
-- following properties:
--
--    * Enlarging a bundle that exceeds the limit should also result in a
--      bundle that exceeds the limit:
--      @
--              f  b1           == TokenBundleSizeExceedsLimit
--          ==> f (b1 `add` b2) == TokenBundleSizeExceedsLimit
--      @
--
--    * Shrinking a bundle that's within the limit should also result in a
--      bundle that's within the limit:
--      @
--              f  b1                  == TokenBundleWithinLimit
--          ==> f (b1 `difference` b2) == TokenBundleWithinLimit
--      @
--
newtype TokenBundleSizeAssessor = TokenBundleSizeAssessor
    { assessTokenBundleSize :: TokenBundle -> TokenBundleSizeAssessment
    }
    deriving Generic

-- | Indicates the size of a token bundle relative to the upper limit of what
--   can be included in a single transaction output.
--
data TokenBundleSizeAssessment
    = TokenBundleSizeWithinLimit
    -- ^ Indicates that the size of a token bundle does not exceed the maximum
    -- size that can be included in a transaction output.
    | TokenBundleSizeExceedsLimit
    -- ^ Indicates that the size of a token bundle exceeds the maximum size
    -- that can be included in a transaction output.
    deriving (Eq, Generic, Show)
