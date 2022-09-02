{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Wallet.Primitive.Types.Tx.TokenBundleAssessments
    (
      TokenBundleSizeAssessor (..)
    , TokenBundleSizeAssessment (..)

    ) where

import Prelude

import GHC.Generics
    ( Generic )

import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle )

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
--   can be included in a single transaction output, defined by the protocol.
--
data TokenBundleSizeAssessment
    = TokenBundleSizeWithinLimit
    -- ^ Indicates that the size of a token bundle does not exceed the maximum
    -- size that can be included in a transaction output.
    | TokenBundleSizeExceedsLimit
    -- ^ Indicates that the size of a token bundle exceeds the maximum size
    -- that can be included in a transaction output.
    deriving (Eq, Generic, Show)
