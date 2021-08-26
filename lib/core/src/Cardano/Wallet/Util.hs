-- |
-- Copyright: © 2020-2021 IOHK
-- License: Apache-2.0
--
-- General utility functions.
--
module Cardano.Wallet.Util
    ( -- * Partial functions for "impossible" situations
      HasCallStack
    , internalError
    , tina
    , invariant

    -- ** Handling errors for "impossible" situations.
    , isInternalError
    , tryInternalError
    ) where

import Prelude

import Control.Exception
    ( ErrorCall, displayException )
import Control.Monad.IO.Unlift
    ( MonadUnliftIO )
import Data.Foldable
    ( asum )
import Data.List
    ( isPrefixOf )
import Data.Maybe
    ( fromMaybe )
import Fmt
    ( Buildable (..), Builder, fmt, (+|) )
import GHC.Generics
    ( Generic )
import GHC.Stack
    ( HasCallStack )
import UnliftIO.Exception
    ( evaluate, tryJust )

-- | Calls the 'error' function, which will usually crash the program.
internalError :: HasCallStack => Builder -> a
internalError msg = error $ fmt $ "INTERNAL ERROR: "+|msg

isInternalErrorMsg :: String -> Bool
isInternalErrorMsg msg = "INTERNAL ERROR" `isPrefixOf` msg

-- | Take the first 'Just' from a list of 'Maybe', or die trying.
-- There is no alternative.
tina :: HasCallStack => Builder -> [Maybe a] -> a
tina msg = fromMaybe (internalError msg) . asum

-- | Checks whether or not an invariant holds, by applying the given predicate
--   to the given value.
--
-- If the invariant does not hold (indicated by the predicate function
-- returning 'False'), throws an error with the specified message.
--
-- >>> invariant "not empty" [1,2,3] (not . null)
-- [1, 2, 3]
--
-- >>> invariant "not empty" [] (not . null)
-- *** Exception: not empty
invariant
    :: HasCallStack
    => String
        -- ^ The message
    -> a
        -- ^ The value to test
    -> (a -> Bool)
        -- ^ The predicate
    -> a
invariant msg a predicate = if predicate a then a else error msg

-- | Tests whether an 'Exception' was caused by 'internalError'.
isInternalError :: ErrorCall -> Maybe String
isInternalError (displayException -> msg)
    | isInternalErrorMsg msg = Just msg
    | otherwise = Nothing

-- | Evaluates a pure expression to WHNF and handles any occurrence of
-- 'internalError'.
--
-- This is intended for use in testing. Don't use this in application code --
-- that's what normal IO exceptions are for.
tryInternalError :: MonadUnliftIO m => a -> m (Either String a)
tryInternalError = tryJust isInternalError . evaluate
