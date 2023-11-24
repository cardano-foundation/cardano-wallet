-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Extra glue functions to work with "MonadUnliftIO" exception types.  We need
-- this because the @retry@ package uses the generalized exception handler type
-- from 'Control.Monad.Catch.Handler'. But the 'UnliftIO.Exceptions' module has
-- its own definition of exactly the same type.

module UnliftIO.Compat
     ( -- * Handler conversion
       coerceHandler
     , coerceHandlers
     , mkRetryHandler

       -- * Missing combinators
     , handleIf

       -- * Re-export unsafe things
     , AsyncCancelled (..)
     ) where

import Prelude

import Cardano.Wallet.Network.Implementation.UnliftIO
    ( coerceHandler
    , coerceHandlers
    )
import Control.Concurrent.Async
    ( AsyncCancelled (..)
    )
import Control.Exception.Base
    ( Exception
    )
import Control.Monad.IO.Unlift
    ( MonadUnliftIO (..)
    )

import qualified Control.Monad.Catch as Exceptions
import qualified UnliftIO.Exception as UnliftIO

-- | Shortcut for creating a single 'Control.Retry' handler, which doesn't use
-- the 'Control.Retry.RetryStatus' info.
mkRetryHandler
    :: Exception e
    => (e -> m Bool)
    -> [a -> Exceptions.Handler m Bool]
mkRetryHandler shouldRetry = [const $ Exceptions.Handler shouldRetry]

-- | A 'MonadUnliftIO' version of 'Control.Monad.Catch.handleIf'.
handleIf
    :: (MonadUnliftIO m, Exception e)
    => (e -> Bool)
    -> (e -> m a)
    -> m a
    -> m a
handleIf f h = UnliftIO.handle
    (\e -> if f e then h e else UnliftIO.throwIO e)
