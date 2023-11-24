module Cardano.Wallet.Network.Implementation.UnliftIO
    ( coerceHandler
    , coerceHandlers
    )
where

import qualified Control.Monad.Catch as Exceptions
import qualified UnliftIO

-- | Convert the generalized handler from 'UnliftIO.Exception' type to 'Control.Monad.Catch' type
coerceHandler :: UnliftIO.Handler IO b -> Exceptions.Handler IO b
coerceHandler (UnliftIO.Handler h) = Exceptions.Handler h

-- | Convert a list of handler factories from the 'UnliftIO.Exception' type to
-- 'Control.Monad.Catch' type. Such handlers are used in
-- 'Control.Retry.Recovering' for example.
coerceHandlers
    :: [a -> UnliftIO.Handler IO b]
    -> [a -> Exceptions.Handler IO b]
coerceHandlers = map (coerceHandler .)
