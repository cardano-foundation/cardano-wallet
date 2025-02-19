module Cardano.Wallet.Deposit.IO.Resource.Event
    ( onResourceChange
    ) where

import Prelude

import Cardano.Wallet.Deposit.IO.Resource
    ( Resource
    , ResourceStatus (..)
    , readStatus
    )
import Control.Concurrent.Async
    ( withAsync
    )
import Control.Concurrent.Class.MonadSTM
    ( MonadSTM (..)
    , atomically
    )
import Control.Monad
    ( void
    )
import Control.Monad.Cont
    ( ContT (..)
    )
import Control.Monad.Fix
    ( fix
    )

-- | Run an action whenever the status of a 'Resource' changes.
onResourceChange
    :: (ResourceStatus e a -> IO ())
    -> Resource e a
    -> ContT x IO ()
onResourceChange f resource = do
    void $ ContT $ withAsync $ ($ Closed) $ fix $ \loop lastStatus -> do
        status <- atomically $ do
            status <- readStatus resource
            case (status, lastStatus) of
                (Closed, Closed) -> retry
                (Opening, Opening) -> retry
                (Open _a, Open _a') -> retry -- this is something to think about
                (FailedToOpen _e, FailedToOpen _e') -> retry
                (Vanished _e, Vanished _e') -> retry
                (Closing, Closing) -> retry
                _ -> pure ()
            pure status
        f status
        loop status
