{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newTVarIO" #-}
{-# HLINT ignore "Use readTVarIO" #-}

module Control.Monitoring.Concurrent
    ( Register (..)
    , newRegister
    )
where

import Prelude

import Control.Concurrent.Class.MonadSTM
    ( MonadSTM (..)
    )

-- | A thread-safe register that can be read, be blocked on changing
data Register m a b = Register
    { readRegister :: m a
    -- ^ Read the register
    , changeRegister :: (a -> Maybe a) -> m ()
    -- ^ Block on the register until a new `a` is ready
    }

-- | Create a new `Register` with an initial value
newRegister :: MonadSTM m => a -> m (Register m a b)
newRegister a = do
    var <- atomically $ newTVar a
    pure
        $ Register
            { readRegister = atomically $ readTVar var
            , changeRegister = \f -> atomically $ do
                v <- readTVar var
                case f v of
                    Just v' -> writeTVar var v'
                    Nothing -> retry
            }
