{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.Network
    (
    -- * Interface
      NetworkLayer (..)

    -- * Helpers
    , waitForConnection
    , defaultRetryPolicy

    -- * Errors
    , ErrNetworkUnreachable(..)
    , ErrNetworkTip(..)
    , ErrPostTx(..)
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types
    ( Block (..), BlockHeader (..), Tx, TxWitness )
import Control.Exception
    ( Exception, throwIO )
import Control.Monad.Trans.Except
    ( ExceptT, runExceptT )
import Control.Retry
import Data.Text
    ( Text )
import GHC.Generics
    ( Generic )

data NetworkLayer t m = NetworkLayer
    { nextBlocks :: BlockHeader -> ExceptT ErrNetworkUnreachable m [Block]
        -- ^ Gets some blocks from the node. It will not necessarily return all
        -- the blocks that the node has, but will receive a reasonable-sized
        -- chunk. It will never return blocks from before the given slot. It
        -- may return an empty list if the node does not have any blocks from
        -- after the starting slot.

    , networkTip
        :: ExceptT ErrNetworkTip m BlockHeader
        -- ^ Get the current network tip from the chain producer

    , postTx
        :: (Tx, [TxWitness]) -> ExceptT ErrPostTx m ()
        -- ^ Broadcast a transaction to the chain producer
    }

-- | Network is not reachable
newtype ErrNetworkUnreachable
    = ErrNetworkUnreachable Text
    deriving (Generic, Show, Eq)

instance Exception ErrNetworkUnreachable

-- | Error while trying to get the network tip
data ErrNetworkTip
    = ErrNetworkTipNetworkUnreachable ErrNetworkUnreachable
    | ErrNetworkTipNotFound
    deriving (Generic, Show, Eq)

instance Exception ErrNetworkTip

-- | Error while trying to send a transaction
data ErrPostTx
    = ErrPostTxNetworkUnreachable ErrNetworkUnreachable
    | ErrPostTxBadRequest Text
    | ErrPostTxProtocolFailure Text
    deriving (Generic, Show, Eq)

instance Exception ErrPostTx

-- | Wait until 'networkTip networkLayer' succeeds according to a given
-- retry policy. Throws an exception otherwise.
waitForConnection
    :: NetworkLayer t IO
    -> RetryPolicyM IO
    -> IO ()
waitForConnection nw policy = do
    r <- retrying policy shouldRetry (const $ runExceptT (networkTip nw))
    case r of
        Right _ -> return ()
        Left e -> throwIO e
  where
    shouldRetry _ = \case
        Right _ ->
            return False
        Left ErrNetworkTipNotFound ->
            return True
        Left (ErrNetworkTipNetworkUnreachable _) ->
            return True

-- | A default 'RetryPolicy' with a constant delay, but retries for no longer
-- than 20 seconds.
defaultRetryPolicy :: Monad m => RetryPolicyM m
defaultRetryPolicy =
    limitRetriesByCumulativeDelay (20 * second) (constantDelay (1 * second))
  where
    second = 1000*1000
