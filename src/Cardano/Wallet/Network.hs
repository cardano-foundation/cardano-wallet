{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.Network
    (
    -- * Interface
      NetworkLayer (..)

    -- * Errors
    , ErrNetworkUnreachable(..)
    , ErrNetworkTip(..)
    , ErrPostTx(..)
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types
    ( Block (..), BlockHeader (..), Hash (..), SignedTx, SlotId (..) )
import Control.Exception
    ( Exception )
import Control.Monad.Trans.Except
    ( ExceptT )
import Data.Text
    ( Text )
import GHC.Generics
    ( Generic )

data NetworkLayer m = NetworkLayer
    { nextBlocks :: SlotId -> ExceptT ErrNetworkUnreachable m [Block]
        -- ^ Gets some blocks from the node. It will not necessarily return all
        -- the blocks that the node has, but will receive a reasonable-sized
        -- chunk. It will never return blocks from before the given slot. It
        -- may return an empty list if the node does not have any blocks from
        -- after the starting slot.

    , networkTip
        :: ExceptT ErrNetworkTip m (Hash "BlockHeader", BlockHeader)
        -- ^ Get the current network tip from the chain producer

    , postTx
        :: SignedTx -> ExceptT ErrPostTx m ()
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
