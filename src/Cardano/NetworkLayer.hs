{-# LANGUAGE DataKinds #-}

module Cardano.NetworkLayer
    ( NetworkLayer (..)
    ) where

import Cardano.Wallet.Primitive
    ( Block, BlockHeader (..), Hash (..), SlotId )
import Control.Monad.Except
    ( ExceptT )


data NetworkLayer m e0 e1 = NetworkLayer
    { nextBlocks :: SlotId -> ExceptT e0 m [Block]
        -- ^ Gets some blocks from the node. It will not necessarily return all
        -- the blocks that the node has, but will receive a reasonable-sized
        -- chunk. It will never return blocks from before the given slot. It
        -- may return an empty list if the node does not have any blocks from
        -- after the starting slot.

    , networkTip
        :: ExceptT e1 m (Hash "BlockHeader", BlockHeader)
    }
