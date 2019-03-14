{-# LANGUAGE DataKinds #-}

module Cardano.NetworkLayer
    ( NetworkLayer (..)
    ) where

import Cardano.Wallet.Primitive
    ( Block, BlockHeader (..), Hash (..), SlotId )
import Control.Monad.Except
    ( ExceptT )
import Data.Word
    ( Word64 )


data NetworkLayer m e0 e1 = NetworkLayer
    { nextBlocks :: Word64 -> SlotId -> ExceptT e0 m [Block]
    , networkTip :: ExceptT e1 m (Hash "BlockHeader", BlockHeader)
    }
