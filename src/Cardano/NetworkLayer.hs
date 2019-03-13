{-# LANGUAGE DataKinds #-}

module Cardano.NetworkLayer where

import Cardano.Wallet.Primitive
    ( Block, BlockHeader (..), Hash (..), SlotId )
import Control.Monad.Except
    ( ExceptT )
import Numeric.Natural
    ( Natural )


data NetworkLayer m e0 e1 = NetworkLayer
    { nextBlocks
        :: Natural -- ^ Number of blocks to retrieve
        -> SlotId -- ^ Starting Point
        -> ExceptT e0 m [Block]

    , networkTip
        :: ExceptT e1 m (Hash "BlockHeader", BlockHeader)
    }
