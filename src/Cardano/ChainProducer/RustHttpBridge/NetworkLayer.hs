{-# LANGUAGE DataKinds #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: MIT
--
-- Representation of a network layer

module Cardano.ChainProducer.RustHttpBridge.NetworkLayer
    ( NetworkLayer (..)
    , NetworkLayerError(..)
    ) where

import Prelude

import Cardano.Wallet.Primitive
    ( Block (..), BlockHeader (..), Hash (..) )
import Cardano.Wallet.Slotting
    ( EpochIndex )
import Control.Exception
    ( Exception (..) )
import Control.Monad.Except
    ( ExceptT )

-- | Endpoints of the cardano-http-bridge API.
data NetworkLayer m = NetworkLayer
    { getBlock
        :: Hash "BlockHeader" -> ExceptT NetworkLayerError m Block
    , getEpoch
        :: EpochIndex -> ExceptT NetworkLayerError m [Block]
    , getNetworkTip
        :: ExceptT NetworkLayerError m (Hash "BlockHeader", BlockHeader)
    }

newtype NetworkLayerError
    = NetworkLayerError String
    deriving (Show, Eq)

instance Exception NetworkLayerError
