module Cardano.Wallet.Spec.Data.Network.Info where

import Cardano.Wallet.Spec.Data.Network.NodeStatus
    ( NodeStatus )

newtype NetworkInfo = NetworkInfo
    { nodeStatus :: NodeStatus
    }
    deriving stock (Show)
