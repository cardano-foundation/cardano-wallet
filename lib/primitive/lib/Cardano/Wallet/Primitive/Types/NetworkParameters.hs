{-# LANGUAGE DeriveGeneric #-}
module Cardano.Wallet.Primitive.Types.NetworkParameters
    ( NetworkParameters (..)
    )
where

import Prelude

import Cardano.Wallet.Primitive.Types.GenesisParameters
    ( GenesisParameters
    )
import Cardano.Wallet.Primitive.Types.ProtocolParameters
    ( ProtocolParameters
    )
import Cardano.Wallet.Primitive.Types.SlottingParameters
    ( SlottingParameters
    )
import Control.DeepSeq
    ( NFData
    )
import Fmt
    ( Buildable (..)
    )
import GHC.Generics
    ( Generic
    )

-- | Records the complete set of parameters currently in use by the network
--   that are relevant to the wallet.
data NetworkParameters = NetworkParameters
    { genesisParameters :: GenesisParameters
    -- ^ See 'GenesisParameters'.
    , slottingParameters :: SlottingParameters
    -- ^ See 'SlottingParameters'.
    , protocolParameters :: ProtocolParameters
    -- ^ See 'ProtocolParameters'.
    }
    deriving (Generic, Show, Eq)

instance NFData NetworkParameters

instance Buildable NetworkParameters where
    build (NetworkParameters gp sp pp) = build gp <> build sp <> build pp
