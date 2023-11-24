{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

module Cardano.Wallet.Primitive.Types.GenesisParameters
    ( GenesisParameters (..)
    )
where

import Prelude

import Cardano.Wallet.Primitive.Slotting (StartTime (StartTime))
import Cardano.Wallet.Primitive.Types.Hash (Hash (getHash))
import Control.DeepSeq (NFData)
import Data.ByteArray.Encoding (Base (Base16), convertToBase)
import Fmt (Buildable (..), blockListF')
import GHC.Generics (Generic)

import qualified Data.Text.Encoding as T

-- | Parameters defined by the __genesis block__.
--
-- At present, these values cannot be changed through the update system.
--
-- They can only be changed through a soft or hard fork.
data GenesisParameters = GenesisParameters
    { getGenesisBlockHash :: Hash "Genesis"
    -- ^ Hash of the very first block
    , getGenesisBlockDate :: StartTime
    -- ^ Start time of the chain.
    }
    deriving (Generic, Show, Eq)

instance NFData GenesisParameters

instance Buildable GenesisParameters where
    build gp =
        blockListF'
            ""
            id
            [ "Genesis block hash: " <> genesisF (getGenesisBlockHash gp)
            , "Genesis block date: "
                <> startTimeF
                    ( getGenesisBlockDate
                        (gp :: GenesisParameters)
                    )
            ]
      where
        genesisF = build . T.decodeUtf8 . convertToBase Base16 . getHash
        startTimeF (StartTime s) = build s