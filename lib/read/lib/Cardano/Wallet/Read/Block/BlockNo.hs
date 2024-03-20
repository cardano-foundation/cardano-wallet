{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Read.Block.BlockNo
    ( getEraBlockNo
    , BlockNo (..)
    ) where

import Prelude

import Cardano.Ledger.Binary.Group
    ( EncCBORGroup
    )
import Cardano.Ledger.Crypto
    ( Crypto
    )
import Cardano.Ledger.Era
    ( EraSegWits (..)
    )
import Cardano.Wallet.Read.Block.Block
    ( Block (..)
    )
import Cardano.Wallet.Read.Eras.KnownEras
    ( Era (..)
    , IsEra (..)
    )
import Numeric.Natural
    ( Natural
    )
import Ouroboros.Consensus.Protocol.Praos
    ( Praos
    )
import Ouroboros.Consensus.Protocol.TPraos
    ( TPraos
    )

import qualified Cardano.Ledger.Api as L
import qualified Cardano.Ledger.Shelley.API as Shelley
import qualified Cardano.Protocol.TPraos.BHeader as Shelley
import qualified Ouroboros.Consensus.Protocol.Praos.Header as O
import qualified Ouroboros.Consensus.Shelley.Ledger.Block as O
import qualified Ouroboros.Network.Block as O

getEraBlockNo :: forall era. IsEra era => Block era -> BlockNo
getEraBlockNo = case theEra @era of
    Byron -> \(Block block) -> k $ O.blockNo block
    Shelley -> \(Block block) -> k $ getBlockNoShelley block
    Allegra -> \(Block block) -> k $ getBlockNoShelley block
    Mary -> \(Block block) -> k $ getBlockNoShelley block
    Alonzo -> \(Block block) -> k $ getBlockNoShelley block
    Babbage -> \(Block block) -> k $ getBlockNoBabbage block
    Conway -> \(Block block) -> k $ getBlockNoBabbage block
  where
    k = BlockNo . fromIntegral . O.unBlockNo

newtype BlockNo = BlockNo {unBlockNo :: Natural}
    deriving (Eq, Show, Enum)

getBlockNoShelley
    :: (L.Era era, EncCBORGroup (TxSeq era), Crypto c)
    => O.ShelleyBlock (TPraos c) era
    -> O.BlockNo
getBlockNoShelley
    (O.ShelleyBlock (Shelley.Block (Shelley.BHeader header _) _) _) =
        Shelley.bheaderBlockNo header
getBlockNoBabbage
    :: (L.Era era, EncCBORGroup (TxSeq era), Crypto crypto)
    => O.ShelleyBlock (Praos crypto) era
    -> O.BlockNo
getBlockNoBabbage
    (O.ShelleyBlock (Shelley.Block (O.Header header _) _) _) =
        O.hbBlockNo header
