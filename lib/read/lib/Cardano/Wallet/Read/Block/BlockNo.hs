{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
    ( Era
    , EraSegWits (..)
    )
import Cardano.Wallet.Read.Block.Block
    ( Block (..)
    )
import Cardano.Wallet.Read.Eras.EraFun
    ( EraFun (..)
    )
import Generics.SOP
    ( K (..)
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

import qualified Cardano.Ledger.Shelley.API as Shelley
import qualified Cardano.Protocol.TPraos.BHeader as Shelley
import qualified Ouroboros.Consensus.Protocol.Praos.Header as O
import qualified Ouroboros.Consensus.Shelley.Ledger.Block as O
import qualified Ouroboros.Network.Block as O

getEraBlockNo :: EraFun Block (K BlockNo)
getEraBlockNo =
    EraFun
        { byronFun = \(Block block) -> k $ O.blockNo block
        , shelleyFun = \(Block block) -> k $ getBlockNoShelley block
        , allegraFun = \(Block block) -> k $ getBlockNoShelley block
        , maryFun = \(Block block) -> k $ getBlockNoShelley block
        , alonzoFun = \(Block block) -> k $ getBlockNoShelley block
        , babbageFun = \(Block block) -> k $ getBlockNoBabbage block
        , conwayFun = \(Block block) -> k $ getBlockNoBabbage block
        }
    where
        k = K . BlockNo . fromIntegral . O.unBlockNo

newtype BlockNo = BlockNo {unBlockNo :: Natural}
    deriving (Eq, Show, Enum)

getBlockNoShelley
    :: (Era era, EncCBORGroup (TxSeq era), Crypto c)
    => O.ShelleyBlock (TPraos c) era
    -> O.BlockNo
getBlockNoShelley
    (O.ShelleyBlock (Shelley.Block (Shelley.BHeader header _) _) _) =
        Shelley.bheaderBlockNo header
getBlockNoBabbage
    :: (Era era, EncCBORGroup (TxSeq era), Crypto crypto)
    => O.ShelleyBlock (Praos crypto) era
    -> O.BlockNo
getBlockNoBabbage
    (O.ShelleyBlock (Shelley.Block (O.Header header _) _) _) =
        O.hbBlockNo header
