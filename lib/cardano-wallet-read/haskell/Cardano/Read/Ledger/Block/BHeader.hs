{-# LANGUAGE UndecidableInstances #-}

{- |
Copyright: © 2024 Cardano Foundation
License: Apache-2.0

Era-indexed block header type and extraction.
-}
module Cardano.Read.Ledger.Block.BHeader
    ( -- * Block header type
      BHeader (..)
    , BHeaderT

      -- * Extraction
    , getEraBHeader
    ) where

import Prelude

import Cardano.Ledger.Block
    ( bheader
    )
import Cardano.Protocol.Crypto
    ( StandardCrypto
    )
import qualified Cardano.Protocol.TPraos.BHeader as TPraos
import Cardano.Read.Ledger.Block.Block
    ( Block (..)
    )
import Cardano.Read.Ledger.Eras
    ( Allegra
    , Alonzo
    , Babbage
    , Byron
    , Conway
    , Era (..)
    , IsEra (..)
    , Mary
    , Shelley
    )
import GHC.Generics
    ( Generic
    )
import Ouroboros.Consensus.Block.Abstract
    ( getHeader
    )
import qualified Ouroboros.Consensus.Byron.Ledger.Block as Byron
import Ouroboros.Consensus.Protocol.Praos.Header
    ( Header
    )
import qualified Ouroboros.Consensus.Shelley.Ledger.Block as O
import Ouroboros.Consensus.Shelley.Protocol.Praos
    ()
import Ouroboros.Consensus.Shelley.Protocol.TPraos
    ()

-- | Family of era-specific block header types.
type family BHeaderT era where
    BHeaderT Byron = Byron.Header Byron.ByronBlock
    BHeaderT Shelley = TPraos.BHeader StandardCrypto
    BHeaderT Allegra = TPraos.BHeader StandardCrypto
    BHeaderT Mary = TPraos.BHeader StandardCrypto
    BHeaderT Alonzo = TPraos.BHeader StandardCrypto
    BHeaderT Babbage = Header StandardCrypto
    BHeaderT Conway = Header StandardCrypto

-- | Era-indexed block header wrapper.
newtype BHeader era = BHeader {unBHeader :: BHeaderT era}
    deriving (Generic)

deriving instance Show (BHeaderT era) => Show (BHeader era)
deriving instance Eq (BHeaderT era) => Eq (BHeader era)

{-# INLINEABLE getEraBHeader #-}

-- | Extract the block header from a block in any era.
getEraBHeader :: forall era. IsEra era => Block era -> BHeader era
getEraBHeader = case theEra :: Era era of
    Byron -> \(Block block) -> BHeader $ getHeader block
    Shelley -> \(Block (O.ShelleyBlock block _)) -> BHeader $ bheader block
    Allegra -> \(Block (O.ShelleyBlock block _)) -> BHeader $ bheader block
    Mary -> \(Block (O.ShelleyBlock block _)) -> BHeader $ bheader block
    Alonzo -> \(Block (O.ShelleyBlock block _)) -> BHeader $ bheader block
    Babbage -> \(Block (O.ShelleyBlock block _)) -> BHeader $ bheader block
    Conway -> \(Block (O.ShelleyBlock block _)) -> BHeader $ bheader block
