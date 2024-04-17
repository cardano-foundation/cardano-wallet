{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Copyright: © 2024 Cardano Foundation
-- License: Apache-2.0
--
-- The 'BHeader' type represents a block header.
-}
module Cardano.Wallet.Read.Block.BHeader
    ( BHeader (..)
    , BHeaderT
    ) where

import Prelude

import Cardano.Ledger.Api
    ( StandardCrypto
    )
import Cardano.Ledger.Block
    ( bheader
    )
import Cardano.Wallet.Read.Eras
    ( Allegra
    , Alonzo
    , Babbage
    , Byron
    , Conway
    , Mary
    , Shelley
    )
import Cardano.Wallet.Read.Eras.KnownEras
    ( Era (..)
    , IsEra (..)
    )
import Ouroboros.Consensus.Protocol.Praos
    ( Header
    , Praos
    )
import Ouroboros.Consensus.Protocol.TPraos
    ( TPraos
    )

import qualified Cardano.Protocol.TPraos.BHeader as TPraos
import qualified Ouroboros.Consensus.Byron.Ledger.Block as Byron
import qualified Ouroboros.Consensus.Shelley.Ledger.Block as O
import qualified Ouroboros.Network.Block as O

type family BHeaderT era where
    BHeaderT Byron = Byron.Header Byron.ByronBlock
    BHeaderT Shelley = TPraos.BHeader StandardCrypto
    BHeaderT Allegra = TPraos.BHeader StandardCrypto
    BHeaderT Mary = TPraos.BHeader StandardCrypto
    BHeaderT Alonzo = TPraos.BHeader StandardCrypto
    BHeaderT Babbage = Header StandardCrypto
    BHeaderT Conway = Header StandardCrypto

newtype BHeader era = BHeader {unBHeader :: BHeaderT era}

deriving instance Show (BHeaderT era) => Show (BHeaderT era)
deriving instance Eq (BHeaderT era) => Eq (BHeaderT era)

{-# INLINABLE getEraBHeader #-}
getEraBHeader :: forall era. IsEra era => Block era -> BHeader era
getEraBHeader = case theEra @era of
    Byron -> \(Block block) -> BHeader $ Byron.getHeader block
    Shelley -> \(Block block) -> BHeader $ bheader block
    Allegra -> \(Block block) -> BHeader $ bheader block
    Mary -> \(Block block) -> BHeader $ bheader block
    Alonzo -> \(Block block) -> BHeader $ bheader block
    Babbage -> \(Block block) -> BHeader $ bheader block
    Conway -> \(Block block) -> BHeader $ bheader block
