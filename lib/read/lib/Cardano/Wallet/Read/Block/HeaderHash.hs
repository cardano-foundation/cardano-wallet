{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Wallet.Read.Block.HeaderHash
    ( getEraHeaderHash
    , HeaderHash (..)
    , HeaderHashT
    , PrevHeaderHash (..)
    , PrevHeaderHashT
    , getEraPrevHeaderHash
    )
where

import Prelude

import Cardano.Ledger.Binary
    ( EncCBOR
    , EncCBORGroup
    )
import Cardano.Ledger.Crypto
    ( StandardCrypto
    )
import Cardano.Ledger.Era
    ( EraSegWits (..)
    )
import Cardano.Protocol.TPraos.BHeader
    ( PrevHash
    )
import Cardano.Wallet.Read.Block.Block
    ( Block (..)
    )
import Cardano.Wallet.Read.Eras
    ( Allegra
    , Alonzo
    , Babbage
    , Byron
    , Conway
    , IsEra
    , Mary
    , Shelley
    )
import Cardano.Wallet.Read.Eras.KnownEras
    ( Era (..)
    , IsEra (..)
    )
import Ouroboros.Consensus.Block.Abstract
    ( headerPrevHash
    )
import Ouroboros.Consensus.Byron.Ledger
    ( ByronBlock
    , ByronHash
    )
import Ouroboros.Consensus.Shelley.Ledger
    ( ShelleyHash
    )
import Ouroboros.Consensus.Shelley.Protocol.Abstract
    ( ProtoCrypto
    )
import Ouroboros.Consensus.Shelley.Protocol.Praos
    ()
import Ouroboros.Consensus.Shelley.Protocol.TPraos
    ()

import qualified Cardano.Ledger.Api as L
import qualified Cardano.Ledger.Shelley.API as Shelley
import qualified Ouroboros.Consensus.Shelley.Ledger.Block as O
import qualified Ouroboros.Consensus.Shelley.Protocol.Abstract as Shelley
import qualified Ouroboros.Network.Block as O

-- | Era-specific header hash type from the ledger
type family HeaderHashT era where
    HeaderHashT Byron = ByronHash
    HeaderHashT Shelley = ShelleyHash StandardCrypto
    HeaderHashT Allegra = ShelleyHash StandardCrypto
    HeaderHashT Mary = ShelleyHash StandardCrypto
    HeaderHashT Alonzo = ShelleyHash StandardCrypto
    HeaderHashT Babbage = ShelleyHash StandardCrypto
    HeaderHashT Conway = ShelleyHash StandardCrypto

-- | Era-specific header hash type from the ledger
newtype HeaderHash era = HeaderHash (HeaderHashT era)

getEraHeaderHash :: forall era . IsEra era => Block era -> HeaderHash era
getEraHeaderHash = case theEra @era of
    Byron -> \(Block block) -> HeaderHash $ O.blockHash block
    Shelley -> \(Block block) -> HeaderHash $ getHeaderHashShelley block
    Allegra -> \(Block block) -> HeaderHash $ getHeaderHashShelley block
    Mary -> \(Block block) -> HeaderHash $ getHeaderHashShelley block
    Alonzo -> \(Block block) -> HeaderHash $ getHeaderHashShelley block
    Babbage -> \(Block block) -> HeaderHash $ getHeaderHashShelley block
    Conway -> \(Block block) -> HeaderHash $ getHeaderHashShelley block

getHeaderHashShelley
    :: ( ProtoCrypto (praos StandardCrypto) ~ StandardCrypto
       , Shelley.ProtocolHeaderSupportsEnvelope (praos StandardCrypto)
       , L.Era era
       , EncCBORGroup (TxSeq era)
       , EncCBOR (Shelley.ShelleyProtocolHeader (praos StandardCrypto))
       )
    => O.ShelleyBlock (praos StandardCrypto) era
    -> ShelleyHash StandardCrypto
getHeaderHashShelley
    (O.ShelleyBlock (Shelley.Block header _) _) = Shelley.pHeaderHash header

-- | Era-specific previous header hash type from the ledger
type family PrevHeaderHashT era where
    PrevHeaderHashT Byron = O.ChainHash ByronBlock
    PrevHeaderHashT Shelley = PrevHash StandardCrypto
    PrevHeaderHashT Allegra = PrevHash StandardCrypto
    PrevHeaderHashT Mary = PrevHash StandardCrypto
    PrevHeaderHashT Alonzo = PrevHash StandardCrypto
    PrevHeaderHashT Babbage = PrevHash StandardCrypto
    PrevHeaderHashT Conway = PrevHash StandardCrypto

-- | Era-specific previous header hash type from the ledger
newtype PrevHeaderHash era = PrevHeaderHash (PrevHeaderHashT era)

getPrevHeaderHashShelley
    :: ( L.Era era
       , EncCBORGroup (TxSeq era)
       , EncCBOR (Shelley.ShelleyProtocolHeader proto)
       , Shelley.ProtocolHeaderSupportsEnvelope proto
       )
    => O.ShelleyBlock proto era
    -> PrevHash (ProtoCrypto proto)
getPrevHeaderHashShelley (O.ShelleyBlock (Shelley.Block header _) _) =
    Shelley.pHeaderPrevHash header

getEraPrevHeaderHash :: forall era . IsEra era => Block era -> PrevHeaderHash era
getEraPrevHeaderHash = case theEra @era of
    Byron -> \(Block block) -> PrevHeaderHash $ headerPrevHash $ O.getHeader block
    Shelley -> \(Block block) -> PrevHeaderHash $ getPrevHeaderHashShelley block
    Allegra -> \(Block block) -> PrevHeaderHash $ getPrevHeaderHashShelley block
    Mary -> \(Block block) -> PrevHeaderHash $ getPrevHeaderHashShelley block
    Alonzo -> \(Block block) -> PrevHeaderHash $ getPrevHeaderHashShelley block
    Babbage -> \(Block block) -> PrevHeaderHash $ getPrevHeaderHashShelley block
    Conway -> \(Block block) -> PrevHeaderHash $ getPrevHeaderHashShelley block
