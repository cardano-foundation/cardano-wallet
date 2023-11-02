{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

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

import Cardano.Api
    ( AllegraEra
    , AlonzoEra
    , BabbageEra
    , ByronEra
    , ConwayEra
    , MaryEra
    , ShelleyEra
    )
import Cardano.Ledger.Binary
    ( EncCBOR
    , EncCBORGroup
    )
import Cardano.Ledger.Crypto
    ( StandardCrypto
    )
import Cardano.Ledger.Era
    ( Era
    , EraSegWits (..)
    )
import Cardano.Protocol.TPraos.BHeader
    ( PrevHash
    )
import Cardano.Wallet.Read
    ( Block (..)
    )
import Cardano.Wallet.Read.Eras.EraFun
    ( EraFun (..)
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

import qualified Cardano.Ledger.Shelley.API as Shelley
import qualified Ouroboros.Consensus.Shelley.Ledger.Block as O
import qualified Ouroboros.Consensus.Shelley.Protocol.Abstract as Shelley
import qualified Ouroboros.Network.Block as O

type family HeaderHashT era where
    HeaderHashT ByronEra = ByronHash
    HeaderHashT ShelleyEra = ShelleyHash StandardCrypto
    HeaderHashT AllegraEra = ShelleyHash StandardCrypto
    HeaderHashT MaryEra = ShelleyHash StandardCrypto
    HeaderHashT AlonzoEra = ShelleyHash StandardCrypto
    HeaderHashT BabbageEra = ShelleyHash StandardCrypto
    HeaderHashT ConwayEra = ShelleyHash StandardCrypto

newtype HeaderHash era = HeaderHash (HeaderHashT era)

getEraHeaderHash :: EraFun Block HeaderHash
getEraHeaderHash =
    EraFun
        { byronFun = \(Block block) -> HeaderHash $ O.blockHash block
        , shelleyFun = \(Block block) -> HeaderHash $ getHeaderHashShelley block
        , allegraFun = \(Block block) -> HeaderHash $ getHeaderHashShelley block
        , maryFun = \(Block block) -> HeaderHash $ getHeaderHashShelley block
        , alonzoFun = \(Block block) -> HeaderHash $ getHeaderHashShelley block
        , babbageFun = \(Block block) -> HeaderHash $ getHeaderHashShelley block
        , conwayFun = \(Block block) -> HeaderHash $ getHeaderHashShelley block
        }

getHeaderHashShelley
    :: ( ProtoCrypto (praos StandardCrypto) ~ StandardCrypto
       , Shelley.ProtocolHeaderSupportsEnvelope (praos StandardCrypto)
       , Era era
       , EncCBORGroup (TxSeq era)
       , EncCBOR (Shelley.ShelleyProtocolHeader (praos StandardCrypto))
       )
    => O.ShelleyBlock (praos StandardCrypto) era
    -> ShelleyHash StandardCrypto
getHeaderHashShelley
    (O.ShelleyBlock (Shelley.Block header _) _) = Shelley.pHeaderHash header

type family PrevHeaderHashT era where
    PrevHeaderHashT ByronEra = O.ChainHash ByronBlock
    PrevHeaderHashT ShelleyEra = PrevHash StandardCrypto
    PrevHeaderHashT AllegraEra = PrevHash StandardCrypto
    PrevHeaderHashT MaryEra = PrevHash StandardCrypto
    PrevHeaderHashT AlonzoEra = PrevHash StandardCrypto
    PrevHeaderHashT BabbageEra = PrevHash StandardCrypto
    PrevHeaderHashT ConwayEra = PrevHash StandardCrypto

newtype PrevHeaderHash era = PrevHeaderHash (PrevHeaderHashT era)

getPrevHeaderHashShelley
    :: ( Era era
       , EncCBORGroup (TxSeq era)
       , EncCBOR (Shelley.ShelleyProtocolHeader proto)
       , Shelley.ProtocolHeaderSupportsEnvelope proto
       )
    => O.ShelleyBlock proto era
    -> PrevHash (ProtoCrypto proto)
getPrevHeaderHashShelley (O.ShelleyBlock (Shelley.Block header _) _) =
    Shelley.pHeaderPrevHash header

getEraPrevHeaderHash :: EraFun Block PrevHeaderHash
getEraPrevHeaderHash =
    EraFun
        { byronFun = \(Block block) ->
            PrevHeaderHash $ headerPrevHash $ O.getHeader block
        , shelleyFun = \(Block block) ->
            PrevHeaderHash $ getPrevHeaderHashShelley block
        , allegraFun = \(Block block) ->
            PrevHeaderHash $ getPrevHeaderHashShelley block
        , maryFun = \(Block block) ->
            PrevHeaderHash $ getPrevHeaderHashShelley block
        , alonzoFun = \(Block block) ->
            PrevHeaderHash $ getPrevHeaderHashShelley block
        , babbageFun = \(Block block) ->
            PrevHeaderHash $ getPrevHeaderHashShelley block
        , conwayFun = \(Block block) ->
            PrevHeaderHash $ getPrevHeaderHashShelley block
        }
