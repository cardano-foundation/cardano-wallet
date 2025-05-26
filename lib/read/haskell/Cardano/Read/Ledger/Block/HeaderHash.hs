{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Read.Ledger.Block.HeaderHash
    ( HeaderHash (..)
    , HeaderHashT
    , getEraHeaderHash
    , EraIndependentBlockHeader
    , RawHeaderHash
    , getRawHeaderHash
    , PrevHeaderHash (..)
    , PrevHeaderHashT
    , getEraPrevHeaderHash

      -- * Testing utilities
    , mockRawHeaderHash
    )
where

import Prelude

import Cardano.Ledger.Binary
    ( EncCBOR
    , EncCBORGroup
    )
import Cardano.Ledger.Core
    ( EraSegWits (..)
    )
import Cardano.Ledger.Hashes
    ( EraIndependentBlockHeader
    )
import Cardano.Protocol.Crypto
    ( StandardCrypto
    )
import Cardano.Protocol.TPraos.BHeader
    ( PrevHash
    )
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
import Cardano.Wallet.Read.Hash
    ( Blake2b_256
    , Hash
    , castHash
    , hashFromBytesShort
    )
import Data.Maybe
    ( fromJust
    )
import Ouroboros.Consensus.Block.Abstract
    ( headerPrevHash
    )
import Ouroboros.Consensus.Byron.Ledger
    ( ByronBlock
    , ByronHash (unByronHash)
    )
import Ouroboros.Consensus.Shelley.Ledger
    ( ShelleyHash (unShelleyHash)
    )
import Ouroboros.Consensus.Shelley.Protocol.Praos
    ()
import Ouroboros.Consensus.Shelley.Protocol.TPraos
    ()

import qualified Cardano.Crypto.Hashing as Byron
import qualified Cardano.Ledger.Api as L
import qualified Cardano.Ledger.Shelley.API as Shelley
import qualified Cardano.Wallet.Read.Hash as Hash
import qualified Data.ByteString.Char8 as B8
import qualified Ouroboros.Consensus.Shelley.Ledger.Block as O
import qualified Ouroboros.Consensus.Shelley.Protocol.Abstract as Shelley
import qualified Ouroboros.Network.Block as O

{-----------------------------------------------------------------------------
    HeaderHash
------------------------------------------------------------------------------}

-- | Era-specific header hash type from the ledger
type family HeaderHashT era where
    HeaderHashT Byron = ByronHash
    HeaderHashT Shelley = ShelleyHash
    HeaderHashT Allegra = ShelleyHash
    HeaderHashT Mary = ShelleyHash
    HeaderHashT Alonzo = ShelleyHash
    HeaderHashT Babbage = ShelleyHash
    HeaderHashT Conway = ShelleyHash

-- | Era-specific header hash type from the ledger
newtype HeaderHash era = HeaderHash (HeaderHashT era)

{-# INLINEABLE getEraHeaderHash #-}
getEraHeaderHash
    :: forall era. IsEra era => Block era -> HeaderHash era
getEraHeaderHash = case theEra @era of
    Byron -> \(Block block) -> HeaderHash $ O.blockHash block
    Shelley -> \(Block block) -> HeaderHash $ getHeaderHashShelley block
    Allegra -> \(Block block) -> HeaderHash $ getHeaderHashShelley block
    Mary -> \(Block block) -> HeaderHash $ getHeaderHashShelley block
    Alonzo -> \(Block block) -> HeaderHash $ getHeaderHashShelley block
    Babbage -> \(Block block) -> HeaderHash $ getHeaderHashShelley block
    Conway -> \(Block block) -> HeaderHash $ getHeaderHashShelley block

getHeaderHashShelley
    :: ( Shelley.ProtocolHeaderSupportsEnvelope (praos StandardCrypto)
       , L.Era era
       , EncCBORGroup (TxSeq era)
       , EncCBOR (Shelley.ShelleyProtocolHeader (praos StandardCrypto))
       )
    => O.ShelleyBlock (praos StandardCrypto) era
    -> ShelleyHash
getHeaderHashShelley
    (O.ShelleyBlock (Shelley.Block header _) _) = Shelley.pHeaderHash header

-- | Raw hash digest for a block header.
type RawHeaderHash = Hash Blake2b_256 EraIndependentBlockHeader

-- | Construct a 'RawHeaderHash' that is good enough for testing.
mockRawHeaderHash :: Integer -> RawHeaderHash
mockRawHeaderHash n =
    Hash.hashWith
        (\_ -> B8.pack $ show n)
        (error "undefined :: EraIndependentBlockHeader")

{-# INLINEABLE getRawHeaderHash #-}
getRawHeaderHash
    :: forall era. IsEra era => HeaderHash era -> RawHeaderHash
getRawHeaderHash = case theEra @era of
    Byron -> \(HeaderHash h) -> fromByron h
    Shelley -> \(HeaderHash h) -> castHash $ unShelleyHash h
    Allegra -> \(HeaderHash h) -> castHash $ unShelleyHash h
    Mary -> \(HeaderHash h) -> castHash $ unShelleyHash h
    Alonzo -> \(HeaderHash h) -> castHash $ unShelleyHash h
    Babbage -> \(HeaderHash h) -> castHash $ unShelleyHash h
    Conway -> \(HeaderHash h) -> castHash $ unShelleyHash h
  where
    fromByron :: ByronHash -> Hash Blake2b_256 EraIndependentBlockHeader
    fromByron =
        fromJust
            . hashFromBytesShort
            . Byron.abstractHashToShort
            . unByronHash

{-----------------------------------------------------------------------------
    PrevHeaderHash
------------------------------------------------------------------------------}

-- | Era-specific previous header hash type from the ledger
type family PrevHeaderHashT era where
    PrevHeaderHashT Byron = O.ChainHash ByronBlock
    PrevHeaderHashT Shelley = PrevHash
    PrevHeaderHashT Allegra = PrevHash
    PrevHeaderHashT Mary = PrevHash
    PrevHeaderHashT Alonzo = PrevHash
    PrevHeaderHashT Babbage = PrevHash
    PrevHeaderHashT Conway = PrevHash

-- | Era-specific previous header hash type from the ledger
newtype PrevHeaderHash era = PrevHeaderHash (PrevHeaderHashT era)

getPrevHeaderHashShelley
    :: ( L.Era era
       , EncCBORGroup (TxSeq era)
       , EncCBOR (Shelley.ShelleyProtocolHeader proto)
       , Shelley.ProtocolHeaderSupportsEnvelope proto
       )
    => O.ShelleyBlock proto era
    -> PrevHash
getPrevHeaderHashShelley (O.ShelleyBlock (Shelley.Block header _) _) =
    Shelley.pHeaderPrevHash header

{-# INLINEABLE getEraPrevHeaderHash #-}
getEraPrevHeaderHash
    :: forall era. IsEra era => Block era -> PrevHeaderHash era
getEraPrevHeaderHash = case theEra @era of
    Byron -> \(Block block) -> PrevHeaderHash $ headerPrevHash $ O.getHeader block
    Shelley -> \(Block block) -> PrevHeaderHash $ getPrevHeaderHashShelley block
    Allegra -> \(Block block) -> PrevHeaderHash $ getPrevHeaderHashShelley block
    Mary -> \(Block block) -> PrevHeaderHash $ getPrevHeaderHashShelley block
    Alonzo -> \(Block block) -> PrevHeaderHash $ getPrevHeaderHashShelley block
    Babbage -> \(Block block) -> PrevHeaderHash $ getPrevHeaderHashShelley block
    Conway -> \(Block block) -> PrevHeaderHash $ getPrevHeaderHashShelley block
