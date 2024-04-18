{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Wallet.Read.Block.HeaderHash
    ( HeaderHash (..)
    , HeaderHashT
    , getEraHeaderHash

    , BHeader
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
import Ouroboros.Consensus.Shelley.Protocol.Abstract
    ( ProtoCrypto
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
    HeaderHashT Shelley = ShelleyHash StandardCrypto
    HeaderHashT Allegra = ShelleyHash StandardCrypto
    HeaderHashT Mary = ShelleyHash StandardCrypto
    HeaderHashT Alonzo = ShelleyHash StandardCrypto
    HeaderHashT Babbage = ShelleyHash StandardCrypto
    HeaderHashT Conway = ShelleyHash StandardCrypto

-- | Era-specific header hash type from the ledger
newtype HeaderHash era = HeaderHash (HeaderHashT era)

{-# INLINABLE getEraHeaderHash #-}
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

-- | Tag representing a block header.
data BHeader

-- | Raw hash digest for a block header.
type RawHeaderHash = Hash Blake2b_256 BHeader

-- | Construct a 'RawHeaderHash' that is good enough for testing.
mockRawHeaderHash :: Integer -> RawHeaderHash
mockRawHeaderHash n =
    Hash.hashWith (\_ -> B8.pack $ show n) (error "undefined :: BHeader")

{-# INLINABLE getRawHeaderHash #-}
getRawHeaderHash :: forall era. IsEra era => HeaderHash era -> RawHeaderHash
getRawHeaderHash = case theEra @era of
    Byron -> \(HeaderHash h) -> fromByron h
    Shelley -> \(HeaderHash h) -> castHash $ unShelleyHash h
    Allegra -> \(HeaderHash h) -> castHash $ unShelleyHash h
    Mary -> \(HeaderHash h) -> castHash $ unShelleyHash h
    Alonzo -> \(HeaderHash h) -> castHash $ unShelleyHash h
    Babbage -> \(HeaderHash h) -> castHash $ unShelleyHash h
    Conway -> \(HeaderHash h) -> castHash $ unShelleyHash h
  where
    fromByron :: ByronHash -> Hash Blake2b_256 BHeader
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

{-# INLINABLE getEraPrevHeaderHash #-}
getEraPrevHeaderHash :: forall era . IsEra era => Block era -> PrevHeaderHash era
getEraPrevHeaderHash = case theEra @era of
    Byron -> \(Block block) -> PrevHeaderHash $ headerPrevHash $ O.getHeader block
    Shelley -> \(Block block) -> PrevHeaderHash $ getPrevHeaderHashShelley block
    Allegra -> \(Block block) -> PrevHeaderHash $ getPrevHeaderHashShelley block
    Mary -> \(Block block) -> PrevHeaderHash $ getPrevHeaderHashShelley block
    Alonzo -> \(Block block) -> PrevHeaderHash $ getPrevHeaderHashShelley block
    Babbage -> \(Block block) -> PrevHeaderHash $ getPrevHeaderHashShelley block
    Conway -> \(Block block) -> PrevHeaderHash $ getPrevHeaderHashShelley block
