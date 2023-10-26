{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.Wallet.Read.Primitive.Block.Header
    ( getBlockHeader
    )
where

import Prelude hiding
    ( (.)
    )

import Cardano.Crypto.Hash.Class
    ( hashToBytes
    )
import Cardano.Ledger.Crypto
    ( StandardCrypto
    )
import Cardano.Wallet.Read
    ( Block
    , ConsensusBlock
    , fromConsensusBlock
    )
import Cardano.Wallet.Read.Block.BlockNo
    ( BlockNo (..)
    , getEraBlockNo
    )
import Cardano.Wallet.Read.Block.HeaderHash
    ( HeaderHash (..)
    , HeaderHashT
    , PrevHeaderHash (..)
    , PrevHeaderHashT
    , getEraHeaderHash
    , getEraPrevHeaderHash
    )
import Cardano.Wallet.Read.Block.SlotNo
    ( SlotNo (..)
    , getEraSlotNo
    )
import Cardano.Wallet.Read.Eras
    ( EraFun
    , applyEraFun
    , extractEraValue
    )
import Cardano.Wallet.Read.Eras.EraFun
    ( EraFun (..)
    , EraFunK (..)
    )
import Control.Category
    ( (.)
    )
import Data.Coerce
    ( coerce
    )
import Data.Quantity
    ( Quantity (..)
    )
import Data.Word
    ( Word32
    )
import Generics.SOP
    ( K (..)
    )
import Ouroboros.Consensus.Byron.Ledger.Block
    ( ByronHash (..)
    )
import Ouroboros.Consensus.Shelley.Protocol.Abstract
    ( ShelleyHash (..)
    )

import qualified Cardano.Crypto.Hashing as CC
import qualified Cardano.Protocol.TPraos.BHeader as SL
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Cardano.Wallet.Primitive.Types.Hash as W
import qualified Ouroboros.Network.Block as O

fromByronHash :: ByronHash -> W.Hash tag
fromByronHash = W.Hash . CC.hashToBytes . unByronHash

getBlockHeader :: W.Hash "Genesis" -> ConsensusBlock -> W.BlockHeader
getBlockHeader gp =
    extractEraValue
        . applyEraFun (fromEraFunK $ getBlockHeaderEra gp)
        . fromConsensusBlock

getBlockHeaderEra :: W.Hash "Genesis" -> EraFunK Block W.BlockHeader
getBlockHeaderEra gp =
    W.BlockHeader
        <$> (O.SlotNo . fromIntegral . unSlotNo <$> EraFunK getEraSlotNo)
        <*> getEraBlockNoK
        <*> EraFunK (primitiveHash . getEraHeaderHash)
        <*> (Just <$> EraFunK (primitivePrevHash gp . getEraPrevHeaderHash))

getEraBlockNoK :: EraFunK Block (Quantity "block" Word32)
getEraBlockNoK = fromBlockNo <$> EraFunK getEraBlockNo
  where
    fromBlockNo (BlockNo h) = Quantity (fromIntegral h)

primitiveHash :: EraFun HeaderHash (K (W.Hash "BlockHeader"))
primitiveHash =
    EraFun
        { byronFun = \(HeaderHash h) -> K . fromByronHash $ h
        , shelleyFun = mkHashShelley
        , allegraFun = mkHashShelley
        , maryFun = mkHashShelley
        , alonzoFun = mkHashShelley
        , babbageFun = mkHashShelley
        , conwayFun = mkHashShelley
        }
  where
    mkHashShelley
        :: HeaderHashT era ~ ShelleyHash crypto
        => HeaderHash era
        -> K (W.Hash "BlockHeader") era
    mkHashShelley (HeaderHash (ShelleyHash h)) = K . W.Hash . hashToBytes $ h

primitivePrevHash
    :: W.Hash "Genesis"
    -> EraFun PrevHeaderHash (K (W.Hash "BlockHeader"))
primitivePrevHash gp =
    EraFun
        { byronFun = \(PrevHeaderHash h) -> K . fromChainHash $ h
        , shelleyFun = mkPrevHashShelley
        , allegraFun = mkPrevHashShelley
        , maryFun = mkPrevHashShelley
        , alonzoFun = mkPrevHashShelley
        , babbageFun = mkPrevHashShelley
        , conwayFun = mkPrevHashShelley
        }
  where
    mkPrevHashShelley
        :: (SL.PrevHash StandardCrypto ~ PrevHeaderHashT era)
        => PrevHeaderHash era
        -> K (W.Hash "BlockHeader") era
    mkPrevHashShelley (PrevHeaderHash h) = K . fromPrevHash $ h
    genesisHash = coerce gp :: W.Hash "BlockHeader"
    fromChainHash = \case
        O.GenesisHash -> genesisHash
        O.BlockHash h -> fromByronHash h
    fromPrevHash
        :: SL.PrevHash StandardCrypto
        -> W.Hash "BlockHeader"
    fromPrevHash = \case
        SL.GenesisHash -> genesisHash
        SL.BlockHash (SL.HashHeader h) -> W.Hash (hashToBytes h)
