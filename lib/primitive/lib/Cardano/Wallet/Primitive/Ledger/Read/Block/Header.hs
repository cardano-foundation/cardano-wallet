{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Wallet.Primitive.Ledger.Read.Block.Header
    ( getBlockHeader
    , primitiveBlockHeader
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
    , Era (..)
    , IsEra
    , fromConsensusBlock
    , theEra
    )
import Cardano.Wallet.Read.Block.BHeader
    ( getEraBHeader
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
import Cardano.Wallet.Read.Eras.EraFun
    ( applyEraFun
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
import Ouroboros.Consensus.Byron.Ledger.Block
    ( ByronBlock
    , ByronHash (..)
    )
import Ouroboros.Consensus.Shelley.Protocol.Abstract
    ( ShelleyHash (..)
    )

import qualified Cardano.Crypto.Hashing as CC
import qualified Cardano.Protocol.TPraos.BHeader as SL
import qualified Cardano.Wallet.Primitive.Types.Block as W
import qualified Cardano.Wallet.Primitive.Types.Hash as W
import qualified Ouroboros.Network.Block as O

fromByronHash :: ByronHash -> W.Hash tag
fromByronHash = W.Hash . CC.hashToBytes . unByronHash

-- | Get a wallet primitive block header from a ledger block
getBlockHeader :: W.Hash "Genesis" -> ConsensusBlock -> W.BlockHeader
getBlockHeader gp =
        applyEraFun (primitiveBlockHeader gp)
        . fromConsensusBlock

-- | Compute a wallet primitive block header from a ledger
primitiveBlockHeader
    :: IsEra era
    => W.Hash "Genesis"
    -> Block era
    -> W.BlockHeader
primitiveBlockHeader gp = do
    slotNo <- fromSlotNo <$> getEraSlotNo . getEraBHeader
    blockNo <- fromBlockNo <$> getEraBlockNo . getEraBHeader
    headerHash <- primitiveHash . getEraHeaderHash
    prevHeaderHash <- primitivePrevHash gp . getEraPrevHeaderHash
    pure $ W.BlockHeader slotNo blockNo headerHash (Just prevHeaderHash)

fromBlockNo :: Num a => BlockNo -> Quantity unit a
fromBlockNo (BlockNo h) = Quantity (fromIntegral h)

fromSlotNo :: SlotNo -> O.SlotNo
fromSlotNo (SlotNo s) = O.SlotNo $ fromIntegral s

{-# INLINEABLE primitiveHash #-}
primitiveHash :: forall era. IsEra era => HeaderHash era -> W.Hash "BlockHeader"
primitiveHash = case theEra @era of
    Byron -> \(HeaderHash h) -> fromByronHash h
    Shelley -> mkHashShelley
    Allegra -> mkHashShelley
    Mary -> mkHashShelley
    Alonzo -> mkHashShelley
    Babbage -> mkHashShelley
    Conway -> mkHashShelley
  where
    mkHashShelley
        :: HeaderHashT era ~ ShelleyHash crypto
        => HeaderHash era
        -> W.Hash "BlockHeader"
    mkHashShelley (HeaderHash (ShelleyHash h)) = W.Hash . hashToBytes $ h

{-# INLINABLE primitivePrevHash #-}
primitivePrevHash
    :: forall era
    . IsEra era
    => W.Hash "Genesis"
    -> PrevHeaderHash era
    -> W.Hash "BlockHeader"
primitivePrevHash gp = case theEra @era of
        Byron -> \(PrevHeaderHash h) -> fromChainHash h
        Shelley -> mkPrevHashShelley
        Allegra -> mkPrevHashShelley
        Mary -> mkPrevHashShelley
        Alonzo -> mkPrevHashShelley
        Babbage -> mkPrevHashShelley
        Conway -> mkPrevHashShelley

  where
    mkPrevHashShelley
        :: (SL.PrevHash StandardCrypto ~ PrevHeaderHashT era)
        => PrevHeaderHash era
        -> W.Hash "BlockHeader"
    mkPrevHashShelley (PrevHeaderHash h) = fromPrevHash h
    genesisHash = coerce gp :: W.Hash "BlockHeader"
    fromChainHash :: O.ChainHash ByronBlock -> W.Hash "BlockHeader"
    fromChainHash = \case
        O.GenesisHash -> genesisHash
        O.BlockHash h -> fromByronHash h
    fromPrevHash
        :: SL.PrevHash StandardCrypto
        -> W.Hash "BlockHeader"
    fromPrevHash = \case
        SL.GenesisHash -> genesisHash
        SL.BlockHash (SL.HashHeader h) -> W.Hash (hashToBytes h)
