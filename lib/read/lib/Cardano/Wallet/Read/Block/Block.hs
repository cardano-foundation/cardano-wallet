{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright: © 2022 IOHK
-- License: Apache-2.0
--
-- The 'Block' type.
--
module Cardano.Wallet.Read.Block.Block
    ( ConsensusBlock
    , Block (..)
    , fromConsensusBlock
    , toConsensusBlock
    ) where

import Prelude

import Cardano.Ledger.Api
    ( StandardCrypto
    )
import Cardano.Wallet.Read.Eras
    ( Allegra
    , Alonzo
    , Babbage
    , Byron
    , Conway
    , EraValue (..)
    , IsEra
    , Mary
    , Shelley
    , eraValue
    )
import Cardano.Wallet.Read.Eras.KnownEras
    ( Era (..)
    , IsEra (..)
    )
import Ouroboros.Consensus.Protocol.Praos
    ( Praos
    )
import Ouroboros.Consensus.Protocol.TPraos
    ( TPraos
    )

import qualified Ouroboros.Consensus.Byron.Ledger as O
import qualified Ouroboros.Consensus.Cardano.Block as O
import qualified Ouroboros.Consensus.Shelley.Ledger as O

-- | Type synonym for 'CardanoBlock',
-- using the same cryptographic functionalities as Mainnet.
type ConsensusBlock = O.CardanoBlock O.StandardCrypto

-- Family of era-specific block types
type family BlockT era where
    BlockT Byron =
        O.ByronBlock
    BlockT Shelley =
        O.ShelleyBlock (TPraos StandardCrypto) (O.ShelleyEra StandardCrypto)
    BlockT Allegra =
        O.ShelleyBlock (TPraos StandardCrypto) (O.AllegraEra StandardCrypto)
    BlockT Mary =
        O.ShelleyBlock (TPraos StandardCrypto) (O.MaryEra StandardCrypto)
    BlockT Alonzo =
        O.ShelleyBlock (TPraos StandardCrypto) (O.AlonzoEra StandardCrypto)
    BlockT Babbage =
        O.ShelleyBlock (Praos StandardCrypto) (O.BabbageEra StandardCrypto)
    BlockT Conway =
        O.ShelleyBlock (Praos StandardCrypto) (O.ConwayEra StandardCrypto)

newtype Block era = Block {unBlock :: BlockT era}

deriving instance Show (BlockT era) => Show (Block era)
deriving instance Eq (BlockT era) => Eq (Block era)

-- | Convert block as received from cardano-node
-- via Haskell library of mini-protocol.
fromConsensusBlock :: ConsensusBlock -> EraValue Block
fromConsensusBlock = \case
    O.BlockByron b -> eraValue @Byron $ Block b
    O.BlockShelley block -> eraValue @Shelley $ Block block
    O.BlockAllegra block -> eraValue @Allegra $ Block block
    O.BlockMary block -> eraValue @Mary $ Block block
    O.BlockAlonzo block -> eraValue @Alonzo $ Block block
    O.BlockBabbage block -> eraValue @Babbage $ Block block
    O.BlockConway block -> eraValue @Conway $ Block block

{-# INLINABLE toConsensusBlock #-}
toConsensusBlock :: forall era . IsEra era => Block era -> ConsensusBlock
toConsensusBlock = case theEra @era of
    Byron -> O.BlockByron . unBlock
    Shelley -> O.BlockShelley . unBlock
    Allegra -> O.BlockAllegra . unBlock
    Mary -> O.BlockMary . unBlock
    Alonzo -> O.BlockAlonzo . unBlock
    Babbage -> O.BlockBabbage . unBlock
    Conway -> O.BlockConway . unBlock
