{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Copyright: © 2022 IOHK
-- License: Apache-2.0
--
-- The 'Block' type represents blocks as they are read from the mainnet ledger.
-- It is compatible with the era-specific types from @cardano-ledger@.
module Cardano.Wallet.Read.Block
    ( ConsensusBlock
    , Block (..)
    , fromConsensusBlock
    , getTxs
    ) where

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
import Cardano.Ledger.Api
    ( StandardCrypto
    )
import Cardano.Ledger.Binary
    ( EncCBOR
    )
import Cardano.Wallet.Read.Eras
    ( (:.:) (..)
    , EraFun (..)
    , EraValue
    , allegra
    , alonzo
    , applyEraFun
    , babbage
    , byron
    , conway
    , inject
    , mary
    , sequenceEraValue
    , shelley
    )
import Cardano.Wallet.Read.Tx
    ( Tx (..)
    , TxT
    )
import Data.Foldable
    ( toList
    )
import Ouroboros.Consensus.Protocol.Praos
    ( Praos
    )
import Ouroboros.Consensus.Protocol.TPraos
    ( TPraos
    )
import Ouroboros.Consensus.Shelley.Protocol.Abstract
    ( ShelleyProtocolHeader
    )

import qualified Cardano.Chain.Block as Byron
import qualified Cardano.Chain.UTxO as Byron
import qualified Cardano.Ledger.Api as Ledger
import qualified Cardano.Ledger.Era as Shelley
import qualified Cardano.Ledger.Shelley.API as Shelley
import qualified Ouroboros.Consensus.Byron.Ledger as Byron
import qualified Ouroboros.Consensus.Byron.Ledger as O
import qualified Ouroboros.Consensus.Cardano.Block as O
import qualified Ouroboros.Consensus.Shelley.Ledger as O

{-------------------------------------------------------------------------------
    Block type
-------------------------------------------------------------------------------}
-- | Type synonym for 'CardanoBlock' with cryptography as used on mainnet.
type ConsensusBlock = O.CardanoBlock O.StandardCrypto

-- Family of era-specific block types
type family BlockT era where
    BlockT ByronEra = O.ByronBlock
    BlockT ShelleyEra =
        O.ShelleyBlock (TPraos StandardCrypto) (O.ShelleyEra StandardCrypto)
    BlockT AllegraEra =
        O.ShelleyBlock (TPraos StandardCrypto) (O.AllegraEra StandardCrypto)
    BlockT MaryEra =
        O.ShelleyBlock (TPraos StandardCrypto) (O.MaryEra StandardCrypto)
    BlockT AlonzoEra =
        O.ShelleyBlock (TPraos StandardCrypto) (O.AlonzoEra StandardCrypto)
    BlockT BabbageEra =
        O.ShelleyBlock (Praos StandardCrypto) (O.BabbageEra StandardCrypto)
    BlockT ConwayEra =
        O.ShelleyBlock (Praos StandardCrypto) (O.ConwayEra StandardCrypto)

newtype Block era = Block {unBlock :: BlockT era}

-- | Get sequence of transactions in the block.
txsFromBlockE :: EraFun Block ([] :.: Tx)
txsFromBlockE =
    EraFun
        { byronFun = getTxs' getTxsFromBlockByron
        , shelleyFun = getTxs' getTxsFromBlockShelleyAndOn
        , maryFun = getTxs' getTxsFromBlockShelleyAndOn
        , allegraFun = getTxs' getTxsFromBlockShelleyAndOn
        , alonzoFun = getTxs' getTxsFromBlockShelleyAndOn
        , babbageFun = getTxs' getTxsFromBlockShelleyAndOn
        , conwayFun = getTxs' getTxsFromBlockShelleyAndOn
        }
  where
    getTxs' f (Block block) = Comp $ Tx <$> f block

getTxsFromBlockByron :: O.ByronBlock -> [TxT ByronEra]
getTxsFromBlockByron block =
    case Byron.byronBlockRaw block of
        Byron.ABOBBlock b ->
            map (() <$) . Byron.unTxPayload . Byron.blockTxPayload $ b
        Byron.ABOBBoundary _ -> []

getTxsFromBlockShelleyAndOn
    :: (Shelley.EraSegWits era, EncCBOR (ShelleyProtocolHeader proto))
    => O.ShelleyBlock proto era
    -> [Ledger.Tx era]
getTxsFromBlockShelleyAndOn (O.ShelleyBlock (Shelley.Block _ txs) _) =
    toList (Shelley.fromTxSeq txs)

-- | Convert block as received from cardano-node
-- via Haskell library of mini-protocol.
fromConsensusBlock :: ConsensusBlock -> EraValue Block
fromConsensusBlock = \case
    O.BlockByron b -> inject byron $ Block b
    O.BlockShelley block -> inject shelley $ Block block
    O.BlockAllegra block -> inject allegra $ Block block
    O.BlockMary block -> inject mary $ Block block
    O.BlockAlonzo block -> inject alonzo $ Block block
    O.BlockBabbage block -> inject babbage $ Block block
    O.BlockConway block -> inject conway $ Block block

getTxs :: O.CardanoBlock StandardCrypto -> [EraValue Tx]
getTxs = sequenceEraValue . applyEraFun txsFromBlockE . fromConsensusBlock
