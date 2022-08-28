{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

{- |
Copyright: © 2022 IOHK
License: Apache-2.0

The 'Block' type represents blocks as they are read from the mainnet ledger.
It is compatible with the era-specific types from @cardano-ledger@.
-}
module Cardano.Wallet.Read.Block
    ( Block
    , getTxsFromBlock
    , getTxsListFromBlock
    ) where

import Prelude

import Cardano.Api
    ( CardanoEra (..) )
import Data.Foldable
    ( toList )
import Data.Sequence.Strict
    ( StrictSeq )

import qualified Ouroboros.Consensus.Cardano.Block as O
import qualified Ouroboros.Consensus.Shelley.Ledger.Block as O

import qualified Cardano.Chain.Block as Byron
import qualified Cardano.Chain.UTxO as Byron
import qualified Ouroboros.Consensus.Byron.Ledger.Block as Byron

import qualified Cardano.Ledger.Era as Shelley
import qualified Cardano.Ledger.Shelley.API as Shelley

import qualified Cardano.Ledger.Alonzo.TxSeq as Alonzo

import qualified Cardano.Wallet.Read.Tx as Read

import qualified Data.Sequence.Strict as Seq

{-------------------------------------------------------------------------------
    Block type
-------------------------------------------------------------------------------}
-- | Type synonym for 'CardanoBlock' with cryptography as used on mainnet.
type Block = O.CardanoBlock O.StandardCrypto

{-------------------------------------------------------------------------------
    Block transactions
-------------------------------------------------------------------------------}
-- | Retrieve the sequence of 'Tx' contained in a block, as a list.
getTxsListFromBlock :: Block -> [Read.Tx]
getTxsListFromBlock = toList . getTxsFromBlock

-- | Retrieve the sequence of 'Tx' contained in a block, as a 'StrictSeq'.
getTxsFromBlock :: Block -> StrictSeq Read.Tx
getTxsFromBlock = \case
    -- See Note [SeeminglyRedundantPatternMatches]

    O.BlockByron block -> getTxsFromBlockByron block

    O.BlockShelley block -> case block of
        O.ShelleyBlock (Shelley.Block _ txs) _ ->
            fmap (Read.Tx ShelleyEra) (Shelley.fromTxSeq txs)

    O.BlockAllegra block -> case block of
        O.ShelleyBlock (Shelley.Block _ txs) _ ->
            fmap (Read.Tx AllegraEra) (Shelley.fromTxSeq txs)

    O.BlockMary block -> case block of
        O.ShelleyBlock (Shelley.Block _ txs) _ ->
            fmap (Read.Tx MaryEra) (Shelley.fromTxSeq txs)

    O.BlockAlonzo block -> case block of
        O.ShelleyBlock (Shelley.Block _ (Alonzo.TxSeq txs)) _ ->
            fmap (Read.Tx AlonzoEra) txs

    O.BlockBabbage block -> case block of
        O.ShelleyBlock (Shelley.Block _ (Alonzo.TxSeq txs)) _ ->
            fmap (Read.Tx BabbageEra) txs

getTxsFromBlockByron :: Byron.ByronBlock -> StrictSeq Read.Tx
getTxsFromBlockByron block = Seq.fromList $
    case Byron.byronBlockRaw block of
        Byron.ABOBBlock b ->
            map mkTx . Byron.unTxPayload . Byron.blockTxPayload $ b
        Byron.ABOBBoundary _ -> []
  where
    mkTx = Read.Tx ByronEra . (() <$)
