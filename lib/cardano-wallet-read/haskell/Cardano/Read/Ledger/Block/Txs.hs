{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Cardano.Read.Ledger.Block.Txs
-- Copyright   : © 2024 Cardano Foundation
-- License     : Apache-2.0
--
-- Transaction extraction from blocks. This module provides the function
-- to extract the list of transactions contained within a block.
module Cardano.Read.Ledger.Block.Txs
    ( getEraTransactions
    ) where

import Prelude

import qualified Cardano.Chain.Block as Byron
import qualified Cardano.Chain.UTxO as Byron
import qualified Cardano.Ledger.Api as Ledger
import Cardano.Ledger.Binary
    ( EncCBOR
    )
import Cardano.Ledger.Core
    ( EraSegWits (..)
    )
import qualified Cardano.Ledger.Shelley.API as Shelley
import Cardano.Read.Ledger.Block.Block
    ( Block (..)
    )
import Cardano.Read.Ledger.Eras
    ( Byron
    , Era (..)
    , IsEra (..)
    )
import Cardano.Read.Ledger.Tx.Tx
    ( Tx (..)
    , TxT
    )
import Data.Foldable
    ( toList
    )
import qualified Ouroboros.Consensus.Byron.Ledger as Byron
import qualified Ouroboros.Consensus.Byron.Ledger as O
import qualified Ouroboros.Consensus.Shelley.Ledger as O
import Ouroboros.Consensus.Shelley.Protocol.Abstract
    ( ShelleyProtocolHeader
    )
import Ouroboros.Consensus.Shelley.Protocol.Praos
    ()
import Ouroboros.Consensus.Shelley.Protocol.TPraos
    ()

{-# INLINEABLE getEraTransactions #-}

-- | Get the list of transactions in the block.
getEraTransactions :: forall era. IsEra era => Block era -> [Tx era]
getEraTransactions = case theEra @era of
    Byron -> getTxs' getTxsFromBlockByron
    Shelley -> getTxs' getTxsFromBlockShelleyAndOn
    Allegra -> getTxs' getTxsFromBlockShelleyAndOn
    Mary -> getTxs' getTxsFromBlockShelleyAndOn
    Alonzo -> getTxs' getTxsFromBlockShelleyAndOn
    Babbage -> getTxs' getTxsFromBlockShelleyAndOn
    Conway -> getTxs' getTxsFromBlockShelleyAndOn
  where
    getTxs' f (Block block) = Tx <$> f block

getTxsFromBlockByron :: O.ByronBlock -> [TxT Byron]
getTxsFromBlockByron block =
    case Byron.byronBlockRaw block of
        Byron.ABOBBlock b -> Byron.unTxPayload . Byron.blockTxPayload $ b
        Byron.ABOBBoundary _ -> []

getTxsFromBlockShelleyAndOn
    :: (EraSegWits era, EncCBOR (ShelleyProtocolHeader proto))
    => O.ShelleyBlock proto era
    -> [Ledger.Tx era]
getTxsFromBlockShelleyAndOn (O.ShelleyBlock (Shelley.Block _ txs) _) =
    toList (fromTxSeq txs)
