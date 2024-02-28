{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Wallet.Read.Block.Txs
    ( getEraTransactions
    ) where

import Prelude

import Cardano.Ledger.Binary
    ( EncCBOR
    )
import Cardano.Wallet.Read.Block
    ( Block (..)
    )
import Cardano.Wallet.Read.Eras
    ( Byron
    , EraFun (..)
    , (:.:) (..)
    )
import Cardano.Wallet.Read.Tx
    ( Tx (..)
    , TxT
    )
import Data.Foldable
    ( toList
    )
import Ouroboros.Consensus.Shelley.Protocol.Abstract
    ( ShelleyProtocolHeader
    )
import Ouroboros.Consensus.Shelley.Protocol.Praos
    ()
import Ouroboros.Consensus.Shelley.Protocol.TPraos
    ()

import qualified Cardano.Chain.Block as Byron
import qualified Cardano.Chain.UTxO as Byron
import qualified Cardano.Ledger.Api as Ledger
import qualified Cardano.Ledger.Era as Shelley
import qualified Cardano.Ledger.Shelley.API as Shelley
import qualified Ouroboros.Consensus.Byron.Ledger as Byron
import qualified Ouroboros.Consensus.Byron.Ledger as O
import qualified Ouroboros.Consensus.Shelley.Ledger as O

-- | Get the list of transactions in the block.
getEraTransactions :: EraFun Block ([] :.: Tx)
getEraTransactions =
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

getTxsFromBlockByron :: O.ByronBlock -> [TxT Byron]
getTxsFromBlockByron block =
    case Byron.byronBlockRaw block of
        Byron.ABOBBlock b -> Byron.unTxPayload . Byron.blockTxPayload $ b
        Byron.ABOBBoundary _ -> []

getTxsFromBlockShelleyAndOn
    :: (Shelley.EraSegWits era, EncCBOR (ShelleyProtocolHeader proto))
    => O.ShelleyBlock proto era
    -> [Ledger.Tx era]
getTxsFromBlockShelleyAndOn (O.ShelleyBlock (Shelley.Block _ txs) _) =
    toList (Shelley.fromTxSeq txs)
