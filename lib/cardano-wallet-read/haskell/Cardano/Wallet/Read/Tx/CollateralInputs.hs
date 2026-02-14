{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Copyright: © 2020-2024 IOHK
-- License: Apache-2.0
module Cardano.Wallet.Read.Tx.CollateralInputs
    ( getCollateralInputs
    )
where

import Cardano.Wallet.Read.Eras
    ( Era (..)
    , IsEra (..)
    )
import Cardano.Wallet.Read.Tx.Tx
    ( Tx
    )
import Cardano.Wallet.Read.Tx.TxIn
    ( TxIn
    )
import Data.Set
    ( Set
    )
import Prelude

import Cardano.Read.Ledger.Tx.CollateralInputs qualified as L
import Data.Set qualified as Set

{-# INLINEABLE getCollateralInputs #-}

-- | Extract the collateral inputs from a transaction in any era.
getCollateralInputs :: forall era. IsEra era => Tx era -> Set TxIn
getCollateralInputs = case theEra :: Era era of
    Byron -> const Set.empty
    Shelley -> const Set.empty
    Allegra -> const Set.empty
    Mary -> const Set.empty
    Alonzo -> unCollateralInputs . L.getEraCollateralInputs
    Babbage -> unCollateralInputs . L.getEraCollateralInputs
    Conway -> unCollateralInputs . L.getEraCollateralInputs
    Dijkstra -> unCollateralInputs . L.getEraCollateralInputs

unCollateralInputs
    :: L.CollateralInputs era -> L.CollateralInputsType era
unCollateralInputs (L.CollateralInputs x) = x
