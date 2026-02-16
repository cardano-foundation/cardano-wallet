{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright: © 2022 IOHK, 2024 Cardano Foundation
-- License: Apache-2.0
--
-- The 'Tx' type represents transactions as they are read from the mainnet ledger.
-- It is compatible with the era-specific index types from @cardano-ledger@.
module Cardano.Read.Ledger.Tx.Tx
    ( -- * Transaction type
      Tx (..)
    , TxT
    ) where

import Cardano.Read.Ledger.Eras
    ( Allegra
    , Alonzo
    , Babbage
    , Byron
    , Conway
    , Dijkstra
    , Mary
    , Shelley
    )
import Prelude

import Cardano.Chain.UTxO qualified as Byron
import Cardano.Ledger.Core qualified as Core

-- |
-- Closed type family returning the ledger transaction type for each known era.
--
-- The transaction type differs between eras:
--
-- * Byron uses 'ATxAux'
-- * Shelley and later use 'Core.Tx' from cardano-ledger-core
type family TxT era where
    TxT Byron = Byron.ATxAux ()
    TxT Shelley = Core.Tx Shelley
    TxT Allegra = Core.Tx Allegra
    TxT Mary = Core.Tx Mary
    TxT Alonzo = Core.Tx Alonzo
    TxT Babbage = Core.Tx Babbage
    TxT Conway = Core.Tx Conway
    TxT Dijkstra = Core.Tx Dijkstra

-- |
-- Era-indexed transaction wrapper.
--
-- Use the accessor functions from the @Cardano.Read.Ledger.Tx.*@ modules
-- to extract transaction components like inputs, outputs, fees, etc.
newtype Tx era = Tx {unTx :: TxT era}

deriving instance Show (TxT era) => Show (Tx era)
deriving instance Eq (TxT era) => Eq (Tx era)
