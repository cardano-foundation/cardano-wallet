{-# LANGUAGE UndecidableInstances #-}

{- |
Copyright: © 2022 IOHK, 2024 Cardano Foundation
License: Apache-2.0

The 'Tx' type represents transactions as they are read from the mainnet ledger.
It is compatible with the era-specific index types from @cardano-ledger@.
-}
module Cardano.Read.Ledger.Tx.Tx
    ( -- * Transaction type
      Tx (..)
    , TxT
    ) where

import Prelude

import qualified Cardano.Chain.UTxO as Byron
import Cardano.Ledger.Alonzo.Tx
    ( AlonzoTx
    )
import Cardano.Ledger.Shelley.Tx
    ( ShelleyTx
    )
import Cardano.Read.Ledger.Eras
    ( Allegra
    , Alonzo
    , Babbage
    , Byron
    , Conway
    , Mary
    , Shelley
    )

-- |
-- Closed type family returning the ledger transaction type for each known era.
--
-- The transaction type differs between eras:
--
-- * Byron uses 'ATxAux'
-- * Shelley through Mary use 'ShelleyTx'
-- * Alonzo and later use 'AlonzoTx'
type family TxT era where
    TxT Byron = Byron.ATxAux ()
    TxT Shelley = ShelleyTx Shelley
    TxT Allegra = ShelleyTx Allegra
    TxT Mary = ShelleyTx Mary
    TxT Alonzo = AlonzoTx Alonzo
    TxT Babbage = AlonzoTx Babbage
    TxT Conway = AlonzoTx Conway

-- |
-- Era-indexed transaction wrapper.
--
-- Use the accessor functions from the @Cardano.Read.Ledger.Tx.*@ modules
-- to extract transaction components like inputs, outputs, fees, etc.
newtype Tx era = Tx {unTx :: TxT era}

deriving instance Show (TxT era) => Show (Tx era)
deriving instance Eq (TxT era) => Eq (Tx era)
