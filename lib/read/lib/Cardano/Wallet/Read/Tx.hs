{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Copyright: © 2022 IOHK
License: Apache-2.0

The 'Tx' type represents transactions as they are read from the mainnet ledger.
It is compatible with the era-specific index types from @cardano-ledger@.
-}
module Cardano.Wallet.Read.Tx
    ( -- * Transactions
      Tx (..)
    , TxT
    ) where

import Prelude

import Cardano.Ledger.Alonzo.Tx
    ( AlonzoTx
    )
import Cardano.Ledger.Api
    ( StandardCrypto
    )
import Cardano.Ledger.Shelley.Tx
    ( ShelleyTx
    )
import Cardano.Wallet.Read.Eras
    ( Allegra
    , Alonzo
    , Babbage
    , Byron
    , Conway
    , Mary
    , Shelley
    )

import qualified Cardano.Chain.UTxO as Byron
import qualified Cardano.Ledger.Api as Ledger

-- | Closed type family returning the ledger 'Tx' type for each known @era@.
type family TxT era where
    TxT Byron = Byron.ATxAux ()
    TxT Shelley = ShelleyTx (Ledger.ShelleyEra StandardCrypto)
    TxT Allegra = ShelleyTx (Ledger.AllegraEra StandardCrypto)
    TxT Mary = ShelleyTx (Ledger.MaryEra StandardCrypto)
    TxT Alonzo = AlonzoTx (Ledger.AlonzoEra  StandardCrypto)
    TxT Babbage = AlonzoTx (Ledger.BabbageEra StandardCrypto)
    TxT Conway = AlonzoTx (Ledger.ConwayEra StandardCrypto)

-- | A tx in any era
newtype Tx era = Tx {unTx :: TxT era}

deriving instance Show (TxT era) => Show (Tx era)
deriving instance Eq (TxT era) => Eq (Tx era)
