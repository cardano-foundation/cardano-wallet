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
It is compatible with the era-specific types from @cardano-ledger@.
-}
module Cardano.Wallet.Types.Read.Tx
    ( -- * Transactions
      Tx (..)
    , TxT
    ) where

import Prelude

import Cardano.Api
    ( AllegraEra, AlonzoEra, BabbageEra, ByronEra, MaryEra, ShelleyEra )

import qualified Cardano.Api.Shelley as Api
import qualified Cardano.Chain.UTxO as Byron
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import qualified Cardano.Ledger.Shelley.API as Shelley

-- | Closed type family returning the ledger 'Tx' type for each known @era@.
type family TxT era where
    TxT ByronEra = Byron.ATxAux ()
    TxT ShelleyEra = Shelley.Tx (Api.ShelleyLedgerEra ShelleyEra)
    TxT AllegraEra = Shelley.Tx (Api.ShelleyLedgerEra AllegraEra)
    TxT MaryEra = Shelley.Tx (Api.ShelleyLedgerEra MaryEra)
    TxT AlonzoEra = Alonzo.ValidatedTx (Api.ShelleyLedgerEra AlonzoEra)
    TxT BabbageEra = Alonzo.ValidatedTx (Api.ShelleyLedgerEra BabbageEra)

newtype Tx era = Tx {unTx :: TxT era}

deriving instance Show (TxT era) => Show (Tx era)
deriving instance Eq (TxT era) => Eq (Tx era)
