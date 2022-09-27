{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright: © 2020-2022 IOHK
-- License: Apache-2.0
--
-- Raw witnesses data extraction from 'Tx'
--

module Cardano.Wallet.Read.Tx.Witnesses
    ( WitnessesType
    , Witnesses (..)
    , getEraWitnesses
    ) where

import Prelude

import Cardano.Api
    ( AllegraEra, AlonzoEra, BabbageEra, ByronEra, MaryEra, ShelleyEra )
import Cardano.Ledger.Crypto
    ( StandardCrypto )
import Cardano.Ledger.Shelley.Tx
    ( WitnessSetHKD )
import Cardano.Ledger.ShelleyMA
    ( MaryOrAllegra (..), ShelleyMAEra )
import Cardano.Wallet.Read.Eras
    ( EraFun (..) )
import Cardano.Wallet.Read.Tx
    ( Tx (..) )
import Cardano.Wallet.Read.Tx.Eras
    ( onTx )
import Data.Functor.Identity
    ( Identity )
import Ouroboros.Consensus.Shelley.Eras
    ( StandardAlonzo, StandardBabbage )

import qualified Cardano.Ledger.Alonzo.Tx as AL
import qualified Cardano.Ledger.Alonzo.TxWitness as AL
import qualified Cardano.Ledger.Shelley.API as SH

type family WitnessesType era where
  WitnessesType ByronEra = ()
  WitnessesType ShelleyEra = ()
  WitnessesType AllegraEra = ()
  WitnessesType MaryEra = WitnessSetHKD Identity
                         (ShelleyMAEra 'Mary StandardCrypto)
  WitnessesType AlonzoEra = AL.TxWitness StandardAlonzo
  WitnessesType BabbageEra = AL.TxWitness StandardBabbage

newtype Witnesses era = Witnesses (WitnessesType era)

deriving instance Show (WitnessesType era) => Show (Witnesses era)
deriving instance Eq (WitnessesType era) => Eq (Witnesses era)

getEraWitnesses :: EraFun Tx Witnesses
getEraWitnesses = EraFun
    { byronFun = \_ -> Witnesses ()
    , shelleyFun = \_ -> Witnesses ()
    , allegraFun = \_  -> Witnesses ()
    , maryFun = onTx $ \(SH.Tx _ wits _ ) -> Witnesses wits
    , alonzoFun = onTx $ \(AL.ValidatedTx _ wits _ _) -> Witnesses wits
    , babbageFun = onTx $ \(AL.ValidatedTx _ wits _ _) -> Witnesses wits
    }

