{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright: © 2020-2022 IOHK
-- License: Apache-2.0
--
-- Raw validity interval data extraction from 'Tx'
--

module Cardano.Wallet.Read.Tx.Validity
    (getEraValidity, ValidityType, Validity (..))
    where

import Prelude

import Cardano.Api
    ( AllegraEra, AlonzoEra, BabbageEra, ByronEra, MaryEra, ShelleyEra )
import Cardano.Ledger.ShelleyMA.Timelocks
    ( ValidityInterval )
import Cardano.Ledger.Slot
    ( SlotNo )
import Cardano.Wallet.Read.Eras
    ( EraFun (..) )
import Cardano.Wallet.Read.Tx
    ( Tx (..) )
import Cardano.Wallet.Read.Tx.Eras
    ( onTx )
import GHC.Records
    ( HasField (..) )

import qualified Cardano.Ledger.Alonzo.Tx as AL
import qualified Cardano.Ledger.Shelley.API as SH

type family ValidityType era where
    ValidityType ByronEra = ()
    ValidityType ShelleyEra = SlotNo
    ValidityType AllegraEra = ValidityInterval
    ValidityType MaryEra = ValidityInterval
    ValidityType AlonzoEra = ValidityInterval
    ValidityType BabbageEra = ValidityInterval

newtype Validity era = Validity (ValidityType era)

deriving instance Show (ValidityType era) => Show (Validity era)
deriving instance Eq (ValidityType era) => Eq (Validity era)

getEraValidity :: EraFun Tx Validity
getEraValidity
    = EraFun
        { byronFun = \_ -> Validity ()
        , shelleyFun = onTx $ \(SH.Tx b _ _) -> Validity . getField @"ttl" $ b
        , allegraFun = onTx $ \(SH.Tx b _ _) -> getValidity b
        , maryFun = onTx $ \(SH.Tx b _ _) -> getValidity b
        , alonzoFun = onTx $ \(AL.ValidatedTx b _ _ _) -> getValidity b
        , babbageFun = onTx $ \(AL.ValidatedTx b _ _ _) -> getValidity b
        }

getValidity :: HasField "vldt" a (ValidityType era) => a -> Validity era
getValidity = Validity . getField @"vldt"
