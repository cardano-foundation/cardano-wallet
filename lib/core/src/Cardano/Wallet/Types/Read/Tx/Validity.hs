{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Wallet.Types.Read.Tx.Validity where

import Prelude

import Cardano.Api
    ( AlonzoEra, BabbageEra, MaryEra )
import Cardano.Ledger.ShelleyMA.Timelocks
    ( ValidityInterval )
import Cardano.Wallet.Types.Read.Tx.Eras
    ( UseTxAllEra, UseTxEra (..) )
import Generics.SOP
    ( NP ((:*), Nil) )
import GHC.Records
    ( HasField (..) )

import qualified Cardano.Ledger.Alonzo.Tx as AL
import qualified Cardano.Ledger.Shelley.API as SH

type family ValidityType era where
    ValidityType MaryEra = ValidityInterval
    ValidityType AlonzoEra = ValidityInterval
    ValidityType BabbageEra = ValidityInterval
    ValidityType _ = ()

newtype Validity era = Validity (ValidityType era)

deriving instance Show (ValidityType era) => Show (Validity era)
deriving instance Eq (ValidityType era) => Eq (Validity era)

getEraValidity :: UseTxAllEra Validity
getEraValidity =
    UseTxEra (\_ -> Validity ())
    :* UseTxEra (\_ -> Validity ())
    :* UseTxEra (\_ -> Validity ())
    :* UseTxEra (\(SH.Tx b _ _) -> Validity $ getField @"vldt" b)
    :* UseTxEra (\(AL.ValidatedTx b _ _ _) -> Validity $ getField @"vldt" b)
    :* UseTxEra (\(AL.ValidatedTx b _ _ _) -> Validity $ getField @"vldt" b)
    :* Nil
