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

module Cardano.Wallet.Types.Read.Tx.Integrity where

import Prelude

import Cardano.Api
    ( AllegraEra, AlonzoEra, BabbageEra, ByronEra, MaryEra, ShelleyEra )
import Cardano.Ledger.Alonzo.Tx
    ( ScriptIntegrityHash )
import Cardano.Ledger.Crypto
    ( StandardCrypto )
import Cardano.Wallet.Types.Read.Eras
    ( EraFun, EraFunR (..), fromEraFunR )
import Cardano.Wallet.Types.Read.Tx
    ( Tx (..) )
import Cardano.Wallet.Types.Read.Tx.Eras
    ( onTx )
import Data.Maybe.Strict
    ( StrictMaybe )
import GHC.Records
    ( HasField (..) )

import qualified Cardano.Ledger.Alonzo.Tx as AL

type family IntegrityType era where
    IntegrityType ByronEra = ()
    IntegrityType ShelleyEra = ()
    IntegrityType AllegraEra = ()
    IntegrityType MaryEra = ()
    IntegrityType AlonzoEra = StrictMaybe (ScriptIntegrityHash StandardCrypto)
    IntegrityType BabbageEra = StrictMaybe (ScriptIntegrityHash StandardCrypto)

newtype Integrity era = Integrity (IntegrityType era)

deriving instance Show (IntegrityType era) => Show (Integrity era)
deriving instance Eq (IntegrityType era) => Eq (Integrity era)

getEraIntegrity :: EraFun Tx Integrity
getEraIntegrity
    = fromEraFunR $ EraFunR
        { byronFun = \_ -> Integrity ()
        , shelleyFun = \_ -> Integrity ()
        , allegraFun = \_ -> Integrity ()
        , maryFun = \_ -> Integrity ()
        , alonzoFun = onTx $ \(AL.ValidatedTx b _ _ _)
                -> Integrity do getField @"scriptIntegrityHash" b
        , babbageFun = onTx $ \(AL.ValidatedTx b _ _ _)
                -> Integrity do getField @"scriptIntegrityHash" b
        }
