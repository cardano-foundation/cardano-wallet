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
    ( AlonzoEra, BabbageEra )
import Cardano.Ledger.Alonzo.Tx
    ( ScriptIntegrityHash )
import Cardano.Ledger.Crypto
    ( StandardCrypto )
import Cardano.Wallet.Types.Read.Tx.Eras
    ( UseTxAllEra, UseTxEra (..) )
import Data.Maybe.Strict
    ( StrictMaybe )
import Generics.SOP
    ( NP ((:*), Nil) )
import GHC.Records
    ( HasField (..) )

import qualified Cardano.Ledger.Alonzo.Tx as AL

type family IntegrityType era where
    IntegrityType AlonzoEra = StrictMaybe (ScriptIntegrityHash StandardCrypto)
    IntegrityType BabbageEra = StrictMaybe (ScriptIntegrityHash StandardCrypto)
    IntegrityType _ = ()

newtype Integrity era = Integrity (IntegrityType era)

deriving instance Show (IntegrityType era) => Show (Integrity era)
deriving instance Eq (IntegrityType era) => Eq (Integrity era)

getEraIntegrity :: UseTxAllEra Integrity
getEraIntegrity =
    UseTxEra (\_ -> Integrity ())
        :* UseTxEra (\_ -> Integrity ())
        :* UseTxEra (\_ -> Integrity ())
        :* UseTxEra (\_ -> Integrity ())
        :* UseTxEra (\(AL.ValidatedTx b _ _ _)
            -> Integrity $ getField @"scriptIntegrityHash" b)
        :* UseTxEra (\(AL.ValidatedTx b _ _ _)
            -> Integrity $ getField @"scriptIntegrityHash" b)
        :* Nil
