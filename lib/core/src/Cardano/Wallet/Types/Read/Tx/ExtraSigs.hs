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

module Cardano.Wallet.Types.Read.Tx.ExtraSigs where

import Prelude

import Cardano.Api
    ( AlonzoEra, BabbageEra )
import Cardano.Ledger.Crypto
    ( StandardCrypto )
import Cardano.Ledger.Keys
    ( KeyHash, KeyRole (..) )
import Cardano.Wallet.Types.Read.Tx.Eras
    ( UseTxAllEra, UseTxEra (..) )
import Data.Set
    ( Set )
import Generics.SOP
    ( NP ((:*), Nil) )
import GHC.Records
    ( HasField (..) )

import qualified Cardano.Ledger.Alonzo.Tx as AL

type family ExtraSigsType era where
    ExtraSigsType AlonzoEra = Set (KeyHash 'Witness StandardCrypto)
    ExtraSigsType BabbageEra = Set (KeyHash 'Witness StandardCrypto)
    ExtraSigsType _ = ()

newtype ExtraSigs era = ExtraSigs (ExtraSigsType era)

deriving instance Show (ExtraSigsType era) => Show (ExtraSigs era)
deriving instance Eq (ExtraSigsType era) => Eq (ExtraSigs era)

getEraExtraSigs :: UseTxAllEra ExtraSigs
getEraExtraSigs =
    UseTxEra (\_ -> ExtraSigs ())
    :* UseTxEra (\_ -> ExtraSigs ())
    :* UseTxEra (\_ -> ExtraSigs ())
    :* UseTxEra (\_ -> ExtraSigs ())
    :* UseTxEra (\(AL.ValidatedTx b _ _ _)
            -> ExtraSigs $ getField @"reqSignerHashes" b)
    :* UseTxEra (\(AL.ValidatedTx b _ _ _)
            -> ExtraSigs $ getField @"reqSignerHashes" b)
    :* Nil
