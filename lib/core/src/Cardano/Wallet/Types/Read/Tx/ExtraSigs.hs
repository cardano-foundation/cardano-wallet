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
    ( AllegraEra, AlonzoEra, BabbageEra, ByronEra, MaryEra, ShelleyEra )
import Cardano.Ledger.Crypto
    ( StandardCrypto )
import Cardano.Ledger.Keys
    ( KeyHash, KeyRole (..) )
import Cardano.Wallet.Types.Read.Eras
    ( EraFun, EraFunR (..), fromEraFunR )
import Cardano.Wallet.Types.Read.Tx
    ( Tx (..) )
import Cardano.Wallet.Types.Read.Tx.Eras
    ( onTx )
import Data.Set
    ( Set )
import GHC.Records
    ( HasField (..) )

import qualified Cardano.Ledger.Alonzo.Tx as AL

type family ExtraSigsType era where
    ExtraSigsType ByronEra = ()
    ExtraSigsType ShelleyEra = ()
    ExtraSigsType AllegraEra = ()
    ExtraSigsType MaryEra = ()
    ExtraSigsType AlonzoEra = Set (KeyHash 'Witness StandardCrypto)
    ExtraSigsType BabbageEra = Set (KeyHash 'Witness StandardCrypto)

newtype ExtraSigs era = ExtraSigs (ExtraSigsType era)

deriving instance Show (ExtraSigsType era) => Show (ExtraSigs era)
deriving instance Eq (ExtraSigsType era) => Eq (ExtraSigs era)

getEraExtraSigs :: EraFun Tx ExtraSigs
getEraExtraSigs
    = fromEraFunR $ EraFunR
        { byronFun = \_ -> ExtraSigs ()
        , shelleyFun = \_ -> ExtraSigs ()
        , allegraFun = \_ -> ExtraSigs ()
        , maryFun = \_ -> ExtraSigs ()
        , alonzoFun = onTx $ \(AL.ValidatedTx b _ _ _)
                    -> ExtraSigs do getField @"reqSignerHashes" b
        , babbageFun = onTx $ \(AL.ValidatedTx b _ _ _)
                    -> ExtraSigs do getField @"reqSignerHashes" b
        }
