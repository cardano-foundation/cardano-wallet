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
-- Raw extra signers required data extraction from 'Tx'
--

module Cardano.Wallet.Read.Tx.ExtraSigs where

import Prelude

import Cardano.Api
    ( AllegraEra, AlonzoEra, BabbageEra, ByronEra, MaryEra, ShelleyEra )
import Cardano.Ledger.Crypto
    ( StandardCrypto )
import Cardano.Ledger.Keys
    ( KeyHash, KeyRole (..) )
import Cardano.Wallet.Read.Eras
    ( EraFun (..) )
import Cardano.Wallet.Read.Tx
    ( Tx (..) )
import Cardano.Wallet.Read.Tx.Eras
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
    = EraFun
        { byronFun = \_ -> ExtraSigs ()
        , shelleyFun = \_ -> ExtraSigs ()
        , allegraFun = \_ -> ExtraSigs ()
        , maryFun = \_ -> ExtraSigs ()
        , alonzoFun = onTx $ \(AL.ValidatedTx b _ _ _)
                    -> ExtraSigs $ getField @"reqSignerHashes" b
        , babbageFun = onTx $ \(AL.ValidatedTx b _ _ _)
                    -> ExtraSigs $ getField @"reqSignerHashes" b
        }
