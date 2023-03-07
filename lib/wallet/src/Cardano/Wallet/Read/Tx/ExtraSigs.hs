{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
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
    ( AllegraEra, AlonzoEra, BabbageEra, ByronEra, ConwayEra, MaryEra, ShelleyEra )
import Cardano.Ledger.Alonzo.TxBody
    ( reqSignerHashesTxBodyL )
import Cardano.Ledger.Core
    ( bodyTxL )
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
import Control.Lens
    ( (^.) )
import Data.Set
    ( Set )

type family ExtraSigsType era where
    ExtraSigsType ByronEra = ()
    ExtraSigsType ShelleyEra = ()
    ExtraSigsType AllegraEra = ()
    ExtraSigsType MaryEra = ()
    ExtraSigsType AlonzoEra = Set (KeyHash 'Witness StandardCrypto)
    ExtraSigsType BabbageEra = Set (KeyHash 'Witness StandardCrypto)
    ExtraSigsType ConwayEra = Set (KeyHash 'Witness StandardCrypto)

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
        , alonzoFun = onTx $ \tx -> ExtraSigs
            $ tx ^. bodyTxL . reqSignerHashesTxBodyL
        , babbageFun = onTx $ \tx -> ExtraSigs
            $ tx ^. bodyTxL . reqSignerHashesTxBodyL
        , conwayFun = onTx $ \tx -> ExtraSigs
            $ tx ^. bodyTxL . reqSignerHashesTxBodyL
        }
