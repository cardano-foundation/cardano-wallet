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
-- Raw era-dependent tx outputs data extraction from 'Tx'
--

module Cardano.Wallet.Read.Tx.Outputs
    ( OutputsType
    , Outputs (..)
    , getEraOutputs
    )
    where

import Prelude

import Cardano.Api
    ( AllegraEra, AlonzoEra, BabbageEra, ByronEra, MaryEra, ShelleyEra )
import Cardano.Ledger.Crypto
    ( StandardCrypto )
import Cardano.Wallet.Read.Eras
    ( EraFun (..) )
import Cardano.Wallet.Read.Tx
    ( Tx (..) )
import Cardano.Wallet.Read.Tx.Eras
    ( onTx )
import Data.List.NonEmpty
    ( NonEmpty )
import Data.Sequence.Strict
    ( StrictSeq )
import GHC.Records
    ( HasField (..) )

import qualified Cardano.Chain.UTxO as BY
import qualified Cardano.Ledger.Alonzo as AL
import qualified Cardano.Ledger.Alonzo.Tx as AL
import qualified Cardano.Ledger.Babbage as BA
import qualified Cardano.Ledger.Shelley as SH
import qualified Cardano.Ledger.Shelley.Tx as SHTx
import qualified Cardano.Ledger.ShelleyMA as SMA

type family OutputsType era where
    OutputsType ByronEra = NonEmpty BY.TxOut
    OutputsType ShelleyEra
        = StrictSeq (SH.TxOut (SH.ShelleyEra StandardCrypto))
    OutputsType AllegraEra
        = StrictSeq (SH.TxOut (SMA.ShelleyMAEra 'SMA.Allegra StandardCrypto))
    OutputsType MaryEra
        = StrictSeq (SH.TxOut (SMA.ShelleyMAEra 'SMA.Mary StandardCrypto))
    OutputsType AlonzoEra
        = StrictSeq (AL.TxOut (AL.AlonzoEra StandardCrypto))
    OutputsType BabbageEra
        = StrictSeq (BA.TxOut (BA.BabbageEra StandardCrypto))

newtype Outputs era = Outputs (OutputsType era)

deriving instance Show (OutputsType era) => Show (Outputs era)
deriving instance Eq (OutputsType era) => Eq (Outputs era)

getEraOutputs :: EraFun Tx Outputs
getEraOutputs
    = EraFun
        { byronFun =  onTx $ \tx -> Outputs $ BY.txOutputs $ BY.taTx tx
        , shelleyFun = onTx $ \((SHTx.Tx b _ _)) -> getOutputs b
        , allegraFun = onTx $ \((SHTx.Tx b _ _)) -> getOutputs b
        , maryFun = onTx $ \(SHTx.Tx b _ _) -> getOutputs b
        , alonzoFun = onTx $ \(AL.ValidatedTx b _ _ _) -> getOutputs b
        , babbageFun = onTx $ \(AL.ValidatedTx b _ _ _) -> getOutputs b
        }

getOutputs
    :: ( HasField "outputs" a (OutputsType b))
    => a -> Outputs b
getOutputs =  Outputs . getField @"outputs"
