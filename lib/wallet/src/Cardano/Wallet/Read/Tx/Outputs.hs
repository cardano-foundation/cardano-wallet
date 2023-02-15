{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
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
import Cardano.Ledger.Core
    ( bodyTxL, outputsTxBodyL )
import Cardano.Ledger.Crypto
    ( StandardCrypto )
import Cardano.Wallet.Read.Eras
    ( EraFun (..) )
import Cardano.Wallet.Read.Tx
    ( Tx (..) )
import Cardano.Wallet.Read.Tx.Eras
    ( onTx )
import Control.Lens
    ( (^.) )
import Data.List.NonEmpty
    ( NonEmpty )
import Data.Sequence.Strict
    ( StrictSeq )

import qualified Cardano.Chain.UTxO as BY
import qualified Cardano.Ledger.Alonzo as AL
import qualified Cardano.Ledger.Babbage as BA
import qualified Cardano.Ledger.Shelley as SH
import qualified Cardano.Ledger.ShelleyMA as SMA

type family OutputsType era where
    OutputsType ByronEra = NonEmpty BY.TxOut
    OutputsType ShelleyEra
        = StrictSeq
            (SH.ShelleyTxOut (SH.ShelleyEra StandardCrypto))
    OutputsType AllegraEra
        = StrictSeq
            (SH.ShelleyTxOut (SMA.ShelleyMAEra 'SMA.Allegra StandardCrypto))
    OutputsType MaryEra
        = StrictSeq
            (SH.ShelleyTxOut (SMA.ShelleyMAEra 'SMA.Mary StandardCrypto))
    OutputsType AlonzoEra
        = StrictSeq
            (AL.AlonzoTxOut (AL.AlonzoEra StandardCrypto))
    OutputsType BabbageEra
        = StrictSeq
            (BA.BabbageTxOut (BA.BabbageEra StandardCrypto))

newtype Outputs era = Outputs (OutputsType era)

deriving instance Show (OutputsType era) => Show (Outputs era)
deriving instance Eq (OutputsType era) => Eq (Outputs era)

getEraOutputs :: EraFun Tx Outputs
getEraOutputs
    = EraFun
        { byronFun =  onTx $ \tx -> Outputs $ BY.txOutputs (BY.taTx tx)
        , shelleyFun = onTx $ \tx -> Outputs (tx ^. bodyTxL . outputsTxBodyL)
        , allegraFun = onTx $ \tx -> Outputs (tx ^. bodyTxL . outputsTxBodyL)
        , maryFun = onTx $ \tx -> Outputs (tx ^. bodyTxL . outputsTxBodyL)
        , alonzoFun = onTx $ \tx -> Outputs (tx ^. bodyTxL . outputsTxBodyL)
        , babbageFun = onTx $ \tx -> Outputs (tx ^. bodyTxL . outputsTxBodyL)
        }
