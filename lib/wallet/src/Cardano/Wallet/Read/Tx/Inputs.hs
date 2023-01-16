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
-- Raw certificate data extraction from 'Tx'
--

module Cardano.Wallet.Read.Tx.Inputs
    ( InputsType
    , Inputs (..)
    , getEraInputs
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
import Data.Set
    ( Set )
import GHC.Records
    ( HasField (..) )

import qualified Cardano.Chain.UTxO as BY
import qualified Cardano.Ledger.Alonzo.Tx as AL
import qualified Cardano.Ledger.Shelley.API as SH

type family InputsType era where
    InputsType ByronEra = NonEmpty BY.TxIn
    InputsType ShelleyEra = Set (SH.TxIn StandardCrypto)
    InputsType AllegraEra = Set (SH.TxIn StandardCrypto)
    InputsType MaryEra = Set (SH.TxIn StandardCrypto)
    InputsType AlonzoEra = Set (SH.TxIn StandardCrypto)
    InputsType BabbageEra = Set (SH.TxIn StandardCrypto)

newtype Inputs era = Inputs (InputsType era)

deriving instance Show (InputsType era) => Show (Inputs era)
deriving instance Eq (InputsType era) => Eq (Inputs era)

getEraInputs :: EraFun Tx Inputs
getEraInputs
    = EraFun
        { byronFun =  onTx $ \tx -> Inputs $ BY.txInputs $ BY.taTx tx
        , shelleyFun = onTx $ \((SH.Tx b _ _)) -> getInputs b
        , allegraFun = onTx $ \((SH.Tx b _ _)) -> getInputs b
        , maryFun = onTx $ \(SH.Tx b _ _) -> getInputs b
        , alonzoFun = onTx $ \(AL.ValidatedTx b _ _ _) -> getInputs b
        , babbageFun = onTx $ \(AL.ValidatedTx b _ _ _) -> getInputs b
        }

getInputs
    :: ( HasField "inputs" a (InputsType b))
    => a -> Inputs b
getInputs =  Inputs . getField @"inputs"
