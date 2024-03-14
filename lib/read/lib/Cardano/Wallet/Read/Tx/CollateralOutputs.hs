{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright: © 2020-2022 IOHK
-- License: Apache-2.0
--
-- Raw collateral output data extraction from 'Tx'
--

module Cardano.Wallet.Read.Tx.CollateralOutputs
    ( CollateralOutputsType
    , CollateralOutputs (..)
    , getEraCollateralOutputs
    )
    where

import Prelude

import Cardano.Ledger.Babbage.Collateral
    ()
import Cardano.Ledger.Babbage.Rules
    ()
import Cardano.Ledger.Babbage.Tx
    ()
import Cardano.Ledger.Babbage.TxBody
    ( BabbageTxOut (..)
    , collateralReturnTxBodyL
    )
import Cardano.Ledger.Core
    ( bodyTxL
    )
import Cardano.Ledger.Crypto
    ( StandardCrypto
    )
import Cardano.Wallet.Read.Eras
    ( Allegra
    , Alonzo
    , Babbage
    , Byron
    , Conway
    , Era (..)
    , IsEra (..)
    , Mary
    , Shelley
    )
import Cardano.Wallet.Read.Tx
    ( Tx (..)
    )
import Cardano.Wallet.Read.Tx.Eras
    ( onTx
    )
import Control.Lens
    ( (^.)
    )
import Data.Maybe.Strict
    ( StrictMaybe
    )

import qualified Cardano.Ledger.Babbage as BA
import qualified Cardano.Ledger.Conway as Conway

type family CollateralOutputsType era where
    CollateralOutputsType Byron = ()
    CollateralOutputsType Shelley = ()
    CollateralOutputsType Allegra = ()
    CollateralOutputsType Mary = ()
    CollateralOutputsType Alonzo =  ()
    CollateralOutputsType Babbage
        = StrictMaybe (BabbageTxOut (BA.BabbageEra StandardCrypto))
    CollateralOutputsType Conway
        = StrictMaybe (BabbageTxOut (Conway.ConwayEra StandardCrypto))

newtype CollateralOutputs era = CollateralOutputs (CollateralOutputsType era)

deriving instance Show (CollateralOutputsType era)
    => Show (CollateralOutputs era)
deriving instance Eq (CollateralOutputsType era) => Eq (CollateralOutputs era)

{-# INLINABLE getEraCollateralOutputs #-}
-- | Get the 'CollateralOutputs' for a given 'Tx' in any era.
getEraCollateralOutputs
    :: forall era. IsEra era => Tx era -> CollateralOutputs era
getEraCollateralOutputs = case theEra @era of
    Byron -> \_ -> CollateralOutputs ()
    Shelley -> \_ -> CollateralOutputs ()
    Allegra -> \_ -> CollateralOutputs ()
    Mary -> \_ -> CollateralOutputs ()
    Alonzo -> \_ -> CollateralOutputs ()
    Babbage -> mkCollateralOutputs
    Conway -> mkCollateralOutputs
  where
    mkCollateralOutputs = onTx $ \tx ->
        CollateralOutputs
            $ tx ^. bodyTxL . collateralReturnTxBodyL
