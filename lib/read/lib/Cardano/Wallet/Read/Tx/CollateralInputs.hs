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
-- Raw collateral inputs data extraction from 'Tx'
--

module Cardano.Wallet.Read.Tx.CollateralInputs
    ( CollateralInputsType
    , CollateralInputs (..)
    , getEraCollateralInputs
    )
    where

import Prelude

import Cardano.Ledger.Api
    ( collateralInputsTxBodyL
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
import Data.Set
    ( Set
    )

import qualified Cardano.Ledger.Shelley.API as SH

type family CollateralInputsType era where
    CollateralInputsType Byron = ()
    CollateralInputsType Shelley = ()
    CollateralInputsType Allegra = ()
    CollateralInputsType Mary = ()
    CollateralInputsType Alonzo = Set (SH.TxIn StandardCrypto)
    CollateralInputsType Babbage = Set (SH.TxIn StandardCrypto)
    CollateralInputsType Conway = Set (SH.TxIn StandardCrypto)

newtype CollateralInputs era = CollateralInputs (CollateralInputsType era)

deriving instance Show (CollateralInputsType era) => Show (CollateralInputs era)
deriving instance Eq (CollateralInputsType era) => Eq (CollateralInputs era)

{-# INLINABLE getEraCollateralInputs #-}
-- | Extract the collateral inputs from a 'Tx' in any era.
getEraCollateralInputs :: forall era. IsEra era => Tx era -> CollateralInputs era
getEraCollateralInputs = case theEra @era of
    Byron -> \_ -> CollateralInputs ()
    Shelley -> \_ -> CollateralInputs ()
    Allegra -> \_ -> CollateralInputs ()
    Mary -> \_ -> CollateralInputs ()
    Alonzo -> mkCollateralInputs
    Babbage -> mkCollateralInputs
    Conway -> mkCollateralInputs
  where
    mkCollateralInputs = onTx $ \tx ->
        CollateralInputs
            $ tx ^. bodyTxL . collateralInputsTxBodyL
