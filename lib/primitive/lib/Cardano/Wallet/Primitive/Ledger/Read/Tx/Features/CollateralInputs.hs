{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.CollateralInputs
    ( getCollateralInputs
    )
    where

import Prelude

import Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Inputs
    ( fromShelleyTxIns
    )
import Cardano.Wallet.Read.Eras
    ( Era (..)
    , IsEra (..)
    )
import Cardano.Wallet.Read.Tx.CollateralInputs
    ( CollateralInputs (..)
    , CollateralInputsType
    )

import qualified Cardano.Ledger.Shelley.API as SH
import qualified Cardano.Wallet.Primitive.Types.Tx.TxIn as W

getCollateralInputs :: forall era. IsEra era => CollateralInputs era -> [W.TxIn]
getCollateralInputs = case theEra @era of
    Byron -> \_ -> []
    Shelley -> \_ -> []
    Allegra -> \_ -> []
    Mary -> \_ -> []
    Alonzo -> mkShelleyTxCollateralInputsIns
    Babbage -> mkShelleyTxCollateralInputsIns
    Conway -> mkShelleyTxCollateralInputsIns

mkShelleyTxCollateralInputsIns
    :: (Foldable t, CollateralInputsType era ~ t (SH.TxIn crypto))
    => CollateralInputs era -- ^
  -> [W.TxIn]
mkShelleyTxCollateralInputsIns (CollateralInputs ins) = fromShelleyTxIns ins
