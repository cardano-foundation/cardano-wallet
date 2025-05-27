{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.CollateralInputs
    ( getCollateralInputs
    )
    where

import Prelude

import Cardano.Read.Ledger.Tx.CollateralInputs
    ( CollateralInputs (..)
    , CollateralInputsType
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Inputs
    ( fromShelleyTxIns
    )
import Cardano.Wallet.Read.Eras
    ( Era (..)
    , IsEra (..)
    )

import qualified Cardano.Ledger.Shelley.API as SH
import qualified Cardano.Wallet.Primitive.Types.Tx.TxIn as W

{-# INLINABLE getCollateralInputs #-}
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
    :: (Foldable t, CollateralInputsType era ~ t SH.TxIn)
    => CollateralInputs era -- ^
  -> [W.TxIn]
mkShelleyTxCollateralInputsIns (CollateralInputs ins) = fromShelleyTxIns ins
