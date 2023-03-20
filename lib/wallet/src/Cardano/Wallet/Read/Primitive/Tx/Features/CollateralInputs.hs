{-# LANGUAGE TypeFamilies #-}

module Cardano.Wallet.Read.Primitive.Tx.Features.CollateralInputs
    ( getCollateralInputs
    )
    where

import Prelude

import Cardano.Wallet.Read.Eras
    ( EraFun (..), K (..) )
import Cardano.Wallet.Read.Primitive.Tx.Features.Inputs
    ( fromShelleyTxIns )
import Cardano.Wallet.Read.Tx.CollateralInputs
    ( CollateralInputs (..), CollateralInputsType )

import qualified Cardano.Ledger.Shelley.API as SH
import qualified Cardano.Wallet.Primitive.Types.Tx.TxIn as W

getCollateralInputs :: EraFun CollateralInputs (K [W.TxIn])
getCollateralInputs = EraFun
    { byronFun = \_ -> K []
    , shelleyFun = \_ -> K []
    , allegraFun = \_ -> K []
    , maryFun = \_ -> K []
    , alonzoFun = mkShelleyTxCollateralInputsIns
    , babbageFun = mkShelleyTxCollateralInputsIns
    , conwayFun = mkShelleyTxCollateralInputsIns
    }

mkShelleyTxCollateralInputsIns
    :: (Foldable t, CollateralInputsType era ~ t (SH.TxIn crypto))
    => CollateralInputs era -- ^
  -> K [W.TxIn] b
mkShelleyTxCollateralInputsIns (CollateralInputs ins) = fromShelleyTxIns ins
