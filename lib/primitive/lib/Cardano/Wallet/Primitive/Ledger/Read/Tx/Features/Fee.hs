{-# LANGUAGE TypeFamilies #-}

module Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Fee
    ( getFee)
    where

import Prelude

import Cardano.Ledger.Coin
    ( Coin
    )
import Cardano.Wallet.Read.Eras
    ( EraFun (..)
    , K (..)
    )
import Cardano.Wallet.Read.Tx.Fee
    ( Fee (..)
    , FeeType
    )

import qualified Cardano.Wallet.Primitive.Ledger.Convert as Ledger
import qualified Cardano.Wallet.Primitive.Types.Coin as W

getFee :: EraFun Fee (K (Maybe W.Coin))
getFee = EraFun
    { byronFun = \_ -> K Nothing
    , shelleyFun = mkShelleyTxFee
    , allegraFun = mkShelleyTxFee
    , maryFun = mkShelleyTxFee
    , alonzoFun = mkShelleyTxFee
    , babbageFun = mkShelleyTxFee
    , conwayFun = mkShelleyTxFee
    }

mkShelleyTxFee :: FeeType era ~ Coin => Fee era -> K (Maybe W.Coin) b
mkShelleyTxFee (Fee c) = K $ Just $ Ledger.toWalletCoin c
