{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Fee
    ( getFee)
    where

import Prelude

import Cardano.Ledger.Coin
    ( Coin
    )
import Cardano.Wallet.Read
    ( Era (..)
    , theEra
    )
import Cardano.Wallet.Read.Eras
    ( IsEra
    )
import Cardano.Wallet.Read.Tx.Fee
    ( Fee (..)
    , FeeType
    )

import qualified Cardano.Wallet.Primitive.Ledger.Convert as Ledger
import qualified Cardano.Wallet.Primitive.Types.Coin as W

{-# INLINABLE getFee #-}
getFee :: forall era . IsEra era => Fee era -> Maybe W.Coin
getFee = case theEra @era of
    Byron -> \_ -> Nothing
    Shelley -> mkShelleyTxFee
    Allegra -> mkShelleyTxFee
    Mary -> mkShelleyTxFee
    Alonzo -> mkShelleyTxFee
    Babbage -> mkShelleyTxFee
    Conway -> mkShelleyTxFee

mkShelleyTxFee :: FeeType era ~ Coin => Fee era -> Maybe W.Coin
mkShelleyTxFee (Fee c) = Just $ Ledger.toWalletCoin c
