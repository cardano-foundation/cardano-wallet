{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}

-- |
-- Copyright: © 2020-2022 IOHK
-- License: Apache-2.0
--

module Cardano.Wallet.Primitive.Ledger.Read.Tx.Sealed
    ( fromSealedTx
    , anythingFromSealedTx
    ) where

import Prelude

import Cardano.Api
    ( InAnyCardanoEra (..)
    )
import Cardano.Wallet.Primitive.Types.Tx.SealedTx
    ( SealedTx (unsafeCardanoTx)
    )
import Cardano.Wallet.Read.Eras
    ( EraValue
    , applyEraFun
    )
import Cardano.Wallet.Read.Tx
    ( Tx (..)
    )
import Cardano.Wallet.Read.Tx.Cardano
    ( fromCardanoApiTx
    )

import qualified Cardano.Wallet.Primitive.Types.Tx.SealedTx as W

fromSealedTx:: W.SealedTx -> EraValue Tx
fromSealedTx sealed =
    case unsafeCardanoTx sealed of
        InAnyCardanoEra _ce tx -> fromCardanoApiTx tx

anythingFromSealedTx :: (forall era . Tx era -> a) -> SealedTx -> a
anythingFromSealedTx f = applyEraFun f . fromSealedTx
