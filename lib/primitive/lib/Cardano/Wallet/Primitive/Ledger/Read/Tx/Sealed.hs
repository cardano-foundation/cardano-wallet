{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}

-- |
-- Copyright: © 2020-2022 IOHK
-- License: Apache-2.0
module Cardano.Wallet.Primitive.Ledger.Read.Tx.Sealed
    ( fromSealedTx
    ) where

import Cardano.Api
    ( InAnyCardanoEra (..)
    )
import Cardano.Wallet.Primitive.Types.Tx.SealedTx
    ( SealedTx (unsafeCardanoTx)
    )
import Cardano.Wallet.Read
    ( EraValue (..)
    , Tx (..)
    )
import Cardano.Wallet.Read.Eras
    ( Allegra
    , Alonzo
    , Babbage
    , Conway
    , Mary
    , Shelley
    )
import Prelude

import qualified Cardano.Api as Cardano
import qualified Cardano.Wallet.Primitive.Types.Tx.SealedTx as W

fromSealedTx :: W.SealedTx -> EraValue Tx
fromSealedTx sealed =
    case unsafeCardanoTx sealed of
        InAnyCardanoEra _ce tx -> fromCardanoApiTx tx

fromCardanoApiTx :: Cardano.Tx era -> EraValue Tx
fromCardanoApiTx tx0 = case tx0 of
    Cardano.ShelleyTx era tx -> case era of
        Cardano.ShelleyBasedEraShelley -> EraValue (Tx tx :: Tx Shelley)
        Cardano.ShelleyBasedEraAllegra -> EraValue (Tx tx :: Tx Allegra)
        Cardano.ShelleyBasedEraMary -> EraValue (Tx tx :: Tx Mary)
        Cardano.ShelleyBasedEraAlonzo -> EraValue (Tx tx :: Tx Alonzo)
        Cardano.ShelleyBasedEraBabbage -> EraValue (Tx tx :: Tx Babbage)
        Cardano.ShelleyBasedEraConway -> EraValue (Tx tx :: Tx Conway)
        _ -> error "fromCardanoApiTx: era not yet supported"
