{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Copyright: © 2020-2022 IOHK
-- License: Apache-2.0
--

module Cardano.Wallet.Read.Tx.Cardano
    ( fromCardanoApiTx
    , fromSealedTx
    , anythingFromSealedTx
    ) where

import Prelude

import Cardano.Api
    ( InAnyCardanoEra (..) )
import Cardano.Wallet.Primitive.Types.Tx.SealedTx
    ( SealedTx (unsafeCardanoTx) )
import Cardano.Wallet.Read.Eras
    ( EraValue
    , K (..)
    , allegra
    , alonzo
    , babbage
    , byron
    , conway
    , inject
    , mary
    , shelley
    )
import Cardano.Wallet.Read.Eras.EraFun
    ( EraFun, applyEraFun )
import Cardano.Wallet.Read.Eras.EraValue
    ( extractEraValue )
import Cardano.Wallet.Read.Tx
    ( Tx (..) )
import Control.Monad
    ( void )

import qualified Cardano.Api as Cardano
import qualified Cardano.Api.Byron as Cardano
import qualified Cardano.Api.Shelley as Cardano
import qualified Cardano.Wallet.Primitive.Types.Tx.SealedTx as W

fromCardanoApiTx :: Cardano.Tx era -> EraValue Tx
fromCardanoApiTx = \case
    Cardano.ShelleyTx era tx -> case era of
        Cardano.ShelleyBasedEraShelley -> inject shelley $ Tx tx
        Cardano.ShelleyBasedEraAllegra -> inject allegra $ Tx tx
        Cardano.ShelleyBasedEraMary -> inject mary $ Tx tx
        Cardano.ShelleyBasedEraAlonzo -> inject alonzo $ Tx tx
        Cardano.ShelleyBasedEraBabbage -> inject babbage $ Tx tx
        Cardano.ShelleyBasedEraConway -> inject conway $ Tx tx
    Cardano.ByronTx tx -> inject byron $ Tx $ void tx

fromSealedTx:: W.SealedTx -> EraValue Tx
fromSealedTx sealed =
    case unsafeCardanoTx sealed of
        InAnyCardanoEra _ce tx -> fromCardanoApiTx tx

anythingFromSealedTx :: EraFun Tx (K a) -> SealedTx -> a
anythingFromSealedTx f = extractEraValue . applyEraFun f . fromSealedTx
