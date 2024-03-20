{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2020-2022 IOHK
-- License: Apache-2.0
--

module Cardano.Wallet.Read.Tx.Cardano
    ( fromCardanoApiTx
    ) where

import Prelude

import Cardano.Wallet.Read.Eras
    ( EraValue
    , eraValue
    )
import Cardano.Wallet.Read.Eras.KnownEras
    ( Allegra
    , Alonzo
    , Babbage
    , Conway
    , Mary
    , Shelley
    )
import Cardano.Wallet.Read.Tx
    ( Tx (..)
    )

import qualified Cardano.Api as Cardano
import qualified Cardano.Api.Shelley as Cardano

fromCardanoApiTx :: Cardano.Tx era -> EraValue Tx
fromCardanoApiTx = \case
    Cardano.ShelleyTx era tx -> case era of
        Cardano.ShelleyBasedEraShelley -> eraValue @Shelley $ Tx tx
        Cardano.ShelleyBasedEraAllegra -> eraValue @Allegra $ Tx tx
        Cardano.ShelleyBasedEraMary -> eraValue @Mary $ Tx tx
        Cardano.ShelleyBasedEraAlonzo -> eraValue @Alonzo $ Tx tx
        Cardano.ShelleyBasedEraBabbage -> eraValue @Babbage $ Tx tx
        Cardano.ShelleyBasedEraConway -> eraValue @Conway $ Tx tx
