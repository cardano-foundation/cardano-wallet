{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Copyright: © 2022 IOHK
-- License: Apache-2.0
--
-- Defines the 'MinimumUTxO' type and related functions.
--
module Cardano.Wallet.Primitive.Types.MinimumUTxO
    ( MinimumUTxO (..)
    , minimumUTxONone
    , minimumUTxOConstant
    , minimumUTxOForShelleyBasedEra
    , MinimumUTxOForShelleyBasedEra (..)
    )
    where

import Prelude

import Cardano.Api.Shelley
    ( ShelleyBasedEra, ShelleyLedgerEra, fromLedgerPParams )
import Cardano.Ledger.Core
    ( PParams )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin )
import Control.DeepSeq
    ( NFData (..) )
import Data.Function
    ( on )
import Fmt
    ( Buildable (..), blockListF )

--------------------------------------------------------------------------------
-- The 'MinimumUTxO' type
--------------------------------------------------------------------------------

data MinimumUTxO where
    MinimumUTxONone
        :: MinimumUTxO
    MinimumUTxOConstant
        :: Coin
        -> MinimumUTxO
    MinimumUTxOForShelleyBasedEraOf
        :: MinimumUTxOForShelleyBasedEra
        -> MinimumUTxO

instance Buildable MinimumUTxO where
    build = \case
        MinimumUTxONone ->
            "MinimumUTxONone"
        MinimumUTxOConstant c -> blockListF
            [ "MinimumUTxOConstant"
            , build c
            ]
        MinimumUTxOForShelleyBasedEraOf m -> blockListF
            [ "MinimumUTxOForShelleyBasedEra"
            , build m
            ]

instance Eq MinimumUTxO where
    (==) = (==) `on` show

instance NFData MinimumUTxO where
    rnf = \case
        MinimumUTxONone ->
            rnf ()
        MinimumUTxOConstant c ->
            rnf c
        MinimumUTxOForShelleyBasedEraOf pp ->
            rnf pp

instance Show MinimumUTxO where
    show = \case
        MinimumUTxONone ->
            "MinimumUTxONone"
        MinimumUTxOConstant c -> unwords
            [ "MinimumUTxOConstant"
            , show c
            ]
        MinimumUTxOForShelleyBasedEraOf pp -> unwords
            [ "MinimumUTxOForShelleyBasedEra"
            , show pp
            ]

minimumUTxONone :: MinimumUTxO
minimumUTxONone = MinimumUTxONone

minimumUTxOConstant :: Coin -> MinimumUTxO
minimumUTxOConstant = MinimumUTxOConstant

minimumUTxOForShelleyBasedEra
    :: ShelleyBasedEra era
    -> PParams (ShelleyLedgerEra era)
    -> MinimumUTxO
minimumUTxOForShelleyBasedEra era pp =
    MinimumUTxOForShelleyBasedEraOf $
    MinimumUTxOForShelleyBasedEra era pp

--------------------------------------------------------------------------------
-- The 'MinimumUTxOForShelleyBasedEra' type
--------------------------------------------------------------------------------

data MinimumUTxOForShelleyBasedEra where
    MinimumUTxOForShelleyBasedEra
        :: ShelleyBasedEra era
        -> PParams (ShelleyLedgerEra era)
        -> MinimumUTxOForShelleyBasedEra

instance Buildable MinimumUTxOForShelleyBasedEra where
    build (MinimumUTxOForShelleyBasedEra era _) = blockListF
        [ "MinimumUTxOForShelleyBasedEra"
        , show era
        ]

instance Eq MinimumUTxOForShelleyBasedEra where
    (==) = (==) `on` show

instance NFData MinimumUTxOForShelleyBasedEra where
    rnf (MinimumUTxOForShelleyBasedEra !_ !_) = rnf ()

instance Show MinimumUTxOForShelleyBasedEra where
    show (MinimumUTxOForShelleyBasedEra era pp) = unwords
        [ show era
        , show (fromLedgerPParams era pp)
        ]
