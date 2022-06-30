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
    , ProtocolParametersForShelleyBasedEra (..)
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
    MinimumUTxOForShelleyBasedEra
        :: ProtocolParametersForShelleyBasedEra
        -> MinimumUTxO

instance Buildable MinimumUTxO where
    build = \case
        MinimumUTxONone ->
            "MinimumUTxONone"
        MinimumUTxOConstant c -> blockListF
            [ "MinimumUTxOConstant"
            , build c
            ]
        MinimumUTxOForShelleyBasedEra pp -> blockListF
            [ "MinimumUTxOForShelleyBasedEra"
            , build pp
            ]

instance Eq MinimumUTxO where
    (==) = (==) `on` show

instance NFData MinimumUTxO where
    rnf = \case
        MinimumUTxONone ->
            rnf ()
        MinimumUTxOConstant c ->
            rnf c
        MinimumUTxOForShelleyBasedEra pp ->
            rnf pp

instance Show MinimumUTxO where
    show = \case
        MinimumUTxONone ->
            "MinimumUTxONone"
        MinimumUTxOConstant c -> unwords
            [ "MinimumUTxOConstant"
            , show c
            ]
        MinimumUTxOForShelleyBasedEra pp -> unwords
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
    MinimumUTxOForShelleyBasedEra $
    ProtocolParametersForShelleyBasedEra era pp

--------------------------------------------------------------------------------
-- The 'ProtocolParametersForShelleyBasedEra' type
--------------------------------------------------------------------------------

data ProtocolParametersForShelleyBasedEra where
    ProtocolParametersForShelleyBasedEra
        :: ShelleyBasedEra era
        -> PParams (ShelleyLedgerEra era)
        -> ProtocolParametersForShelleyBasedEra

instance Buildable ProtocolParametersForShelleyBasedEra where
    build (ProtocolParametersForShelleyBasedEra era _) = blockListF
        [ "ProtocolParametersForShelleyBasedEra"
        , show era
        ]

instance Eq ProtocolParametersForShelleyBasedEra where
    (==) = (==) `on` show

instance NFData ProtocolParametersForShelleyBasedEra where
    rnf (ProtocolParametersForShelleyBasedEra !_ !_) = rnf ()

instance Show ProtocolParametersForShelleyBasedEra where
    show (ProtocolParametersForShelleyBasedEra era pp) = unwords
        [ show era
        , show (fromLedgerPParams era pp)
        ]
