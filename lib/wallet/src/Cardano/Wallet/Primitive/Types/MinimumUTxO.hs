{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Copyright: © 2022 IOHK
-- License: Apache-2.0
--
-- Defines the 'MinimumUTxO' type and related functions.
module Cardano.Wallet.Primitive.Types.MinimumUTxO
    ( -- * Types
      MinimumUTxO (..)
    , MinimumUTxOForShelleyBasedEra (..)

      -- * Constructor functions
    , minimumUTxONone
    , minimumUTxOConstant
    , minimumUTxOForShelleyBasedEra
    )
where

import Prelude

import Cardano.Api.Shelley
    ( ShelleyBasedEra
    , ShelleyLedgerEra
    , fromLedgerPParams
    )
import Cardano.Ledger.Core
    ( PParams
    )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin
    )
import Control.DeepSeq
    ( NFData (..)
    )
import Data.Function
    ( on
    )
import Fmt
    ( Buildable (..)
    , blockListF
    )

--------------------------------------------------------------------------------
-- The 'MinimumUTxO' type
--------------------------------------------------------------------------------

-- | Represents a function for computing minimum UTxO values.
data MinimumUTxO where
    MinimumUTxONone
        :: MinimumUTxO
        -- ^ Indicates that there is no minimum UTxO value.
    MinimumUTxOConstant
        :: Coin
        -> MinimumUTxO
        -- ^ Indicates a constant minimum UTxO value. This constructor is
        -- useful for writing tests, where we often want to have precise
        -- control over the value that is chosen.
    MinimumUTxOForShelleyBasedEraOf
        :: MinimumUTxOForShelleyBasedEra
        -> MinimumUTxO
        -- ^ Indicates a Shelley-based era-specific minimum UTxO function.

instance Buildable MinimumUTxO where
    build = \case
        MinimumUTxONone ->
            "MinimumUTxONone"
        MinimumUTxOConstant c ->
            blockListF
                [ "MinimumUTxOConstant"
                , build c
                ]
        MinimumUTxOForShelleyBasedEraOf m ->
            blockListF
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
        MinimumUTxOConstant c ->
            unwords
                [ "MinimumUTxOConstant"
                , show c
                ]
        MinimumUTxOForShelleyBasedEraOf pp ->
            unwords
                [ "MinimumUTxOForShelleyBasedEra"
                , show pp
                ]

--------------------------------------------------------------------------------
-- The 'MinimumUTxOForShelleyBasedEra' type
--------------------------------------------------------------------------------

-- | Represents a minimum UTxO function that is specific to a Shelley-based era.
data MinimumUTxOForShelleyBasedEra where
    MinimumUTxOForShelleyBasedEra
        :: ShelleyBasedEra era
        -> PParams (ShelleyLedgerEra era)
        -> MinimumUTxOForShelleyBasedEra

instance Buildable MinimumUTxOForShelleyBasedEra where
    build (MinimumUTxOForShelleyBasedEra era _) =
        blockListF
            [ "MinimumUTxOForShelleyBasedEra"
            , show era
            ]

instance Eq MinimumUTxOForShelleyBasedEra where
    (==) = (==) `on` show

instance NFData MinimumUTxOForShelleyBasedEra where
    rnf (MinimumUTxOForShelleyBasedEra !_ !_) = rnf ()

instance Show MinimumUTxOForShelleyBasedEra where
    show (MinimumUTxOForShelleyBasedEra era pp) =
        unwords
            [ show era
            , show (fromLedgerPParams era pp)
            ]

--------------------------------------------------------------------------------
-- Constructor functions
--------------------------------------------------------------------------------

minimumUTxONone :: MinimumUTxO
minimumUTxONone = MinimumUTxONone

minimumUTxOConstant :: Coin -> MinimumUTxO
minimumUTxOConstant = MinimumUTxOConstant

minimumUTxOForShelleyBasedEra
    :: ShelleyBasedEra era
    -> PParams (ShelleyLedgerEra era)
    -> MinimumUTxO
minimumUTxOForShelleyBasedEra era pp =
    MinimumUTxOForShelleyBasedEraOf
        $ MinimumUTxOForShelleyBasedEra era pp
