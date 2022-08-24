{-# LANGUAGE LambdaCase #-}

-- |
-- Copyright: © 2022 IOHK
-- License: Apache-2.0
--
-- Computing minimum UTxO values: internal interface.
--
module Cardano.Wallet.Shelley.MinimumUTxO.Internal
    ( unsafeCoinFromCardanoApiCalculateMinimumUTxOResult
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.Coin
    ( Coin )
import Cardano.Wallet.Shelley.Compatibility
    ( unsafeLovelaceToWalletCoin, unsafeValueToLovelace )
import Data.Function
    ( (&) )
import GHC.Stack
    ( HasCallStack )

import qualified Cardano.Api.Shelley as Cardano

-- | Extracts a 'Coin' value from the result of calling the Cardano API
--   function 'calculateMinimumUTxO'.
--
unsafeCoinFromCardanoApiCalculateMinimumUTxOResult
    :: HasCallStack
    => Either Cardano.MinimumUTxOError Cardano.Value
    -> Coin
unsafeCoinFromCardanoApiCalculateMinimumUTxOResult = \case
    Right value ->
        -- We assume that the returned value is a non-negative ada quantity
        -- with no other assets. If this assumption is violated, we have no
        -- way to continue, and must raise an error:
        value
            & unsafeValueToLovelace
            & unsafeLovelaceToWalletCoin
    Left e ->
        -- The 'Cardano.calculateMinimumUTxO' function should only return
        -- an error if a required protocol parameter is missing.
        --
        -- However, given that values of 'MinimumUTxOForShelleyBasedEra'
        -- can only be constructed by supplying an era-specific protocol
        -- parameters record, it should be impossible to trigger this
        -- condition.
        --
        -- Any violation of this assumption indicates a programming error.
        -- If this condition is triggered, we have no way to continue, and
        -- must raise an error:
        --
        error $ unwords
            [ "unsafeCoinFromCardanoApiCalculateMinimumUTxOResult:"
            , "unexpected error:"
            , show e
            ]
