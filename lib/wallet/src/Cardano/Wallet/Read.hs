-- |
-- Copyright: © 2022 IOHK
-- License: Apache-2.0
--
-- Data that is read from the mainnet ledger, represented in a way
-- that is compatible with the era-specific types from @cardano-ledger@.
--
-- This module re-exports the children of this module hierarchy
-- and is meant to be imported qualified, e.g.
--
-- @
-- import qualified Cardano.Wallet.Read as Read
-- @
module Cardano.Wallet.Read
    ( module Cardano.Wallet.Read.Block
    , module Cardano.Wallet.Read.Tx
    ) where

import Cardano.Wallet.Read.Block
import Cardano.Wallet.Read.Tx
