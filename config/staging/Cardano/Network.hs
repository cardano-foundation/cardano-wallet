-- |
-- Copyright: © 2018-2019 IOHK
-- License: MIT
--
-- The network for cardano-wallet is specified via a compile-time flag. This
-- module contains the configuration for staging.
--
-- This config is enabled by default.

module Cardano.Network where

import Data.Int
    ( Int32 )

protocolMagic :: Int32
protocolMagic = 633343913
