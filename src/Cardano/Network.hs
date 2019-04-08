{-# LANGUAGE CPP #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: MIT
--
-- The module contains network configuration and allows it to be controlled via
-- via compile-time flags.
--
-- testnet is default.
-- mainnet can be enabled by passing `--flag cardano-wallet:mainnet` to stack.
-- staging can be enabled by passing `--flag cardano-wallet:staging` to stack.

module Cardano.Network where

import Prelude

import Data.Int
    ( Int32 )

protocolMagic :: Int32
networkName :: String

#ifdef CARDANO_NETWORK_MAINNET

protocolMagic = 764824073
networkName = "mainnet"

#endif
#ifdef CARDANO_NETWORK_TESTNET

protocolMagic = 1097911063
networkName = "testnet"

#endif
#ifdef CARDANO_NETWORK_STAGING

protocolMagic = 633343913
networkName = "staging"

#endif
