-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Provides functions for checking if TCP ports are available to listen
-- on. These can be used to start servers for testing when there may be multiple
-- test suites running in parallel.
--
-- Includes code from nh2: https://stackoverflow.com/a/57022572

module Test.Utils.Ports
    ( randomUnusedTCPPorts
    , findPort
    ) where

import Prelude

import Cardano.Wallet.Network.Ports
    ( isPortOpen, simpleSockAddr )
import Control.Monad
    ( filterM )
import Data.List
    ( sort )
import System.Random.Shuffle
    ( shuffleM )

-- | Get a list of random TCPv4 ports that currently do not have any servers
-- listening on them. It may return less than the requested number of ports.
--
-- Note that this method of allocating ports is subject to race
-- conditions. Production code should use better methods such as passing a
-- listening socket to the child process.
randomUnusedTCPPorts :: Int -> IO [Int]
randomUnusedTCPPorts count = do
    usablePorts <- shuffleM [1024..49151]
    sort <$> filterM unused (take count usablePorts)
  where
    unused = fmap not . isPortOpen . simpleSockAddr (127,0,0,1) . fromIntegral

-- | Returen a single TCP port that was unused at the time this function was
-- called.
findPort :: IO Int
findPort = head <$> randomUnusedTCPPorts 1
