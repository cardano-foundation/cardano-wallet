-- |
-- Copyright: © 2018-2019 IOHK
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
    , isPortOpen
    , simpleSockAddr
    ) where

import Prelude

import Control.Monad
    ( filterM )
import Data.List
    ( sort )
import Data.Word
    ( Word8 )
import Foreign.C.Error
    ( Errno (..), eCONNREFUSED )
import GHC.IO.Exception
    ( IOException (..) )
import Network.Socket
    ( Family (AF_INET)
    , PortNumber
    , SockAddr (SockAddrInet)
    , SocketType (Stream)
    , close'
    , connect
    , socket
    , tupleToHostAddress
    )
import System.Random.Shuffle
    ( shuffleM )
import UnliftIO.Exception
    ( bracket, throwIO, try )

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

-- | Checks whether @connect()@ to a given TCPv4 `SockAddr` succeeds or
-- returns `eCONNREFUSED`.
--
-- Rethrows connection exceptions in all other cases (e.g. when the host
-- is unroutable).
isPortOpen :: SockAddr -> IO Bool
isPortOpen sockAddr = do
  bracket (socket AF_INET Stream 6 {- TCP -}) close' $ \sock -> do
    res <- try $ connect sock sockAddr
    case res of
      Right () -> return True
      Left e ->
        if (Errno <$> ioe_errno e) == Just eCONNREFUSED
          then return False
          else throwIO e


-- | Creates a `SockAttr` from host IP and port number.
--
-- Example:
-- > simpleSockAddr (127,0,0,1) 8000
simpleSockAddr :: (Word8, Word8, Word8, Word8) -> PortNumber -> SockAddr
simpleSockAddr addr port = SockAddrInet port (tupleToHostAddress addr)
