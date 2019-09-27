-- |
-- Copyright: © 2018-2019 IOHK
-- License: Apache-2.0
--
-- Provides functions for checking if TCP ports can be connected to, or are
-- available to listen on.
--
-- These can be used for:
--  - Waiting until a server in another process has started.
--  - Start servers for testing when there may be multiple
--    test suites running in parallel.
--

module Cardano.Wallet.Network.Ports
    (
    -- * Allocation
      PortNumber
    , getRandomPort

    -- * Status
    , isPortOpen
    , simpleSockAddr

    -- * Helpers
    , waitForPort
    ) where

import Prelude

import Control.Monad.IO.Class
    ( liftIO )
import Control.Retry
    ( RetryPolicyM, retrying )
import Data.Streaming.Network
    ( bindRandomPortTCP )
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
import UnliftIO.Exception
    ( bracket, throwIO, try )


-- | Wait until a TCP port is open to connections according to a given retry
-- policy. Throws an exception if the time out is reached.
waitForPort :: RetryPolicyM IO -> PortNumber -> IO Bool
waitForPort policy port =
    retrying policy (const (pure . not)) (const $ isPortOpen addr)
  where
    addr = simpleSockAddr (127,0,0,1) port

-- | Find a TCPv4 port which is likely to be free for listening on
-- @localhost@. This binds a socket, receives an OS-assigned port, then closes
-- the socket.
--
-- Note that this is vulnerable to race conditions if another process binds the
-- port returned by 'getRandomPort' before this process does.
--
-- Do not use this unless you have no other option.
getRandomPort :: IO PortNumber
getRandomPort = do
    let hostPreference = "127.0.0.1"
    (port, sock) <- bindRandomPortTCP hostPreference
    liftIO $ close' sock
    return $ fromIntegral port

-- | Checks whether @connect()@ to a given TCPv4 `SockAddr` succeeds or
-- returns `eCONNREFUSED`.
--
-- Rethrows connection exceptions in all other cases (e.g. when the host
-- is unroutable).
--
-- Code courtesy of nh2: https://stackoverflow.com/a/57022572
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
