{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: MIT
--
-- Provides a mechanism for Daedalus to discover what port the cardano-wallet
-- server is listening on.
--
-- See <https://nodejs.org/api/child_process.html#child_process_child_process_spawn_command_args_options>
-- for more information about the message protocol.

module Cardano.Wallet.DaedalusIPC
    ( daedalusIPC
    ) where

import Prelude

import Cardano.BM.Trace
    ( Trace, logError, logInfo, logNotice )
import Control.Concurrent
    ( threadDelay )
import Control.Concurrent.Async
    ( concurrently_, race )
import Control.Concurrent.MVar
    ( MVar, newEmptyMVar, putMVar, takeMVar )
import Control.Exception
    ( IOException, catch, tryJust )
import Control.Monad
    ( forever )
import Data.Aeson
    ( FromJSON (..)
    , ToJSON (..)
    , Value (..)
    , eitherDecode
    , encode
    , object
    , withObject
    , (.:)
    , (.=)
    )
import Data.Bifunctor
    ( first )
import Data.Binary.Get
    ( getWord32le, getWord64le, runGet )
import Data.Binary.Put
    ( putLazyByteString, putWord32le, putWord64le, runPut )
import Data.Functor
    ( ($>) )
import Data.Maybe
    ( fromMaybe )
import Data.Text
    ( Text )
import Data.Word
    ( Word32, Word64 )
import Fmt
    ( fmt, (+||), (||+) )
import GHC.IO.Handle.FD
    ( fdToHandle )
import System.Environment
    ( lookupEnv )
import System.Info
    ( arch )
import System.IO
    ( Handle, hFlush, hGetLine, hSetNewlineMode, noNewlineTranslation )
import System.IO.Error
    ( IOError )
import Text.Read
    ( readEither )

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Text as T

----------------------------------------------------------------------------
-- Daedalus <-> Wallet child process port discovery protocol

data MsgIn = QueryPort
    deriving (Show, Eq)
data MsgOut = Started | ReplyPort Int | ParseError Text
    deriving (Show, Eq)

instance FromJSON MsgIn where
    parseJSON = withObject "MsgIn" $ \v -> do
        (_ :: [()]) <- v .: "QueryPort"
        pure QueryPort

instance ToJSON MsgOut where
    toJSON Started = object [ "Started" .= Array mempty ]
    toJSON (ReplyPort p) = object [ "ReplyPort" .= p ]
    toJSON (ParseError e) = object [ "ParseError" .= e ]

-- | Start up the Daedalus IPC process. It's called 'daedalusIPC', but this
-- could be any nodejs program that needs to start cardano-wallet. All it does
-- is reply with a port number when asked, using a very nodejs-specific IPC
-- method.
--
-- If the IPC channel was successfully set up, this function won't return until
-- the parent process exits. Otherwise, it will return immediately. Before
-- returning, it will log an message about why it has exited.
daedalusIPC
    :: Trace IO Text
    -- ^ Logging object
    -> Int
    -- ^ Port number to send to Daedalus
    -> IO ()
daedalusIPC trace port = withNodeChannel (pure . msg) action >>= \case
    Right runServer -> do
        logInfo trace "Daedalus IPC server starting"
        runServer >>= \case
            Left (NodeChannelFinished err) ->
                logNotice trace $ fmt $
                "Daedalus IPC finished for this reason: "+||err||+""
            Right () -> logError trace "Unreachable code"
    Left NodeChannelDisabled -> do
        logInfo trace $ "Daedalus IPC is not enabled."
        sleep
    Left (NodeChannelBadFD err) ->
        logError trace $ fmt $ "Problem starting Daedalus IPC: "+||err||+""
  where
    -- How to respond to an incoming message, or when there is an incoming
    -- message that couldn't be parsed.
    msg (Right QueryPort) = Just (ReplyPort port)
    msg (Left e) = Just (ParseError e)

    -- What to do in context of withNodeChannel
    action :: (MsgOut -> IO ()) -> IO ()
    action send = send Started >> sleep

    sleep = threadDelay maxBound

----------------------------------------------------------------------------
-- NodeJS child_process IPC protocol
-- https://nodejs.org/api/child_process.html#child_process_child_process_spawn_command_args_options

-- | Possible reasons why the node channel can't be set up.
data NodeChannelError
    = NodeChannelDisabled
      -- ^ This process has not been started as a nodejs @'ipc'@ child_process.
    | NodeChannelBadFD Text
      -- ^ The @NODE_CHANNEL_FD@ environment variable has an incorrect value.
    deriving (Show, Eq)

-- | The only way a node channel finishes on its own is if there is some error
-- reading or writing to its file descriptor.
newtype NodeChannelFinished = NodeChannelFinished IOError

-- | Communicate with a parent process using a NodeJS-specific protocol. This
-- process must have been spawned with one of @stdio@ array entries set to
-- @'ipc'@.
--
-- If the channel could be set up, then it returns a function for communicating
-- with the parent process.
withNodeChannel
    :: (FromJSON msgin, ToJSON msgout)
    => (Either Text msgin -> IO (Maybe msgout))
       -- ^ Handler for messages coming from the parent process. Left values are
       -- for JSON parse errors. The handler can optionally return a reply
       -- message.
    -> ((msgout -> IO ()) -> IO a)
       -- ^ Action to run with the channel. It is passed a function for sending
       -- messages to the parent process.
    -> IO (Either NodeChannelError (IO (Either NodeChannelFinished a)))
withNodeChannel onMsg handleMsg = fmap setup <$> lookupNodeChannel
  where
    setup handle = do
        chan <- newEmptyMVar
        let ipc = ipcListener handle onMsg chan
            action' = handleMsg (putMVar chan)
        race ipc action'

-- | Parse the NODE_CHANNEL_FD variable, if it's set, and convert to a
-- 'System.IO.Handle'.
lookupNodeChannel :: IO (Either NodeChannelError Handle)
lookupNodeChannel = (fromMaybe "" <$> lookupEnv "NODE_CHANNEL_FD") >>= \case
    "" -> pure (Left NodeChannelDisabled)
    var -> case readEither var of
        Left err -> pure . Left . NodeChannelBadFD $
           "unable to parse NODE_CHANNEL_FD: " <> T.pack err
        Right fd -> tryJust handleBadFd (fdToHandle fd)
  where
    handleBadFd :: IOException -> Maybe NodeChannelError
    handleBadFd = Just . NodeChannelBadFD . T.pack . show

ipcListener
    :: forall msgin msgout. (FromJSON msgin, ToJSON msgout)
    => Handle
    -> (Either Text msgin -> IO (Maybe msgout))
    -> MVar msgout
    -> IO NodeChannelFinished
ipcListener handle onMsg chan = NodeChannelFinished <$> do
    hSetNewlineMode handle noNewlineTranslation
    (concurrently_ replyLoop sendLoop $> unexpected) `catch` pure
  where
    sendLoop, replyLoop :: IO ()
    replyLoop = forever (recvMsg >>= onMsg >>= maybeSend)
    sendLoop = forever (takeMVar chan >>= sendMsg)

    recvMsg :: IO (Either Text msgin)
    recvMsg = first T.pack . eitherDecode <$> readMessage handle

    sendMsg :: msgout -> IO ()
    sendMsg = sendMessage handle . encode

    maybeSend :: Maybe msgout -> IO ()
    maybeSend = maybe (pure ()) (putMVar chan)

    unexpected = userError "ipcListener: unreachable code"

readMessage :: Handle -> IO BL.ByteString
readMessage = if isWindows then windowsReadMessage else posixReadMessage

isWindows :: Bool
isWindows = arch == "windows"

windowsReadMessage :: Handle -> IO BL.ByteString
windowsReadMessage handle = do
    _int1 <- readInt32 handle
    _int2 <- readInt32 handle
    size <- readInt64 handle
    -- logInfo $ "int is: " <> (show [_int1, _int2]) <> " and blob is: " <> (show blob)
    BL.hGet handle $ fromIntegral size
  where
    readInt64 :: Handle -> IO Word64
    readInt64 hnd = do
        bs <- BL.hGet hnd 8
        pure $ runGet getWord64le bs

    readInt32 :: Handle -> IO Word32
    readInt32 hnd = do
        bs <- BL.hGet hnd 4
        pure $ runGet getWord32le bs

posixReadMessage :: Handle -> IO BL.ByteString
posixReadMessage = fmap L8.pack . hGetLine

sendMessage :: Handle -> BL.ByteString -> IO ()
sendMessage handle msg = send handle msg >> hFlush handle
  where
    send = if isWindows then sendMessageWindows else sendMessagePosix

sendMessageWindows :: Handle -> BL.ByteString -> IO ()
sendMessageWindows = sendWindowsMessage' 1 0

sendWindowsMessage' :: Word32 -> Word32 -> Handle -> BL.ByteString -> IO ()
sendWindowsMessage' int1 int2 handle blob =
    L8.hPut handle $ runPut $ mconcat parts
  where
    blob' = blob <> "\n"
    parts =
        [ putWord32le int1
        , putWord32le int2
        , putWord64le $ fromIntegral $ BL.length blob'
        , putLazyByteString blob'
        ]

sendMessagePosix :: Handle -> BL.ByteString -> IO ()
sendMessagePosix = L8.hPutStrLn
