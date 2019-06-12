{-# LANGUAGE DeriveGeneric #-}
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

import Control.Concurrent
    ( threadDelay )
import Control.Concurrent.Async
    ( race_ )
import Control.Concurrent.MVar
    ( MVar, newEmptyMVar, putMVar, takeMVar )
import Control.Exception
    ( IOException, catch, tryJust )
import Control.Monad
    ( forever, when )
import Data.Aeson
    ( FromJSON (..)
    , ToJSON (..)
    , defaultOptions
    , eitherDecode
    , encode
    , genericParseJSON
    , genericToEncoding
    )
import Data.Aeson.Types
    ( Options, SumEncoding (ObjectWithSingleField), sumEncoding )
import Data.Bifunctor
    ( first )
import Data.Binary.Get
    ( getWord32le, getWord64le, runGet )
import Data.Binary.Put
    ( putLazyByteString, putWord32le, putWord64le, runPut )
import Data.Maybe
    ( fromMaybe )
import Data.Text
    ( Text )
import Data.Word
    ( Word32, Word64 )
import Distribution.System
    ( OS (Windows), buildOS )
import GHC.Generics
    ( Generic )
import GHC.IO.Handle.FD
    ( fdToHandle )
import Say
    ( sayErr, sayErrString )
import System.Environment
    ( lookupEnv )
import System.IO
    ( Handle, hFlush, hGetLine, hSetNewlineMode, noNewlineTranslation, stdout )
import System.IO.Error
    ( IOError, isEOFError )
import Text.Read
    ( readEither )

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Text as T

----------------------------------------------------------------------------
-- Daedalus <-> Wallet child process port discovery protocol

data MsgIn  = QueryPort
    deriving (Show, Eq, Generic)
data MsgOut = Started | ReplyPort Int | ParseError Text
    deriving (Show, Eq, Generic)

aesonOpts :: Options
aesonOpts = defaultOptions { sumEncoding = ObjectWithSingleField }

instance FromJSON MsgIn where
    parseJSON = genericParseJSON aesonOpts
instance ToJSON MsgOut where
    toEncoding = genericToEncoding aesonOpts

daedalusIPC :: Int -> IO ()
daedalusIPC port = withNodeChannel (pure . msg) action >>= \case
    Right act -> do
        sayErr "[INFO] Daedalus IPC server starting"
        act
    Left NodeChannelDisabled -> do
        sayErr "[INFO] Daedalus IPC is not enabled"
        sleep
    Left (NodeChannelBadFD err) ->
        sayErr $ "[ERROR] Starting Daedalus IPC: " <> err
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

data NodeChannelError
    = NodeChannelDisabled
      -- ^ This process has not been started as a nodejs @'ipc'@ child_process.
    | NodeChannelBadFD Text
      -- ^ The @NODE_CHANNEL_FD@ environment variable has an incorrect value.
    deriving (Show, Eq)

withNodeChannel
    :: (FromJSON msgin, ToJSON msgout)
    => (Either Text msgin -> IO (Maybe msgout))
       -- ^ Incoming message handler
    -> ((msgout -> IO ()) -> IO a)
       -- ^ Action to run
    -> IO (Either NodeChannelError (IO ()))
withNodeChannel onMsg handleMsg = fmap setup <$> lookupNodeChannel
  where
    setup handle = do
        chan <- newEmptyMVar
        let ipc = ipcListener handle onMsg chan
            action' = handleMsg (putMVar chan)
        race_ action' ipc

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
    -> IO ()
ipcListener handle onMsg chan = do
    hSetNewlineMode handle noNewlineTranslation
    catch (race_ replyLoop sendLoop) onIOError
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

    onIOError :: IOError -> IO ()
    onIOError err = do
      sayErrString $ "[ERROR] Exception caught in DaedalusIPC: " <> show err
      when (isEOFError err) $ sayErr "[DEBUG] it's an eof"
      hFlush stdout

readMessage :: Handle -> IO BL.ByteString
readMessage
    | buildOS == Windows = windowsReadMessage
    | otherwise = posixReadMessage

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
    send
        | buildOS == Windows = sendWindowsMessage
        | otherwise = sendLinuxMessage

sendWindowsMessage :: Handle -> BL.ByteString -> IO ()
sendWindowsMessage = sendWindowsMessage' 1 0

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

sendLinuxMessage :: Handle -> BL.ByteString -> IO ()
sendLinuxMessage = L8.hPutStrLn
