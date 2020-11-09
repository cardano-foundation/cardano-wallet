{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Test.Integration.Jcli
    (
    -- * Call jcli
      jcli
    , jcli_
    , jcliStdin
    , jcliStdin_

    -- * Constants
    , getBlock0H
    , sinkAddress

    -- * Utils
    , argHex
    , argInt
    ) where


import Prelude

import Cardano.Wallet.Jormungandr.Binary
    ( getBlockId, runGet )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Control.Monad.IO.Class
    ( MonadIO, liftIO )
import Data.ByteArray.Encoding
    ( Base (..), convertToBase )
import Data.Quantity
    ( Quantity (..) )
import System.Command
    ( CmdOption (..), Stdout (..), command )
import System.FilePath
    ( (</>) )
import Test.Utils.Paths
    ( getTestData )

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

-- | Run a `jcli` command, collecting the output
jcli :: (MonadIO m) => [String] -> m String
jcli = liftIO . fmap (trim . fromStdout) . command [] "jcli"

-- | Run a `jcli` command but discards the output
jcli_ :: (MonadIO m) => [String] -> m ()
jcli_ = liftIO . command [] "jcli"

-- | Run a `jcli` command, collecting the output and using the given string as
-- standard input
jcliStdin :: (MonadIO m) => String -> [String] -> m String
jcliStdin stdin = liftIO . fmap (trim . fromStdout) . command [Stdin stdin] "jcli"

-- | Like 'jcliStdin' but discards the output.
jcliStdin_ :: (MonadIO m) => String -> [String] -> m ()
jcliStdin_ stdin = liftIO . command [Stdin stdin] "jcli"

-- | trim newlines caracters in a string
trim :: String -> String
trim = filter (/= '\n')

-- | Get the genesis block hash for the integration tests
getBlock0H :: IO (Hash "Genesis")
getBlock0H = extractId <$> BL.readFile block0
  where
    block0 = $(getTestData) </> "jormungandr" </> "block0.bin"
    extractId = Hash . getHash . runGet getBlockId

-- A sink address where change can be sent.
sinkAddress :: String
sinkAddress =
    "sink1sw76ufc5c58mg5cn2dmze70rrpcpy4vxch60lheuzaq5up83ccatse6wns7"

-- | Easily convert any 'Hash' to an hex-encoded string to be passed as argument
argHex :: Hash any -> String
argHex = T.unpack . T.decodeUtf8 . convertToBase Base16 . getHash

-- | Easily convert any 'Quantity _ Double' to a stringified int. This is
-- particularly useful for passing fee policy values as argument
argInt :: Quantity any Double -> String
argInt (Quantity a) = show (round a :: Int)
