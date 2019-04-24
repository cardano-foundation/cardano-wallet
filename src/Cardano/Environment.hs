{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: MIT
--
-- This module contains static configuration parameters. Rather than providing
-- and carrying around a configuration file through the application, we resolve
-- configuration data at runtime using the available environment.
--
-- This gives us a flexible and portable approach to software configuration, and
-- remove some pain from the development perspective. Prior to starting, the
-- wallet is expected to have a few configuration parameter available. One may
-- rely on a `.env` file to bundle configuration settings together for a given
-- target environment.

module Cardano.Environment
    (
    -- * Networking
      Network(..)
    , network
    , ProtocolMagic(..)
    , protocolMagic
    ) where

import Prelude

import Control.Exception
    ( Exception (..), throwIO )
import Data.Int
    ( Int32 )
import Data.Text
    ( Text )
import Data.Text.Class
    ( FromText (..), TextDecodingError (..), ToText (..) )
import Fmt
    ( Buildable (..), nameF, padLeftF, pretty )
import GHC.Generics
    ( Generic )
import System.Environment
    ( getProgName, lookupEnv )
import System.IO.Unsafe
    ( unsafePerformIO )

import qualified Data.Text as T


-- | Fatal exception thrown when a required ENV var is missing upon start-up.
data ErrMissingOrInvalidEnvVar = ErrMissingOrInvalidEnvVar
    { name :: String
    , command :: String
    , additionalContext :: Maybe (String, TextDecodingError)
    }

instance Show ErrMissingOrInvalidEnvVar where
    show = displayException

-- | Produces a nice terminal output so that the error is very readable.
--
-- @
-- $ NETWORK=patate cardano-wallet-launcher
-- Starting...
-- cardano-wallet-launcher: Missing or invalid ENV var:
--
--     ENV[NETWORK] = patate
--                     |
--                     |
--                     *--> patate is neither "mainnet", "testnet", "staging" nor "local".
--
-- @
--
-- @
-- $ cardano-wallet-launcher
-- Starting...
-- cardano-wallet-launcher: Missing or invalid ENV var:
--
--     ENV[NETWORK] = ?
--
-- What about trying to provide a valid ENV var `NETWORK=value cardano-wallet-launcher` ?
-- @
instance Exception ErrMissingOrInvalidEnvVar where
    displayException (ErrMissingOrInvalidEnvVar n cmd ctx) = pretty $ mempty
        <> nameF "Missing or invalid ENV var"
            ( "\n  ENV[" <> build n <> "] = " <> ctxF )
      where
        ctxF = case ctx of
            Nothing -> "?"
                <> "\n\nWhat about trying to provide a valid ENV var "
                <> "`" <> build n <> "=value " <> build cmd <> "` ?"
            Just (v, err) ->
                let
                    pad = length n + (length v `div` 2) + 11
                in
                    build v
                        <> "\n  " <> padLeftF @Text pad ' ' "|    "
                        <> "\n  " <> padLeftF @Text pad ' ' "|    "
                        <> "\n  " <> padLeftF @Text pad ' ' "*--> "
                        <> build err

-- | Lookup the environment for a given variable
unsafeLookupEnv
    :: FromText a
    => String
    -> a
unsafeLookupEnv k = unsafePerformIO $ do
    cmd <- getProgName
    v <- lookupEnv k >>= \case
        Just v -> return v
        Nothing -> throwIO $ ErrMissingOrInvalidEnvVar
            { name = k
            , command =  cmd
            , additionalContext = Nothing
            }
    case fromText (T.pack v) of
        Right a -> return a
        Left err -> throwIO $ ErrMissingOrInvalidEnvVar
            { name = k
            , command = cmd
            , additionalContext = Just (v, err)
            }

{-------------------------------------------------------------------------------
                                 Environment
-------------------------------------------------------------------------------}

-- | Available network options. 'Local' means a local cluster running on the
-- host machine.
data Network = Mainnet | Testnet | Staging | Local
    deriving (Generic, Show, Eq, Enum)

instance FromText Network where
    fromText = \case
        "mainnet" -> Right Mainnet
        "testnet" -> Right Testnet
        "staging" -> Right Staging
        "local" -> Right Local
        s -> Left $ TextDecodingError $ T.unpack s
            <> " is neither \"mainnet\", \"testnet\", \"staging\" nor \"local\"."

instance ToText Network where
    toText = \case
        Mainnet -> "mainnet"
        Testnet -> "testnet"
        Staging -> "staging"
        Local -> "local"

-- | Get the current target 'Network' from the Environment.
--
-- Throws a runtime exception is the ENV var isn't set or, is invalid.
network :: Network
network =
    unsafeLookupEnv "NETWORK"
{-# NOINLINE network #-}

newtype ProtocolMagic = ProtocolMagic Int32
    deriving (Generic, Show)

-- | Get the 'ProtocolMagic' corresponding to a given 'Network'.
--
-- Note that the 'ProtocolMagic' for 'Local' and 'Testnet' are the same.
protocolMagic :: Network -> ProtocolMagic
protocolMagic = \case
    Mainnet -> ProtocolMagic 764824073
    Staging -> ProtocolMagic 633343913
    Testnet -> ProtocolMagic 1097911063
    Local   -> ProtocolMagic 1097911063
