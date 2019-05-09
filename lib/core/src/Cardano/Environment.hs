{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

-- | Helpers for reading ENV vars using 'unsafePerformIO' with readable error
-- messages.
--
-- Copyright: © 2018-2019 IOHK
-- License: MIT
--
module Cardano.Environment
    (
      ErrMissingOrInvalidEnvVar(..)
    , unsafeLookupEnv
    ) where

import Prelude

import Control.Exception
    ( Exception (..), throwIO )
import Data.Text
    ( Text )
import Data.Text.Class
    ( FromText (..), TextDecodingError (..) )
import Fmt
    ( Buildable (..), nameF, padLeftF, pretty )
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
--                     *--> patate is neither "mainnet", "testnet" nor "staging"
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
