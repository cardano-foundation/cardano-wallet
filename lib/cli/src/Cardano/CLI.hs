{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: MIT
--
-- Shared types and helpers for CLI parsing

module Cardano.CLI
    (
    -- * Types
      Port(..)

    -- * Parsing Arguments
    , parseArgWith
    , getRequiredSensitiveValue
    , getOptionalSensitiveValue
    ) where

import Prelude

import Control.Exception
    ( finally )
import Data.Text
    ( Text )
import Data.Text.Class
    ( FromText (..), TextDecodingError (..), ToText (..) )
import Fmt
    ( Buildable, pretty )
import GHC.Generics
    ( Generic )
import GHC.TypeLits
    ( Symbol )
import System.Console.Docopt
    ( Arguments, Docopt, Option, getArgOrExitWith )

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified System.Console.ANSI as ANSI


{-------------------------------------------------------------------------------
                                Extra Types
-------------------------------------------------------------------------------}

-- | Port number with a tag for describing what it is used for
newtype Port (tag :: Symbol) = Port Int
    deriving (Eq, Show, Generic)

instance FromText (Port tag) where
    fromText = fmap Port . fromText

instance ToText (Port tag) where
    toText (Port p) = toText p

{-------------------------------------------------------------------------------
                             Parsing Arguments
-------------------------------------------------------------------------------}

parseArgWith :: FromText a => Docopt -> Arguments -> Option -> IO a
parseArgWith cli args option =
    either (fail . getTextDecodingError) pure . fromText . T.pack
        =<< args `getArgOrExit` option
  where
    getArgOrExit :: Arguments -> Option -> IO String
    getArgOrExit = getArgOrExitWith cli

{-------------------------------------------------------------------------------
                         Processing of Sensitive Data
-------------------------------------------------------------------------------}

-- | Repeatedly prompt a user for a sensitive value, until the supplied value is
-- valid.
getRequiredSensitiveValue
    :: Buildable e
    => (Text -> Either e a)
    -> String
    -> IO (a, Text)
getRequiredSensitiveValue parse prompt = loop where
    loop = do
        putStrLn prompt
        line <- getLineWithSensitiveData
        case parse line of
            Left e -> do
                TIO.putStrLn (pretty e)
                loop
            Right v -> pure (v, line)

-- | Repeatedly prompt a user for an optional sensitive value, until either the
-- supplied value is valid, or until the user enters an empty line (indicating
-- that they do not wish to specify such a value).
getOptionalSensitiveValue
    :: Buildable e
    => (Text -> Either e a)
    -> String
    -> IO (Maybe (a, Text))
getOptionalSensitiveValue parse prompt = loop where
    loop = do
        putStrLn prompt
        line <- getLineWithSensitiveData
        if T.length line == 0
        then pure Nothing
        else case parse line of
            Left e -> do
                TIO.putStrLn (pretty e)
                loop
            Right v ->
                pure (Just (v, line))

-- | Read a line of user input containing sensitive data from the terminal.
--
-- The terminal lines containing the data are cleared once the user has finished
-- entering data.
--
-- The terminal lines containing the data are also cleared if the application
-- exits abnormally, before the user has finished entering data.
--
getLineWithSensitiveData :: IO Text
getLineWithSensitiveData = do
    finally TIO.getLine . clearSensitiveData =<< ANSI.getCursorPosition0
  where
    clearSensitiveData cursorPosition = case cursorPosition of
        Just (_, y) -> do
            -- We know the original position of the cursor.
            -- Just clear everything from that line onwards.
            ANSI.setCursorPosition 0 y
            ANSI.clearFromCursorToScreenEnd
        Nothing ->
            -- We don't know the original position of the cursor.
            -- For safety, we must clear the entire screen.
            ANSI.clearScreen
