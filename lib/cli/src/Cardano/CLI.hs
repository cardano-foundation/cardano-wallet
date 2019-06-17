{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
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

    -- * Unicode Terminal Helpers
    , setUtf8Encoding

    -- * ANSI Terminal Helpers
    , putErrLn
    , hPutErrLn

    -- * Parsing Arguments
    , OptionValue (..)
    , parseArgWith
    , parseAllArgsWith
    , help

    -- * Working with Sensitive Data
    , getLine
    , hGetLine
    , getSensitiveLine
    , hGetSensitiveLine
    ) where

import Prelude hiding
    ( getLine )

import Cardano.BM.Data.Severity
    ( Severity (..) )
import Control.Exception
    ( bracket )
import Data.Functor
    ( (<$) )
import qualified Data.List.NonEmpty as NE
import Data.Text
    ( Text )
import Data.Text.Class
    ( CaseStyle (..)
    , FromText (..)
    , TextDecodingError (..)
    , ToText (..)
    , fromTextToBoundedEnum
    , toTextFromBoundedEnum
    )
import Fmt
    ( Buildable, pretty )
import GHC.Generics
    ( Generic )
import GHC.TypeLits
    ( Symbol )
import System.Console.ANSI
    ( Color (..)
    , ColorIntensity (..)
    , ConsoleLayer (..)
    , SGR (..)
    , hCursorBackward
    , hSetSGR
    )
import System.Console.Docopt
    ( Arguments
    , Docopt
    , Option
    , exitWithUsageMessage
    , getAllArgs
    , getArgOrExitWith
    , usage
    )
import System.Exit
    ( exitFailure, exitSuccess )
import System.IO
    ( BufferMode (..)
    , Handle
    , hGetBuffering
    , hGetChar
    , hGetEcho
    , hIsTerminalDevice
    , hPutChar
    , hSetBuffering
    , hSetEcho
    , hSetEncoding
    , stderr
    , stdin
    , stdout
    , utf8
    )

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

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

instance FromText (OptionValue Severity) where
    fromText = fmap OptionValue . fromTextToBoundedEnum KebabLowerCase

instance ToText (OptionValue Severity) where
    toText = toTextFromBoundedEnum KebabLowerCase . getOptionValue

{-------------------------------------------------------------------------------
                             Parsing Arguments
-------------------------------------------------------------------------------}

-- | A wrapper to avoid orphan instances for types defined externally.
newtype OptionValue a = OptionValue { getOptionValue :: a }
  deriving (Enum, Eq, Ord, Generic, Read, Show)

parseArgWith :: FromText a => Docopt -> Arguments -> Option -> IO a
parseArgWith cli args option = do
    (fromText . T.pack <$> args `getArgOrExit` option) >>= \case
        Right a -> return a
        Left e -> do
            putErrLn $ T.pack $ getTextDecodingError e
            exitFailure
  where
    getArgOrExit :: Arguments -> Option -> IO String
    getArgOrExit = getArgOrExitWith cli

parseAllArgsWith
    :: FromText a => Docopt -> Arguments -> Option -> IO (NE.NonEmpty a)
parseAllArgsWith cli args option = do
    (mapM (fromText . T.pack) <$> args `getAllArgsOrExit` option) >>= \case
        Right a -> return a
        Left e -> do
            putErrLn $ T.pack $ getTextDecodingError e
            exitFailure
  where
    getAllArgsOrExit :: Arguments -> Option -> IO (NE.NonEmpty String)
    getAllArgsOrExit = getAllArgsOrExitWith cli

-- | Same as 'getAllArgs', but 'exitWithUsage' if empty list.
--
--   As in 'getAllArgs', if your usage pattern required the option,
--   'getAllArgsOrExitWith' will not exit.
getAllArgsOrExitWith :: Docopt -> Arguments -> Option -> IO (NE.NonEmpty String)
getAllArgsOrExitWith doc args opt =
    maybe err pure . NE.nonEmpty $ getAllArgs args opt
  where
    err = exitWithUsageMessage doc $ "argument expected for: " ++ show opt

-- | Like 'exitWithUsage', but with a success exit code
help :: Docopt -> IO ()
help cli = do
    TIO.putStrLn $ T.pack $ usage cli
    exitSuccess

{-------------------------------------------------------------------------------
                            Unicode Terminal Helpers
-------------------------------------------------------------------------------}

-- | Override the system output encoding setting. This is needed because the CLI
-- prints UTF-8 characters regardless of the @LANG@ environment variable.
setUtf8Encoding :: IO ()
setUtf8Encoding = do
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8

{-------------------------------------------------------------------------------
                            ANSI Terminal Helpers
-------------------------------------------------------------------------------}

-- | Print an error message in red
hPutErrLn :: Handle -> Text -> IO ()
hPutErrLn h msg = withSGR h (SetColor Foreground Vivid Red) $ do
    TIO.hPutStrLn h msg

-- | Like 'hPutErrLn' but with provided default 'Handle'
putErrLn :: Text -> IO ()
putErrLn = hPutErrLn stderr

{-------------------------------------------------------------------------------
                         Processing of Sensitive Data
-------------------------------------------------------------------------------}

-- | Prompt user and parse the input. Re-prompt on invalid inputs.
hGetLine
    :: Buildable e
    => (Handle, Handle)
    -> Text
    -> (Text -> Either e a)
    -> IO (a, Text)
hGetLine (hstdin, hstderr) prompt fromT = do
    TIO.hPutStr hstderr prompt
    txt <- TIO.hGetLine hstdin
    case fromT txt of
        Right a ->
            return (a, txt)
        Left e -> do
            hPutErrLn hstderr (pretty e)
            hGetLine (hstdin, hstderr) prompt fromT

-- | Like 'hGetLine' but with default handles
getLine
    :: Buildable e
    => Text
    -> (Text -> Either e a)
    -> IO (a, Text)
getLine = hGetLine (stdin, stderr)

-- | Gather user inputs until a newline is met, hiding what's typed with a
-- placeholder character.
hGetSensitiveLine
    :: Buildable e
    => (Handle, Handle)
    -> Text
    -> (Text -> Either e a)
    -> IO (a, Text)
hGetSensitiveLine (hstdin, hstderr) prompt fromT =
    withBuffering hstderr NoBuffering $
    withBuffering hstdin NoBuffering $
    withEcho hstdin False $ do
        TIO.hPutStr hstderr prompt
        txt <- getLineProtected '*'
        case fromT txt of
            Right a ->
                return (a, txt)
            Left e -> do
                hPutErrLn hstderr (pretty e)
                hGetSensitiveLine (hstdin, hstderr) prompt fromT
  where
    getLineProtected :: Char -> IO Text
    getLineProtected placeholder =
        getLineProtected' mempty
      where
        backspace = toEnum 127
        getLineProtected' line = do
            hGetChar hstdin >>= \case
                '\n' -> do
                    hPutChar hstderr '\n'
                    return line
                c | c == backspace ->
                    if T.null line
                        then getLineProtected' line
                        else do
                            hCursorBackward hstderr  1
                            hPutChar hstderr ' '
                            hCursorBackward hstderr 1
                            getLineProtected' (T.init line)
                c -> do
                    hPutChar hstderr placeholder
                    getLineProtected' (line <> T.singleton c)

-- | Like 'hGetSensitiveLine' but with default handles
getSensitiveLine
    :: Buildable e
    => Text
    -- ^ A message to prompt the user
    -> (Text -> Either e a)
    -- ^ An explicit parser from 'Text'
    -> IO (a, Text)
getSensitiveLine = hGetSensitiveLine (stdin, stderr)

{-------------------------------------------------------------------------------
                                Internals
-------------------------------------------------------------------------------}

withBuffering :: Handle -> BufferMode -> IO a -> IO a
withBuffering h buffering action = bracket aFirst aLast aBetween
  where
    aFirst = (hGetBuffering h <* hSetBuffering h buffering)
    aLast = hSetBuffering h
    aBetween = const action

withEcho :: Handle -> Bool -> IO a -> IO a
withEcho h echo action = bracket aFirst aLast aBetween
  where
    aFirst = (hGetEcho h <* hSetEcho h echo)
    aLast = hSetEcho h
    aBetween = const action

withSGR :: Handle -> SGR -> IO a -> IO a
withSGR h sgr action = hIsTerminalDevice h >>= \case
    True -> bracket aFirst aLast aBetween
    False -> action
  where
    aFirst = ([] <$ hSetSGR h [sgr])
    aLast = hSetSGR h
    aBetween = const action
