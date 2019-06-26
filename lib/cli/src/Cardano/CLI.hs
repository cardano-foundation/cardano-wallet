{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
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
      Port (..)

    -- * Logging
    , Verbosity (..)
    , initTracer
    , minSeverityFromArgs
    , verbosityFromArgs
    , verbosityToArgs

    -- * Unicode Terminal Helpers
    , setUtf8Encoding

    -- * ANSI Terminal Helpers
    , putErrLn
    , hPutErrLn

    -- * Parsing Arguments
    , OptionValue (..)
    , optional
    , parseArgWith
    , parseAllArgsWith
    , help

    -- * Working with Sensitive Data
    , getLine
    , hGetLine
    , getSensitiveLine
    , hGetSensitiveLine

    -- * Helpers
    , decodeError
    , showT
    ) where

import Prelude hiding
    ( getLine )

import Cardano.BM.Configuration.Model
    ( setMinSeverity )
import Cardano.BM.Configuration.Static
    ( defaultConfigStdout )
import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Setup
    ( setupTrace )
import Cardano.BM.Trace
    ( Trace, appendName )
import Control.Arrow
    ( first )
import Control.Exception
    ( bracket )
import Control.Monad
    ( unless )
import Data.Aeson
    ( (.:) )
import Data.Bifunctor
    ( bimap )
import Data.Functor
    ( (<$) )
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
import Data.Text.Read
    ( decimal )
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
    , isPresent
    , longOption
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

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

{-------------------------------------------------------------------------------
                                Extra Types
-------------------------------------------------------------------------------}

-- | Port number with a tag for describing what it is used for
newtype Port (tag :: Symbol) = Port { getPort :: Int }
    deriving stock (Eq, Show, Generic)
    deriving newtype (Enum, Ord)

-- NOTE
-- TCP port ranges from [[-1;65535]] \ {0}
-- However, ports in [[-1; 1023]] \ {0} are well-known ports reserved
-- and only "bindable" through root privileges.
--
-- Ports above 49151 are also reserved for ephemeral connections. This
-- leaves us with a valid range of [[1024;49151]] for TCP ports.
instance Bounded (Port tag) where
    minBound = Port 1024
    maxBound = Port 49151

instance FromText (Port tag) where
    fromText t = do
        (p, unconsumed) <- bimap (const err) (first Port) (decimal t)
        unless (T.null unconsumed && p >= minBound && p <= maxBound) $ Left err
        return p
      where
        err = TextDecodingError
            $ "expected a TCP port number between "
            <> show (getPort minBound)
            <> " and "
            <> show (getPort maxBound)

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

-- | Make an existing parser optional. Returns 'Right Nothing' if the input is
-- empty, without running the parser.
optional
    :: (Monoid m, Eq m)
    => (m -> Either e a)
    -> (m -> Either e (Maybe a))
optional parse = \case
    m | m == mempty -> Right Nothing
    m  -> Just <$> parse m

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
                                  Logging
-------------------------------------------------------------------------------}

-- | Controls how much information to include in log output.
data Verbosity
    = Default
        -- ^ The default level of verbosity.
    | Quiet
        -- ^ Include less information in the log output.
    | Verbose
        -- ^ Include more information in the log output.
    deriving (Eq, Show)

-- | Determine the minimum 'Severity' level from the specified command line
--   arguments.
minSeverityFromArgs :: Arguments -> Severity
minSeverityFromArgs = verbosityToMinSeverity . verbosityFromArgs

-- | Determine the desired 'Verbosity' level from the specified command line
--   arguments.
verbosityFromArgs :: Arguments -> Verbosity
verbosityFromArgs args
    | args `isPresent` longOption "quiet"   = Quiet
    | args `isPresent` longOption "verbose" = Verbose
    | otherwise = Default

-- | Convert a given 'Verbosity' level into a list of command line arguments
--   that can be passed through to a sub-process.
verbosityToArgs :: Verbosity -> [String]
verbosityToArgs = \case
    Default -> []
    Quiet   -> ["--quiet"]
    Verbose -> ["--verbose"]

-- | Map a given 'Verbosity' level onto a 'Severity' level.
verbosityToMinSeverity :: Verbosity -> Severity
verbosityToMinSeverity = \case
    Default -> Info
    Quiet   -> Error
    Verbose -> Debug

-- | Initialize logging at the specified minimum 'Severity' level.
initTracer :: Severity -> Text -> IO (Trace IO Text)
initTracer minSeverity cmd = do
    c <- defaultConfigStdout
    setMinSeverity c minSeverity
    setupTrace (Right c) "cardano-wallet" >>= appendName cmd

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

{-------------------------------------------------------------------------------
                                 Helpers
-------------------------------------------------------------------------------}

-- | Decode API error messages and extract the corresponding message.
decodeError
    :: BL.ByteString
    -> Maybe Text
decodeError bytes = do
    obj <- Aeson.decode bytes
    Aeson.parseMaybe (Aeson.withObject "Error" (.: "message")) obj

-- | Show a data-type through its 'ToText' instance
showT :: ToText a => a -> String
showT = T.unpack . toText

