{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
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

    -- * ANSI Terminal Helpers
    , putErrLn

    -- * Parsing Arguments
    , parseArgWith

    -- * Working with Sensitive Data
    , getSensitiveLine
    ) where

import Prelude

import Control.Exception
    ( bracket )
import Data.Functor
    ( (<$) )
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
import System.Console.ANSI
    ( Color (..), ColorIntensity (..), ConsoleLayer (..), SGR (..), hSetSGR )
import System.Console.Docopt
    ( Arguments, Docopt, Option, getArgOrExitWith )
import System.Exit
    ( exitFailure )
import System.IO
    ( BufferMode (..)
    , Handle
    , hGetBuffering
    , hGetEcho
    , hSetBuffering
    , hSetEcho
    , stdin
    , stdout
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

{-------------------------------------------------------------------------------
                             Parsing Arguments
-------------------------------------------------------------------------------}

parseArgWith :: FromText a => Docopt -> Arguments -> Option -> IO a
parseArgWith cli args option = do
    (fromText . T.pack <$> args `getArgOrExit` option) >>= \case
        Right a -> do
            return a
        Left e -> do
            putErrLn $ T.pack $ getTextDecodingError e
            exitFailure
  where
    getArgOrExit :: Arguments -> Option -> IO String
    getArgOrExit = getArgOrExitWith cli

{-------------------------------------------------------------------------------
                            ANSI Terminal Helpers
-------------------------------------------------------------------------------}

-- | Print an error message in red
putErrLn :: Text -> IO ()
putErrLn msg = withSGR (SetColor Foreground Vivid Red) $ do
    TIO.putStrLn msg

{-------------------------------------------------------------------------------
                         Processing of Sensitive Data
-------------------------------------------------------------------------------}

-- | Gather user inputs until a newline is met, hiding what's typed with
getSensitiveLine
    :: Buildable e
    => Text
    -- ^ A message to prompt the user
    -> Maybe Char
    -- ^ Create groups of 'n :: Int' hidden characters when 'c :: Char' is met
    -> (Text -> Either e a)
    -- ^ An explicit parser from 'Text'
    -> IO (a, Text)
getSensitiveLine prompt separator fromT =
    withBuffering stdout NoBuffering $
    withBuffering stdin NoBuffering $
    withEcho False $ do
        TIO.putStr prompt
        txt <- getLineProtected separator '*'
        case fromT txt of
            Right a ->
                return (a, txt)
            Left e -> do
                putErrLn (pretty e)
                getSensitiveLine prompt separator fromT
  where
    getLineProtected :: Maybe Char -> Char -> IO Text
    getLineProtected sep placeholder =
        getLineProtected' ""
      where
        getLineProtected' line = do
            getChar >>= \case
                '\n' -> do
                    putChar '\n'
                    return line
                c | Just c == sep -> do
                    putChar ' '
                    getLineProtected' (line <> T.singleton c)
                c -> do
                    putChar placeholder
                    getLineProtected' (line <> T.singleton c)

{-------------------------------------------------------------------------------
                                Internals
-------------------------------------------------------------------------------}

withBuffering :: Handle -> BufferMode -> IO a -> IO a
withBuffering h buffering action = bracket aFirst aLast aBetween
  where
    aFirst = (hGetBuffering h <* hSetBuffering h buffering)
    aLast = hSetBuffering h
    aBetween = const action

withEcho :: Bool -> IO a -> IO a
withEcho echo action = bracket aFirst aLast aBetween
  where
    aFirst = (hGetEcho stdin <* hSetEcho stdin echo)
    aLast = hSetEcho stdin
    aBetween = const action

withSGR :: SGR -> IO a -> IO a
withSGR sgr action = bracket aFirst aLast aBetween
  where
    aFirst = ([] <$ hSetSGR stdout [sgr])
    aLast = hSetSGR stdout
    aBetween = const action
