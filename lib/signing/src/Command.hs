{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_HADDOCK hide #-}

module Command
    ( CLI

    -- * I/O interface
    , setup
    , parse
    , run
    ) where

import Prelude

import Control.Exception
    ( handle
    )
import Control.Monad
    ( void
    )
import Options.Applicative
    ( ParserInfo
    , customExecParser
    , footerDoc
    , helper
    , info
    , prefs
    , progDesc
    , showHelpOnEmpty
    , subparser
    , (<|>)
    )
import Options.Applicative.Help.Pretty
    ( annotate
    , bold
    , hsep
    , pretty
    , vsep
    )
import System.Console.ANSI
    ( hSupportsANSI
    )
import System.IO
    ( BufferMode (..)
    , Handle
    , hSetBuffering
    , stderr
    , stdin
    , stdout
    )
import System.IO.Extra
    ( prettyIOException
    , progName
    )

import qualified Command.Sign as Sign
import qualified Command.Version as Version

data CLI
    = Sign Sign.Cmd
    | Version
    deriving (Show)

-- | Run a given command
run :: CLI -> IO ()
run = handle prettyIOException . \case
    Sign sub -> Sign.run sub
    Version -> Version.run

cli :: ParserInfo CLI
cli = info (helper <*> parser) $ mempty
    <> progDesc "Command-line tool for external signing of a transaction in Cardano."
    <> footerDoc (Just $ vsep
        [ pretty @String "💡 Need auto-completion?"
        , pretty @String ""
        , hsep
            [ pretty @String "  ↳"
            , annotate bold $ pretty @String "source <("
            , annotate bold $ pretty @String progName
            , annotate bold $ pretty @String $ "--bash-completion-script `which "<>progName<>"`)"
            ]
        , pretty @String ""
        , pretty @String "Or alternatively --fish-completion-script / --zsh-completion-script."
        , pretty @String "For a long-term solution, you may want to put this script in the relevant place. e.g.:"
        , pretty @String ""
        , hsep [pretty @String "  ↳", annotate bold $ pretty @String "/etc/bash_completion.d"]
        ])
  where
    parser = Version.opt Version <|> subparser (mconcat
        [ Sign.mod Sign
        ])

-- | Parse command line options and arguments
parse :: IO CLI
parse = customExecParser (prefs showHelpOnEmpty) cli

-- | Enable ANSI colors on Windows and correct output buffering
setup :: IO ()
setup =
    mapM_ hSetup [stderr, stdout, stdin]
  where
    hSetup :: Handle -> IO ()
    hSetup h = do
      void $ hSupportsANSI h
      hSetBuffering h NoBuffering
