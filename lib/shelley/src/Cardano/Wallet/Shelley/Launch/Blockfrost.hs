{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.Shelley.Launch.Blockfrost
    ( TokenFile (..)
    , readToken
    , tokenFileOption
    , TokenException(..)
    , TokenFileException(..)
    ) where

import Prelude

import Blockfrost.Client.Types
    ( Project (..) )
import Blockfrost.Env
    ( parseEnv )
import Control.Exception
    ( Exception, IOException, catch, throw )
import Control.Monad
    ( when )
import Options.Applicative
    ( Parser, help, long, metavar, option, str )

import qualified Data.Text as T
import qualified Data.Text.IO as T

newtype TokenFile = TokenFile FilePath
    deriving newtype (Eq, Show)

data TokenException = EmptyToken FilePath | InvalidToken FilePath
    deriving stock (Eq, Show)
    deriving anyclass (Exception)

newtype TokenFileException = TokenFileException FilePath
    deriving stock (Eq, Show)
    deriving anyclass (Exception)

-- | --blockfrost-token-file FILE
tokenFileOption :: Parser TokenFile
tokenFileOption = option (TokenFile <$> str) $ mconcat
    [ long "blockfrost-token-file"
    , metavar "FILE"
    , help $ mconcat
        [ "FILE contains an authentication token for "
        , "BlockFrost Cardano API (https://blockfrost.io)."
        ]
    ]

readToken :: TokenFile -> IO Project
readToken (TokenFile fp) = do
    -- Can't use `Blockfrost.Client.Core.projectFromFile` as it uses `error`
    -- and it leads to an unnecessary output that pollutes stdout.
    line <- T.readFile fp `catch` \(_ :: IOException) ->
        throw $ TokenFileException fp
    let tokenSrc = T.strip line
    when (T.null tokenSrc) $ throw $ EmptyToken fp
    let tEnv = T.dropEnd 32 tokenSrc
        token = T.drop (T.length tEnv) tokenSrc
    case Project <$> parseEnv tEnv <*> pure token of
      Left _ -> throw $ InvalidToken fp
      Right project -> pure project
