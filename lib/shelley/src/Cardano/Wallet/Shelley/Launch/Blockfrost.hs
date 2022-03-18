{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.Shelley.Launch.Blockfrost
    ( TokenFile (..)
    , readToken
    , tokenFileOption
    , TokenFileException(..)
    ) where

import Prelude

import Blockfrost.Client.Types
    ( Project (..) )
import Blockfrost.Env
    ( parseEnv )
import Control.Exception
    ( Exception, throw )
import Data.Text
    ( Text )
import Options.Applicative
    ( Parser, help, long, metavar, option, str )

import qualified Data.Text as Text
import qualified Data.Text.IO as Text

newtype TokenFile = TokenFile FilePath
    deriving newtype (Eq, Show)

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
readToken (TokenFile fp) = Text.readFile fp >>=
    either (throw (TokenFileException fp) . const) pure . mkProject
  where
    -- Can't use `Blockfrost.Client.Core.projectFromFile` as it uses `error`
    -- and it leads to an unnecessary output that pollutes stdout.
    mkProject :: Text -> Either Text Project
    mkProject t =
      let st = Text.strip t
          tEnv = Text.dropEnd 32 st
          token = Text.drop (Text.length tEnv) st
      in Project <$> parseEnv tEnv <*> pure token
