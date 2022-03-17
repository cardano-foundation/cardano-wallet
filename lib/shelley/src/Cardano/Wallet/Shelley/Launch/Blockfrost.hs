{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cardano.Wallet.Shelley.Launch.Blockfrost
    ( TokenFile
    , readToken
    , tokenFileOption
    ) where

import Prelude

import Blockfrost.Client.Core
    ( projectFromFile )
import Blockfrost.Client.Types
    ( Project (..) )
import Options.Applicative
    ( Parser, help, long, metavar, option, str )

newtype TokenFile = TokenFile FilePath
    deriving newtype (Eq, Show)

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
readToken (TokenFile fp) = projectFromFile fp
