{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}


{-# OPTIONS_HADDOCK hide #-}

module Command.Deriving
    ( Cmd
    , mod
    , run

    ) where

import Prelude hiding
    ( mod
    )

import Cardano.Wallet.Deriving
    ( DerivedKeys (..)
    , deriveKeys
    , prettyErrDeriveKey
    , toHex
    )
import Data.Text
    ( Text
    )
import Data.Word
    ( Word32
    )
import Options.Applicative
    ( CommandFields
    , Mod
    , Parser
    , command
    , eitherReader
    , footerDoc
    , header
    , help
    , helper
    , info
    , long
    , metavar
    , option
    , progDesc
    )
import Options.Applicative.Help.Pretty
    ( Doc
    , annotate
    , bold
    , indent
    , pretty
    , vsep
    )
import System.IO
    ( stdin
    , stdout
    )
import System.IO.Extra
    ( hGetBytes
    , hPutString
    , progName
    )
import Text.Read
    ( readEither
    )

import qualified Data.Text as T


data Cmd = Deriving
    { derivationIndex :: Word32
    } deriving (Show)

mod :: (Cmd -> parent) -> Mod CommandFields parent
mod liftCmd = command "derive" $
    info (helper <*> fmap liftCmd parser) $ mempty
        <> progDesc "Derive payment public/signing keys of account public key for a given index"
        <> header "Deliver payment credential related keys: public key, with and without chain code, the respective public key hash aka verification key hash and the corresponding extended and signing private keys"
        <> footerDoc (Just $ vsep
            [ prettyText "The extended account private key is read from stdin and credential payment index is taken as an argument. External role is assumed, ie. 0."
            , prettyText "Meaning when one requests credential payment index 3, then relative derivation path on top of account derivation node is 0/3."
            , prettyText ""
            , prettyText "Example:"
            , indent 2 $ annotate bold $ prettyText "$ cat acct.xsk"
            , indent 4 $ prettyText "acct_xsk1mqqrxjztq35xkjzvx8lw730xnxf8lyrgd8fmtrk588fetal8wfvc37fnv7udef5uuktx7lxw3c3msnjmma20k823vy4z9qmclj4352uzq8r72xw2d77l9jafnzr4zjpl9jk7xkeewwq90nweh4q95rd5cud5269p"
            , indent 2 $ annotate bold $ pretty $ "$ " <> progName <>" derive --index 0 <<< $(cat acct.xsk)"
            , indent 4 $ prettyText "xprv: 600d9fc48ef9c6249a6d8190762bdad8439a2c2d13cdd1661878023a06e8725993e19505261ca08fa403d91c975d6e842164fc0614fabbeb998bcb1f71001e07ff273a153a3905108dbb490112d4e3740ec5a3d206423188523bee1c0e8a2c17"
            , indent 4 $ prettyText "prv: 600d9fc48ef9c6249a6d8190762bdad8439a2c2d13cdd1661878023a06e8725993e19505261ca08fa403d91c975d6e842164fc0614fabbeb998bcb1f71001e07"
            , indent 4 $ prettyText "xpub: cc1c6c947f4245d297a7fe9f34bffe08fb6f25322057cc3eb87556fdbca5eaa9ff273a153a3905108dbb490112d4e3740ec5a3d206423188523bee1c0e8a2c17"
            , indent 4 $ prettyText "pub: cc1c6c947f4245d297a7fe9f34bffe08fb6f25322057cc3eb87556fdbca5eaa9"
            , indent 4 $ prettyText "vkh: c8f945808fa4e5e7fa8fdf8f30f2363123c058415825707b26ca9406"
            ])
  where
    parser = Deriving
        <$> derivationIndexOpt

    prettyText :: Text -> Doc
    prettyText = pretty

    derivationIndexFromString :: String -> Either String Word32
    derivationIndexFromString str = case readEither @Word32 str of
        Right word32 -> pure word32
        Left _ -> Left "Derivation index expects non-negative integer"

    derivationIndexOpt :: Parser Word32
    derivationIndexOpt = option (eitherReader derivationIndexFromString) $ mempty
        <> long "index"
        <> metavar "INT"
        <> help "Valid index of payment credential, must be between 0 and 2147483647."

run :: Cmd -> IO ()
run Deriving{derivationIndex} = do
    acctXPrv <- hGetBytes stdin
    case deriveKeys acctXPrv derivationIndex of
        Right derivedKeys -> hPutString stdout (prettyDerivedKeys derivedKeys)
        Left err -> hPutString stdout (T.unpack $ prettyErrDeriveKey err)
  where
    prettyDerivedKeys DerivedKeys{extendedPrivate, private, extendedPublic, public, credential} =
        "xprv: " <> T.unpack (toHex extendedPrivate) <> "\n" <>
        "prv: " <> T.unpack (toHex private) <> "\n" <>
        "xpub: " <> T.unpack (toHex extendedPublic) <> "\n" <>
        "pub: " <> T.unpack (toHex public) <> "\n" <>
        "vkh: " <> T.unpack (toHex credential)
