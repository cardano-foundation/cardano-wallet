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
        <> header "Deliver payment credential keys: public key with and without chain code and private key extended and signing"
        <> footerDoc (Just $ vsep
            [ prettyText "The extended account private key is read from stdin and credential index is taken as an argument."
            , prettyText ""
            , prettyText "Example:"
            ])
  where
    parser = Deriving
        <$> derivationIndexOpt

    prettyText :: Text -> Doc
    prettyText = pretty

    derivationIndexFromString :: String -> Either String Word32
    derivationIndexFromString str = case readEither @Word32 str of
        Right word32 -> pure word32
        Left _ -> Left "Derivation index expects positive integer"

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
    prettyDerivedKeys DerivedKeys{extendedPrivate, private, extendedPublic, public} =
        "xprv: " <> T.unpack (toHex extendedPrivate) <> "\n" <>
        "prv: " <> T.unpack (toHex private) <> "\n" <>
        "xpub: " <> T.unpack (toHex extendedPublic) <> "\n" <>
        "pub: " <> T.unpack (toHex public) <> "\n"
