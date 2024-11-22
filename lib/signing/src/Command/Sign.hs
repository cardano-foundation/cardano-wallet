{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_HADDOCK hide #-}

module Command.Sign
    ( Cmd
    , mod
    , run

    ) where

import Prelude hiding
    ( mod
    )

import Data.ByteString
    ( ByteString
    )
import Data.Text
    ( Text
    )
import Options.Applicative
    ( CommandFields
    , Mod
    , command
    , footerDoc
    , header
    , helper
    , info
    , progDesc
    )
import Options.Applicative.Help.Pretty
    ( Doc
    , pretty
    , vsep
    )

data Cmd = Cmd
    { cbor :: ByteString
    , xpub :: Text
    , path :: Text
    } deriving (Show)

mod :: (Cmd -> parent) -> Mod CommandFields parent
mod liftCmd = command "sign" $
    info (helper <*> fmap liftCmd parser) $ mempty
        <> progDesc "Sign a transaction"
        <> header "Add witness of enterprise address of path xxxx given its parent account xpub and cbor."
        <> footerDoc (Just $ vsep
            [ prettyText "The script is taken as argument."
            , prettyText ""
            , prettyText "Example:"
            ])
  where
    parser = undefined

    prettyText :: Text -> Doc
    prettyText = pretty

run :: Cmd -> IO ()
run _ = undefined
