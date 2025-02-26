{-# LANGUAGE TemplateHaskell #-}

module Cardano.Wallet.UI.Static
    ( favicon
    , englishWords
    )
where

import Prelude

import Data.FileEmbed
import Data.Text
    ( Text
    )

import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as T

favicon :: BL.ByteString
favicon =
    BL.fromStrict
        $(makeRelativeToProject "data/images/icon.png" >>= embedFile)

englishWords :: [Text]
englishWords =
    fmap T.decodeUtf8 . B8.words
        $ $(makeRelativeToProject "data/english.txt" >>= embedFile)
