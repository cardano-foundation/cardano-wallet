module Data.Text.Class.Extended
    ( module Data.Text.Class
    , fromText'
    ) where

import Data.Text.Class
import Prelude

import Data.Bifunctor
    ( first
    )
import Data.Text
    ( Text
    )

import qualified Data.Text as T

-- | 'fromText' but with a simpler error type.
fromText' :: (FromText a) => Text -> Either Text a
fromText' = first (T.pack . getTextDecodingError) . fromText
