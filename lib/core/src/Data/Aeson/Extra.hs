{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2021 IOHK
-- License: Apache-2.0
--
-- This module provides functions and types that extend those provided by
-- the 'aeson' package.
--
module Data.Aeson.Extra
    ( parseBoundedIntegral
    ) where

import Prelude

import Data.Aeson
    ( Value (Number) )
import Data.Aeson.Types
    ( Parser )

import qualified Data.Scientific as Scientific

parseBoundedIntegral
    :: forall a. (Bounded a, Integral a) => String -> Value -> Parser a
parseBoundedIntegral typeName =
    maybe (fail errorMessage) pure . parseInner
  where
    parseInner :: Value -> Maybe a
    parseInner = \case
        Number n -> Scientific.toBoundedInteger n
        _        -> Nothing

    errorMessage :: String
    errorMessage = mconcat
        [ "Failed to parse value of type '" ++ typeName ++ "'. "
        , "Expected an integral value in the range ["
        , show (toInteger $ minBound @a)
        , ", "
        , show (toInteger $ maxBound @a)
        , "]."
        ]
