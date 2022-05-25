{-# LANGUAGE DeriveFunctor #-}

-- |
-- Copyright: © 2022 IOHK
-- License: Apache-2.0
--
-- This module provides a utility function 'variants' for parsing
-- JSON values using one out of many parsers.
module Cardano.Wallet.Api.Aeson.Variant
    ( variants
    , Variant
    , variant
    ) where

import Prelude

import Data.Aeson.Types
    ( Object, Parser, Value, modifyFailure, withObject )

-- | Specification of a JSON parser suitable for 'variants'.
data Variant a = Variant
    { _errContext :: String
    , _acceptance :: Object -> Bool
    , _parser :: Value -> Parser a
    }
    deriving (Functor)

-- | Define a 'Variant' for parsing a JSON value.
--
-- A predicate checks whether a given 'Value' belongs to this variant;
-- the 'Value' is parsed only if this this check succeeds.
variant
    :: String -- ^ Error message suffix in case of parse failure.
    -> (Object -> Bool) -- ^ Check whether this variant applies.
    -> (Value -> Parser a) -- ^ Parser for this variant.
    -> Variant a
variant = Variant

-- | Construct a parser for @a@ from parsers for its variants.
--
-- The parser succeeds iff exactly one of the predicates of the
-- variants succeeds and the parser of that variant also succeeds.
-- Using the predicate in this way improves error messages in case
-- of parse failure.
--
-- For example, 'variants' can be used to parse a JSON value
-- into a disjoint sum ('Either') without needing a tag
-- representing the 'Left'/'Right' cases.
-- Instead, the predicates of the variants can be used to disambiguate a
-- 'Value' by checking the presence of absence of certain JSON object keys.
variants 
    :: String -- ^ Error message suffix in case of parse failure.
    -> [Variant a] -- ^ Possible variants.
    -> Value -- ^ Value to parse.
    -> Parser a
variants ctx xs v = withObject ctx run v
  where
    run obj = case xs >>= mkParser obj of
        [] -> fail "not enough fields to select a variant"
        [p] -> p
        _ -> fail "multiple variants are possible"
    mkParser :: Object -> Variant a -> [Parser a]
    mkParser obj (Variant v_ctx s p)
        | s obj =
            let ctx' = ", " <> v_ctx <> " variant"
            in pure $ modifyFailure (<> ctx') $ p v
        | otherwise = []
