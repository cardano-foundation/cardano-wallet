{-# LANGUAGE DeriveFunctor #-}

module Cardano.Wallet.Api.Aeson.Variant
    ( variants
    , Variant
    , variant
    ) where

import Prelude

import Data.Aeson.Types
    ( Object, Parser, Value, modifyFailure, withObject )

data Variant a = Variant
    { _errContext :: String
    , _acceptance :: Object -> Bool
    , _parser :: Value -> Parser a
    }
    deriving (Functor)

-- | Define a variant for the object json parsing
variant
    :: String -- ^ variant error context
    -> (Object -> Bool) -- ^ variance acceptance 
    -> (Value -> Parser a) -- ^ variance parser 
    -> Variant a
variant = Variant

-- | Compute a parser for `a` given parsers for its variants
variants 
    :: String -- ^ external parsing error context 
    -> [Variant a] -- ^ possible variants
    -> Value -- ^ value to parse
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
