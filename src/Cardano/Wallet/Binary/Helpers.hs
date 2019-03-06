{-# LANGUAGE RankNTypes #-}

-- | Some extra helpers on top of the CBOR library to help writing decoders.

module Cardano.Wallet.Binary.Helpers
    ( inspectNextToken
    , decodeList
    , decodeListIndef
    ) where

import qualified Codec.CBOR.Decoding as CBOR
import Debug.Trace
    ( traceShow )
import Prelude

-- | Inspect the next token that has to be decoded and print it to the console
-- as a trace. Useful for debugging Decoders.
-- Example:
--
--     myDecoder :: CBOR.Decoder s MyType
--     myDecoder = do
--         a <- CBOR.decodeWord64
--         inspectNextToken
--         [...]
--
inspectNextToken :: CBOR.Decoder s ()
inspectNextToken =
  CBOR.peekTokenType >>= flip traceShow (return ())

-- | Decode an list of known length. Very similar to @decodeListIndef@.
--
-- Example:
--
--     myDecoder :: CBOR.Decoder s [MyType]
--     myDecoder = decodeList decodeOne
--       where
--         decodeOne :: CBOR.Decoder s MyType
--
decodeList :: forall s a . CBOR.Decoder s a -> CBOR.Decoder s [a]
decodeList decodeOne = do
    l <- CBOR.decodeListLenCanonical
    CBOR.decodeSequenceLenN (flip (:)) [] reverse l decodeOne

-- | Decode an arbitrary long list. CBOR introduce a "break" character to
-- mark the end of the list, so we simply decode each item until we encounter
-- a break character.
--
-- Example:
--
--     myDecoder :: CBOR.Decoder s [MyType]
--     myDecoder = decodeListIndef decodeOne
--       where
--         decodeOne :: CBOR.Decoder s MyType
--
decodeListIndef :: forall s a. CBOR.Decoder s a -> CBOR.Decoder s [a]
decodeListIndef decodeOne = do
    _ <- CBOR.decodeListLenIndef
    CBOR.decodeSequenceLenIndef (flip (:)) [] reverse decodeOne
