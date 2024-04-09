module Cryptography.Padding.PKCS7
    ( pad
    , unpad
    )
    where

import Prelude

import Data.ByteString
    ( ByteString
    )
import Data.Semigroup.Cancellative
    ( RightReductive (stripSuffix)
    )

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8

-- | Appends a PKCS#7 padding suffix to a byte string.
--
-- For a given string @b@, this function returns a padded string @r = b <> s@,
-- where @s@ is a padding suffix chosen such that the length of @r@ is exactly
-- divisible by 16 bytes.
--
-- A padding suffix is characterised by the following properties:
--
-- - The length of a padding suffix is a value in the interval @[1, 16]@.
-- - The value of each byte in a suffix is identical to the suffix length.
--
-- These properties generate the following set of 16 possible padding suffixes:
--
-- @
-- { "\x01"
-- , "\x02\x02"
-- , "\x03\x03\x03"
-- , ...
-- , "\x0e\x0e\x0e\x0e\x0e\x0e\x0e\x0e\x0e\x0e\x0e\x0e\x0e\x0e"
-- , "\x0f\x0f\x0f\x0f\x0f\x0f\x0f\x0f\x0f\x0f\x0f\x0f\x0f\x0f\x0f"
-- , "\x10\x10\x10\x10\x10\x10\x10\x10\x10\x10\x10\x10\x10\x10\x10\x10"
-- }
-- @
--
-- See:
-- https://datatracker.ietf.org/doc/html/rfc5652#section-6.3.
--
pad :: ByteString -> ByteString
pad payload =
    BS.append payload padding
  where
    padding :: ByteString
    padding = B8.replicate paddingLength (toEnum paddingLength)

    paddingLength :: Int
    paddingLength = 16 - (BS.length payload `mod` 16)

-- | Removes a PKCS#7 padding suffix from a byte string.
--
-- Returns 'Nothing' if the given byte string does not have a valid PKCS#7
-- padding suffix.
--
unpad :: ByteString -> Maybe ByteString
unpad paddedPayload =
    stripPadding =<< BS.unsnoc paddedPayload
  where
    stripPadding (_, lastByte)
        | BS.length paddedPayload `mod` 16 /= 0 =
            Nothing
        | paddingLength > 16 =
            Nothing
        | otherwise =
            stripSuffix padding paddedPayload
      where
        padding :: ByteString
        padding = B8.replicate paddingLength (toEnum paddingLength)

        paddingLength :: Int
        paddingLength = fromEnum lastByte
