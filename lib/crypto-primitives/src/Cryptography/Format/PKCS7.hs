module Cryptography.Format.PKCS7
    ( pad
    , unpad
    )
    where

import Prelude

import Data.ByteString
    ( ByteString
    )

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8

-- | Apply PKCS#7 padding to payload and end up with a multiple of a block
-- size, i.e., 16 bytes, according to
-- https://datatracker.ietf.org/doc/html/rfc5652#section-6.3.
-- The padding value is the number of padding bytes.
-- If 1 byte of padding is required, the padding is "01".
-- If 2 bytes of padding, it's "02 02".
-- If no padding is required, an extra block of 0x10 bytes is added,
-- i.e., sixteen copies of 16, which is the blocksize.
-- This means that payload can only fit 15 bytes into a single block with
-- padding. A 16 byte payload requires 2 blocks with padding.
pad :: ByteString -> Maybe ByteString
pad payload
    | BS.null payload = Nothing
    | otherwise = Just $ BS.append payload padding
  where
    padding = B8.replicate paddingLength (toEnum paddingLength)
    paddingLength = 16 - (BS.length payload `mod` 16)

unpad :: ByteString -> Maybe ByteString
unpad payload =
    stripPadding <$> BS.unsnoc payload
  where
    stripPadding (_, lastByte) = BS.dropEnd paddingLength payload
      where
        paddingLength :: Int
        paddingLength = fromEnum lastByte
