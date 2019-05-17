{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright: © 2017 Marko Bencun, 2018-2019 IOHK
-- License: MIT
--
-- Implementation of the [Bech32]
-- (https://github.com/bitcoin/bips/blob/master/bip-0173.mediawiki)
-- address format.
--
-- From an original implementation by Marko Bencun:
--
-- [sipa/bech32](https://github.com/sipa/bech32/tree/bdc264f84014c234e908d72026b7b780122be11f/ref/haskell)

module Codec.Binary.Bech32.Internal
    (
      -- * Encoding & Decoding
      encode
    , EncodingError (..)
    , decode
    , DecodingError (..)
    , checksumLength
    , encodedStringMaxLength
    , encodedStringMinLength
    , separatorLength

      -- * Human-Readable Parts
    , HumanReadablePart
    , HumanReadablePartError (..)
    , mkHumanReadablePart
    , humanReadablePartToBytes
    , humanReadableCharsetMinBound
    , humanReadableCharsetMaxBound
    , humanReadablePartMinLength
    , humanReadablePartMaxLength

      -- * Bit Manipulation
    , convertBits
    , Word5
    , word5
    , getWord5
    , toBase256
    , toBase32
    , noPadding
    , yesPadding

      -- * Character Set Manipulation
    , charset
    , charsetMap

    ) where

import Prelude

import Control.Monad
    ( guard )
import Data.Array
    ( Array )
import Data.Bifunctor
    ( first )
import Data.Bits
    ( Bits, testBit, unsafeShiftL, unsafeShiftR, xor, (.&.), (.|.) )
import Data.ByteString
    ( ByteString )
import Data.Char
    ( ord, toLower, toUpper )
import Data.Either.Extra
    ( maybeToEither )
import Data.Foldable
    ( foldl' )
import Data.Functor.Identity
    ( Identity, runIdentity )
import Data.Ix
    ( Ix (..) )
import Data.Maybe
    ( isJust )
import Data.Word
    ( Word8 )

import qualified Data.Array as Arr
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8

{-------------------------------------------------------------------------------
                          Human Readable Parts
-------------------------------------------------------------------------------}

newtype HumanReadablePart = HumanReadablePart ByteString
    deriving (Show, Eq)

mkHumanReadablePart
    :: ByteString -> Either HumanReadablePartError HumanReadablePart
mkHumanReadablePart hrp
    | BS.length hrp < humanReadablePartMinLength =
        Left HumanReadablePartTooShort
    | BS.length hrp > humanReadablePartMaxLength =
        Left HumanReadablePartTooLong
    | BS.length invalidPortion > 0 =
        Left $ HumanReadablePartContainsInvalidChar $ CharPosition $
            BS.length validPortion
    | otherwise =
        Right $ HumanReadablePart hrp
  where
    (validPortion, invalidPortion) = BS.break (not . valid) hrp
    valid c =
        c >= humanReadableCharsetMinBound &&
        c <= humanReadableCharsetMaxBound

data HumanReadablePartError
    = HumanReadablePartTooShort
    | HumanReadablePartTooLong
    | HumanReadablePartContainsInvalidChar CharPosition
    deriving (Eq, Show)

humanReadableCharsetMinBound :: Word8
humanReadableCharsetMinBound = 33

humanReadableCharsetMaxBound :: Word8
humanReadableCharsetMaxBound = 126

humanReadablePartToBytes :: HumanReadablePart -> ByteString
humanReadablePartToBytes (HumanReadablePart bytes) = bytes

humanReadablePartMinLength :: Int
humanReadablePartMinLength = 1

humanReadablePartMaxLength :: Int
humanReadablePartMaxLength = 83

{-------------------------------------------------------------------------------
                            Encoding & Decoding
-------------------------------------------------------------------------------}

encode :: HumanReadablePart -> ByteString -> Either EncodingError ByteString
encode hrp@(HumanReadablePart hrpBytes) payload = do
    let payload5 = toBase32 (BS.unpack payload)
    let payload' = payload5 ++ bech32CreateChecksum hrp payload5
    let rest = map (charset Arr.!) payload'
    let output = B8.map toLower hrpBytes <> B8.pack "1" <> B8.pack rest
    guardE (BS.length output <= encodedStringMaxLength) EncodedStringTooLong
    return output

data EncodingError = EncodedStringTooLong
    deriving (Eq, Show)

decode :: ByteString -> Either DecodingError (HumanReadablePart, ByteString)
decode bech32 = do
    (hrpUnparsed , dcpUnparsed) <-
        maybeToEither StringToDecodeMissingSeparatorChar $
            splitAtLastOccurrence separatorChar $ B8.map toLower bech32
    hrp <- first hrpError $ mkHumanReadablePart hrpUnparsed
    dcp <- first
        (\(CharPosition p) -> StringToDecodeContainsInvalidChar $
            CharPosition $ p + BS.length hrpUnparsed + separatorLength)
        (parseDataWithChecksumPart $ B8.unpack dcpUnparsed)
    guardE (BS.length bech32 <= encodedStringMaxLength)
        StringToDecodeTooLong
    guardE (BS.length bech32 >= encodedStringMinLength)
        StringToDecodeTooShort
    guardE (B8.map toUpper bech32 == bech32 || B8.map toLower bech32 == bech32)
        StringToDecodeHasMixedCase
    guardE (length dcp >= checksumLength)
        StringToDecodeTooShort
    guardE (bech32VerifyChecksum hrp dcp)
        StringToDecodeContainsInvalidChars
    dp <- maybeToEither StringToDecodeContainsInvalidChars $
        toBase256 (take (length dcp - checksumLength) dcp)
    return (hrp, BS.pack dp)
  where
    parseDataWithChecksumPart :: String -> Either CharPosition [Word5]
    parseDataWithChecksumPart dcpUnparsed =
        case mapM charsetMap dcpUnparsed of
            Just dp -> pure dp
            Nothing -> Left $ CharPosition $ length $
                takeWhile isJust (charsetMap <$> dcpUnparsed)
    hrpError = \case
        HumanReadablePartTooLong ->
            StringToDecodeContainsInvalidChar $ CharPosition
                humanReadablePartMaxLength
        HumanReadablePartTooShort ->
            StringToDecodeContainsInvalidChar $ CharPosition $
                humanReadablePartMinLength - 1
        HumanReadablePartContainsInvalidChar p ->
            StringToDecodeContainsInvalidChar p

data DecodingError
    = StringToDecodeTooLong
    | StringToDecodeTooShort
    | StringToDecodeHasMixedCase
    | StringToDecodeMissingSeparatorChar
    | StringToDecodeContainsInvalidChar CharPosition
    | StringToDecodeContainsInvalidChars
    deriving (Eq, Show)

-- | The separator character. This character appears immediately after the
-- human-readable part and before the data part.
separatorChar :: Word8
separatorChar = fromIntegral $ ord '1'

-- | The length of the checksum portion of an encoded string, in bytes.
checksumLength :: Int
checksumLength = 6

-- | The length of the separator portion of an encoded string, in bytes.
separatorLength :: Int
separatorLength = 1

-- | The maximum length of an encoded string, in bytes. This length includes the
--   human-readable part, the separator character, the encoded data portion,
--   and the checksum.
encodedStringMaxLength :: Int
encodedStringMaxLength = 90

-- | The minimum length of an encoded string, in bytes. This length includes the
--   human-readable part, the separator character, the encoded data portion,
--   and the checksum.
encodedStringMinLength :: Int
encodedStringMinLength =
    humanReadablePartMinLength + separatorLength + checksumLength

{-------------------------------------------------------------------------------
                            Character Manipulation
-------------------------------------------------------------------------------}

charset :: Array Word5 Char
charset =
    Arr.listArray
        (Word5 0, Word5 31)
        "qpzry9x8gf2tvdw0s3jn54khce6mua7l"

charsetMap :: Char -> Maybe Word5
charsetMap c
    | inRange (Arr.bounds inv) upperC = inv Arr.! upperC
    | otherwise = Nothing
  where
    upperC = toUpper c
    swap (a, b) = (toUpper b, Just a)
    inv =
        Arr.listArray ('0', 'Z') (repeat Nothing)
        Arr.//
        (map swap (Arr.assocs charset))

-- | The zero-based position of a character in a string, counting from the left.
newtype CharPosition = CharPosition Int
    deriving (Eq, Show)

{-------------------------------------------------------------------------------
                              Bit Manipulation
-------------------------------------------------------------------------------}

(.>>.), (.<<.) :: Bits a => a -> Int -> a
(.>>.) = unsafeShiftR
(.<<.) = unsafeShiftL

newtype Word5 = Word5 { getWord5 :: Word8 }
    deriving (Eq, Ord, Show)

instance Ix Word5 where
    range (Word5 m, Word5 n) = map Word5 $ range (m, n)
    index (Word5 m, Word5 n) (Word5 i) = index (m, n) i
    inRange (m,n) i = m <= i && i <= n

word5 :: Integral a => a -> Word5
word5 x = Word5 ((fromIntegral x) .&. 31)
{-# INLINE word5 #-}
{-# SPECIALIZE INLINE word5 :: Word8 -> Word5 #-}

fromWord5 :: Integral a => Word5 -> a
fromWord5 (Word5 x) = fromIntegral x
{-# INLINE fromWord5 #-}
{-# SPECIALIZE INLINE fromWord5 :: Word5 -> Word8 #-}

bech32Polymod :: [Word5] -> Word
bech32Polymod values = foldl' go 1 values .&. 0x3fffffff
  where
    go chk value =
        foldl' xor chk' [g | (g, i) <- zip generator [25 ..], testBit chk i]
      where
        chk' = chk .<<. 5 `xor` (fromWord5 value)
        generator =
            [ 0x3b6a57b2
            , 0x26508e6d
            , 0x1ea119fa
            , 0x3d4233dd
            , 0x2a1462b3 ]

bech32HRPExpand :: HumanReadablePart -> [Word5]
bech32HRPExpand (HumanReadablePart hrp) =
    map (Word5 . (.>>. 5)) (BS.unpack hrp)
    ++ [Word5 0]
    ++ map word5 (BS.unpack hrp)

bech32CreateChecksum :: HumanReadablePart -> [Word5] -> [Word5]
bech32CreateChecksum hrp dat = [word5 (polymod .>>. i) | i <- [25, 20 .. 0]]
  where
    values = bech32HRPExpand hrp ++ dat
    polymod =
        bech32Polymod (values ++ map Word5 [0, 0, 0, 0, 0, 0]) `xor` 1

bech32VerifyChecksum :: HumanReadablePart -> [Word5] -> Bool
bech32VerifyChecksum hrp dat = bech32Polymod (bech32HRPExpand hrp ++ dat) == 1

type Pad f = Int -> Int -> Word -> [[Word]] -> f [[Word]]

yesPadding :: Pad Identity
yesPadding _ 0 _ result = return result
yesPadding _ _ padValue result = return $ [padValue] : result
{-# INLINE yesPadding #-}

noPadding :: Pad Maybe
noPadding frombits bits padValue result = do
    guard $ bits < frombits && padValue == 0
    return result
{-# INLINE noPadding #-}

-- Big-endian conversion of a word string from base 2^frombits to base 2^tobits.
-- The frombits and twobits parameters must be positive, while 2^frombits and
-- 2^tobits must be smaller than the size of Word. Every value in dat must be
-- strictly smaller than 2^frombits.
convertBits :: Functor f => [Word] -> Int -> Int -> Pad f -> f [Word]
convertBits dat frombits tobits pad = concat . reverse <$> go dat 0 0 []
  where
    go [] acc bits result =
        let padValue = (acc .<<. (tobits - bits)) .&. maxv
        in pad frombits bits padValue result
    go (value:dat') acc bits result =
        go dat' acc' (bits' `rem` tobits) (result' : result)
      where
        acc' = (acc .<<. frombits) .|. fromIntegral value
        bits' = bits + frombits
        result' =
            [ (acc' .>>. b) .&. maxv
            | b <- [bits' - tobits, bits' - 2 * tobits .. 0] ]
    maxv = (1 .<<. tobits) - 1
{-# INLINE convertBits #-}

toBase32 :: [Word8] -> [Word5]
toBase32 dat =
    map word5 $ runIdentity $ convertBits (map fromIntegral dat) 8 5 yesPadding

toBase256 :: [Word5] -> Maybe [Word8]
toBase256 dat =
    map fromIntegral <$> convertBits (map fromWord5 dat) 5 8 noPadding

{-------------------------------------------------------------------------------
                                   Utilities
-------------------------------------------------------------------------------}

guardE :: Bool -> e -> Either e ()
guardE b e = if b then Right () else Left e

-- | Splits the given 'ByteString' into a prefix and a suffix using the last
-- occurrence of the specified separator character as a splitting point.
-- Evaluates to 'Nothing' if the 'ByteString` does not contain the separator
-- character.
splitAtLastOccurrence :: Word8 -> ByteString -> Maybe (ByteString, ByteString)
splitAtLastOccurrence w s =
    (\i -> (BS.take i s, BS.drop (i + 1) s)) <$> BS.elemIndexEnd w s
