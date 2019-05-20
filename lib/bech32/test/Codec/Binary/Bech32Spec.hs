{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Codec.Binary.Bech32Spec
    ( spec
    ) where

import Prelude

import Codec.Binary.Bech32.Internal
    ( HumanReadablePart, humanReadablePartToBytes, mkHumanReadablePart )
import Control.Monad
    ( forM_ )
import Data.Bits
    ( xor, (.&.) )
import Data.ByteString
    ( ByteString )
import Data.Char
    ( toLower, toUpper )
import Data.Either
    ( isLeft )
import Data.Either.Extra
    ( eitherToMaybe )
import Data.Functor.Identity
    ( runIdentity )
import Data.Maybe
    ( catMaybes, isJust )
import Data.Word
    ( Word8 )
import Test.Hspec
    ( Spec, describe, expectationFailure, it, shouldBe, shouldSatisfy )
import Test.QuickCheck
    ( Arbitrary (..)
    , Positive (..)
    , choose
    , elements
    , property
    , vectorOf
    , (.&&.)
    , (===)
    , (==>)
    )

import qualified Codec.Binary.Bech32.Internal as Bech32
import qualified Data.Array as Arr
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8

spec :: Spec
spec = do
    describe "Valid Checksums" $ forM_ validChecksums $ \checksum ->
        it (B8.unpack checksum) $ case Bech32.decode checksum of
            Left _ ->
                expectationFailure (show checksum)
            Right (resultHRP, resultData) -> do
                -- test that a corrupted checksum fails decoding.
                let (hrp, rest) = B8.breakEnd (== '1') checksum
                let Just (first, rest') = BS.uncons rest
                let checksumCorrupted =
                        (hrp `BS.snoc` (first `xor` 1)) `BS.append` rest'
                (Bech32.decode checksumCorrupted) `shouldSatisfy` isLeft
                -- test that re-encoding the decoded checksum results in
                -- the same checksum.
                let checksumEncoded = Bech32.encode resultHRP resultData
                let expectedChecksum = Right $ B8.map toLower checksum
                checksumEncoded `shouldBe` expectedChecksum

    describe "Invalid Checksums" $ forM_ invalidChecksums $ \checksum ->
        it (B8.unpack checksum) $
            Bech32.decode checksum `shouldSatisfy` isLeft

    describe "More Encoding/Decoding Cases" $ do
        it "length > maximum" $ do
            let hrpUnpacked = "ca"
            let hrpLength = length hrpUnpacked
            let (Right hrp) = mkHumanReadablePart (B8.pack hrpUnpacked)
            let maxDataLength =
                    Bech32.encodedStringMaxLength
                    - Bech32.checksumLength - Bech32.separatorLength - hrpLength
            Bech32.encode hrp (BS.pack (replicate (maxDataLength + 1) 1))
                `shouldBe` Left Bech32.EncodedStringTooLong

        it "hrp lowercased" $ do
            let (Right hrp) = mkHumanReadablePart (B8.pack "HRP")
            Bech32.encode hrp mempty
                `shouldBe` Right (B8.pack "hrp1g9xj8m")

    describe "Roundtrip (encode . decode)" $ do
        it "Can perform roundtrip for valid data" $ property $ \(hrp, bytes) ->
            (eitherToMaybe (Bech32.encode hrp bytes)
                >>= eitherToMaybe . Bech32.decode) === Just (hrp, bytes)

    describe "Roundtrip (toBase256 . toBase32)" $ do
        it "Can perform roundtrip base conversion" $ property $ \ws ->
            (Bech32.toBase256 . Bech32.toBase32) ws === Just ws

    describe "Roundtrip (toBase32 . toBase256)" $ do
        it "Can perform roundtrip base conversion" $ property $ \ws ->
            isJust (Bech32.toBase256 ws) ==>
                (Bech32.toBase32 <$> Bech32.toBase256 ws) === Just ws

    describe "Roundtrip (charToWord5 . word5ToChar)" $ do
        it "can perform roundtrip character set conversion (lower-case)" $
            property $ \w ->
                Bech32.charToWord5 (toLower (Bech32.word5ToChar Arr.! w))
                    === Just w

    describe "Roundtrip (charToWord5 . word5ToChar)" $ do
        it "can perform roundtrip character set conversion (upper-case)" $
            property $ \w ->
                Bech32.charToWord5 (toUpper (Bech32.word5ToChar Arr.! w))
                    === Just w

    describe "Conversion of word string from one word size to another" $ do

        it "With identical word sizes, conversion is the identity transform" $
            property $ \inputWordsUnmasked -> do
                size <- choose (1, 16)
                let mask (Positive w) = w .&. (2 ^ size - 1)
                let inputWords = mask <$> inputWordsUnmasked
                pure $ inputWords === runIdentity
                    (Bech32.convertBits inputWords size size Bech32.yesPadding)

        it "With different word sizes, roundtripping preserves data" $
            property $ \inputWordsUnmasked -> do
                sourceSize <- choose (1, 16)
                targetSize <- choose (1, 16)
                let mask size (Positive w) = w .&. (2 ^ size - 1)
                let inputWords = mask sourceSize <$> inputWordsUnmasked
                let convert s0 s1 inputData =
                        runIdentity $
                            Bech32.convertBits inputData s0 s1 Bech32.yesPadding
                let outputWords =
                        convert targetSize sourceSize $
                        convert sourceSize targetSize inputWords
                let outputWordsPrefix = take (length inputWords) outputWords
                let outputWordsSuffix = drop (length inputWords) outputWords
                pure $
                    (inputWords === outputWordsPrefix)
                    .&&.
                    (outputWordsSuffix `shouldSatisfy` all (== 0))

    describe "Pointless test to trigger coverage on derived instances" $ do
        it (show $ mkHumanReadablePart $ B8.pack "ca") True

validChecksums :: [ByteString]
validChecksums = map B8.pack
    [ "A12UEL5L"
    , "an83characterlonghumanreadablepartthatcontain\
      \sthenumber1andtheexcludedcharactersbio1tt5tgs"
    , "abcdef1qpzry9x8gf2tvdw0s3jn54khce6mua7lmqqqxw"
    , "11qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq\
      \qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqc8247j"
    , "split1checkupstagehandshakeupstreamerranterredcaperred2y9e3w"
    ]

invalidChecksums :: [ByteString]
invalidChecksums = map B8.pack
    [ " 1nwldj5"
    , "\DEL1axkwrx"
    , "an84characterslonghumanreadablepartthatcontain\
      \sthenumber1andtheexcludedcharactersbio1569pvx"
    , "pzry9x0s0muk"
    , "1pzry9x0s0muk"
    , "x1b4n0q5v"
    , "li1dgmt3"
    , "de1lg7wt\xFF"
    ]

instance Arbitrary HumanReadablePart where
    shrink hrp = catMaybes $ eitherToMaybe .
        mkHumanReadablePart <$> shrink (humanReadablePartToBytes hrp)
    arbitrary = do
        let range =
                ( Bech32.humanReadableCharsetMinBound
                , Bech32.humanReadableCharsetMaxBound )
        bytes <-
            choose (1, 10) >>= \n -> vectorOf n (choose range)
        let (Right hrp) = mkHumanReadablePart (B8.map toLower $ BS.pack bytes)
        return hrp

instance Arbitrary ByteString where
    shrink bytes | BS.null bytes = []
    shrink bytes =
        [ BS.take (BS.length bytes `div` 2) bytes
        , BS.drop 1 bytes
        ]
    arbitrary = do
        bytes <- choose (0, 10) >>= \n -> vectorOf n (elements Bech32.charset)
        return (B8.pack bytes)

instance Arbitrary Bech32.Word5 where
    arbitrary = Bech32.word5 @Word8 <$> arbitrary
    shrink w = Bech32.word5 <$> shrink (Bech32.getWord5 w)
