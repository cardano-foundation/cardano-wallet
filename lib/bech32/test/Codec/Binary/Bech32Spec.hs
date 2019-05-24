{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Codec.Binary.Bech32Spec
    ( spec
    ) where

import Prelude

import Codec.Binary.Bech32.Internal
    ( CharPosition (..)
    , DataPart (..)
    , DecodingError (..)
    , HumanReadablePart
    , humanReadablePartToBytes
    , mkHumanReadablePart
    )
import Control.Monad
    ( forM_, replicateM )
import Data.Bits
    ( xor, (.&.) )
import Data.ByteString
    ( ByteString )
import Data.Char
    ( toLower, toUpper )
import Data.Either
    ( fromRight, isLeft, isRight )
import Data.Either.Extra
    ( eitherToMaybe )
import Data.Functor.Identity
    ( runIdentity )
import Data.Maybe
    ( catMaybes, fromMaybe, isJust )
import Data.Word
    ( Word8 )
import Test.Hspec
    ( Spec, describe, expectationFailure, it, shouldBe, shouldSatisfy )
import Test.QuickCheck
    ( Arbitrary (..)
    , Positive (..)
    , choose
    , counterexample
    , elements
    , property
    , vectorOf
    , (.&&.)
    , (.||.)
    , (===)
    , (==>)
    )
import Text.Read
    ( readEither )

import qualified Codec.Binary.Bech32.Internal as Bech32
import qualified Data.Array as Arr
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8

spec :: Spec
spec = do
    describe "Valid Reference Strings" $
        it "should always decode successfully" $
            forM_ validBech32Strings $ \s ->
                Bech32.decode s `shouldSatisfy` isRight

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

    describe "Invalid Checksums" $ forM_ invalidChecksums $ \(checksum, expect) ->
        it (B8.unpack checksum) $
            Bech32.decode checksum `shouldBe` (Left expect)

    describe "More Encoding/Decoding Cases" $ do
        it "length > maximum" $ do
            let hrpUnpacked = "ca"
            let hrpLength = length hrpUnpacked
            let (Right hrp) = mkHumanReadablePart (B8.pack hrpUnpacked)
            let maxDataLength =
                    Bech32.encodedStringMaxLength
                    - Bech32.checksumLength - Bech32.separatorLength - hrpLength
            Bech32.encode hrp
                (DataPart (replicate (maxDataLength + 1)
                    $ Bech32.word5 @Word8 1))
                `shouldBe` Left Bech32.EncodedStringTooLong

        it "hrp lowercased" $ do
            let (Right hrp) = mkHumanReadablePart (B8.pack "HRP")
            Bech32.encode hrp mempty
                `shouldBe` Right (B8.pack "hrp1g9xj8m")

    describe "Arbitrary ValidBech32String" $

        it "Generation always produces a valid string that can be decoded." $
            property $ \v ->
                Bech32.decode (getValidBech32String v) `shouldBe`
                    Right (humanReadablePart v, unencodedDataPart v)

    describe "Decoding a corrupted string should fail" $ do

        it "Decoding fails when an adjacent pair of characters is swapped." $
            property $ \s -> do
                let validString = getValidBech32String s
                index <- choose (0, BS.length validString - 2)
                let prefix = BS.take index validString
                let suffix = BS.drop (index + 2) validString
                let char0 = BS.singleton (BS.index validString index)
                let char1 = BS.singleton (BS.index validString $ index + 1)
                let recombinedString = prefix <> char1 <> char0 <> suffix
                return $
                    (BS.length recombinedString ===  BS.length validString)
                    .&&.
                    (Bech32.decode recombinedString `shouldSatisfy`
                        (if char0 == char1 then isRight else isLeft))

        it "Decoding fails when a character is omitted." $
            property $ \s -> do
                let validString = getValidBech32String s
                index <- choose (0, BS.length validString - 1)
                let prefix = BS.take index validString
                let suffix = BS.drop (index + 1) validString
                let recombinedString = prefix <> suffix
                return $
                    (BS.length recombinedString === BS.length validString - 1)
                    .&&.
                    (Bech32.decode recombinedString `shouldSatisfy` isLeft)

        it "Decoding fails when a character is inserted." $
            property $ \s c -> do
                let validString = getValidBech32String s
                let validChar = getValidBech32Char c
                index <- choose (0, BS.length validString - 1)
                let prefix = BS.take index validString
                let suffix = BS.drop index validString
                let recombinedString =
                        prefix <> B8.singleton validChar <> suffix
                return $
                    (BS.length recombinedString === BS.length validString + 1)
                    .&&.
                    (Bech32.decode recombinedString `shouldSatisfy` isLeft)

        it "Decoding fails when a single character is mutated." $
           property $ \s c -> do
                let validString = getValidBech32String s
                let validChar = getValidBech32Char c
                let separatorIndex = BS.length $
                        Bech32.humanReadablePartToBytes $ humanReadablePart s
                index <- choose (0, BS.length validString - 1)
                let prefix = BS.take index validString
                let suffix = BS.drop (index + 1) validString
                let recombinedString =
                        prefix <> B8.singleton validChar <> suffix
                return $
                    index /= separatorIndex ==>
                    recombinedString /= validString ==>
                    BS.length recombinedString == BS.length validString ==> (
                        -- error location detection is best effort:
                        (Bech32.decode recombinedString `shouldBe`
                            Left (StringToDecodeContainsInvalidChars
                                [CharPosition index]))
                         .||.
                        (Bech32.decode recombinedString `shouldBe`
                            Left (StringToDecodeContainsInvalidChars []))
                    )

        it "Decoding fails for an upper-case string with a lower-case \
           \character." $
            property $ \s -> do
                let validString = getValidBech32String s
                index <- choose (0, BS.length validString - 1)
                let prefix = B8.map toUpper $ BS.take index validString
                let suffix = B8.map toUpper $ BS.drop (index + 1) validString
                let char = B8.singleton $ toLower $ B8.index validString index
                let recombinedString = prefix <> char <> suffix
                return $ counterexample
                    (show validString <> " : " <> show recombinedString) $
                    (BS.length recombinedString === BS.length validString)
                    .&&.
                    (Bech32.decode recombinedString `shouldSatisfy`
                        (if B8.map toUpper validString == recombinedString
                            then isRight
                            else isLeft))

        it "Decoding fails for a lower-case string with an upper-case \
           \character." $
            property $ \s -> do
                let validString = getValidBech32String s
                index <- choose (0, BS.length validString - 1)
                let prefix = B8.map toLower $ BS.take index validString
                let suffix = B8.map toLower $ BS.drop (index + 1) validString
                let char = B8.singleton $ toUpper $ B8.index validString index
                let recombinedString = prefix <> char <> suffix
                return $ counterexample
                    (show validString <> " : " <> show recombinedString) $
                    (BS.length recombinedString === BS.length validString)
                    .&&.
                    (Bech32.decode recombinedString `shouldSatisfy`
                        (if B8.map toLower validString == recombinedString
                            then isRight
                            else isLeft))

    describe "Roundtrip (encode . decode)" $ do
        it "Can perform roundtrip for valid data" $ property $ \(hrp, dp) ->
            (eitherToMaybe (Bech32.encode hrp dp)
                >>= eitherToMaybe . Bech32.decode) === Just (hrp, dp)

    describe "Roundtrip (read . show)" $ do
        it "For DataPart" $ property $ \(dp :: DataPart) ->
            readEither (show dp) === Right dp

    describe "Roundtrip (dataPartToBytes . dataPartFromBytes)" $ do
        it "Can perform roundtrip base conversion" $ property $ \bs ->
            (Bech32.dataPartToBytes . Bech32.dataPartFromBytes) bs === Just bs

    describe "Roundtrip (dataPartFromText . dataPartToText)" $ do
        it "Can perform roundtrip conversion" $ property $ \dp ->
            (Bech32.dataPartFromText . Bech32.dataPartToText) dp === Just dp

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

-- Taken from the BIP 0173 specification: https://git.io/fjBIN
validBech32Strings :: [ByteString]
validBech32Strings = map B8.pack
    [ "A12UEL5L"
    , "a12uel5l"
    , "an83characterlonghumanreadablepartthatcontainsthenumber1andtheexcluded\
      \charactersbio1tt5tgs"
    , "abcdef1qpzry9x8gf2tvdw0s3jn54khce6mua7lmqqqxw"
    , "11qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq\
      \qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqc8247j"
    , "split1checkupstagehandshakeupstreamerranterredcaperred2y9e3w"
    , "?1ezyfcl"
    , "BC1SW50QA3JX3S"
    , "bc1zw508d6qejxtdg4y5r3zarvaryvg6kdaj"
    , "bc1qw508d6qejxtdg4y5r3zarvary0c5xw7kv8f3t4"
    , "BC1QW508D6QEJXTDG4Y5R3ZARVARY0C5XW7KV8F3T4"
    , "tb1qrp33g0q5c5txsp9arysrx4k6zdkfs4nce4xj0gdcccefvpysxf3q0sl5k7"
    , "tb1qqqqqp399et2xygdj5xreqhjjvcmzhxw4aywxecjdzew6hylgvsesrxh6hy"
    , "bc1pw508d6qejxtdg4y5r3zarvary0c5xw7kw508d6qejxtdg4y5r3zarvary0\
      \c5xw7k7grplx"
    ]

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

invalidChecksums :: [(ByteString, Bech32.DecodingError)]
invalidChecksums =
    [ ( " 1nwldj5"
      , Bech32.StringToDecodeContainsInvalidChars [Bech32.CharPosition 0] )
    , ( "\DEL1axkwrx"
      , Bech32.StringToDecodeContainsInvalidChars [Bech32.CharPosition 0] )
    , ( "an84characterslonghumanreadablepartthatcontain\
        \sthenumber1andtheexcludedcharactersbio1569pvx"
      , Bech32.StringToDecodeTooLong)
    , ( "pzry9x0s0muk", Bech32.StringToDecodeMissingSeparatorChar )
    , ( "1pzry9x0s0muk"
      , Bech32.StringToDecodeContainsInvalidChars [Bech32.CharPosition 0] )
    , ( "x1b4n0q5v"
      , Bech32.StringToDecodeContainsInvalidChars [Bech32.CharPosition 2] )
    , ( "x1n4n0q5v"
      , Bech32.StringToDecodeContainsInvalidChars [] )
    , ( "11111111111111111111111111111111111111111111111111111111111111\
        \1111111111111111111111111111"
       , Bech32.StringToDecodeContainsInvalidChars [Bech32.CharPosition 83] )
    , ( "li1dgmt3"
      , Bech32.StringToDecodeTooShort )
    , ( "", Bech32.StringToDecodeTooShort )
    , ( "de1lg7wt\xFF"
      , Bech32.StringToDecodeContainsInvalidChars [Bech32.CharPosition 8] )
    , ( "aBcdef1qpzry9x8gf2tvDw0s3jn54khce6mua7lmqqqXw"
      , Bech32.StringToDecodeHasMixedCase )
    ]

newtype ValidBech32Char = ValidBech32Char
    { getValidBech32Char :: Char
    } deriving (Eq, Ord, Show)

instance Arbitrary ValidBech32Char where
    arbitrary = ValidBech32Char <$> elements Bech32.charset
    shrink (ValidBech32Char c) =
        ValidBech32Char . (Bech32.word5ToChar Arr.!) <$> shrink
            (fromMaybe
                (error "unable to shrink a Bech32 character.")
                (Bech32.charToWord5 c))

data ValidBech32String = ValidBech32String
    { getValidBech32String :: ByteString
    , humanReadablePart :: HumanReadablePart
    , unencodedDataPart :: DataPart
    } deriving (Eq, Show)

mkValidBech32String :: HumanReadablePart -> DataPart -> ValidBech32String
mkValidBech32String hrp udp =
    ValidBech32String
        (fromRight (error "unable to make a valid Bech32 string.") $
            Bech32.encode hrp udp)
        hrp udp

instance Arbitrary ValidBech32String where
    arbitrary = mkValidBech32String <$> arbitrary <*> arbitrary
    shrink v = do
        let hrpOriginal = humanReadablePart v
        let udpOriginal = unencodedDataPart v
        hrpShrunk <- take 3 $ shrink $ humanReadablePart v
        udpShrunk <- take 3 $ shrink $ unencodedDataPart v
        uncurry mkValidBech32String <$>
            [ (hrpShrunk, udpShrunk)
            , (hrpShrunk, udpOriginal)
            , (hrpOriginal, udpShrunk) ]

instance Arbitrary DataPart where
    arbitrary = do
        len <- choose (0, 64)
        DataPart <$> replicateM len arbitrary
    shrink (DataPart dp)
        | null dp   = []
        | otherwise = DataPart <$>
            [ take (length dp `div` 2) dp
            , drop 1 dp
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
        bytes <- choose (0, 32) >>= \n -> vectorOf n (elements Bech32.charset)
        return (B8.pack bytes)

instance Arbitrary Bech32.Word5 where
    arbitrary = Bech32.word5 @Word8 <$> arbitrary
    shrink w = Bech32.word5 <$> shrink (Bech32.getWord5 w)
