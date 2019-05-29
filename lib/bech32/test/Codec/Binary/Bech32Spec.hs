{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
    , DataPart
    , DecodingError (..)
    , HumanReadablePart
    , dataPartFromBytes
    , dataPartFromText
    , dataPartFromWords
    , dataPartIsValid
    , dataPartToWords
    , humanReadableCharMaxBound
    , humanReadableCharMinBound
    , humanReadablePartFromText
    , humanReadablePartToText
    , separatorChar
    )
import Control.Monad
    ( forM_, replicateM )
import Data.Bits
    ( xor, (.&.) )
import Data.ByteString
    ( ByteString )
import Data.Char
    ( chr, isUpper, ord, toLower, toUpper )
import Data.Either
    ( fromRight, isLeft, isRight )
import Data.Either.Extra
    ( eitherToMaybe )
import Data.Functor.Identity
    ( runIdentity )
import Data.List
    ( intercalate )
import Data.Maybe
    ( catMaybes, fromMaybe, isJust )
import Data.Set
    ( Set )
import Data.Text
    ( Text )
import Data.Vector
    ( Vector )
import Data.Word
    ( Word8 )
import Test.Hspec
    ( Spec, describe, expectationFailure, it, shouldBe, shouldSatisfy )
import Test.QuickCheck
    ( Arbitrary (..)
    , Positive (..)
    , arbitraryBoundedEnum
    , choose
    , counterexample
    , cover
    , elements
    , property
    , withMaxSuccess
    , (.&&.)
    , (.||.)
    , (===)
    , (==>)
    )

import qualified Codec.Binary.Bech32.Internal as Bech32
import qualified Data.ByteString as BS
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Vector as V

spec :: Spec
spec = do
    describe "Valid Reference Strings" $
        it "should always decode successfully" $
            forM_ validBech32Strings $ \s ->
                Bech32.decode s `shouldSatisfy` isRight

    describe "Valid Checksums" $ forM_ validChecksums $ \checksum ->
        it (T.unpack checksum) $ case Bech32.decode checksum of
            Left _ ->
                expectationFailure (show checksum)
            Right (resultHRP, resultData) -> do
                -- test that a corrupted checksum fails decoding.
                let (hrp, rest) =
                        T.breakOnEnd (T.singleton separatorChar) checksum
                let Just (first, rest') = T.uncons rest
                let checksumCorrupted =
                        (hrp `T.snoc` (chr (ord first `xor` 1)))
                        `T.append` rest'
                (Bech32.decode checksumCorrupted) `shouldSatisfy` isLeft
                -- test that re-encoding the decoded checksum results in
                -- the same checksum.
                let checksumEncoded = Bech32.encode resultHRP resultData
                let expectedChecksum = Right $ T.map toLower checksum
                checksumEncoded `shouldBe` expectedChecksum

    describe "Invalid Checksums" $ forM_ invalidChecksums $
        \(checksum, expect) ->
            it (T.unpack checksum) $
                Bech32.decode checksum `shouldBe` (Left expect)

    describe "More Encoding/Decoding Cases" $ do
        it "length > maximum" $ do
            let hrpUnpacked = "ca"
            let hrpLength = length hrpUnpacked
            let (Right hrp) = humanReadablePartFromText (T.pack hrpUnpacked)
            let maxDataLength =
                    Bech32.encodedStringMaxLength
                    - Bech32.checksumLength - Bech32.separatorLength - hrpLength
            Bech32.encode hrp
                (dataPartFromWords (replicate (maxDataLength + 1)
                    $ Bech32.word5 @Word8 1))
                `shouldBe` Left Bech32.EncodedStringTooLong

        it "hrp lowercased" $ do
            let (Right hrp) = humanReadablePartFromText "HRP"
            Bech32.encode hrp mempty `shouldBe` Right "hrp1vhqs52"

    describe "Arbitrary ValidBech32String" $

        it "Generation always produces a valid string that can be decoded." $
            property $ \v ->
                Bech32.decode (getValidBech32String v) `shouldBe`
                    Right (humanReadablePart v, unencodedDataPart v)

    describe "Arbitrary Bech32Char" $ do

        it "Generation always produces a valid character." $
            property $ withMaxSuccess 10000 $ \c ->
                let char = getBech32Char c in
                cover 30 (      isDataChar char) "is a data character: TRUE"  $
                cover 30 (not $ isDataChar char) "is a data character: FALSE" $
                isBech32Char char

        it "Shrinking always produces valid characters." $
            property $ withMaxSuccess 10000 $ \c ->
                all (isBech32Char . getBech32Char) $ shrink c

        it "Shrinking always produces characters with codes that are smaller." $
            property $ withMaxSuccess 10000 $ \c ->
                all (< c) $ shrink (c :: Bech32Char)

    describe "Decoding a corrupted string should fail" $ do
        let chooseWithinDataPart originalString = do
                let sepIx = maybe
                        (error "couldn't find separator in valid bech32 string")
                        (\ix -> T.length originalString - ix - 1)
                        (T.findIndex (== '1') (T.reverse originalString))
                choose (sepIx + 1, T.length originalString - 2)

        it "Decoding fails when an adjacent pair of characters is swapped." $
            property $ withMaxSuccess 10000 $ \s -> do
                let originalString = getValidBech32String s
                index <- choose (0, T.length originalString - 2)
                let prefix = T.take index originalString
                let suffix = T.drop (index + 2) originalString
                let char1 = T.singleton (T.index originalString index)
                let char2 = T.singleton (T.index originalString $ index + 1)
                let corruptedString = prefix <> char2 <> char1 <> suffix
                let description = intercalate "\n"
                        [ "index of char #1: " <> show index
                        , "index of char #2: " <> show (index + 1)
                        , "         char #1: " <> show char1
                        , "         char #2: " <> show char2
                        , " original string: " <> show originalString
                        , "corrupted string: " <> show corruptedString ]
                return $ counterexample description $
                    char1 /= char2 ==>
                        (T.length corruptedString === T.length originalString)
                        .&&.
                        (Bech32.decode corruptedString `shouldSatisfy` isLeft)

        it "Decoding fails when a character is omitted." $
            property $ withMaxSuccess 10000 $ \s -> do
                let originalString = getValidBech32String s
                index <- choose (0, T.length originalString - 1)
                let char = T.index originalString index
                let prefix = T.take index originalString
                let suffix = T.drop (index + 1) originalString
                let corruptedString = prefix <> suffix
                let description = intercalate "\n"
                        [ "index of omitted char: " <> show index
                        , "         omitted char: " <> show char
                        , "      original string: " <> show originalString
                        , "     corrupted string: " <> show corruptedString ]
                return $ counterexample description $
                    (T.length corruptedString === T.length originalString - 1)
                    .&&.
                    (Bech32.decode corruptedString `shouldSatisfy` isLeft)
                    .||.
                    -- In the case where the tail of a valid Bech32 string is
                    -- composed of one or more consecutive 'q' characters
                    -- followed by a single 'p' character, omitting any or all
                    -- of the 'q' characters will still result in a valid
                    -- Bech32 string:
                    (T.length suffix > 0
                        && T.last suffix == 'p'
                        && char == 'q'
                        && T.all (== 'q') (T.dropEnd 1 suffix))

        it "Decoding fails when a character is inserted." $
            property $ withMaxSuccess 10000 $ \s c -> do
                let originalString = getValidBech32String s
                let char = getBech32Char c
                index <- choose (0, T.length originalString)
                let prefix = T.take index originalString
                let suffix = T.drop index originalString
                let corruptedString = prefix <> T.singleton char <> suffix
                let description = intercalate "\n"
                        [ "index of inserted char: " <> show index
                        , "         inserted char: " <> show char
                        , "       original string: " <> show originalString
                        , "      corrupted string: " <> show corruptedString ]
                return $
                    counterexample description $
                    cover 2 (T.null prefix)
                        "inserted before the start" $
                    cover 2 (T.null suffix)
                        "inserted after the end" $
                    cover 10 (not (T.null prefix) && not (T.null suffix))
                        "inserted into the middle" $
                    (T.length corruptedString === T.length originalString + 1)
                    .&&.
                    (Bech32.decode corruptedString `shouldSatisfy` isLeft)
                    .||.
                    -- In the case where the last character of a valid Bech32
                    -- string is the character 'p', inserting any number of
                    -- consecutive 'q' characters immediately before the 'p'
                    -- will still result in a valid Bech32 string.
                    (T.length suffix > 0
                        && T.last suffix == 'p'
                        && char == 'q'
                        && T.all (== 'q') (T.dropEnd 1 suffix))

        it "Decoding fails when a single character is mutated." $
           withMaxSuccess 10000 $ property $ \s c -> do
                let originalString = getValidBech32String s
                index <- choose (0, T.length originalString - 1)
                let originalChar = T.index originalString index
                let replacementChar = getBech32Char c
                let prefix = T.take index originalString
                let suffix = T.drop (index + 1) originalString
                let corruptedString =
                        prefix <> T.singleton replacementChar <> suffix
                let description = intercalate "\n"
                        [ "index of mutated char: " <> show index
                        , "        original char: " <> show originalChar
                        , "     replacement char: " <> show replacementChar
                        , "      original string: " <> show originalString
                        , "     corrupted string: " <> show corruptedString ]
                let result = Bech32.decode corruptedString
                return $ counterexample description $
                    corruptedString /= originalString ==>
                        (T.length corruptedString === T.length originalString)
                        .&&.
                        (result `shouldSatisfy` isLeft)

        it "Decoding fails for an upper-case string with a lower-case \
           \character." $
            withMaxSuccess 10000 $ property $ \s -> do
                let originalString = T.map toUpper $ getValidBech32String s
                index <- choose (0, T.length originalString - 1)
                let prefix = T.take index originalString
                let suffix = T.drop (index + 1) originalString
                let char = toLower $ T.index originalString index
                let corruptedString = prefix <> T.singleton char <> suffix
                let description = intercalate "\n"
                        [ "index of mutated char: " <> show index
                        , "      original string: " <> show originalString
                        , "     corrupted string: " <> show corruptedString ]
                return $ counterexample description $
                    corruptedString /= originalString ==>
                        (T.length corruptedString === T.length originalString)
                        .&&.
                        (Bech32.decode corruptedString `shouldBe` Left
                            StringToDecodeHasMixedCase)

        it "Decoding fails for a lower-case string with an upper-case \
           \character." $
            withMaxSuccess 10000 $ property $ \s -> do
                let originalString = T.map toLower $ getValidBech32String s
                index <- choose (0, T.length originalString - 1)
                let prefix = T.take index originalString
                let suffix = T.drop (index + 1) originalString
                let char = toUpper $ T.index originalString index
                let corruptedString = prefix <> T.singleton char <> suffix
                let description = intercalate "\n"
                        [ "index of mutated char: " <> show index
                        , "      original string: " <> show originalString
                        , "     corrupted string: " <> show corruptedString ]
                return $ counterexample description $
                    corruptedString /= originalString ==>
                        (T.length corruptedString === T.length originalString)
                        .&&.
                        (Bech32.decode corruptedString `shouldBe` Left
                            StringToDecodeHasMixedCase)

    describe "Roundtrip (encode . decode)" $ do
        it "Can perform roundtrip for valid data" $ property $ \(hrp, dp) ->
            (eitherToMaybe (Bech32.encode hrp dp)
                >>= eitherToMaybe . Bech32.decode) === Just (hrp, dp)

    describe "Roundtrip (dataPartToBytes . dataPartFromBytes)" $ do
        it "Can perform roundtrip base conversion" $ property $ \bs ->
            (Bech32.dataPartToBytes . Bech32.dataPartFromBytes) bs === Just bs

    describe "Roundtrip (dataPartFromText . dataPartToText)" $ do
        it "Can perform roundtrip conversion" $ property $ \dp ->
            (Bech32.dataPartFromText . Bech32.dataPartToText) dp === Just dp

    describe "Roundtrip (dataPartFromWords . dataPartToWords)" $ do
        it "Can perform roundtrip conversion" $ property $ \dp ->
            (Bech32.dataPartFromWords . Bech32.dataPartToWords) dp === dp

    describe "Roundtrip (dataPartToWords . dataPartFromWords)" $ do
        it "Can perform roundtrip conversion" $ property $ \ws ->
            (Bech32.dataPartToWords . Bech32.dataPartFromWords) ws === ws

    describe "Roundtrip (humanReadablePartFromText . humanReadablePartToText)" $
        it "Can perform roundtrip conversion" $ property $ \hrp ->
            (Bech32.humanReadablePartFromText . Bech32.humanReadablePartToText)
                hrp === Right hrp

    describe "Roundtrip (toBase256 . toBase32)" $ do
        it "Can perform roundtrip base conversion" $ property $ \ws ->
            (Bech32.toBase256 . Bech32.toBase32) ws === Just ws

    describe "Roundtrip (toBase32 . toBase256)" $ do
        it "Can perform roundtrip base conversion" $ property $ \ws ->
            isJust (Bech32.toBase256 ws) ==>
                (Bech32.toBase32 <$> Bech32.toBase256 ws) === Just ws

    describe "Roundtrip (dataCharToWord . dataCharFromWord)" $ do
        it "can perform roundtrip character set conversion" $
            property $ \w ->
                Bech32.dataCharToWord (toLower (Bech32.dataCharFromWord w))
                    === Just w

    describe "Constructors produce valid values" $ do

        it "dataPartFromBytes" $
            property $ \bytes -> do
                let value = dataPartFromBytes bytes
                let counterexampleText = mconcat
                        [ "input:  ", show bytes, "\n"
                        , "output: ", show value, "\n" ]
                counterexample counterexampleText $
                    dataPartIsValid value

        it "dataPartFromText" $
            property $ \chars -> do
                let value = dataPartFromText (T.pack $ getDataChar <$> chars)
                let counterexampleText = mconcat
                        [ "input:  ", show chars, "\n"
                        , "output: ", show value, "\n" ]
                counterexample counterexampleText $
                    fmap dataPartIsValid value === Just True

        it "dataPartFromWords" $
            property $ \ws -> do
                let value = dataPartFromWords ws
                let counterexampleText = mconcat
                        [ "input:  ", show ws   , "\n"
                        , "output: ", show value, "\n" ]
                counterexample counterexampleText $
                    dataPartIsValid value

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
        it (show $ humanReadablePartFromText $ T.pack "ca") True

-- Taken from the BIP 0173 specification: https://git.io/fjBIN
validBech32Strings :: [Text]
validBech32Strings =
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
    , "tc1qw508d6qejxtdg4y5r3zarvary0c5xw7kg3g4ty"
    , "BC13W508D6QEJXTDG4Y5R3ZARVARY0C5XW7KN40WF2"
    , "bc1rw5uspcuh"
    , "bc10w508d6qejxtdg4y5r3zarvary0c5xw7kw508d6qejxtdg4y5r3zarvary0c5xw7kw5rljs90"
    , "BC1QR508D6QEJXTDG4Y5R3ZARVARYV98GJ9P"
    , "bc1zw508d6qejxtdg4y5r3zarvaryvqyzf3du"
    , "tb1qrp33g0q5c5txsp9arysrx4k6zdkfs4nce4xj0gdcccefvpysxf3pjxtptv"
    , "bc1gmk9yu"
    ]

validChecksums :: [Text]
validChecksums =
    [ "A12UEL5L"
    , "an83characterlonghumanreadablepartthatcontain\
      \sthenumber1andtheexcludedcharactersbio1tt5tgs"
    , "abcdef1qpzry9x8gf2tvdw0s3jn54khce6mua7lmqqqxw"
    , "11qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq\
      \qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqc8247j"
    , "split1checkupstagehandshakeupstreamerranterredcaperred2y9e3w"
    ]

invalidChecksums :: [(Text, Bech32.DecodingError)]
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
    , ( "A1G7SGD8", Bech32.StringToDecodeContainsInvalidChars [] )
    , ( "10a06t8", Bech32.StringToDecodeTooShort )
    , ( "1qzzfhee", Bech32.StringToDecodeContainsInvalidChars [CharPosition 0] )
    , ( "tb1qrp33g0q5c5txsp9arysrx4k6zdkfs4nce4xj0gdcccefvpysxf3q0sL5k7"
      , Bech32.StringToDecodeHasMixedCase )
    , ( "bc1qw508d6qejxtdg4y5r3zarvary0c5xw7kv8f3t5"
      , Bech32.StringToDecodeContainsInvalidChars [CharPosition 41] )
    ]

-- | Represents a character that is permitted to appear within a Bech32 string.
newtype Bech32Char = Bech32Char
    { getBech32Char :: Char }
    deriving newtype (Eq, Ord, Show)

instance Arbitrary Bech32Char where
    arbitrary =
        Bech32Char . (bech32CharVector V.!) <$>
            choose (0, V.length bech32CharVector - 1)
    shrink (Bech32Char c) =
        case sortedVectorElemIndex c bech32CharVector of
            Nothing -> []
            Just ci -> Bech32Char . (bech32CharVector V.!) <$> shrink ci

-- | Returns true iff. the specified character is permitted to appear within
--   a Bech32 string AND is not upper case.
isBech32Char :: Char -> Bool
isBech32Char c = Set.member c bech32CharSet

-- | Returns true iff. the specified character is permitted to appear within
--   the data portion of a Bech32 string AND is not upper case.
isDataChar :: Char -> Bool
isDataChar = isJust . Bech32.dataCharToWord

-- | A vector containing all valid Bech32 characters in ascending sorted order.
--   Upper-case characters are not included.
bech32CharVector :: Vector Char
bech32CharVector = V.fromList $ Set.toAscList bech32CharSet

-- | The set of all valid Bech32 characters.
--   Upper-case characters are not included.
bech32CharSet :: Set Char
bech32CharSet =
    Set.filter (not . isUpper) $
        Set.fromList [humanReadableCharMinBound .. humanReadableCharMaxBound]
            `Set.union` (Set.singleton separatorChar)
            `Set.union` (Set.fromList Bech32.dataCharList)

-- | Find the index of an element in a sorted vector using simple binary search.
sortedVectorElemIndex :: Ord a => a -> Vector a -> Maybe Int
sortedVectorElemIndex a v = search 0 (V.length v - 1)
  where
    search l r
        | l >  r    = Nothing
        | a == b    = Just m
        | a <  b    = search l (m - 1)
        | a >  b    = search (m + 1) r
        | otherwise = Nothing
      where
        b = v V.! m
        m = (l + r) `div` 2

newtype DataChar = DataChar
    { getDataChar :: Char
    } deriving (Eq, Ord, Show)

instance Arbitrary DataChar where
    arbitrary = DataChar <$> elements Bech32.dataCharList
    shrink (DataChar c) =
        DataChar . Bech32.dataCharFromWord <$> shrink
            (fromMaybe
                (error "unable to shrink a Bech32 data character.")
                (Bech32.dataCharToWord c))

newtype HumanReadableChar = HumanReadableChar
    { getHumanReadableChar :: Char
    } deriving (Eq, Ord, Show)

instance Arbitrary HumanReadableChar where
    arbitrary = HumanReadableChar <$>
        choose (humanReadableCharMinBound, humanReadableCharMaxBound)

data ValidBech32String = ValidBech32String
    { getValidBech32String :: Text
    , humanReadablePart :: HumanReadablePart
    , unencodedDataPart :: DataPart
    } deriving (Eq, Show)

mkValidBech32String :: HumanReadablePart -> DataPart -> ValidBech32String
mkValidBech32String hrp udp = ValidBech32String
    { getValidBech32String =
        fromRight (error "unable to make a valid Bech32 string.") $
            Bech32.encode hrp udp
    , humanReadablePart = hrp
    , unencodedDataPart = udp
    }

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
        dataPartFromWords <$> replicateM len arbitrary
    shrink dp
        | null ws = []
        | otherwise = dataPartFromWords <$>
            [ take (length ws `div` 2) ws
            , drop 1 ws
            ]
      where
        ws = dataPartToWords dp

instance Arbitrary HumanReadablePart where
    arbitrary = do
        len <- choose (1, 10)
        chars <- replicateM len arbitrary
        let (Right hrp) = humanReadablePartFromText $ T.pack $
                getHumanReadableChar <$> chars
        return hrp
    shrink hrp
        | T.null chars = []
        | otherwise = catMaybes $ eitherToMaybe . humanReadablePartFromText <$>
            [ T.take (T.length chars `div` 2) chars
            , T.drop 1 chars
            ]
      where
        chars = humanReadablePartToText hrp

instance Arbitrary ByteString where
    shrink bytes | BS.null bytes = []
    shrink bytes =
        [ BS.take (BS.length bytes `div` 2) bytes
        , BS.drop 1 bytes
        ]
    arbitrary = do
        count <- choose (0, 32)
        BS.pack <$> replicateM count arbitrary

instance Arbitrary Bech32.Word5 where
    arbitrary = arbitraryBoundedEnum
    shrink w = Bech32.word5 <$> shrink (Bech32.getWord5 w)
