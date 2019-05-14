{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Codec.Binary.Bech32Spec
    ( spec
    ) where

import Prelude

import Codec.Binary.Bech32
    ( HumanReadablePart, humanReadablePartToBytes, mkHumanReadablePart )
import Control.Monad
    ( forM_ )
import Data.Bits
    ( xor )
import Data.ByteString
    ( ByteString )
import Data.Char
    ( toLower )
import Data.Maybe
    ( catMaybes, isNothing )
import Test.Hspec
    ( Spec, describe, expectationFailure, it, shouldBe, shouldSatisfy )
import Test.QuickCheck
    ( Arbitrary (..), choose, elements, property, vectorOf, (===) )

import qualified Codec.Binary.Bech32 as Bech32
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8

spec :: Spec
spec = do
    describe "Valid Checksums" $ forM_ validChecksums $ \checksum ->
        it (B8.unpack checksum) $ case Bech32.decode checksum of
            Nothing ->
                expectationFailure (show checksum)
            Just (resultHRP, resultData) -> do
                -- test that a corrupted checksum fails decoding.
                let (hrp, rest) = B8.breakEnd (== '1') checksum
                let Just (first, rest') = BS.uncons rest
                let checksumCorrupted =
                        (hrp `BS.snoc` (first `xor` 1)) `BS.append` rest'
                (Bech32.decode checksumCorrupted) `shouldSatisfy` isNothing
                -- test that re-encoding the decoded checksum results in
                -- the same checksum.
                let checksumEncoded = Bech32.encode resultHRP resultData
                let expectedChecksum = Just $ B8.map toLower checksum
                checksumEncoded `shouldBe` expectedChecksum

    describe "Invalid Checksums" $ forM_ invalidChecksums $ \checksum ->
        it (B8.unpack checksum) $
            Bech32.decode checksum `shouldSatisfy` isNothing

    describe "More Encoding/Decoding Cases" $ do
        it "length > 90" $ do
            let (Just hrp) = mkHumanReadablePart (B8.pack "ca")
            Bech32.encode hrp (BS.pack (replicate 82 1))
                `shouldSatisfy` isNothing
        it "hrp lowercased" $ do
            let (Just hrp) = mkHumanReadablePart (B8.pack "HRP")
            Bech32.encode hrp mempty
                `shouldBe` Just (B8.pack "hrp1g9xj8m")

    describe "Roundtrip (encode . decode)" $ do
        it "Can perform roundtrip for valid data" $ property $ \(hrp, bytes) ->
            (Bech32.encode hrp bytes >>= Bech32.decode) === Just (hrp, bytes)

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
    shrink hrp = catMaybes
        (mkHumanReadablePart <$> shrink (humanReadablePartToBytes hrp))
    arbitrary = do
        bytes <- choose (1, 10) >>= \n -> vectorOf n (choose (33, 126))
        let (Just hrp) = mkHumanReadablePart (B8.map toLower $ BS.pack bytes)
        return hrp

instance Arbitrary ByteString where
    shrink bytes | BS.null bytes = []
    shrink bytes =
        [ BS.take (BS.length bytes `div` 2) bytes
        , BS.drop 1 bytes
        ]
    arbitrary = do
        let alphabet = B8.unpack "qpzry9x8gf2tvdw0s3jn54khce6mua7l"
        bytes <- choose (0, 10) >>= \n -> vectorOf n (elements alphabet)
        return (B8.pack bytes)
