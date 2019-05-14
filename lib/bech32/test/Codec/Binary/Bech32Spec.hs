module Codec.Binary.Bech32Spec
    ( spec
    ) where

import Prelude

import Codec.Binary.Bech32
    ( bech32Decode, bech32Encode, segwitDecode, segwitEncode, word5 )
import Control.Monad
    ( forM_ )
import Data.Bits
    ( xor )
import Data.ByteArray.Encoding
    ( Base (Base16), convertFromBase )
import Data.ByteString
    ( ByteString )
import Data.Char
    ( toLower )
import Data.Maybe
    ( isJust, isNothing )
import Data.Word
    ( Word8 )
import Test.Hspec
    ( Spec, describe, expectationFailure, it, shouldBe, shouldSatisfy )

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8

spec :: Spec
spec = do
    describe "Valid Checksums" $ forM_ validChecksums $ \checksum ->
        it (B8.unpack checksum) $ case bech32Decode checksum of
            Nothing ->
                expectationFailure (show checksum)
            Just (resultHRP, resultData) -> do
                -- test that a corrupted checksum fails decoding.
                let (hrp, rest) = B8.breakEnd (== '1') checksum
                let Just (first, rest') = BS.uncons rest
                let checksumCorrupted =
                        (hrp `BS.snoc` (first `xor` 1)) `BS.append` rest'
                (bech32Decode checksumCorrupted) `shouldSatisfy` isNothing
                -- test that re-encoding the decoded checksum results in
                -- the same checksum.
                let checksumEncoded = bech32Encode resultHRP resultData
                let expectedChecksum = Just $ B8.map toLower checksum
                checksumEncoded `shouldBe` expectedChecksum

    describe "Invalid Checksums" $ forM_ invalidChecksums $ \checksum ->
        it (B8.unpack checksum) $
            bech32Decode checksum `shouldSatisfy` isNothing

    describe "Addresses" $ forM_ validAddresses $ \(address, hexscript) ->
        it (B8.unpack address) $ do
            let address' = B8.map toLower address
            let hrp = B8.take 2 address'
            case segwitDecode hrp address of
                Nothing ->
                    expectationFailure "decode failed"
                Just (witver, witprog) -> do
                    segwitScriptPubkey witver witprog
                        `shouldBe` unsafeFromHex hexscript
                    segwitEncode hrp witver witprog
                        `shouldBe` Just address'

    describe "Invalid Addresses" $ forM_ invalidAddresses $ \address ->
        it (B8.unpack address) $ do
            segwitDecode (B8.pack "bc") address `shouldSatisfy` isNothing
            segwitDecode (B8.pack "tb") address `shouldSatisfy` isNothing

    describe "More Encoding/Decoding Cases" $ do
        it "length > 90" $
            bech32Encode (B8.pack "bc") (replicate 82 (word5 (1 :: Word8)))
                `shouldSatisfy` isNothing
        it "segwit version bounds" $
            segwitEncode (B8.pack "bc") 17 []
                `shouldSatisfy` isNothing
        it "segwit prog len version 0" $
            segwitEncode (B8.pack "bc") 0 (replicate 30 1)
                `shouldSatisfy` isNothing
        it "segwit prog len version != 0" $
            segwitEncode (B8.pack "bc") 1 (replicate 30 1)
                `shouldSatisfy` isJust
        it "segwit prog len version != 0" $
            segwitEncode (B8.pack "bc") 1 (replicate 41 1)
                `shouldSatisfy` isNothing
        it "empty HRP encode" $
            bech32Encode (B8.pack "") []
                `shouldSatisfy` isNothing
        it "empty HRP decode" $
            bech32Decode (B8.pack "10a06t8")
                `shouldSatisfy` isNothing
        it "hrp lowercased" $
            (bech32Encode (B8.pack "HRP") [])
                `shouldBe` Just (B8.pack "hrp1g9xj8m")

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

validAddresses :: [(ByteString, ByteString)]
validAddresses = map mapTuple
    [ ( "BC1QW508D6QEJXTDG4Y5R3ZARVARY0C5XW7KV8F3T4"
      , "0014751e76e8199196d454941c45d1b3a323f1433bd6" )
    , ( "tb1qrp33g0q5c5txsp9arysrx4k6zdkfs4nce4xj0gdcccefvpysxf3q0sl5k7"
      , "00201863143c14c5166804bd19203356da136c985678cd4d27a1b8c6329604903262" )
    , ( "bc1pw508d6qejxtdg4y5r3zarvary0c5xw7kw\
        \508d6qejxtdg4y5r3zarvary0c5xw7k7grplx"
      , "5128751e76e8199196d454941c45d1b3a323f1433b\
        \d6751e76e8199196d454941c45d1b3a323f1433bd6" )
    , ( "BC1SW50QA3JX3S", "6002751e" )
    , ( "bc1zw508d6qejxtdg4y5r3zarvaryvg6kdaj"
      , "5210751e76e8199196d454941c45d1b3a323" )
    , ( "tb1qqqqqp399et2xygdj5xreqhjjvcmzhxw4aywxecjdzew6hylgvsesrxh6hy"
      , "0020000000c4a5cad46221b2a187905e5266362b99d5e91c6ce24d165dab93e86433" )
    ]
  where
    mapTuple (a, b) = (B8.pack a, B8.pack b)

invalidAddresses :: [ByteString]
invalidAddresses = map B8.pack
    [ "tc1qw508d6qejxtdg4y5r3zarvary0c5xw7kg3g4ty"
    , "bc1qw508d6qejxtdg4y5r3zarvary0c5xw7kv8f3t5"
    , "BC13W508D6QEJXTDG4Y5R3ZARVARY0C5XW7KN40WF2"
    , "bc1rw5uspcuh"
    , "bc10w508d6qejxtdg4y5r3zarvary0c5xw7kw508d6\
      \qejxtdg4y5r3zarvary0c5xw7kw5rljs90"
    , "BC1QR508D6QEJXTDG4Y5R3ZARVARYV98GJ9P"
    , "tb1qrp33g0q5c5txsp9arysrx4k6zdkfs4nce4xj0gdcccefvpysxf3q0sL5k7"
    , "bc1zw508d6qejxtdg4y5r3zarvaryvqyzf3du"
    , "tb1qrp33g0q5c5txsp9arysrx4k6zdkfs4nce4xj0gdcccefvpysxf3pjxtptv"
    , "bc1gmk9yu"
    ]

unsafeFromHex :: ByteString -> ByteString
unsafeFromHex hex =
    case convertFromBase Base16 hex of
        Left e -> error $ "unsafeFromHex: " <> e
        Right bytes -> bytes

segwitScriptPubkey :: Word8 -> [Word8] -> ByteString
segwitScriptPubkey witver witprog =
    BS.pack $ witver' : fromIntegral (length witprog) : witprog
  where
    witver' = if witver == 0 then 0 else witver + 0x50
