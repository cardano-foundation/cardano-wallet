{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Cardano.Wallet.Api.Types.Dapp.Context
    ( ApiDappChainPoint (..)
    , ApiDappHex (..)
    , ApiDappCredentialKind (..)
    , ApiDappOwnershipKind (..)
    , ApiDappOutpoint (..)
    , ApiDappPendingState (..)
    , ApiDappProvenance (..)
    , ApiDappProofKind (..)
    , ApiDappRole (..)
    , ApiDappTransactionContextRequest
    , ApiDappWord64 (..)
    , ContextDigestInput (..)
    , ContextRecord (..)
    , ContextTokenClaims (..)
    , canonicalContextRecords
    , computeContextDigest
    , decodeContextTokenClaims
    , decodeTransactionContextRequest
    , encodeContextRecord
    , encodeContextToken
    , validateContextToken
    )
import Data.ByteString
    ( ByteString
    )
import Data.Text
    ( Text
    )
import Test.Hspec
    ( describe
    , hspec
    , it
    , shouldBe
    , shouldSatisfy
    )
import Prelude

import qualified Data.ByteArray.Encoding as BAE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Text.Encoding as T

main :: IO ()
main = hspec $ do
    describe "revision-1 transaction context" $ do
        it "strictly validates the closed request schema" $ do
            decodeRequest validRequest `shouldSatisfy` isRight
            mapM_
                (\request -> decodeRequest request `shouldSatisfy` isLeft)
                invalidRequests

        it "matches all frozen record and Blake2b-256 goldens" $ do
            encodeContextRecord fullOutputRecord `shouldBe` Right fullOutputGolden
            encodeContextRecord protocolRecord `shouldBe` Right protocolGolden
            encodeContextRecord pendingRecord `shouldBe` Right pendingGolden
            encodeContextRecord ownershipRecord `shouldBe` Right ownershipGolden
            encodeContextRecord requiredProofRecord `shouldBe` Right requiredProofGolden
            computeContextDigest digestInput `shouldBe` Right digestGolden

        it "sorts complete records and rejects duplicates" $ do
            canonicalContextRecords [protocolRecord, fullOutputRecord]
                `shouldBe` Right [fullOutputGolden, protocolGolden]
            canonicalContextRecords [protocolRecord, protocolRecord]
                `shouldSatisfy` isLeft

        it "matches the token golden and rejects changed MACs and bindings" $ do
            let encoded = encodeContextToken key tokenClaims
            encoded `shouldBe` Right tokenGolden
            encoded
                `shouldSatisfy` either (const False) (validateContextToken key tokenClaims)
            fmap (validateContextToken key tokenClaims . BS.init) encoded
                `shouldBe` Right False
            fmap (validateContextToken (BS.replicate 32 0x54) tokenClaims) encoded
                `shouldBe` Right False
            fmap (validateContextToken key changedClaims) encoded
                `shouldBe` Right False
            decodeContextTokenClaims (BS.drop 1 tokenGolden)
                `shouldSatisfy` isLeft

decodeRequest
    :: BL8.ByteString -> Either String ApiDappTransactionContextRequest
decodeRequest = decodeTransactionContextRequest

validRequest :: BL8.ByteString
validRequest =
    "{\"revision\":1,\"network\":{\"network_id\":0,\"network_magic\":1,\"genesis_hash\":\"0000000000000000000000000000000000000000000000000000000000000000\"},\"transactions\":[\"84a0a0f5f6\"]}"

invalidRequests :: [BL8.ByteString]
invalidRequests =
    [ "{\"revision\":1,\"revision\":2,\"network\":{\"network_id\":0,\"network_magic\":1,\"genesis_hash\":\"0000000000000000000000000000000000000000000000000000000000000000\"},\"transactions\":[\"84a0a0f5f6\"]}"
    , "{\"revision\":2,\"network\":{\"network_id\":0,\"network_magic\":1,\"genesis_hash\":\"0000000000000000000000000000000000000000000000000000000000000000\"},\"transactions\":[\"84a0a0f5f6\"]}"
    , "{\"revision\":1,\"network\":{\"network_id\":2,\"network_magic\":1,\"genesis_hash\":\"0000000000000000000000000000000000000000000000000000000000000000\"},\"transactions\":[\"84a0a0f5f6\"]}"
    , "{\"revision\":1,\"network\":{\"network_id\":0,\"network_magic\":1,\"genesis_hash\":\"0000000000000000000000000000000000000000000000000000000000000000\"},\"transactions\":[\"84A0A0F5F6\"]}"
    , "{\"revision\":1,\"network\":{\"network_id\":0,\"network_magic\":1,\"genesis_hash\":\"0000000000000000000000000000000000000000000000000000000000000000\"},\"transactions\":[],\"inputs\":[]}"
    ]

fullOutputRecord :: ContextRecord
fullOutputRecord =
    FullOutputRecord
        (ApiDappOutpoint (ApiDappHex $ BS.replicate 32 0xaa) 0)
        [Node]
        [Normal]
        True
        None
        (hex "82011a000f4240")

protocolRecord :: ContextRecord
protocolRecord = ProtocolRecord 0 42 9 0 (hex "a0")

pendingRecord :: ContextRecord
pendingRecord =
    PendingTransactionRecord
        (BS.replicate 32 0xbb)
        (hex "84a0a0f5f6")
        [ApiDappOutpoint (ApiDappHex $ BS.replicate 32 0xaa) 1]
        []
        (Just 42)

ownershipRecord :: ContextRecord
ownershipRecord =
    OwnershipRecord
        PaymentCredential
        (BS.replicate 28 0xaa)
        OwnedKey
        [0x8000073c, 0x80000717, 0x80000000, 0, 0]
        [NormalInputProof]

requiredProofRecord :: ContextRecord
requiredProofRecord =
    RequiredProofRecord
        0
        NormalInputProof
        PaymentCredential
        (BS.replicate 28 0xaa)
        True

digestInput :: ContextDigestInput
digestInput =
    ContextDigestInput
        "wallet-test"
        (BS.replicate 32 0x01)
        ( ApiDappChainPointBlock
            (ApiDappWord64 42)
            (ApiDappHex $ BS.replicate 32 0x02)
        )
        7
        9
        [hex "84a0a0f5f6"]
        [protocolRecord, fullOutputRecord, ownershipRecord, requiredProofRecord, pendingRecord]

tokenClaims :: ContextTokenClaims
tokenClaims =
    ContextTokenClaims
        (BS.replicate 16 0x44)
        1
        "wallet-test"
        (BS.replicate 32 0x01)
        digestGolden

changedClaims :: ContextTokenClaims
changedClaims =
    ContextTokenClaims
        (BS.replicate 16 0x45)
        1
        "wallet-test"
        (BS.replicate 32 0x01)
        digestGolden

key
    , digestGolden
    , fullOutputGolden
    , protocolGolden
    , pendingGolden
    , ownershipGolden
    , requiredProofGolden
    , tokenGolden
        :: ByteString
key = BS.replicate 32 0x55
digestGolden =
    hex "affe83d478d48ad55b7625e811c97cfbc4d73642dc9c92f6840e0f0a978dbdd7"
fullOutputGolden =
    hex
        "0100000033aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa00000000040101000000000782011a000f4240"
protocolGolden =
    hex
        "030000001c00000006636f6e776179000000002a000000090000000000000001a0"
pendingGolden =
    hex
        "0700000063bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb040000000584a0a0f5f60000000100000024aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa000000010000000001000000000000002a"
ownershipGolden =
    hex
        "020000003e010000001caaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01000000058000073c8000071780000000000000000000000000000001"
requiredProofGolden =
    hex
        "06000000270000000001010000001caaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01"
tokenGolden =
    hex
        "0144444444444444444444444444444444000000010000000b77616c6c65742d746573740101010101010101010101010101010101010101010101010101010101010101affe83d478d48ad55b7625e811c97cfbc4d73642dc9c92f6840e0f0a978dbdd73fdf81f358155841353664c84eaeb5c5a0b9ec488e213187c2d1b355c2d96288"

hex :: Text -> ByteString
hex =
    either (error . show) id
        . BAE.convertFromBase BAE.Base16
        . T.encodeUtf8

isLeft :: Either a b -> Bool
isLeft = either (const True) (const False)

isRight :: Either a b -> Bool
isRight = not . isLeft
