{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where


import Cardano.Wallet.Api.Types.Dapp.Context
    ( ApiDappChainPoint (..)
    , ApiDappHex (..)
    , ApiDappCredentialKind (..)
    , ApiDappOwnershipKind (..)
    , ApiDappOwnership (..)
    , ApiDappOutpoint (..)
    , ApiDappPendingState (..)
    , ApiDappProvenance (..)
    , ApiDappProofKind (..)
    , ApiDappRequiredWalletProof (..)
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
import Cardano.Wallet.Api.Http.Shelley.TransactionContext
    ( ProofObligation (DirectProofObligation)
    , ProofObligationResult (..)
    , dependencySource
    , evaluateObligation
    , requiredProofs
    , scriptProofKinds
    , validatePendingProvenance
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
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

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

        it "ORs requiredness across obligations sharing a frozen row key" $ do
            requiredProofs [ownedSigner] [optionalSigner, requiredSigner]
                `shouldBe` [requiredSignerRow True]
            requiredProofs [ownedSigner] [witnessedSigner]
                `shouldBe` [requiredSignerRow False]
            let result = evaluateObligation
                    (Map.singleton 0 $ Set.singleton proofHash)
                    (Set.singleton proofHash)
                    (DirectProofObligation 0 RequiredSignerProof proofHash)
            result.satisfied `shouldBe` True
            Map.lookup proofHash result.satisfiedWithoutCandidate `shouldBe` Just True

        it "classifies Plutus spending scripts without native proof ownership" $ do
            scriptProofKinds NativeScriptProof False `shouldBe` []
            scriptProofKinds NativeScriptProof True `shouldBe` [NativeScriptProof]
            scriptProofKinds PolicyProof False `shouldBe` [PolicyProof]

        it "validates pending provenance in both directions and uses authority for dependencies" $ do
            validatePendingProvenance True [Pending] `shouldBe` Right ()
            validatePendingProvenance False [Node] `shouldBe` Right ()
            validatePendingProvenance True [Node] `shouldBe` Left "pending provenance mismatch"
            validatePendingProvenance False [Pending] `shouldBe` Left "pending provenance mismatch"
            dependencySource True True `shouldBe` Just Earlier
            dependencySource False True `shouldBe` Just Pending
            dependencySource False False `shouldBe` Nothing

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

proofHash :: ByteString
proofHash = BS.replicate 28 0x42

ownedSigner :: ApiDappOwnership
ownedSigner =
    ApiDappOwnership
        PaymentCredential
        (ApiDappHex proofHash)
        OwnedKey
        [0x8000073c, 0x80000717, 0x80000000, 0, 0]
        [RequiredSignerProof]

optionalSigner, requiredSigner, witnessedSigner :: ProofObligationResult
optionalSigner = proofResult True True
requiredSigner = proofResult True False
witnessedSigner = proofResult True True

proofResult :: Bool -> Bool -> ProofObligationResult
proofResult satisfied without =
    ProofObligationResult
        (DirectProofObligation 0 RequiredSignerProof proofHash)
        (Set.singleton proofHash)
        satisfied
        (Map.singleton proofHash without)

requiredSignerRow :: Bool -> ApiDappRequiredWalletProof
requiredSignerRow =
    ApiDappRequiredWalletProof
        0 RequiredSignerProof PaymentCredential (ApiDappHex proofHash)

fullOutputRecord :: ContextRecord
fullOutputRecord =
    FullOutputRecord
        (ApiDappOutpoint (ApiDappHex $ BS.replicate 32 0xaa) 0)
        [Earlier, Node]
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
    hex "14cf3140f56400ae803e79a961fb1d98302ae1fe7d8d2e44638bb51021ea21e4"
fullOutputGolden =
    hex
        "0100000033aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa00000000050101000000000782011a000f4240"
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
        "0144444444444444444444444444444444000000010000000b77616c6c65742d74657374010101010101010101010101010101010101010101010101010101010101010114cf3140f56400ae803e79a961fb1d98302ae1fe7d8d2e44638bb51021ea21e44057536bc375dc881d56cd05a793359bbe754786193b4465141774f074d99d7c"

hex :: Text -> ByteString
hex =
    either (error . show) id
        . BAE.convertFromBase BAE.Base16
        . T.encodeUtf8

isLeft :: Either a b -> Bool
isLeft = either (const True) (const False)

isRight :: Either a b -> Bool
isRight = not . isLeft
