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
    ( DecodedTx (valid)
    , ProofObligation (DirectProofObligation, NativeProofObligation)
    , ProofObligationResult (..)
    , candidateOwnershipAssociations
    , decodeTx
    , dependencySource
    , evaluateObligation
    , requiredProofs
    , scriptProofKinds
    , supportedCertificate
    , validatePendingProvenance
    )
import Cardano.Ledger.Allegra.Scripts
    ( ValidityInterval (..)
    , mkRequireAllOfTimelock
    , mkRequireAnyOfTimelock
    , mkRequireMOfTimelock
    , mkRequireSignatureTimelock
    , mkTimeExpireTimelock
    , mkTimeStartTimelock
    )
import Cardano.Ledger.BaseTypes
    ( SlotNo (..)
    )
import Cardano.Ledger.Conway.TxCert
    ( ConwayDelegCert (ConwayRegCert, ConwayUnRegCert)
    , ConwayGovCert (ConwayUpdateDRep)
    , ConwayTxCert (ConwayTxCertDeleg, ConwayTxCertGov)
    )
import Cardano.Ledger.Credential
    ( Credential (KeyHashObj)
    )
import Data.ByteString
    ( ByteString
    )
import Data.Coerce
    ( coerce
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
import qualified Data.Maybe.Strict as Strict
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import qualified Cardano.Crypto.Hash.Class as Crypto
import qualified Cardano.Ledger.Keys as LedgerKeys

main :: IO ()
main = hspec $ do
    describe "revision-1 transaction context" $ do
        it "strictly validates the closed request schema" $ do
            decodeRequest validRequest `shouldSatisfy` isRight
            mapM_
                (\request -> decodeRequest request `shouldSatisfy` isLeft)
                invalidRequests

        it "decodes the exact accepted and rejected validity boundary" $ do
            fmap (.valid) (decodeTx $ ApiDappHex acceptedTransaction)
                `shouldBe` Right True
            fmap (.valid) (decodeTx $ ApiDappHex rejectedTransaction)
                `shouldBe` Right False

        it "gates supported and rejected Conway certificate constructors" $ do
            let stakeCredential = KeyHashObj $ coerce $ witnessKeyHash proofHash
            mapM_ (\(certificate, expected) ->
                supportedCertificate certificate `shouldBe` expected)
                [ (ConwayTxCertDeleg $ ConwayRegCert stakeCredential Strict.SNothing, True)
                , (ConwayTxCertDeleg $ ConwayUnRegCert stakeCredential Strict.SNothing, True)
                , (ConwayTxCertGov $ ConwayUpdateDRep (coerce stakeCredential) Strict.SNothing, False)
                ]

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
            mapM_ (\(existing, expected) -> do
                let witnessResult = evaluateObligation existing mempty
                        $ DirectProofObligation 0 RequiredSignerProof proofHash
                witnessResult.satisfied `shouldBe` expected)
                [ (Map.singleton 0 $ Set.singleton proofHash, True)
                , (mempty, False)
                ]

        it "associates producible candidates only with applicable transactions" $ do
            let obligations =
                    [ DirectProofObligation 0 RequiredSignerProof proofHash
                    , DirectProofObligation 2 RequiredSignerProof proofHash
                    ]
            candidateOwnershipAssociations obligations
                [ownedSigner, ownedStakeSigner, walletSnapshotOnly, unrelatedInput]
                `shouldBe`
                    [ (0, ownedSigner)
                    , (0, ownedStakeSigner)
                    , (2, ownedSigner)
                    , (2, ownedStakeSigner)
                    ]

        it "evaluates all, any, threshold, and time native-script requiredness" $ do
            let keyA = witnessKeyHash proofHash
                keyB = witnessKeyHash otherProofHash
                signatureA = mkRequireSignatureTimelock keyA
                signatureB = mkRequireSignatureTimelock keyB
                scripts =
                    [ (mkRequireAllOfTimelock $ StrictSeq.fromList [signatureA, signatureB], False)
                    , (mkRequireAnyOfTimelock $ StrictSeq.fromList [signatureA, signatureB], True)
                    , (mkRequireMOfTimelock 1 $ StrictSeq.fromList [signatureA, signatureB], True)
                    , ( mkRequireAllOfTimelock $ StrictSeq.fromList
                            [ signatureA
                            , mkTimeStartTimelock $ SlotNo 10
                            , mkTimeExpireTimelock $ SlotNo 20
                            ]
                      , False
                      )
                    ]
                validity = ValidityInterval (Strict.SJust $ SlotNo 10) (Strict.SJust $ SlotNo 20)
                evaluate script = evaluateObligation mempty
                    (Set.fromList [proofHash, otherProofHash])
                    (NativeProofObligation 0 NativeScriptProof script validity)
            mapM_ (\(script, remainsSatisfied) -> do
                let result = evaluate script
                result.satisfied `shouldBe` True
                Map.lookup proofHash result.satisfiedWithoutCandidate `shouldBe` Just remainsSatisfied
                ) scripts

        it "keeps mint policy identity separate from its owned key leaf" $ do
            let nativeObligation = NativeProofObligation 0 PolicyProof
                    (mkRequireSignatureTimelock $ witnessKeyHash proofHash)
                    (ValidityInterval Strict.SNothing Strict.SNothing)
                policyScript = ApiDappOwnership PolicyCredential (ApiDappHex otherProofHash) ScriptOwned [] [PolicyProof]
                policyLeaf = ApiDappOwnership PolicyCredential (ApiDappHex proofHash) OwnedKey
                    [0x8000073f, 0x80000717, 0x80000000] [PolicyProof]
            candidateOwnershipAssociations [nativeObligation] [policyScript, policyLeaf]
                `shouldBe` [(0, policyLeaf)]
            scriptProofKinds PolicyProof True `shouldBe` [PolicyProof]

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
            mapM_ (\(earlier, pending, _node, expected) ->
                dependencySource earlier pending `shouldBe` expected)
                [ (True, False, False, Just Earlier)
                , (True, True, False, Just Earlier)
                , (True, False, True, Just Earlier)
                , (True, True, True, Just Earlier)
                , (False, True, False, Just Pending)
                , (False, True, True, Just Pending)
                , (False, False, True, Nothing)
                ]

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

ownedStakeSigner, walletSnapshotOnly, unrelatedInput :: ApiDappOwnership
ownedStakeSigner =
    ApiDappOwnership StakeCredential (ApiDappHex proofHash) OwnedKey
        [0x8000073c, 0x80000717, 0x80000000, 2, 0] [RequiredSignerProof]
walletSnapshotOnly =
    ApiDappOwnership PaymentCredential (ApiDappHex otherProofHash) OwnedKey
        [0x8000073c, 0x80000717, 0x80000000, 0, 1] []
unrelatedInput =
    ApiDappOwnership PaymentCredential (ApiDappHex otherProofHash) OwnedKey
        [0x8000073c, 0x80000717, 0x80000000, 0, 1] [NormalInputProof]

otherProofHash :: ByteString
otherProofHash = BS.replicate 28 0x43

witnessKeyHash :: ByteString -> LedgerKeys.KeyHash LedgerKeys.Witness
witnessKeyHash bytes = case Crypto.hashFromBytes bytes of
    Just hash -> LedgerKeys.KeyHash hash
    Nothing -> error "invalid test key hash"

optionalSigner, requiredSigner, witnessedSigner :: ProofObligationResult
optionalSigner = proofResult True True
requiredSigner = proofResult True False
witnessedSigner = proofResult True True

proofResult :: Bool -> Bool -> ProofObligationResult
proofResult isSatisfied without =
    ProofObligationResult
        (DirectProofObligation 0 RequiredSignerProof proofHash)
        (Set.singleton proofHash)
        isSatisfied
        (Map.singleton proofHash without)

requiredSignerRow :: Bool -> ApiDappRequiredWalletProof
requiredSignerRow =
    ApiDappRequiredWalletProof
        0 RequiredSignerProof PaymentCredential (ApiDappHex proofHash)

fullOutputRecord :: ContextRecord
fullOutputRecord =
    FullOutputRecord
        (ApiDappOutpoint (ApiDappHex $ hex "35eb18701459203ce42156af66c76e3f00b9e2a1b9d48192fbbf0d233a652c9f") 0)
        [Earlier, Node]
        [Normal]
        True
        None
        (hex "82581d60aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa1a000f4240")

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
        1
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
        fixtureTransactions
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
    hex "6e87935bf459d9d6a33dd0df3ad8a1178ffe380bdc33fdbc8bb78d865d082446"
fullOutputGolden =
    hex
        "010000005135eb18701459203ce42156af66c76e3f00b9e2a1b9d48192fbbf0d233a652c9f00000000050101000000002582581d60aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa1a000f4240"
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
        "06000000270000000101010000001caaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01"
tokenGolden =
    hex
        "0144444444444444444444444444444444000000010000000b77616c6c65742d7465737401010101010101010101010101010101010101010101010101010101010101016e87935bf459d9d6a33dd0df3ad8a1178ffe380bdc33fdbc8bb78d865d082446e019bfde0ef59a5a9861653b0ae9658e8e3ba6528436f05a59ede65791e21bc5"

fixtureTransactions :: [ByteString]
fixtureTransactions = hex <$>
    [ acceptedTransactionHex
    , "84a3008182582035eb18701459203ce42156af66c76e3f00b9e2a1b9d48192fbbf0d233a652c9f00018182581d60aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa1a000dbba0021a000186a0a0f5f6"
    , "84a40081825820222222222222222222222222222222222222222222222222222222222222222200018182581d60aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa1a000c3500021a00030d400d8182582035eb18701459203ce42156af66c76e3f00b9e2a1b9d48192fbbf0d233a652c9f00a0f5f6"
    ]

acceptedTransaction :: ByteString
acceptedTransaction = hex acceptedTransactionHex

acceptedTransactionHex :: Text
acceptedTransactionHex =
    "84a30081825820111111111111111111111111111111111111111111111111111111111111111100018182581d60aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa1a000f42400200a0f5f6"

rejectedTransaction :: ByteString
rejectedTransaction =
    hex "84a30081825820111111111111111111111111111111111111111111111111111111111111111100018182581d60aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa1a000f42400200a0f4f6"

hex :: Text -> ByteString
hex =
    either (error . show) id
        . BAE.convertFromBase BAE.Base16
        . T.encodeUtf8

isLeft :: Either a b -> Bool
isLeft = either (const True) (const False)

isRight :: Either a b -> Bool
isRight = not . isLeft
