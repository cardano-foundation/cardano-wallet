{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where


import Cardano.Wallet
    ( DappStakeRegistration (..)
    )
import Cardano.Wallet.Api.Http.Server.Error
    ( dappServerError
    )
import Cardano.Wallet.Api.Lib.ApiT
    ( ApiT (..)
    )
import Cardano.Wallet.Api.Types.Dapp.Context
    ( ApiDappBatchOverlay (..)
    , ApiDappChainPoint (..)
    , ApiDappContextNetwork (..)
    , ApiDappDataSignRequest (..)
    , ApiDappDataSignResponse (..)
    , ApiDappHex (..)
    , ApiDappCredentialKind (..)
    , ApiDappOwnershipKind (..)
    , ApiDappOwnership (..)
    , ApiDappOutpoint (..)
    , ApiDappPendingOverlay (..)
    , ApiDappPendingState (..)
    , ApiDappProtocolVersion (..)
    , ApiDappProvenance (..)
    , ApiDappProofKind (..)
    , ApiDappRequiredWalletProof (..)
    , ApiDappRole (..)
    , ApiDappTransactionContextRequest (..)
    , ApiDappTransactionContextResponse (..)
    , ApiDappVolatileDelta (..)
    , ApiDappWitnessSignItem (..)
    , ApiDappWitnessSignRequest (..)
    , ApiDappWord64 (..)
    , ContextDigestInput (..)
    , ContextRecord (..)
    , ContextTokenClaims (..)
    , canonicalContextRecords
    , decodeDappCip95KeyState
    , computeContextDigest
    , decodeContextTokenClaims
    , decodeDappDataSignRequest
    , decodeDappDataSignResponse
    , decodeDappWitnessSignRequest
    , decodeDappWitnessSignResponse
    , decodeTransactionContextRequest
    , encodeContextRecord
    , encodeContextToken
    , validateContextToken
    , validateDappWitnessBinding
    )
import Cardano.Wallet.Api.Http.Shelley.Server
    ( DataCredential (..)
    , classifyDRepDataCredential
    , encodeProtectedDataAddress
    , encodeSignatureStructure
    , mkDappDataSignResponse
    , stakeRegistrationEffects
    , validateDataSignRequest
    )
import Cardano.Wallet.Api.Http.Shelley.TransactionContext
    ( DecodedTx (txId, valid)
    , ProofInventory (..)
    , ProofObligation (DirectProofObligation, NativeProofObligation)
    , ProofObligationResult (..)
    , candidateOwnershipAssociations
    , decodeDappTx
    , decodeTx
    , dependencySource
    , evaluateObligation
    , requiredProofs
    , reviewedBatchComplete
    , scriptProofKinds
    , supportedCertificate
    , validatePendingProvenance
    , validateTransactionContextResponseForRequest
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
import Cardano.Ledger.Coin
    ( Coin (..)
    )
import Cardano.Ledger.Conway.TxCert
    ( ConwayDelegCert (ConwayRegCert, ConwayRegDelegCert, ConwayUnRegCert)
    , ConwayGovCert (ConwayRegDRep, ConwayUnRegDRep, ConwayUpdateDRep)
    , ConwayTxCert (ConwayTxCertDeleg, ConwayTxCertGov)
    , Delegatee (DelegVote)
    )
import Cardano.Ledger.DRep
    ( DRep (DRepAlwaysAbstain)
    )
import Cardano.Ledger.Credential
    ( Credential (KeyHashObj)
    )
import Cardano.Wallet.Api.Types.Error
    ( DappError (..)
    )
import Cardano.Wallet.Primitive.Passphrase
    ( Passphrase (..)
    )
import Control.Monad
    ( forM_
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
import Servant.Server
    ( ServerError (errBody, errHTTPCode)
    )
import Test.Hspec
    ( describe
    , hspec
    , it
    , shouldBe
    , shouldSatisfy
    )
import Prelude

import qualified Cryptography.Hash.Blake as Blake
import qualified Data.Aeson as Aeson
import qualified Data.ByteArray.Encoding as BAE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Text as Text
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


        describe "DAPP_DATA_SIGNING schema" $ do
            it "accepts only the closed revision-1 request and response" $ do
                decodeDappDataSignRequest validDataSignRequest `shouldSatisfy` isRight
                decodeDappDataSignResponse validDataSignResponse `shouldSatisfy` isRight
                mapM_
                    (\request -> decodeDappDataSignRequest request `shouldSatisfy` isLeft)
                    [ BL8.pack
                        "{\"revision\":2,\"network\":{\"network_id\":0,\"network_magic\":42,\"genesis_hash\":\"0000000000000000000000000000000000000000000000000000000000000000\"},\"address\":\"60aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",\"payload\":\"\",\"passphrase\":\"pass\"}"
                    , BL8.pack
                        "{\"revision\":1,\"network\":{\"network_id\":0,\"network_magic\":42,\"genesis_hash\":\"0000000000000000000000000000000000000000000000000000000000000000\"},\"address\":\"60AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\",\"payload\":\"\",\"passphrase\":\"pass\"}"
                    , BL8.pack
                        "{\"revision\":1,\"network\":{\"network_id\":0,\"network_magic\":42,\"genesis_hash\":\"0000000000000000000000000000000000000000000000000000000000000000\"},\"address\":\"60aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",\"payload\":\"\",\"passphrase\":\"pass\",\"extra\":true}"
                    ]
            it "accepts only fixed raw public fields in CIP-95 key state" $ do
                decodeDappCip95KeyState
                    "{\"drep_public_key\":\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",\"registered_stake_public_keys\":[],\"unregistered_stake_public_keys\":[\"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb\"]}"
                    `shouldSatisfy` isRight
                decodeDappCip95KeyState
                    "{\"drep_public_key\":\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",\"registered_stake_public_keys\":[],\"unregistered_stake_public_keys\":[],\"private_key\":\"00\"}"
                    `shouldSatisfy` isLeft
        describe "DAPP_WITNESS_SIGNING request decoder" $ do
            it "accepts the canonical request" $
                decodeDappWitnessSignRequest (validWitnessRequest [witnessItem])
                    `shouldSatisfy` isRight
            forM_ invalidWitnessRequests $ \(label, request) ->
                it ("rejects " <> label) $
                    decodeDappWitnessSignRequest request `shouldSatisfy` isLeft

        it "authenticates request bindings before any context conflict" $ do
            let original = [ApiDappHex acceptedTransaction, ApiDappHex $ fixtureTransactions !! 1]
                reviewedContext = validDappWitnessContext original
                request = ApiDappTransactionContextRequest 1 dappNetwork original
                alternateEnvelope = BS.init acceptedTransaction <> BS.singleton 0xa0
                rejected =

                    [ ( dappGeneration
                      , request
                      , tamperContextToken
                            ( ApiDappHex
                                $ BS.init
                                $ getApiDappHex reviewedContext.contextToken
                            )
                            reviewedContext
                      )
                    , ( dappGeneration
                      , request
                      , tamperContextWallet (dappWalletId <> "b") reviewedContext
                      )
                    , ( dappGeneration
                      , request
                      , tamperContextNetwork
                            (ApiDappContextNetwork 1 1 (ApiDappHex $ BS.replicate 32 0))
                            reviewedContext
                      )
                    , (BS.replicate 16 0x99, request, reviewedContext)
                    , ( dappGeneration
                      , tamperContextRequestTransactions (reverse original) request
                      , reviewedContext
                      )
                    , ( dappGeneration
                      , tamperContextRequestTransactions
                            (ApiDappHex alternateEnvelope : drop 1 original)
                            request
                      , reviewedContext
                      )
                    ]
            validateDappWitnessBinding
                dappNetwork
                key
                dappGeneration
                dappWalletId
                request
                reviewedContext
                `shouldBe` Right ()
            forM_ rejected $ \(generation, boundRequest, boundContext) ->
                validateDappWitnessBinding
                    dappNetwork
                    key
                    generation
                    dappWalletId
                    boundRequest
                    boundContext
                    `shouldBe` Left InvalidDappRequest
        describe "DAPP_DATA_SIGNING address dispatch and COSE" $ do
            it "selects payment credentials for base, enterprise, and pointer addresses" $ do
                mapM_
                    (\raw ->
                        validateDataSignRequest dappNetwork (dataSignRequest raw)
                            `shouldBe` Right (PaymentCredential, KeyDataCredential dataCredential, raw)
                    )
                    [ BS.pack (0x00 : BS.unpack dataCredential <> replicate 28 0xbb)
                    , enterpriseAddress
                    , BS.pack (0x40 : BS.unpack dataCredential <> [0, 0, 0])
                    ]
            it "selects the stake credential for reward addresses" $
                validateDataSignRequest dappNetwork (dataSignRequest rewardAddress)
                    `shouldBe` Right (StakeCredential, KeyDataCredential dataCredential, rewardAddress)
            it "accepts only a raw 28-byte DRep ID outside Shelley address bounds" $ do
                validateDataSignRequest dappNetwork (dataSignRequest dataCredential)
                    `shouldBe` Right (DRepCredential, DRepDataCredential dataCredential, dataCredential)
                validateDataSignRequest dappNetwork (dataSignRequest $ BS.replicate 27 0xaa)
                    `shouldBe` Left InvalidDappRequest
            it "dispatches only raw and matching type-6 credentials to DRep" $ do
                let drepHash = dataCredential
                    matching = BS.cons 0x61 drepHash
                    nonmatching = BS.cons 0x61 $ BS.replicate 28 0xbb
                classifyDRepDataCredential DRepCredential drepHash drepHash drepHash
                    `shouldBe` Right True
                classifyDRepDataCredential PaymentCredential matching drepHash drepHash
                    `shouldBe` Right True
                classifyDRepDataCredential PaymentCredential nonmatching
                    (BS.replicate 28 0xbb) drepHash
                    `shouldBe` Right False
                classifyDRepDataCredential DRepCredential drepHash
                    (BS.replicate 28 0xbb) drepHash
                    `shouldBe` Left ()
            it "normalizes a DRep COSE protected address to its raw hash" $ do
                let protected = encodeProtectedDataAddress dataCredential
                    signatureStructure = encodeSignatureStructure protected nonUtf8Payload
                    publicKey = BS.replicate 32 0x42
                    keyHash = Blake.blake2b224 publicKey
                mkDappDataSignResponse
                    DRepCredential keyHash dataCredential nonUtf8Payload
                    protected signatureStructure (publicKey, BS.replicate 64 0x11)
                    `shouldSatisfy` isRight
            it "classifies script credentials without falling through to proof generation" $ do
                validateDataSignRequest dappNetwork (dataSignRequest scriptPaymentAddress)
                    `shouldBe` Right (PaymentCredential, ScriptDataCredential, scriptPaymentAddress)
                validateDataSignRequest dappNetwork (dataSignRequest scriptStakeAddress)
                    `shouldBe` Right (StakeCredential, ScriptDataCredential, scriptStakeAddress)
            it "constructs exact untagged COSE bytes over the raw address and non-UTF8 payload" $ do
                let protected = encodeProtectedDataAddress enterpriseAddress
                    signatureStructure = encodeSignatureStructure protected nonUtf8Payload
                    publicKey = BS.replicate 32 0x42
                    keyHash = Blake.blake2b224 publicKey
                response <- case mkDappDataSignResponse
                    PaymentCredential keyHash enterpriseAddress nonUtf8Payload
                    protected signatureStructure (publicKey, BS.replicate 64 0x11) of
                    Left err -> error err
                    Right value -> pure value
                response.credential `shouldBe` ApiDappHex keyHash
                response.coseSign1 `shouldBe` ApiDappHex
                    ( hex
                        "84582aa201276761646472657373581d60aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa266686173686564f46776657273696f6e014400ff8041584011111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111"
                    )
                response.coseKey `shouldBe` ApiDappHex
                    (hex "a40101032720062158204242424242424242424242424242424242424242424242424242424242424242")

        it "serializes fixed redacted dapp witness errors" $ do
            let errors =
                    [ (InvalidDappRequest, 400)
                    , (DappContextConflictError, 400)
                    , (DappTxProofGenerationError, 403)
                    , (DappDeprecatedCertificateError, 403)
                    , (DappDataProofGenerationError, 403)
                    , (DappDataAddressNotPkError, 403)
                    , (DappInternalErrorResponse, 500)
                    ]
                secrets =
                    [ "84a30081825820"
                    , "cbor"
                    , "token"
                    , "hash"
                    , "000102030405060708090a0b0c0d0e0f"
                    , "1852/1815/0/0/0"
                    , "path"
                    , "passphrase"
                    , "exception"
                    ]
            forM_ errors $ \(dappError, status) -> do
                let serverError = dappServerError dappError
                    body = BL8.toStrict serverError.errBody
                serverError.errHTTPCode `shouldBe` status
                forM_ secrets $ \secret ->
                    body `shouldSatisfy` not . BS.isInfixOf secret


        it "strictly validates witness-only response ordering and shape" $ do
            decodeDappWitnessSignResponse
                "{\"revision\":1,\"witnesses\":[{\"transaction_index\":0,\"body_hash\":\"0000000000000000000000000000000000000000000000000000000000000000\",\"witness_set_cbor\":\"a0\"}]}"
                `shouldSatisfy` isRight
            mapM_
                (\response -> decodeDappWitnessSignResponse response `shouldSatisfy` isLeft)
                [ "{\"revision\":1,\"witnesses\":[{\"transaction_index\":1,\"body_hash\":\"0000000000000000000000000000000000000000000000000000000000000000\",\"witness_set_cbor\":\"a0\"}]}"
                , "{\"revision\":1,\"witnesses\":[{\"transaction_index\":0,\"body_hash\":\"0000000000000000000000000000000000000000000000000000000000000000\",\"witness_set_cbor\":\"a0\",\"transaction\":\"84\"}]}"
                ]
        it "decodes the exact accepted and rejected validity boundary" $ do
            fmap (.valid) (decodeTx $ ApiDappHex acceptedTransaction)
                `shouldBe` Right True
            fmap (.valid) (decodeTx $ ApiDappHex rejectedTransaction)
                `shouldBe` Right False

        describe "DAPP_WITNESS_SIGNING" $ do
            it "accepts collateral-only and explicit required-signer proofs" $ do
                let collateral = evaluateObligation mempty (Set.singleton proofHash)
                        $ DirectProofObligation 0 CollateralProof proofHash
                    requiredSignerProof = evaluateObligation mempty (Set.singleton proofHash)
                        $ DirectProofObligation 1 RequiredSignerProof proofHash
                collateral.satisfied `shouldBe` True
                requiredSignerProof.satisfied `shouldBe` True

            it "permits incomplete partial items but rejects an atomic batch failure" $ do
                let inventory = ProofInventory
                        []
                        mempty
                        []
                        []
                        (Map.fromList [(0, True), (1, False)])
                        []
                        []
                reviewedBatchComplete [True, True] inventory `shouldBe` True
                reviewedBatchComplete [True, False] inventory `shouldBe` False

        it "rejects unequal envelopes with the same transaction id" $ do
            let alternateEnvelope =
                    BS.init acceptedTransaction <> BS.singleton 0xa0
                decoded = mapM (decodeTx . ApiDappHex) [acceptedTransaction, alternateEnvelope]
            fmap (map (.txId)) decoded `shouldSatisfy` \case
                Right [firstId, secondId] -> firstId == secondId
                _ -> False
            validateTransactionContextResponseForRequest
                ( ApiDappTransactionContextRequest
                    1
                    (ApiDappContextNetwork 0 1 $ ApiDappHex $ BS.replicate 32 0)
                    (ApiDappHex <$> [acceptedTransaction, alternateEnvelope])
                )
                (error "duplicate rejection must not inspect the response")
                `shouldBe` Left "duplicate transaction envelopes differ"

        it "recognizes supported Conway certificate constructors" $ do
            let stakeCredential = KeyHashObj $ coerce $ witnessKeyHash proofHash
            mapM_ (\(certificate, expected) ->
                supportedCertificate certificate `shouldBe` expected)
                [ (ConwayTxCertDeleg $ ConwayRegCert stakeCredential Strict.SNothing, True)
                , (ConwayTxCertGov $ ConwayRegDRep (coerce stakeCredential) (Coin 1) Strict.SNothing, True)
                , (ConwayTxCertGov $ ConwayUnRegDRep (coerce stakeCredential) (Coin 1), True)
                , (ConwayTxCertGov $ ConwayUpdateDRep (coerce stakeCredential) Strict.SNothing, True)
                ]
            fmap (const ()) (decodeDappTx $ ApiDappHex acceptedTransaction)
                `shouldBe` Right ()
            fmap (const ()) (decodeDappTx $ ApiDappHex rejectedTransaction)
                `shouldBe` Left InvalidDappRequest
        it "classifies legacy Genesis and MIR certificates as deprecated" $
            mapM_
                ( \transaction ->
                    fmap (const ()) (decodeDappTx $ ApiDappHex transaction)
                        `shouldBe` Left DappDeprecatedCertificateError
                )
                [legacyGenesisTransaction, legacyMirTransaction]
        it "extracts only valid matching pending stake certificate effects" $ do
            let stakeCredential = KeyHashObj $ coerce $ witnessKeyHash proofHash
                foreignCredential =
                    KeyHashObj $ coerce $ witnessKeyHash $ BS.replicate 28 0x99
                register stakeCred =
                    ConwayTxCertDeleg $ ConwayRegCert stakeCred Strict.SNothing
                registerAndDelegate stakeCred =
                    ConwayTxCertDeleg
                        $ ConwayRegDelegCert stakeCred
                            (DelegVote DRepAlwaysAbstain) (Coin 1)
                deregister stakeCred =
                    ConwayTxCertDeleg $ ConwayUnRegCert stakeCred Strict.SNothing
            stakeRegistrationEffects proofHash
                [ (True, [register stakeCredential])
                , (True, [registerAndDelegate stakeCredential])
                , (False, [deregister stakeCredential])
                , (True, [register foreignCredential])
                , (True, [deregister stakeCredential])
                ]
                `shouldBe`
                    [ RegisterStakeKey
                    , RegisterStakeKey
                    , DeregisterStakeKey
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
            mapM_
                (\claims -> fmap (validateContextToken key claims) encoded `shouldBe` Right False)
                [ changedClaims
                , ContextTokenClaims
                    (BS.replicate 16 0x44)
                    1
                    "other-wallet"
                    (BS.replicate 32 0x01)
                    digestGolden
                , ContextTokenClaims
                    (BS.replicate 16 0x44)
                    1
                    "wallet-test"
                    (BS.replicate 32 0x02)
                    digestGolden
                , ContextTokenClaims
                    (BS.replicate 16 0x44)
                    1
                    "wallet-test"
                    (BS.replicate 32 0x01)
                    (BS.replicate 32 0x03)
                ]
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

invalidWitnessRequests :: [(String, BL8.ByteString)]
invalidWitnessRequests =
    [ ("an unknown field", BL8.init base <> ",\"unexpected\":true}")
    , ("a duplicate field", "{\"revision\":1," <> BL8.tail base)
    , ("an empty batch", validWitnessRequest [])
    , ("a 51-item batch", validWitnessRequest $ replicate 51 witnessItem)
    , ( "a transaction CBOR value over 64 KiB"
      , validWitnessRequest
            [ApiDappWitnessSignItem (ApiDappHex $ BS.replicate 65537 0) True]
      )
    ]
  where
    base = validWitnessRequest [witnessItem]

validWitnessRequest :: [ApiDappWitnessSignItem] -> BL8.ByteString
validWitnessRequest items =
    Aeson.encode
        $ ApiDappWitnessSignRequest
            1
            (validDappWitnessContext [ApiDappHex acceptedTransaction])
            items
            (ApiT $ Passphrase "test")

witnessItem :: ApiDappWitnessSignItem
witnessItem = ApiDappWitnessSignItem (ApiDappHex "\xa0") True

dappWalletId :: Text
dappWalletId = Text.replicate 40 "a"

dappNetwork :: ApiDappContextNetwork
dappNetwork = ApiDappContextNetwork 0 1 (ApiDappHex $ BS.replicate 32 0)

dappGeneration :: ByteString
dappGeneration = BS.replicate 16 0x44

validDappWitnessContext
    :: [ApiDappHex]
    -> ApiDappTransactionContextResponse
validDappWitnessContext requestedTransactions =
    ApiDappTransactionContextResponse
        1
        dappWalletId
        dappNetwork
        ApiDappChainPointGenesis
        (ApiDappWord64 0)
        (ApiDappWord64 0)
        "conway"
        (ApiDappProtocolVersion 0 0)
        (ApiDappHex "\xa0")
        (ApiDappVolatileDelta ApiDappChainPointGenesis [])
        []
        (ApiDappPendingOverlay [] [] [])
        []
        []
        (ApiDappBatchOverlay [] [])
        (ApiDappHex <$> encodedRecords)
        (ApiDappHex digest)
        (ApiDappHex token)
  where
    recordValues = [ProtocolRecord 0 1 0 0 "\xa0"]
    encodedRecords = either error id $ canonicalContextRecords recordValues
    digest =
        either error id
            $ computeContextDigest
            $ ContextDigestInput
                (T.encodeUtf8 dappWalletId)
                (BS.replicate 32 0)
                ApiDappChainPointGenesis
                0
                0
                (getApiDappHex <$> requestedTransactions)
                recordValues
    token =
        either error id
            $ encodeContextToken
                key
                (ContextTokenClaims dappGeneration 1 (T.encodeUtf8 dappWalletId) (BS.replicate 32 0) digest)

tamperContextToken
    :: ApiDappHex
    -> ApiDappTransactionContextResponse
    -> ApiDappTransactionContextResponse
tamperContextToken token response = response{contextToken = token}

mapDappBindings
    :: (Text -> Text)
    -> (ApiDappContextNetwork -> ApiDappContextNetwork)
    -> ApiDappTransactionContextResponse
    -> ApiDappTransactionContextResponse
mapDappBindings alterWallet alterNetwork
    ( ApiDappTransactionContextResponse
        oldRevision
        currentWallet
        currentNetwork
        oldChainPoint
        oldWalletGeneration
        oldPendingGeneration
        oldEra
        oldProtocolVersion
        oldProtocolParametersCbor
        oldVolatileDelta
        oldOutputs
        oldPendingOverlay
        oldOwnership
        oldRequiredWalletProofs
        oldBatchOverlay
        oldRecords
        oldContextDigest
        oldContextToken
    ) =
        ApiDappTransactionContextResponse
            oldRevision
            (alterWallet currentWallet)
            (alterNetwork currentNetwork)
            oldChainPoint
            oldWalletGeneration
            oldPendingGeneration
            oldEra
            oldProtocolVersion
            oldProtocolParametersCbor
            oldVolatileDelta
            oldOutputs
            oldPendingOverlay
            oldOwnership
            oldRequiredWalletProofs
            oldBatchOverlay
            oldRecords
            oldContextDigest
            oldContextToken

tamperContextWallet
    :: Text
    -> ApiDappTransactionContextResponse
    -> ApiDappTransactionContextResponse
tamperContextWallet newWallet = mapDappBindings (const newWallet) id

tamperContextNetwork
    :: ApiDappContextNetwork
    -> ApiDappTransactionContextResponse
    -> ApiDappTransactionContextResponse
tamperContextNetwork newNetwork = mapDappBindings id (const newNetwork)

tamperContextRequestTransactions
    :: [ApiDappHex]
    -> ApiDappTransactionContextRequest
    -> ApiDappTransactionContextRequest
tamperContextRequestTransactions newTransactions
    (ApiDappTransactionContextRequest oldRevision oldNetwork _) =
        ApiDappTransactionContextRequest oldRevision oldNetwork newTransactions

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

dataCredential :: ByteString
dataCredential = BS.replicate 28 0xaa

enterpriseAddress, rewardAddress, scriptPaymentAddress, scriptStakeAddress :: ByteString
enterpriseAddress = BS.cons 0x60 dataCredential
rewardAddress = BS.cons 0xe0 dataCredential
scriptPaymentAddress = BS.cons 0x70 dataCredential
scriptStakeAddress = BS.cons 0xf0 dataCredential
nonUtf8Payload :: ByteString
nonUtf8Payload = BS.pack [0x00, 0xff, 0x80, 0x41]

dataSignRequest :: ByteString -> ApiDappDataSignRequest
dataSignRequest rawAddress =
    ApiDappDataSignRequest
        1
        dappNetwork
        (ApiDappHex rawAddress)
        (ApiDappHex nonUtf8Payload)
        (ApiT $ Passphrase "test")

validDataSignRequest :: BL8.ByteString
validDataSignRequest = BL8.pack
    "{\"revision\":1,\"network\":{\"network_id\":0,\"network_magic\":42,\"genesis_hash\":\"0000000000000000000000000000000000000000000000000000000000000000\"},\"address\":\"60aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",\"payload\":\"00ff\",\"passphrase\":\"pass\"}"

validDataSignResponse :: BL8.ByteString
validDataSignResponse = BL8.pack
    "{\"revision\":1,\"credential_kind\":\"payment\",\"credential\":\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",\"cose_sign1\":\"80\",\"cose_key\":\"a0\"}"

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

legacyGenesisTransaction :: ByteString
legacyGenesisTransaction =
    legacyTransaction
        $ "8405581c" <> Text.replicate 56 "1"
            <> "581c" <> Text.replicate 56 "2"
            <> "5820" <> Text.replicate 64 "3"

legacyMirTransaction :: ByteString
legacyMirTransaction = legacyTransaction "82068200a0"

legacyTransaction :: Text -> ByteString
legacyTransaction certificate =
    hex
        $ "83a50081825820" <> Text.replicate 64 "0"
            <> "00018182581d60" <> Text.replicate 56 "a"
            <> "00020003000481" <> certificate <> "a0f6"

hex :: Text -> ByteString
hex =
    either (error . show) id
        . BAE.convertFromBase BAE.Base16
        . T.encodeUtf8

isLeft :: Either a b -> Bool
isLeft = either (const True) (const False)

isRight :: Either a b -> Bool
isRight = not . isLeft
