{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Api.Types.Dapp.Context
    ( DappJSON
    , ApiDappTransactionContextRequest (..)
    , ApiDappTransactionContextResponse (..)
    , ApiDappContextNetwork (..)
    , ApiDappChainPoint (..)
    , ApiDappProtocolVersion (..)
    , ApiDappVolatileDelta (..)
    , ApiDappContextOutput (..)
    , ApiDappOutpoint (..)
    , ApiDappProvenance (..)
    , ApiDappRole (..)
    , ApiDappPendingState (..)
    , ApiDappPendingOverlay (..)
    , ApiDappPendingTransaction (..)
    , ApiDappCredentialKind (..)
    , ApiDappOwnershipKind (..)
    , ApiDappProofKind (..)
    , ApiDappOwnership (..)
    , ApiDappRequiredWalletProof (..)
    , ApiDappDependency (..)
    , ApiDappConflict (..)
    , ApiDappBatchOverlay (..)
    , ApiDappHex (..)
    , ApiDappWord64 (..)
    , decodeTransactionContextRequest
    , ContextRecord (..)
    , ContextDigestInput (..)
    , ContextTokenClaims (..)
    , encodeContextRecord
    , canonicalContextRecords
    , computeContextDigest
    , encodeContextToken
    , decodeContextTokenClaims
    , validateContextToken
    ) where

import Cardano.Wallet.Api.Lib.Options
    ( explicitNothingRecordTypeOptions
    , strictRecordTypeOptions
    )
import Control.Monad
    ( unless
    , when
    )
import Cryptography.Hash.Blake
    ( blake2b256
    )
import Cryptography.Hash.Core
    ( HMAC
    , SHA256
    , hmac
    )
import Data.Aeson
    ( FromJSON (..)
    , ToJSON (..)
    , Value (String)
    , eitherDecode
    , genericParseJSON
    , genericToJSON
    , withText
    )
import Data.Aeson.Types
    ( Parser
    )
import Data.Binary.Get
    ( Get
    , getByteString
    , getRemainingLazyByteString
    , getWord32be
    , getWord8
    , runGetOrFail
    )
import Data.Binary.Put
    ( Put
    , putByteString
    , putWord32be
    , putWord64be
    , putWord8
    , runPut
    )
import Data.ByteString
    ( ByteString
    )
import Data.Char
    ( isDigit
    , isHexDigit
    )
import Data.List
    ( group
    , sort
    )
import Data.Text
    ( Text
    )
import Data.Tuple
    ( swap
    )
import Data.Word
    ( Word32
    , Word64
    , Word8
    )
import GHC.Generics
    ( Generic
    )
import Network.HTTP.Media
    ( (//)
    )
import Servant.API.ContentTypes
    ( Accept (..)
    , MimeRender (..)
    , MimeUnrender (..)
    )
import Prelude

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteArray as BA
import qualified Data.ByteArray.Encoding as BAE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

data DappJSON

instance Accept DappJSON where
    contentType _ = "application" // "json"

instance MimeUnrender DappJSON ApiDappTransactionContextRequest where
    mimeUnrender _ = decodeTransactionContextRequest

instance MimeRender DappJSON ApiDappTransactionContextRequest where
    mimeRender _ = Aeson.encode

instance MimeUnrender DappJSON ApiDappTransactionContextResponse where
    mimeUnrender _ = Aeson.eitherDecode

instance MimeRender DappJSON ApiDappTransactionContextResponse where
    mimeRender _ = Aeson.encode

newtype ApiDappHex = ApiDappHex {getApiDappHex :: ByteString}
    deriving (Eq, Ord, Show)

newtype ApiDappWord64 = ApiDappWord64 {getApiDappWord64 :: Word64}
    deriving (Eq, Ord, Show)

data ApiDappContextNetwork = ApiDappContextNetwork
    { networkId :: !Word8
    , networkMagic :: !Word32
    , genesisHash :: !ApiDappHex
    }
    deriving (Eq, Generic, Show)

data ApiDappTransactionContextRequest = ApiDappTransactionContextRequest
    { revision :: !Word32
    , network :: !ApiDappContextNetwork
    , transactions :: ![ApiDappHex]
    }
    deriving (Eq, Generic, Show)

data ApiDappChainPoint
    = ApiDappChainPointGenesis
    | ApiDappChainPointBlock
        { slot :: !ApiDappWord64
        , blockHash :: !ApiDappHex
        }
    deriving (Eq, Show)

data ApiDappProtocolVersion = ApiDappProtocolVersion
    { major :: !Word32
    , minor :: !Word32
    }
    deriving (Eq, Generic, Show)

data ApiDappVolatileDelta = ApiDappVolatileDelta
    { point :: !ApiDappChainPoint
    , nodeTransactionInputs :: ![ApiDappHex]
    }
    deriving (Eq, Generic, Show)

data ApiDappOutpoint = ApiDappOutpoint
    { transactionId :: !ApiDappHex
    , index :: !Word32
    }
    deriving (Eq, Generic, Ord, Show)

data ApiDappProvenance = Earlier | Pending | Node
    deriving (Eq, Ord, Show)

data ApiDappRole = Normal | Collateral | Reference | WalletSnapshot
    deriving (Eq, Ord, Show)

data ApiDappPendingState = None | OutcomeUnknown
    deriving (Eq, Ord, Show)

data ApiDappContextOutput = ApiDappContextOutput
    { outpoint :: !ApiDappOutpoint
    , transactionInputCbor :: !ApiDappHex
    , sourceTransactionOutputCbor :: !ApiDappHex
    , canonicalTransactionOutputCbor :: !ApiDappHex
    , transactionUnspentOutputCbor :: !ApiDappHex
    , provenance :: ![ApiDappProvenance]
    , roles :: ![ApiDappRole]
    , walletMember :: !Bool
    , pendingState :: !ApiDappPendingState
    }
    deriving (Eq, Generic, Show)

data ApiDappPendingTransaction = ApiDappPendingTransaction
    { transactionId :: !ApiDappHex
    , state :: !ApiDappPendingState
    , transactionCbor :: !ApiDappHex
    , normalInputs :: ![ApiDappOutpoint]
    , collateralInputs :: ![ApiDappOutpoint]
    , expirySlot :: !(Maybe ApiDappWord64)
    }
    deriving (Eq, Generic, Show)

data ApiDappPendingOverlay = ApiDappPendingOverlay
    { transactions :: ![ApiDappPendingTransaction]
    , spentWalletInputs :: ![ApiDappOutpoint]
    , producedWalletOutputs :: ![ApiDappOutpoint]
    }
    deriving (Eq, Generic, Show)

data ApiDappCredentialKind = PaymentCredential | StakeCredential | PolicyCredential
    deriving (Eq, Ord, Show)

data ApiDappOwnershipKind = Unowned | OwnedKey | ScriptOwned
    deriving (Eq, Ord, Show)

data ApiDappProofKind
    = NormalInputProof
    | CollateralProof
    | WithdrawalProof
    | CertificateProof
    | RequiredSignerProof
    | NativeScriptProof
    | PolicyProof
    deriving (Bounded, Enum, Eq, Ord, Show)

data ApiDappOwnership = ApiDappOwnership
    { credentialKind :: !ApiDappCredentialKind
    , credential :: !ApiDappHex
    , ownership :: !ApiDappOwnershipKind
    , derivationPath :: ![Word32]
    , proofKinds :: ![ApiDappProofKind]
    }
    deriving (Eq, Generic, Ord, Show)

data ApiDappRequiredWalletProof = ApiDappRequiredWalletProof
    { transactionIndex :: !Word32
    , proofKind :: !ApiDappProofKind
    , credentialKind :: !ApiDappCredentialKind
    , credential :: !ApiDappHex
    , required :: !Bool
    }
    deriving (Eq, Generic, Ord, Show)

data ApiDappDependency = ApiDappDependency
    { transactionIndex :: !Word32
    , inputRole :: !ApiDappRole
    , outpoint :: !ApiDappOutpoint
    , source :: !ApiDappProvenance
    , sourceTransactionIndex :: !(Maybe Word32)
    }
    deriving (Eq, Generic, Ord, Show)

data ApiDappConflict = ApiDappConflict
    { transactionIndex :: !Word32
    , inputRole :: !ApiDappRole
    , outpoint :: !ApiDappOutpoint
    , earlierTransactionIndex :: !Word32
    }
    deriving (Eq, Generic, Ord, Show)

data ApiDappBatchOverlay = ApiDappBatchOverlay
    { dependencies :: ![ApiDappDependency]
    , conflicts :: ![ApiDappConflict]
    }
    deriving (Eq, Generic, Show)

data ApiDappTransactionContextResponse = ApiDappTransactionContextResponse
    { revision :: !Word32
    , walletId :: !Text
    , network :: !ApiDappContextNetwork
    , chainPoint :: !ApiDappChainPoint
    , walletGeneration :: !ApiDappWord64
    , pendingGeneration :: !ApiDappWord64
    , era :: !Text
    , protocolVersion :: !ApiDappProtocolVersion
    , protocolParametersCbor :: !ApiDappHex
    , volatileDelta :: !ApiDappVolatileDelta
    , outputs :: ![ApiDappContextOutput]
    , pendingOverlay :: !ApiDappPendingOverlay
    , ownership :: ![ApiDappOwnership]
    , requiredWalletProofs :: ![ApiDappRequiredWalletProof]
    , batchOverlay :: !ApiDappBatchOverlay
    , records :: ![ApiDappHex]
    , contextDigest :: !ApiDappHex
    , contextToken :: !ApiDappHex
    }
    deriving (Eq, Generic, Show)

instance FromJSON ApiDappHex where
    parseJSON = withText "lowercase hexadecimal bytes" $ \value -> do
        unless (isLowerHex value && even (T.length value))
            $ fail "expected lowercase even-length hexadecimal"
        bytes <-
            either fail pure $ BAE.convertFromBase BAE.Base16 (T.encodeUtf8 value)
        pure $ ApiDappHex bytes

instance ToJSON ApiDappHex where
    toJSON =
        String . T.decodeUtf8 . BAE.convertToBase BAE.Base16 . getApiDappHex

instance FromJSON ApiDappWord64 where
    parseJSON = withText "Word64 decimal string" $ \value -> do
        unless (canonicalDecimal value)
            $ fail "expected canonical Word64 decimal string"
        case reads (T.unpack value) of
            [(word, "")] -> pure $ ApiDappWord64 word
            _ -> fail "decimal value does not fit Word64"

instance ToJSON ApiDappWord64 where
    toJSON = String . T.pack . show . getApiDappWord64

instance FromJSON ApiDappContextNetwork where
    parseJSON value = do
        result@ApiDappContextNetwork{networkId, genesisHash} <-
            genericParseJSON strictRecordTypeOptions value
        unless (networkId <= 1) $ fail "network_id must be 0 or 1"
        requireLength "genesis_hash" 32 genesisHash
        pure result

instance ToJSON ApiDappContextNetwork where
    toJSON = genericToJSON strictRecordTypeOptions

instance FromJSON ApiDappTransactionContextRequest where
    parseJSON value = do
        result@ApiDappTransactionContextRequest{revision, transactions} <-
            genericParseJSON strictRecordTypeOptions value
        unless (revision == 1) $ fail "revision must be 1"
        unless (length transactions >= 1 && length transactions <= 50)
            $ fail "transactions must contain 1 to 50 entries"
        mapM_ (requireLengthBetween "transaction" 1 65536) transactions
        pure result

instance ToJSON ApiDappTransactionContextRequest where
    toJSON = genericToJSON strictRecordTypeOptions

decodeTransactionContextRequest
    :: BL.ByteString -> Either String ApiDappTransactionContextRequest
decodeTransactionContextRequest bytes =
    rejectDuplicateFields (BL.toStrict bytes) >> eitherDecode bytes

instance FromJSON ApiDappChainPoint where
    parseJSON = Aeson.withObject "chain point" $ \object -> do
        kind <- object Aeson..: "kind"
        case kind :: Text of
            "genesis" -> do
                rejectKeys object ["kind"]
                pure ApiDappChainPointGenesis
            "block" -> do
                rejectKeys object ["kind", "slot", "block_hash"]
                slot <- object Aeson..: "slot"
                blockHash <- object Aeson..: "block_hash"
                requireLength "block_hash" 32 blockHash
                pure ApiDappChainPointBlock{slot, blockHash}
            _ -> fail "kind must be genesis or block"

instance ToJSON ApiDappChainPoint where
    toJSON ApiDappChainPointGenesis = Aeson.object ["kind" Aeson..= ("genesis" :: Text)]
    toJSON ApiDappChainPointBlock{slot, blockHash} =
        Aeson.object
            [ "kind" Aeson..= ("block" :: Text)
            , "slot" Aeson..= slot
            , "block_hash" Aeson..= blockHash
            ]

instance FromJSON ApiDappProtocolVersion where
    parseJSON = genericParseJSON strictRecordTypeOptions
instance ToJSON ApiDappProtocolVersion where
    toJSON = genericToJSON strictRecordTypeOptions

instance FromJSON ApiDappVolatileDelta where
    parseJSON value = do
        result@ApiDappVolatileDelta{nodeTransactionInputs} <-
            genericParseJSON strictRecordTypeOptions value
        requireSortedUnique "node_transaction_inputs" nodeTransactionInputs
        mapM_ (requireNonEmpty "node transaction input") nodeTransactionInputs
        pure result
instance ToJSON ApiDappVolatileDelta where
    toJSON = genericToJSON strictRecordTypeOptions

instance FromJSON ApiDappOutpoint where
    parseJSON value = do
        result@ApiDappOutpoint{transactionId} <-
            genericParseJSON strictRecordTypeOptions value
        requireLength "transaction_id" 32 transactionId
        pure result
instance ToJSON ApiDappOutpoint where
    toJSON = genericToJSON strictRecordTypeOptions

instance FromJSON ApiDappProvenance where
    parseJSON = parseEnum "provenance" [("earlier", Earlier), ("pending", Pending), ("node", Node)]
instance ToJSON ApiDappProvenance where
    toJSON = enumJson [(Earlier, "earlier"), (Pending, "pending"), (Node, "node")]

instance FromJSON ApiDappRole where
    parseJSON =
        parseEnum
            "role"
            [ ("normal", Normal)
            , ("collateral", Collateral)
            , ("reference", Reference)
            , ("wallet_snapshot", WalletSnapshot)
            ]
instance ToJSON ApiDappRole where
    toJSON =
        enumJson
            [ (Normal, "normal")
            , (Collateral, "collateral")
            , (Reference, "reference")
            , (WalletSnapshot, "wallet_snapshot")
            ]

instance FromJSON ApiDappPendingState where
    parseJSON =
        parseEnum
            "pending state"
            [("none", None), ("outcome_unknown", OutcomeUnknown)]
instance ToJSON ApiDappPendingState where
    toJSON = enumJson [(None, "none"), (OutcomeUnknown, "outcome_unknown")]

instance FromJSON ApiDappContextOutput where
    parseJSON value = do
        result@ApiDappContextOutput
            { transactionInputCbor
            , sourceTransactionOutputCbor
            , canonicalTransactionOutputCbor
            , transactionUnspentOutputCbor
            , provenance
            , roles
            } <-
            genericParseJSON strictRecordTypeOptions value
        mapM_
            (requireNonEmpty "CBOR")
            [ transactionInputCbor
            , sourceTransactionOutputCbor
            , canonicalTransactionOutputCbor
            , transactionUnspentOutputCbor
            ]
        requireFixedSubset "provenance" [Earlier, Pending, Node] provenance
        requireFixedSubset
            "roles"
            [Normal, Collateral, Reference, WalletSnapshot]
            roles
        pure result
instance ToJSON ApiDappContextOutput where
    toJSON = genericToJSON strictRecordTypeOptions

instance FromJSON ApiDappPendingTransaction where
    parseJSON value = do
        result@ApiDappPendingTransaction
            { transactionId
            , state
            , transactionCbor
            , normalInputs
            , collateralInputs
            } <-
            genericParseJSON explicitStrictOptions value
        requireLength "transaction_id" 32 transactionId
        unless (state == OutcomeUnknown)
            $ fail "pending transaction state must be outcome_unknown"
        requireNonEmpty "transaction_cbor" transactionCbor
        requireSortedUnique "normal_inputs" normalInputs
        requireSortedUnique "collateral_inputs" collateralInputs
        pure result
instance ToJSON ApiDappPendingTransaction where
    toJSON = genericToJSON explicitStrictOptions

instance FromJSON ApiDappPendingOverlay where
    parseJSON value = do
        result@ApiDappPendingOverlay
            { transactions
            , spentWalletInputs
            , producedWalletOutputs
            } <-
            genericParseJSON strictRecordTypeOptions value
        requireSortedUnique
            "pending transactions"
            ( (\ApiDappPendingTransaction{transactionId} -> transactionId)
                <$> transactions
            )
        requireSortedUnique "spent_wallet_inputs" spentWalletInputs
        unless (null producedWalletOutputs)
            $ fail "produced_wallet_outputs must be empty in revision 1"
        pure result
instance ToJSON ApiDappPendingOverlay where
    toJSON = genericToJSON strictRecordTypeOptions

instance FromJSON ApiDappCredentialKind where
    parseJSON = parseEnum "credential kind" [("payment", PaymentCredential), ("stake", StakeCredential), ("policy", PolicyCredential)]
instance ToJSON ApiDappCredentialKind where
    toJSON = enumJson [(PaymentCredential, "payment"), (StakeCredential, "stake"), (PolicyCredential, "policy")]

instance FromJSON ApiDappOwnershipKind where
    parseJSON = parseEnum "ownership" [("unowned", Unowned), ("owned_key", OwnedKey), ("script", ScriptOwned)]
instance ToJSON ApiDappOwnershipKind where
    toJSON = enumJson [(Unowned, "unowned"), (OwnedKey, "owned_key"), (ScriptOwned, "script")]

instance FromJSON ApiDappProofKind where
    parseJSON = parseEnum "proof kind" proofKindJson
instance ToJSON ApiDappProofKind where
    toJSON = enumJson $ swap <$> proofKindJson

instance FromJSON ApiDappOwnership where
    parseJSON value = do
        result@ApiDappOwnership{credentialKind, credential, ownership, derivationPath, proofKinds} <- genericParseJSON strictRecordTypeOptions value
        requireLength "credential" 28 credential
        unless ((ownership == OwnedKey) == not (null derivationPath)) $ fail "invalid ownership path"
        when (ownership == OwnedKey) $ unless (validPath credentialKind derivationPath) $ fail "invalid derivation path"
        requireFixedSubsetAllowEmpty "proof_kinds" proofKindOrder proofKinds
        pure result
instance ToJSON ApiDappOwnership where
    toJSON = genericToJSON strictRecordTypeOptions

instance FromJSON ApiDappRequiredWalletProof where
    parseJSON value = do
        result@ApiDappRequiredWalletProof{credential} <- genericParseJSON strictRecordTypeOptions value
        requireLength "credential" 28 credential
        pure result
instance ToJSON ApiDappRequiredWalletProof where
    toJSON = genericToJSON strictRecordTypeOptions

instance FromJSON ApiDappDependency where
    parseJSON value = do
        result@ApiDappDependency{inputRole, source, sourceTransactionIndex} <- genericParseJSON explicitStrictOptions value
        unless (inputRole `elem` [Normal, Collateral, Reference]) $ fail "invalid dependency role"
        unless (source `elem` [Earlier, Pending]) $ fail "invalid dependency source"
        unless ((source == Earlier) == maybe False (const True) sourceTransactionIndex) $ fail "invalid source transaction index"
        pure result
instance ToJSON ApiDappDependency where
    toJSON = genericToJSON explicitStrictOptions

instance FromJSON ApiDappConflict where
    parseJSON value = do
        result@ApiDappConflict{inputRole} <- genericParseJSON strictRecordTypeOptions value
        unless (inputRole `elem` [Normal, Collateral]) $ fail "invalid conflict role"
        pure result
instance ToJSON ApiDappConflict where
    toJSON = genericToJSON strictRecordTypeOptions

instance FromJSON ApiDappBatchOverlay where
    parseJSON value = do
        result@ApiDappBatchOverlay{dependencies, conflicts} <- genericParseJSON strictRecordTypeOptions value
        requireSortedUnique "dependencies" dependencies
        requireSortedUnique "conflicts" conflicts
        pure result
instance ToJSON ApiDappBatchOverlay where
    toJSON = genericToJSON strictRecordTypeOptions

instance FromJSON ApiDappTransactionContextResponse where
    parseJSON value = do
        result@ApiDappTransactionContextResponse
            { revision
            , walletId
            , network
            , chainPoint
            , era
            , protocolVersion
            , protocolParametersCbor
            , volatileDelta
            , outputs
            , pendingOverlay
            , ownership
            , requiredWalletProofs
            , batchOverlay = ApiDappBatchOverlay{dependencies, conflicts}
            , records
            , contextDigest
            , contextToken
            } <-
            genericParseJSON strictRecordTypeOptions value
        unless (revision == 1) $ fail "revision must be 1"
        unless (isLowerHexOfLength 40 walletId)
            $ fail "wallet_id must be 40 lowercase hexadecimal characters"
        unless (era == "conway") $ fail "era must be conway"
        requireNonEmpty "protocol_parameters_cbor" protocolParametersCbor
        requireSortedUnique "records" records
        requireSortedUnique "ownership" ownership
        requireSortedUnique "required_wallet_proofs" requiredWalletProofs
        requireSortedUnique "dependencies" dependencies
        requireSortedUnique "conflicts" conflicts
        mapM_ (requireNonEmpty "record") records
        requireLength "context_digest" 32 contextDigest
        validateResponseBindings
            walletId
            network
            chainPoint
            protocolVersion
            protocolParametersCbor
            volatileDelta
            outputs
            pendingOverlay
            ownership
            requiredWalletProofs
            records
            contextDigest
            contextToken
        pure result
instance ToJSON ApiDappTransactionContextResponse where
    toJSON = genericToJSON strictRecordTypeOptions

data ContextRecord
    = FullOutputRecord
        { outpoint :: !ApiDappOutpoint
        , provenance :: ![ApiDappProvenance]
        , roles :: ![ApiDappRole]
        , walletMember :: !Bool
        , pendingState :: !ApiDappPendingState
        , exactLedgerTxOutCbor :: !ByteString
        }
    | OwnershipRecord
        { credentialKind :: !ApiDappCredentialKind
        , credential :: !ByteString
        , ownership :: !ApiDappOwnershipKind
        , derivationPath :: ![Word32]
        , proofKinds :: ![ApiDappProofKind]
        }
    | ProtocolRecord
        { networkId :: !Word8
        , networkMagic :: !Word32
        , protocolMajor :: !Word32
        , protocolMinor :: !Word32
        , protocolParametersCbor :: !ByteString
        }
    | PendingTransactionRecord
        { transactionId :: !ByteString
        , exactSealedTransaction :: !ByteString
        , normalInputs :: ![ApiDappOutpoint]
        , collateralInputs :: ![ApiDappOutpoint]
        , expirySlot :: !(Maybe Word64)
        }
    | RequiredProofRecord
        { transactionIndex :: !Word32
        , proofKind :: !ApiDappProofKind
        , credentialKind :: !ApiDappCredentialKind
        , credential :: !ByteString
        , required :: !Bool
        }
    deriving (Eq, Show)

data ContextDigestInput = ContextDigestInput
    { walletId :: !ByteString
    , genesisHash :: !ByteString
    , chainPoint :: !ApiDappChainPoint
    , walletGeneration :: !Word64
    , pendingGeneration :: !Word64
    , transactions :: ![ByteString]
    , records :: ![ContextRecord]
    }
    deriving (Eq, Show)

data ContextTokenClaims = ContextTokenClaims
    { processGeneration :: !ByteString
    , capabilityRevision :: !Word32
    , walletId :: !ByteString
    , genesisHash :: !ByteString
    , contextDigest :: !ByteString
    }
    deriving (Eq, Show)

encodeContextRecord :: ContextRecord -> Either String ByteString
encodeContextRecord =
    fmap strictPut . \case
        FullOutputRecord
            { outpoint
            , provenance
            , roles
            , walletMember
            , pendingState
            , exactLedgerTxOutCbor
            } -> do
                outpointBytes <- encodeOutpoint outpoint
                provenanceBits <- encodeProvenance provenance
                roleBits <- encodeRoles roles
                requireBytes "exact ledger TxOut CBOR" exactLedgerTxOutCbor
                pure $ record 0x01 $ do
                    putByteString outpointBytes
                    putWord8 provenanceBits
                    putWord8 roleBits
                    putBool walletMember
                    putWord8 $ pendingStateCode pendingState
                    putBytes exactLedgerTxOutCbor
        OwnershipRecord{credentialKind, credential, ownership, derivationPath, proofKinds} -> do
            requireByteLength "credential" 28 credential
            unless ((ownership == OwnedKey) == not (null derivationPath)) $ Left "invalid ownership path"
            when (ownership == OwnedKey) $ unless (validPath credentialKind derivationPath) $ Left "invalid derivation path"
            requireCanonicalSubsetAllowEmpty "proof kinds" proofKindOrder proofKinds
            pure $ record 0x02 $ do
                putWord8 $ credentialKindCode credentialKind
                putBytes credential
                putWord8 $ ownershipCode ownership
                putWord32be $ fromIntegral $ length derivationPath
                mapM_ putWord32be derivationPath
                putWord32be $ sum $ proofBit <$> proofKinds
        ProtocolRecord
            { networkId
            , networkMagic
            , protocolMajor
            , protocolMinor
            , protocolParametersCbor
            } -> do
                unless (networkId <= 1) $ Left "network id must be 0 or 1"
                requireBytes "protocol parameters CBOR" protocolParametersCbor
                pure $ record 0x03 $ do
                    putBytes "conway"
                    putWord8 networkId
                    putWord32be networkMagic
                    putWord32be protocolMajor
                    putWord32be protocolMinor
                    putBytes protocolParametersCbor
        PendingTransactionRecord
            { transactionId
            , exactSealedTransaction
            , normalInputs
            , collateralInputs
            , expirySlot
            } -> do
                requireByteLength "transaction id" 32 transactionId
                requireBytes "exact sealed transaction" exactSealedTransaction
                normal <- canonicalOutpoints "normal inputs" normalInputs
                collateral <- canonicalOutpoints "collateral inputs" collateralInputs
                pure $ record 0x07 $ do
                    putByteString transactionId
                    putWord8 4
                    putBytes exactSealedTransaction
                    putVector normal
                    putVector collateral
                    maybe
                        (putBool False)
                        (\slot -> putBool True >> putWord64be slot)
                        expirySlot
        RequiredProofRecord{transactionIndex, proofKind, credentialKind, credential, required} -> do
            requireByteLength "credential" 28 credential
            pure $ record 0x06 $ do
                putWord32be transactionIndex
                putWord8 $ proofKindCode proofKind
                putWord8 $ credentialKindCode credentialKind
                putBytes credential
                putBool required

canonicalContextRecords
    :: [ContextRecord] -> Either String [ByteString]
canonicalContextRecords values = do
    encoded <- sort <$> mapM encodeContextRecord values
    when (hasDuplicates encoded) $ Left "duplicate context record"
    pure encoded

computeContextDigest :: ContextDigestInput -> Either String ByteString
computeContextDigest
    ContextDigestInput
        { walletId
        , genesisHash
        , chainPoint
        , walletGeneration
        , pendingGeneration
        , transactions
        , records
        } = do
        requireBytes "wallet id" walletId
        requireByteLength "genesis hash" 32 genesisHash
        unless (length transactions >= 1 && length transactions <= 50)
            $ Left "transactions must contain 1 to 50 entries"
        mapM_ (requireByteLengthBetween "transaction" 1 65536) transactions
        canonicalRecords <- canonicalContextRecords records
        point <- encodeChainPoint chainPoint
        pure $ blake2b256 $ strictPut $ do
            putByteString "daedalus-dapp-context-v1"
            putBytes walletId
            putBytes genesisHash
            putByteString point
            putWord64be walletGeneration
            putWord64be pendingGeneration
            putVector transactions
            putVector canonicalRecords

encodeContextToken
    :: ByteString -> ContextTokenClaims -> Either String ByteString
encodeContextToken key claims = do
    requireByteLength "HMAC key" 32 key
    payload <- encodeTokenPayload claims
    let mac = hmacSha256 key $ strictPut $ do
            putByteString "daedalus-dapp-context-token-v1"
            putBytes payload
    pure $ payload <> mac

decodeContextTokenClaims
    :: ByteString -> Either String ContextTokenClaims
decodeContextTokenClaims token
    | BS.length token < 32 = Left "context token is shorter than its MAC"
    | otherwise = parseTokenPayload $ BS.take (BS.length token - 32) token

validateContextToken
    :: ByteString -> ContextTokenClaims -> ByteString -> Bool
validateContextToken key expected token
    | BS.length key /= 32 || BS.length token < 32 = False
    | otherwise =
        let (payload, suppliedMac) = BS.splitAt (BS.length token - 32) token
            expectedMac = hmacSha256 key $ strictPut $ do
                putByteString "daedalus-dapp-context-token-v1"
                putBytes payload
        in  BA.constEq suppliedMac expectedMac
                && parseTokenPayload payload == Right expected

validateResponseBindings
    :: Text
    -> ApiDappContextNetwork
    -> ApiDappChainPoint
    -> ApiDappProtocolVersion
    -> ApiDappHex
    -> ApiDappVolatileDelta
    -> [ApiDappContextOutput]
    -> ApiDappPendingOverlay
    -> [ApiDappOwnership]
    -> [ApiDappRequiredWalletProof]
    -> [ApiDappHex]
    -> ApiDappHex
    -> ApiDappHex
    -> Parser ()
validateResponseBindings
    walletId
    ApiDappContextNetwork
        { networkId
        , networkMagic
        , genesisHash = ApiDappHex genesisHash
        }
    chainPoint
    ApiDappProtocolVersion{major, minor}
    (ApiDappHex protocolParametersCbor)
    ApiDappVolatileDelta{point, nodeTransactionInputs}
    outputs
    ApiDappPendingOverlay{transactions}
    ownership
    requiredWalletProofs
    records
    (ApiDappHex contextDigest)
    (ApiDappHex contextToken) = do
        unless (point == chainPoint)
            $ fail "volatile_delta.point must equal chain_point"
        let expectedNodeInputs =
                sort
                    [ transactionInputCbor
                    | ApiDappContextOutput{transactionInputCbor, provenance} <- outputs
                    , Node `elem` provenance
                    ]
        unless (nodeTransactionInputs == expectedNodeInputs)
            $ fail "node_transaction_inputs does not match node output provenance"
        outputRecords <- mapM outputRecord outputs
        requireSortedUnique "outputs" outputRecords
        pendingRecords <- mapM pendingRecord transactions
        expectedRecords <-
            either fail pure
                $ canonicalContextRecords
                $ ProtocolRecord
                    networkId
                    networkMagic
                    major
                    minor
                    protocolParametersCbor
                    : outputRecordsToValues outputs
                        <> map ownershipRecordValue ownership
                        <> pendingRecords
                        <> map requiredRecordValue requiredWalletProofs
        unless ((getApiDappHex <$> records) == expectedRecords)
            $ fail "records do not exactly match response context"
        claims <- either fail pure $ decodeContextTokenClaims contextToken
        unless
            ( walletIdBytes claims == T.encodeUtf8 walletId
                && genesisHashBytes claims == genesisHash
                && contextDigestBytes claims == contextDigest
            )
            $ fail "context token payload does not match response bindings"
      where
        outputRecord output@ApiDappContextOutput{provenance, roles, walletMember, pendingState} = do
            unless (walletMember == (WalletSnapshot `elem` roles))
                $ fail "wallet_member must match wallet_snapshot role"
            unless
                ((pendingState == OutcomeUnknown) == (Pending `elem` provenance))
                $ fail "pending_state must match pending provenance"
            either fail pure $ encodeContextRecord $ outputRecordValue output
        outputRecordsToValues = map outputRecordValue
        pendingRecord
            ApiDappPendingTransaction
                { transactionId = ApiDappHex transactionId
                , transactionCbor = ApiDappHex transactionCbor
                , normalInputs
                , collateralInputs
                , expirySlot
                } =
                pure
                    $ PendingTransactionRecord
                        transactionId
                        transactionCbor
                        normalInputs
                        collateralInputs
                        (getApiDappWord64 <$> expirySlot)

outputRecordValue :: ApiDappContextOutput -> ContextRecord
outputRecordValue
    ApiDappContextOutput
        { outpoint
        , sourceTransactionOutputCbor = ApiDappHex sourceTransactionOutputCbor
        , provenance
        , roles
        , walletMember
        , pendingState
        } =
        FullOutputRecord
            outpoint
            provenance
            roles
            walletMember
            pendingState
            sourceTransactionOutputCbor

ownershipRecordValue :: ApiDappOwnership -> ContextRecord
ownershipRecordValue ApiDappOwnership{credentialKind, credential = ApiDappHex credential, ownership, derivationPath, proofKinds} =
    OwnershipRecord credentialKind credential ownership derivationPath proofKinds

requiredRecordValue :: ApiDappRequiredWalletProof -> ContextRecord
requiredRecordValue ApiDappRequiredWalletProof{transactionIndex, proofKind, credentialKind, credential = ApiDappHex credential, required} =
    RequiredProofRecord transactionIndex proofKind credentialKind credential required

walletIdBytes :: ContextTokenClaims -> ByteString
walletIdBytes ContextTokenClaims{walletId} = walletId

genesisHashBytes :: ContextTokenClaims -> ByteString
genesisHashBytes ContextTokenClaims{genesisHash} = genesisHash

contextDigestBytes :: ContextTokenClaims -> ByteString
contextDigestBytes ContextTokenClaims{contextDigest} = contextDigest

encodeTokenPayload :: ContextTokenClaims -> Either String ByteString
encodeTokenPayload
    ContextTokenClaims
        { processGeneration
        , capabilityRevision
        , walletId
        , genesisHash
        , contextDigest
        } = do
        requireByteLength "process generation" 16 processGeneration
        unless (capabilityRevision == 1)
            $ Left "capability revision must be 1"
        requireBytes "wallet id" walletId
        requireByteLength "genesis hash" 32 genesisHash
        requireByteLength "context digest" 32 contextDigest
        pure $ strictPut $ do
            putWord8 1
            putByteString processGeneration
            putWord32be capabilityRevision
            putBytes walletId
            putByteString genesisHash
            putByteString contextDigest

parseTokenPayload :: ByteString -> Either String ContextTokenClaims
parseTokenPayload payload = case runGetOrFail getPayload (BL.fromStrict payload) of
    Left (_, _, err) -> Left err
    Right (rest, _, claims)
        | BL.null rest -> Right claims
        | otherwise -> Left "trailing token payload bytes"
  where
    getPayload = do
        version <- getWord8
        unless (version == 1) $ fail "unsupported token version"
        processGeneration <- getByteString 16
        capabilityRevision <- getWord32be
        walletId <- getBytes
        genesisHash <- getByteString 32
        contextDigest <- getByteString 32
        trailing <- getRemainingLazyByteString
        unless (BL.null trailing) $ fail "trailing token payload bytes"
        unless (capabilityRevision == 1)
            $ fail "unsupported capability revision"
        when (BS.null walletId) $ fail "empty wallet id"
        pure
            ContextTokenClaims
                { processGeneration
                , capabilityRevision
                , walletId
                , genesisHash
                , contextDigest
                }

encodeChainPoint :: ApiDappChainPoint -> Either String ByteString
encodeChainPoint ApiDappChainPointGenesis = Right $ strictPut $ putWord8 0
encodeChainPoint
    ApiDappChainPointBlock
        { slot = ApiDappWord64 slot
        , blockHash = ApiDappHex blockHash
        } = do
        requireByteLength "block hash" 32 blockHash
        pure
            $ strictPut
            $ putWord8 1 >> putWord64be slot >> putBytes blockHash

encodeOutpoint :: ApiDappOutpoint -> Either String ByteString
encodeOutpoint ApiDappOutpoint{transactionId = ApiDappHex transactionId, index} = do
    requireByteLength "transaction id" 32 transactionId
    pure $ strictPut $ putByteString transactionId >> putWord32be index

canonicalOutpoints
    :: String -> [ApiDappOutpoint] -> Either String [ByteString]
canonicalOutpoints label values = do
    encoded <- sort <$> mapM encodeOutpoint values
    when (hasDuplicates encoded) $ Left $ "duplicate " <> label
    pure encoded

encodeProvenance :: [ApiDappProvenance] -> Either String Word8
encodeProvenance values = do
    requireCanonicalSubset "provenance" [Earlier, Pending, Node] values
    pure $ sum $ map (\case Earlier -> 0x01; Pending -> 0x02; Node -> 0x04) values

encodeRoles :: [ApiDappRole] -> Either String Word8
encodeRoles values = do
    requireCanonicalSubset
        "roles"
        [Normal, Collateral, Reference, WalletSnapshot]
        values
    pure
        $ sum
        $ map
            ( \case
                Normal -> 0x01
                Collateral -> 0x02
                Reference -> 0x04
                WalletSnapshot -> 0x08
            )
            values

pendingStateCode :: ApiDappPendingState -> Word8
pendingStateCode None = 0
pendingStateCode OutcomeUnknown = 4

record :: Word8 -> Put -> Put
record recordType body = putWord8 recordType >> putBytes (strictPut body)

putBytes :: ByteString -> Put
putBytes bytes = putWord32be (fromIntegral $ BS.length bytes) >> putByteString bytes

putVector :: [ByteString] -> Put
putVector values = putWord32be (fromIntegral $ length values) >> mapM_ putBytes values

putBool :: Bool -> Put
putBool = putWord8 . fromIntegral . fromEnum

getBytes :: Get ByteString
getBytes = getWord32be >>= getByteString . fromIntegral

strictPut :: Put -> ByteString
strictPut = BL.toStrict . runPut

hmacSha256 :: ByteString -> ByteString -> ByteString
hmacSha256 key message = BA.convert (hmac key message :: HMAC SHA256)

requireLength :: MonadFail m => String -> Int -> ApiDappHex -> m ()
requireLength label expected (ApiDappHex bytes) =
    unless (BS.length bytes == expected)
        $ fail
        $ label <> " has invalid length"

requireLengthBetween
    :: MonadFail m => String -> Int -> Int -> ApiDappHex -> m ()
requireLengthBetween label lower upper (ApiDappHex bytes) =
    unless (BS.length bytes >= lower && BS.length bytes <= upper)
        $ fail
        $ label <> " has invalid length"

requireNonEmpty :: MonadFail m => String -> ApiDappHex -> m ()
requireNonEmpty label = requireLengthBetween label 1 maxBound

requireByteLength :: String -> Int -> ByteString -> Either String ()
requireByteLength label expected bytes =
    unless (BS.length bytes == expected)
        $ Left
        $ label <> " has invalid length"

requireByteLengthBetween
    :: String -> Int -> Int -> ByteString -> Either String ()
requireByteLengthBetween label lower upper bytes =
    unless (BS.length bytes >= lower && BS.length bytes <= upper)
        $ Left
        $ label <> " has invalid length"

requireBytes :: String -> ByteString -> Either String ()
requireBytes label =
    requireByteLengthBetween label 1 (fromIntegral (maxBound :: Word32))

requireSortedUnique :: (MonadFail m, Ord a) => String -> [a] -> m ()
requireSortedUnique label values =
    unless (values == sort values && not (hasDuplicates values))
        $ fail
        $ label <> " must be sorted and duplicate-free"

requireFixedSubset
    :: (MonadFail m, Eq a) => String -> [a] -> [a] -> m ()
requireFixedSubset label allowed values = do
    when (null values) $ fail $ label <> " must not be empty"
    unless
        (values == filter (`elem` values) allowed && not (hasDuplicates values))
        $ fail
        $ label <> " has invalid order or duplicates"

requireCanonicalSubset
    :: Eq a => String -> [a] -> [a] -> Either String ()
requireCanonicalSubset label allowed values
    | null values = Left $ label <> " must not be empty"
    | values /= filter (`elem` values) allowed || hasDuplicates values =
        Left $ label <> " has invalid order or duplicates"
    | otherwise = Right ()

requireFixedSubsetAllowEmpty :: (MonadFail m, Eq a) => String -> [a] -> [a] -> m ()
requireFixedSubsetAllowEmpty label allowed values =
    unless (values == filter (`elem` values) allowed && not (hasDuplicates values))
        $ fail $ label <> " has invalid order or duplicates"

requireCanonicalSubsetAllowEmpty :: Eq a => String -> [a] -> [a] -> Either String ()
requireCanonicalSubsetAllowEmpty label allowed values =
    unless (values == filter (`elem` values) allowed && not (hasDuplicates values))
        $ Left $ label <> " has invalid order or duplicates"

proofKindJson :: [(Text, ApiDappProofKind)]
proofKindJson =
    [ ("normal_input", NormalInputProof)
    , ("collateral", CollateralProof)
    , ("withdrawal", WithdrawalProof)
    , ("certificate", CertificateProof)
    , ("required_signer", RequiredSignerProof)
    , ("native_script", NativeScriptProof)
    , ("policy", PolicyProof)
    ]

proofKindOrder :: [ApiDappProofKind]
proofKindOrder = snd <$> proofKindJson

credentialKindCode :: ApiDappCredentialKind -> Word8
credentialKindCode PaymentCredential = 1
credentialKindCode StakeCredential = 2
credentialKindCode PolicyCredential = 4

ownershipCode :: ApiDappOwnershipKind -> Word8
ownershipCode Unowned = 0
ownershipCode OwnedKey = 1
ownershipCode ScriptOwned = 2

proofKindCode :: ApiDappProofKind -> Word8
proofKindCode = (1 +) . fromIntegral . fromEnum

proofBit :: ApiDappProofKind -> Word32
proofBit = (2 ^) . fromEnum

validPath :: ApiDappCredentialKind -> [Word32] -> Bool
validPath PaymentCredential [0x8000073c, 0x80000717, account, role, index] =
    account >= 0x80000000 && role <= 1 && index < 0x80000000
validPath StakeCredential [0x8000073c, 0x80000717, account, 2, 0] =
    account >= 0x80000000
validPath PolicyCredential [0x8000073f, 0x80000717, 0x80000000] = True
validPath _ _ = False

hasDuplicates :: Eq a => [a] -> Bool
hasDuplicates = any ((> 1) . length) . group

isLowerHex :: Text -> Bool
isLowerHex = T.all (\c -> isHexDigit c && not (c >= 'A' && c <= 'F'))

isLowerHexOfLength :: Int -> Text -> Bool
isLowerHexOfLength expected value = T.length value == expected && isLowerHex value

canonicalDecimal :: Text -> Bool
canonicalDecimal value =
    value == "0"
        || (not (T.null value) && T.head value /= '0' && T.all isDigit value)

parseEnum :: String -> [(Text, a)] -> Value -> Parser a
parseEnum label values = withText label $ \value ->
    maybe (fail $ "invalid " <> label) pure $ lookup value values

enumJson :: Eq a => [(a, Text)] -> a -> Value
enumJson values value =
    String
        $ maybe (error "incomplete enum encoding") id
        $ lookup value values

explicitStrictOptions :: Aeson.Options
explicitStrictOptions = explicitNothingRecordTypeOptions{Aeson.rejectUnknownFields = True}

rejectKeys :: KeyMap.KeyMap Value -> [Text] -> Parser ()
rejectKeys object allowed =
    unless (sort (Key.toText <$> KeyMap.keys object) == sort allowed)
        $ fail "unexpected or missing chain point fields"

rejectDuplicateFields :: ByteString -> Either String ()
rejectDuplicateFields = go []
  where
    go _ bytes | BS.null bytes = Right ()
    go stack bytes = case BS.uncons bytes of
        Nothing -> Right ()
        Just (0x7b, rest) -> go (Just Set.empty : stack) rest
        Just (0x7d, rest) -> go (drop 1 stack) rest
        Just (0x5b, rest) -> go (Nothing : stack) rest
        Just (0x5d, rest) -> go (drop 1 stack) rest
        Just (0x22, rest) -> do
            (field, escaped, suffix) <- jsonString rest
            let isField = BS.take 1 (BS.dropWhile jsonSpace suffix) == ":"
            stack' <- case (isField, stack) of
                (True, Just fields : parents)
                    | escaped -> Left "escaped JSON field names are not accepted"
                    | field `Set.member` fields -> Left "duplicate JSON field"
                    | otherwise -> Right $ Just (Set.insert field fields) : parents
                _ -> Right stack
            go stack' suffix
        Just (_, rest) -> go stack rest

    jsonString = string False []
      where
        string _ _ bytes | BS.null bytes = Left "unterminated JSON string"
        string escaped chunks bytes = case BS.uncons bytes of
            Just (0x22, rest) -> Right (BS.pack $ reverse chunks, escaped, rest)
            Just (0x5c, rest) -> case BS.uncons rest of
                Nothing -> Left "unterminated JSON escape"
                Just (_, suffix) -> string True chunks suffix
            Just (byte, rest) -> string escaped (byte : chunks) rest
            Nothing -> Left "unterminated JSON string"

    jsonSpace byte = byte == 0x20 || byte == 0x09 || byte == 0x0a || byte == 0x0d
