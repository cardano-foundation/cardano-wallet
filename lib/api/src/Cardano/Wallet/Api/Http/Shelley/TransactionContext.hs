{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Api.Http.Shelley.TransactionContext
    ( DecodedTx (..)
    , addRoles
    , contextSets
    , decodeTx
    , resolveOutput
    , resolveTransactionContext
    , validateTransactionContextResponseForRequest
    ) where

import Cardano.Api
    ( AnyCardanoEra (AnyCardanoEra)
    , CardanoEra (ConwayEra)
    )
import Cardano.Address.Derivation
    ( xpubPublicKey
    )
import Cardano.Balance.Tx.Eras
    ( MaybeInRecentEra (InRecentEraConway)
    )
import Cardano.Ledger.Api
    ( Addr (..)
    , addrTxOutL
    , ppProtocolVersionL
    , serialiseAddr
    )
import Cardano.Ledger.Credential
    ( Credential (..)
    , StakeReference (StakeRefNull)
    )
import Cardano.Ledger.Conway.TxCert
    ( ConwayTxCert (ConwayTxCertDeleg)
    )
import Cardano.Ledger.Conway.Governance
    ( VotingProcedures (VotingProcedures)
    )
import Cardano.Ledger.Conway.TxBody
    ( proposalProceduresTxBodyL
    , votingProceduresTxBodyL
    )
import Cardano.Ledger.Core
    ( bodyTxL
    , certsTxBodyL
    , getScriptWitnessTxCert
    , getVKeyWitnessTxCert
    )
import Cardano.Ledger.Address
    ( AccountAddress (AccountAddress)
    , AccountId (AccountId)
    , Withdrawals (Withdrawals)
    )
import Cardano.Ledger.Alonzo.TxBody
    ( reqSignerHashesTxBodyL
    )
import Cardano.Ledger.Core
    ( withdrawalsTxBodyL
    )
import Cardano.Ledger.Api.UTxO
    ( UTxO (UTxO)
    )
import Cardano.Ledger.BaseTypes
    ( Network (Mainnet, Testnet)
    , ProtVer (ProtVer)
    , TxIx (TxIx)
    )
import Cardano.Ledger.Binary
    ( getVersion
    , serialize'
    , shelleyProtVer
    )
import Cardano.Read.Ledger.Tx.CBOR
    ( TxWithOutputBytes (..)
    , deserializeConwayTxWithOutputBytes
    )
import Cardano.Read.Ledger.Tx.Output
    ( Output (Output)
    )
import Cardano.Read.Ledger.Tx.ReferenceInputs
    ( ReferenceInputs (ReferenceInputs)
    , getEraReferenceInputs
    )
import Cardano.Wallet
    ( WalletLayer
    , dbLayer
    , networkLayer
    )
import Cardano.Wallet.Api
    ( ApiLayer (..)
    )
import Cardano.Wallet.Api.Lib.ApiT
    ( ApiT (ApiT)
    )
import Cardano.Wallet.Api.Types.Dapp.Context
import Cardano.Wallet.Api.Types.Error
    ( DappError (..)
    )
import Cardano.Wallet.Address.Derivation
    ( DerivationIndex (getDerivationIndex)
    , Index (Index, getIndex)
    , Role (UtxoExternal, UtxoInternal)
    , SoftDerivation (deriveAddressPublicKey)
    , stakeDerivationPath
    )
import Cardano.Wallet.Address.Derivation.Shelley
    ( ShelleyKey
    )
import Cardano.Wallet.Address.Discovery
    ( IsOurs (isOurs)
    )
import Cardano.Wallet.Address.Discovery.Sequential
    ( SeqState
    )
import Cardano.Wallet.DB
    ( ContextClock (..)
    , DBLayer (..)
    )
import Cardano.Wallet.Network
    ( DappTransactionContext (..)
    , NetworkLayer (..)
    )
import Cardano.Wallet.Primitive.NetworkId
    ( HasSNetworkId (sNetworkId)
    , SNetworkId (..)
    )
import Cardano.Wallet.Primitive.Types
    ( GenesisParameters (getGenesisBlockHash)
    , NetworkParameters (NetworkParameters)
    , WalletId
    , chainPointFromBlockHeader
    )
import Cardano.Wallet.Primitive.Types.Block
    ( fromWalletChainPoint
    )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (Address)
    )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (Hash)
    )
import Cardano.Wallet.Primitive.Types.ProtocolMagic
    ( ProtocolMagic (getProtocolMagic)
    , magicSNetworkId
    )
import Cardano.Wallet.Primitive.Types.Tx
    ( SealedTx
    , serialisedTx
    )
import Cardano.Wallet.Address.Keys.WalletKey
    ( getRawKey
    )
import Cardano.Wallet.Flavor
    ( KeyFlavorS (ShelleyKeyS)
    )
import Control.Lens
    ( view
    , (^.)
    )
import Control.Monad
    ( foldM
    , unless
    )
import Control.Monad.IO.Class
    ( liftIO
    )
import Control.Monad.Trans.Except
    ( ExceptT (ExceptT)
    , runExceptT
    , throwE
    )
import Cryptography.Hash.Blake
    ( blake2b224
    )
import Data.Bifunctor
    ( first
    )
import Data.ByteString
    ( ByteString
    )
import Data.Function
    ( (&)
    )
import Data.Coerce
    ( coerce
    )
import Data.Foldable
    ( toList
    )
import Data.List
    ( sort
    , sortOn
    )
import Data.List.NonEmpty qualified as NE
import Data.Maybe
    ( catMaybes
    )
import Data.Map.Strict
    ( Map
    )
import Data.Set
    ( Set
    )
import Data.Text.Class
    ( toText
    )
import Data.Word
    ( Word32
    , Word64
    )
import Prelude

import Cardano.Crypto.Hash.Class qualified as Crypto
import Cardano.Ledger.Hashes qualified as Ledger
import Cardano.Ledger.Keys qualified as LedgerKeys
import Cardano.Ledger.TxIn qualified as Ledger
import Cardano.Wallet.Primitive.Model qualified as Wallet
import Cardano.Wallet.Primitive.Types.Tx.TxIn qualified as Wallet
import Cardano.Wallet.Primitive.Types.UTxO qualified as Wallet
import Cardano.Wallet.Read qualified as Read
import Cardano.Wallet.Read.Hash qualified as ReadHash
import Cardano.Wallet.Address.Discovery.Sequential qualified as Seq
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text.Encoding qualified as Text

data DecodedTx = DecodedTx
    { bytes :: !ByteString
    , transaction :: !(Read.Tx Read.Conway)
    , txId :: !ByteString
    , normal :: !(Set Ledger.TxIn)
    , collateral :: !(Set Ledger.TxIn)
    , reference :: !(Set Ledger.TxIn)
    , outputs :: !(Map Ledger.TxIn (Output Read.Conway, ByteString))
    , expiry :: !(Maybe Word64)
    , valid :: !Bool
    }

data Capture s = Capture
    { point :: !Read.ChainPoint
    , clock :: !ContextClock
    , checkpoint :: !(Set Ledger.TxIn)
    , pending :: ![DecodedTx]
    , discovery :: !s
    }

resolveTransactionContext
    :: forall n
     . HasSNetworkId n
    => ApiLayer (SeqState n ShelleyKey)
    -> WalletLayer IO (SeqState n ShelleyKey)
    -> ApiT WalletId
    -> ApiDappTransactionContextRequest
    -> IO (Either DappError ApiDappTransactionContextResponse)
resolveTransactionContext api worker (ApiT wid) request = runExceptT $ do
    expectedNetwork <- fromEither $ configuredNetwork @n api
    require InvalidDappRequest $ request.network == expectedNetwork
    requested <-
        fromEither
            $ first (const InvalidDappRequest)
            $ mapM decodeTx request.transactions
    require InvalidDappRequest $ all noNormalCollateralOverlap requested
    require InvalidDappRequest $ all (.valid) requested
    require InvalidDappRequest $ all supportedCredentialSurfaces requested
    retry (3 :: Int) expectedNetwork requested
  where
    retry 0 _ _ = throwE DappContextUnavailableError
    retry attempts expectedNetwork requested = do
        capture <- ExceptT $ captureContext worker
        let (available, spent, wanted) =
                contextSets capture.checkpoint capture.pending requested
        queried <-
            liftIO
                $ getDappTransactionContext (api ^. networkLayer) capture.point wanted
        case queried of
            Left _ -> retry (attempts - 1) expectedNetwork requested
            Right context -> do
                confirmed <- ExceptT $ confirmContext worker capture
                if not confirmed
                    then retry (attempts - 1) expectedNetwork requested
                    else
                        fromEither
                            $ assemble
                                api
                                wid
                                expectedNetwork
                                request
                                requested
                                capture
                                available
                                spent
                                wanted
                                context

captureContext
    :: WalletLayer IO (SeqState n ShelleyKey)
    -> IO (Either DappError (Capture (SeqState n ShelleyKey)))
captureContext worker =
    worker ^. dbLayer & \DBLayer{..} -> do
        ((wallet, submissions), clock) <-
            atomicallyReadContext
                $ (,) <$> readCheckpoint <*> readInSubmissionTransactions
        pure $ do
            requireEither DappAccountChangedError $ not clock.contextDeleted
            pending <-
                first (const DappContextUnavailableError)
                    $ mapM (uncurry decodePending) submissions
            requireEither DappContextUnavailableError
                $ all noNormalCollateralOverlap pending
            checkpoint <-
                Set.fromList
                    <$> first
                        (const DappInternalErrorResponse)
                        ( mapM walletInputToLedger
                            $ Set.toList
                            $ Wallet.dom
                            $ Wallet.utxo wallet
                        )
            pure
                Capture
                    { point =
                        fromWalletChainPoint
                            $ chainPointFromBlockHeader
                            $ Wallet.currentTip wallet
                    , clock
                    , checkpoint
                    , pending
                    , discovery = Wallet.getState wallet
                    }

confirmContext
    :: WalletLayer IO s -> Capture s -> IO (Either DappError Bool)
confirmContext worker Capture{point, clock} =
    worker ^. dbLayer & \DBLayer{..} -> do
        (wallet, currentClock) <- atomicallyReadContext readCheckpoint
        pure $ do
            requireEither DappAccountChangedError
                $ not currentClock.contextDeleted
            pure
                $ clock == currentClock
                    && point
                        == fromWalletChainPoint
                            (chainPointFromBlockHeader $ Wallet.currentTip wallet)

decodePending :: Hash "Tx" -> SealedTx -> Either String DecodedTx
decodePending (Hash storedId) sealed = do
    decoded <- decodeTx $ ApiDappHex $ serialisedTx sealed
    requireEither "pending transaction id mismatch"
        $ decoded.txId == storedId
    pure decoded

decodeTx :: ApiDappHex -> Either String DecodedTx
decodeTx (ApiDappHex bytes) = do
    TxWithOutputBytes{transaction, outputsWithBytes} <-
        first show $ deserializeConwayTxWithOutputBytes $ BL.fromStrict bytes
    let txId = txIdBytes $ Read.getTxId transaction
        normal = Read.getInputs transaction
        collateral = Read.getCollateralInputs transaction
        ReferenceInputs reference = getEraReferenceInputs transaction
        outputs =
            Map.fromList
                [ ( Ledger.TxIn (Read.getTxId transaction) (TxIx $ fromIntegral ix)
                  , (output, BL.toStrict source)
                  )
                | (ix, (output, source)) <- zip [0 :: Word32 ..] outputsWithBytes
                ]
        expiry =
            (\(Read.SlotNo slot) -> fromIntegral slot)
                <$> Read.invalidHereafter (Read.getValidityInterval transaction)
        Read.IsValidC valid = Read.getScriptValidity transaction
    pure
        DecodedTx{bytes, transaction, txId, normal, collateral, reference, outputs, expiry, valid}

noNormalCollateralOverlap :: DecodedTx -> Bool
noNormalCollateralOverlap DecodedTx{normal, collateral} = Set.disjoint normal collateral

supportedCredentialSurfaces :: DecodedTx -> Bool
supportedCredentialSurfaces DecodedTx{transaction = Read.Tx ledgerTx} =
    all supportedCertificate (toList $ ledgerTx ^. bodyTxL . certsTxBodyL)
        && Map.null voting
        && null (ledgerTx ^. bodyTxL . proposalProceduresTxBodyL)
  where
    VotingProcedures voting = ledgerTx ^. bodyTxL . votingProceduresTxBodyL
    supportedCertificate ConwayTxCertDeleg{} = True
    supportedCertificate _ = False

contextSets
    :: Set Ledger.TxIn
    -> [DecodedTx]
    -> [DecodedTx]
    -> (Set Ledger.TxIn, Set Ledger.TxIn, Set Ledger.TxIn)
contextSets checkpoint pending requested =
    (available, checkpoint Set.\\ available, available <> requestedInputs)
  where
    pendingNormal = Set.unions $ normal <$> pending
    pendingCollateral = Set.unions $ collateral <$> pending
    available = checkpoint Set.\\ (pendingNormal <> pendingCollateral)
    requestedInputs =
        Set.unions
            $ concatMap (\tx -> [tx.normal, tx.collateral, tx.reference]) requested

assemble
    :: HasSNetworkId n
    => ApiLayer (SeqState n ShelleyKey)
    -> WalletId
    -> ApiDappContextNetwork
    -> ApiDappTransactionContextRequest
    -> [DecodedTx]
    -> Capture (SeqState n ShelleyKey)
    -> Set Ledger.TxIn
    -> Set Ledger.TxIn
    -> Set Ledger.TxIn
    -> DappTransactionContext
    -> Either DappError ApiDappTransactionContextResponse
assemble api wid configured request requested capture available spent wanted DappTransactionContext{..} = do
    requireEither InvalidDappRequest
        $ contextEra == AnyCardanoEra ConwayEra
    (protocolVersion, protocolBytes) <- case contextProtocolParameters of
        Read.EraValue (Read.PParams pparams :: Read.PParams era) ->
            case Read.theEra @era of
                Read.Conway ->
                    let ProtVer major minor = view ppProtocolVersionL pparams
                    in  Right
                            ( ApiDappProtocolVersion (getVersion major) (fromIntegral minor)
                            , serialize' shelleyProtVer pparams
                            )
                _ -> Left InvalidDappRequest
    nodeOutputs <- case contextUTxO of
        InRecentEraConway (UTxO values) -> Right values
        _ -> Left InvalidDappRequest
    let pendingOutputs = Map.unions $ (.outputs) <$> capture.pending
        roleMap = foldl' addRoles mempty requested
        nodeSources =
            (\output -> (Output output, serialize' shelleyProtVer output))
                <$> nodeOutputs
    (earlierOutputs, batchOverlay) <-
        buildBatchOverlay requested capture.pending pendingOutputs
    resolved <-
        traverse
            (resolveOutput available roleMap earlierOutputs pendingOutputs nodeSources)
            $ Set.toList wanted
    let outputValues = (\(_, value, _) -> value) <$> sortOn (\(encoded, _, _) -> encoded) resolved
        pendingValues = sortOn (.txId) capture.pending
        pendingApi = toPending <$> pendingValues
        protocolRecord =
            ProtocolRecord
                configured.networkId
                configured.networkMagic
                protocolVersion.major
                protocolVersion.minor
                protocolBytes
        outputRecords = outputRecord <$> outputValues
        pendingRecords = pendingRecord <$> pendingValues
    payment <- paymentOwnership capture.discovery resolved
    let (stake, stakeProofs) = stakeEvidence capture.discovery requested
    (signers, signerProofs) <- signerEvidence configured capture.discovery requested
    let ownership = mergeOwnership $ payment <> stake <> signers
    paymentProofs <- paymentRequiredProofs requested ownership resolved
    let requiredWalletProofs = Set.toList $ Set.fromList $ paymentProofs <> stakeProofs <> signerProofs
    let ownershipRecords = ownershipRecord <$> ownership
        requiredRecords = requiredProofRecord <$> requiredWalletProofs
        recordValues = protocolRecord : outputRecords <> ownershipRecords <> pendingRecords <> requiredRecords
        point = toApiPoint capture.point
        walletText = toText wid
        walletBytes = Text.encodeUtf8 walletText
        genesisBytes = getApiDappHex configured.genesisHash
        ContextClock{walletGeneration, pendingGeneration} = capture.clock
    records <-
        first (const DappInternalErrorResponse)
            $ canonicalContextRecords recordValues
    contextDigest <-
        first (const DappInternalErrorResponse)
            $ computeContextDigest
                ContextDigestInput
                    { walletId = walletBytes
                    , genesisHash = genesisBytes
                    , chainPoint = point
                    , walletGeneration
                    , pendingGeneration
                    , transactions = getApiDappHex <$> request.transactions
                    , records = recordValues
                    }
    contextToken <-
        first (const DappInternalErrorResponse)
            $ encodeContextToken
                api.dappHmacKey
                ContextTokenClaims
                    { processGeneration = api.dappProcessGeneration
                    , capabilityRevision = 1
                    , walletId = walletBytes
                    , genesisHash = genesisBytes
                    , contextDigest
                    }
    let response = ApiDappTransactionContextResponse
            { revision = 1
            , walletId = walletText
            , network = configured
            , chainPoint = point
            , walletGeneration = ApiDappWord64 walletGeneration
            , pendingGeneration = ApiDappWord64 pendingGeneration
            , era = "conway"
            , protocolVersion
            , protocolParametersCbor = ApiDappHex protocolBytes
            , volatileDelta =
                ApiDappVolatileDelta point
                    $ sort
                        [ transactionInputCbor
                        | ApiDappContextOutput{transactionInputCbor, provenance} <- outputValues
                        , Node `elem` provenance
                        ]
            , outputs = outputValues
            , pendingOverlay =
                ApiDappPendingOverlay
                    pendingApi
                    (sort $ toOutpoint <$> Set.toList spent)
                    []
            , ownership
            , requiredWalletProofs
            , batchOverlay
            , records = ApiDappHex <$> records
            , contextDigest = ApiDappHex contextDigest
            , contextToken = ApiDappHex contextToken
            }
    first (const DappInternalErrorResponse)
        $ validateTransactionContextResponseForRequest request response
    pure response
resolveOutput
    :: Set Ledger.TxIn
    -> Map Ledger.TxIn (Set ApiDappRole)
    -> Map Ledger.TxIn (Output Read.Conway, ByteString)
    -> Map Ledger.TxIn (Output Read.Conway, ByteString)
    -> Map Ledger.TxIn (Output Read.Conway, ByteString)
    -> Ledger.TxIn
    -> Either DappError (ByteString, ApiDappContextOutput, Output Read.Conway)
resolveOutput available roleMap earlier pending node input = do
    let sources = catMaybes
            [ (\(output, bytes) -> (Earlier, output, bytes)) <$> Map.lookup input earlier
            , (\(output, bytes) -> (Pending, output, bytes)) <$> Map.lookup input pending
            , (\(output, bytes) -> (Node, output, bytes)) <$> Map.lookup input node
            ]
    (provenance, output, source) <- case sources of
        [] -> Left DappContextUnavailableError
        ((_, firstOutput, firstBytes) : _) -> do
            requireEither DappContextConflictError $ all (\(_, _, bytes) -> bytes == firstBytes) sources
            requireEither DappInternalErrorResponse $ all (\(_, value, _) -> value == firstOutput) sources
            pure ((\(kind, _, _) -> kind) <$> sources, firstOutput, firstBytes)
    let walletMember = input `Set.member` available
        roles =
            [Normal | hasRole Normal]
                <> [Collateral | hasRole Collateral]
                <> [Reference | hasRole Reference]
                <> [WalletSnapshot | walletMember]
        inputBytes = serialize' shelleyProtVer input
        Output ledgerOutput = output
        outputBytes = serialize' shelleyProtVer ledgerOutput
        pairBytes = serialize' shelleyProtVer (input, ledgerOutput)
        value =
            ApiDappContextOutput
                { outpoint = toOutpoint input
                , transactionInputCbor = ApiDappHex inputBytes
                , sourceTransactionOutputCbor = ApiDappHex source
                , canonicalTransactionOutputCbor = ApiDappHex outputBytes
                , transactionUnspentOutputCbor = ApiDappHex pairBytes
                , provenance
                , roles
                , walletMember
                , pendingState =
                    if Pending `elem` provenance then OutcomeUnknown else None
                }
    encoded <-
        first (const DappInternalErrorResponse)
            $ encodeContextRecord
            $ outputRecord value
    pure (encoded, value, output)
  where
    hasRole role = maybe False (Set.member role) $ Map.lookup input roleMap

paymentOwnership
    :: forall n
     . HasSNetworkId n
    => SeqState n ShelleyKey
    -> [(ByteString, ApiDappContextOutput, Output Read.Conway)]
    -> Either DappError [ApiDappOwnership]
paymentOwnership initial values =
    fmap (sort . Map.elems . snd)
        $ foldM classify
            ( initial
            , Map.empty :: Map (ByteString, ApiDappOwnershipKind, [Word32]) ApiDappOwnership
            )
            values
  where
    classify
        :: ( SeqState n ShelleyKey
           , Map (ByteString, ApiDappOwnershipKind, [Word32]) ApiDappOwnership
           )
        -> (ByteString, ApiDappContextOutput, Output Read.Conway)
        -> Either DappError
            ( SeqState n ShelleyKey
            , Map (ByteString, ApiDappOwnershipKind, [Word32]) ApiDappOwnership
            )
    classify (discovery, found) (_, ApiDappContextOutput{roles}, Output ledgerOutput) =
        case ledgerOutput ^. addrTxOutL of
            AddrBootstrap{} -> Left InvalidDappRequest
            address@(Addr _ credential _) -> case credential of
                ScriptHashObj (Ledger.ScriptHash hash) ->
                    pure (discovery, insert (Crypto.hashToBytes hash) ScriptOwned [] found)
                KeyHashObj (LedgerKeys.KeyHash hash) ->
                    let (path, discovery') = isOurs (Address $ serialiseAddr address) discovery
                        ownership = maybe Unowned (const OwnedKey) path
                        indexes = maybe [] (map getDerivationIndex . NE.toList) path
                        credentialBytes = Crypto.hashToBytes hash
                    in  do
                            case path of
                                Nothing -> pure ()
                                Just _ -> requireEither DappInternalErrorResponse
                                    $ verifiesPaymentPath discovery indexes credentialBytes
                            pure (discovery', insert credentialBytes ownership indexes found)
      where
        proofs =
            [NormalInputProof | Normal `elem` roles]
                <> [CollateralProof | Collateral `elem` roles]
        insert credential ownership derivationPath =
            Map.insertWith merge (credential, ownership, derivationPath)
                ApiDappOwnership
                    { credentialKind = PaymentCredential
                    , credential = ApiDappHex credential
                    , ownership
                    , derivationPath
                    , proofKinds = proofs
                    }
        merge ApiDappOwnership{proofKinds = newProofs}
            ApiDappOwnership{credentialKind, credential, ownership, derivationPath, proofKinds = oldProofs} =
                ApiDappOwnership
                    credentialKind
                    credential
                    ownership
                    derivationPath
                    (sort $ Set.toList $ Set.fromList $ oldProofs <> newProofs)

verifiesPaymentPath :: SeqState n ShelleyKey -> [Word32] -> ByteString -> Bool
verifiesPaymentPath discovery [0x8000073c, 0x80000717, accountIndex, roleIndex, addressIndex] expectedCredential
    | accountIndex == getIndex account && roleIndex <= 1 && addressIndex < 0x80000000 =
        let role = if roleIndex == 0 then UtxoExternal else UtxoInternal
            child = deriveAddressPublicKey (Seq.accountXPub discovery) role (Index addressIndex)
        in  blake2b224 (xpubPublicKey $ getRawKey ShelleyKeyS child) == expectedCredential
  where
    Seq.DerivationPrefix (_, _, account) = Seq.derivationPrefix discovery
verifiesPaymentPath _ _ _ = False

ownershipRecord :: ApiDappOwnership -> ContextRecord
ownershipRecord ApiDappOwnership{credentialKind, credential = ApiDappHex credential, ownership, derivationPath, proofKinds} =
    OwnershipRecord credentialKind credential ownership derivationPath proofKinds

stakeEvidence
    :: SeqState n ShelleyKey
    -> [DecodedTx]
    -> ([ApiDappOwnership], [ApiDappRequiredWalletProof])
stakeEvidence discovery requested =
    (concat ownership, concat proofs)
  where
    stakeHash = blake2b224 $ xpubPublicKey $ getRawKey ShelleyKeyS $ Seq.rewardAccountKey discovery
    path = map getDerivationIndex $ NE.toList $ stakeDerivationPath $ Seq.derivationPrefix discovery
    (ownership, proofs) = unzip $ concatMap transactionEvidence $ zip [0 :: Word32 ..] requested
    transactionEvidence (transactionIndex, DecodedTx{transaction = Read.Tx ledgerTx}) =
        withdrawalEvidence transactionIndex ledgerTx <> certificateEvidence transactionIndex ledgerTx
    withdrawalEvidence transactionIndex ledgerTx =
        [ evidence transactionIndex WithdrawalProof credential
        | (AccountAddress _ (AccountId credential), _) <- Map.toList withdrawals
        ]
      where
        Withdrawals withdrawals = ledgerTx ^. bodyTxL . withdrawalsTxBodyL
    certificateEvidence transactionIndex ledgerTx =
        [ case (getVKeyWitnessTxCert certificate, getScriptWitnessTxCert certificate) of
            (Just (LedgerKeys.KeyHash hash), _) -> evidence transactionIndex CertificateProof $ KeyHashObj $ LedgerKeys.KeyHash hash
            (_, Just scriptHash) -> evidence transactionIndex CertificateProof $ ScriptHashObj scriptHash
            _ -> ([], [])
        | certificate <- toList $ ledgerTx ^. bodyTxL . certsTxBodyL
        ]
    evidence transactionIndex proofKind = \case
        KeyHashObj (LedgerKeys.KeyHash hash) ->
            let bytes = Crypto.hashToBytes hash
                owned = bytes == stakeHash
                row = ApiDappOwnership StakeCredential (ApiDappHex bytes) (if owned then OwnedKey else Unowned) (if owned then path else []) [proofKind]
                proof = [ApiDappRequiredWalletProof transactionIndex proofKind StakeCredential (ApiDappHex bytes) True | owned]
            in  ([row], proof)
        ScriptHashObj (Ledger.ScriptHash hash) ->
            ([ApiDappOwnership StakeCredential (ApiDappHex $ Crypto.hashToBytes hash) ScriptOwned [] [proofKind]], [])

mergeOwnership :: [ApiDappOwnership] -> [ApiDappOwnership]
mergeOwnership = sort . Map.elems . Map.fromListWith merge . map (\value -> ((value.credentialKind, value.credential, value.ownership, value.derivationPath), value))
  where
    merge ApiDappOwnership{proofKinds = newProofs}
        ApiDappOwnership{credentialKind, credential, ownership, derivationPath, proofKinds = oldProofs} =
            ApiDappOwnership credentialKind credential ownership derivationPath
                $ sort $ Set.toList $ Set.fromList $ oldProofs <> newProofs

signerEvidence
    :: HasSNetworkId n
    => ApiDappContextNetwork
    -> SeqState n ShelleyKey
    -> [DecodedTx]
    -> Either DappError ([ApiDappOwnership], [ApiDappRequiredWalletProof])
signerEvidence configured initial requested = do
    (_, ownership, proofs) <- foldM perSigner (initial, [], [])
        [ (transactionIndex, keyHash)
        | (transactionIndex, DecodedTx{transaction = Read.Tx ledgerTx}) <- zip [0 :: Word32 ..] requested
        , keyHash <- Set.toList $ ledgerTx ^. bodyTxL . reqSignerHashesTxBodyL
        ]
    pure (ownership, proofs)
  where
    ledgerNetwork = if configured.networkId == 1 then Mainnet else Testnet
    stakeHash = blake2b224 $ xpubPublicKey $ getRawKey ShelleyKeyS $ Seq.rewardAccountKey initial
    stakePath = map getDerivationIndex $ NE.toList $ stakeDerivationPath $ Seq.derivationPrefix initial
    policy = (\key -> (blake2b224 $ xpubPublicKey $ getRawKey ShelleyKeyS key, [0x8000073f, 0x80000717, 0x80000000])) <$> Seq.policyXPub initial
    perSigner (discovery, ownership, proofs) (transactionIndex, keyHash@(LedgerKeys.KeyHash hash)) = do
        let bytes = Crypto.hashToBytes hash
            address = Addr ledgerNetwork (KeyHashObj $ coerce keyHash) StakeRefNull
            (paymentPath, discovery') = isOurs (Address $ serialiseAddr address) discovery
            paymentRows = case paymentPath of
                Nothing -> []
                Just path ->
                    let indexes = map getDerivationIndex $ NE.toList path
                    in  [ApiDappOwnership PaymentCredential (ApiDappHex bytes) OwnedKey indexes [RequiredSignerProof]
                        | verifiesPaymentPath discovery indexes bytes]
            stakeRows = [ApiDappOwnership StakeCredential (ApiDappHex bytes) OwnedKey stakePath [RequiredSignerProof] | bytes == stakeHash]
            policyRows = case policy of
                Just (policyHash, path) | bytes == policyHash -> [ApiDappOwnership PolicyCredential (ApiDappHex bytes) OwnedKey path [RequiredSignerProof]]
                _ -> []
            rows = paymentRows <> stakeRows <> policyRows
            proofRows =
                [ ApiDappRequiredWalletProof transactionIndex RequiredSignerProof kind (ApiDappHex bytes) True
                | ApiDappOwnership{credentialKind = kind} <- rows
                ]
        pure (discovery', rows <> ownership, proofRows <> proofs)

paymentRequiredProofs
    :: [DecodedTx]
    -> [ApiDappOwnership]
    -> [(ByteString, ApiDappContextOutput, Output Read.Conway)]
    -> Either DappError [ApiDappRequiredWalletProof]
paymentRequiredProofs requested ownership values =
    fmap (Set.toList . Set.fromList . concat) $ mapM perTransaction (zip [0 :: Word32 ..] requested)
  where
    outputs = Map.fromList [(value.outpoint, output) | (_, value, output) <- values]
    owned = Set.fromList
        [ credential
        | ApiDappOwnership
            { credentialKind = PaymentCredential
            , credential = ApiDappHex credential
            , ownership = OwnedKey
            } <- ownership
        ]
    perTransaction (transactionIndex, tx) =
        (<>) <$> perRole transactionIndex NormalInputProof tx.normal
            <*> perRole transactionIndex CollateralProof tx.collateral
    perRole transactionIndex proofKind inputs = fmap concat $ mapM (\input -> do
        output <- maybe (Left DappContextUnavailableError) Right $ Map.lookup (toOutpoint input) outputs
        credential <- paymentKeyCredential output
        pure $ case credential of
            Just value | value `Set.member` owned ->
                [ApiDappRequiredWalletProof transactionIndex proofKind PaymentCredential (ApiDappHex value) True]
            _ -> []
        ) $ Set.toList inputs

paymentKeyCredential :: Output Read.Conway -> Either DappError (Maybe ByteString)
paymentKeyCredential (Output ledgerOutput) = case ledgerOutput ^. addrTxOutL of
    AddrBootstrap{} -> Left InvalidDappRequest
    Addr _ (ScriptHashObj _) _ -> Right Nothing
    Addr _ (KeyHashObj (LedgerKeys.KeyHash hash)) _ -> Right $ Just $ Crypto.hashToBytes hash

requiredProofRecord :: ApiDappRequiredWalletProof -> ContextRecord
requiredProofRecord ApiDappRequiredWalletProof{transactionIndex, proofKind, credentialKind, credential = ApiDappHex credential, required} =
    RequiredProofRecord transactionIndex proofKind credentialKind credential required

buildBatchOverlay
    :: [DecodedTx]
    -> [DecodedTx]
    -> Map Ledger.TxIn (Output Read.Conway, ByteString)
    -> Either DappError
        ( Map Ledger.TxIn (Output Read.Conway, ByteString)
        , ApiDappBatchOverlay
        )
buildBatchOverlay requested pendingTransactions pending = do
    requireEither InvalidDappRequest $ all duplicateIsIdentical txGroups
    (priorOutputs, _, dependencies, conflicts) <-
        foldM step (mempty, mempty, [], []) $ zip [0 :: Word32 ..] requested
    pure
        ( priorOutputs
        , ApiDappBatchOverlay (sort dependencies) (sort conflicts)
        )
  where
    txGroups = Map.elems $ Map.fromListWith (<>) [(tx.txId, [tx.bytes]) | tx <- requested]
    duplicateIsIdentical [] = True
    duplicateIsIdentical (value : values) = all (== value) values
    requestedIds = Set.fromList $ (.txId) <$> requested
    pendingClaims = Set.unions $ concatMap (\tx -> [tx.normal, tx.collateral]) pendingTransactions

    step (priorOutputs, consumed, dependencies, conflicts) (txIndex, tx) = do
        (dependencies', conflicts') <- foldM
            (inputStep txIndex priorOutputs consumed)
            (dependencies, conflicts)
            (inputsWithRoles tx)
        let priorOutputs' = Map.union priorOutputs tx.outputs
            consumed' = foldl' (\m input -> Map.insertWith min input txIndex m) consumed $ Set.toList tx.normal
        pure (priorOutputs', consumed', dependencies', conflicts')

    inputStep txIndex priorOutputs consumed (dependencies, conflicts) (role, input) = do
        let earlier = Map.member input priorOutputs
            pendingSource = Map.member input pending
            sourceIndex = case
                [ index
                | (index, parent) <- zip [0 :: Word32 ..] requested
                , index < txIndex
                , Map.member input parent.outputs
                ] of
                [] -> Nothing
                indexes -> Just $ minimum indexes
            dependency
                | earlier = Just $ ApiDappDependency txIndex role (toOutpoint input) Earlier sourceIndex
                | pendingSource = Just $ ApiDappDependency txIndex role (toOutpoint input) Pending Nothing
                | otherwise = Nothing
            conflict = case (role, Map.lookup input consumed) of
                (Normal, Just earlierIndex) -> Just $ ApiDappConflict txIndex role (toOutpoint input) earlierIndex
                (Collateral, Just earlierIndex) -> Just $ ApiDappConflict txIndex role (toOutpoint input) earlierIndex
                _ -> Nothing
            Ledger.TxIn inputId _ = input
        requireEither InvalidDappRequest $ earlier || not (txIdBytes inputId `Set.member` requestedIds)
        requireEither DappContextConflictError
            $ role == Reference || input `Set.notMember` pendingClaims
        pure (maybe dependencies (: dependencies) dependency, maybe conflicts (: conflicts) conflict)

    inputsWithRoles :: DecodedTx -> [(ApiDappRole, Ledger.TxIn)]
    inputsWithRoles tx =
        [(Normal, input) | input <- Set.toAscList tx.normal]
            <> [(Collateral, input) | input <- Set.toAscList tx.collateral]
            <> [(Reference, input) | input <- Set.toAscList tx.reference]

validateTransactionContextResponseForRequest
    :: ApiDappTransactionContextRequest
    -> ApiDappTransactionContextResponse
    -> Either String ()
validateTransactionContextResponseForRequest request response = do
    requested <- mapM decodeTx request.transactions
    unless (all (.valid) requested) $ Left "invalid transaction validity"
    let provenanceByOutpoint = Map.fromList
            [(value.outpoint, value.provenance) | value <- response.outputs]
        requestedIds = Set.fromList $ (.txId) <$> requested
    (_, _, dependencies, conflicts) <-
        foldM (step requested provenanceByOutpoint requestedIds) (mempty, mempty, [], [])
            $ zip [0 :: Word32 ..] requested
    let expected = ApiDappBatchOverlay (sort dependencies) (sort conflicts)
    unless (response.batchOverlay == expected) $ Left "batch overlay does not match request"
  where
    step
        :: [DecodedTx]
        -> Map ApiDappOutpoint [ApiDappProvenance]
        -> Set ByteString
        -> (Map Ledger.TxIn (Output Read.Conway, ByteString), Map Ledger.TxIn Word32, [ApiDappDependency], [ApiDappConflict])
        -> (Word32, DecodedTx)
        -> Either String (Map Ledger.TxIn (Output Read.Conway, ByteString), Map Ledger.TxIn Word32, [ApiDappDependency], [ApiDappConflict])
    step requested provenanceByOutpoint requestedIds (priorOutputs, consumed, dependencies, conflicts) (txIndex, tx) = do
        (dependencies', conflicts') <- foldM
            (inputStep requested provenanceByOutpoint requestedIds txIndex priorOutputs consumed)
            (dependencies, conflicts)
            (inputsWithRoles tx)
        pure
            ( Map.union priorOutputs tx.outputs
            , foldl' (\m input -> Map.insertWith min input txIndex m) consumed $ Set.toList tx.normal
            , dependencies'
            , conflicts'
            )
    inputStep
        :: [DecodedTx]
        -> Map ApiDappOutpoint [ApiDappProvenance]
        -> Set ByteString
        -> Word32
        -> Map Ledger.TxIn (Output Read.Conway, ByteString)
        -> Map Ledger.TxIn Word32
        -> ([ApiDappDependency], [ApiDappConflict])
        -> (ApiDappRole, Ledger.TxIn)
        -> Either String ([ApiDappDependency], [ApiDappConflict])
    inputStep requested provenanceByOutpoint requestedIds txIndex priorOutputs consumed (dependencies, conflicts) (role, input) = do
        let earlier = Map.member input priorOutputs
            provenance = Map.findWithDefault [] (toOutpoint input) provenanceByOutpoint
            sourceIndex = case
                [ index
                | (index, parent) <- zip [0 :: Word32 ..] requested
                , index < txIndex
                , Map.member input parent.outputs
                ] of
                [] -> Nothing
                indexes -> Just $ minimum indexes
            dependency
                | earlier = Just $ ApiDappDependency txIndex role (toOutpoint input) Earlier sourceIndex
                | Pending `elem` provenance = Just $ ApiDappDependency txIndex role (toOutpoint input) Pending Nothing
                | otherwise = Nothing
            conflict = case (role, Map.lookup input consumed) of
                (Normal, Just index) -> Just $ ApiDappConflict txIndex role (toOutpoint input) index
                (Collateral, Just index) -> Just $ ApiDappConflict txIndex role (toOutpoint input) index
                _ -> Nothing
            Ledger.TxIn inputId _ = input
        unless (earlier || not (txIdBytes inputId `Set.member` requestedIds)) $ Left "self or forward input"
        pure (maybe dependencies (: dependencies) dependency, maybe conflicts (: conflicts) conflict)
    inputsWithRoles :: DecodedTx -> [(ApiDappRole, Ledger.TxIn)]
    inputsWithRoles tx =
        [(Normal, input) | input <- Set.toAscList tx.normal]
            <> [(Collateral, input) | input <- Set.toAscList tx.collateral]
            <> [(Reference, input) | input <- Set.toAscList tx.reference]

addRoles
    :: Map Ledger.TxIn (Set ApiDappRole)
    -> DecodedTx
    -> Map Ledger.TxIn (Set ApiDappRole)
addRoles values DecodedTx{normal, collateral, reference} =
    add Reference reference
        $ add Collateral collateral
        $ add Normal normal values
  where
    add role inputs' values' =
        Set.foldl'
            (\m input -> Map.insertWith (<>) input (Set.singleton role) m)
            values'
            inputs'

outputRecord :: ApiDappContextOutput -> ContextRecord
outputRecord
    ApiDappContextOutput
        { outpoint
        , sourceTransactionOutputCbor = ApiDappHex source
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
            source

pendingRecord :: DecodedTx -> ContextRecord
pendingRecord DecodedTx{bytes, txId, normal, collateral, expiry} =
    PendingTransactionRecord
        txId
        bytes
        (sort $ toOutpoint <$> Set.toList normal)
        (sort $ toOutpoint <$> Set.toList collateral)
        expiry

toPending :: DecodedTx -> ApiDappPendingTransaction
toPending DecodedTx{bytes, txId, normal, collateral, expiry} =
    ApiDappPendingTransaction
        (ApiDappHex txId)
        OutcomeUnknown
        (ApiDappHex bytes)
        (sort $ toOutpoint <$> Set.toList normal)
        (sort $ toOutpoint <$> Set.toList collateral)
        (ApiDappWord64 <$> expiry)

toOutpoint :: Ledger.TxIn -> ApiDappOutpoint
toOutpoint (Ledger.TxIn txid (TxIx index)) =
    ApiDappOutpoint (ApiDappHex $ txIdBytes txid) (fromIntegral index)

txIdBytes :: Ledger.TxId -> ByteString
txIdBytes = ReadHash.hashToBytes . Read.hashFromTxId

walletInputToLedger :: Wallet.TxIn -> Either String Ledger.TxIn
walletInputToLedger (Wallet.TxIn (Hash txid) index) = do
    hash <-
        maybe (Left "invalid wallet transaction id") Right
            $ Crypto.hashFromBytes txid
    pure
        $ Ledger.TxIn
            (Ledger.TxId $ Ledger.unsafeMakeSafeHash hash)
            (TxIx $ fromIntegral index)

configuredNetwork
    :: forall n s
     . HasSNetworkId n
    => ApiLayer s -> Either DappError ApiDappContextNetwork
configuredNetwork api = do
    let (_, NetworkParameters genesis _ _) = api.netParams
        Hash genesisHash = getGenesisBlockHash genesis
        networkId = case sNetworkId @n of SMainnet -> 1; STestnet _ -> 0
        magic = fromIntegral $ getProtocolMagic $ magicSNetworkId $ sNetworkId @n
    pure $ ApiDappContextNetwork networkId magic (ApiDappHex genesisHash)

toApiPoint :: Read.ChainPoint -> ApiDappChainPoint
toApiPoint Read.GenesisPoint = ApiDappChainPointGenesis
toApiPoint (Read.BlockPoint (Read.SlotNo slot) hash) =
    ApiDappChainPointBlock
        (ApiDappWord64 $ fromIntegral slot)
        (ApiDappHex $ ReadHash.hashToBytes hash)

require :: e -> Bool -> ExceptT e IO ()
require err condition = unless condition $ throwE err

requireEither :: e -> Bool -> Either e ()
requireEither err condition = unless condition $ Left err

fromEither :: Either e a -> ExceptT e IO a
fromEither = ExceptT . pure
