{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Api.Http.Shelley.TransactionContext
    ( resolveTransactionContext
    ) where

import Cardano.Api
    ( AnyCardanoEra (AnyCardanoEra)
    , CardanoEra (ConwayEra)
    )
import Cardano.Balance.Tx.Eras
    ( MaybeInRecentEra (InRecentEraConway)
    )
import Cardano.Ledger.Api
    ( ppProtocolVersionL
    )
import Cardano.Ledger.Api.UTxO
    ( UTxO (UTxO)
    )
import Cardano.Ledger.BaseTypes
    ( ProtVer (ProtVer)
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
import Control.Lens
    ( view
    , (^.)
    )
import Control.Monad
    ( unless
    )
import Control.Monad.IO.Class
    ( liftIO
    )
import Control.Monad.Trans.Except
    ( ExceptT (ExceptT)
    , runExceptT
    , throwE
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
import Data.List
    ( sort
    , sortOn
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
import Cardano.Ledger.TxIn qualified as Ledger
import Cardano.Wallet.Primitive.Model qualified as Wallet
import Cardano.Wallet.Primitive.Types.Tx.TxIn qualified as Wallet
import Cardano.Wallet.Primitive.Types.UTxO qualified as Wallet
import Cardano.Wallet.Read qualified as Read
import Cardano.Wallet.Read.Hash qualified as ReadHash
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text.Encoding qualified as Text

data DecodedTx = DecodedTx
    { bytes :: !ByteString
    , txId :: !ByteString
    , normal :: !(Set Ledger.TxIn)
    , collateral :: !(Set Ledger.TxIn)
    , reference :: !(Set Ledger.TxIn)
    , outputs :: !(Map Ledger.TxIn (Output Read.Conway, ByteString))
    , expiry :: !(Maybe Word64)
    }

data Capture s = Capture
    { point :: !Read.ChainPoint
    , clock :: !ContextClock
    , checkpoint :: !(Set Ledger.TxIn)
    , pending :: ![DecodedTx]
    }

resolveTransactionContext
    :: forall n s
     . HasSNetworkId n
    => ApiLayer s
    -> WalletLayer IO s
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
    retry (3 :: Int) expectedNetwork requested
  where
    retry 0 _ _ = throwE DappContextUnavailableError
    retry attempts expectedNetwork requested = do
        capture <- ExceptT $ captureContext worker
        let pendingNormal = Set.unions $ normal <$> capture.pending
            pendingCollateral = Set.unions $ collateral <$> capture.pending
            available = capture.checkpoint Set.\\ (pendingNormal <> pendingCollateral)
            spent = capture.checkpoint Set.\\ available
            requestedInputs =
                Set.unions
                    $ concatMap (\tx -> [tx.normal, tx.collateral, tx.reference]) requested
            wanted = available <> requestedInputs
        queried <-
            liftIO
                $ getDappTransactionContext (api ^. networkLayer) capture.point wanted
        case queried of
            Left _ -> retry (attempts - 1) expectedNetwork requested
            Right context -> do
                confirmed <- liftIO $ confirmContext worker capture
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
    :: WalletLayer IO s -> IO (Either DappError (Capture s))
captureContext worker =
    worker ^. dbLayer & \DBLayer{..} -> do
        ((wallet, submissions), clock) <-
            atomicallyReadContext
                $ (,) <$> readCheckpoint <*> readInSubmissionTransactions
        pure $ do
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
                    }

confirmContext :: WalletLayer IO s -> Capture s -> IO Bool
confirmContext worker Capture{point, clock} =
    worker ^. dbLayer & \DBLayer{..} -> do
        (wallet, currentClock) <- atomicallyReadContext readCheckpoint
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
    pure
        DecodedTx{bytes, txId, normal, collateral, reference, outputs, expiry}

noNormalCollateralOverlap :: DecodedTx -> Bool
noNormalCollateralOverlap DecodedTx{normal, collateral} = Set.disjoint normal collateral

assemble
    :: ApiLayer s
    -> WalletId
    -> ApiDappContextNetwork
    -> ApiDappTransactionContextRequest
    -> [DecodedTx]
    -> Capture s
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
    resolved <-
        traverse
            (resolveOutput available roleMap pendingOutputs nodeSources)
            $ Set.toList wanted
    let outputValues = snd <$> sortOn fst resolved
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
        recordValues = protocolRecord : outputRecords <> pendingRecords
        point = toApiPoint capture.point
        walletText = toText wid
        walletBytes = Text.encodeUtf8 walletText
        genesisBytes = getApiDappHex configured.genesisHash
        ContextClock walletGeneration pendingGeneration = capture.clock
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
    pure
        ApiDappTransactionContextResponse
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
            , records = ApiDappHex <$> records
            , contextDigest = ApiDappHex contextDigest
            , contextToken = ApiDappHex contextToken
            }
resolveOutput
    :: Set Ledger.TxIn
    -> Map Ledger.TxIn (Set ApiDappRole)
    -> Map Ledger.TxIn (Output Read.Conway, ByteString)
    -> Map Ledger.TxIn (Output Read.Conway, ByteString)
    -> Ledger.TxIn
    -> Either DappError (ByteString, ApiDappContextOutput)
resolveOutput available roleMap pending node input = do
    (output, source, provenance) <- case (Map.lookup input pending, Map.lookup input node) of
        (Just (pendingOutput, pendingBytes), Just (nodeOutput, nodeBytes)) -> do
            requireEither DappContextConflictError $ pendingBytes == nodeBytes
            requireEither DappInternalErrorResponse $ pendingOutput == nodeOutput
            pure (pendingOutput, pendingBytes, [Pending, Node])
        (Just (pendingOutput, pendingBytes), Nothing) ->
            pure (pendingOutput, pendingBytes, [Pending])
        (Nothing, Just (nodeOutput, nodeBytes)) ->
            pure (nodeOutput, nodeBytes, [Node])
        (Nothing, Nothing) -> Left DappContextUnavailableError
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
    pure (encoded, value)
  where
    hasRole role = maybe False (Set.member role) $ Map.lookup input roleMap

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
