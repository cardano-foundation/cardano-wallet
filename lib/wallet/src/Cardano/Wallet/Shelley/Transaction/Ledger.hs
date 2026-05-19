{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

-- |
-- Copyright: © 2024 Cardano Foundation
-- License: Apache-2.0
--
-- Transaction construction using ledger types directly,
-- bypassing @Cardano.Api@.
module Cardano.Wallet.Shelley.Transaction.Ledger
    ( -- * Transaction construction
      mkTransaction
    , constructUnsignedTxLedger
    , buildLedgerTx
    , buildLedgerTxRaw

      -- * Script-witness builder argument
    , ScriptWitnesses (..)
    , noScriptWitnesses

      -- * Sealing
    , sealWriteTx

      -- * Signing
    , signTransaction
    , mkShelleyWitnessLedger
    , mkByronWitnessLedger

      -- * Payload
    , TxPayload (..)

      -- * Constraints
    , txConstraints

      -- * Witness tags
    , TxWitnessTag (..)
    , txWitnessTagForKey

      -- * Reward withdrawal cost estimation
    , _txRewardWithdrawalCost
    , _txRewardWithdrawalSize

      -- * Ledger-native certificates
    , certificateFromDelegationActionLedger
    , certificateFromVotingActionLedger
    -- TODO: mkUnsignedTransaction uses Cardano.NetworkId,
    -- Cardano.Certificate, and other cardano-api types
    -- pervasively. Needs a ledger-native rewrite.
    ) where

import Cardano.Address.Derivation
    ( XPrv
    , XPub
    , toXPub
    , xpubPublicKey
    )
import Cardano.Address.KeyHash
    ( KeyHash (..)
    )
import Cardano.Address.Script
    ( Script (..)
    )
import Cardano.Balance.Tx.Eras
    ( IsRecentEra
    , RecentEra (..)
    )
import Cardano.Balance.Tx.SizeEstimation
    ( TxWitnessTag (..)
    )
import Cardano.Crypto.DSIGN
    ( SignedDSIGN (SignedDSIGN)
    , rawDeserialiseSigDSIGN
    , rawDeserialiseVerKeyDSIGN
    )
import Cardano.Crypto.Hash.Class
    ( Hash (UnsafeHash)
    , hashToBytes
    )
import Cardano.Ledger.Address
    ( Withdrawals (Withdrawals)
    )
import Cardano.Ledger.Allegra.Scripts
    ( ValidityInterval (..)
    )
import Cardano.Ledger.Api
    ( bodyTxL
    , outputsTxBodyL
    , referenceInputsTxBodyL
    , referenceScriptTxOutL
    , scriptTxWitsL
    , witsTxL
    )
import Cardano.Ledger.BaseTypes
    ( Network (Mainnet, Testnet)
    , StrictMaybe (SJust, SNothing)
    )
import Cardano.Ledger.Core
    ( TxCert
    )
import Cardano.Ledger.Hashes
    ( EraIndependentTxBody
    , HashAnnotated (hashAnnotated)
    , extractHash
    )
import Cardano.Ledger.Keys
    ( VKey (VKey)
    )
import Cardano.Ledger.Keys.Bootstrap
    ( BootstrapWitness
    , makeBootstrapWitness
    )
import Cardano.Ledger.Keys.WitVKey
    ( WitVKey (WitVKey)
    )
import Cardano.Ledger.Mary.Value
    ( MultiAsset
    )
import Cardano.Ledger.Metadata
    ( Metadatum
    )
import Cardano.Wallet.Address.Derivation
    ( Depth (..)
    , RewardAccount
    )
import Cardano.Wallet.Address.Derivation.Shelley
    ( toRewardAccountRaw
    )
import Cardano.Wallet.Address.Encoding
    ( toHDPayloadAddress
    )
import Cardano.Wallet.Flavor
    ( KeyFlavorS
    )
import Cardano.Wallet.Primitive.Ledger.Convert
    ( Convert (toLedger)
    , toLedgerDelegatee
    , toLedgerMintValue
    , toLedgerTimelockScript
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.TxExtended
    ( getTxExtended
    )
import Cardano.Wallet.Primitive.Ledger.Shelley
    ( toCardanoLovelace
    , toLedgerStakeCredential
    )
import Cardano.Wallet.Primitive.Passphrase
    ( Passphrase (..)
    )
import Cardano.Wallet.Primitive.Types.Address
    ( Address
    )
import Cardano.Wallet.Primitive.Types.AssetId
    ( AssetId (..)
    )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin
    )
import Cardano.Wallet.Primitive.Types.TokenMapWithScripts
    ( ReferenceInput (..)
    )
import Cardano.Wallet.Primitive.Types.Tx
    ( SealedTx
    )
import Cardano.Wallet.Primitive.Types.Tx.Constraints
    ( txOutMaxTokenQuantity
    )
import Cardano.Wallet.Primitive.Types.Tx.SealedTx
    ( sealedTxFromLedgerTx
    )
import Cardano.Wallet.Primitive.Types.Tx.Tx
    ( TxMetadata
    )
import Cardano.Wallet.Primitive.Types.Tx.TxExtended
    ( TxExtended (walletTx)
    )
import Cardano.Wallet.Primitive.Types.Tx.TxIn
    ( TxIn
    )
import Cardano.Wallet.Primitive.Types.Tx.TxMetadata
    ( toShelleyMetadata
    , unTxMetadata
    )
import Cardano.Wallet.Primitive.Types.Tx.TxOut
    ( TxOut (..)
    )
import Cardano.Wallet.Shelley.Transaction
    ( signTransaction
    , txConstraints
    , txWitnessTagForKey
    , _txRewardWithdrawalCost
    , _txRewardWithdrawalSize
    )
import Cardano.Wallet.Shelley.Transaction.Build
    ( mkLedgerTx
    )
import Cardano.Wallet.Transaction
    ( DelegationAction (..)
    , ErrMkTransaction (..)
    , ErrMkTransactionOutputTokenQuantityExceedsLimitError (..)
    , PreSelection (..)
    , ScriptSource
    , SelectionOf (..)
    , TransactionCtx (..)
    , VotingAction (..)
    , Withdrawal (..)
    , WitnessCountCtx (..)
    , selectionDelta
    )
import Control.Lens
    ( (%~)
    , (&)
    , (.~)
    )
import Control.Monad
    ( forM_
    , when
    )
import Cryptography.Hash.Blake
    ( blake2b224
    )
import Data.ByteString.Short
    ( toShort
    )
import Data.Generics.Internal.VL.Lens
    ( view
    )
import Data.Map.Strict
    ( Map
    )
import Data.Maybe
    ( maybeToList
    )
import Data.Sequence.Strict
    ( StrictSeq
    )
import Data.Set
    ( Set
    )
import Data.Word
    ( Word64
    )
import Ouroboros.Network.Block
    ( SlotNo
    )
import Prelude

import qualified Cardano.Api as Cardano
import qualified Cardano.Api.Byron as Byron
import qualified Cardano.Balance.Tx.Eras as Write
import qualified Cardano.Balance.Tx.Tx as Write
import qualified Cardano.Crypto as CC
import qualified Cardano.Crypto.Wallet as Crypto.HD
import qualified Cardano.Ledger.Address as Ledger
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import qualified Cardano.Ledger.Api as LApi
import qualified Cardano.Ledger.Coin as Ledger
import qualified Cardano.Ledger.Conway.TxCert as Conway
import qualified Cardano.Ledger.Credential as Cred
import qualified Cardano.Ledger.Keys as Keys
import qualified Cardano.Ledger.TxIn as Ledger
import qualified Cardano.Wallet.Primitive.Ledger.Convert as Convert
import qualified Cardano.Wallet.Primitive.Types.Coin as W
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap
import qualified Cardano.Wallet.Primitive.Types.Tx.Tx as W
import qualified Cardano.Wallet.Read as Read
import qualified Data.ByteString as BS
import qualified Data.Foldable as F
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set

-- | Transaction payload using ledger types directly.
--
-- Replaces the cardano-api 'Cardano.Wallet.Shelley.Transaction.TxPayload'
-- which uses @Cardano.TxMetadata@ and @Cardano.Certificate@.
data TxPayload era = TxPayload
    { _metadata :: Maybe TxMetadata
    -- ^ User or application-defined metadata to be
    -- included in the transaction.
    , _certificates :: StrictSeq (TxCert era)
    -- ^ Certificates to be included in the transaction.
    }

-- | Script-witness inputs accepted by 'buildLedgerTx' and
-- 'buildLedgerTxRaw'.
--
-- Mirrors the script-witness surface of the legacy
-- 'Cardano.Wallet.Shelley.Transaction.mkUnsignedTx': native-script
-- spending witnesses, an optional staking script, mint/burn
-- 'ScriptSource' entries, and an output-attached reference script.
--
-- All fields default to "no witness" via 'noScriptWitnesses'. Existing
-- (non-script) call sites pass 'noScriptWitnesses' so this change is
-- bisect-safe.
--
-- See @specs\/008-script-witness-parity\/data-model.md@ for the
-- body\/witness-set mapping each field touches.
data ScriptWitnesses = ScriptWitnesses
    { swNativeInputs :: Map TxIn (Script KeyHash)
    -- ^ Mirrors 'TransactionCtx.txNativeScriptInputs'. Each entry
    -- pushes the script into the witness set under its 'hashScript'.
    , swStakingScript :: Maybe (Script KeyHash)
    -- ^ Mirrors the resolved
    -- 'TransactionCtx.txStakingCredentialScriptTemplate'. Pushed
    -- into the witness set; the caller is responsible for using the
    -- corresponding script hash as the withdrawal / cert credential.
    , swMintingSources :: Map AssetId ScriptSource
    -- ^ Mirrors the union of the mint and burn
    -- 'TransactionCtx.txAssetsTo{Mint,Burn}' script-source maps.
    -- @Left script@ entries are added to the witness set; @Right
    -- (ReferenceInput txin)@ entries are added to
    -- 'referenceInputsTxBodyL'.
    , swReferenceScript :: Maybe (Script KeyHash)
    -- ^ Mirrors 'TransactionCtx.txReferenceScript'. Attached as the
    -- reference script of the first output (if any).
    }

-- | The "no scripts" default; equivalent to the pre-#5288 behavior.
noScriptWitnesses :: ScriptWitnesses
noScriptWitnesses =
    ScriptWitnesses
        { swNativeInputs = Map.empty
        , swStakingScript = Nothing
        , swMintingSources = Map.empty
        , swReferenceScript = Nothing
        }

-- | Build and sign a transaction using ledger types
-- directly.
--
-- Uses 'mkLedgerTx' instead of the cardano-api unsigned transaction
-- builder, and converts wallet types to ledger types internally.
mkTransaction
    :: forall era k
     . IsRecentEra era
    => RecentEra era
    -> Cardano.NetworkId
    -> KeyFlavorS k
    -> (XPrv, Passphrase "encryption")
    -> ( Address
         -> Maybe
                (k 'CredFromKeyK XPrv, Passphrase "encryption")
       )
    -> TransactionCtx
    -> SelectionOf TxOut
    -> Either ErrMkTransaction (W.Tx, SealedTx)
mkTransaction
    era
    net
    keyF
    stakeCreds
    addrResolver
    ctx
    cs = do
        let network = ledgerNetworkOf net
        let ttl = txValidityInterval ctx
        let wdrl = view #txWithdrawal ctx
        let delta = selectionDelta cs
        let md = view #txMetadata ctx
        let ledgerCerts =
                case view #txDelegationAction ctx of
                    Nothing -> []
                    Just action ->
                        let stakeXPub =
                                toXPub $ fst stakeCreds
                        in  certificateFromDelegationActionLedger
                                era
                                (Left stakeXPub)
                                (view #txDeposit ctx)
                                action
        outs <- extractValidatedOutputs (Right cs)
        let ledgerMint =
                toLedgerMintValue
                    (fst $ view #txAssetsToMint ctx)
                    (fst $ view #txAssetsToBurn ctx)
        let ledgerTx =
                buildLedgerTx
                    era
                    ttl
                    network
                    wdrl
                    delta
                    md
                    ledgerCerts
                    outs
                    cs
                    ledgerMint
                    noScriptWitnesses
        let signed =
                signTransaction
                    keyF
                    net
                    AnyWitnessCountCtx
                    acctResolver
                    (const Nothing)
                    Nothing
                    (const Nothing)
                    addrResolver
                    inputResolver
                    ledgerTx
        let (txExt, sealed) = case era of
                RecentEraConway ->
                    let readTx =
                            Read.Tx signed
                                :: Read.Tx Read.Conway
                    in  ( getTxExtended readTx
                        , sealWriteTx era signed
                        )
                RecentEraDijkstra ->
                    error
                        "mkTransaction: Dijkstra \
                        \not yet supported"
        let withResolvedInputs tx =
                tx
                    { W.resolvedInputs =
                        fmap Just
                            <$> F.toList (view #inputs cs)
                    }
        Right
            ( withResolvedInputs (walletTx txExt)
            , sealed
            )
      where
        inputResolver :: TxIn -> Maybe Address
        inputResolver i =
            let index =
                    Map.fromList
                        (F.toList $ view #inputs cs)
            in  do
                    TxOut addr _ <- Map.lookup i index
                    pure addr

        acctResolver
            :: RewardAccount
            -> Maybe (XPrv, Passphrase "encryption")
        acctResolver acct =
            let (rewardAcnt, pwdAcnt) = stakeCreds
                acct' =
                    toRewardAccountRaw $ toXPub rewardAcnt
            in  if acct == acct'
                    then Just (rewardAcnt, pwdAcnt)
                    else Nothing

-- | Build an unsigned transaction using ledger types
-- directly.
--
-- Replaces 'constructUnsignedTx' by calling 'mkLedgerTx'
-- instead of 'mkUnsignedTx'.
constructUnsignedTxLedger
    :: forall era
     . IsRecentEra era
    => RecentEra era
    -> Network
    -> ( Maybe TxMetadata
       , [TxCert era]
       )
    -> (Maybe SlotNo, SlotNo)
    -> Withdrawal
    -> (TokenMap.TokenMap, TokenMap.TokenMap)
    -> Either PreSelection (SelectionOf TxOut)
    -> W.Coin
    -> Either ErrMkTransaction (Write.Tx era)
constructUnsignedTxLedger
    era
    network
    (md, lCerts)
    ttl
    wdrl
    (mint, burn)
    cs
    fee = do
        outs <- extractValidatedOutputs cs
        let ledgerMint = toLedgerMintValue mint burn
        Right
            $ buildLedgerTxRaw
                era
                ttl
                network
                wdrl
                fee
                md
                lCerts
                outs
                cs
                ledgerMint
                noScriptWitnesses

-- | Convert wallet/context types and call 'mkLedgerTx'. Threads
-- 'ScriptWitnesses' through to the body and witness-set lenses
-- (per @specs\/008-script-witness-parity\/data-model.md@).
buildLedgerTx
    :: forall era
     . IsRecentEra era
    => RecentEra era
    -> (Maybe SlotNo, SlotNo)
    -> Network
    -> Withdrawal
    -> W.Coin
    -> Maybe TxMetadata
    -> [TxCert era]
    -> [TxOut]
    -> SelectionOf TxOut
    -> MultiAsset
    -> ScriptWitnesses
    -> Write.Tx era
buildLedgerTx era ttl network wdrl fee md certs outs cs mint sws =
    installScriptWitnesses era sws
        $ mkLedgerTx
            era
            ins
            ledgerOuts
            ledgerFee
            validity
            wdrls
            ledgerCerts
            mint
            metadata
  where
    ins :: Set Ledger.TxIn
    ins =
        Set.fromList
            $ map (toLedger . fst)
            $ F.toList (view #inputs cs)

    ledgerOuts :: StrictSeq (Write.TxOut era)
    ledgerOuts =
        StrictSeq.fromList $ map toLedgerTxOut outs

    ledgerFee :: Ledger.Coin
    ledgerFee = toCardanoLovelace fee

    validity :: ValidityInterval
    validity = mkValidityInterval ttl

    wdrls :: Withdrawals
    wdrls = mkWithdrawalsLedger network wdrl

    ledgerCerts :: StrictSeq (TxCert era)
    ledgerCerts = StrictSeq.fromList certs

    metadata :: Map Word64 Metadatum
    metadata = case md of
        Nothing -> Map.empty
        Just m -> toShelleyMetadata (unTxMetadata m)

-- | Lower-level builder that takes already-converted
-- types. Like 'buildLedgerTx', it accepts a 'ScriptWitnesses' record
-- whose default ('noScriptWitnesses') reproduces the pre-#5288
-- behavior.
buildLedgerTxRaw
    :: forall era
     . IsRecentEra era
    => RecentEra era
    -> (Maybe SlotNo, SlotNo)
    -> Network
    -> Withdrawal
    -> W.Coin
    -> Maybe TxMetadata
    -> [TxCert era]
    -> [TxOut]
    -> Either PreSelection (SelectionOf TxOut)
    -> MultiAsset
    -> ScriptWitnesses
    -> Write.Tx era
buildLedgerTxRaw
    era
    ttl
    network
    wdrl
    fee
    md
    certs
    outs
    cs
    mint
    sws =
        installScriptWitnesses era sws
            $ mkLedgerTx
                era
                ins
                ledgerOuts
                ledgerFee
                validity
                wdrls
                ledgerCerts
                mint
                metadata
      where
        ins :: Set Ledger.TxIn
        ins = case cs of
            Right selOf ->
                Set.fromList
                    $ map (toLedger . fst)
                    $ F.toList (view #inputs selOf)
            Left _preSel ->
                Set.empty

        ledgerOuts :: StrictSeq (Write.TxOut era)
        ledgerOuts =
            StrictSeq.fromList $ map toLedgerTxOut outs

        ledgerFee :: Ledger.Coin
        ledgerFee = toCardanoLovelace fee

        validity :: ValidityInterval
        validity = mkValidityInterval ttl

        wdrls :: Withdrawals
        wdrls = mkWithdrawalsLedger network wdrl

        ledgerCerts :: StrictSeq (TxCert era)
        ledgerCerts = StrictSeq.fromList certs

        metadata :: Map Word64 Metadatum
        metadata = case md of
            Nothing -> Map.empty
            Just m -> toShelleyMetadata (unTxMetadata m)

-- | Install the script-witness extras on a freshly-built ledger
-- 'Tx': reference inputs collected from 'swMintingSources'
-- @Right@-branches, the first-output reference script attachment
-- from 'swReferenceScript', and the native-script witness set
-- from 'swNativeInputs' values, one local script per minting policy
-- from 'swMintingSources' @Left@-branches, and 'swStakingScript'.
installScriptWitnesses
    :: RecentEra era
    -> ScriptWitnesses
    -> Write.Tx era
    -> Write.Tx era
installScriptWitnesses era sws tx = case era of
    RecentEraConway -> installScriptWitnessesConway sws tx
    RecentEraDijkstra ->
        error
            "installScriptWitnesses: Dijkstra not yet supported"

-- | Conway-specialised core of 'installScriptWitnesses'. The
-- type signature pins @era ~ Conway@ so 'Alonzo.NativeScript' can
-- resolve its 'Core.NativeScript era ~ Timelock era' constraint
-- from 'RecentEraConstraints'.
installScriptWitnessesConway
    :: ScriptWitnesses
    -> Write.Tx Write.Conway
    -> Write.Tx Write.Conway
installScriptWitnessesConway sws tx =
    tx
        & bodyTxL . referenceInputsTxBodyL
            .~ refInputs
        & bodyTxL . outputsTxBodyL
            %~ attachReferenceScript (swReferenceScript sws)
        & witsTxL . scriptTxWitsL
            .~ scriptWits
  where
    refInputs :: Set Ledger.TxIn
    refInputs =
        Set.fromList
            [ toLedger txin
            | Right (ReferenceInput txin) <-
                Map.elems (swMintingSources sws)
            ]

    scriptWits
        :: Map
            LApi.ScriptHash
            (Alonzo.AlonzoScript Write.Conway)
    scriptWits =
        let nativeMintScripts =
                Map.elems
                    $ Map.foldlWithKey'
                        collectMintScript
                        Map.empty
                        (swMintingSources sws)
            stakingScripts =
                maybeToList (swStakingScript sws)
            allScripts =
                Map.elems (swNativeInputs sws)
                    ++ nativeMintScripts
                    ++ stakingScripts
            toLedgerScript
                :: Script KeyHash -> Alonzo.AlonzoScript Write.Conway
            toLedgerScript script =
                Alonzo.NativeScript (toLedgerTimelockScript script)
        in  Map.fromList
                [ ( LApi.hashScript @Write.Conway ledgerScript
                  , ledgerScript
                  )
                | script <- allScripts
                , let ledgerScript = toLedgerScript script
                ]

    collectMintScript scriptsByPolicy aid = \case
        Left script ->
            Map.insertWith
                (\_ existing -> existing)
                (policyId aid)
                script
                scriptsByPolicy
        Right _ -> scriptsByPolicy

    attachReferenceScript
        :: Maybe (Script KeyHash)
        -> StrictSeq (Write.TxOut Write.Conway)
        -> StrictSeq (Write.TxOut Write.Conway)
    attachReferenceScript Nothing outs = outs
    attachReferenceScript (Just script) outs =
        case outs of
            StrictSeq.Empty -> outs
            firstOut StrictSeq.:<| rest ->
                let scriptVal :: Alonzo.AlonzoScript Write.Conway
                    scriptVal =
                        Alonzo.NativeScript
                            (toLedgerTimelockScript script)
                    firstOut' =
                        firstOut
                            & referenceScriptTxOutL
                                .~ SJust scriptVal
                in  firstOut' StrictSeq.:<| rest

-- | Convert a wallet 'TxOut' to a ledger 'TxOut'.
toLedgerTxOut
    :: forall era
     . IsRecentEra era
    => TxOut
    -> Write.TxOut era
toLedgerTxOut =
    case Write.recentEra @era :: Write.RecentEra era of
        RecentEraConway -> Convert.toConwayTxOut
        RecentEraDijkstra ->
            error
                "toLedgerTxOut: Dijkstra not yet supported"

-- | Convert a @(Maybe SlotNo, SlotNo)@ validity interval
-- pair to a ledger 'ValidityInterval'.
mkValidityInterval
    :: (Maybe SlotNo, SlotNo) -> ValidityInterval
mkValidityInterval (mStart, end) =
    ValidityInterval
        { invalidBefore = maybe SNothing SJust mStart
        , invalidHereafter = SJust end
        }

-- | Build ledger 'Withdrawals' directly from wallet types.
mkWithdrawalsLedger
    :: Network -> Withdrawal -> Withdrawals
mkWithdrawalsLedger network = \case
    NoWithdrawal -> Withdrawals Map.empty
    WithdrawalExternal acc _ amt _ ->
        Withdrawals
            $ Map.singleton
                (mkRewardAccount network acc)
                (toCardanoLovelace amt)
    WithdrawalSelf acc _ amt ->
        Withdrawals
            $ Map.singleton
                (mkRewardAccount network acc)
                (toCardanoLovelace amt)

-- | Build a ledger 'RewardAccount' from a wallet
-- 'RewardAccount' and a 'Network'.
mkRewardAccount
    :: Network
    -> RewardAccount
    -> Ledger.AccountAddress
mkRewardAccount network acct =
    Ledger.AccountAddress
        network
        (Ledger.AccountId (toLedgerStakeCredential acct))

-- | Seal a ledger 'Write.Tx' into a 'SealedTx'. Ledger-native;
-- does not round-trip through cardano-api.
sealWriteTx
    :: forall era
     . RecentEra era
    -> Write.Tx era
    -> SealedTx
sealWriteTx era tx = case era of
    RecentEraConway ->
        sealedTxFromLedgerTx (Read.Tx tx :: Read.Tx Read.Conway)
    RecentEraDijkstra ->
        sealedTxFromLedgerTx (Read.Tx tx :: Read.Tx Read.Dijkstra)

-- | Project a 'Cardano.NetworkId' onto the ledger's
-- 'Network' (magic-free) for cert / withdrawal construction.
ledgerNetworkOf :: Cardano.NetworkId -> Network
ledgerNetworkOf Cardano.Mainnet = Mainnet
ledgerNetworkOf (Cardano.Testnet _) = Testnet

-- | Validate transaction outputs (token quantities).
extractValidatedOutputs
    :: Either PreSelection (SelectionOf TxOut)
    -> Either ErrMkTransaction [TxOut]
extractValidatedOutputs sel =
    mapM validateOut $ case sel of
        Right selOf ->
            view #outputs selOf
                ++ F.toList (view #change selOf)
        Left preSel -> view #outputs preSel
  where
    validateOut
        :: TxOut -> Either ErrMkTransaction TxOut
    validateOut out = do
        verifyOutputTokenQuantities out
        return out
      where
        verifyOutputTokenQuantities
            :: TxOut -> Either ErrMkTransaction ()
        verifyOutputTokenQuantities
            ( TxOut
                    addr
                    (TokenBundle.TokenBundle _ toks)
                ) =
                forM_
                    (TokenMap.toFlatList toks)
                    $ \(aid, qty) ->
                        when
                            (qty > txOutMaxTokenQuantity)
                            $ Left
                            $ ErrMkTransactionOutputTokenQuantityExceedsLimit
                            $ ErrMkTransactionOutputTokenQuantityExceedsLimitError
                                { address = addr
                                , asset = aid
                                , quantity = qty
                                , quantityMaxBound =
                                    txOutMaxTokenQuantity
                                }

{--------------------------------------------------------------
    Ledger-native certificates
--------------------------------------------------------------}

-- | Build ledger delegation certificates directly,
-- without going through cardano-api.
certificateFromDelegationActionLedger
    :: RecentEra era
    -> Either XPub (Script KeyHash)
    -> Maybe Coin
    -> DelegationAction
    -> [TxCert era]
certificateFromDelegationActionLedger
    RecentEraConway
    cred
    depositM
    da =
        case (da, depositM) of
            (Join poolId, _) ->
                [ Conway.mkDelegTxCert
                    (toLedgerStakeCred cred)
                    ( toLedgerDelegatee
                        (Just poolId)
                        Nothing
                    )
                ]
            (JoinRegisteringKey poolId, Just deposit) ->
                [ Conway.mkRegDepositTxCert
                    (toLedgerStakeCred cred)
                    (toLedgerCoin deposit)
                , Conway.mkDelegTxCert
                    (toLedgerStakeCred cred)
                    ( toLedgerDelegatee
                        (Just poolId)
                        Nothing
                    )
                ]
            (JoinRegisteringKey _, Nothing) ->
                error
                    "certificateFromDelegationAction\
                    \Ledger: deposit required in \
                    \Conway era for registration \
                    \(joining)"
            (Quit, Just deposit) ->
                [ Conway.mkUnRegDepositTxCert
                    (toLedgerStakeCred cred)
                    (toLedgerCoin deposit)
                ]
            (Quit, Nothing) ->
                error
                    "certificateFromDelegationAction\
                    \Ledger: deposit required in \
                    \Conway era for registration \
                    \(quitting)"
certificateFromDelegationActionLedger
    RecentEraDijkstra
    _
    _
    _ =
        error
            "certificateFromDelegationAction\
            \Ledger: Dijkstra not yet supported"

-- | Build ledger voting certificates directly,
-- without going through cardano-api.
certificateFromVotingActionLedger
    :: RecentEra era
    -> Either XPub (Script KeyHash)
    -> Maybe Coin
    -> VotingAction
    -> [TxCert era]
certificateFromVotingActionLedger
    RecentEraConway
    cred
    depositM
    va =
        case (va, depositM) of
            (Vote action, _) ->
                [ Conway.mkDelegTxCert
                    (toLedgerStakeCred cred)
                    ( toLedgerDelegatee
                        Nothing
                        (Just action)
                    )
                ]
            (VoteRegisteringKey action, Just deposit) ->
                [ Conway.mkRegDepositTxCert
                    (toLedgerStakeCred cred)
                    (toLedgerCoin deposit)
                , Conway.mkDelegTxCert
                    (toLedgerStakeCred cred)
                    ( toLedgerDelegatee
                        Nothing
                        (Just action)
                    )
                ]
            (VoteRegisteringKey _, Nothing) ->
                error
                    "certificateFromVotingAction\
                    \Ledger: deposit required in \
                    \Conway era for registration"
certificateFromVotingActionLedger
    RecentEraDijkstra
    _
    _
    _ =
        error
            "certificateFromVotingAction\
            \Ledger: Dijkstra not yet supported"

-- | Build a ledger 'StakeCredential' from an XPub
-- or script, without cardano-api.
toLedgerStakeCred
    :: Either XPub (Script KeyHash)
    -> Cred.StakeCredential
toLedgerStakeCred = \case
    Left xpub ->
        Cred.KeyHashObj
            . Keys.KeyHash
            . UnsafeHash
            . toShort
            . blake2b224
            $ xpubPublicKey xpub
    Right script ->
        Cred.ScriptHashObj
            $ LApi.hashScript
                @LApi.ConwayEra
            $ Alonzo.NativeScript
            $ toLedgerTimelockScript script

-- | Convert a wallet 'Coin' to a ledger 'Coin'.
toLedgerCoin :: Coin -> Ledger.Coin
toLedgerCoin = toCardanoLovelace

-- | Construct a Shelley-era key witness directly from
-- ledger types, bypassing cardano-api.
--
-- Signs the transaction body hash with the extended
-- private key and constructs a 'WitVKey'.
mkShelleyWitnessLedger
    :: forall era
     . Write.IsRecentEra era
    => RecentEra era
    -> Write.TxBody era
    -> (XPrv, Passphrase "encryption")
    -> Keys.WitVKey Keys.Witness
mkShelleyWitnessLedger _era body (xprv, pwd) =
    WitVKey vkey sig
  where
    xprv' =
        Crypto.HD.xPrvChangePass pwd BS.empty xprv
    bodyHash =
        hashToBytes
            $ extractHash
            $ hashAnnotated @_ @EraIndependentTxBody body
    xsig =
        Crypto.HD.sign
            (BS.empty :: BS.ByteString)
            xprv'
            bodyHash
    vkey =
        case rawDeserialiseVerKeyDSIGN
            (Crypto.HD.xpubPublicKey $ toXPub xprv') of
            Just vk -> VKey vk
            Nothing ->
                error
                    "mkShelleyWitnessLedger: \
                    \invalid public key"
    sig =
        case rawDeserialiseSigDSIGN
            (Crypto.HD.unXSignature xsig) of
            Just s -> SignedDSIGN s
            Nothing ->
                error
                    "mkShelleyWitnessLedger: \
                    \invalid signature"

-- | Construct a Byron bootstrap witness directly from
-- ledger types, bypassing cardano-api.
--
-- Hashes the transaction body and creates a bootstrap
-- witness with Byron address attributes.
mkByronWitnessLedger
    :: forall era
     . Write.IsRecentEra era
    => RecentEra era
    -> Write.TxBody era
    -> Cardano.NetworkId
    -> Address
    -> (XPrv, Passphrase "encryption")
    -> BootstrapWitness
mkByronWitnessLedger _era body net addr (xprv, pwd) =
    makeBootstrapWitness txHash signingKey addrAttr
  where
    txHash =
        extractHash $ hashAnnotated @_ @EraIndependentTxBody body
    signingKey =
        CC.SigningKey
            $ Crypto.HD.xPrvChangePass pwd BS.empty xprv
    addrAttr =
        Byron.mkAttributes
            $ Byron.AddrAttributes
                (toHDPayloadAddress addr)
                networkMagic
    networkMagic = case net of
        Cardano.Mainnet -> Byron.NetworkMainOrStage
        Cardano.Testnet (Cardano.NetworkMagic m) ->
            Byron.NetworkTestnet m
