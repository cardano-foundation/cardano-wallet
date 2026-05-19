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
-- Unsigned Shelley transaction construction using ledger types directly,
-- bypassing @Cardano.Api.TxBodyContent@.
module Cardano.Wallet.Shelley.Transaction.Unsigned
    ( -- * Payload
      TxPayload (..)

      -- * Script-witness builder argument
    , ScriptWitnesses (..)
    , noScriptWitnesses

      -- * Transaction construction
    , constructUnsignedTxLedger
    , buildLedgerTx
    , buildLedgerTxRaw

      -- * Conversions and validation
    , ledgerNetworkOf
    , extractValidatedOutputs

      -- * Ledger-native certificates
    , certificateFromDelegationActionLedger
    , certificateFromVotingActionLedger
    ) where

import Cardano.Address.Derivation
    ( XPub
    , xpubPublicKey
    )
import Cardano.Address.KeyHash
    ( KeyHash
    )
import Cardano.Address.Script
    ( Script
    )
import Cardano.Balance.Tx.Eras
    ( IsRecentEra
    , RecentEra (..)
    )
import Cardano.Crypto.Hash.Class
    ( Hash (UnsafeHash)
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
import Cardano.Ledger.Mary.Value
    ( MultiAsset
    )
import Cardano.Ledger.Metadata
    ( Metadatum
    )
import Cardano.Wallet.Address.Derivation
    ( RewardAccount
    )
import Cardano.Wallet.Primitive.Ledger.Convert
    ( Convert (toLedger)
    , toLedgerDelegatee
    , toLedgerMintValue
    , toLedgerTimelockScript
    )
import Cardano.Wallet.Primitive.Ledger.Shelley
    ( toCardanoLovelace
    , toLedgerStakeCredential
    )
import Cardano.Wallet.Primitive.Types.AssetId
    ( AssetId (..)
    )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin
    )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( TokenMap
    )
import Cardano.Wallet.Primitive.Types.Tx.Constraints
    ( txOutMaxTokenQuantity
    )
import Cardano.Wallet.Primitive.Types.Tx.Tx
    ( TxMetadata
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
import Cardano.Wallet.Shelley.Transaction.Build
    ( mkLedgerTx
    )
import Cardano.Wallet.Transaction
    ( DelegationAction (..)
    , ErrMkTransaction (..)
    , ErrMkTransactionOutputTokenQuantityExceedsLimitError (..)
    , PreSelection (..)
    , ReferenceInput (..)
    , ScriptSource
    , SelectionOf (..)
    , VotingAction (..)
    , Withdrawal (..)
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
import qualified Cardano.Balance.Tx.Eras as Write
import qualified Cardano.Balance.Tx.Tx as Write
import qualified Cardano.Ledger.Address as Ledger
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import qualified Cardano.Ledger.Api as LApi
import qualified Cardano.Ledger.Coin as Ledger
import qualified Cardano.Ledger.Conway.TxCert as Conway
import qualified Cardano.Ledger.Credential as Cred
import qualified Cardano.Ledger.Keys as Keys
import qualified Cardano.Ledger.TxIn as Ledger
import qualified Cardano.Wallet.Primitive.Ledger.Convert as Convert
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap
import qualified Data.Foldable as F
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set

-- | Transaction payload using ledger types directly.
data TxPayload era = TxPayload
    { _metadata :: Maybe TxMetadata
    -- ^ User or application-defined metadata to be included in the
    -- transaction.
    , _certificates :: StrictSeq (TxCert era)
    -- ^ Certificates to be included in the transaction.
    }

-- | Script-witness inputs accepted by 'buildLedgerTx' and
-- 'buildLedgerTxRaw'.
data ScriptWitnesses = ScriptWitnesses
    { swNativeInputs :: Map TxIn (Script KeyHash)
    -- ^ Native-script spending witnesses.
    , swStakingScript :: Maybe (Script KeyHash)
    -- ^ Optional staking script witness.
    , swMintingSources :: Map AssetId ScriptSource
    -- ^ Mint/burn script sources; reference inputs go into the body.
    , swReferenceScript :: Maybe (Script KeyHash)
    -- ^ Optional reference script attached to the first output.
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

-- | Build an unsigned transaction using ledger types directly.
constructUnsignedTxLedger
    :: forall era
     . IsRecentEra era
    => RecentEra era
    -> Network
    -> (Maybe TxMetadata, [TxCert era])
    -> (Maybe SlotNo, SlotNo)
    -> Withdrawal
    -> (TokenMap, TokenMap)
    -> Either PreSelection (SelectionOf TxOut)
    -> Coin
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
-- 'ScriptWitnesses' through to the body and witness-set lenses.
buildLedgerTx
    :: forall era
     . IsRecentEra era
    => RecentEra era
    -> (Maybe SlotNo, SlotNo)
    -> Network
    -> Withdrawal
    -> Coin
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

-- | Lower-level builder that takes already-converted types.
buildLedgerTxRaw
    :: forall era
     . IsRecentEra era
    => RecentEra era
    -> (Maybe SlotNo, SlotNo)
    -> Network
    -> Withdrawal
    -> Coin
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

-- | Install script-witness extras on a freshly-built ledger 'Tx'.
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

mkValidityInterval
    :: (Maybe SlotNo, SlotNo) -> ValidityInterval
mkValidityInterval (mStart, end) =
    ValidityInterval
        { invalidBefore = maybe SNothing SJust mStart
        , invalidHereafter = SJust end
        }

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

mkRewardAccount
    :: Network
    -> RewardAccount
    -> Ledger.AccountAddress
mkRewardAccount network acct =
    Ledger.AccountAddress
        network
        (Ledger.AccountId (toLedgerStakeCredential acct))

-- | Project a 'Cardano.NetworkId' onto the ledger's 'Network'.
ledgerNetworkOf :: Cardano.NetworkId -> Network
ledgerNetworkOf Cardano.Mainnet = Mainnet
ledgerNetworkOf (Cardano.Testnet _) = Testnet

-- | Validate transaction outputs for token quantity bounds.
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

toLedgerCoin :: Coin -> Ledger.Coin
toLedgerCoin = toCardanoLovelace
