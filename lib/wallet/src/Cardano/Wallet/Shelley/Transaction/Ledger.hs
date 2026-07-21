{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
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
    , mkShelleyWitnessFromExtKeyMaterial
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
    ) where

import Cardano.Address.Derivation
    ( XPrv
    , toXPub
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
    ( hashToBytes
    )
import Cardano.Crypto.WalletHD.Encrypted
    ( ExtKeyMaterial
    , Validated
    , extKeyMaterialPublicKey
    , publicKeyByteString
    , signWithExtKeyMaterial
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
    ( toLedgerMintValue
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.TxExtended
    ( getTxExtended
    )
import Cardano.Wallet.Primitive.Passphrase
    ( Passphrase (..)
    )
import Cardano.Wallet.Primitive.Types.Address
    ( Address
    )
import Cardano.Wallet.Primitive.Types.Tx
    ( SealedTx
    )
import Cardano.Wallet.Primitive.Types.Tx.SealedTx
    ( sealedTxFromLedgerTx
    )
import Cardano.Wallet.Primitive.Types.Tx.TxExtended
    ( TxExtended (walletTx)
    )
import Cardano.Wallet.Primitive.Types.Tx.TxIn
    ( TxIn
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
import Cardano.Wallet.Shelley.Transaction.Unsigned
    ( ScriptWitnesses (..)
    , TxPayload (..)
    , buildLedgerTx
    , buildLedgerTxRaw
    , certificateFromDelegationActionLedger
    , certificateFromVotingActionLedger
    , constructUnsignedTxLedger
    , extractValidatedOutputs
    , ledgerNetworkOf
    , noScriptWitnesses
    )
import Cardano.Wallet.Transaction
    ( ErrMkTransaction (..)
    , SelectionOf (..)
    , TransactionCtx (..)
    , WitnessCountCtx (..)
    , selectionDelta
    )
import Data.Generics.Internal.VL.Lens
    ( view
    )
import Prelude

import qualified Cardano.Api as Cardano
import qualified Cardano.Api.Byron as Byron
import qualified Cardano.Balance.Tx.Eras as Write
import qualified Cardano.Balance.Tx.Tx as Write
import qualified Cardano.Crypto as CC
import qualified Cardano.Crypto.Wallet as Crypto.HD
import qualified Cardano.Crypto.WalletHD.Encrypted as EncHD
    ( Signature (..)
    )
import qualified Cardano.Ledger.Keys as Keys
import qualified Cardano.Wallet.Primitive.Types.Tx.Tx as W
import qualified Cardano.Wallet.Read as Read
import qualified Data.ByteString as BS
import qualified Data.Foldable as F
import qualified Data.Map.Strict as Map

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

-- | Construct a Shelley-era key witness from a V2 'ExtKeyMaterial'.
-- Hashes the transaction body, calls 'signWithExtKeyMaterial', and
-- assembles a 'WitVKey' from the resulting signature and public key.
mkShelleyWitnessFromExtKeyMaterial
    :: forall era s
     . Write.IsRecentEra era
    => RecentEra era
    -> Write.TxBody era
    -> ExtKeyMaterial s Validated
    -> IO (Keys.WitVKey Keys.Witness)
mkShelleyWitnessFromExtKeyMaterial _era body km = do
    let bodyHash =
            hashToBytes
                $ extractHash
                $ hashAnnotated @_ @EraIndependentTxBody body
    sigResult <- signWithExtKeyMaterial km bodyHash
    case sigResult of
        Left err ->
            error $ "mkShelleyWitnessFromExtKeyMaterial: " <> show err
        Right (EncHD.Signature sigBytes) ->
            let pubBytes = publicKeyByteString (extKeyMaterialPublicKey km)
                vkey = case rawDeserialiseVerKeyDSIGN pubBytes of
                    Just vk -> VKey vk
                    Nothing ->
                        error
                            "mkShelleyWitnessFromExtKeyMaterial: \
                            \invalid public key"
                sig = case rawDeserialiseSigDSIGN sigBytes of
                    Just s -> SignedDSIGN s
                    Nothing ->
                        error
                            "mkShelleyWitnessFromExtKeyMaterial: \
                            \invalid signature"
            in  pure $ WitVKey vkey sig

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
