{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{- HLINT ignore "Use <$>" -}
{- HLINT ignore "Use camelCase" -}

-- NamedFieldPuns needed to suppress warnings:
{- HLINT ignore "Unused LANGUAGE pragma" -}

-- |
-- Copyright: © 2020 IOHK
-- License: Apache-2.0
--
-- Working with Shelley transactions.

module Cardano.Wallet.Shelley.Transaction
    ( newTransactionLayer

    -- * Internals
    , TxPayload (..)
    , TxWitnessTag (..)
    , EraConstraints
    , _decodeSealedTx
    , mkDelegationCertificates
    , mkByronWitness
    , mkShelleyWitness
    , mkTx
    , mkUnsignedTx
    , txWitnessTagForKey
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPrv, toXPub )
import Cardano.Address.Script
    ( KeyHash (..)
    , KeyRole (..)
    , Script (..)
    , ScriptHash (..)
    , foldScript
    , toScriptHash
    )
import Cardano.Api
    ( AnyCardanoEra (..)
    , ByronEra
    , CardanoEra (..)
    , InAnyCardanoEra (..)
    , IsShelleyBasedEra (..)
    , NetworkId
    , ShelleyBasedEra (..)
    , ToCBOR
    )
import Cardano.Binary
    ( serialize' )
import Cardano.Crypto.Wallet
    ( XPub )
import Cardano.Ledger.Allegra.Core
    ( inputsTxBodyL )
import Cardano.Ledger.Crypto
    ( DSIGN )
import Cardano.Wallet.Address.Derivation
    ( Depth (..), RewardAccount (..) )
import Cardano.Wallet.Address.Derivation.SharedKey
    ( replaceCosignersWithVerKeys )
import Cardano.Wallet.Address.Derivation.Shelley
    ( toRewardAccountRaw )
import Cardano.Wallet.Address.Encoding
    ( toHDPayloadAddress )
import Cardano.Wallet.Address.Keys.WalletKey
    ( getRawKey )
import Cardano.Wallet.Flavor
    ( KeyFlavorS (..) )
import Cardano.Wallet.Primitive.Passphrase
    ( Passphrase (..) )
import Cardano.Wallet.Primitive.Types
    ( Certificate )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..) )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle (..) )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( AssetId (..), TokenMap )
import Cardano.Wallet.Primitive.Types.Tx
    ( SealedTx (..)
    , Tx (..)
    , cardanoTxIdeallyNoLaterThan
    , sealedTxFromCardano'
    )
import Cardano.Wallet.Primitive.Types.Tx.Constraints
    ( txOutMaxTokenQuantity )
import Cardano.Wallet.Primitive.Types.Tx.TxIn
    ( TxIn (..) )
import Cardano.Wallet.Primitive.Types.Tx.TxOut
    ( TxOut (..) )
import Cardano.Wallet.Read.Primitive.Tx
    ( fromCardanoTx )
import Cardano.Wallet.Shelley.Compatibility
    ( cardanoCertKeysForWitnesses
    , fromCardanoAddress
    , fromCardanoWdrls
    , toCardanoLovelace
    , toCardanoPolicyId
    , toCardanoSimpleScript
    , toCardanoStakeCredential
    , toCardanoTxIn
    , toCardanoTxOut
    , toCardanoValue
    , toStakeKeyDeregCert
    , toStakeKeyRegCert
    , toStakePoolDlgCert
    )
import Cardano.Wallet.Shelley.Compatibility.Ledger
    ( Convert (toLedger) )
import Cardano.Wallet.Transaction
    ( AnyExplicitScript (..)
    , AnyScript (..)
    , DelegationAction (..)
    , ErrMkTransaction (..)
    , ErrMkTransactionOutputTokenQuantityExceedsLimitError (..)
    , PreSelection (..)
    , SelectionOf (..)
    , TokenMapWithScripts
    , TransactionCtx (..)
    , TransactionLayer (..)
    , ValidityIntervalExplicit
    , Withdrawal (..)
    , WitnessCount (..)
    , WitnessCountCtx (..)
    , selectionDelta
    )
import Cardano.Wallet.TxWitnessTag
    ( TxWitnessTag (..) )
import Cardano.Wallet.Util
    ( HasCallStack, internalError )
import Control.Arrow
    ( left, second )
import Control.Lens
    ( over )
import Control.Monad
    ( forM_, guard, when )
import Data.Bifunctor
    ( bimap )
import Data.Function
    ( (&) )
import Data.Functor
    ( ($>) )
import Data.Generics.Internal.VL.Lens
    ( view, (^.) )
import Data.Generics.Labels
    ()
import Data.Map.Strict
    ( Map )
import Data.Maybe
    ( mapMaybe )
import Data.Type.Equality
    ( type (==) )
import Ouroboros.Network.Block
    ( SlotNo )

import qualified Cardano.Address.Style.Shelley as CA
import qualified Cardano.Api as Cardano
import qualified Cardano.Api.Byron as Byron
import qualified Cardano.Api.Shelley as Cardano
import qualified Cardano.Chain.Common as Byron
import qualified Cardano.Crypto as CC
import qualified Cardano.Crypto.DSIGN as DSIGN
import qualified Cardano.Crypto.Hash.Class as Crypto
import qualified Cardano.Crypto.Wallet as Crypto.HD
import qualified Cardano.Ledger.Api as Ledger
import qualified Cardano.Ledger.Keys.Bootstrap as SL
import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap
import qualified Cardano.Wallet.Shelley.Compatibility as Compatibility
import qualified Cardano.Wallet.Write.Tx as Write
import qualified Data.ByteString as BS
import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T


-- | Type encapsulating what we need to know to add things -- payloads,
-- certificates -- to a transaction.
--
-- Designed to allow us to have /one/ @mkTx@ which doesn't care whether we
-- include certificates or not.
data TxPayload era = TxPayload
    { _metadata ::  Maybe Cardano.TxMetadata
      -- ^ User or application-defined metadata to be included in the
      -- transaction.

    , _certificates :: [Cardano.Certificate]
      -- ^ Certificates to be included in the transactions.

    , _extraWitnesses :: Cardano.TxBody era -> [Cardano.KeyWitness era]
      -- ^ Create payload-specific witnesses given the unsigned transaction body.
      --
      -- Caller has the freedom and responsibility to provide the correct
      -- witnesses for what they're trying to do.
    }

type EraConstraints era =
    ( IsShelleyBasedEra era
    , ToCBOR (Ledger.TxBody (Cardano.ShelleyLedgerEra era))
    , DSIGN (Ledger.EraCrypto (Cardano.ShelleyLedgerEra era)) ~ DSIGN.Ed25519DSIGN
    , (era == ByronEra) ~ 'False
    )


constructUnsignedTx
    :: forall era
     . IsShelleyBasedEra era
    => Cardano.NetworkId
    -> (Maybe Cardano.TxMetadata, [Cardano.Certificate])
    -> (Maybe SlotNo, SlotNo)
    -- ^ Slot at which the transaction will optionally start and expire.
    -> Withdrawal
    -> Either PreSelection (SelectionOf TxOut)
    -- ^ Finalized asset selection
    -> Coin
    -- ^ Explicit fee amount
    -> (TokenMap, Map AssetId (Script KeyHash))
    -- ^ Assets to be minted
    -> (TokenMap, Map AssetId (Script KeyHash))
    -- ^ Assets to be burned
    -> Map TxIn (Script KeyHash)
    -- ^ scripts for inputs
    -> Maybe (Script KeyHash)
    -- ^ Delegation script
    -> Maybe (Script KeyHash)
    -- ^ Reference script
    -> ShelleyBasedEra era
    -> Either ErrMkTransaction (Cardano.TxBody era)
constructUnsignedTx
    networkId (md, certs) ttl wdrl
    cs fee toMint toBurn inpScripts stakingScriptM refScriptM era =
        mkUnsignedTx
            era ttl cs md wdrls certs (toCardanoLovelace fee)
            (fst toMint) (fst toBurn) mintingScripts inpScripts
            stakingScriptM refScriptM
  where
    wdrls = mkWithdrawals networkId wdrl
    mintingScripts = Map.union (snd toMint) (snd toBurn)

mkTx
    :: forall k era
     . EraConstraints era
    => KeyFlavorS k
    -> Cardano.NetworkId
    -> TxPayload era
    -> (Maybe SlotNo, SlotNo)
    -- ^ Slot at which the transaction will start and expire.
    -> (XPrv, Passphrase "encryption")
    -- ^ Reward account
    -> (Address -> Maybe (k 'CredFromKeyK XPrv, Passphrase "encryption"))
    -- ^ Key store
    -> Withdrawal
    -- ^ An optional withdrawal
    -> SelectionOf TxOut
    -- ^ Finalized asset selection
    -> Coin
    -- ^ Explicit fee amount
    -> ShelleyBasedEra era
    -> Either ErrMkTransaction (Tx, SealedTx)
mkTx keyF networkId payload ttl (rewardAcnt, pwdAcnt) addrResolver wdrl cs fees era =
    do

    let TxPayload md certs mkExtraWits = payload
    let wdrls = mkWithdrawals networkId wdrl

    unsigned <- mkUnsignedTx era ttl (Right cs) md wdrls certs
        (toCardanoLovelace fees)
        TokenMap.empty TokenMap.empty Map.empty Map.empty Nothing Nothing
    let signed = signTransaction keyF networkId AnyWitnessCountCtx acctResolver
            (const Nothing) (const Nothing) addrResolver inputResolver
            (unsigned, mkExtraWits unsigned)

    let withResolvedInputs (tx, _, _, _, _, _) = tx
            { resolvedInputs = second Just <$> F.toList (view #inputs cs)
            }
    Right ( withResolvedInputs (fromCardanoTx AnyWitnessCountCtx signed)
          , sealedTxFromCardano' signed
          )
  where
    inputResolver :: TxIn -> Maybe Address
    inputResolver i =
        let index = Map.fromList (F.toList $ view #inputs cs)
         in do
            TxOut addr _ <- Map.lookup i index
            pure addr

    acctResolver :: RewardAccount -> Maybe (XPrv, Passphrase "encryption")
    acctResolver acct = do
        let acct' = toRewardAccountRaw $ toXPub rewardAcnt
        guard (acct == acct') $> (rewardAcnt, pwdAcnt)

-- Adds VK witnesses to an already constructed transactions. The function
-- preserves any existing witnesses on the transaction, and resolve inputs
-- dynamically using the provided lookup function.
--
-- If a key for a given input isn't found, the input is skipped.
signTransaction
    :: forall k ktype era
     . EraConstraints era
    => KeyFlavorS k
    -> Cardano.NetworkId
    -- ^ Network identifier (e.g. mainnet, testnet)
    -> WitnessCountCtx
    -> (RewardAccount -> Maybe (XPrv, Passphrase "encryption"))
    -- ^ Stake key store / reward account resolution
    -> (KeyHash -> Maybe (XPrv, Passphrase "encryption"))
    -- ^ Policy key resolution
    -> (KeyHash -> Maybe (XPrv, Passphrase "encryption"))
    -- ^ Staking script key resolution
    -> (Address -> Maybe (k ktype XPrv, Passphrase "encryption"))
    -- ^ Payment key store
    -> (TxIn -> Maybe Address)
    -- ^ Input resolver
    -> (Cardano.TxBody era, [Cardano.KeyWitness era])
    -- ^ The transaction to sign, possibly with already some existing witnesses
    -> Cardano.Tx era
signTransaction
    keyF
    networkId
    witCountCtx
    resolveRewardAcct
    resolvePolicyKey
    resolveStakingKeyInScript
    resolveAddress
    resolveInput
    (body, wits) =
        Cardano.makeSignedTransaction wits' body
 where
    wits' = mconcat
        [ wits
        , mapMaybe mkTxInWitness inputs
        , mapMaybe mkTxInWitness collaterals
        , mapMaybe mkWdrlCertWitness wdrls
        , mapMaybe mkExtraWitness extraKeys
        , mapMaybe mkWdrlCertWitness certs
        , mapMaybe mkPolicyWitness mintBurnScriptsKeyHashes
        , mapMaybe mkStakingScriptWitness stakingScriptsKeyHashes
        ]
      where
        Cardano.TxBody bodyContent = body

        inputs =
            [ Compatibility.fromCardanoTxIn i
            | (i, _) <- Cardano.txIns bodyContent
            ]

        collaterals =
            case Cardano.txInsCollateral bodyContent of
                Cardano.TxInsCollateralNone ->
                    []
                Cardano.TxInsCollateral _ is ->
                    Compatibility.fromCardanoTxIn <$> is

        extraKeys =
            case Cardano.txExtraKeyWits bodyContent of
                Cardano.TxExtraKeyWitnessesNone ->
                    []
                Cardano.TxExtraKeyWitnesses _ xs ->
                    xs
        wdrls =
            [ addr
            | (addr, _) <- fromCardanoWdrls $ Cardano.txWithdrawals bodyContent
            ]

        certs = cardanoCertKeysForWitnesses $ Cardano.txCertificates bodyContent

        mintBurnScriptsKeyHashes =
            let (_, toMint, toBurn, _, _, _) = fromCardanoTx witCountCtx $
                    Cardano.makeSignedTransaction wits body
            in
            -- Note that we use 'nub' here because multiple scripts can share
            -- the same policyXPub. It's sufficient to have one witness for
            -- each.
            L.nub $ getScriptsKeyHashes toMint <> getScriptsKeyHashes toBurn

        stakingScriptsKeyHashes =
            let (_, _, _, _, _, (WitnessCount _ nativeScripts _)) =
                    fromCardanoTx witCountCtx $
                    Cardano.makeSignedTransaction wits body
                isDelegationKeyHash (KeyHash Delegation _) = True
                isDelegationKeyHash (KeyHash _ _) = False
            in
                filter isDelegationKeyHash $
                L.nub $ concatMap retrieveAllKeyHashesE $
                filter isTimelockE nativeScripts

    retrieveAllKeyHashesE (NativeExplicitScript s _) = foldScript (:) [] s
    retrieveAllKeyHashesE _ = []

    isTimelockE (NativeExplicitScript _ _) = True
    isTimelockE _ = False

    retrieveAllKeyHashes (NativeScript s _) = foldScript (:) [] s
    retrieveAllKeyHashes _ = []

    isTimelock (NativeScript _ _) = True
    isTimelock _ = False

    getScriptsKeyHashes :: TokenMapWithScripts -> [KeyHash]
    getScriptsKeyHashes scripts =
        concatMap retrieveAllKeyHashes $
        filter isTimelock $
        Map.elems $ scripts ^. #txScripts

    mkTxInWitness :: TxIn -> Maybe (Cardano.KeyWitness era)
    mkTxInWitness i = do
        addr <- resolveInput i
        (k, pwd) <- resolveAddress addr
        let  pk = (getRawKey keyF k, pwd)
        pure $ case txWitnessTagForKey keyF of
            TxWitnessShelleyUTxO -> mkShelleyWitness body pk
            TxWitnessByronUTxO ->
                mkByronWitness body networkId addr pk

    mkWdrlCertWitness :: RewardAccount -> Maybe (Cardano.KeyWitness era)
    mkWdrlCertWitness a =
        mkShelleyWitness body <$> resolveRewardAcct a

    mkPolicyWitness :: KeyHash -> Maybe (Cardano.KeyWitness era)
    mkPolicyWitness a =
        mkShelleyWitness body <$> resolvePolicyKey a

    mkStakingScriptWitness :: KeyHash -> Maybe (Cardano.KeyWitness era)
    mkStakingScriptWitness a =
        mkShelleyWitness body <$> resolveStakingKeyInScript a

    mkExtraWitness :: Cardano.Hash Cardano.PaymentKey -> Maybe (Cardano.KeyWitness era)
    mkExtraWitness vkh = do
        -- NOTE: We cannot resolve key hashes directly, so create a one-time
        -- temporary address with that key hash which is fine to lookup via the
        -- address lookup provided above. It works _fine_ because the discovery
        -- of addresses is done properly based on the address constituents (i.e.
        -- the key hash) and not the overall address itself.
        let addr = Cardano.makeShelleyAddress networkId
                (Cardano.PaymentCredentialByKey vkh)
                Cardano.NoStakeAddress
        (k, pwd) <- resolveAddress (fromCardanoAddress addr)
        pure $ mkShelleyWitness body (getRawKey keyF k, pwd)

newTransactionLayer
    :: KeyFlavorS k
    -> NetworkId
    -> TransactionLayer k ktype SealedTx
newTransactionLayer keyF networkId = TransactionLayer
    { mkTransaction = \era stakeCreds keystore _pp ctx selection -> do
        let ttl   = txValidityInterval ctx
        let wdrl = view #txWithdrawal ctx
        let delta = selectionDelta selection
        case view #txDelegationAction ctx of
            Nothing -> withShelleyBasedEra era $ do
                let payload = TxPayload (view #txMetadata ctx) mempty mempty
                mkTx keyF networkId payload ttl stakeCreds keystore wdrl
                    selection delta

            Just action -> withShelleyBasedEra era $ do
                let stakeXPub = toXPub $ fst stakeCreds
                let certs = mkDelegationCertificates action (Left stakeXPub)
                let payload = TxPayload (view #txMetadata ctx) certs (const [])
                mkTx keyF networkId payload ttl stakeCreds keystore wdrl
                    selection delta

    , addVkWitnesses =
        \era witCountCtx stakeCreds policyCreds scriptStakingCredM addressResolver
        inputResolver sealedTx -> do
            let acctMap :: Map RewardAccount (XPrv, Passphrase "encryption")
                acctMap = Map.fromList $ map
                    (\(xprv, pwd) -> (toRewardAccountRaw $ toXPub xprv,(xprv, pwd)))
                    stakeCreds

            let acctResolver
                    :: RewardAccount -> Maybe (XPrv, Passphrase "encryption")
                acctResolver acct = Map.lookup acct acctMap

            let policyResolver
                    :: KeyHash -> Maybe (XPrv, Passphrase "encryption")
                policyResolver keyhash = do
                    (keyhash', xprv, encP) <- policyCreds
                    guard (keyhash == keyhash') $> (xprv, encP)
            let stakingScriptResolver
                    :: KeyHash -> Maybe (XPrv, Passphrase "encryption")
                stakingScriptResolver keyhash = case scriptStakingCredM of
                    Just scriptStakingCred -> do
                        let (keyhash', xprv, encP) = scriptStakingCred
                        guard (keyhash == keyhash') $> (xprv, encP)
                    Nothing -> Nothing
            case cardanoTxIdeallyNoLaterThan era sealedTx of
                InAnyCardanoEra ByronEra _ ->
                    sealedTx
                InAnyCardanoEra ShelleyEra (Cardano.Tx body wits) ->
                    signTransaction keyF networkId witCountCtx acctResolver (const Nothing)
                    (const Nothing) addressResolver inputResolver (body, wits)
                    & sealedTxFromCardano'
                InAnyCardanoEra AllegraEra (Cardano.Tx body wits) ->
                    signTransaction keyF networkId witCountCtx acctResolver (const Nothing)
                    (const Nothing) addressResolver inputResolver (body, wits)
                    & sealedTxFromCardano'
                InAnyCardanoEra MaryEra (Cardano.Tx body wits) ->
                    signTransaction keyF networkId witCountCtx acctResolver policyResolver
                    stakingScriptResolver addressResolver inputResolver (body, wits)
                    & sealedTxFromCardano'
                InAnyCardanoEra AlonzoEra (Cardano.Tx body wits) ->
                    signTransaction keyF networkId witCountCtx acctResolver policyResolver
                    stakingScriptResolver addressResolver inputResolver (body, wits)
                    & sealedTxFromCardano'
                InAnyCardanoEra BabbageEra (Cardano.Tx body wits) ->
                    signTransaction keyF networkId witCountCtx acctResolver policyResolver
                    stakingScriptResolver addressResolver inputResolver (body, wits)
                    & sealedTxFromCardano'
                InAnyCardanoEra ConwayEra (Cardano.Tx body wits) ->
                    signTransaction keyF networkId witCountCtx acctResolver policyResolver
                    stakingScriptResolver addressResolver inputResolver (body, wits)
                    & sealedTxFromCardano'

    , mkUnsignedTransaction = \stakeCred ctx selection -> do
        let ttl   = txValidityInterval ctx
        let wdrl  = view #txWithdrawal ctx
        let delta = case selection of
                Right selOf -> selectionDelta selOf
                Left _preSel -> Coin 0
        let assetsToBeMinted = view #txAssetsToMint ctx
        let assetsToBeBurned = view #txAssetsToBurn ctx
        let inpsScripts = view #txNativeScriptInputs ctx
        let refScriptM = view #txReferenceScript ctx
        let stakingScriptM =
                flip (replaceCosignersWithVerKeys CA.Stake) minBound <$>
                view #txStakingCredentialScriptTemplate ctx
        case view #txDelegationAction ctx of
            Nothing -> do
                let md = view #txMetadata ctx
                let ourRewardAcctM = FromScriptHash . unScriptHash . toScriptHash <$> stakingScriptM
                case wdrl of
                    WithdrawalSelf rewardAcct _ _ ->
                        if ourRewardAcctM == Just rewardAcct then
                            constructUnsignedTx networkId (md, []) ttl wdrl
                            selection delta assetsToBeMinted assetsToBeBurned inpsScripts
                            stakingScriptM refScriptM
                            (Write.shelleyBasedEraFromRecentEra Write.recentEra)
                        else
                            constructUnsignedTx networkId (md, []) ttl wdrl
                            selection delta assetsToBeMinted assetsToBeBurned inpsScripts
                            Nothing refScriptM
                            (Write.shelleyBasedEraFromRecentEra Write.recentEra)
                    _ ->
                        constructUnsignedTx networkId (md, []) ttl wdrl
                        selection delta assetsToBeMinted assetsToBeBurned inpsScripts
                        Nothing refScriptM
                        (Write.shelleyBasedEraFromRecentEra Write.recentEra)
            Just action -> do
                let certs = case stakeCred of
                        Left xpub ->
                            mkDelegationCertificates action (Left xpub)
                        Right (Just script) ->
                            mkDelegationCertificates action (Right script)
                        Right Nothing ->
                            error $ "stakeCred in mkUnsignedTransaction must be either "
                            <> "xpub or script when there is delegation action"
                let payload = (view #txMetadata ctx, certs)
                constructUnsignedTx networkId payload ttl wdrl
                    selection delta assetsToBeMinted assetsToBeBurned inpsScripts
                    stakingScriptM refScriptM
                    (Write.shelleyBasedEraFromRecentEra Write.recentEra)

    , decodeTx = _decodeSealedTx

    , transactionWitnessTag = txWitnessTagForKey keyF
    }

_decodeSealedTx
    :: AnyCardanoEra
    -> WitnessCountCtx
    -> SealedTx ->
        ( Tx
        , TokenMapWithScripts
        , TokenMapWithScripts
        , [Certificate]
        , Maybe ValidityIntervalExplicit
        , WitnessCount
        )
_decodeSealedTx preferredLatestEra witCtx (cardanoTxIdeallyNoLaterThan preferredLatestEra -> Cardano.InAnyCardanoEra _ tx) =
    fromCardanoTx witCtx tx

mkDelegationCertificates
    :: DelegationAction
        -- Pool Id to which we're planning to delegate
    -> Either XPub (Script KeyHash)
        --Staking credential
    -> [Cardano.Certificate]
mkDelegationCertificates da cred =
    case da of
       Join poolId ->
               [ toStakePoolDlgCert cred poolId ]
       JoinRegisteringKey poolId ->
            [ toStakeKeyRegCert cred
            , toStakePoolDlgCert cred poolId
            ]
       Quit -> [toStakeKeyDeregCert cred]

withShelleyBasedEra
    :: forall a
     . AnyCardanoEra
    -> ( forall era. EraConstraints era
         => ShelleyBasedEra era -> Either ErrMkTransaction a
       )
    -> Either ErrMkTransaction a
withShelleyBasedEra era fn = case era of
    AnyCardanoEra ByronEra    -> Left $ ErrMkTransactionInvalidEra era
    AnyCardanoEra ShelleyEra  -> fn ShelleyBasedEraShelley
    AnyCardanoEra AllegraEra  -> fn ShelleyBasedEraAllegra
    AnyCardanoEra MaryEra     -> fn ShelleyBasedEraMary
    AnyCardanoEra AlonzoEra   -> fn ShelleyBasedEraAlonzo
    AnyCardanoEra BabbageEra  -> fn ShelleyBasedEraBabbage
    AnyCardanoEra ConwayEra   -> fn ShelleyBasedEraConway

-- FIXME: Make this a Allegra or Shelley transaction depending on the era we're
-- in. However, quoting Duncan:
--
--    "Yes, you can submit Shelley format transactions in the Allegra era.The
--    proper way to do that is marking it as a Shelley tx using GenTxShelley.
--    The improper way is to submit it as a GenTxAllegra. The latter should
--    still work since the binary formats are upwards compatible."
--
-- Which suggests that we may get away with Shelley-only transactions for now?
mkUnsignedTx
    :: forall era. Cardano.IsCardanoEra era
    => ShelleyBasedEra era
    -> (Maybe SlotNo, SlotNo)
    -> Either PreSelection (SelectionOf TxOut)
    -> Maybe Cardano.TxMetadata
    -> [(Cardano.StakeAddress, Cardano.Lovelace)]
    -> [Cardano.Certificate]
    -> Cardano.Lovelace
    -> TokenMap
    -> TokenMap
    -> Map AssetId (Script KeyHash)
    -> Map TxIn (Script KeyHash)
    -> Maybe (Script KeyHash)
    -> Maybe (Script KeyHash)
    -> Either ErrMkTransaction (Cardano.TxBody era)
mkUnsignedTx
    era
    ttl
    cs
    md
    wdrls
    certs
    fees
    mintData
    burnData
    mintingScripts
    inpsScripts
    stakingScriptM
    refScriptM = extractValidatedOutputs cs >>= \outs ->
    left toErrMkTx $ fmap removeDummyInput $ Cardano.createAndValidateTransactionBody
    Cardano.TxBodyContent
    { Cardano.txIns = inputWits

    , txInsReference = Cardano.TxInsReferenceNone

    , Cardano.txOuts = map (toCardanoTxOut era) outs
    , Cardano.txWithdrawals = case stakingScriptM of
        Nothing ->
            let ctx = Cardano.BuildTxWith
                    $ Cardano.KeyWitness Cardano.KeyWitnessForStakeAddr
            in
                Cardano.TxWithdrawals wdrlsSupported
                (map (\(key, coin) -> (key, coin, ctx)) wdrls)
        Just stakingScript ->
            let
                buildVal = Cardano.ScriptWitness Cardano.ScriptWitnessForStakeAddr
                    (toScriptWitness stakingScript)
                ctx = Cardano.BuildTxWith buildVal
            in
                Cardano.TxWithdrawals wdrlsSupported
                (map (\(key, coin) -> (key, coin, ctx)) wdrls)

    -- @mkUnsignedTx@ is never used with Plutus scripts, and so we never have to
    -- care about collateral or PParams (for script integrity hash) here.
    --
    -- If constructTransaction because of multisig in the future ever needs to
    -- run/redeem Plutus scripts, we should re-use balanceTransaction and remove
    -- this @mkUnsignedTx@. (We should do this regardless.)
    , txInsCollateral = Cardano.TxInsCollateralNone
    , txTotalCollateral = Cardano.TxTotalCollateralNone
    , txReturnCollateral = Cardano.TxReturnCollateralNone
    , txProtocolParams = Cardano.BuildTxWith Nothing
    , txScriptValidity = Cardano.TxScriptValidityNone
    , txExtraKeyWits = Cardano.TxExtraKeyWitnessesNone

    , Cardano.txCertificates = case stakingScriptM of
        Nothing ->
            let
                witMap = Map.empty
                ctx = Cardano.BuildTxWith witMap
            in
                Cardano.TxCertificates certSupported certs ctx
        Just stakingScript ->
            let
                buildKey =
                    Cardano.StakeCredentialByScript
                    . Cardano.hashScript
                    . Cardano.SimpleScript
                    $ toCardanoSimpleScript stakingScript
                buildVal = Cardano.ScriptWitness Cardano.ScriptWitnessForStakeAddr
                    (toScriptWitness stakingScript)
                witMap = Map.fromList [(buildKey, buildVal)]
                ctx = Cardano.BuildTxWith witMap
            in
                Cardano.TxCertificates certSupported certs ctx

    , Cardano.txFee = explicitFees era fees

    , Cardano.txValidityRange =
        let toLowerBound from = case txValidityLowerBoundSupported of
                Just lowerBoundSupported ->
                    Cardano.TxValidityLowerBound lowerBoundSupported from
                Nothing ->
                    Cardano.TxValidityNoLowerBound
        in
        bimap
            (maybe Cardano.TxValidityNoLowerBound toLowerBound)
            (Cardano.TxValidityUpperBound txValidityUpperBoundSupported)
            ttl

    , Cardano.txMetadata =
        maybe
            Cardano.TxMetadataNone
            (Cardano.TxMetadataInEra metadataSupported)
            md

    , Cardano.txAuxScripts =
        Cardano.TxAuxScriptsNone

    , Cardano.txUpdateProposal =
        Cardano.TxUpdateProposalNone

    , Cardano.txMintValue =
        case txMintingSupported of
            Nothing -> Cardano.TxMintNone
            Just mintedEra ->
                let mintValue = toCardanoValue (TokenBundle (Coin 0) mintData)
                    burnValue =
                        Cardano.negateValue $
                        toCardanoValue (TokenBundle (Coin 0) burnData)
                    witMap =
                        Map.map toScriptWitness $
                        Map.mapKeys (toCardanoPolicyId . TokenMap.tokenPolicyId)
                        mintingScripts
                    ctx = Cardano.BuildTxWith witMap
                in Cardano.TxMintValue mintedEra (mintValue <> burnValue) ctx
    }
  where
    toErrMkTx :: Cardano.TxBodyError -> ErrMkTransaction
    toErrMkTx = ErrMkTransactionTxBodyError . T.pack . Cardano.displayError

    -- Extra validation for HTTP API backward compatibility:
    --
    -- Previously validation of user-provided payment outputs was handled by
    -- coin-selection, as coin-selection was run before assembling the
    -- transaction as a real 'Cardano.Tx'. Now, with 'balanceTx' this is no
    -- longer the case, meaning that validation from cardano-api would take
    -- precedence without this extra preceeding validation.
    --
    -- We may concider to, after ADP-2268:
    -- - Embrace cardano-api errors and remove this function
    -- - Extract and re-use validation from coin-selection, rather than
    --     duplicating.
    -- - Remove validation from coin-selection itself
    extractValidatedOutputs
        :: Either PreSelection (SelectionOf TxOut)
        -> Either ErrMkTransaction [TxOut]
    extractValidatedOutputs sel =
        mapM validateOut $ case sel of
            Right selOf -> view #outputs selOf ++ F.toList (view #change selOf)
            Left preSel -> view #outputs preSel
      where
        validateOut :: TxOut -> Either ErrMkTransaction TxOut
        validateOut out = do
            verifyOutputTokenQuantities out
            return out
          where
            verifyOutputTokenQuantities :: TxOut -> Either ErrMkTransaction ()
            verifyOutputTokenQuantities (TxOut addr (TokenBundle _ tokens)) = do
                forM_ (TokenMap.toFlatList tokens) $ \(asset, quantity) -> do
                    when (quantity > txOutMaxTokenQuantity) $
                        Left (mkErr asset quantity)
              where
                mkErr aid q
                    = ErrMkTransactionOutputTokenQuantityExceedsLimit
                    $ ErrMkTransactionOutputTokenQuantityExceedsLimitError
                        { address = addr
                        , asset = aid
                        , quantity = q
                        , quantityMaxBound = txOutMaxTokenQuantity
                        }

    metadataSupported :: Cardano.TxMetadataSupportedInEra era
    metadataSupported = case era of
        ShelleyBasedEraShelley -> Cardano.TxMetadataInShelleyEra
        ShelleyBasedEraAllegra -> Cardano.TxMetadataInAllegraEra
        ShelleyBasedEraMary -> Cardano.TxMetadataInMaryEra
        ShelleyBasedEraAlonzo -> Cardano.TxMetadataInAlonzoEra
        ShelleyBasedEraBabbage -> Cardano.TxMetadataInBabbageEra
        ShelleyBasedEraConway -> Cardano.TxMetadataInConwayEra

    certSupported :: Cardano.CertificatesSupportedInEra era
    certSupported = case era of
        ShelleyBasedEraShelley -> Cardano.CertificatesInShelleyEra
        ShelleyBasedEraAllegra -> Cardano.CertificatesInAllegraEra
        ShelleyBasedEraMary    -> Cardano.CertificatesInMaryEra
        ShelleyBasedEraAlonzo -> Cardano.CertificatesInAlonzoEra
        ShelleyBasedEraBabbage -> Cardano.CertificatesInBabbageEra
        ShelleyBasedEraConway -> Cardano.CertificatesInConwayEra

    wdrlsSupported :: Cardano.WithdrawalsSupportedInEra era
    wdrlsSupported = case era of
        ShelleyBasedEraShelley -> Cardano.WithdrawalsInShelleyEra
        ShelleyBasedEraAllegra -> Cardano.WithdrawalsInAllegraEra
        ShelleyBasedEraMary    -> Cardano.WithdrawalsInMaryEra
        ShelleyBasedEraAlonzo -> Cardano.WithdrawalsInAlonzoEra
        ShelleyBasedEraBabbage -> Cardano.WithdrawalsInBabbageEra
        ShelleyBasedEraConway -> Cardano.WithdrawalsInConwayEra

    txValidityUpperBoundSupported :: Cardano.ValidityUpperBoundSupportedInEra era
    txValidityUpperBoundSupported = case era of
        ShelleyBasedEraShelley -> Cardano.ValidityUpperBoundInShelleyEra
        ShelleyBasedEraAllegra -> Cardano.ValidityUpperBoundInAllegraEra
        ShelleyBasedEraMary -> Cardano.ValidityUpperBoundInMaryEra
        ShelleyBasedEraAlonzo -> Cardano.ValidityUpperBoundInAlonzoEra
        ShelleyBasedEraBabbage -> Cardano.ValidityUpperBoundInBabbageEra
        ShelleyBasedEraConway -> Cardano.ValidityUpperBoundInConwayEra

    txValidityLowerBoundSupported
        :: Maybe (Cardano.ValidityLowerBoundSupportedInEra era)
    txValidityLowerBoundSupported = case era of
        ShelleyBasedEraShelley -> Nothing
        ShelleyBasedEraAllegra -> Just Cardano.ValidityLowerBoundInAllegraEra
        ShelleyBasedEraMary -> Just Cardano.ValidityLowerBoundInMaryEra
        ShelleyBasedEraAlonzo -> Just Cardano.ValidityLowerBoundInAlonzoEra
        ShelleyBasedEraBabbage -> Just Cardano.ValidityLowerBoundInBabbageEra
        ShelleyBasedEraConway -> Just Cardano.ValidityLowerBoundInConwayEra

    txMintingSupported :: Maybe (Cardano.MultiAssetSupportedInEra era)
    txMintingSupported = case era of
        ShelleyBasedEraShelley -> Nothing
        ShelleyBasedEraAllegra -> Nothing
        ShelleyBasedEraMary -> Just Cardano.MultiAssetInMaryEra
        ShelleyBasedEraAlonzo -> Just Cardano.MultiAssetInAlonzoEra
        ShelleyBasedEraBabbage -> Just Cardano.MultiAssetInBabbageEra
        ShelleyBasedEraConway -> Just Cardano.MultiAssetInConwayEra

    scriptWitsSupported
        :: Cardano.ScriptLanguageInEra Cardano.SimpleScript' era
    scriptWitsSupported = case era of
        ShelleyBasedEraShelley -> internalError
            "scriptWitsSupported: we should be at least in Mary"
        ShelleyBasedEraAllegra -> internalError
            "scriptWitsSupported: we should be at least in Mary"
        ShelleyBasedEraMary -> Cardano.SimpleScriptInMary
        ShelleyBasedEraAlonzo -> Cardano.SimpleScriptInAlonzo
        ShelleyBasedEraBabbage -> Cardano.SimpleScriptInBabbage
        ShelleyBasedEraConway -> Cardano.SimpleScriptInConway

    toScriptWitness :: Script KeyHash -> Cardano.ScriptWitness witctx era
    toScriptWitness script =
        Cardano.SimpleScriptWitness
        scriptWitsSupported
        (Cardano.SScript $ toCardanoSimpleScript script)

    constructInpScriptWit inp =
        let script = case Map.lookup inp inpsScripts of
                Nothing -> error "constructInpScriptWit: each input should have script in multisig"
                Just script' -> script'
            scriptWit = toScriptWitness script
        in ( toCardanoTxIn inp
           , Cardano.BuildTxWith
             (Cardano.ScriptWitness Cardano.ScriptWitnessForSpending scriptWit)
           )
    inputWits = case cs of
        Right selOf ->
            if inpsScripts == Map.empty then
                (, buildTxCommand)
                    . toCardanoTxIn
                    . fst <$> F.toList (view #inputs selOf)
            else
                constructInpScriptWit . fst <$> F.toList (view #inputs selOf)
        Left _preSel ->
            [(toCardanoTxIn dummyInput, buildTxCommand)]
      where
        buildTxCommand
            = Cardano.BuildTxWith
            $ Cardano.KeyWitness Cardano.KeyWitnessForSpending

-- TODO: ADP-2257
-- cardano-node does not allow to construct tx without inputs at this moment.
-- this should change and this hack should be removed
dummyInput :: TxIn
dummyInput = TxIn (Hash $ BS.replicate 32 0) 999

removeDummyInput :: HasCallStack => Cardano.TxBody era -> Cardano.TxBody era
removeDummyInput = \case
    Byron.ByronTxBody{} -> bailOut
    Cardano.ShelleyTxBody era body scripts scriptData aux val -> case era of
        ShelleyBasedEraShelley -> bailOut
        ShelleyBasedEraAllegra -> bailOut
        ShelleyBasedEraMary -> bailOut
        ShelleyBasedEraAlonzo ->
            Cardano.ShelleyTxBody
                era
                (over inputsTxBodyL (Set.delete (toLedger dummyInput)) body)
                scripts
                scriptData
                aux
                val
        ShelleyBasedEraBabbage ->
            Cardano.ShelleyTxBody
                era
                (over inputsTxBodyL (Set.delete (toLedger dummyInput)) body)
                scripts
                scriptData
                aux
                val
        ShelleyBasedEraConway ->
            Cardano.ShelleyTxBody
                era
                (over inputsTxBodyL (Set.delete (toLedger dummyInput)) body)
                scripts
                scriptData
                aux
                val
  where
    bailOut =
        error
            "removing dummy inputs is only supported \
            \for the Alonzo or Babbage era"

mkWithdrawals
    :: NetworkId
    -> Withdrawal
    -> [(Cardano.StakeAddress, Cardano.Lovelace)]
mkWithdrawals networkId wdrl = case wdrl of
    NoWithdrawal -> []
    WithdrawalExternal acc _ amt _ ->
        [(stakeAddress acc, toCardanoLovelace amt)]
    WithdrawalSelf acc _ amt ->
        [(stakeAddress acc, toCardanoLovelace amt)]
  where
    stakeAddress = Cardano.makeStakeAddress networkId . toCardanoStakeCredential

mkShelleyWitness
    :: IsShelleyBasedEra era
    => Cardano.TxBody era
    -> (XPrv, Passphrase "encryption")
    -> Cardano.KeyWitness era
mkShelleyWitness body key =
    Cardano.makeShelleyKeyWitness body (unencrypt key)
  where
    unencrypt (xprv, pwd) = Cardano.WitnessPaymentExtendedKey
        $ Cardano.PaymentExtendedSigningKey
        $ Crypto.HD.xPrvChangePass pwd BS.empty xprv

mkByronWitness
    :: forall era. EraConstraints era
    => Cardano.TxBody era
    -> Cardano.NetworkId
    -> Address
    -> (XPrv, Passphrase "encryption")
    -> Cardano.KeyWitness era
mkByronWitness
    (Cardano.ShelleyTxBody era body _scripts _scriptData _auxData _scriptValidity)
    nw
    addr
    encryptedKey =
    Cardano.ShelleyBootstrapWitness era $
        SL.makeBootstrapWitness txHash (unencrypt encryptedKey) addrAttr
  where
    txHash = Crypto.castHash $ Crypto.hashWith serialize' body

    unencrypt (xprv, pwd) = CC.SigningKey
        $ Crypto.HD.xPrvChangePass pwd BS.empty xprv

    addrAttr = Byron.mkAttributes $ Byron.AddrAttributes
        (toHDPayloadAddress addr)
        (Byron.toByronNetworkMagic nw)

explicitFees :: ShelleyBasedEra era -> Cardano.Lovelace -> Cardano.TxFee era
explicitFees era = case era of
    ShelleyBasedEraShelley ->
        Cardano.TxFeeExplicit Cardano.TxFeesExplicitInShelleyEra
    ShelleyBasedEraAllegra ->
        Cardano.TxFeeExplicit Cardano.TxFeesExplicitInAllegraEra
    ShelleyBasedEraMary ->
        Cardano.TxFeeExplicit Cardano.TxFeesExplicitInMaryEra
    ShelleyBasedEraAlonzo ->
        Cardano.TxFeeExplicit Cardano.TxFeesExplicitInAlonzoEra
    ShelleyBasedEraBabbage ->
        Cardano.TxFeeExplicit Cardano.TxFeesExplicitInBabbageEra
    ShelleyBasedEraConway ->
        Cardano.TxFeeExplicit Cardano.TxFeesExplicitInConwayEra

-- NOTE: Should probably not exist.  We could consider replacing it with
-- `UTxOAssumptions`, which has the benefit of containing the script template we
-- often need in the case of shared wallets. `UTxOAssumptions` is difficult to
-- construct, but we need to do so anyway as part of constructing txs. We could
-- ensure 'checkRewardIsWorthTxCost' can reuse that `UTxOAssumptions`. A hickup
-- regarding the name however would be that 'checkRewardIsWorthTxCost' cares
-- about assumptions about the reward account credentials, not about the utxo
-- (credentials).
txWitnessTagForKey :: KeyFlavorS a -> TxWitnessTag
txWitnessTagForKey = \case
    ByronKeyS -> TxWitnessByronUTxO
    IcarusKeyS -> TxWitnessByronUTxO
    ShelleyKeyS -> TxWitnessShelleyUTxO
    SharedKeyS -> TxWitnessShelleyUTxO
