{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyCase #-}
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

-- |
-- Copyright: © 2020 IOHK
-- License: Apache-2.0
--
-- Working with Shelley transactions.

module Cardano.Wallet.Shelley.Transaction
    ( newTransactionLayer

    -- * Updating SealedTx
    , TxUpdate (..)
    , noTxUpdate
    , updateTx
    , TxFeeUpdate (..)

    -- * For balancing (To be moved)
    , estimateKeyWitnessCount
    , evaluateMinimumFee
    , estimateSignedTxSize
    , KeyWitnessCount (..)
    , distributeSurplus
    , distributeSurplusDelta
    , sizeOfCoin
    , maximumCostOfIncreasingCoin
    , costOfIncreasingCoin
    , assignScriptRedeemers

    -- * Internals
    , TxPayload (..)
    , TxSkeleton (..)
    , TxWitnessTag (..)
    , TxWitnessTagFor (..)
    , EraConstraints
    , _decodeSealedTx
    , mkDelegationCertificates
    , getFeePerByteFromWalletPParams
    , _txRewardWithdrawalCost
    , estimateTxCost
    , estimateTxSize
    , mkByronWitness
    , mkShelleyWitness
    , mkTx
    , mkTxSkeleton
    , mkUnsignedTx
    , txConstraints
    , sizeOf_BootstrapWitnesses
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPrv, toXPub )
import Cardano.Address.Script
    ( Cosigner
    , KeyHash (..)
    , KeyRole (..)
    , Script (..)
    , ScriptHash (..)
    , ScriptTemplate (..)
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
    ( inputsTxBodyL, ppMinFeeAL )
import Cardano.Ledger.Api
    ( bodyTxL
    , collateralInputsTxBodyL
    , feeTxBodyL
    , outputsTxBodyL
    , scriptIntegrityHashTxBodyL
    )
import Cardano.Ledger.Babbage.TxBody
    ( outputsBabbageTxBodyL )
import Cardano.Ledger.Crypto
    ( DSIGN )
import Cardano.Ledger.Shelley.API
    ( StrictMaybe (..) )
import Cardano.Slotting.EpochInfo
    ( EpochInfo, hoistEpochInfo )
import Cardano.Tx.Balance.Internal.CoinSelection
    ( SelectionOf (..)
    , SelectionOutputTokenQuantityExceedsLimitError (..)
    , SelectionSkeleton (..)
    , selectionDelta
    )
import Cardano.Wallet.Address.Derivation
    ( Depth (..), RewardAccount (..) )
import Cardano.Wallet.Address.Derivation.SharedKey
    ( replaceCosignersWithVerKeys )
import Cardano.Wallet.Address.Derivation.Shelley
    ( toRewardAccountRaw )
import Cardano.Wallet.Address.Discovery.Shared
    ( estimateMaxWitnessRequiredPerInput )
import Cardano.Wallet.Address.Keys.WalletKey
    ( getRawKey )
import Cardano.Wallet.Flavor
    ( KeyFlavorS )
import Cardano.Wallet.Primitive.Passphrase
    ( Passphrase (..) )
import Cardano.Wallet.Primitive.Types
    ( Certificate
    , FeePolicy (..)
    , LinearFunction (..)
    , ProtocolParameters (..)
    , TxParameters (..)
    )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..) )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Cardano.Wallet.Primitive.Types.Redeemer
    ( Redeemer, redeemerData )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle (..) )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( AssetId (..), TokenMap )
import Cardano.Wallet.Primitive.Types.TokenPolicy
    ( TokenName (..) )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity (..) )
import Cardano.Wallet.Primitive.Types.Tx
    ( SealedTx (..)
    , Tx (..)
    , cardanoTxIdeallyNoLaterThan
    , sealedTxFromCardano'
    , sealedTxFromCardanoBody
    )
import Cardano.Wallet.Primitive.Types.Tx.Constraints
    ( TxConstraints (..), TxSize (..), txOutMaxTokenQuantity, txSizeDistance )
import Cardano.Wallet.Primitive.Types.Tx.TxIn
    ( TxIn (..) )
import Cardano.Wallet.Primitive.Types.Tx.TxOut
    ( TxOut (..) )
import Cardano.Wallet.Read.Primitive.Tx
    ( fromCardanoTx )
import Cardano.Wallet.Shelley.Compatibility
    ( cardanoCertKeysForWitnesses
    , fromCardanoAddress
    , fromCardanoLovelace
    , fromCardanoWdrls
    , toCardanoLovelace
    , toCardanoPolicyId
    , toCardanoSimpleScript
    , toCardanoStakeCredential
    , toCardanoTxIn
    , toCardanoTxOut
    , toCardanoValue
    , toHDPayloadAddress
    , toScriptPurpose
    , toStakeKeyDeregCert
    , toStakeKeyRegCert
    , toStakePoolDlgCert
    )
import Cardano.Wallet.Shelley.Compatibility.Ledger
    ( toBabbageTxOut
    , toConwayTxOut
    , toLedger
    , toWallet
    , toWalletCoin
    , toWalletScript
    )
import Cardano.Wallet.Shelley.MinimumUTxO
    ( computeMinimumCoinForUTxO, isBelowMinimumCoinForUTxO )
import Cardano.Wallet.Transaction
    ( AnyExplicitScript (..)
    , AnyScript (..)
    , DelegationAction (..)
    , ErrAssignRedeemers (..)
    , ErrMkTransaction (..)
    , ErrMoreSurplusNeeded (ErrMoreSurplusNeeded)
    , ErrUpdateSealedTx (..)
    , PreSelection (..)
    , TokenMapWithScripts
    , TransactionCtx (..)
    , TransactionLayer (..)
    , TxFeeAndChange (..)
    , ValidityIntervalExplicit
    , Withdrawal (..)
    , WitnessCount (..)
    , WitnessCountCtx (..)
    , mapTxFeeAndChange
    )
import Cardano.Wallet.TxWitnessTag
    ( TxWitnessTag (..), TxWitnessTagFor (..) )
import Cardano.Wallet.Util
    ( HasCallStack, internalError, modifyM )
import Cardano.Wallet.Write.Tx
    ( FeePerByte (..)
    , IsRecentEra (recentEra)
    , KeyWitnessCount (..)
    , RecentEra (..)
    , fromCardanoUTxO
    )
import Cardano.Wallet.Write.Tx.TimeTranslation
    ( TimeTranslation, epochInfo, systemStartTime )
import Codec.Serialise
    ( deserialiseOrFail )
import Control.Arrow
    ( left, second )
import Control.Lens
    ( over, (.~) )
import Control.Monad
    ( forM, forM_, guard, when )
import Control.Monad.Trans.Class
    ( lift )
import Control.Monad.Trans.State.Strict
    ( StateT (..), execStateT, get, modify' )
import Data.Bifunctor
    ( bimap )
import Data.Function
    ( (&) )
import Data.Functor
    ( ($>), (<&>) )
import Data.Generics.Internal.VL.Lens
    ( view, (^.) )
import Data.Generics.Labels
    ()
import Data.IntCast
    ( intCast, intCastMaybe )
import Data.Map.Strict
    ( Map, (!) )
import Data.Maybe
    ( fromMaybe, mapMaybe )
import Data.Quantity
    ( Quantity (..) )
import Data.Set
    ( Set )
import Data.Type.Equality
    ( type (==) )
import Data.Word
    ( Word64, Word8 )
import GHC.Generics
    ( Generic )
import Numeric.Natural
    ( Natural )
import Ouroboros.Consensus.Cardano.Block
    ( StandardConway )
import Ouroboros.Consensus.Shelley.Eras
    ( StandardBabbage )
import Ouroboros.Network.Block
    ( SlotNo )

import qualified Cardano.Address.Script as CA
import qualified Cardano.Address.Style.Shelley as CA
import qualified Cardano.Api as Cardano
import qualified Cardano.Api.Byron as Byron
import qualified Cardano.Api.Shelley as Cardano
import qualified Cardano.Chain.Common as Byron
import qualified Cardano.Crypto as CC
import qualified Cardano.Crypto.DSIGN as DSIGN
import qualified Cardano.Crypto.Hash.Class as Crypto
import qualified Cardano.Crypto.Wallet as Crypto.HD
import qualified Cardano.Ledger.Alonzo.PlutusScriptApi as Alonzo
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import qualified Cardano.Ledger.Alonzo.Scripts.Data as Alonzo
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import qualified Cardano.Ledger.Alonzo.TxWits as Alonzo
import qualified Cardano.Ledger.Api as Ledger
import qualified Cardano.Ledger.Coin as Ledger
import qualified Cardano.Ledger.Keys.Bootstrap as SL
import qualified Cardano.Wallet.Primitive.Types.Coin as Coin
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap
import qualified Cardano.Wallet.Primitive.Types.Tx.TxOut as TxOut
import qualified Cardano.Wallet.Shelley.Compatibility as Compatibility
import qualified Cardano.Wallet.Shelley.Compatibility.Ledger as Ledger
import qualified Cardano.Wallet.Write.Tx as Write
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Write as CBOR
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.Map as Map
import qualified Data.Map.Merge.Strict as Map
import qualified Data.Sequence.Strict as StrictSeq
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
    -> ShelleyBasedEra era
    -> Either ErrMkTransaction (Cardano.TxBody era)
constructUnsignedTx
    networkId (md, certs) ttl wdrl
    cs fee toMint toBurn inpScripts stakingScriptM era =
        mkUnsignedTx
            era ttl cs md wdrls certs (toCardanoLovelace fee)
            (fst toMint) (fst toBurn) mintingScripts inpScripts
            stakingScriptM
  where
    wdrls = mkWithdrawals networkId wdrl
    mintingScripts = Map.union (snd toMint) (snd toBurn)

mkTx
    :: forall k era
     .  (TxWitnessTagFor k, EraConstraints era)
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
        TokenMap.empty TokenMap.empty Map.empty Map.empty Nothing
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
     . (EraConstraints era, TxWitnessTagFor k)
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
        pure $ case txWitnessTagFor @k of
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
    :: forall k ktype
     . TxWitnessTagFor k
    => KeyFlavorS k
    -> NetworkId
    -> TransactionLayer k ktype SealedTx
newTransactionLayer keyF networkId = TransactionLayer
    { mkTransaction = \era stakeCreds keystore _pp ctx selection -> do
        let ttl   = txValidityInterval ctx
        let wdrl = view #txWithdrawal ctx
        let delta = selectionDelta TxOut.coin selection
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
                Right selOf -> selectionDelta TxOut.coin selOf
                Left _preSel -> Coin 0
        let assetsToBeMinted = view #txAssetsToMint ctx
        let assetsToBeBurned = view #txAssetsToBurn ctx
        let inpsScripts = view #txNativeScriptInputs ctx
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
                            stakingScriptM
                            (Write.shelleyBasedEraFromRecentEra Write.recentEra)
                        else
                            constructUnsignedTx networkId (md, []) ttl wdrl
                            selection delta assetsToBeMinted assetsToBeBurned inpsScripts
                            Nothing
                            (Write.shelleyBasedEraFromRecentEra Write.recentEra)
                    _ ->
                        constructUnsignedTx networkId (md, []) ttl wdrl
                        selection delta assetsToBeMinted assetsToBeBurned inpsScripts
                        Nothing
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
                    stakingScriptM
                    (Write.shelleyBasedEraFromRecentEra Write.recentEra)

    , tokenBundleSizeAssessor =
        Compatibility.tokenBundleSizeAssessor

    , constraints = \pp -> txConstraints pp (txWitnessTagFor @k)

    , decodeTx = _decodeSealedTx

    , transactionWitnessTag = txWitnessTagFor @k
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


-- | Describes modifications that can be made to a `Tx` using `updateTx`.
data TxUpdate = TxUpdate
    { extraInputs :: [(TxIn, TxOut)]
    , extraCollateral :: [TxIn]
       -- ^ Only used in the Alonzo era and later. Will be silently ignored in
       -- previous eras.
    , extraOutputs :: [TxOut]
    , extraInputScripts :: [Script KeyHash]
    , feeUpdate :: TxFeeUpdate
        -- ^ Set a new fee or use the old one.
    }

-- | For testing that
-- @
--   forall tx. updateTx noTxUpdate tx
--      == Right tx or Left
-- @
noTxUpdate :: TxUpdate
noTxUpdate = TxUpdate [] [] [] [] UseOldTxFee

-- | Method to use when updating the fee of a transaction.
data TxFeeUpdate
    = UseOldTxFee
        -- ^ Instead of updating the fee, just use the old fee of the
        -- Tx (no-op for fee update).
    | UseNewTxFee Coin
        -- ^ Specify a new fee to use instead.
    deriving (Eq, Show)
-- Used to add inputs and outputs when balancing a transaction.
--
-- If the transaction contains existing key witnesses, it will return `Left`,
-- *even if `noTxUpdate` is used*. This last detail could be changed.
--
-- == Notes on implementation choices
--
-- We cannot rely on cardano-api here because `Cardano.TxBodyContent BuildTx`
-- cannot be extracted from an existing `TxBody`.
--
-- To avoid the need for `ledger -> wallet` conversions, this function can only
-- be used to *add* tx body content.
updateTx
    :: forall era. Write.IsRecentEra era
    => Cardano.Tx era
    -> TxUpdate
    -> Either ErrUpdateSealedTx (Cardano.Tx era)
updateTx (Cardano.Tx body existingKeyWits) extraContent = do
    -- NOTE: The script witnesses are carried along with the cardano-api
    -- `anyEraBody`.
    body' <- modifyTxBody extraContent body

    if null existingKeyWits
       then Right $ Cardano.Tx body' mempty
       else Left $ ErrExistingKeyWitnesses $ length existingKeyWits
  where
    era = recentEra @era

    modifyTxBody
        :: TxUpdate
        -> Cardano.TxBody era
        -> Either ErrUpdateSealedTx (Cardano.TxBody era)
    modifyTxBody ebc = \case
        Cardano.ShelleyTxBody shelleyEra bod scripts scriptData aux val ->
            Right $ Cardano.ShelleyTxBody shelleyEra
                (modifyShelleyTxBody ebc era bod)
                (scripts ++ (flip toLedgerScript era
                    <$> extraInputScripts))
                scriptData
                aux
                val
        Byron.ByronTxBody _ -> case Cardano.shelleyBasedEra @era of {}

    TxUpdate _ _ _ extraInputScripts _ = extraContent

    toLedgerScript
        :: Script KeyHash
        -> RecentEra era
        -> Ledger.Script (Cardano.ShelleyLedgerEra era)
    toLedgerScript walletScript = \case
        RecentEraBabbage ->
            Cardano.toShelleyScript $ Cardano.ScriptInEra
            Cardano.SimpleScriptInBabbage
            (Cardano.SimpleScript $ toCardanoSimpleScript walletScript)
        RecentEraConway ->
            Cardano.toShelleyScript $ Cardano.ScriptInEra
            Cardano.SimpleScriptInConway
            (Cardano.SimpleScript $ toCardanoSimpleScript walletScript)

modifyShelleyTxBody
    :: TxUpdate
    -> RecentEra era
    -> Ledger.TxBody (Cardano.ShelleyLedgerEra era)
    -> Ledger.TxBody (Cardano.ShelleyLedgerEra era)
modifyShelleyTxBody txUpdate = \case
    RecentEraBabbage ->
        over feeTxBodyL modifyFee
        . over outputsBabbageTxBodyL
            (<> StrictSeq.fromList (toBabbageTxOut <$> extraOutputs))
        . over inputsTxBodyL
            (<> Set.fromList (Cardano.toShelleyTxIn <$> extraInputs'))
        . over collateralInputsTxBodyL
            (<> Set.fromList (Cardano.toShelleyTxIn <$> extraCollateral'))
    RecentEraConway ->
        over feeTxBodyL modifyFee
        . over outputsTxBodyL
            (<> StrictSeq.fromList (toConwayTxOut <$> extraOutputs))
        . over inputsTxBodyL
            (<> Set.fromList (Cardano.toShelleyTxIn <$> extraInputs'))
        . over collateralInputsTxBodyL
            (<> Set.fromList (Cardano.toShelleyTxIn <$> extraCollateral'))
  where
    TxUpdate extraInputs extraCollateral extraOutputs _ feeUpdate = txUpdate
    extraInputs' = toCardanoTxIn . fst <$> extraInputs
    extraCollateral' = toCardanoTxIn <$> extraCollateral
    modifyFee old =
        case feeUpdate of
            UseNewTxFee (Coin c) -> Ledger.Coin (intCast c)
            UseOldTxFee -> old

-- | Evaluate a minimal fee amount necessary to pay for a given tx
-- using ledger's functionality.
evaluateMinimumFee
    :: Cardano.IsShelleyBasedEra era
    => Cardano.BundledProtocolParameters era
    -> KeyWitnessCount
    -> Cardano.TxBody era
    -> Coin
evaluateMinimumFee pp (KeyWitnessCount nWits nBootWits) body =
    fromCardanoLovelace (Cardano.evaluateTransactionFee pp body nWits 0)
        <> bootWitFees
    -- NOTE: Cardano.evaluateTransactionFee will error if passed non-zero
    -- nBootWits, so we need to account for it separately.
  where
    bootWitFees = Coin.fromNatural $ feePerByte * bytes
      where
        feePerByte :: Natural
        feePerByte =
            Coin.toNatural . fromCardanoLovelace
                $ Cardano.protocolParamTxFeePerByte
                $ Cardano.unbundleProtocolParams pp

        bytes :: Natural
        bytes = fromIntegral $ sizeOf_BootstrapWitnesses $ intCast nBootWits

-- | Estimate the size of the transaction (body) when fully signed.
estimateSignedTxSize
    :: forall era. Write.IsRecentEra era
    => Write.PParams (Write.ShelleyLedgerEra era)
    -> KeyWitnessCount
    -> Cardano.TxBody era
    -> TxSize
estimateSignedTxSize pparams nWits body =
    let
        -- Hack which allows us to rely on the ledger to calculate the size of
        -- witnesses:
        feeOfWits :: Coin
        feeOfWits = minfee nWits `Coin.difference` minfee mempty

        sizeOfWits :: TxSize
        sizeOfWits =
            case feeOfWits `coinQuotRem` feePerByte of
                (n, 0) -> TxSize n
                (_, _) -> error $ unwords
                    [ "estimateSignedTxSize:"
                    , "the impossible happened!"
                    , "Couldn't divide"
                    , show feeOfWits
                    , "lovelace (the fee contribution of"
                    , show nWits
                    , "witnesses) with"
                    , show feePerByte
                    , "lovelace/byte"
                    ]
        sizeOfTx :: TxSize
        sizeOfTx = TxSize
            . fromIntegral
            . BS.length
            . serialisedTx
            $ sealedTxFromCardanoBody body
    in
        sizeOfTx <> sizeOfWits
  where
    coinQuotRem :: Coin -> Coin -> (Natural, Natural)
    coinQuotRem (Coin p) (Coin q) = quotRem p q

    minfee :: KeyWitnessCount -> Coin
    minfee witCount = toWalletCoin $ Write.evaluateMinimumFee
        (Write.recentEra @era) pparams (toLedgerTx body) witCount

    toLedgerTx :: Cardano.TxBody era -> Write.Tx (Write.ShelleyLedgerEra era)
    toLedgerTx b = case Cardano.Tx b [] of
        Byron.ByronTx {} -> case Write.recentEra @era of {}
        Cardano.ShelleyTx _era ledgerTx -> ledgerTx

    feePerByte :: Coin
    feePerByte = Ledger.toWalletCoin $
        case Write.recentEra @era of
            Write.RecentEraBabbage -> pparams ^. ppMinFeeAL
            Write.RecentEraConway -> pparams ^. ppMinFeeAL

numberOfShelleyWitnesses :: Word -> KeyWitnessCount
numberOfShelleyWitnesses n = KeyWitnessCount n 0

-- | Estimates the required number of Shelley-era witnesses.
--
-- Because we don't take into account whether two pieces of tx content will need
-- the same key for signing, the result may be an overestimate.
--
-- For instance, this may happen if:
-- 1. Multiple inputs share the same payment key (like in a single address
-- wallet)
-- 2. We are updating our delegation and withdrawing rewards at the same time.
--
-- FIXME [ADP-1515] Improve estimation
--
-- NOTE: Similar to 'estimateTransactionKeyWitnessCount' from cardano-api, which
-- we cannot use because it requires a 'TxBodyContent BuildTx era'.
estimateKeyWitnessCount
    :: forall era. IsRecentEra era
    => Cardano.UTxO era
    -- ^ Must contain all inputs from the 'TxBody' or
    -- 'estimateKeyWitnessCount will 'error'.
    -> Cardano.TxBody era
    -> KeyWitnessCount
estimateKeyWitnessCount utxo txbody@(Cardano.TxBody txbodycontent) =
    let txIns = map fst $ Cardano.txIns txbodycontent
        txInsCollateral =
            case Cardano.txInsCollateral txbodycontent of
                Cardano.TxInsCollateral _ ins -> ins
                Cardano.TxInsCollateralNone -> []
        vkInsUnique = L.nub $ filter (hasVkPaymentCred utxo) $
            txIns ++ txInsCollateral
        txExtraKeyWits = Cardano.txExtraKeyWits txbodycontent
        txExtraKeyWits' = case txExtraKeyWits of
            Cardano.TxExtraKeyWitnesses _ khs -> khs
            _ -> []
        txWithdrawals = Cardano.txWithdrawals txbodycontent
        txWithdrawals' = case txWithdrawals of
            Cardano.TxWithdrawals _ wdls ->
                [ () | (_, _, Cardano.ViewTx) <- wdls ]
            _ -> []
        txUpdateProposal = Cardano.txUpdateProposal txbodycontent
        txUpdateProposal' = case txUpdateProposal of
            Cardano.TxUpdateProposal _
                (Cardano.UpdateProposal updatePerGenesisKey _) ->
                    Map.size updatePerGenesisKey
            _ -> 0
        txCerts = case Cardano.txCertificates txbodycontent of
            Cardano.TxCertificatesNone -> 0
            Cardano.TxCertificates _ certs _ ->
                sumVia estimateDelegSigningKeys certs
        scriptVkWitsUpperBound =
            fromIntegral
            $ sumVia estimateMaxWitnessRequiredPerInput
            $ mapMaybe toTimelockScript scripts
        nonInputWits = numberOfShelleyWitnesses $ fromIntegral $
            length txExtraKeyWits' +
            length txWithdrawals' +
            txUpdateProposal' +
            fromIntegral txCerts +
            scriptVkWitsUpperBound
        inputWits = KeyWitnessCount
            { nKeyWits = fromIntegral
                . length
                $ filter (not . hasBootstrapAddr utxo) vkInsUnique
            , nBootstrapWits = fromIntegral
                . length
                $ filter (hasBootstrapAddr utxo) vkInsUnique
            }
        in
            nonInputWits <> inputWits
  where
    scripts = case txbody of
        Cardano.ShelleyTxBody _ _ shelleyBodyScripts _ _ _ -> shelleyBodyScripts
        Byron.ByronTxBody {} -> error "estimateKeyWitnessCount: ByronTxBody"

    dummyKeyRole = Payment


    estimateDelegSigningKeys :: Cardano.Certificate -> Integer
    estimateDelegSigningKeys = \case
        Cardano.StakeAddressRegistrationCertificate _ -> 0
        Cardano.StakeAddressDeregistrationCertificate cred ->
            estimateWitNumForCred cred
        Cardano.StakeAddressPoolDelegationCertificate cred _ ->
            estimateWitNumForCred cred
        _ -> 1
      where
        -- Does not include the key witness needed for script credentials.
        -- They are accounted for separately in @scriptVkWitsUpperBound@.
        estimateWitNumForCred = \case
            Cardano.StakeCredentialByKey _ -> 1
            Cardano.StakeCredentialByScript _ -> 0


    toTimelockScript
        :: Ledger.Script (Cardano.ShelleyLedgerEra era)
        -> Maybe (Script KeyHash)
    toTimelockScript anyScript = case recentEra @era of
        RecentEraConway ->
            case anyScript of
                Alonzo.TimelockScript timelock ->
                    Just $ toWalletScript (const dummyKeyRole) timelock
                Alonzo.PlutusScript _ _ -> Nothing
        RecentEraBabbage ->
            case anyScript of
                Alonzo.TimelockScript timelock ->
                    Just $ toWalletScript (const dummyKeyRole) timelock
                Alonzo.PlutusScript _ _ -> Nothing

    hasVkPaymentCred
        :: Cardano.UTxO era
        -> Cardano.TxIn
        -> Bool
    hasVkPaymentCred (Cardano.UTxO u) inp = case Map.lookup inp u of
        Just (Cardano.TxOut addrInEra _ _ _) -> Cardano.isKeyAddress addrInEra
        Nothing ->
            error $ unwords
                [ "estimateMaxWitnessRequiredPerInput: input not in utxo."
                , "Caller is expected to ensure this does not happen."
                ]

    hasBootstrapAddr
        :: Cardano.UTxO era
        -> Cardano.TxIn
        -> Bool
    hasBootstrapAddr (Cardano.UTxO u) inp = case Map.lookup inp u of
        Just (Cardano.TxOut addrInEra _ _ _) ->
            case addrInEra of
                Cardano.AddressInEra Cardano.ByronAddressInAnyEra _ -> True
                _ -> False
        Nothing ->
            error $ unwords
                [ "estimateMaxWitnessRequiredPerInput: input not in utxo."
                , "Caller is expected to ensure this does not happen."
                ]

type BabbageTx =
    Ledger.Tx (Cardano.ShelleyLedgerEra Cardano.BabbageEra)

type ConwayTx =
    Ledger.Tx (Cardano.ShelleyLedgerEra Cardano.ConwayEra)

assignScriptRedeemers
    :: forall era. Write.IsRecentEra era
    => Write.PParams (Write.ShelleyLedgerEra era)
    -> TimeTranslation
    -> Cardano.UTxO era
    -> [Redeemer]
    -> Cardano.Tx era
    -> Either ErrAssignRedeemers (Cardano.Tx era)
assignScriptRedeemers pparams timeTranslation utxo redeemers tx =
    case Write.recentEra @era of
        Write.RecentEraBabbage -> do
            let Cardano.ShelleyTx _ babbageTx = tx
            babbageTx' <- flip execStateT babbageTx $ do
                indexedRedeemers <- StateT assignNullRedeemersBabbage
                executionUnits <- get
                    >>= lift . evaluateExecutionUnitsBabbage indexedRedeemers
                modifyM (assignExecutionUnitsBabbage executionUnits)
                modify' addScriptIntegrityHashBabbage
            pure $ Cardano.ShelleyTx ShelleyBasedEraBabbage babbageTx'
        Write.RecentEraConway -> do
            let Cardano.ShelleyTx _ conwayTx = tx
            conwayTx' <- flip execStateT conwayTx $ do
                indexedRedeemers <- StateT assignNullRedeemersConway
                executionUnits <- get
                    >>= lift . evaluateExecutionUnitsConway indexedRedeemers
                modifyM (assignExecutionUnitsConway executionUnits)
                modify' addScriptIntegrityHashConway
            pure $ Cardano.ShelleyTx ShelleyBasedEraConway conwayTx'
  where
    epochInformation :: EpochInfo (Either T.Text)
    epochInformation =
        hoistEpochInfo (left (T.pack . show)) $ epochInfo timeTranslation

    systemStart = systemStartTime timeTranslation

    -- | Assign redeemers with null execution units to the input transaction.
    --
    -- Redeemers are determined from the context given to the caller via the
    -- 'Redeemer' type which is mapped to an 'Alonzo.ScriptPurpose'.
    assignNullRedeemersBabbage
        :: BabbageTx
        -> Either ErrAssignRedeemers (Map Alonzo.RdmrPtr Redeemer, BabbageTx)
    assignNullRedeemersBabbage babbageTx = do
        (indexedRedeemers, nullRedeemers) <- fmap unzip $ forM redeemers $ \rd -> do
            ptr <-
                case Alonzo.rdptr (Alonzo.body babbageTx) (toScriptPurpose rd) of
                    SNothing -> Left $ ErrAssignRedeemersTargetNotFound rd
                    SJust ptr -> pure ptr
            rData <- case deserialiseOrFail (BL.fromStrict $ redeemerData rd) of
                Left e -> Left $ ErrAssignRedeemersInvalidData rd (show e)
                Right d -> pure (Alonzo.Data d)
            pure ((ptr, rd), (ptr, (rData, mempty)))
        pure
            ( Map.fromList indexedRedeemers
            , babbageTx
                { Alonzo.wits = (Alonzo.wits babbageTx)
                    { Alonzo.txrdmrs =
                        Alonzo.Redeemers (Map.fromList nullRedeemers)
                    }
                }
            )

    assignNullRedeemersConway
        :: ConwayTx
        -> Either ErrAssignRedeemers (Map Alonzo.RdmrPtr Redeemer, ConwayTx)
    assignNullRedeemersConway conwayTx = do
        (indexedRedeemers, nullRedeemers) <- fmap unzip $ forM redeemers $ \rd -> do
            ptr <-
                case Alonzo.rdptr (Alonzo.body conwayTx) (toScriptPurpose rd) of
                    SNothing -> Left $ ErrAssignRedeemersTargetNotFound rd
                    SJust ptr -> pure ptr
            rData <- case deserialiseOrFail (BL.fromStrict $ redeemerData rd) of
                Left e -> Left $ ErrAssignRedeemersInvalidData rd (show e)
                Right d -> pure (Alonzo.Data d)
            pure ((ptr, rd), (ptr, (rData, mempty)))
        pure
            ( Map.fromList indexedRedeemers
            , conwayTx
                { Alonzo.wits = (Alonzo.wits conwayTx)
                    { Alonzo.txrdmrs =
                        Alonzo.Redeemers (Map.fromList nullRedeemers)
                    }
                }
            )

    -- | Evaluate execution units of each script/redeemer in the transaction.
    -- This may fail for each script.
    evaluateExecutionUnitsBabbage
        :: era ~ Cardano.BabbageEra
        => Map Alonzo.RdmrPtr Redeemer
        -> BabbageTx
        -> Either ErrAssignRedeemers
            (Map Alonzo.RdmrPtr (Either ErrAssignRedeemers Alonzo.ExUnits))
    evaluateExecutionUnitsBabbage indexedRedeemers babbageTx = do
        let res =
                Ledger.evalTxExUnits
                    pparams
                    babbageTx
                    (fromCardanoUTxO utxo)
                    epochInformation
                    systemStart
        case res of
            Left translationError ->
                Left $ ErrAssignRedeemersTranslationError translationError
            Right report ->
                Right $ hoistScriptFailure indexedRedeemers report

    evaluateExecutionUnitsConway
        :: era ~ Cardano.ConwayEra
        => Map Alonzo.RdmrPtr Redeemer
        -> ConwayTx
        -> Either ErrAssignRedeemers
            (Map Alonzo.RdmrPtr (Either ErrAssignRedeemers Alonzo.ExUnits))
    evaluateExecutionUnitsConway indexedRedeemers conwayTx = do
        let res =
                Ledger.evalTxExUnits
                    pparams
                    conwayTx
                    (fromCardanoUTxO utxo)
                    epochInformation
                    systemStart
        case res of
            Left translationError ->
                Left $ ErrAssignRedeemersTranslationError translationError
            Right report ->
                Right $ hoistScriptFailure indexedRedeemers report

    hoistScriptFailure
        :: Show scriptFailure
        => Map Alonzo.RdmrPtr Redeemer
        -> Map Alonzo.RdmrPtr (Either scriptFailure a)
        -> Map Alonzo.RdmrPtr (Either ErrAssignRedeemers a)
    hoistScriptFailure indexedRedeemers = Map.mapWithKey $ \ptr -> left $ \e ->
        ErrAssignRedeemersScriptFailure (indexedRedeemers ! ptr) (show e)

    -- | Change execution units for each redeemers in the transaction to what
    -- they ought to be.
    assignExecutionUnitsBabbage
        :: Map Alonzo.RdmrPtr (Either ErrAssignRedeemers Alonzo.ExUnits)
        -> BabbageTx
        -> Either ErrAssignRedeemers BabbageTx
    assignExecutionUnitsBabbage exUnits babbageTx = do
        let wits = Alonzo.wits babbageTx
        let Alonzo.Redeemers rdmrs = Alonzo.txrdmrs wits
        rdmrs' <- Map.mergeA
            Map.preserveMissing
            Map.dropMissing
            (Map.zipWithAMatched (const assignUnits))
            rdmrs
            exUnits
        pure babbageTx
            { Alonzo.wits = wits { Alonzo.txrdmrs = Alonzo.Redeemers rdmrs' } }

    assignExecutionUnitsConway
        :: Map Alonzo.RdmrPtr (Either ErrAssignRedeemers Alonzo.ExUnits)
        -> ConwayTx
        -> Either ErrAssignRedeemers ConwayTx
    assignExecutionUnitsConway exUnits conwayTx = do
        let wits = Alonzo.wits conwayTx
        let Alonzo.Redeemers rdmrs = Alonzo.txrdmrs wits
        rdmrs' <- Map.mergeA
            Map.preserveMissing
            Map.dropMissing
            (Map.zipWithAMatched (const assignUnits))
            rdmrs
            exUnits
        pure conwayTx
            { Alonzo.wits = wits { Alonzo.txrdmrs = Alonzo.Redeemers rdmrs' } }

    assignUnits
        :: (dat, Alonzo.ExUnits)
        -> Either err Alonzo.ExUnits
        -> Either err (dat, Alonzo.ExUnits)
    assignUnits (dats, _zero) = fmap (dats,)

    -- | Finally, calculate and add the script integrity hash with the new
    -- final redeemers, if any.
    addScriptIntegrityHashBabbage
        :: era ~ Cardano.BabbageEra => BabbageTx -> BabbageTx
    addScriptIntegrityHashBabbage babbageTx =
        babbageTx & bodyTxL . scriptIntegrityHashTxBodyL .~
            Alonzo.hashScriptIntegrity
                (Set.fromList $ Alonzo.getLanguageView pparams <$> langs)
                (Alonzo.txrdmrs wits)
                (Alonzo.txdats wits)
      where
        wits = Alonzo.wits babbageTx
        langs =
            [ l
            | (_hash, script) <- Map.toList (Alonzo.txscripts wits)
            , (not . Ledger.isNativeScript @StandardBabbage) script
            , Just l <- [Alonzo.language script]
            ]

    addScriptIntegrityHashConway
        :: era ~ Cardano.ConwayEra => ConwayTx -> ConwayTx
    addScriptIntegrityHashConway conwayTx =
        conwayTx & bodyTxL . scriptIntegrityHashTxBodyL .~
            Alonzo.hashScriptIntegrity
                (Set.fromList $ Alonzo.getLanguageView pparams <$> langs)
                (Alonzo.txrdmrs wits)
                (Alonzo.txdats wits)
      where
        wits = Alonzo.wits conwayTx
        langs =
            [ l
            | (_hash, script) <- Map.toList (Alonzo.txscripts wits)
            , (not . Ledger.isNativeScript @StandardConway) script
            , Just l <- [Alonzo.language script]
            ]

getFeePerByteFromWalletPParams
    :: ProtocolParameters
    -> FeePerByte
getFeePerByteFromWalletPParams pp =
    FeePerByte $ ceiling slope
  where
    LinearFee LinearFunction{slope} = getFeePolicy $ txParameters pp

-- | Like the 'TxConstraints' field 'txRewardWithdrawalCost', but with added
-- support for shared wallets via the 'CA.ScriptTemplate' argument.
--
-- We may or may not want to support shared wallets in the full txConstraints.
_txRewardWithdrawalCost
    :: Write.FeePerByte
    -> Either CA.ScriptTemplate TxWitnessTag
    -> Coin
    -> Coin
_txRewardWithdrawalCost feePerByte witType =
    toWallet
    . Write.feeOfBytes feePerByte
    . unTxSize
    . _txRewardWithdrawalSize witType

-- | Like the 'TxConstraints' field 'txRewardWithdrawalSize', but with added
-- support for shared wallets via the 'CA.ScriptTemplate' argument.
--
-- We may or may not want to support shared wallets in the full txConstraints.
_txRewardWithdrawalSize
    :: Either CA.ScriptTemplate TxWitnessTag
    -> Coin
    -> TxSize
_txRewardWithdrawalSize _ (Coin 0) = TxSize 0
_txRewardWithdrawalSize witType _ = TxSize
        $ fromMaybe (error "txRewardWithdrawalSize: negative size")
        $ intCastMaybe
        $ sizeOf_Withdrawals 1 - sizeOf_Withdrawals 0 + wits
      where
        wits = case witType of
            Right TxWitnessByronUTxO ->
                sizeOf_BootstrapWitnesses 1 - sizeOf_BootstrapWitnesses 0
            Right TxWitnessShelleyUTxO ->
                sizeOf_VKeyWitnesses 1
            Left scriptTemplate ->
                let n = fromIntegral $ estimateMaxWitnessRequiredPerInput
                        $ view #template scriptTemplate
                in sizeOf_VKeyWitnesses n - sizeOf_VKeyWitnesses 0

txConstraints
    :: ProtocolParameters -> TxWitnessTag -> TxConstraints
txConstraints protocolParams witnessTag = TxConstraints
    { txBaseCost
    , txBaseSize
    , txInputCost
    , txInputSize
    , txOutputCost
    , txOutputSize
    , txOutputMaximumSize
    , txOutputMaximumTokenQuantity
    , txOutputMinimumAdaQuantity
    , txOutputBelowMinimumAdaQuantity
    , txRewardWithdrawalCost
    , txRewardWithdrawalSize
    , txMaximumSize
    }
  where
    txBaseCost =
        constantTxFee <> estimateTxCost feePerByte empty

    constantTxFee = Coin $ ceiling intercept
    feePerByte = getFeePerByteFromWalletPParams protocolParams
    LinearFee LinearFunction {intercept}
        = getFeePolicy $ txParameters protocolParams

    txBaseSize =
        estimateTxSize empty

    txInputCost =
        marginalCostOf empty {txInputCount = 1}

    txInputSize =
        marginalSizeOf empty {txInputCount = 1}

    txOutputCost bundle =
        marginalCostOf empty {txOutputs = [mkTxOut bundle]}

    txOutputSize bundle =
        marginalSizeOf empty {txOutputs = [mkTxOut bundle]}

    txOutputMaximumSize = (<>)
        (txOutputSize mempty)
        (view
            (#txParameters . #getTokenBundleMaxSize . #unTokenBundleMaxSize)
            protocolParams)

    txOutputMaximumTokenQuantity =
        TokenQuantity $ fromIntegral $ maxBound @Word64

    txOutputMinimumAdaQuantity =
        computeMinimumCoinForUTxO (minimumUTxO protocolParams)

    txOutputBelowMinimumAdaQuantity =
        isBelowMinimumCoinForUTxO (minimumUTxO protocolParams)

    txRewardWithdrawalCost =
        _txRewardWithdrawalCost feePerByte (Right witnessTag)

    txRewardWithdrawalSize =
        _txRewardWithdrawalSize (Right witnessTag)

    txMaximumSize = protocolParams
        & view (#txParameters . #getTxMaxSize)
        & getQuantity
        & fromIntegral
        & TxSize

    empty :: TxSkeleton
    empty = emptyTxSkeleton witnessTag

    -- Computes the size difference between the given skeleton and an empty
    -- skeleton.
    marginalCostOf :: TxSkeleton -> Coin
    marginalCostOf skeleton =
        Coin.distance
            (estimateTxCost feePerByte empty)
            (estimateTxCost feePerByte skeleton)

    -- Computes the size difference between the given skeleton and an empty
    -- skeleton.
    marginalSizeOf :: TxSkeleton -> TxSize
    marginalSizeOf =
        txSizeDistance txBaseSize . estimateTxSize

    -- Constructs a real transaction output from a token bundle.
    mkTxOut :: TokenBundle -> TxOut
    mkTxOut = TxOut dummyAddress
      where
        dummyAddress :: Address
        dummyAddress = Address $ BS.replicate dummyAddressLength nullByte

        dummyAddressLength :: Int
        dummyAddressLength = 57
        -- Note: We are at liberty to overestimate the length of an address
        -- (which is safe). Therefore, we can choose a length that we know is
        -- greater than or equal to all address lengths.

        nullByte :: Word8
        nullByte = 0

-- | Includes just the parts of a transaction necessary to estimate its size.
--
-- In particular, this record type includes the minimal set of data needed for
-- the 'estimateTxCost' and 'estimateTxSize' functions to perform their
-- calculations, and nothing else.
--
-- The data included in 'TxSkeleton' is a subset of the data included in the
-- union of 'SelectionSkeleton' and 'TransactionCtx'.
--
data TxSkeleton = TxSkeleton
    { txWitnessTag :: !TxWitnessTag
    , txInputCount :: !Int
    , txOutputs :: ![TxOut]
    , txChange :: ![Set AssetId]
    , txPaymentTemplate :: !(Maybe (Script Cosigner))
    }
    deriving (Eq, Show, Generic)

-- | Constructs an empty transaction skeleton.
--
-- This may be used to estimate the size and cost of an empty transaction.
--
emptyTxSkeleton :: TxWitnessTag -> TxSkeleton
emptyTxSkeleton txWitnessTag = TxSkeleton
    { txWitnessTag
    , txInputCount = 0
    , txOutputs = []
    , txChange = []
    , txPaymentTemplate = Nothing
    }

-- | Constructs a transaction skeleton from wallet primitive types.
--
-- This function extracts a subset of the data included in 'SelectionSkeleton'
-- and 'TransactionCtx'.
--
mkTxSkeleton
    :: TxWitnessTag
    -> TransactionCtx
    -> SelectionSkeleton
    -> TxSkeleton
mkTxSkeleton witness context skeleton = TxSkeleton
    { txWitnessTag = witness
    , txInputCount = view #skeletonInputCount skeleton
    , txOutputs = view #skeletonOutputs skeleton
    , txChange = view #skeletonChange skeleton
    , txPaymentTemplate =
        template <$>
        view #txPaymentCredentialScriptTemplate context
    }

-- | Estimates the final cost of a transaction based on its skeleton.
--
-- The constant tx fee is /not/ included in the result of this function.
estimateTxCost :: FeePerByte -> TxSkeleton -> Coin
estimateTxCost (FeePerByte feePerByte) skeleton =
    computeFee (estimateTxSize skeleton)
  where
    computeFee :: TxSize -> Coin
    computeFee (TxSize size) = Coin $ feePerByte * size

-- | Calculate the cost of increasing a CBOR-encoded Coin-value by another Coin
-- with the lovelace/byte cost given by the 'FeePolicy'.
--
-- Outputs values in the range of [0, 8 * perByteFee]
--
-- >>> let p = FeePolicy (Quantity 0) (Quantity 44)
--
-- >>> costOfIncreasingCoin p 4294967295 1
-- Coin 176 -- (9 bytes - 5 bytes) * 44 lovelace/byte
--
-- >>> costOfIncreasingCoin p 0 4294967296
-- Coin 352 -- 8 bytes * 44 lovelace/byte
costOfIncreasingCoin
    :: FeePerByte
    -> Coin -- ^ Original coin
    -> Coin -- ^ Increment
    -> Coin
costOfIncreasingCoin (FeePerByte perByte) from delta =
    costOfCoin (from <> delta) `Coin.difference` costOfCoin from
  where
    costOfCoin = Coin . (perByte *) . unTxSize . sizeOfCoin

-- The maximum cost increase 'costOfIncreasingCoin' can return, which is the
-- cost of 8 bytes.
maximumCostOfIncreasingCoin :: FeePerByte -> Coin
maximumCostOfIncreasingCoin (FeePerByte perByte) = Coin $ 8 * perByte

-- | Calculate the size of a coin when encoded as CBOR.
sizeOfCoin :: Coin -> TxSize
sizeOfCoin (Coin c)
    | c >= 4_294_967_296 = TxSize 9 -- c >= 2^32
    | c >=        65_536 = TxSize 5 -- c >= 2^16
    | c >=           256 = TxSize 3 -- c >= 2^ 8
    | c >=            24 = TxSize 2
    | otherwise          = TxSize 1

-- | Distributes a surplus transaction balance between the given change
-- outputs and the given fee. This function is aware of the fact that
-- any increase in a 'Coin' value could increase the size and fee
-- requirement of a transaction.
--
-- When comparing the original fee and change outputs to the adjusted
-- fee and change outputs, this function guarantees that:
--
--    - The number of the change outputs remains constant;
--
--    - The fee quantity either remains the same or increases.
--
--    - For each change output:
--        - the ada quantity either remains constant or increases.
--        - non-ada quantities remain the same.
--
--    - The surplus is conserved:
--        The total increase in the fee and change ada quantities is
--        exactly equal to the surplus.
--
--    - Any increase in cost is covered:
--        If the total cost has increased by 𝛿c, then the fee value
--        will have increased by at least 𝛿c.
--
-- If the cost of distributing the provided surplus is greater than the
-- surplus itself, the function will return 'ErrMoreSurplusNeeded'. If
-- the provided surplus is greater or equal to
-- @maximumCostOfIncreasingCoin feePolicy@, the function will always
-- return 'Right'.
distributeSurplus
    :: FeePerByte
    -> Coin
    -- ^ Surplus transaction balance to distribute.
    -> TxFeeAndChange [TxOut]
    -- ^ Original fee and change outputs.
    -> Either ErrMoreSurplusNeeded (TxFeeAndChange [TxOut])
    -- ^ Adjusted fee and change outputs.
distributeSurplus feePolicy surplus fc@(TxFeeAndChange fee change) =
    distributeSurplusDelta feePolicy surplus
        (mapTxFeeAndChange id (fmap TxOut.coin) fc)
    <&> mapTxFeeAndChange
        (fee <>)
        (zipWith (flip TxOut.addCoin) change)

distributeSurplusDelta
    :: FeePerByte
    -> Coin
    -- ^ Surplus to distribute
    -> TxFeeAndChange [Coin]
    -> Either ErrMoreSurplusNeeded (TxFeeAndChange [Coin])
distributeSurplusDelta feePolicy surplus (TxFeeAndChange fee change) =
    case change of
        changeHead : changeTail ->
            distributeSurplusDeltaWithOneChangeCoin feePolicy surplus
                (TxFeeAndChange fee changeHead)
            <&> mapTxFeeAndChange id
                (: (Coin 0 <$ changeTail))
        [] ->
            burnSurplusAsFees feePolicy surplus
                (TxFeeAndChange fee ())
            <&> mapTxFeeAndChange id
                (\() -> [])

distributeSurplusDeltaWithOneChangeCoin
    :: FeePerByte
    -> Coin -- ^ Surplus to distribute
    -> TxFeeAndChange Coin
    -> Either ErrMoreSurplusNeeded (TxFeeAndChange Coin)
distributeSurplusDeltaWithOneChangeCoin
    feePolicy surplus fc@(TxFeeAndChange fee0 change0) =
    let
        -- We calculate the maximum possible fee increase, by assuming the
        -- **entire** surplus is added to the change.
        extraFee = findFixpointIncreasingFeeBy $
            costOfIncreasingCoin feePolicy change0 surplus
    in
        case surplus `Coin.subtract` extraFee of
            Just extraChange ->
                Right $ TxFeeAndChange
                    { fee = extraFee
                    , change = extraChange
                    }
            Nothing ->
                -- The fee increase from adding the surplus to the change was
                -- greater than the surplus itself. This could happen if the
                -- surplus is small.
                burnSurplusAsFees feePolicy surplus
                    (mapTxFeeAndChange id (const ()) fc)
                        <&> mapTxFeeAndChange id (\() -> Coin 0)
  where
    -- Increasing the fee may itself increase the fee. If that is the case, this
    -- function will increase the fee further. The process repeats until the fee
    -- doesn't need to be increased.
    --
    -- The function will always converge because the result of
    -- 'costOfIncreasingCoin' is bounded to @8 * feePerByte@.
    --
    -- On mainnet it seems unlikely that the function would recurse more than
    -- one time, and certainly not more than twice. If the protocol parameters
    -- are updated to allow for slightly more expensive txs, it might be
    -- possible to hit the boundary at ≈4 ada where the fee would need 9 bytes
    -- rather than 5. This is already the largest boundary.
    --
    -- Note that both the argument and the result of this function are increases
    -- relative to 'fee0'.
    --
    -- == Example ==
    --
    -- In this more extreme example the fee is increased from increasing the fee
    -- itself:
    --
    -- @@
    --     let fee0 = 23
    --     let feePolicy = -- 300 lovelace / byte
    --
    --     findFixpointIncreasingFeeBy 1 = go 0 1
    --     -- Recurse:
    --     = go (0 + 1) (costOfIncreasingCoin feePolicy (23 + 0) 1)
    --     = go (0 + 1) 300
    --     -- Recurse:
    --     = go (1 + 300) (costOfIncreasingCoin feePolicy (23 + 1) 300)
    --     = go 301 300
    --     = go (301 + 300) (costOfIncreasingCoin feePolicy (23 + 301) 300)
    --     = go (301 + 300) 0
    --     = go 601 0
    --     = 601
    -- @@
    findFixpointIncreasingFeeBy = go mempty
      where
        go :: Coin -> Coin -> Coin
        go c (Coin 0) = c
        go c increase = go
            (c <> increase)
            (costOfIncreasingCoin feePolicy (c <> fee0) increase)

burnSurplusAsFees
    :: FeePerByte
    -> Coin -- Surplus
    -> TxFeeAndChange ()
    -> Either ErrMoreSurplusNeeded (TxFeeAndChange ())
burnSurplusAsFees feePolicy surplus (TxFeeAndChange fee0 ())
    | shortfall > Coin 0 =
        Left $ ErrMoreSurplusNeeded shortfall
    | otherwise =
        Right $ TxFeeAndChange surplus ()
  where
    costOfBurningSurplus = costOfIncreasingCoin feePolicy fee0 surplus
    shortfall = costOfBurningSurplus `Coin.difference` surplus

-- | Estimates the final size of a transaction based on its skeleton.
--
-- This function uses the upper bounds of CBOR serialized objects as the basis
-- for many of its calculations. The following document is used as a reference:
--
-- https://github.com/input-output-hk/cardano-ledger/blob/master/eras/shelley/test-suite/cddl-files/shelley.cddl
-- https://github.com/input-output-hk/cardano-ledger/blob/master/eras/shelley-ma/test-suite/cddl-files/shelley-ma.cddl
-- https://github.com/input-output-hk/cardano-ledger/blob/master/eras/alonzo/test-suite/cddl-files/alonzo.cddl
--
estimateTxSize
    :: TxSkeleton
    -> TxSize
estimateTxSize skeleton =
    TxSize $ fromIntegral sizeOf_Transaction
  where
    TxSkeleton
        { txWitnessTag
        , txInputCount
        , txOutputs
        , txChange
        , txPaymentTemplate
        } = skeleton

    numberOf_Inputs
        = fromIntegral txInputCount

    numberOf_ScriptVkeyWitnessesForPayment
        = intCast $ maybe 0 estimateMaxWitnessRequiredPerInput txPaymentTemplate

    numberOf_VkeyWitnesses
        = case txWitnessTag of
            TxWitnessByronUTxO -> 0
            TxWitnessShelleyUTxO ->
                -- there cannot be missing payment script if there is delegation script
                -- the latter is optional
                if numberOf_ScriptVkeyWitnessesForPayment == 0 then
                    numberOf_Inputs
                else
                    (numberOf_Inputs * numberOf_ScriptVkeyWitnessesForPayment)

    numberOf_BootstrapWitnesses
        = case txWitnessTag of
            TxWitnessByronUTxO -> numberOf_Inputs
            TxWitnessShelleyUTxO -> 0

    -- transaction =
    --   [ transaction_body
    --   , transaction_witness_set
    --   , transaction_metadata / null
    --   ]
    sizeOf_Transaction
        = sizeOf_SmallArray
        + sizeOf_TransactionBody
        + sizeOf_WitnessSet

    -- transaction_body =
    --   { 0 : set<transaction_input>
    --   , 1 : [* transaction_output]
    --   , 2 : coin ; fee
    --   , 3 : uint ; ttl
    --   , ? 4 : [* certificate]
    --   , ? 5 : withdrawals
    --   , ? 6 : update
    --   , ? 7 : metadata_hash
    --   , ? 8 : uint ; validity interval start
    --   , ? 9 : mint
    --   }
    sizeOf_TransactionBody
        = sizeOf_SmallMap
        + sizeOf_Inputs
        + sizeOf_Outputs
        + sizeOf_Fee
        + sizeOf_Ttl
        + sizeOf_Update
        + sizeOf_ValidityIntervalStart
        + sizeOf_HistoricalPadding
      where
        -- Preserved out of caution during refactoring. We should be able to
        -- drop this, but we may as well wait until we wave completely
        -- water-proof testing of the size estimation, e.g:
        -- prop> forall baseTx update.
        --      sizeOf_Update x >=
        --          (serializedSize (update baseTx)
        --          - serializedSize baseTx)
        -- where update is something similar to 'TxUpdate' or 'TxSkeleton'.
        sizeOf_HistoricalPadding = sizeOf_NoMetadata
          where
            -- When it's "empty", metadata are represented by a special
            -- "null byte" in CBOR `F6`.
            sizeOf_NoMetadata = 1

        -- 0 => set<transaction_input>
        sizeOf_Inputs
            = sizeOf_SmallUInt
            + sizeOf_Array
            + sizeOf_Input * numberOf_Inputs

        -- 1 => [* transaction_output]
        sizeOf_Outputs
            = sizeOf_SmallUInt
            + sizeOf_Array
            + F.sum (sizeOf_Output <$> txOutputs)
            + F.sum (sizeOf_ChangeOutput <$> txChange)

        -- 2 => fee
        sizeOf_Fee
            = sizeOf_SmallUInt
            + sizeOf_UInt

        -- 3 => ttl
        sizeOf_Ttl
            = sizeOf_SmallUInt
            + sizeOf_UInt

        -- ?6 => update
        sizeOf_Update
            = 0 -- Assuming no updates is running through cardano-wallet

        -- ?8 => uint ; validity interval start
        sizeOf_ValidityIntervalStart
            = sizeOf_UInt

    -- transaction_input =
    --   [ transaction_id : $hash32
    --   , index : uint
    --   ]
    sizeOf_Input
        = sizeOf_SmallArray
        + sizeOf_Hash32
        + sizeOf_UInt

    -- post_alonzo_transaction_output =
    --   { 0 : address
    --   , 1 : value
    --   , ? 2 : datum_option ; New; datum option
    --   , ? 3 : script_ref   ; New; script reference
    --   }
    -- value =
    --   coin / [coin,multiasset<uint>]
    sizeOf_PostAlonzoTransactionOutput TxOut {address, tokens}
        = sizeOf_SmallMap
        + sizeOf_SmallUInt
        + sizeOf_Address address
        + sizeOf_SmallUInt
        + sizeOf_SmallArray
        + sizeOf_Coin (TokenBundle.getCoin tokens)
        + sumVia sizeOf_NativeAsset (TokenBundle.getAssets tokens)

    sizeOf_Output
        = sizeOf_PostAlonzoTransactionOutput

    sizeOf_ChangeOutput :: Set AssetId -> Integer
    sizeOf_ChangeOutput
        = sizeOf_PostAlonzoChangeOutput

    -- post_alonzo_transaction_output =
    --   { 0 : address
    --   , 1 : value
    --   , ? 2 : datum_option ; New; datum option
    --   , ? 3 : script_ref   ; New; script reference
    --   }
    -- value =
    --   coin / [coin,multiasset<uint>]
    sizeOf_PostAlonzoChangeOutput :: Set AssetId -> Integer
    sizeOf_PostAlonzoChangeOutput xs
        = sizeOf_SmallMap
        + sizeOf_SmallUInt
        + sizeOf_ChangeAddress
        + sizeOf_SmallMap
        + sizeOf_SmallUInt
        + sizeOf_LargeUInt
        + sumVia sizeOf_NativeAsset xs

    -- We carry addresses already serialized, so it's a matter of measuring.
    sizeOf_Address addr
        = 2 + toInteger (BS.length (unAddress addr))

    -- For change address, we consider the worst-case scenario based on the
    -- given wallet scheme. Byron addresses are larger.
    --
    -- NOTE: we could do slightly better if we wanted to for Byron addresses and
    -- discriminate based on the network as well since testnet addresses are
    -- larger than mainnet ones. But meh.
    sizeOf_ChangeAddress
        = case txWitnessTag of
            TxWitnessByronUTxO -> 85
            TxWitnessShelleyUTxO -> 59

    -- value = coin / [coin,multiasset<uint>]
    -- We consider "native asset" to just be the "multiasset<uint>" part of the
    -- above, hence why we don't also include the size of the coin. Where this
    -- is used, the size of the coin and array are are added too.
    sizeOf_NativeAsset AssetId{tokenName}
        = sizeOf_MultiAsset sizeOf_LargeUInt tokenName

    -- multiasset<a> = { * policy_id => { * asset_name => a } }
    -- policy_id = scripthash
    -- asset_name = bytes .size (0..32)
    sizeOf_MultiAsset sizeOf_a name
      = sizeOf_SmallMap -- NOTE: Assuming < 23 policies per output
      + sizeOf_Hash28
      + sizeOf_SmallMap -- NOTE: Assuming < 23 assets per policy
      + sizeOf_AssetName name
      + sizeOf_a

    -- asset_name = bytes .size (0..32)
    sizeOf_AssetName name
        = 2 + toInteger (BS.length $ unTokenName name)

    -- Coins can really vary so it's very punishing to always assign them the
    -- upper bound. They will typically be between 3 and 9 bytes (only 6 bytes
    -- difference, but on 20+ outputs, one starts feeling it).
    --
    -- So, for outputs, since we have the values, we can compute it accurately.
    sizeOf_Coin
        = toInteger
        . BS.length
        . CBOR.toStrictByteString
        . CBOR.encodeWord64
        . Coin.unsafeToWord64

    determinePaymentTemplateSize scriptCosigner
        = sizeOf_Array
        + sizeOf_SmallUInt
        + numberOf_Inputs * (sizeOf_NativeScript scriptCosigner)

    -- transaction_witness_set =
    --   { ?0 => [* vkeywitness ]
    --   , ?1 => [* native_script ]
    --   , ?2 => [* bootstrap_witness ]
    --   }
    sizeOf_WitnessSet
        = sizeOf_SmallMap
        + sizeOf_VKeyWitnesses numberOf_VkeyWitnesses
        + maybe 0 determinePaymentTemplateSize txPaymentTemplate
        -- FIXME: Payment template needs to be multiplied with number of inputs
        + sizeOf_BootstrapWitnesses numberOf_BootstrapWitnesses

-- ?5 => withdrawals
sizeOf_Withdrawals :: Integer -> Integer
sizeOf_Withdrawals n
    = (if n > 0
        then sizeOf_SmallUInt + sizeOf_SmallMap
        else 0)
    + sizeOf_Withdrawal * n

  where
    -- withdrawals =
    --   { * reward_account => coin }
    sizeOf_Withdrawal
        = sizeOf_Hash28
        + sizeOf_LargeUInt

-- ?0 => [* vkeywitness ]
sizeOf_VKeyWitnesses :: Integer -> Integer
sizeOf_VKeyWitnesses n
    = (if n > 0
        then sizeOf_Array + sizeOf_SmallUInt else 0)
    + sizeOf_VKeyWitness * n

-- ?2 => [* bootstrap_witness ]
sizeOf_BootstrapWitnesses :: Integer -> Integer
sizeOf_BootstrapWitnesses n
    = (if n > 0
        then sizeOf_Array + sizeOf_SmallUInt
        else 0)
    + sizeOf_BootstrapWitness * n

-- vkeywitness =
--  [ $vkey
--  , $signature
--  ]
sizeOf_VKeyWitness :: Integer
sizeOf_VKeyWitness
    = sizeOf_SmallArray
    + sizeOf_VKey
    + sizeOf_Signature

-- bootstrap_witness =
--  [ public_key : $vkey
--  , signature  : $signature
--  , chain_code : bytes .size 32
--  , attributes : bytes
--  ]
sizeOf_BootstrapWitness :: Integer
sizeOf_BootstrapWitness
    = sizeOf_SmallArray
    + sizeOf_VKey
    + sizeOf_Signature
    + sizeOf_ChainCode
    + sizeOf_Attributes
  where
    sizeOf_ChainCode  = 34
    sizeOf_Attributes = 45 -- NOTE: could be smaller by ~34 for Icarus

-- native_script =
--   [ script_pubkey      = (0, addr_keyhash)
--   // script_all        = (1, [ * native_script ])
--   // script_any        = (2, [ * native_script ])
--   // script_n_of_k     = (3, n: uint, [ * native_script ])
--   // invalid_before    = (4, uint)
--      ; Timelock validity intervals are half-open intervals [a, b).
--      ; This field specifies the left (included) endpoint a.
--   // invalid_hereafter = (5, uint)
--      ; Timelock validity intervals are half-open intervals [a, b).
--      ; This field specifies the right (excluded) endpoint b.
--   ]
sizeOf_NativeScript :: Script object -> Integer
sizeOf_NativeScript = \case
    RequireSignatureOf _ ->
        sizeOf_SmallUInt + sizeOf_Hash28
    RequireAllOf ss ->
        sizeOf_SmallUInt + sizeOf_Array + sumVia sizeOf_NativeScript ss
    RequireAnyOf ss ->
        sizeOf_SmallUInt + sizeOf_Array + sumVia sizeOf_NativeScript ss
    RequireSomeOf _ ss ->
        sizeOf_SmallUInt
            + sizeOf_UInt
            + sizeOf_Array
            + sumVia sizeOf_NativeScript ss
    ActiveFromSlot _ ->
        sizeOf_SmallUInt + sizeOf_UInt
    ActiveUntilSlot _ ->
        sizeOf_SmallUInt + sizeOf_UInt

-- A Blake2b-224 hash, resulting in a 28-byte digest wrapped in CBOR, so
-- with 2 bytes overhead (length <255, but length > 23)
sizeOf_Hash28 :: Integer
sizeOf_Hash28
    = 30

-- A Blake2b-256 hash, resulting in a 32-byte digest wrapped in CBOR, so
-- with 2 bytes overhead (length <255, but length > 23)
sizeOf_Hash32 :: Integer
sizeOf_Hash32
    = 34

-- A 32-byte Ed25519 public key, encoded as a CBOR-bytestring so with 2
-- bytes overhead (length < 255, but length > 23)
sizeOf_VKey :: Integer
sizeOf_VKey
    = 34

-- A 64-byte Ed25519 signature, encoded as a CBOR-bytestring so with 2
-- bytes overhead (length < 255, but length > 23)
sizeOf_Signature :: Integer
sizeOf_Signature
    = 66

-- A CBOR UInt which is less than 23 in value fits on a single byte. Beyond,
-- the first byte is used to encode the number of bytes necessary to encode
-- the number itself, followed by the number itself.
--
-- When considering a 'UInt', we consider the worst case scenario only where
-- the uint is encoded over 4 bytes, so up to 2^32 which is fine for most
-- cases but coin values.
sizeOf_SmallUInt :: Integer
sizeOf_SmallUInt = 1

sizeOf_UInt :: Integer
sizeOf_UInt = 5

sizeOf_LargeUInt :: Integer
sizeOf_LargeUInt = 9

-- A CBOR array with less than 23 elements, fits on a single byte, followed
-- by each key-value pair (encoded as two concatenated CBOR elements).
sizeOf_SmallMap :: Integer
sizeOf_SmallMap = 1

-- A CBOR array with less than 23 elements, fits on a single byte, followed
-- by each elements. Otherwise, the length of the array is encoded first,
-- very much like for UInt.
--
-- When considering an 'Array', we consider large scenarios where arrays can
-- have up to 65536 elements.
sizeOf_SmallArray :: Integer
sizeOf_SmallArray = 1

sizeOf_Array :: Integer
sizeOf_Array = 3

-- Small helper function for summing values. Given a list of values, get the sum
-- of the values, after the given function has been applied to each value.
sumVia :: (Foldable t, Num m) => (a -> m) -> t a -> m
sumVia f = F.foldl' (\t -> (t +) . f) 0

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
    :: forall era.  Cardano.IsCardanoEra era
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
    stakingScriptM = extractValidatedOutputs cs >>= \outs ->
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
                mkErr aid q = ErrMkTransactionTokenQuantityExceedsLimit
                    $ SelectionOutputTokenQuantityExceedsLimitError
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
    :: forall era. (EraConstraints era)
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
