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
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

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
    , updateSealedTx

    -- * Internals
    , TxPayload (..)
    , TxSkeleton (..)
    , TxWitnessTag (..)
    , TxWitnessTagFor (..)
    , _decodeSealedTx
    , _estimateMaxNumberOfInputs
    , _maxScriptExecutionCost
    , mkDelegationCertificates
    , estimateTxCost
    , estimateTxSize
    , mkByronWitness
    , mkShelleyWitness
    , mkTx
    , mkTxSkeleton
    , mkUnsignedTx
    , txConstraints
    , costOfIncreasingCoin
    , _distributeSurplus
    , distributeSurplusDelta
    , sizeOfCoin
    , maximumCostOfIncreasingCoin
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPrv, toXPub )
import Cardano.Address.Script
    ( KeyHash, Script (..), foldScript )
import Cardano.Api
    ( AnyCardanoEra (..)
    , ByronEra
    , CardanoEra (..)
    , InAnyCardanoEra (..)
    , IsShelleyBasedEra (..)
    , NetworkId
    , SerialiseAsCBOR (..)
    , ShelleyBasedEra (..)
    , ToCBOR
    )
import Cardano.Binary
    ( serialize' )
import Cardano.Crypto.Wallet
    ( XPub )
import Cardano.Ledger.Alonzo.Tools
    ( BasicFailure (..), evaluateTransactionExecutionUnits )
import Cardano.Ledger.Crypto
    ( DSIGN )
import Cardano.Ledger.Era
    ( Crypto, Era, ValidateScript (..) )
import Cardano.Ledger.Shelley.API
    ( StrictMaybe (..) )
import Cardano.Slotting.EpochInfo.API
    ( hoistEpochInfo )
import Cardano.Wallet.CoinSelection
    ( SelectionLimitOf (..)
    , SelectionOf (..)
    , SelectionSkeleton (..)
    , selectionDelta
    )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..), RewardAccount (..), WalletKey (..) )
import Cardano.Wallet.Primitive.AddressDerivation.Byron
    ( ByronKey )
import Cardano.Wallet.Primitive.AddressDerivation.Icarus
    ( IcarusKey )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( ShelleyKey, toRewardAccountRaw )
import Cardano.Wallet.Primitive.Passphrase
    ( Passphrase (..) )
import Cardano.Wallet.Primitive.Slotting
    ( PastHorizonException, TimeInterpreter, getSystemStart, toEpochInfo )
import Cardano.Wallet.Primitive.Types
    ( Certificate
    , ExecutionUnitPrices (..)
    , ExecutionUnits (..)
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
    , TxConstraints (..)
    , TxIn
    , TxMetadata (..)
    , TxOut (..)
    , TxSize (..)
    , sealedTxFromCardano'
    , sealedTxFromCardanoBody
    , txOutAddCoin
    , txOutCoin
    , txSizeDistance
    )
import Cardano.Wallet.Primitive.Types.UTxO
    ( UTxO (..) )
import Cardano.Wallet.Shelley.Compatibility
    ( cardanoCertKeysForWitnesses
    , fromCardanoAddress
    , fromCardanoLovelace
    , fromCardanoTx
    , fromCardanoTxIn
    , fromCardanoWdrls
    , fromShelleyTxIn
    , toAlonzoPParams
    , toCardanoLovelace
    , toCardanoPolicyId
    , toCardanoSimpleScript
    , toCardanoStakeCredential
    , toCardanoTxIn
    , toCardanoTxOut
    , toCardanoValue
    , toCostModelsAsArray
    , toHDPayloadAddress
    , toScriptPurpose
    , toStakeKeyDeregCert
    , toStakeKeyRegCert
    , toStakePoolDlgCert
    )
import Cardano.Wallet.Shelley.Compatibility.Ledger
    ( computeMinimumAdaQuantity, toAlonzoTxOut )
import Cardano.Wallet.Transaction
    ( AnyScript (..)
    , DelegationAction (..)
    , ErrAssignRedeemers (..)
    , ErrMkTransaction (..)
    , ErrMoreSurplusNeeded (ErrMoreSurplusNeeded)
    , ErrUpdateSealedTx (..)
    , TokenMapWithScripts
    , TransactionCtx (..)
    , TransactionLayer (..)
    , TxFeeAndChange (..)
    , TxFeeUpdate (..)
    , TxUpdate (..)
    , mapTxFeeAndChange
    , withdrawalToCoin
    )
import Cardano.Wallet.Util
    ( internalError, modifyM )
import Codec.Serialise
    ( deserialiseOrFail )
import Control.Arrow
    ( left, second )
import Control.Monad
    ( forM, guard )
import Control.Monad.Trans.Class
    ( lift )
import Control.Monad.Trans.Except
    ( runExceptT )
import Control.Monad.Trans.State.Strict
    ( StateT (..), execStateT, get, modify' )
import Data.Bifunctor
    ( bimap )
import Data.Function
    ( (&) )
import Data.Functor
    ( ($>), (<&>) )
import Data.Functor.Identity
    ( runIdentity )
import Data.Generics.Internal.VL.Lens
    ( view, (^.) )
import Data.Generics.Labels
    ()
import Data.IntCast
    ( intCast )
import Data.Kind
    ( Type )
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
    ( Word16, Word64, Word8 )
import GHC.Generics
    ( Generic )
import Numeric.Natural
    ( Natural )
import Ouroboros.Network.Block
    ( SlotNo )

import qualified Cardano.Api as Cardano
import qualified Cardano.Api.Byron as Byron
import qualified Cardano.Api.Shelley as Cardano
import qualified Cardano.Chain.Common as Byron
import qualified Cardano.Crypto as CC
import qualified Cardano.Crypto.DSIGN as DSIGN
import qualified Cardano.Crypto.Hash.Class as Crypto
import qualified Cardano.Crypto.Wallet as Crypto.HD
import qualified Cardano.Ledger.Alonzo.Data as Alonzo
import qualified Cardano.Ledger.Alonzo.PlutusScriptApi as Alonzo
import qualified Cardano.Ledger.Alonzo.PParams as Alonzo
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import qualified Cardano.Ledger.Alonzo.TxWitness as Alonzo
import qualified Cardano.Ledger.Coin as Ledger
import qualified Cardano.Ledger.Core as Ledger
import qualified Cardano.Ledger.Shelley.Address.Bootstrap as SL
import qualified Cardano.Ledger.Shelley.Tx as Shelley
import qualified Cardano.Ledger.Shelley.UTxO as Ledger
import qualified Cardano.Ledger.ShelleyMA.TxBody as ShelleyMA
import qualified Cardano.Wallet.Primitive.Types.Coin as Coin
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap
import qualified Cardano.Wallet.Shelley.Compatibility as Compatibility
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

data TxWitnessTag
    = TxWitnessByronUTxO WalletStyle
    | TxWitnessShelleyUTxO
    deriving (Show, Eq)

data WalletStyle
    = Icarus
    | Byron
    deriving (Show, Eq)

type EraConstraints era =
    ( IsShelleyBasedEra era
    , ToCBOR (Ledger.TxBody (Cardano.ShelleyLedgerEra era))
    , Era (Cardano.ShelleyLedgerEra era)
    , DSIGN (Crypto (Cardano.ShelleyLedgerEra era)) ~ DSIGN.Ed25519DSIGN
    , (era == ByronEra) ~ 'False
    )

-- | Provide a transaction witness for a given private key. The type of witness
-- is different between types of keys and, with backward-compatible support, we
-- need to support many types for one backend target.
class TxWitnessTagFor (k :: Depth -> Type -> Type) where
    txWitnessTagFor :: TxWitnessTag

instance TxWitnessTagFor ShelleyKey where
    txWitnessTagFor = TxWitnessShelleyUTxO

instance TxWitnessTagFor IcarusKey where
    txWitnessTagFor = TxWitnessByronUTxO Icarus

instance TxWitnessTagFor ByronKey where
    txWitnessTagFor = TxWitnessByronUTxO Byron

constructUnsignedTx
    :: forall era.
        ( EraConstraints era
        )
    => Cardano.NetworkId
    -> (Maybe Cardano.TxMetadata, [Cardano.Certificate])
    -> SlotNo
    -- ^ Slot at which the transaction will expire.
    -> RewardAccount
    -- ^ Reward account
    -> Coin
    -- ^ An optional withdrawal amount, can be zero
    -> SelectionOf TxOut
    -- ^ Finalized asset selection
    -> Coin
    -- ^ Explicit fee amount
    -> (TokenMap, Map AssetId (Script KeyHash))
    -- ^ Assets to be minted
    -> (TokenMap, Map AssetId (Script KeyHash))
    -- ^ Assets to be burned
    -> ShelleyBasedEra era
    -> Either ErrMkTransaction SealedTx
constructUnsignedTx
    networkId (md, certs) ttl rewardAcnt wdrl cs fee toMint toBurn era =
        sealedTxFromCardanoBody <$> tx
  where
    tx = mkUnsignedTx era ttl cs md wdrls certs (toCardanoLovelace fee)
        (fst toMint) (fst toBurn) allScripts
    wdrls = mkWithdrawals networkId rewardAcnt wdrl
    allScripts = Map.union (snd toMint) (snd toBurn)

mkTx
    :: forall k era.
        ( TxWitnessTagFor k
        , WalletKey k
        , EraConstraints era
        )
    => Cardano.NetworkId
    -> TxPayload era
    -> SlotNo
    -- ^ Slot at which the transaction will expire.
    -> (XPrv, Passphrase "encryption")
    -- ^ Reward account
    -> (Address -> Maybe (k 'AddressK XPrv, Passphrase "encryption"))
    -- ^ Key store
    -> Coin
    -- ^ An optional withdrawal amount, can be zero
    -> SelectionOf TxOut
    -- ^ Finalized asset selection
    -> Coin
    -- ^ Explicit fee amount
    -> ShelleyBasedEra era
    -> Either ErrMkTransaction (Tx, SealedTx)
mkTx networkId payload ttl (rewardAcnt, pwdAcnt) addrResolver wdrl cs fees era = do
    let TxPayload md certs mkExtraWits = payload
    let wdrls = mkWithdrawals
            networkId
            (toRewardAccountRaw . toXPub $ rewardAcnt)
            wdrl

    unsigned <- mkUnsignedTx era ttl cs md wdrls certs (toCardanoLovelace fees)
        TokenMap.empty TokenMap.empty Map.empty
    let signed = signTransaction networkId acctResolver (const Nothing)
            addrResolver inputResolver (unsigned, mkExtraWits unsigned)

    let withResolvedInputs (tx, _, _, _) = tx
            { resolvedInputs = second txOutCoin <$> F.toList (view #inputs cs)
            }
    Right ( withResolvedInputs (fromCardanoTx signed)
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
    :: forall k era.
        ( EraConstraints era
        , TxWitnessTagFor k
        , WalletKey k
        )
    => Cardano.NetworkId
    -- ^ Network identifier (e.g. mainnet, testnet)
    -> (RewardAccount -> Maybe (XPrv, Passphrase "encryption"))
    -- ^ Stake key store / reward account resolution
    -> (KeyHash -> Maybe (XPrv, Passphrase "encryption"))
    -- ^ Policy key resolution
    -> (Address -> Maybe (k 'AddressK XPrv, Passphrase "encryption"))
    -- ^ Payment key store
    -> (TxIn -> Maybe Address)
    -- ^ Input resolver
    -> (Cardano.TxBody era, [Cardano.KeyWitness era])
    -- ^ The transaction to sign, possibly with already some existing witnesses
    -> Cardano.Tx era
signTransaction
    networkId
    resolveRewardAcct
    resolvePolicyKey
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
        , mapMaybe mkPolicyWitness mintBurnScripts
        ]
      where
        Cardano.TxBody bodyContent = body

        inputs =
            [ fromCardanoTxIn i
            | (i, _) <- Cardano.txIns bodyContent
            ]

        collaterals =
            case Cardano.txInsCollateral bodyContent of
                Cardano.TxInsCollateralNone ->
                    []
                Cardano.TxInsCollateral _ is ->
                    fromCardanoTxIn <$> is

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

        mintBurnScripts =
            let (_, toMint, toBurn, _) = fromCardanoTx $
                    Cardano.makeSignedTransaction wits body
            in
            -- Note that we use 'nub' here because multiple scripts can share
            -- the same policyXPub. It's sufficient to have one witness for
            -- each.
            L.nub $ getScripts toMint <> getScripts toBurn

    getScripts :: TokenMapWithScripts -> [KeyHash]
    getScripts scripts =
        let retrieveAllKeyHashes (NativeScript s) = foldScript (:) [] s
            retrieveAllKeyHashes _ = []
            isTimelock (NativeScript _) = True
            isTimelock _ = False
        in concatMap retrieveAllKeyHashes $
           filter isTimelock $
           Map.elems $ scripts ^. #txScripts

    mkTxInWitness :: TxIn -> Maybe (Cardano.KeyWitness era)
    mkTxInWitness i = do
        addr <- resolveInput i
        (k, pwd) <- resolveAddress addr
        pure $ case (txWitnessTagFor @k) of
            TxWitnessShelleyUTxO ->
                mkShelleyWitness body (getRawKey k, pwd)
            TxWitnessByronUTxO{} ->
                mkByronWitness body networkId addr (getRawKey k, pwd)

    mkWdrlCertWitness :: RewardAccount -> Maybe (Cardano.KeyWitness era)
    mkWdrlCertWitness a =
        mkShelleyWitness body <$> resolveRewardAcct a

    mkPolicyWitness :: KeyHash -> Maybe (Cardano.KeyWitness era)
    mkPolicyWitness a =
        mkShelleyWitness body <$> resolvePolicyKey a

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
        pure $ mkShelleyWitness body (getRawKey k, pwd)

newTransactionLayer
    :: forall k.
        ( TxWitnessTagFor k
        , WalletKey k
        )
    => NetworkId
    -> TransactionLayer k SealedTx
newTransactionLayer networkId = TransactionLayer
    { mkTransaction = \era stakeCreds keystore _pp ctx selection -> do
        let ttl   = txTimeToLive ctx
        let wdrl  = withdrawalToCoin $ view #txWithdrawal ctx
        let delta = selectionDelta txOutCoin selection
        case view #txDelegationAction ctx of
            Nothing -> do
                withShelleyBasedEra era $ do
                    let payload = TxPayload (view #txMetadata ctx) mempty mempty
                    mkTx networkId payload ttl stakeCreds keystore wdrl
                        selection delta

            Just action -> do
                withShelleyBasedEra era $ do
                    let stakeXPub = toXPub $ fst stakeCreds
                    let certs = mkDelegationCertificates action stakeXPub
                    let payload = TxPayload (view #txMetadata ctx) certs (const [])
                    mkTx networkId payload ttl stakeCreds keystore wdrl
                        selection delta

    , addVkWitnesses =
        \_era stakeCreds policyCreds addressResolver inputResolver sealedTx ->
        do
            let acctResolver
                    :: RewardAccount -> Maybe (XPrv, Passphrase "encryption")
                acctResolver acct = do
                    let acct' = toRewardAccountRaw $ toXPub $ fst stakeCreds
                    guard (acct == acct') $> stakeCreds
            let policyResolver
                    :: KeyHash -> Maybe (XPrv, Passphrase "encryption")
                policyResolver keyhash = do
                    let (keyhash', xprv, encP) = policyCreds
                    guard (keyhash == keyhash') $> (xprv, encP)
            case cardanoTx sealedTx of
                InAnyCardanoEra ByronEra _ ->
                    sealedTx
                InAnyCardanoEra ShelleyEra (Cardano.Tx body wits) ->
                    signTransaction networkId acctResolver (const Nothing)
                    addressResolver inputResolver (body, wits)
                    & sealedTxFromCardano'
                InAnyCardanoEra AllegraEra (Cardano.Tx body wits) ->
                    signTransaction networkId acctResolver (const Nothing)
                    addressResolver inputResolver (body, wits)
                    & sealedTxFromCardano'
                InAnyCardanoEra MaryEra (Cardano.Tx body wits) ->
                    signTransaction networkId acctResolver policyResolver
                    addressResolver inputResolver (body, wits)
                    & sealedTxFromCardano'
                InAnyCardanoEra AlonzoEra (Cardano.Tx body wits) ->
                    signTransaction networkId acctResolver policyResolver
                    addressResolver inputResolver (body, wits)
                    & sealedTxFromCardano'

    , mkUnsignedTransaction = \era stakeXPub _pp ctx selection -> do
        let ttl   = txTimeToLive ctx
        let wdrl  = withdrawalToCoin $ view #txWithdrawal ctx
        let delta = selectionDelta txOutCoin selection
        let rewardAcct = toRewardAccountRaw stakeXPub
        let assetsToBeMinted = view #txAssetsToMint ctx
        let assetsToBeBurned = view #txAssetsToBurn ctx
        case view #txDelegationAction ctx of
            Nothing -> do
                withShelleyBasedEra era $ do
                    let md = view #txMetadata ctx
                    constructUnsignedTx networkId (md, []) ttl rewardAcct wdrl
                        selection delta assetsToBeMinted assetsToBeBurned

            Just action -> do
                withShelleyBasedEra era $ do
                    let certs = mkDelegationCertificates action stakeXPub
                    let payload = (view #txMetadata ctx, certs)
                    constructUnsignedTx networkId payload ttl rewardAcct wdrl
                        selection delta assetsToBeMinted assetsToBeBurned

    , estimateSignedTxSize = \pp (Cardano.Tx body _) -> do
        _estimateSignedTxSize pp body

    , calcMinimumCost = \pp ctx skeleton ->
        estimateTxCost pp (mkTxSkeleton (txWitnessTagFor @k) ctx skeleton)
        <>
        txFeePadding ctx

    , maxScriptExecutionCost =
        _maxScriptExecutionCost

    , distributeSurplus = _distributeSurplus

    , assignScriptRedeemers =
        _assignScriptRedeemers

    , evaluateMinimumFee =
        _evaluateMinimumFee

    , evaluateTransactionBalance = _evaluateTransactionBalance

    , computeSelectionLimit = \pp ctx outputsToCover ->
        let txMaxSize = getTxMaxSize $ txParameters pp in
        MaximumInputLimit $
            _estimateMaxNumberOfInputs @k txMaxSize ctx outputsToCover

    , tokenBundleSizeAssessor =
        Compatibility.tokenBundleSizeAssessor

    , constraints = \pp -> txConstraints pp (txWitnessTagFor @k)

    , decodeTx = _decodeSealedTx

    , updateTx = updateSealedTx
    }

_decodeSealedTx
    :: SealedTx
    -> (Tx, TokenMapWithScripts, TokenMapWithScripts, [Certificate])
_decodeSealedTx (cardanoTx -> InAnyCardanoEra _era tx) = fromCardanoTx tx

_evaluateTransactionBalance
    :: forall era. IsShelleyBasedEra era
    => Cardano.Tx era
    -> Cardano.ProtocolParameters
    -> UTxO
    -> [(TxIn, TxOut, Maybe (Hash "Datum"))]
    -> Cardano.Value
_evaluateTransactionBalance (Cardano.Tx body _) pp utxo extraUTxO =
        let
            utxo' = Map.fromList
                . map (bimap toCardanoTxIn (toCardanoTxOut era))
                . Map.toList
                $ unUTxO utxo

            extraUTxO' = Map.fromList
                . map (\(i, o, mDatumHash) ->
                    (toCardanoTxIn i
                    , setDatumHash era mDatumHash (toCardanoTxOut era o))
                    )
                $ extraUTxO
        in
            lovelaceFromCardanoTxOutValue
                $ Cardano.evaluateTransactionBalance
                    pp
                    mempty
                    (Cardano.UTxO $ utxo' <> extraUTxO')
                    -- The two UTxO sets could overlap here. When called by
                    -- 'balanceTransaction' the user-specified input resolution
                    -- will overwrite the wallet UTxO (if in conflict).
                    --
                    -- If the overridden outputs are incorrect, the wallet will
                    -- incorrectly calculate the balance, and the transaction
                    -- will ultimately be rejected by the node.
                    --
                    -- If the overwridden outputs simply adds datum hashes
                    -- (which the wallet cannot currently represent), this
                    -- shouldn't affect the balance.
                    --
                    -- Ultimately, however, it might be be wiser to error out of
                    -- caution.
                    --
                    -- NOTE: There is a similar case in the 'resolveInput' of
                    -- 'balanceTransaction'.
                    body
  where
    era = Cardano.shelleyBasedEra @era

    setDatumHash
        :: ShelleyBasedEra era
        -> Maybe (Hash "Datum")
        -> Cardano.TxOut ctx era
        -> Cardano.TxOut ctx era
    setDatumHash _era Nothing o = o
    setDatumHash _era (Just (Hash datumHash)) (Cardano.TxOut addr val _) =
        Cardano.TxOut addr val (Cardano.TxOutDatumHash scriptDataSupported hash)
      where
        scriptDataSupported = case era of
            ShelleyBasedEraAlonzo -> Cardano.ScriptDataInAlonzoEra
            ShelleyBasedEraMary -> errBadEra
            ShelleyBasedEraAllegra -> errBadEra
            ShelleyBasedEraShelley -> errBadEra
          where
            -- FIXME [ADP-1479] Proper error handling
            errBadEra = error $ unwords
                [ "evaluateTransactionBalance:"
                , "cannot add a datum hash to the transaction body of an"
                , "era that doesn't support datum hashes."
                ]
        hash = fromMaybe errBadHash $ Cardano.deserialiseFromRawBytes
            (Cardano.AsHash Cardano.AsScriptData)
            datumHash
          where
            -- FIXME [ADP-1479] Proper error handling
            errBadHash = error $ unwords
                [ "evaluateTransactionBalance: couldn't convert hash "
                , show datumHash
                ]

    lovelaceFromCardanoTxOutValue
        :: Cardano.TxOutValue era -> Cardano.Value
    lovelaceFromCardanoTxOutValue = \case
        Cardano.TxOutAdaOnly _ ada -> Cardano.lovelaceToValue ada
        Cardano.TxOutValue _ val   -> val

mkDelegationCertificates
    :: DelegationAction
        -- Pool Id to which we're planning to delegate
    -> XPub
        -- Reward account public key
    -> [Cardano.Certificate]
mkDelegationCertificates da accXPub =
    case da of
       Join poolId ->
               [ toStakePoolDlgCert accXPub poolId ]
       RegisterKeyAndJoin poolId ->
               [ toStakeKeyRegCert  accXPub
               , toStakePoolDlgCert accXPub poolId
               ]
       Quit -> [toStakeKeyDeregCert accXPub]


-- | For testing that
-- @
--   forall tx. updateSealedTx noTxUpdate tx
--      == Right tx or Left
-- @
noTxUpdate :: TxUpdate
noTxUpdate = TxUpdate [] [] [] UseOldTxFee

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
updateSealedTx
    :: forall era. Cardano.IsShelleyBasedEra era
    => Cardano.Tx era
    -> TxUpdate
    -> Either ErrUpdateSealedTx (Cardano.Tx era)
updateSealedTx (Cardano.Tx body existingKeyWits) extraContent = do
    -- NOTE: The script witnesses are carried along with the cardano-api
    -- `anyEraBody`.
    body' <- modifyLedgerTx extraContent body

    if (null existingKeyWits)
       then Right $ Cardano.Tx body' mempty
       else Left $ ErrExistingKeyWitnesses $ length existingKeyWits

modifyLedgerTx
    :: forall era. Cardano.IsShelleyBasedEra era
    => TxUpdate
    -> Cardano.TxBody era
    -> Either ErrUpdateSealedTx (Cardano.TxBody era)
modifyLedgerTx ebc txBody@(Cardano.ShelleyTxBody {}) =
    let Cardano.ShelleyTxBody shelleyEra bod scripts scriptData aux val
            = txBody
    in
    Right $ Cardano.ShelleyTxBody shelleyEra
        (adjustBodyShelley ebc shelleyEra bod)
        scripts
        scriptData
        aux
        val
modifyLedgerTx _ (Byron.ByronTxBody _) =
    case Cardano.shelleyBasedEra @era of {}

-- NOTE: If the ShelleyMA MAClass were exposed, the Allegra and Mary
-- cases could perhaps be joined. It is not however. And we still need
-- to treat Alonzo and Shelley differently.
adjustBodyShelley
    :: TxUpdate
    -> ShelleyBasedEra era
    -> Ledger.TxBody (Cardano.ShelleyLedgerEra era)
    -> Ledger.TxBody (Cardano.ShelleyLedgerEra era)
adjustBodyShelley txUpdate era ledgerBody = case era of
    ShelleyBasedEraAlonzo -> ledgerBody
        { Alonzo.outputs = Alonzo.outputs ledgerBody
            <> StrictSeq.fromList (Cardano.toShelleyTxOut era . Cardano.toCtxUTxOTxOut . toCardanoTxOut era <$> extraOutputs)
        , Alonzo.inputs = Alonzo.inputs ledgerBody
            <> Set.fromList (Cardano.toShelleyTxIn <$> extraInputs')
        , Alonzo.collateral = Alonzo.collateral ledgerBody
            <> Set.fromList (Cardano.toShelleyTxIn <$> extraCollateral')
        , Alonzo.txfee =
            modifyFee $ Alonzo.txfee ledgerBody
        }
    ShelleyBasedEraMary ->
        let
            ShelleyMA.TxBody inputs outputs certs wdrls txfee vldt update adHash mint = ledgerBody
            toTxOut = Cardano.toShelleyTxOut era . Cardano.toCtxUTxOTxOut . toCardanoTxOut era
        in
        ShelleyMA.TxBody
            (inputs
                <> Set.fromList (Cardano.toShelleyTxIn <$> extraInputs'))
            (outputs
                <> StrictSeq.fromList (toTxOut <$> extraOutputs))
            certs
            wdrls
            (modifyFee txfee)
            vldt
            update
            adHash
            mint
    ShelleyBasedEraAllegra ->
        let
            ShelleyMA.TxBody inputs outputs certs wdrls txfee vldt update adHash mint = ledgerBody
            toTxOut = Cardano.toShelleyTxOut era . Cardano.toCtxUTxOTxOut . toCardanoTxOut era
        in
        ShelleyMA.TxBody
            (inputs
                <> Set.fromList (Cardano.toShelleyTxIn <$> extraInputs'))
            (outputs
                <> StrictSeq.fromList (toTxOut <$> extraOutputs))
            certs
            wdrls
            (modifyFee txfee)
            vldt
            update
            adHash
            mint
    ShelleyBasedEraShelley ->
        let
            Shelley.TxBody inputs outputs certs wdrls txfee ttl txUpdate' mdHash = ledgerBody
            toTxOut = Cardano.toShelleyTxOut era . Cardano.toCtxUTxOTxOut . toCardanoTxOut era
        in
        Shelley.TxBody
            (inputs
                <> Set.fromList (Cardano.toShelleyTxIn <$> extraInputs'))
            (outputs
                <> StrictSeq.fromList (toTxOut <$> extraOutputs))
            certs
            wdrls
            (modifyFee txfee)
            ttl
            txUpdate'
            mdHash
  where
    TxUpdate extraInputs extraCollateral extraOutputs feeUpdate
        = txUpdate

    extraInputs' = toCardanoTxIn . fst <$> extraInputs
    extraCollateral' = toCardanoTxIn <$> extraCollateral

    modifyFee old = case feeUpdate of
        UseNewTxFee new -> toLedgerCoin new
        UseOldTxFee -> old
      where
        toLedgerCoin :: Coin -> Ledger.Coin
        toLedgerCoin (Coin c) = Ledger.Coin (intCast c)

-- NOTE / FIXME: This is an 'estimation' because it is actually quite hard to
-- estimate what would be the cost of a selecting a particular input. Indeed, an
-- input may contain any arbitrary assets, which has a direct impact on the
-- shape of change outputs. In practice, this should work out pretty well
-- because of other approximations done along the way which should compensate
-- for possible extra assets in inputs not counted as part of this estimation.
--
-- Worse that may happen here is the wallet generating a transaction that is
-- slightly too big, For a better user experience, we could detect that earlier
-- before submitting the transaction and return a more user-friendly error.
--
-- Or... to be even better, the 'SelectionLimit' from the RoundRobin module
-- could be a function of the 'SelectionState' already selected. With this
-- information and the shape of the requested output, we can get down to a
-- pretty accurate result.
_estimateMaxNumberOfInputs
    :: forall k. TxWitnessTagFor k
    => Quantity "byte" Word16
     -- ^ Transaction max size in bytes
    -> TransactionCtx
     -- ^ An additional transaction context
    -> [TxOut]
     -- ^ A list of outputs being considered.
    -> Int
_estimateMaxNumberOfInputs txMaxSize ctx outs =
    fromIntegral $ findLargestUntil ((> maxSize) . txSizeGivenInputs) 0
  where
    -- | Find the largest amount of inputs that doesn't make the tx too big.
    -- Tries in sequence from 0 and upward (up to 255, but smaller than 50 in
    -- practice because of the max transaction size).
    findLargestUntil :: (Integer -> Bool) -> Integer -> Integer
    findLargestUntil isTxTooLarge inf
        | inf == maxNInps        = maxNInps
        | isTxTooLarge (inf + 1) = inf
        | otherwise              = findLargestUntil isTxTooLarge (inf + 1)

    maxSize  = toInteger (getQuantity txMaxSize)
    maxNInps = 255 -- Arbitrary, but large enough.

    txSizeGivenInputs nInps = fromIntegral size
      where
        TxSize size = estimateTxSize $ mkTxSkeleton
            (txWitnessTagFor @k) ctx sel
        sel  = dummySkeleton (fromIntegral nInps) outs

dummySkeleton :: Int -> [TxOut] -> SelectionSkeleton
dummySkeleton inputCount outputs = SelectionSkeleton
    { skeletonInputCount =
        inputCount
    , skeletonOutputs =
        outputs
    , skeletonChange =
        TokenBundle.getAssets . view #tokens <$> outputs
    }

-- ^ Evaluate a minimal fee amount necessary to pay for a given tx
-- using ledger's functionality
--
-- Will estimate how many witnesses there /should be/, so it works even
-- for unsigned transactions.
_evaluateMinimumFee
    :: Cardano.IsShelleyBasedEra era
    => Cardano.ProtocolParameters
    -> Cardano.Tx era
    -> Coin
_evaluateMinimumFee pp (Cardano.Tx body _) = fromCardanoLovelace $
    Cardano.evaluateTransactionFee pp body nWits 0
  where
    nWits = (estimateNumberOfWitnesses body)

-- | Estimate the size of the transaction (body) when fully signed.
_estimateSignedTxSize
    :: Cardano.IsShelleyBasedEra era
    => Cardano.ProtocolParameters
    -> Cardano.TxBody era
    -> TxSize
_estimateSignedTxSize pparams body =
    let
        nWits :: Word
        nWits = estimateNumberOfWitnesses body

        -- Hack which allows us to rely on the ledger to calculate the size of
        -- witnesses:
        feeOfWits :: Coin
        feeOfWits = minfee nWits `Coin.difference` minfee 0

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

    minfee :: Word -> Coin
    minfee nWits = fromCardanoLovelace $
        Cardano.evaluateTransactionFee pparams body nWits 0

    feePerByte :: Coin
    feePerByte = Coin.fromNatural $
        view #protocolParamTxFeePerByte pparams

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
estimateNumberOfWitnesses
    :: forall era. Cardano.IsShelleyBasedEra era
    => Cardano.TxBody era
    -> Word
estimateNumberOfWitnesses (Cardano.TxBody txbodycontent) =
    let txIns = Cardano.txIns txbodycontent
        txIns' = [ txin | (txin, Cardano.ViewTx) <- txIns ]
        txInsCollateral = Cardano.txInsCollateral txbodycontent
        txIns'' = case txInsCollateral of
            Cardano.TxInsCollateral _ collaterals -> collaterals
            _ -> []
        txInsUnique = L.nub $ txIns' ++ txIns''
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
            Cardano.TxCertificates _ certs _ -> length certs
            -- FIXME [ADP-1515] Not all certificates require witnesses. Will
            -- over-estimate unnecessarily.
    in
    fromIntegral $
        length txInsUnique +
        length txExtraKeyWits' +
        length txWithdrawals' +
        txUpdateProposal' +
        txCerts
  where
    -- Silence warning from redundant @IsShelleyBasedEra@ constraint:
    _ = shelleyBasedEra @era

_maxScriptExecutionCost
    :: ProtocolParameters
    -> [Redeemer]
    -> Coin
_maxScriptExecutionCost pp redeemers
    | not (null redeemers) = case view #executionUnitPrices pp of
        Just prices -> executionCost prices maxExecutionUnits
        Nothing     -> Coin 0
    | otherwise = Coin 0
  where
    maxExecutionUnits :: ExecutionUnits
    maxExecutionUnits = view (#txParameters . #getMaxExecutionUnits) pp

    executionCost :: ExecutionUnitPrices -> ExecutionUnits -> Coin
    executionCost ps us = Coin.fromNatural . ceiling
        $ (ps ^. #pricePerStep)       * toRational (us ^. #executionSteps)
        + (ps ^. #pricePerMemoryUnit) * toRational (us ^. #executionMemory)

type AlonzoTx =
    Ledger.Tx (Cardano.ShelleyLedgerEra Cardano.AlonzoEra)

_assignScriptRedeemers
    :: forall era. Cardano.IsShelleyBasedEra era
    => Cardano.ProtocolParameters
    -> TimeInterpreter (Either PastHorizonException)
    -> (TxIn -> Maybe (TxOut, Maybe (Hash "Datum")))
    -> [Redeemer]
    -> Cardano.Tx era
    -> Either ErrAssignRedeemers (Cardano.Tx era )
_assignScriptRedeemers (toAlonzoPParams -> pparams) ti resolveInput redeemers tx =
    case Cardano.shelleyBasedEra @era of
        Cardano.ShelleyBasedEraShelley ->
            pure tx
        Cardano.ShelleyBasedEraAllegra ->
            pure tx
        Cardano.ShelleyBasedEraMary->
            pure tx
        Cardano.ShelleyBasedEraAlonzo -> do
            let Cardano.ShelleyTx _ alonzoTx = tx
            alonzoTx' <- flip execStateT alonzoTx $ do
                indexedRedeemers <- StateT assignNullRedeemers
                executionUnits <- get
                    >>= lift . evaluateExecutionUnits indexedRedeemers
                modifyM (assignExecutionUnits executionUnits)
                modify' addScriptIntegrityHash
            pure $ Cardano.ShelleyTx ShelleyBasedEraAlonzo alonzoTx'
  where
    -- | Assign redeemers with null execution units to the input transaction.
    --
    -- Redeemers are determined from the context given to the caller via the
    -- 'Redeemer' type which is mapped to an 'Alonzo.ScriptPurpose'.
    assignNullRedeemers
        :: AlonzoTx
        -> Either ErrAssignRedeemers (Map Alonzo.RdmrPtr Redeemer, AlonzoTx)
    assignNullRedeemers alonzoTx = do
        (indexedRedeemers, nullRedeemers) <- fmap unzip $ forM redeemers $ \rd -> do
            ptr <- case Alonzo.rdptr (Alonzo.body alonzoTx) (toScriptPurpose rd) of
                SNothing ->
                    Left $ ErrAssignRedeemersTargetNotFound rd
                SJust ptr ->
                    pure ptr

            rData <- case deserialiseOrFail (BL.fromStrict $ redeemerData rd) of
                Left e ->
                    Left $ ErrAssignRedeemersInvalidData rd (show e)
                Right d ->
                    pure (Alonzo.Data d)

            pure ((ptr, rd), (ptr, (rData, mempty)))

        pure
            ( Map.fromList indexedRedeemers
            , alonzoTx
                { Alonzo.wits = (Alonzo.wits alonzoTx)
                    { Alonzo.txrdmrs = Alonzo.Redeemers (Map.fromList nullRedeemers)
                    }
                }
            )

    utxoFromAlonzoTx
        :: AlonzoTx
        -> Ledger.UTxO (Cardano.ShelleyLedgerEra Cardano.AlonzoEra)
    utxoFromAlonzoTx alonzoTx =
        let
            inputs = Alonzo.inputs (Alonzo.body alonzoTx)
            utxo = flip mapMaybe (F.toList inputs) $ \i -> do
                (o, dt) <- resolveInput (fromShelleyTxIn i)
                -- NOTE: 'toAlonzoTxOut' only partially represent the information
                -- because the wallet internal types aren't capturing datum at
                -- the moment. It is _okay_ in this context because the
                -- resulting UTxO is only used by 'evaluateTransactionExecutionUnits'
                -- to lookup addresses corresponding to inputs.
                pure (i, toAlonzoTxOut o dt)
         in
            Ledger.UTxO (Map.fromList utxo)

    -- | Evaluate execution units of each script/redeemer in the transaction.
    -- This may fail for each script.
    evaluateExecutionUnits
        :: Map Alonzo.RdmrPtr Redeemer
        -> AlonzoTx
        -> Either ErrAssignRedeemers
            (Map Alonzo.RdmrPtr (Either ErrAssignRedeemers Alonzo.ExUnits))
    evaluateExecutionUnits indexedRedeemers alonzoTx = do
        let utxo = utxoFromAlonzoTx alonzoTx
        let costs = toCostModelsAsArray (Alonzo._costmdls pparams)
        let systemStart = getSystemStart ti

        epochInfo <- hoistEpochInfo (left ErrAssignRedeemersPastHorizon . runIdentity . runExceptT)
            <$> left ErrAssignRedeemersPastHorizon (toEpochInfo ti)

        res <- evaluateTransactionExecutionUnits
                pparams
                alonzoTx
                utxo
                epochInfo
                systemStart
                costs
        case res of
            Left (UnknownTxIns ins) ->
                Left $ ErrAssignRedeemersUnresolvedTxIns $ map fromShelleyTxIn (F.toList ins)
            Right report ->
                Right $ hoistScriptFailure report
      where
        hoistScriptFailure
            :: Show scriptFailure
            => Map Alonzo.RdmrPtr (Either scriptFailure a)
            -> Map Alonzo.RdmrPtr (Either ErrAssignRedeemers a)
        hoistScriptFailure = Map.mapWithKey $ \ptr -> left $ \e ->
            ErrAssignRedeemersScriptFailure (indexedRedeemers ! ptr) (show e)

    -- | Change execution units for each redeemers in the transaction to what
    -- they ought to be.
    assignExecutionUnits
        :: Map Alonzo.RdmrPtr (Either ErrAssignRedeemers Alonzo.ExUnits)
        -> AlonzoTx
        -> Either ErrAssignRedeemers AlonzoTx
    assignExecutionUnits exUnits alonzoTx = do
        let wits = Alonzo.wits alonzoTx
        let Alonzo.Redeemers rdmrs = Alonzo.txrdmrs wits
        rdmrs' <- Map.mergeA
            Map.preserveMissing
            Map.dropMissing
            (Map.zipWithAMatched (const assignUnits))
            rdmrs
            exUnits
        pure $ alonzoTx
            { Alonzo.wits = wits
                { Alonzo.txrdmrs = Alonzo.Redeemers rdmrs'
                }
            }
      where
        assignUnits
            :: (dat, Alonzo.ExUnits)
            -> Either err Alonzo.ExUnits
            -> Either err (dat, Alonzo.ExUnits)
        assignUnits (dats, _zero) =
            fmap (dats,)

    -- | Finally, calculate and add the script integrity hash with the new
    -- final redeemers, if any.
    addScriptIntegrityHash
        :: forall e. (e ~ Cardano.ShelleyLedgerEra Cardano.AlonzoEra)
        => AlonzoTx
        -> AlonzoTx
    addScriptIntegrityHash alonzoTx =
        let
            wits  = Alonzo.wits alonzoTx
            langs =
                [ l
                | (_hash, script) <- Map.toList (Alonzo.txscripts wits)
                , (not . isNativeScript @e) script
                , Just l <- [Alonzo.language script]
                ]
         in
            alonzoTx
                { Alonzo.body = (Alonzo.body alonzoTx)
                    { Alonzo.scriptIntegrityHash = Alonzo.hashScriptIntegrity
                        pparams
                        (Set.fromList langs)
                        (Alonzo.txrdmrs wits)
                        (Alonzo.txdats wits)
                    }
                }

txConstraints :: ProtocolParameters -> TxWitnessTag -> TxConstraints
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
    , txRewardWithdrawalCost
    , txRewardWithdrawalSize
    , txMaximumSize
    }
  where
    txBaseCost =
        estimateTxCost protocolParams empty

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
        computeMinimumAdaQuantity (minimumUTxOvalue protocolParams)

    txRewardWithdrawalCost c =
        marginalCostOf empty {txRewardWithdrawal = c}

    txRewardWithdrawalSize c =
        marginalSizeOf empty {txRewardWithdrawal = c}

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
    marginalCostOf =
        Coin.distance txBaseCost . estimateTxCost protocolParams

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
    { txMetadata :: !(Maybe TxMetadata)
    , txDelegationAction :: !(Maybe DelegationAction)
    , txRewardWithdrawal :: !Coin
    , txWitnessTag :: !TxWitnessTag
    , txInputCount :: !Int
    , txOutputs :: ![TxOut]
    , txChange :: ![Set AssetId]
    , txScripts :: [Script KeyHash]
    , txAssetsToMintOrBurn :: Set AssetId
    -- ^ The set of assets to mint or burn.
    , txScriptExecutionCost :: !Coin
    }
    deriving (Eq, Show, Generic)

-- | Constructs an empty transaction skeleton.
--
-- This may be used to estimate the size and cost of an empty transaction.
--
emptyTxSkeleton :: TxWitnessTag -> TxSkeleton
emptyTxSkeleton txWitnessTag = TxSkeleton
    { txMetadata = Nothing
    , txDelegationAction = Nothing
    , txRewardWithdrawal = Coin 0
    , txWitnessTag
    , txInputCount = 0
    , txOutputs = []
    , txChange = []
    , txScripts = []
    , txAssetsToMintOrBurn = Set.empty
    , txScriptExecutionCost = Coin 0
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
    { txMetadata = view #txMetadata context
    , txDelegationAction = view #txDelegationAction context
    , txRewardWithdrawal = withdrawalToCoin $ view #txWithdrawal context
    , txWitnessTag = witness
    , txInputCount = view #skeletonInputCount skeleton
    , txOutputs = view #skeletonOutputs skeleton
    , txChange = view #skeletonChange skeleton
    , txScripts = (<>)
        (Map.elems (snd $ view #txAssetsToMint context))
        (Map.elems (snd $ view #txAssetsToBurn context))
    , txAssetsToMintOrBurn = (<>)
        (TokenMap.getAssets (fst $ view #txAssetsToMint context))
        (TokenMap.getAssets (fst $ view #txAssetsToBurn context))
    , txScriptExecutionCost = view #txPlutusScriptExecutionCost context
    }

-- | Estimates the final cost of a transaction based on its skeleton.
--
estimateTxCost :: ProtocolParameters -> TxSkeleton -> Coin
estimateTxCost pp skeleton =
    F.fold
        [ computeFee (estimateTxSize skeleton)
        , view #txScriptExecutionCost skeleton
        ]
  where
    computeFee :: TxSize -> Coin
    computeFee (TxSize size) =
        let LinearFee LinearFunction {..} = getFeePolicy $ txParameters pp
        in Coin $ ceiling $ intercept + slope * fromIntegral size

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
    :: FeePolicy
    -> Coin -- ^ Original coin
    -> Coin -- ^ Increment
    -> Coin
costOfIncreasingCoin (LinearFee fee) from delta =
    costOfCoin (from <> delta) `Coin.difference` costOfCoin from
  where
    perByte = ceiling $ slope fee
    costOfCoin = Coin . (perByte *) . unTxSize . sizeOfCoin

-- The maximum cost increase 'costOfIncreasingCoin' can return, which is the
-- cost of 8 bytes.
maximumCostOfIncreasingCoin :: FeePolicy -> Coin
maximumCostOfIncreasingCoin (LinearFee fee) = Coin $ ceiling $ 8 * perByte
  where
    perByte = slope fee

-- | Calculate the size of a coin when encoded as CBOR.
sizeOfCoin :: Coin -> TxSize
sizeOfCoin (Coin c)
    | c >= 4_294_967_296 = TxSize 9 -- c >= 2^32
    | c >=        65_536 = TxSize 5 -- c >= 2^16
    | c >=           256 = TxSize 3 -- c >= 2^ 8
    | c >=            24 = TxSize 2
    | otherwise          = TxSize 1

-- | Distributes a surplus transaction balance between the given change outputs
--   and the given fee.
--
-- See documentation for 'TransactionLayer.distributeSurplus' for more details.
--
_distributeSurplus
    :: FeePolicy
    -> Coin
    -- ^ Surplus transaction balance to distribute.
    -> TxFeeAndChange [TxOut]
    -- ^ Original fee and change outputs.
    -> Either ErrMoreSurplusNeeded (TxFeeAndChange [TxOut])
    -- ^ Adjusted fee and change outputs.
_distributeSurplus feePolicy surplus fc@(TxFeeAndChange fee change) =
    distributeSurplusDelta feePolicy surplus
        (mapTxFeeAndChange id (fmap txOutCoin) fc)
    <&> mapTxFeeAndChange
        (fee <>)
        (zipWith (flip txOutAddCoin) change)

distributeSurplusDelta
    :: FeePolicy
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
    :: FeePolicy
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
    :: FeePolicy
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
estimateTxSize :: TxSkeleton -> TxSize
estimateTxSize skeleton =
    TxSize $ fromIntegral sizeOf_Transaction
  where
    TxSkeleton
        { txMetadata
        , txDelegationAction
        , txRewardWithdrawal
        , txWitnessTag
        , txInputCount
        , txOutputs
        , txChange
        , txScripts
        , txAssetsToMintOrBurn
        } = skeleton

    numberOf_Inputs
        = fromIntegral txInputCount

    numberOf_CertificateSignatures
        = maybe 0 (const 1) txDelegationAction

    numberOf_Withdrawals
        = if txRewardWithdrawal > Coin 0 then 1 else 0

    -- Total number of signatures the scripts require
    numberOf_ScriptVkeyWitnesses
        = sumVia scriptRequiredKeySigs txScripts

    scriptRequiredKeySigs :: Num num => Script KeyHash -> num
    scriptRequiredKeySigs = \case
        RequireSignatureOf _ ->
            1
        RequireAllOf ss ->
            sumVia scriptRequiredKeySigs ss
        RequireAnyOf ss ->
            sumVia scriptRequiredKeySigs ss
        ActiveFromSlot _ ->
            0
        ActiveUntilSlot _ ->
            0
        RequireSomeOf _ ss ->
            -- We don't know how many the user will sign with, so we just assume
            -- the worst case of "signs with all".
            sumVia scriptRequiredKeySigs ss

    numberOf_VkeyWitnesses
        = case txWitnessTag of
            TxWitnessByronUTxO{} -> 0
            TxWitnessShelleyUTxO ->
                numberOf_Inputs
                + numberOf_Withdrawals
                + numberOf_CertificateSignatures
                + numberOf_ScriptVkeyWitnesses

    numberOf_BootstrapWitnesses
        = case txWitnessTag of
            TxWitnessByronUTxO{} -> numberOf_Inputs
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
        + sizeOf_Metadata

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
        + sizeOf_Certificates
        + sizeOf_Withdrawals
        + sizeOf_Update
        + sizeOf_MetadataHash
        + sizeOf_ValidityIntervalStart
        + sumVia sizeOf_Mint (F.toList txAssetsToMintOrBurn)
      where
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

        -- ?4 => [* certificates ]
        sizeOf_Certificates
            = case txDelegationAction of
                Nothing ->
                    0
                Just RegisterKeyAndJoin{} ->
                    sizeOf_SmallUInt
                    + sizeOf_SmallArray
                    + sizeOf_StakeRegistration
                    + sizeOf_StakeDelegation
                Just Join{} ->
                    sizeOf_SmallUInt
                    + sizeOf_SmallArray
                    + sizeOf_StakeDelegation
                Just Quit{} ->
                    sizeOf_SmallUInt
                    + sizeOf_SmallArray
                    + sizeOf_StakeDeregistration

        -- ?5 => withdrawals
        sizeOf_Withdrawals
            = (if numberOf_Withdrawals > 0
                then sizeOf_SmallUInt + sizeOf_SmallMap
                else 0)
            + sizeOf_Withdrawal * numberOf_Withdrawals

        -- ?6 => update
        sizeOf_Update
            = 0 -- Assuming no updates is running through cardano-wallet

        -- ?7 => metadata_hash
        sizeOf_MetadataHash
            = maybe 0 (const (sizeOf_SmallUInt + sizeOf_Hash32)) txMetadata

        -- ?8 => uint ; validity interval start
        sizeOf_ValidityIntervalStart
            = sizeOf_UInt

        -- ?9 => mint = multiasset<int64>
        -- mint = multiasset<int64>
        sizeOf_Mint AssetId{tokenName}
          = sizeOf_MultiAsset sizeOf_Int64 tokenName

    -- For metadata, we can't choose a reasonable upper bound, so it's easier to
    -- measure the serialize data since we have it anyway. When it's "empty",
    -- metadata are represented by a special "null byte" in CBOR `F6`.
    sizeOf_Metadata
        = maybe 1 (toInteger . BS.length . serialiseToCBOR) txMetadata

    -- transaction_input =
    --   [ transaction_id : $hash32
    --   , index : uint
    --   ]
    sizeOf_Input
        = sizeOf_SmallArray
        + sizeOf_Hash32
        + sizeOf_UInt

    -- transaction_output =
    --   [address, amount : value]
    -- value =
    --   coin / [coin,multiasset<uint>]
    sizeOf_Output TxOut {address, tokens}
        = sizeOf_SmallArray
        + sizeOf_Address address
        + sizeOf_SmallArray
        + sizeOf_Coin (TokenBundle.getCoin tokens)
        + sumVia sizeOf_NativeAsset (TokenBundle.getAssets tokens)

    -- transaction_output =
    --   [address, amount : value]
    -- value =
    --   coin / [coin,multiasset<uint>]
    sizeOf_ChangeOutput xs
        = sizeOf_SmallArray
        + sizeOf_ChangeAddress
        + sizeOf_SmallArray
        + sizeOf_LargeUInt
        + sumVia sizeOf_NativeAsset xs

    -- stake_registration =
    --   (0, stake_credential)
    sizeOf_StakeRegistration
        = sizeOf_SmallArray
        + sizeOf_SmallUInt
        + sizeOf_StakeCredential

    -- stake_deregistration =
    --   (1, stake_credential)
    sizeOf_StakeDeregistration
        = sizeOf_StakeRegistration

    -- stake_delegation =
    --   (2, stake_credential, pool_keyhash)
    sizeOf_StakeDelegation
        = sizeOf_SmallArray
        + sizeOf_SmallUInt
        + sizeOf_StakeCredential
        + sizeOf_Hash28

    -- stake_credential =
    --   [  0, addr_keyhash
    --   // 1, scripthash
    --   ]
    sizeOf_StakeCredential
        = sizeOf_SmallArray
        + sizeOf_SmallUInt
        + sizeOf_Hash28

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
            TxWitnessByronUTxO{} -> 85
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

    -- withdrawals =
    --   { * reward_account => coin }
    sizeOf_Withdrawal
        = sizeOf_Hash28
        + sizeOf_LargeUInt

    -- transaction_witness_set =
    --   { ?0 => [* vkeywitness ]
    --   , ?1 => [* multisig_script ]
    --   , ?2 => [* bootstrap_witness ]
    --   }
    sizeOf_WitnessSet
        = sizeOf_SmallMap
        + sizeOf_VKeyWitnesses
        + sizeOf_NativeScripts txScripts
        + sizeOf_BootstrapWitnesses
      where
        -- ?0 => [* vkeywitness ]
        sizeOf_VKeyWitnesses
            = (if numberOf_VkeyWitnesses > 0
                then sizeOf_Array + sizeOf_SmallUInt else 0)
            + sizeOf_VKeyWitness * numberOf_VkeyWitnesses

        -- ?1 => [* native_script ]
        sizeOf_NativeScripts []
            = 0
        sizeOf_NativeScripts ss
            = sizeOf_Array
            + sizeOf_SmallUInt
            + sumVia sizeOf_NativeScript ss

        -- ?2 => [* bootstrap_witness ]
        sizeOf_BootstrapWitnesses
            = (if numberOf_BootstrapWitnesses > 0
                then sizeOf_Array + sizeOf_SmallUInt
                else 0)
            + sizeOf_BootstrapWitness * numberOf_BootstrapWitnesses

    -- vkeywitness =
    --  [ $vkey
    --  , $signature
    --  ]
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
    sizeOf_Hash28
        = 30

    -- A Blake2b-256 hash, resulting in a 32-byte digest wrapped in CBOR, so
    -- with 2 bytes overhead (length <255, but length > 23)
    sizeOf_Hash32
        = 34

    -- A 32-byte Ed25519 public key, encoded as a CBOR-bytestring so with 2
    -- bytes overhead (length < 255, but length > 23)
    sizeOf_VKey
        = 34

    -- A 64-byte Ed25519 signature, encoded as a CBOR-bytestring so with 2
    -- bytes overhead (length < 255, but length > 23)
    sizeOf_Signature
        = 66

    -- A CBOR UInt which is less than 23 in value fits on a single byte. Beyond,
    -- the first byte is used to encode the number of bytes necessary to encode
    -- the number itself, followed by the number itself.
    --
    -- When considering a 'UInt', we consider the worst case scenario only where
    -- the uint is encoded over 4 bytes, so up to 2^32 which is fine for most
    -- cases but coin values.
    sizeOf_SmallUInt = 1
    sizeOf_UInt = 5
    sizeOf_LargeUInt = 9

    -- A CBOR Int which is less than 23 in value fits on a single byte. Beyond,
    -- the first byte is used to encode the number of bytes necessary to encode
    -- the number, followed by the number itself. In this case, 8 bytes are used
    -- to encode an int64, plus one byte to encode the number of bytes
    -- necessary.
    sizeOf_Int64 = 9

    -- A CBOR array with less than 23 elements, fits on a single byte, followed
    -- by each key-value pair (encoded as two concatenated CBOR elements).
    sizeOf_SmallMap = 1

    -- A CBOR array with less than 23 elements, fits on a single byte, followed
    -- by each elements. Otherwise, the length of the array is encoded first,
    -- very much like for UInt.
    --
    -- When considering an 'Array', we consider large scenarios where arrays can
    -- have up to 65536 elements.
    sizeOf_SmallArray = 1
    sizeOf_Array = 3

-- Small helper function for summing values. Given a list of values, get the sum
-- of the values, after the given function has been applied to each value.
sumVia :: (Foldable t, Num m) => (a -> m) -> t a -> m
sumVia f = F.foldl' (\t -> (t +) . f) 0

withShelleyBasedEra
    :: forall a. ()
    => AnyCardanoEra
    -> (forall era. EraConstraints era => ShelleyBasedEra era -> Either ErrMkTransaction a)
    -> Either ErrMkTransaction a
withShelleyBasedEra era fn = case era of
    AnyCardanoEra ByronEra    -> Left $ ErrMkTransactionInvalidEra era
    AnyCardanoEra ShelleyEra  -> fn ShelleyBasedEraShelley
    AnyCardanoEra AllegraEra  -> fn ShelleyBasedEraAllegra
    AnyCardanoEra MaryEra     -> fn ShelleyBasedEraMary
    AnyCardanoEra AlonzoEra   -> fn ShelleyBasedEraAlonzo

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
    -> Cardano.SlotNo
    -> SelectionOf TxOut
    -> Maybe Cardano.TxMetadata
    -> [(Cardano.StakeAddress, Cardano.Lovelace)]
    -> [Cardano.Certificate]
    -> Cardano.Lovelace
    -> TokenMap
    -> TokenMap
    -> Map AssetId (Script KeyHash)
    -> Either ErrMkTransaction (Cardano.TxBody era)
mkUnsignedTx era ttl cs md wdrls certs fees mintData burnData allScripts =
    left toErrMkTx $ Cardano.makeTransactionBody $ Cardano.TxBodyContent
    { Cardano.txIns =
        (,Cardano.BuildTxWith (Cardano.KeyWitness Cardano.KeyWitnessForSpending))
        . toCardanoTxIn
        . fst <$> F.toList (view #inputs cs)

    , Cardano.txOuts =
        toCardanoTxOut era <$> view #outputs cs ++ F.toList (view #change cs)

    , Cardano.txWithdrawals =
        let
            wit = Cardano.BuildTxWith
                $ Cardano.KeyWitness Cardano.KeyWitnessForStakeAddr
        in
            Cardano.TxWithdrawals wdrlsSupported
                (map (\(key, coin) -> (key, coin, wit)) wdrls)

    , txInsCollateral =
        -- TODO: [ADP-957] Support collateral.
        Cardano.TxInsCollateralNone

    , txProtocolParams =
        -- TODO: [ADP-1058] We presumably need to provide the protocol params if
        -- our tx uses scripts?
        Cardano.BuildTxWith Nothing

    , txScriptValidity =
        Cardano.TxScriptValidityNone

    , txExtraKeyWits = Cardano.TxExtraKeyWitnessesNone

    , Cardano.txCertificates =
        let
            -- It seems that passing Map.empty here works just fine.
            witMap = Map.empty
            ctx = Cardano.BuildTxWith witMap
        in
            Cardano.TxCertificates certSupported certs ctx

    , Cardano.txFee = explicitFees era fees

    , Cardano.txValidityRange =
        ( Cardano.TxValidityNoLowerBound
        , Cardano.TxValidityUpperBound txValidityUpperBoundSupported ttl
        )

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
                    toScriptWitness script =
                        Cardano.SimpleScriptWitness scriptWitsSupported
                        Cardano.SimpleScriptV2 (toCardanoSimpleScript script)
                    witMap =
                        Map.map toScriptWitness $
                        Map.mapKeys (toCardanoPolicyId . TokenMap.tokenPolicyId)
                        allScripts
                    ctx = Cardano.BuildTxWith witMap
                in Cardano.TxMintValue mintedEra (mintValue <> burnValue) ctx
    }
  where
    toErrMkTx :: Cardano.TxBodyError -> ErrMkTransaction
    toErrMkTx = ErrMkTransactionTxBodyError . T.pack . Cardano.displayError

    metadataSupported :: Cardano.TxMetadataSupportedInEra era
    metadataSupported = case era of
        ShelleyBasedEraShelley -> Cardano.TxMetadataInShelleyEra
        ShelleyBasedEraAllegra -> Cardano.TxMetadataInAllegraEra
        ShelleyBasedEraMary -> Cardano.TxMetadataInMaryEra
        ShelleyBasedEraAlonzo -> Cardano.TxMetadataInAlonzoEra

    certSupported :: Cardano.CertificatesSupportedInEra era
    certSupported = case era of
        ShelleyBasedEraShelley -> Cardano.CertificatesInShelleyEra
        ShelleyBasedEraAllegra -> Cardano.CertificatesInAllegraEra
        ShelleyBasedEraMary    -> Cardano.CertificatesInMaryEra
        ShelleyBasedEraAlonzo -> Cardano.CertificatesInAlonzoEra

    wdrlsSupported :: Cardano.WithdrawalsSupportedInEra era
    wdrlsSupported = case era of
        ShelleyBasedEraShelley -> Cardano.WithdrawalsInShelleyEra
        ShelleyBasedEraAllegra -> Cardano.WithdrawalsInAllegraEra
        ShelleyBasedEraMary    -> Cardano.WithdrawalsInMaryEra
        ShelleyBasedEraAlonzo -> Cardano.WithdrawalsInAlonzoEra

    txValidityUpperBoundSupported :: Cardano.ValidityUpperBoundSupportedInEra era
    txValidityUpperBoundSupported = case era of
        ShelleyBasedEraShelley -> Cardano.ValidityUpperBoundInShelleyEra
        ShelleyBasedEraAllegra -> Cardano.ValidityUpperBoundInAllegraEra
        ShelleyBasedEraMary -> Cardano.ValidityUpperBoundInMaryEra
        ShelleyBasedEraAlonzo -> Cardano.ValidityUpperBoundInAlonzoEra

    txMintingSupported :: Maybe (Cardano.MultiAssetSupportedInEra era)
    txMintingSupported = case era of
        ShelleyBasedEraShelley -> Nothing
        ShelleyBasedEraAllegra -> Nothing
        ShelleyBasedEraMary -> Just Cardano.MultiAssetInMaryEra
        ShelleyBasedEraAlonzo -> Just Cardano.MultiAssetInAlonzoEra

    scriptWitsSupported
        :: Cardano.ScriptLanguageInEra Cardano.SimpleScriptV2 era
    scriptWitsSupported = case era of
        ShelleyBasedEraShelley -> internalError
            "scriptWitsSupported: we should be at least in Mary"
        ShelleyBasedEraAllegra -> internalError
            "scriptWitsSupported: we should be at least in Mary"
        ShelleyBasedEraMary -> Cardano.SimpleScriptV2InMary
        ShelleyBasedEraAlonzo -> Cardano.SimpleScriptV2InAlonzo

mkWithdrawals
    :: NetworkId
    -> RewardAccount
    -> Coin
    -> [(Cardano.StakeAddress, Cardano.Lovelace)]
mkWithdrawals networkId acc amount
    | amount == Coin 0 = mempty
    | otherwise = [ (stakeAddress, toCardanoLovelace amount) ]
  where
    cred = toCardanoStakeCredential acc
    stakeAddress = Cardano.makeStakeAddress networkId cred

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
    ShelleyBasedEraShelley -> Cardano.TxFeeExplicit Cardano.TxFeesExplicitInShelleyEra
    ShelleyBasedEraAllegra -> Cardano.TxFeeExplicit Cardano.TxFeesExplicitInAllegraEra
    ShelleyBasedEraMary    -> Cardano.TxFeeExplicit Cardano.TxFeesExplicitInMaryEra
    ShelleyBasedEraAlonzo -> Cardano.TxFeeExplicit Cardano.TxFeesExplicitInAlonzoEra
