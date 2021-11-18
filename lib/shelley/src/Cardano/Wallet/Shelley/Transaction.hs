{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
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
    , estimateTxCost
    , estimateTxSize
    , mkByronWitness
    , mkShelleyWitness
    , mkTx
    , mkTxSkeleton
    , mkUnsignedTx
    , txConstraints
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPrv, toXPub )
import Cardano.Address.Script
    ( KeyHash, Script (..) )
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
    ( evaluateTransactionExecutionUnits )
import Cardano.Ledger.Crypto
    ( DSIGN )
import Cardano.Ledger.Era
    ( Crypto, Era, ValidateScript (..) )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..), Passphrase (..), RewardAccount (..), WalletKey (..) )
import Cardano.Wallet.Primitive.AddressDerivation.Byron
    ( ByronKey )
import Cardano.Wallet.Primitive.AddressDerivation.Icarus
    ( IcarusKey )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( ShelleyKey, toRewardAccountRaw )
import Cardano.Wallet.Primitive.CoinSelection
    ( SelectionOf (..), selectionDelta )
import Cardano.Wallet.Primitive.CoinSelection.Balance
    ( SelectionLimitOf (..), SelectionSkeleton (..) )
import Cardano.Wallet.Primitive.Slotting
    ( PastHorizonException, TimeInterpreter, getSystemStart, toEpochInfo )
import Cardano.Wallet.Primitive.Types
    ( Certificates
    , ExecutionUnitPrices (..)
    , ExecutionUnits (..)
    , FeePolicy (..)
    , ProtocolParameters (..)
    , TxParameters (..)
    )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..) )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash )
import Cardano.Wallet.Primitive.Types.Redeemer
    ( Redeemer, redeemerData )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle )
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
    , getSealedTxBody
    , sealedTxFromCardano'
    , sealedTxFromCardanoBody
    , txOutCoin
    , txSizeDistance
    )
import Cardano.Wallet.Shelley.Compatibility
    ( fromCardanoAddress
    , fromCardanoLovelace
    , fromCardanoTx
    , fromCardanoTxIn
    , fromCardanoWdrls
    , fromShelleyTxIn
    , toAlonzoPParams
    , toCardanoLovelace
    , toCardanoStakeCredential
    , toCardanoTxIn
    , toCardanoTxOut
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
    ( DelegationAction (..)
    , ErrAssignRedeemers (..)
    , ErrMkTransaction (..)
    , ErrUpdateSealedTx (..)
    , TransactionCtx (..)
    , TransactionLayer (..)
    , TxUpdate (..)
    , withdrawalToCoin
    )
import Cardano.Wallet.Util
    ( modifyM )
import Codec.Serialise
    ( deserialiseOrFail )
import Control.Arrow
    ( left, second )
import Control.Monad
    ( forM, guard, unless )
import Control.Monad.Trans.Class
    ( lift )
import Control.Monad.Trans.Except
    ( runExceptT )
import Control.Monad.Trans.State.Strict
    ( StateT (..), execStateT, get, modify' )
import Data.Function
    ( (&) )
import Data.Functor
    ( ($>) )
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
    ( mapMaybe )
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
import Ouroboros.Network.Block
    ( SlotNo )
import Shelley.Spec.Ledger.API
    ( StrictMaybe (..) )

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
import qualified Shelley.Spec.Ledger.Address.Bootstrap as SL
import qualified Shelley.Spec.Ledger.Tx as Shelley
import qualified Shelley.Spec.Ledger.UTxO as Ledger

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
      -- ^ Create payload-specific witesses given the unsigned transaction body.
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
    -> ShelleyBasedEra era
    -> Either ErrMkTransaction SealedTx
constructUnsignedTx networkId (md, certs) ttl rewardAcnt wdrl cs fee era =
    sealedTxFromCardanoBody <$> tx
  where
    tx = mkUnsignedTx era ttl cs md wdrls certs (toCardanoLovelace fee)
    wdrls = mkWithdrawals networkId rewardAcnt wdrl

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
    let signed = signTransaction networkId acctResolver addrResolver inputResolver
            (unsigned, mkExtraWits unsigned)

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
    -- ^ Network identifer (e.g. mainnet, testnet)
    -> (RewardAccount -> Maybe (XPrv, Passphrase "encryption"))
    -- ^ Stake key store / reward account resolution
    -> (Address -> Maybe (k 'AddressK XPrv, Passphrase "encryption"))
    -- ^ Payment key store
    -> (TxIn -> Maybe Address)
    -- ^ Input resolver
    -> (Cardano.TxBody era, [Cardano.KeyWitness era])
    -- ^ The transaction to sign, possibly with already some existing witnesses
    -> Cardano.Tx era
signTransaction networkId resolveRewardAcct resolveAddress resolveInput (body, wits) =
    Cardano.makeSignedTransaction wits' body
 where
    wits' = mconcat
        [ wits
        , mapMaybe mkTxInWitness  inputs
        , mapMaybe mkTxInWitness  collaterals
        , mapMaybe mkWdrlWitness  wdrls
        , mapMaybe mkExtraWitness extraKeys
        -- TODO: delegation certificates & key-deregistrations
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

    mkTxInWitness :: TxIn -> Maybe (Cardano.KeyWitness era)
    mkTxInWitness i = do
        addr <- resolveInput i
        (k, pwd) <- resolveAddress addr
        pure $ case (txWitnessTagFor @k) of
            TxWitnessShelleyUTxO ->
                mkShelleyWitness body (getRawKey k, pwd)
            TxWitnessByronUTxO{} ->
                mkByronWitness body networkId addr (getRawKey k, pwd)

    mkWdrlWitness :: RewardAccount -> Maybe (Cardano.KeyWitness era)
    mkWdrlWitness a = do
        mkShelleyWitness body <$> resolveRewardAcct a

    mkExtraWitness :: Cardano.Hash Cardano.PaymentKey -> Maybe (Cardano.KeyWitness era)
    mkExtraWitness vkh = do
        -- NOTE: We cannot resolve key hashes directly, so create a one-time
        -- temporary address with that key hash which is fine to lookup via the
        -- address lookup provided above. It works _fine_ because the discovery
        -- of addresses is done properly based on the address constituants (i.e.
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
                    let mkWits unsigned =
                            [ mkShelleyWitness unsigned stakeCreds
                            ]
                    let payload = TxPayload (view #txMetadata ctx) certs mkWits
                    mkTx networkId payload ttl stakeCreds keystore wdrl
                        selection delta

    , addVkWitnesses = \_era stakeCreds addressResolver inputResolver sealedTx -> do
        let acctResolver :: RewardAccount -> Maybe (XPrv, Passphrase "encryption")
            acctResolver acct = do
                let acct' = toRewardAccountRaw $ toXPub $ fst stakeCreds
                guard (acct == acct') $> stakeCreds

        case cardanoTx sealedTx of
            InAnyCardanoEra ByronEra _ ->
                sealedTx
            InAnyCardanoEra ShelleyEra (Cardano.Tx body wits) ->
                signTransaction networkId acctResolver addressResolver inputResolver (body, wits)
                & sealedTxFromCardano'
            InAnyCardanoEra AllegraEra (Cardano.Tx body wits) ->
                signTransaction networkId acctResolver addressResolver inputResolver (body, wits)
                & sealedTxFromCardano'
            InAnyCardanoEra MaryEra (Cardano.Tx body wits) ->
                signTransaction networkId acctResolver addressResolver inputResolver (body, wits)
                & sealedTxFromCardano'
            InAnyCardanoEra AlonzoEra (Cardano.Tx body wits) ->
                signTransaction networkId acctResolver addressResolver inputResolver (body, wits)
                & sealedTxFromCardano'

    , mkUnsignedTransaction = \era stakeXPub _pp ctx selection -> do
        let ttl   = txTimeToLive ctx
        let wdrl  = withdrawalToCoin $ view #txWithdrawal ctx
        let delta = selectionDelta txOutCoin selection
        let rewardAcct = toRewardAccountRaw stakeXPub
        case view #txDelegationAction ctx of
            Nothing -> do
                withShelleyBasedEra era $ do
                    let md = view #txMetadata ctx
                    constructUnsignedTx networkId (md, []) ttl rewardAcct wdrl
                        selection delta

            Just action -> do
                withShelleyBasedEra era $ do
                    let certs = mkDelegationCertificates action stakeXPub
                    let payload = (view #txMetadata ctx, certs)
                    constructUnsignedTx networkId payload ttl rewardAcct wdrl
                        selection delta

    , calcMinimumCost = \pp ctx skeleton ->
        estimateTxCost pp (mkTxSkeleton (txWitnessTagFor @k) ctx skeleton)
        <>
        txFeePadding ctx

    , maxScriptExecutionCost =
        _maxScriptExecutionCost

    , assignScriptRedeemers =
        _assignScriptRedeemers

    , evaluateMinimumFee =
        _evaluateMinimumFee

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

_decodeSealedTx :: SealedTx -> (Tx, TokenMap, TokenMap, Certificates)
_decodeSealedTx (cardanoTx -> InAnyCardanoEra _era tx) = fromCardanoTx tx

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
noTxUpdate = TxUpdate [] [] [] id

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
    :: SealedTx
    -> TxUpdate
    -> Either ErrUpdateSealedTx SealedTx
updateSealedTx (cardanoTx -> InAnyCardanoEra _era tx) extraContent = do
    -- NOTE: The script witnesses are carried along with the cardano-api
    -- `anyEraBody`.
    let (Cardano.Tx anyEraBody existingKeyWits) = tx
    body' <- modifyLedgerTx extraContent anyEraBody

    unless (null existingKeyWits) $
       Left $ ErrExistingKeyWitnesses $ length existingKeyWits

    return $ sealedTxFromCardanoBody body'

  where
    modifyLedgerTx
        :: forall era crypto. (crypto ~ Crypto (Cardano.ShelleyLedgerEra era))
        => TxUpdate
        -> Cardano.TxBody era
        -> Either ErrUpdateSealedTx (Cardano.TxBody era)
    modifyLedgerTx ebc (Cardano.ShelleyTxBody shelleyEra bod scripts scriptData aux val) =
        Right $ Cardano.ShelleyTxBody shelleyEra
            (adjustBody ebc shelleyEra bod)
            scripts
            scriptData
            aux
            val
      where
        -- NOTE: If the ShelleyMA MAClass were exposed, the Allegra and Mary
        -- cases could perhaps be joined. It is not however. And we still need
        -- to treat Alonzo and Shelley differently.
        adjustBody
            :: TxUpdate
            -> ShelleyBasedEra era
            -> Ledger.TxBody (Cardano.ShelleyLedgerEra era)
            -> Ledger.TxBody (Cardano.ShelleyLedgerEra era)
        adjustBody (TxUpdate extraInputs extraCollateral extraOutputs modifyFee) era body = case era of
            ShelleyBasedEraAlonzo -> body
                    { Alonzo.outputs = Alonzo.outputs body
                        <> StrictSeq.fromList (Cardano.toShelleyTxOut era <$> extraOutputs')
                    , Alonzo.inputs = Alonzo.inputs body
                        <> Set.fromList (Cardano.toShelleyTxIn <$> extraInputs')
                    , Alonzo.collateral = Alonzo.collateral body
                        <> Set.fromList (Cardano.toShelleyTxIn <$> extraCollateral')
                    , Alonzo.txfee =
                        modifyFee' $ Alonzo.txfee body
                    }
            ShelleyBasedEraMary ->
                let
                    ShelleyMA.TxBody inputs outputs certs wdrls txfee vldt update adHash mint = body
                in
                    ShelleyMA.TxBody
                        (inputs
                            <> Set.fromList (Cardano.toShelleyTxIn <$> extraInputs'))
                        (outputs
                            <> StrictSeq.fromList (Cardano.toShelleyTxOut era <$> extraOutputs'))
                        certs
                        wdrls
                        (modifyFee' txfee)
                        vldt
                        update
                        adHash
                        mint
            ShelleyBasedEraAllegra ->
                let
                    ShelleyMA.TxBody inputs outputs certs wdrls txfee vldt update adHash mint = body
                in
                    ShelleyMA.TxBody
                        (inputs
                            <> Set.fromList (Cardano.toShelleyTxIn <$> extraInputs'))
                        (outputs
                            <> StrictSeq.fromList (Cardano.toShelleyTxOut era <$> extraOutputs'))
                        certs
                        wdrls
                        (modifyFee' txfee)
                        vldt
                        update
                        adHash
                        mint
            ShelleyBasedEraShelley ->
                let
                    Shelley.TxBody inputs outputs certs wdrls txfee ttl txUpdate mdHash = body
                in
                    Shelley.TxBody
                        (inputs
                            <> Set.fromList (Cardano.toShelleyTxIn <$> extraInputs'))
                        (outputs
                            <> StrictSeq.fromList (Cardano.toShelleyTxOut era <$> extraOutputs'))
                        certs
                        wdrls
                        (modifyFee' txfee)
                        ttl
                        txUpdate
                        mdHash
          where
            extraInputs' = toCardanoTxIn . fst <$> extraInputs
            extraCollateral' = toCardanoTxIn <$> extraCollateral
            extraOutputs' = toCardanoTxOut era <$> extraOutputs
            modifyFee' old = toLedgerCoin $ modifyFee $ fromLedgerCoin old
              where
                toLedgerCoin :: Coin -> Ledger.Coin
                toLedgerCoin (Coin c) =
                    Ledger.Coin (intCast c)
                fromLedgerCoin :: Ledger.Coin -> Coin
                fromLedgerCoin (Ledger.Coin c) =
                    Coin.unsafeFromIntegral c
                    -- fromIntegral will throw "Exception: arithmetic underflow"
                    -- if (c :: Integral) for some reason were to be negative.

    modifyLedgerTx _ (Byron.ByronTxBody _)
        = Left ErrByronTxNotSupported

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
--
-- Returns `Nothing` for ByronEra transactions.
_evaluateMinimumFee
    :: Cardano.ProtocolParameters
    -> SealedTx
    -> Maybe Coin
_evaluateMinimumFee pp tx =
    fromCardanoLovelace <$> minFee
  where
    -- NOTE: Assuming one witness per certificate is wrong. KeyReg certs don't
    -- require witnesses, and several certs may share the same key.
    minFee = case getSealedTxBody tx of
        InAnyCardanoEra ShelleyEra txbody ->
            let (Cardano.ShelleyTxBody _ ledgertxbody _ _ _ _) = txbody
                certNum = length $ Shelley._certs ledgertxbody
            in Just $ Cardano.evaluateTransactionFee @Cardano.ShelleyEra pp txbody (witsNum txbody certNum) 0
        InAnyCardanoEra AllegraEra txbody ->
            let (Cardano.ShelleyTxBody _ ledgertxbody _ _ _ _) = txbody
                certNum = length $ ShelleyMA.certs' ledgertxbody
            in Just $ Cardano.evaluateTransactionFee @Cardano.AllegraEra pp txbody (witsNum txbody certNum) 0
        InAnyCardanoEra MaryEra txbody ->
            let (Cardano.ShelleyTxBody _ ledgertxbody _ _ _ _) = txbody
                certNum = length $ ShelleyMA.certs' ledgertxbody
            in Just $ Cardano.evaluateTransactionFee @Cardano.MaryEra pp txbody (witsNum txbody certNum) 0
        InAnyCardanoEra AlonzoEra txbody ->
            let (Cardano.ShelleyTxBody _ ledgertxbody _ _ _ _) = txbody
                certNum = length $ Alonzo.txcerts ledgertxbody
            in Just $ Cardano.evaluateTransactionFee @Cardano.AlonzoEra pp txbody (witsNum txbody certNum) 0
        InAnyCardanoEra ByronEra _ -> Nothing

    witsNum txbody certNum =
        let (Cardano.TxBody txbodycontent) = txbody
            txIns = Cardano.txIns txbodycontent
            txIns' = [ txin | (txin, Cardano.ViewTx) <- txIns ]
            txInsCollateral = Cardano.txInsCollateral txbodycontent
            txIns'' = case txInsCollateral of
                Cardano.TxInsCollateral _ collaterals -> collaterals
                _ -> []
            txInsUnique =  L.nub $ txIns' ++ txIns''
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
                Cardano.TxUpdateProposal _ (Cardano.UpdateProposal updatePerGenesisKey _) ->
                    Map.size updatePerGenesisKey
                _ -> 0
        in fromIntegral $
           length txInsUnique +
           length txExtraKeyWits' +
           length txWithdrawals' +
           txUpdateProposal' +
           certNum

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
    :: Cardano.ProtocolParameters
    -> TimeInterpreter (Either PastHorizonException)
    -> (TxIn -> Maybe (TxOut, Maybe (Hash "Datum")))
    -> [Redeemer]
    -> SealedTx
    -> Either ErrAssignRedeemers SealedTx
_assignScriptRedeemers (toAlonzoPParams -> pparams) ti resolveInput redeemers tx =
    case cardanoTx tx of
        InAnyCardanoEra ByronEra _ ->
            pure tx
        InAnyCardanoEra ShelleyEra _ ->
            pure tx
        InAnyCardanoEra AllegraEra _ ->
            pure tx
        InAnyCardanoEra MaryEra _ ->
            pure tx
        InAnyCardanoEra AlonzoEra (Cardano.ShelleyTx shelleyEra alonzoTx) -> do
            alonzoTx' <- flip execStateT alonzoTx $ do
                indexedRedeemers <- StateT assignNullRedeemers
                executionUnits <- get >>= lift . evaluateExecutionUnits indexedRedeemers
                modifyM (assignExecutionUnits executionUnits)
                modify' addScriptIntegrityHash
            pure $ sealedTxFromCardano' (Cardano.ShelleyTx shelleyEra alonzoTx')
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
    evaluateExecutionUnits indexedRedeemers alonzoTx =
        left ErrAssignRedeemersPastHorizon $ do
        let utxo = utxoFromAlonzoTx alonzoTx
        let costs = toCostModelsAsArray (Alonzo._costmdls pparams)
        let systemStart = getSystemStart ti
        epochInfo <- toEpochInfo ti

        hoistScriptFailure $ runIdentity $ runExceptT $ do
            evaluateTransactionExecutionUnits
                pparams
                alonzoTx
                utxo
                epochInfo
                systemStart
                costs
      where
        hoistScriptFailure
            :: Show scriptFailure
            => Either PastHorizonException (Map Alonzo.RdmrPtr (Either scriptFailure a))
            -> Either PastHorizonException (Map Alonzo.RdmrPtr (Either ErrAssignRedeemers a))
        hoistScriptFailure =
            fmap $ Map.mapWithKey
                (\ptr -> left $
                    \e -> ErrAssignRedeemersScriptFailure (indexedRedeemers ! ptr) (show e)
                )

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
        :: forall era. (era ~ Cardano.ShelleyLedgerEra Cardano.AlonzoEra)
        => AlonzoTx
        -> AlonzoTx
    addScriptIntegrityHash alonzoTx =
        let
            wits  = Alonzo.wits alonzoTx
            langs =
                [ l
                | (_hash, script) <- Map.toList (Alonzo.txscripts wits)
                , (not . isNativeScript @era) script
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
    -- Until we actually support minting and burning, leave these as empty.
    , txScripts = []
    , txAssetsToMintOrBurn = (<>)
        (TokenMap.getAssets (view #txAssetsToMint context))
        (TokenMap.getAssets (view #txAssetsToBurn context))
    , txScriptExecutionCost = view #txPlutusScriptExecutionCost context
    }

-- | Estimates the final cost of a transaction based on its skeleton.
--
estimateTxCost :: ProtocolParameters -> TxSkeleton -> Coin
estimateTxCost pp skeleton =
    F.fold
        [ computeFee (estimateTxSize skeleton)
        , scriptExecutionCosts
        ]
  where
    LinearFee (Quantity a) (Quantity b) = getFeePolicy $ txParameters pp

    computeFee :: TxSize -> Coin
    computeFee (TxSize size) =
        Coin $ ceiling (a + b * fromIntegral size)

    scriptExecutionCosts = view #txScriptExecutionCost skeleton

-- | Estimates the final size of a transaction based on its skeleton.
--
-- This function uses the upper bounds of CBOR serialized objects as the basis
-- for many of its calculations. The following document is used as a reference:
--
-- https://github.com/input-output-hk/cardano-ledger-specs/blob/master/shelley/chain-and-ledger/shelley-spec-ledger-test/cddl-files/shelley.cddl
-- https://github.com/input-output-hk/cardano-ledger-specs/blob/master/shelley-ma/shelley-ma-test/cddl-files/shelley-ma.cddl
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
    -> Either ErrMkTransaction (Cardano.TxBody era)
mkUnsignedTx era ttl cs md wdrls certs fees =
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

    , txExtraScriptData = Cardano.BuildTxWith Cardano.TxExtraScriptDataNone

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
        Cardano.TxMintNone
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
