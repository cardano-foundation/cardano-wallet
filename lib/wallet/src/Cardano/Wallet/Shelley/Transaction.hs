{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
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
    , txConstraints
    , mkUnsignedTransaction
    , mkTransaction

    -- * Internals
    , TxPayload (..)
    , TxWitnessTag (..)
    , _decodeSealedTx
    , mkDelegationCertificates
    , mkByronWitness
    , mkShelleyWitness
    , mkUnsignedTx
    , _txRewardWithdrawalCost
    , _txRewardWithdrawalSize
    , txWitnessTagForKey
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPrv
    , toXPub
    )
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
    , InAnyCardanoEra (..)
    , NetworkId
    , ShelleyBasedEra (..)
    )
import Cardano.Binary
    ( serialize'
    )
import Cardano.Crypto.Wallet
    ( XPub
    )
import Cardano.Ledger.Allegra.Core
    ( inputsTxBodyL
    )
import Cardano.Wallet.Address.Derivation
    ( Depth (..)
    , RewardAccount (..)
    )
import Cardano.Wallet.Address.Derivation.SharedKey
    ( replaceCosignersWithVerKeys
    )
import Cardano.Wallet.Address.Derivation.Shelley
    ( toRewardAccountRaw
    )
import Cardano.Wallet.Address.Encoding
    ( toHDPayloadAddress
    )
import Cardano.Wallet.Address.Keys.WalletKey
    ( getRawKey
    )
import Cardano.Wallet.Flavor
    ( KeyFlavorS (..)
    )
import Cardano.Wallet.Primitive.Ledger.Convert
    ( Convert (toLedger)
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx
    ( fromCardanoTx
    )
import Cardano.Wallet.Primitive.Ledger.Shelley
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
    )
import Cardano.Wallet.Primitive.Passphrase
    ( Passphrase (..)
    )
import Cardano.Wallet.Primitive.Types
    ( Certificate
    )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..)
    )
import Cardano.Wallet.Primitive.Types.AssetId
    ( AssetId (..)
    )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..)
    )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..)
    )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle (..)
    )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( TokenMap
    )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity (TokenQuantity)
    )
import Cardano.Wallet.Primitive.Types.Tx
    ( SealedTx (..)
    , Tx (..)
    , cardanoTxIdeallyNoLaterThan
    , sealedTxFromCardano
    , sealedTxFromCardano'
    )
import Cardano.Wallet.Primitive.Types.Tx.Constraints
    ( TxConstraints (..)
    , TxSize (..)
    , txOutMaxCoin
    , txOutMaxTokenQuantity
    )
import Cardano.Wallet.Primitive.Types.Tx.TxIn
    ( TxIn (..)
    )
import Cardano.Wallet.Primitive.Types.Tx.TxOut
    ( TxOut (..)
    )
import Cardano.Wallet.Transaction
    ( AnyExplicitScript (..)
    , AnyScript (..)
    , DelegationAction (..)
    , ErrMkTransaction (..)
    , ErrMkTransactionOutputTokenQuantityExceedsLimitError (..)
    , PreSelection (..)
    , ReferenceInput (..)
    , ScriptSource
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
import Cardano.Wallet.Transaction.Delegation
    ( toStakeKeyDeregCert
    , toStakeKeyRegCert
    , toStakePoolDlgCert
    )
import Cardano.Wallet.Util
    ( HasCallStack
    )
import Control.Arrow
    ( left
    , second
    )
import Control.Lens
    ( over
    )
import Control.Monad
    ( forM_
    , guard
    , when
    )
import Data.Bifunctor
    ( bimap
    )
import Data.Functor
    ( ($>)
    )
import Data.Generics.Internal.VL.Lens
    ( view
    , (^.)
    )
import Data.Generics.Labels
    ()
import Data.Map.Strict
    ( Map
    )
import Data.Maybe
    ( fromMaybe
    , mapMaybe
    )
import Data.Monoid.Monus
    ( Monus ((<\>))
    )
import Data.Word
    ( Word64
    , Word8
    )
import Internal.Cardano.Write.Tx
    ( CardanoApiEra
    , IsRecentEra
    , RecentEra (..)
    , ShelleyLedgerEra
    )
import Internal.Cardano.Write.Tx.SizeEstimation
    ( TxSkeleton (..)
    , TxWitnessTag (..)
    , estimateTxCost
    , estimateTxSize
    )
import Ouroboros.Network.Block
    ( SlotNo
    )

import qualified Cardano.Address.Script as CA
import qualified Cardano.Address.Style.Shelley as CA
import qualified Cardano.Api as Cardano
import qualified Cardano.Api.Byron as Byron
import qualified Cardano.Api.Shelley as Cardano
import qualified Cardano.Chain.Common as Byron
import qualified Cardano.Crypto as CC
import qualified Cardano.Crypto.Hash.Class as Crypto
import qualified Cardano.Crypto.Wallet as Crypto.HD
import qualified Cardano.Ledger.Api as Ledger
import qualified Cardano.Ledger.Keys.Bootstrap as SL
import qualified Cardano.Wallet.Primitive.Ledger.Convert as Convert
import qualified Cardano.Wallet.Primitive.Ledger.Shelley as Compatibility
import qualified Cardano.Wallet.Primitive.Types.AssetId as AssetId
import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap
import qualified Data.ByteString as BS
import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Internal.Cardano.Write.Tx as Write
import qualified Internal.Cardano.Write.Tx.Sign as Write
import qualified Internal.Cardano.Write.Tx.SizeEstimation as Write

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

constructUnsignedTx
    :: forall era
     . IsRecentEra era
    => RecentEra era
    -> Cardano.NetworkId
    -> (Maybe Cardano.TxMetadata, [Cardano.Certificate])
    -> (Maybe SlotNo, SlotNo)
    -- ^ Slot at which the transaction will optionally start and expire.
    -> Withdrawal
    -> Either PreSelection (SelectionOf TxOut)
    -- ^ Finalized asset selection
    -> Coin
    -- ^ Explicit fee amount
    -> (TokenMap, Map AssetId ScriptSource)
    -- ^ Assets to be minted
    -> (TokenMap, Map AssetId ScriptSource)
    -- ^ Assets to be burned
    -> Map TxIn (Script KeyHash)
    -- ^ scripts for inputs
    -> Maybe (Script KeyHash)
    -- ^ Delegation script
    -> Maybe (Script KeyHash)
    -- ^ Reference script
    -> Either ErrMkTransaction (Cardano.TxBody (Write.CardanoApiEra era))
constructUnsignedTx
    era networkId (md, certs) ttl wdrl
    cs fee toMint toBurn =
        mkUnsignedTx @era
            era ttl cs md wdrls certs (toCardanoLovelace fee)
            (fst toMint) (fst toBurn) mintingScripts

  where
    wdrls = mkWithdrawals networkId wdrl
    mintingScripts = Map.union (snd toMint) (snd toBurn)

mkTransaction
    :: forall era k. IsRecentEra era
    => RecentEra era
    -- ^ Era for which the transaction should be created.
    -> Cardano.NetworkId
    -> KeyFlavorS k
    -> (XPrv, Passphrase "encryption")
    -- ^ Reward account.
    -> (Address -> Maybe (k 'CredFromKeyK XPrv, Passphrase "encryption"))
    -- ^ Key store.
    -> TransactionCtx
    -- ^ Additional context about the transaction.
    -> SelectionOf TxOut
    -- ^ A balanced coin selection where all change addresses have been
    -- assigned.
    -> Either ErrMkTransaction (Tx, SealedTx)
mkTransaction era networkId keyF stakeCreds addrResolver ctx cs = do
    let ttl = txValidityInterval ctx
    let wdrl = view #txWithdrawal ctx
    let delta = selectionDelta cs
    let md = view #txMetadata ctx
    let certs =
            case view #txDelegationAction ctx of
                Nothing ->
                    mempty
                Just action ->
                    let stakeXPub = toXPub $ fst stakeCreds
                    in mkDelegationCertificates action (Left stakeXPub)
    let wdrls = mkWithdrawals networkId wdrl
    unsigned <-
        mkUnsignedTx
            era
            ttl
            (Right cs)
            md
            wdrls
            certs
            (toCardanoLovelace delta)
            TokenMap.empty
            TokenMap.empty
            Map.empty
            Map.empty
            Nothing
            Nothing
    let signed :: Cardano.Tx (CardanoApiEra era)
        signed = Write.toCardanoApiTx $
            signTransaction
                @era
                keyF
                networkId
                AnyWitnessCountCtx
                acctResolver
                (const Nothing)
                Nothing
                (const Nothing)
                addrResolver
                inputResolver
                (Write.fromCardanoApiTx $ Cardano.Tx unsigned [])
    let withResolvedInputs (tx, _, _, _, _, _) =
            tx {resolvedInputs = second Just <$> F.toList (view #inputs cs)}
    Right
        ( withResolvedInputs (fromCardanoTx AnyWitnessCountCtx signed)
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
        let (rewardAcnt, pwdAcnt) = stakeCreds
        let acct' = toRewardAccountRaw $ toXPub rewardAcnt
        guard (acct == acct') $> (rewardAcnt, pwdAcnt)

-- Adds VK witnesses to an already constructed transactions. The function
-- preserves any existing witnesses on the transaction, and resolve inputs
-- dynamically using the provided lookup function.
--
-- If a key for a given input isn't found, the input is skipped.
signTransaction
    :: forall era k ktype
     . Write.IsRecentEra era
    => KeyFlavorS k
    -> Cardano.NetworkId
    -- ^ Network identifier (e.g. mainnet, testnet)
    -> WitnessCountCtx
    -> (RewardAccount -> Maybe (XPrv, Passphrase "encryption"))
    -- ^ Stake key store / reward account resolution
    -> (KeyHash -> Maybe (XPrv, Passphrase "encryption"))
    -- ^ Policy key resolution
    -> Maybe KeyHash
    -- ^ Optional policy key
    -> (KeyHash -> Maybe (XPrv, Passphrase "encryption"))
    -- ^ Staking script key resolution
    -> (Address -> Maybe (k ktype XPrv, Passphrase "encryption"))
    -- ^ Payment key store
    -> (TxIn -> Maybe Address)
    -- ^ Input resolver
    -> Write.Tx era
    -- ^ The transaction to sign, possibly with already some existing witnesses
    -> Write.Tx era
signTransaction
    keyF
    networkId
    witCountCtx
    resolveRewardAcct
    resolvePolicyKey
    policyKeyM
    resolveStakingKeyInScript
    resolveAddress
    resolveInput
    txToSign
    =
    Write.fromCardanoApiTx $ Cardano.makeSignedTransaction wits' body
 where
    era = Write.recentEra @era

    Cardano.Tx body wits = Write.toCardanoApiTx txToSign

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

    isTimelockOrRef (NativeScript _ _) = True
    isTimelockOrRef (AnyScriptReference _ _) = True
    isTimelockOrRef _ = False

    getScriptsKeyHashes :: TokenMapWithScripts -> [KeyHash]
    getScriptsKeyHashes scripts =
        let getKeyHash script@(NativeScript _ _) =
                retrieveAllKeyHashes script
            getKeyHash (AnyScriptReference _ _) = case policyKeyM of
                Just policyKey -> [policyKey]
                Nothing -> []
            getKeyHash _ = error "getKeyHash: this should be filtered at at this stage"
        in
        concatMap getKeyHash $
        filter isTimelockOrRef $
        Map.elems $ scripts ^. #txScripts

    mkTxInWitness :: TxIn -> Maybe (Cardano.KeyWitness (CardanoApiEra era))
    mkTxInWitness i = do
        addr <- resolveInput i
        (k, pwd) <- resolveAddress addr
        let  pk = (getRawKey keyF k, pwd)
        pure $ case txWitnessTagForKey keyF of
            TxWitnessShelleyUTxO -> mkShelleyWitness era body pk
            TxWitnessByronUTxO -> mkByronWitness era body networkId addr pk

    mkWdrlCertWitness
        :: RewardAccount
        -> Maybe (Cardano.KeyWitness (CardanoApiEra era))
    mkWdrlCertWitness a =
        mkShelleyWitness era body <$> resolveRewardAcct a

    mkPolicyWitness
        :: KeyHash
        -> Maybe (Cardano.KeyWitness (CardanoApiEra era))
    mkPolicyWitness a =
        mkShelleyWitness era body <$> resolvePolicyKey a

    mkStakingScriptWitness
        :: KeyHash
        -> Maybe (Cardano.KeyWitness (CardanoApiEra era))
    mkStakingScriptWitness a =
        mkShelleyWitness era body <$> resolveStakingKeyInScript a

    mkExtraWitness
        :: Cardano.Hash Cardano.PaymentKey
        -> Maybe (Cardano.KeyWitness (CardanoApiEra era))
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
        pure $ mkShelleyWitness era body (getRawKey keyF k, pwd)

newTransactionLayer
    :: KeyFlavorS k
    -> NetworkId
    -> TransactionLayer k ktype SealedTx
newTransactionLayer keyF networkId = TransactionLayer
    { addVkWitnesses =
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

            let policyKeyM :: Maybe KeyHash
                policyKeyM = do
                    (keyhash', _, _) <- policyCreds
                    pure keyhash'

            let errNonRecentEra = error $ unwords
                    [ "addVkWitnesses: cannot add witnesses to a"
                    , "transaction from a non-recent era."
                    ]

            sealedTxFromCardano
                $ fromMaybe errNonRecentEra
                $ withRecentEraLedgerTx
                    (cardanoTxIdeallyNoLaterThan era sealedTx)
                    $ \ledgerTx ->
                        signTransaction
                            keyF networkId witCountCtx acctResolver
                            policyResolver policyKeyM stakingScriptResolver
                            addressResolver inputResolver
                            ledgerTx

    , decodeTx = _decodeSealedTx

    , transactionWitnessTag = txWitnessTagForKey keyF
    }

withRecentEraLedgerTx
    :: InAnyCardanoEra Cardano.Tx
    -> (forall era. IsRecentEra era => Write.Tx era -> Write.Tx era)
    -> Maybe (InAnyCardanoEra Cardano.Tx)
withRecentEraLedgerTx (InAnyCardanoEra era tx) f = case era of
    Cardano.ConwayEra -> Just
        . InAnyCardanoEra era
        . Write.toCardanoApiTx
        . f
        . Write.fromCardanoApiTx
        $ tx
    Cardano.BabbageEra -> Just
        . InAnyCardanoEra era
        . Write.toCardanoApiTx
        . f
        . Write.fromCardanoApiTx
        $ tx
    Cardano.AlonzoEra
        -> Nothing
    Cardano.MaryEra
        -> Nothing
    Cardano.AllegraEra
        -> Nothing
    Cardano.ShelleyEra
        -> Nothing
    Cardano.ByronEra
        -> Nothing

-- | Construct a standard unsigned transaction.
--
-- The term "standard" refers to the fact that we do not deal with redemption,
-- multi-signature transactions, etc.
--
-- This function returns a CBOR-ed transaction body, which should be signed
-- in separate step.
--
mkUnsignedTransaction
    :: forall era
     . Write.IsRecentEra era
    => Write.RecentEra era
    -> NetworkId
    -> Either XPub (Maybe (Script KeyHash))
    -- ^ Reward account public key or optional script hash.
    -> TransactionCtx
    -- ^ Additional context about the transaction.
    -> Either PreSelection (SelectionOf TxOut)
    -- ^ A balanced coin selection where all change addresses have been
    -- assigned.
    -> Either ErrMkTransaction (Cardano.TxBody (CardanoApiEra era))
mkUnsignedTransaction era networkId stakeCred ctx selection = do
    let ttl = txValidityInterval ctx
    let wdrl = view #txWithdrawal ctx
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
            let ourRewardAcctM
                    = FromScriptHash
                    . unScriptHash
                    . toScriptHash <$> stakingScriptM
            case wdrl of
                WithdrawalSelf rewardAcct _ _ ->
                    if ourRewardAcctM == Just rewardAcct
                    then
                        constructUnsignedTx era
                            networkId (md, []) ttl wdrl selection delta
                            assetsToBeMinted assetsToBeBurned
                            inpsScripts stakingScriptM refScriptM
                    else
                        constructUnsignedTx era
                            networkId (md, []) ttl wdrl selection delta
                            assetsToBeMinted assetsToBeBurned
                            inpsScripts Nothing refScriptM
                _ ->
                    constructUnsignedTx era
                        networkId (md, []) ttl wdrl
                        selection delta assetsToBeMinted assetsToBeBurned
                        inpsScripts
                    Nothing refScriptM
        Just action -> do
            let certs = case stakeCred of
                    Left xpub ->
                        mkDelegationCertificates action (Left xpub)
                    Right (Just script) ->
                        mkDelegationCertificates action (Right script)
                    Right Nothing ->
                        error $ unwords
                            [ "stakeCred in mkUnsignedTransaction must be"
                            , "either xpub or script when there is delegation"
                            , "action"
                            ]
            let payload = (view #txMetadata ctx, certs)
            constructUnsignedTx era networkId payload ttl wdrl
                selection delta assetsToBeMinted assetsToBeBurned inpsScripts
                stakingScriptM refScriptM

_decodeSealedTx
    :: AnyCardanoEra
    -> WitnessCountCtx
    -> SealedTx
    ->  ( Tx
        , TokenMapWithScripts
        , TokenMapWithScripts
        , [Certificate]
        , Maybe ValidityIntervalExplicit
        , WitnessCount
        )
_decodeSealedTx preferredLatestEra witCtx sealedTx =
    case cardanoTxIdeallyNoLaterThan preferredLatestEra sealedTx of
        Cardano.InAnyCardanoEra _ tx ->
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
    :: forall era. Write.IsRecentEra era
    => Write.RecentEra era
    -> (Maybe SlotNo, SlotNo)
    -> Either PreSelection (SelectionOf TxOut)
    -> Maybe Cardano.TxMetadata
    -> [(Cardano.StakeAddress, Cardano.Lovelace)]
    -> [Cardano.Certificate]
    -> Cardano.Lovelace
    -> TokenMap
    -> TokenMap
    -> Map AssetId ScriptSource
    -> Map TxIn (Script KeyHash)
    -> Maybe (Script KeyHash)
    -> Maybe (Script KeyHash)
    -> Either ErrMkTransaction (Cardano.TxBody (Write.CardanoApiEra era))
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
    mintingSource
    inpsScripts
    stakingScriptM
    refScriptM = extractValidatedOutputs cs >>= \outs ->
    left toErrMkTx
    $ fmap removeDummyInput
    $ Cardano.createAndValidateTransactionBody
    Cardano.TxBodyContent
    { Cardano.txIns = inputWits

    , txInsReference =
        let hasRefInp = \case
                Left _ -> False
                Right _ -> True
            filteredRefInp = filter hasRefInp $ Map.elems mintingSource
            toNodeTxIn (Right (ReferenceInput txin)) = toCardanoTxIn txin
            toNodeTxIn _ = error "at this moment we should have reference input"
        in
        if null filteredRefInp
        then
            Cardano.TxInsReferenceNone
        else
            case referenceInpsSupported of
                Nothing -> Cardano.TxInsReferenceNone
                Just support ->
                    Cardano.TxInsReference support
                    (toNodeTxIn <$> filteredRefInp)

    , Cardano.txOuts = case refScriptM of
        Nothing ->
            map (toCardanoTxOut shelleyEra Nothing) outs
        Just _ -> case outs of
            firstOut:rest ->
                let cardanoFirstTxOut =
                        toCardanoTxOut shelleyEra refScriptM firstOut
                in
                cardanoFirstTxOut :
                    (map (toCardanoTxOut shelleyEra Nothing) rest)
            _ ->
                []

    , Cardano.txWithdrawals = case stakingScriptM of
        Nothing ->
            let ctx = Cardano.BuildTxWith
                    $ Cardano.KeyWitness Cardano.KeyWitnessForStakeAddr
            in
            Cardano.TxWithdrawals wdrlsSupported
                (map (\(key, coin) -> (key, coin, ctx)) wdrls)
        Just stakingScript ->
            let
                buildVal =
                    Cardano.ScriptWitness Cardano.ScriptWitnessForStakeAddr
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
                buildVal =
                    Cardano.ScriptWitness
                        Cardano.ScriptWitnessForStakeAddr
                        (toScriptWitness stakingScript)
                witMap = Map.fromList [(buildKey, buildVal)]
                ctx = Cardano.BuildTxWith witMap
            in
            Cardano.TxCertificates certSupported certs ctx

    , Cardano.txFee = explicitFees shelleyEra fees

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
                    toScriptWitnessGeneral = \case
                        Left script -> toScriptWitness script
                        Right (ReferenceInput txin) ->
                            Cardano.SimpleScriptWitness
                            scriptWitsSupported $
                                Cardano.SReferenceScript
                                (toCardanoTxIn txin)
                                Nothing
                    witMap =
                            Map.map toScriptWitnessGeneral $
                            Map.mapKeys
                                (toCardanoPolicyId . AssetId.policyId)
                            mintingSource
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

    metadataSupported
        :: Cardano.TxMetadataSupportedInEra (Write.CardanoApiEra era)
    metadataSupported = case era of
        RecentEraBabbage -> Cardano.TxMetadataInBabbageEra
        RecentEraConway -> Cardano.TxMetadataInConwayEra

    certSupported
        :: Cardano.CertificatesSupportedInEra (Write.CardanoApiEra era)
    certSupported = case era of
        RecentEraBabbage -> Cardano.CertificatesInBabbageEra
        RecentEraConway -> Cardano.CertificatesInConwayEra

    wdrlsSupported
        :: Cardano.WithdrawalsSupportedInEra (Write.CardanoApiEra era)
    wdrlsSupported = case era of
        RecentEraBabbage -> Cardano.WithdrawalsInBabbageEra
        RecentEraConway -> Cardano.WithdrawalsInConwayEra

    txValidityUpperBoundSupported
        :: Cardano.ValidityUpperBoundSupportedInEra (Write.CardanoApiEra era)
    txValidityUpperBoundSupported = case era of
        RecentEraBabbage -> Cardano.ValidityUpperBoundInBabbageEra
        RecentEraConway -> Cardano.ValidityUpperBoundInConwayEra

    txValidityLowerBoundSupported
        :: Maybe
            (Cardano.ValidityLowerBoundSupportedInEra (Write.CardanoApiEra era))
    txValidityLowerBoundSupported = case era of
        RecentEraBabbage -> Just Cardano.ValidityLowerBoundInBabbageEra
        RecentEraConway -> Just Cardano.ValidityLowerBoundInConwayEra

    txMintingSupported
        :: Maybe (Cardano.MultiAssetSupportedInEra (Write.CardanoApiEra era))
    txMintingSupported = case era of
        RecentEraBabbage -> Just Cardano.MultiAssetInBabbageEra
        RecentEraConway -> Just Cardano.MultiAssetInConwayEra

    scriptWitsSupported
        :: Cardano.ScriptLanguageInEra
            Cardano.SimpleScript'
            (Write.CardanoApiEra era)
    scriptWitsSupported = case era of
        RecentEraBabbage -> Cardano.SimpleScriptInBabbage
        RecentEraConway -> Cardano.SimpleScriptInConway

    referenceInpsSupported
        :: Maybe
            (Cardano.ReferenceTxInsScriptsInlineDatumsSupportedInEra
                (Write.CardanoApiEra era)
            )
    referenceInpsSupported = case era of
        RecentEraBabbage ->
            Just Cardano.ReferenceTxInsScriptsInlineDatumsInBabbageEra
        RecentEraConway ->
            Just Cardano.ReferenceTxInsScriptsInlineDatumsInConwayEra

    toScriptWitness
        :: Script KeyHash
        -> Cardano.ScriptWitness witctx (Write.CardanoApiEra era)
    toScriptWitness script =
        Cardano.SimpleScriptWitness
        scriptWitsSupported
        (Cardano.SScript $ toCardanoSimpleScript script)

    constructInpScriptWit inp =
        let script = case Map.lookup inp inpsScripts of
                Nothing ->
                    error $ unwords
                        [ "constructInpScriptWit:"
                        , "each input should have script in multisig"
                        ]
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
    shelleyEra = Write.shelleyBasedEraFromRecentEra era

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
    :: Write.IsRecentEra era
    => RecentEra era
    -> Cardano.TxBody (CardanoApiEra era)
    -> (XPrv, Passphrase "encryption")
    -> Cardano.KeyWitness (CardanoApiEra era)
mkShelleyWitness _era body key =
    Cardano.makeShelleyKeyWitness body (unencrypt key)
  where
    unencrypt (xprv, pwd) = Cardano.WitnessPaymentExtendedKey
        $ Cardano.PaymentExtendedSigningKey
        $ Crypto.HD.xPrvChangePass pwd BS.empty xprv

mkByronWitness
    :: forall era. IsRecentEra era
    => RecentEra era
    -> Cardano.TxBody (CardanoApiEra era)
    -> Cardano.NetworkId
    -> Address
    -> (XPrv, Passphrase "encryption")
    -> Cardano.KeyWitness (CardanoApiEra era)
mkByronWitness _ (Byron.ByronTxBody _ :: Cardano.TxBody byronEra) _ _ _ =
    case Write.recentEra @(ShelleyLedgerEra byronEra) of {}
mkByronWitness
    era
    (Cardano.ShelleyTxBody
        cardanoEra
        body
        _scripts
        _scriptData
        _auxData
        _scriptValidity)
    nw
    addr
    encryptedKey =
    Cardano.ShelleyBootstrapWitness cardanoEra $
        SL.makeBootstrapWitness txHash (unencrypt encryptedKey) addrAttr
  where
    txHash = case era of
        RecentEraBabbage -> Crypto.castHash $ Crypto.hashWith serialize' body
        RecentEraConway  -> Crypto.castHash $ Crypto.hashWith serialize' body

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

txConstraints
    :: forall era. IsRecentEra era
    => Write.PParams era
    -> TxWitnessTag
    -> TxConstraints
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
    era = Write.recentEra @era

    txBaseCost =
        constantTxFee <> estimateTxCost feePerByte empty

    constantTxFee =
        Convert.toWallet $ protocolParams ^. Ledger.ppMinFeeBL

    feePerByte = Write.getFeePerByte protocolParams

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
        (TxSize (protocolParams ^. Ledger.ppMaxValSizeL))

    txOutputMaximumTokenQuantity =
        TokenQuantity $ fromIntegral $ maxBound @Word64

    txOutputMinimumAdaQuantity addr tokens = Convert.toWallet $
        Write.computeMinimumCoinForTxOut
            protocolParams
            (mkLedgerTxOut addr (TokenBundle txOutMaxCoin tokens))

    txOutputBelowMinimumAdaQuantity addr bundle =
        Write.isBelowMinimumCoinForTxOut
            protocolParams
            (mkLedgerTxOut addr bundle)

    txRewardWithdrawalCost =
        _txRewardWithdrawalCost feePerByte (Right witnessTag)

    txRewardWithdrawalSize =
        _txRewardWithdrawalSize (Right witnessTag)

    txMaximumSize =
        TxSize $ protocolParams ^. Ledger.ppMaxTxSizeL

    empty :: TxSkeleton
    empty = TxSkeleton
        { txWitnessTag = witnessTag
        , txInputCount = 0
        , txOutputs = []
        , txChange = []
        , txPaymentTemplate = Nothing
        }

    -- Computes the size difference between the given skeleton and an empty
    -- skeleton.
    marginalCostOf :: TxSkeleton -> Coin
    marginalCostOf skeleton =
        estimateTxCost feePerByte skeleton <\>
        estimateTxCost feePerByte empty

    -- Computes the size difference between the given skeleton and an empty
    -- skeleton.
    marginalSizeOf :: TxSkeleton -> TxSize
    marginalSizeOf =
        (<\> txBaseSize) . estimateTxSize

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

    mkLedgerTxOut
        :: HasCallStack
        => Address
        -> TokenBundle
        -> Write.TxOut era
    mkLedgerTxOut address bundle =
        case era of
            Write.RecentEraBabbage -> Convert.toBabbageTxOut txOut
            Write.RecentEraConway -> Convert.toConwayTxOut txOut
          where
            txOut = TxOut address bundle

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
    Convert.toWallet
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
_txRewardWithdrawalSize witType _ =
    Write.sizeOf_Withdrawals 1 <> wits
  where
    wits = case witType of
        Right TxWitnessByronUTxO ->
            Write.sizeOf_BootstrapWitnesses 1 -
            Write.sizeOf_BootstrapWitnesses 0
        Right TxWitnessShelleyUTxO ->
            Write.sizeOf_VKeyWitnesses 1
        Left scriptTemplate ->
            let n = fromIntegral
                    $ Write.estimateMaxWitnessRequiredPerInput
                    $ view #template scriptTemplate
            in Write.sizeOf_VKeyWitnesses n

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
