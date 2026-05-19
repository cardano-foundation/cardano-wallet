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
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

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

      -- * Internals
    , TxPayload (..)
    , TxWitnessTag (..)
    , mkByronWitness
    , mkShelleyWitness
    , mkUnsignedTx
    , signTransaction
    , _txRewardWithdrawalCost
    , _txRewardWithdrawalSize
    , txWitnessTagForKey
    ) where

import Cardano.Address.Derivation
    ( XPrv
    , toXPub
    )
import Cardano.Address.KeyHash
    ( KeyHash (..)
    , KeyRole (..)
    )
import Cardano.Address.Script
    ( Script (..)
    , ScriptHash (..)
    , foldScript
    , toScriptHash
    )
import Cardano.Api
    ( NetworkId
    )
import Cardano.Api.Extra
    ( CardanoApiEra
    , fromCardanoApiTx
    , shelleyBasedEraFromRecentEra
    , toCardanoApiTx
    )
import Cardano.Balance.Tx.Eras
    ( RecentEra (..)
    )
import Cardano.Balance.Tx.SizeEstimation
    ( TxSkeleton (..)
    , TxWitnessTag (..)
    )
import Cardano.Binary
    ( serialize'
    )
import Cardano.Crypto.Wallet
    ( XPub
    )
import Cardano.Ledger.Core
    ( TxCert
    )
import Cardano.Wallet.Address.Derivation
    ( RewardAccount (..)
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
    ( toLedgerMintValue
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.TxExtended
    ( fromCardanoTx
    , getTxExtended
    )
import Cardano.Wallet.Primitive.Ledger.Shelley
    ( cardanoCertKeysForWitnesses
    , fromCardanoAddress
    , fromCardanoWdrls
    )
import Cardano.Wallet.Primitive.Passphrase
    ( Passphrase (..)
    )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..)
    )
import Cardano.Wallet.Primitive.Types.AssetId
    ( AssetId (..)
    )
import Cardano.Wallet.Primitive.Types.AssetName
    ( AssetName (UnsafeAssetName)
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
import Cardano.Wallet.Primitive.Types.TokenPolicyId
    ( TokenPolicyId (UnsafeTokenPolicyId)
    )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity (TokenQuantity)
    )
import Cardano.Wallet.Primitive.Types.Tx
    ( SealedTx (..)
    , sealedTxFromBytes'
    , sealedTxFromLedgerTx
    )
import Cardano.Wallet.Primitive.Types.Tx.Constraints
    ( TxConstraints (..)
    , TxSize (..)
    , txOutMaxCoin
    )
import Cardano.Wallet.Primitive.Types.Tx.TxExtended
    ( TxExtended (..)
    )
import Cardano.Wallet.Primitive.Types.Tx.TxIn
    ( TxIn (..)
    )
import Cardano.Wallet.Primitive.Types.Tx.TxMetadata
    ( TxMetadata (..)
    )
import Cardano.Wallet.Primitive.Types.Tx.TxOut
    ( TxOut (..)
    )
import Cardano.Wallet.Shelley.Transaction.Unsigned
    ( ScriptWitnesses (..)
    , TxPayload (..)
    , buildLedgerTxRaw
    , certificateFromDelegationActionLedger
    , certificateFromVotingActionLedger
    , extractValidatedOutputs
    , ledgerNetworkOf
    , noScriptWitnesses
    )
import Cardano.Wallet.Transaction
    ( AnyExplicitScript (..)
    , AnyScript (..)
    , ErrMkTransaction (..)
    , PreSelection (..)
    , ScriptSource
    , SelectionOf (..)
    , TokenMapWithScripts
    , TransactionCtx (..)
    , TransactionLayer (..)
    , Withdrawal (..)
    , WitnessCount (..)
    , WitnessCountCtx (..)
    , selectionDelta
    )
import Control.Monad
    ( guard
    )
import Data.Functor
    ( ($>)
    )
import Data.Generics.Internal.VL.Lens
    ( view
    , (^.)
    )
import Data.Generics.Labels
    (
    )
import Data.IntCast
    ( intCast
    )
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
import Ouroboros.Network.Block
    ( SlotNo
    )
import Prelude

import qualified Cardano.Address.Script as CA
import qualified Cardano.Address.Style.Shelley as CA
import qualified Cardano.Api as Cardano
import qualified Cardano.Api.Byron as Byron
import qualified Cardano.Balance.Tx.Eras as Write
import qualified Cardano.Balance.Tx.Primitive as BT
import qualified Cardano.Balance.Tx.Sign as Write
    ( estimateMaxWitnessRequiredPerInput
    )
import qualified Cardano.Balance.Tx.SizeEstimation as BT
    ( estimateTxCost
    , estimateTxSize
    )
import qualified Cardano.Balance.Tx.SizeEstimation as Write
    ( sizeOf_BootstrapWitnesses
    , sizeOf_VKeyWitnesses
    , sizeOf_Withdrawals
    )
import qualified Cardano.Balance.Tx.Tx as Write
    ( FeePerByte
    , PParams
    , Tx
    , TxOut
    , computeMinimumCoinForTxOut
    , feeOfBytes
    , getFeePerByte
    , isBelowMinimumCoinForTxOut
    )
import qualified Cardano.Crypto as CC
import qualified Cardano.Crypto.Hash.Class as Crypto
import qualified Cardano.Crypto.Wallet as Crypto.HD
import qualified Cardano.Ledger.Api as Ledger
import qualified Cardano.Ledger.Keys.Bootstrap as SL
import qualified Cardano.Wallet.Primitive.Ledger.Convert as Convert
import qualified Cardano.Wallet.Primitive.Ledger.Shelley as Compatibility
import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap
import qualified Cardano.Wallet.Read as Read
import qualified Data.ByteString as BS
import qualified Data.List as L
import qualified Data.Map as Map

constructUnsignedTx
    :: forall era
     . Write.IsRecentEra era
    => Cardano.NetworkId
    -> (Maybe TxMetadata, [TxCert era])
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
    -> Either ErrMkTransaction (Cardano.TxBody (CardanoApiEra era))
constructUnsignedTx
    networkId
    (md, certs)
    ttl
    wdrl
    cs
    fee
    toMint
    toBurn =
        mkUnsignedTx
            networkId
            ttl
            cs
            md
            wdrl
            certs
            fee
            (fst toMint)
            (fst toBurn)
            mintingScripts
      where
        mintingScripts = Map.union (snd toMint) (snd toBurn)

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
    txToSign =
        fromCardanoApiTx $ Cardano.makeSignedTransaction wits' body
      where
        Cardano.Tx body wits = toCardanoApiTx txToSign

        wits' =
            mconcat
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
            bodyContent = Cardano.getTxBodyContent body
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
            TxExtended{..} = fromCardanoTx $ Cardano.makeSignedTransaction wits body
            mintBurnScriptsKeyHashes =
                -- Note that we use 'nub' here because multiple scripts can share
                -- the same policyXPub. It's sufficient to have one witness for
                -- each.
                L.nub $ getScriptsKeyHashes toMint <> getScriptsKeyHashes toBurn

            stakingScriptsKeyHashes =
                let WitnessCount _ nativeScripts _ = witnessCount witCountCtx

                    isDelegationKeyHash (KeyHash Delegation _) = True
                    isDelegationKeyHash (KeyHash _ _) = False
                in  filter isDelegationKeyHash
                        $ L.nub
                        $ concatMap retrieveAllKeyHashesE
                        $ filter isTimelockE nativeScripts

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
            in  concatMap getKeyHash
                    $ filter isTimelockOrRef
                    $ Map.elems
                    $ scripts ^. #txScripts

        mkTxInWitness
            :: TxIn -> Maybe (Cardano.KeyWitness (CardanoApiEra era))
        mkTxInWitness i = do
            addr <- resolveInput i
            (k, pwd) <- resolveAddress addr
            let pk = (getRawKey keyF k, pwd)
            pure $ case txWitnessTagForKey keyF of
                TxWitnessShelleyUTxO -> mkShelleyWitness body pk
                TxWitnessByronUTxO -> mkByronWitness body networkId addr pk

        mkWdrlCertWitness
            :: RewardAccount
            -> Maybe (Cardano.KeyWitness (CardanoApiEra era))
        mkWdrlCertWitness a =
            mkShelleyWitness body <$> resolveRewardAcct a

        mkPolicyWitness
            :: KeyHash
            -> Maybe (Cardano.KeyWitness (CardanoApiEra era))
        mkPolicyWitness a =
            mkShelleyWitness body <$> resolvePolicyKey a

        mkStakingScriptWitness
            :: KeyHash
            -> Maybe (Cardano.KeyWitness (CardanoApiEra era))
        mkStakingScriptWitness a =
            mkShelleyWitness body <$> resolveStakingKeyInScript a

        mkExtraWitness
            :: Cardano.Hash Cardano.PaymentKey
            -> Maybe (Cardano.KeyWitness (CardanoApiEra era))
        mkExtraWitness vkh = do
            -- NOTE: We cannot resolve key hashes directly, so create a one-time
            -- temporary address with that key hash which is fine to lookup via the
            -- address lookup provided above. It works _fine_ because the discovery
            -- of addresses is done properly based on the address constituents (i.e.
            -- the key hash) and not the overall address itself.
            let addr =
                    Cardano.makeShelleyAddress
                        networkId
                        (Cardano.PaymentCredentialByKey vkh)
                        Cardano.NoStakeAddress
            (k, pwd) <- resolveAddress (fromCardanoAddress addr)
            pure $ mkShelleyWitness body (getRawKey keyF k, pwd)

newTransactionLayer
    :: KeyFlavorS k
    -> NetworkId
    -> TransactionLayer k ktype SealedTx
newTransactionLayer keyF networkId =
    TransactionLayer
        { addVkWitnesses =
            \preferredLatestEra
             witCountCtx
             stakeCreds
             policyCreds
             scriptStakingCredM
             addressResolver
             inputResolver
             sealedTx -> do
                    let acctMap :: Map RewardAccount (XPrv, Passphrase "encryption")
                        acctMap =
                            Map.fromList
                                $ map
                                    (\(xprv, pwd) -> (toRewardAccountRaw $ toXPub xprv, (xprv, pwd)))
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

                    let errNonRecentEra =
                            error
                                $ unwords
                                    [ "addVkWitnesses: cannot add witnesses to a"
                                    , "transaction from a non-recent era."
                                    ]

                    fromMaybe errNonRecentEra
                        $ withSealedTxRecentEra preferredLatestEra sealedTx
                        $ \era' ledgerTx ->
                            sealRecentTx era'
                                $ signTransaction
                                    keyF
                                    networkId
                                    witCountCtx
                                    acctResolver
                                    policyResolver
                                    policyKeyM
                                    stakingScriptResolver
                                    addressResolver
                                    inputResolver
                                    ledgerTx
        , decodeTx = _decodeSealedTx
        , transactionWitnessTag = txWitnessTagForKey keyF
        }

withSealedTxRecentEra
    :: Read.EraValue Read.Era
    -> SealedTx
    -> ( forall era
          . Write.IsRecentEra era => RecentEra era -> Write.Tx era -> a
       )
    -> Maybe a
withSealedTxRecentEra preferredLatestEra sealedTx f =
    case readSealedTxIdeallyNoLaterThan preferredLatestEra sealedTx of
        Read.EraValue (Read.Tx tx :: Read.Tx era) -> case Read.theEra @era of
            Read.Conway ->
                Just $ f RecentEraConway tx
            Read.Dijkstra ->
                Just $ f RecentEraDijkstra tx
            _ ->
                Nothing

readSealedTxIdeallyNoLaterThan
    :: Read.EraValue Read.Era
    -> SealedTx
    -> Read.EraValue Read.Tx
readSealedTxIdeallyNoLaterThan preferredLatestEra sealedTx =
    unsafeReadTx
        $ fromMaybe sealedTx
        $ either (const Nothing) Just
        $ sealedTxFromBytes'
            preferredLatestEra
            (serialisedTx sealedTx)

sealRecentTx
    :: RecentEra era
    -> Write.Tx era
    -> SealedTx
sealRecentTx era tx = case era of
    RecentEraConway ->
        sealedTxFromLedgerTx (Read.Tx tx :: Read.Tx Read.Conway)
    RecentEraDijkstra ->
        sealedTxFromLedgerTx (Read.Tx tx :: Read.Tx Read.Dijkstra)

-- | Construct a standard unsigned transaction.
--
-- The term "standard" refers to the fact that we do not deal with redemption,
-- multi-signature transactions, etc.
--
-- This function returns a CBOR-ed transaction body, which should be signed
-- in separate step.
mkUnsignedTransaction
    :: forall era
     . Write.IsRecentEra era
    => NetworkId
    -> Either XPub (Maybe (Script KeyHash))
    -- ^ Reward account public key or optional script hash.
    -> TransactionCtx
    -- ^ Additional context about the transaction.
    -> Either PreSelection (SelectionOf TxOut)
    -- ^ A balanced coin selection where all change addresses have been
    -- assigned.
    -> Either ErrMkTransaction (Cardano.TxBody (CardanoApiEra era))
mkUnsignedTransaction networkId stakeCred ctx selection = do
    let era = Write.recentEra @era
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
            flip (replaceCosignersWithVerKeys CA.Stake) minBound
                <$> view #txStakingCredentialScriptTemplate ctx
    let depositM = view #txDeposit ctx
    let votingCerts = case view #txVotingAction ctx of
            Nothing ->
                []
            Just action -> case stakeCred of
                Left xpub ->
                    certificateFromVotingActionLedger
                        era
                        (Left xpub)
                        depositM
                        action
                Right (Just script) ->
                    certificateFromVotingActionLedger
                        era
                        (Right script)
                        depositM
                        action
                Right Nothing ->
                    error
                        $ unwords
                            [ "stakeCred in mkUnsignedTransaction must be"
                            , "either xpub or script when there is voting"
                            , "action"
                            ]
    case view #txDelegationAction ctx of
        Nothing -> do
            let md = view #txMetadata ctx
            let ourRewardAcctM =
                    FromScriptHash
                        . unScriptHash
                        . toScriptHash
                        <$> stakingScriptM
            case wdrl of
                WithdrawalSelf rewardAcct _ _ ->
                    if ourRewardAcctM == Just rewardAcct
                        then
                            constructUnsignedTx
                                networkId
                                (md, [])
                                ttl
                                wdrl
                                selection
                                delta
                                assetsToBeMinted
                                assetsToBeBurned
                                inpsScripts
                                stakingScriptM
                                refScriptM
                        else
                            constructUnsignedTx
                                networkId
                                (md, [])
                                ttl
                                wdrl
                                selection
                                delta
                                assetsToBeMinted
                                assetsToBeBurned
                                inpsScripts
                                Nothing
                                refScriptM
                _ ->
                    constructUnsignedTx
                        networkId
                        (md, votingCerts)
                        ttl
                        wdrl
                        selection
                        delta
                        assetsToBeMinted
                        assetsToBeBurned
                        inpsScripts
                        Nothing
                        refScriptM
        Just action -> do
            let delegCerts = case stakeCred of
                    Left xpub ->
                        certificateFromDelegationActionLedger
                            era
                            (Left xpub)
                            depositM
                            action
                    Right (Just script) ->
                        certificateFromDelegationActionLedger
                            era
                            (Right script)
                            depositM
                            action
                    Right Nothing ->
                        error
                            $ unwords
                                [ "stakeCred in mkUnsignedTransaction must be"
                                , "either xpub or script when there is delegation"
                                , "action"
                                ]
            let certs = L.nub $ delegCerts <> votingCerts
            let payload = (view #txMetadata ctx, certs)
            constructUnsignedTx
                networkId
                payload
                ttl
                wdrl
                selection
                delta
                assetsToBeMinted
                assetsToBeBurned
                inpsScripts
                stakingScriptM
                refScriptM

_decodeSealedTx
    :: Read.EraValue Read.Era
    -> SealedTx
    -> TxExtended
_decodeSealedTx preferredLatestEra sealedTx =
    case readSealedTxIdeallyNoLaterThan preferredLatestEra sealedTx of
        Read.EraValue tx -> getTxExtended tx

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
    :: forall era
     . Write.IsRecentEra era
    => Cardano.NetworkId
    -> (Maybe SlotNo, SlotNo)
    -> Either PreSelection (SelectionOf TxOut)
    -> Maybe TxMetadata
    -> Withdrawal
    -> [TxCert era]
    -> Coin
    -> TokenMap
    -> TokenMap
    -> Map AssetId ScriptSource
    -> Map TxIn (Script KeyHash)
    -> Maybe (Script KeyHash)
    -> Maybe (Script KeyHash)
    -> Either ErrMkTransaction (Cardano.TxBody (CardanoApiEra era))
mkUnsignedTx
    networkId
    ttl
    cs
    md
    wdrl
    certs
    fees
    mintData
    burnData
    mintingSource
    inpsScripts
    stakingScriptM
    refScriptM =
        extractValidatedOutputs cs >>= \outs ->
            let ledgerTx =
                    buildLedgerTxRaw
                        era
                        ttl
                        (ledgerNetworkOf networkId)
                        wdrl
                        fees
                        md
                        certs
                        outs
                        cs
                        (toLedgerMintValue mintData burnData)
                        scriptWitnesses
                Cardano.Tx body _ = toCardanoApiTx ledgerTx
            in  Right body
      where
        era = Write.recentEra @era

        scriptWitnesses :: ScriptWitnesses
        scriptWitnesses =
            noScriptWitnesses
                { swNativeInputs = inpsScripts
                , swStakingScript = stakingScriptM
                , swMintingSources = mintingSource
                , swReferenceScript = refScriptM
                }

mkShelleyWitness
    :: forall era
     . Write.IsRecentEra era
    => Cardano.TxBody (CardanoApiEra era)
    -> (XPrv, Passphrase "encryption")
    -> Cardano.KeyWitness (CardanoApiEra era)
mkShelleyWitness body key =
    Cardano.makeShelleyKeyWitness shelleyBasedEra body (unencrypt key)
  where
    shelleyBasedEra =
        shelleyBasedEraFromRecentEra (Write.recentEra @era)
    unencrypt (xprv, pwd) =
        Cardano.WitnessPaymentExtendedKey
            $ Cardano.PaymentExtendedSigningKey
            $ Crypto.HD.xPrvChangePass pwd BS.empty xprv

mkByronWitness
    :: forall era
     . Write.IsRecentEra era
    => Cardano.TxBody (CardanoApiEra era)
    -> Cardano.NetworkId
    -> Address
    -> (XPrv, Passphrase "encryption")
    -> Cardano.KeyWitness (CardanoApiEra era)
mkByronWitness
    ( Cardano.ShelleyTxBody
            cardanoEra
            body
            _scripts
            _scriptData
            _auxData
            _scriptValidity
        )
    nw
    addr
    encryptedKey =
        Cardano.ShelleyBootstrapWitness cardanoEra
            $ SL.makeBootstrapWitness txHash (unencrypt encryptedKey) addrAttr
      where
        era = Write.recentEra @era
        txHash = case era of
            RecentEraConway -> Crypto.castHash $ Crypto.hashWith serialize' body
            RecentEraDijkstra ->
                error "mkByronWitness: Dijkstra era not yet supported"

        unencrypt (xprv, pwd) =
            CC.SigningKey
                $ Crypto.HD.xPrvChangePass pwd BS.empty xprv

        addrAttr =
            Byron.mkAttributes
                $ Byron.AddrAttributes
                    (toHDPayloadAddress addr)
                    (Byron.toByronNetworkMagic nw)

txConstraints
    :: forall era
     . Write.IsRecentEra era
    => Write.PParams era
    -> TxWitnessTag
    -> TxConstraints
txConstraints protocolParams witnessTag =
    TxConstraints
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

    fromBTCoin :: BT.Coin -> Coin
    fromBTCoin (BT.Coin n) = Coin n

    fromBTTxSize :: BT.TxSize -> TxSize
    fromBTTxSize (BT.TxSize n) = TxSize n

    toBTTxOut :: TxOut -> BT.TxOut
    toBTTxOut (TxOut (Address a) b) =
        BT.TxOut (BT.Address a) (toBTTokenBundle b)

    toBTTokenBundle :: TokenBundle -> BT.TokenBundle
    toBTTokenBundle (TokenBundle c tm) =
        BT.TokenBundle (toBTCoin c) (toBTTokenMap tm)
      where
        toBTCoin (Coin n) = BT.Coin n
        toBTTokenMap =
            BT.fromFlatList
                . fmap toBTEntry
                . TokenMap.toFlatList
        toBTEntry
            ( AssetId
                    (UnsafeTokenPolicyId (Hash pid))
                    (UnsafeAssetName an)
                , TokenQuantity q
                ) =
                ( BT.AssetId
                    (BT.UnsafeTokenPolicyId (BT.Hash pid))
                    (BT.UnsafeAssetName an)
                , BT.TokenQuantity q
                )

    estimateTxCost' :: Write.FeePerByte -> TxSkeleton -> Coin
    estimateTxCost' fp = fromBTCoin . BT.estimateTxCost fp

    estimateTxSize' :: TxSkeleton -> TxSize
    estimateTxSize' = fromBTTxSize . BT.estimateTxSize

    txBaseCost =
        constantTxFee <> estimateTxCost' feePerByte empty

    constantTxFee =
        Convert.toWallet $ protocolParams ^. Ledger.ppMinFeeBL

    feePerByte = Write.getFeePerByte protocolParams

    txBaseSize =
        estimateTxSize' empty

    txInputCost =
        marginalCostOf empty{txInputCount = 1}

    txInputSize =
        marginalSizeOf empty{txInputCount = 1}

    txOutputCost bundle =
        marginalCostOf empty{txOutputs = [toBTTxOut (mkTxOut bundle)]}

    txOutputSize bundle =
        marginalSizeOf empty{txOutputs = [toBTTxOut (mkTxOut bundle)]}

    txOutputMaximumSize =
        (<>)
            (txOutputSize mempty)
            (TxSize (fromIntegral $ protocolParams ^. Ledger.ppMaxValSizeL))

    txOutputMaximumTokenQuantity =
        TokenQuantity $ fromIntegral $ maxBound @Word64

    txOutputMinimumAdaQuantity addr tokens =
        Convert.toWallet
            $ Write.computeMinimumCoinForTxOut
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
        TxSize $ intCast $ protocolParams ^. Ledger.ppMaxTxSizeL

    empty :: TxSkeleton
    empty =
        TxSkeleton
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
        estimateTxCost' feePerByte skeleton
            <\> estimateTxCost' feePerByte empty

    -- Computes the size difference between the given skeleton and an empty
    -- skeleton.
    marginalSizeOf :: TxSkeleton -> TxSize
    marginalSizeOf =
        (<\> txBaseSize) . estimateTxSize'

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
        :: Address
        -> TokenBundle
        -> Write.TxOut era
    mkLedgerTxOut address bundle =
        case era of
            Write.RecentEraConway -> Convert.toConwayTxOut txOut
            Write.RecentEraDijkstra ->
                error "mkLedgerTxOut: Dijkstra era not yet supported"
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
    fromBTTxSize $ Write.sizeOf_Withdrawals 1 <> wits
  where
    fromBTTxSize (BT.TxSize n) = TxSize n
    wits = case witType of
        Right TxWitnessByronUTxO ->
            Write.sizeOf_BootstrapWitnesses 1
                - Write.sizeOf_BootstrapWitnesses 0
        Right TxWitnessShelleyUTxO ->
            Write.sizeOf_VKeyWitnesses 1
        Left scriptTemplate ->
            let n =
                    fromIntegral
                        $ Write.estimateMaxWitnessRequiredPerInput
                        $ view #template scriptTemplate
            in  Write.sizeOf_VKeyWitnesses n

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
