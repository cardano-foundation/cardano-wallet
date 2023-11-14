{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2023 IOHK, 2023 Cardano Foundation
-- License: Apache-2.0
--
-- Module for 'signTx' and signing-related utilities for balancing.
module Internal.Cardano.Write.Tx.Sign
    (
    -- * Signing transactions
    -- TODO: Move signTx function here

    -- * Signing-related utilities required for balancing
      estimateSignedTxSize

    , KeyWitnessCount (..)
    , estimateKeyWitnessCount

    , estimateMaxWitnessRequiredPerInput
    , estimateMinWitnessRequiredPerInput
    )
    where

import Prelude

import Cardano.Ledger.Api
    ( Addr (..)
    , addrTxOutL
    , addrTxWitsL
    , bootAddrTxWitsL
    , ppMinFeeAL
    , sizeTxF
    , witsTxL
    )
import Cardano.Ledger.Credential
    ( Credential (..)
    )
import Cardano.Ledger.UTxO
    ( txinLookup
    )
import Cardano.Wallet.Primitive.Types.Tx.Constraints
    ( TxSize (..)
    )
import Control.Lens
    ( view
    , (&)
    , (.~)
    , (^.)
    )
import Data.Maybe
    ( mapMaybe
    )
import Data.Monoid.Monus
    ( Monus ((<\>))
    )
import Internal.Cardano.Write.Tx
    ( IsRecentEra (..)
    , KeyWitnessCount (..)
    , PParams
    , RecentEra (..)
    , ShelleyLedgerEra
    , Tx
    , TxIn
    , UTxO
    , withConstraints
    )
import Numeric.Natural
    ( Natural
    )

import qualified Cardano.Address.Script as CA
import qualified Cardano.Api as CardanoApi
import qualified Cardano.Api.Byron as CardanoApi
import qualified Cardano.Api.Shelley as CardanoApi
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import qualified Cardano.Ledger.Api as Ledger
import qualified Cardano.Wallet.Primitive.Types.Coin as W
    ( Coin (..)
    )
import qualified Cardano.Wallet.Shelley.Compatibility.Ledger as Convert
import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.Map as Map
import qualified Internal.Cardano.Write.Tx as Write

-- | Estimate the size of the transaction when fully signed.
--
-- NOTE: Existing key witnesses in the tx are ignored.
estimateSignedTxSize
    :: forall era. RecentEra era
    -> PParams (ShelleyLedgerEra era)
    -> KeyWitnessCount
    -> Tx (ShelleyLedgerEra era) -- ^ existing wits in tx are ignored
    -> TxSize
estimateSignedTxSize era pparams nWits txWithWits = withConstraints era $
    let
        -- Hack which allows us to rely on the ledger to calculate the size of
        -- witnesses:
        feeOfWits :: W.Coin
        feeOfWits = minfee nWits <\> minfee mempty

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
        sizeOfTx = withConstraints era
            $ fromIntegral @Integer @TxSize
            $ unsignedTx ^. sizeTxF
    in
        sizeOfTx <> sizeOfWits
  where
    unsignedTx :: Tx (ShelleyLedgerEra era)
    unsignedTx = withConstraints era $
        txWithWits
            & (witsTxL . addrTxWitsL) .~ mempty
            & (witsTxL . bootAddrTxWitsL) .~ mempty

    coinQuotRem :: W.Coin -> W.Coin -> (Natural, Natural)
    coinQuotRem (W.Coin p) (W.Coin q) = quotRem p q

    minfee :: KeyWitnessCount -> W.Coin
    minfee witCount = Convert.toWalletCoin $ Write.evaluateMinimumFee
        era pparams unsignedTx witCount

    feePerByte :: W.Coin
    feePerByte = withConstraints era $ Convert.toWalletCoin $
        pparams ^. ppMinFeeAL

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
    => UTxO (ShelleyLedgerEra era)
    -- ^ Must contain all inputs from the 'TxBody' or
    -- 'estimateKeyWitnessCount will 'error'.
    -> CardanoApi.TxBody era
    -> KeyWitnessCount
estimateKeyWitnessCount utxo txbody@(CardanoApi.TxBody txbodycontent) =
    let txIns = map fst $ CardanoApi.txIns txbodycontent
        txInsCollateral =
            case CardanoApi.txInsCollateral txbodycontent of
                CardanoApi.TxInsCollateral _ ins -> ins
                CardanoApi.TxInsCollateralNone -> []
        vkInsUnique = L.nub $ filter (not . hasScriptCred utxo) $
            map CardanoApi.toShelleyTxIn $
            txIns ++ txInsCollateral
        txExtraKeyWits = CardanoApi.txExtraKeyWits txbodycontent
        txExtraKeyWits' = case txExtraKeyWits of
            CardanoApi.TxExtraKeyWitnesses _ khs -> khs
            _ -> []
        txWithdrawals = CardanoApi.txWithdrawals txbodycontent
        txWithdrawals' = case txWithdrawals of
            CardanoApi.TxWithdrawals _ wdls ->
                [ () | (_, _, CardanoApi.ViewTx) <- wdls ]
            _ -> []
        txUpdateProposal = CardanoApi.txUpdateProposal txbodycontent
        txUpdateProposal' = case txUpdateProposal of
            CardanoApi.TxUpdateProposal _
                (CardanoApi.UpdateProposal updatePerGenesisKey _) ->
                    Map.size updatePerGenesisKey
            _ -> 0
        txCerts = case CardanoApi.txCertificates txbodycontent of
            CardanoApi.TxCertificatesNone -> 0
            CardanoApi.TxCertificates _ certs _ ->
                sumVia estimateDelegSigningKeys certs
        scriptVkWitsUpperBound =
            fromIntegral
            $ sumVia estimateMaxWitnessRequiredPerInput
            $ mapMaybe toTimelockScript scripts
        -- when wallets uses reference input it means script containing its
        -- policy key was already published in previous tx if so we need to add
        -- one witness that will stem from policy signing key. As it is not
        -- allowed to publish and consume in the same transaction we are not
        -- going to double count.
        txRefInpsWit = case CardanoApi.txInsReference txbodycontent of
            CardanoApi.TxInsReferenceNone -> 0
            CardanoApi.TxInsReference{} ->
                case CardanoApi.txMintValue txbodycontent of
                    CardanoApi.TxMintNone -> 0
                    CardanoApi.TxMintValue{} -> 1
        nonInputWits = numberOfShelleyWitnesses $ fromIntegral $
            length txExtraKeyWits' +
            length txWithdrawals' +
            txUpdateProposal' +
            fromIntegral txCerts +
            scriptVkWitsUpperBound +
            txRefInpsWit
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
        CardanoApi.ShelleyTxBody _ _ shelleyBodyScripts _ _ _ ->
            shelleyBodyScripts
        CardanoApi.ByronTxBody {} ->
            error "estimateKeyWitnessCount: ByronTxBody"

    dummyKeyRole = CA.Payment

    estimateDelegSigningKeys :: CardanoApi.Certificate -> Integer
    estimateDelegSigningKeys = \case
        CardanoApi.StakeAddressRegistrationCertificate _ -> 0
        CardanoApi.StakeAddressDeregistrationCertificate cred ->
            estimateWitNumForCred cred
        CardanoApi.StakeAddressPoolDelegationCertificate cred _ ->
            estimateWitNumForCred cred
        _ -> 1
      where
        -- Does not include the key witness needed for script credentials.
        -- They are accounted for separately in @scriptVkWitsUpperBound@.
        estimateWitNumForCred = \case
            CardanoApi.StakeCredentialByKey _ -> 1
            CardanoApi.StakeCredentialByScript _ -> 0
    toTimelockScript
        :: Ledger.Script (CardanoApi.ShelleyLedgerEra era)
        -> Maybe (CA.Script CA.KeyHash)
    toTimelockScript anyScript = case recentEra @era of
        RecentEraConway ->
            case anyScript of
                Alonzo.TimelockScript timelock ->
                    Just $ Convert.toWalletScript (const dummyKeyRole) timelock
                Alonzo.PlutusScript _ _ -> Nothing
        RecentEraBabbage ->
            case anyScript of
                Alonzo.TimelockScript timelock ->
                    Just $ Convert.toWalletScript (const dummyKeyRole) timelock
                Alonzo.PlutusScript _ _ -> Nothing

    hasScriptCred
        :: UTxO (ShelleyLedgerEra era)
        -> TxIn
        -> Bool
    hasScriptCred u inp = withConstraints (recentEra @era) $
        case view addrTxOutL <$> txinLookup inp u of
            Just (Addr _ (KeyHashObj _) _) -> False
            Just (Addr _ (ScriptHashObj _) _) -> True
            Just (AddrBootstrap _) -> False
            Nothing ->
                error $ unwords
                    [ "estimateMaxWitnessRequiredPerInput: input not in utxo."
                    , "Caller is expected to ensure this does not happen."
                    ]

    hasBootstrapAddr
        :: UTxO (ShelleyLedgerEra era)
        -> TxIn
        -> Bool
    hasBootstrapAddr u inp = withConstraints (recentEra @era) $
        case view addrTxOutL <$> txinLookup inp u of
            Just Addr{} -> False
            Just (AddrBootstrap _) -> True
            Nothing ->
                error $ unwords
                    [ "estimateMaxWitnessRequiredPerInput: input not in utxo."
                    , "Caller is expected to ensure this does not happen."
                    ]

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- Small helper function for summing values. Given a list of values, get the sum
-- of the values, after the given function has been applied to each value.
sumVia :: (Foldable t, Num m) => (a -> m) -> t a -> m
sumVia f = F.foldl' (\t -> (t +) . f) 0

estimateMinWitnessRequiredPerInput :: CA.Script k -> Natural
estimateMinWitnessRequiredPerInput = \case
    CA.RequireSignatureOf _ -> 1
    CA.RequireAllOf xs      ->
        sum $ map estimateMinWitnessRequiredPerInput xs
    CA.RequireAnyOf xs      ->
        optimumIfNotEmpty minimum $ map estimateMinWitnessRequiredPerInput xs
    CA.RequireSomeOf m xs   ->
        let smallestReqFirst =
                L.sort $ map estimateMinWitnessRequiredPerInput xs
        in sum $ take (fromIntegral m) smallestReqFirst
    CA.ActiveFromSlot _     -> 0
    CA.ActiveUntilSlot _    -> 0

optimumIfNotEmpty :: (Foldable t, Num p) => (t a -> p) -> t a -> p
optimumIfNotEmpty f xs =
    if null xs then
        0
    else f xs

estimateMaxWitnessRequiredPerInput :: CA.Script k -> Natural
estimateMaxWitnessRequiredPerInput = \case
    CA.RequireSignatureOf _ -> 1
    CA.RequireAllOf xs      ->
        sum $ map estimateMaxWitnessRequiredPerInput xs
    CA.RequireAnyOf xs      ->
        sum $ map estimateMaxWitnessRequiredPerInput xs
    -- Estimate (and tx fees) could be lowered with:
    --
    -- optimumIfNotEmpty maximum $ map estimateMaxWitnessRequiredPerInput xs
    -- however signTransaction
    --
    -- however we'd then need to adjust signTx accordingly such that it still
    -- doesn't add more witnesses than we plan for.
    --
    -- Partially related task:
    -- https://cardanofoundation.atlassian.net/browse/ADP-2676
    CA.RequireSomeOf _m xs   ->
        sum $ map estimateMaxWitnessRequiredPerInput xs
    -- Estimate (and tx fees) could be lowered with:
    --
    -- let largestReqFirst =
    --      reverse $ L.sort $ map estimateMaxWitnessRequiredPerInput xs
    -- in sum $ take (fromIntegral m) largestReqFirst
    --
    -- however we'd then need to adjust signTx accordingly such that it still
    -- doesn't add more witnesses than we plan for.
    --
    -- Partially related task:
    -- https://cardanofoundation.atlassian.net/browse/ADP-2676
    CA.ActiveFromSlot _     -> 0
    CA.ActiveUntilSlot _    -> 0
