{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2023 IOHK
-- License: Apache-2.0
--
-- Module for 'signTx' and signing-related utilities for balancing.
module Cardano.Wallet.Write.Tx.Sign
    (
    -- * Signing transactions
    -- TODO: Move signTx function here

    -- * Signing-related utilities required for balancing
      estimateSignedTxSize

    , KeyWitnessCount (..)
    , estimateKeyWitnessCount
    )
    where

import Prelude

import Cardano.Ledger.Api
    ( ppMinFeeAL )
import Cardano.Wallet.Address.Discovery.Shared
    ( estimateMaxWitnessRequiredPerInput )
import qualified Cardano.Wallet.Primitive.Types.Coin as W
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.Tx
    ( sealedTxFromCardanoBody, serialisedTx )
import Cardano.Wallet.Primitive.Types.Tx.Constraints
    ( TxSize (..) )
import Cardano.Wallet.Shelley.Compatibility.Ledger
    ( toWalletCoin, toWalletScript )
import Cardano.Wallet.Write.Tx
    ( IsRecentEra (..), KeyWitnessCount (..), RecentEra (..) )
import Control.Lens
    ( (^.) )
import Data.Maybe
    ( mapMaybe )
import Numeric.Natural
    ( Natural )

import qualified Cardano.Address.Script as CA
import qualified Cardano.Api as Cardano
import qualified Cardano.Api.Byron as Byron
import qualified Cardano.Api.Shelley as Cardano
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import qualified Cardano.Ledger.Api as Ledger
import qualified Cardano.Wallet.Primitive.Types.Coin as W.Coin
import qualified Cardano.Wallet.Shelley.Compatibility.Ledger as Ledger
import qualified Cardano.Wallet.Write.Tx as Write
import qualified Data.ByteString as BS
import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.Map as Map

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
        feeOfWits :: W.Coin
        feeOfWits = minfee nWits `W.Coin.difference` minfee mempty

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
    coinQuotRem :: W.Coin -> W.Coin -> (Natural, Natural)
    coinQuotRem (W.Coin p) (W.Coin q) = quotRem p q

    minfee :: KeyWitnessCount -> W.Coin
    minfee witCount = toWalletCoin $ Write.evaluateMinimumFee
        (recentEra @era) pparams (toLedgerTx body) witCount

    toLedgerTx :: Cardano.TxBody era -> Write.Tx (Write.ShelleyLedgerEra era)
    toLedgerTx b = case Cardano.Tx b [] of
        Byron.ByronTx {} -> case Write.recentEra @era of {}
        Cardano.ShelleyTx _era ledgerTx -> ledgerTx

    feePerByte :: W.Coin
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

    dummyKeyRole = CA.Payment

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
        -> Maybe (CA.Script CA.KeyHash)
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

--
-- Helpers
--

-- Small helper function for summing values. Given a list of values, get the sum
-- of the values, after the given function has been applied to each value.
sumVia :: (Foldable t, Num m) => (a -> m) -> t a -> m
sumVia f = F.foldl' (\t -> (t +) . f) 0
