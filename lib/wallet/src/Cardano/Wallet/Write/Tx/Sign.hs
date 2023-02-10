{-# LANGUAGE ExplicitForAll #-}
module Cardano.Wallet.Write.Tx.Sign where

import Prelude

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
    => Cardano.UTxO era
    -- ^ Must contain all inputs from the 'TxBody' or
    -- 'estimateNumberOfWitnesses' will 'error'.
    -> Cardano.TxBody era
    -> Word
estimateNumberOfWitnesses utxo txbody@(Cardano.TxBody txbodycontent) =
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
                length $ filter (not . isStakeKeyRegCert) certs
        scriptVkWitsUpperBound =
            fromIntegral
            $ sumVia estimateMaxWitnessRequiredPerInput
            $ mapMaybe toTimelockScript scripts
    in
    fromIntegral $
        length vkInsUnique +
        length txExtraKeyWits' +
        length txWithdrawals' +
        txUpdateProposal' +
        txCerts +
        scriptVkWitsUpperBound
  where
    (Cardano.ShelleyTxBody _ _ scripts _ _ _) = txbody

    dummyKeyRole = Payment

    isStakeKeyRegCert (Cardano.StakeAddressRegistrationCertificate _) = True
    isStakeKeyRegCert _ = False

    toTimelockScript
        :: Ledger.Script (Cardano.ShelleyLedgerEra era)
        -> Maybe (Script KeyHash)
    toTimelockScript anyScript = case Cardano.shelleyBasedEra @era of
        Cardano.ShelleyBasedEraBabbage ->
            case anyScript of
                (Alonzo.TimelockScript timelock)
                    -> Just $ toWalletScript (const dummyKeyRole) timelock
                (Alonzo.PlutusScript _ _)
                    -> Nothing
        Cardano.ShelleyBasedEraAlonzo ->
            case anyScript of
                (Alonzo.TimelockScript timelock)
                    -> Just $ toWalletScript (const dummyKeyRole) timelock
                (Alonzo.PlutusScript _ _)
                    -> Nothing
        Cardano.ShelleyBasedEraMary ->
            Just $ toWalletScript (const dummyKeyRole) anyScript
        Cardano.ShelleyBasedEraAllegra ->
            Just $ toWalletScript (const dummyKeyRole) anyScript
        Cardano.ShelleyBasedEraShelley ->
            Just $ toWalletScriptFromShelley dummyKeyRole anyScript

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
