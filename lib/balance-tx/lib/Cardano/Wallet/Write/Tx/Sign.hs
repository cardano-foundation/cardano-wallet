{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2023 IOHK, 2023 Cardano Foundation
-- License: Apache-2.0
--
-- Module for 'signTx' and signing-related utilities for balancing.
module Cardano.Wallet.Write.Tx.Sign
    (
    -- * Signing transactions
      signTx
    , KeyStore (..)
    , keyStoreFromXPrv
    , keyStoreFromMaybeXPrv
    , keyStoreFromKeyHashLookup
    , keyStoreFromAddressLookup
    , keyStoreFromBootstrapAddressLookup

    , KeyHash'
    , keyHashToBytes
    , keyHashFromBytes
    , keyHashFromXPrv
    -- * Signing-related utilities required for balancing
    , estimateSignedTxSize

    , KeyWitnessCount (..)
    , estimateKeyWitnessCount

    , estimateMaxWitnessRequiredPerInput
    , estimateMinWitnessRequiredPerInput
    )
    where

import Prelude

import Cardano.Crypto.Hash.Class
    ( hashFromBytes, hashToBytes )
import Cardano.Crypto.Wallet
    ( XPrv, toXPub, xpubPublicKey )
import Cardano.Ledger.Alonzo.Tx
    ( ScriptPurpose, sizeAlonzoTxF )
import Cardano.Ledger.Alonzo.UTxO
    ( AlonzoScriptsNeeded (..) )
import Cardano.Ledger.Api
    ( Addr (..)
    , BootstrapAddress (unBootstrapAddress)
    , BootstrapWitness
    , Script
    , ScriptHash
    , addrTxOutL
    , addrTxWitsL
    , bodyTxL
    , bootAddrTxWitsL
    , collateralInputsTxBodyL
    , inputsTxBodyL
    , ppMinFeeAL
    , reqSignerHashesTxBodyL
    , sizeTxF
    , witsTxL
    )
import Cardano.Ledger.Api
    ( Addr (..)
    , AlonzoEraTxBody
    , Era (EraCrypto)
    , EraTx
    , addrTxOutL
    , ppMinFeeAL
    )
import Cardano.Ledger.Api
    ( StandardCrypto )
import Cardano.Ledger.Api
    ( WitVKey )
import Cardano.Ledger.Api.UTxO
    ( EraUTxO (..) )
import Cardano.Ledger.Credential
    ( Credential (..) )
import Cardano.Ledger.Keys
    ( GenDelegs, KeyHash (..) )
import Cardano.Ledger.Keys
    ( GenDelegs (GenDelegs), KeyRole (Witness) )
import Cardano.Ledger.Shelley.API
    ( addKeyWitnesses )
import Cardano.Ledger.UTxO
    ( txinLookup )
import qualified Cardano.Wallet.Primitive.Types.Coin as W
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.Tx.Constraints
    ( TxSize (..) )
import Cardano.Wallet.Shelley.Compatibility.Ledger
    ( Convert (toWallet), toWalletCoin, toWalletScript )
import Cardano.Wallet.Write.Tx
    ( Address
    , IsRecentEra (..)
    , KeyWitnessCount (..)
    , PParams
    , RecentEra (..)
    , ShelleyLedgerEra
    , Tx
    , TxIn
    , TxOut
    , UTxO
    , toCardanoTx
    , txBody
    , withConstraints
    )
import Control.Applicative
    ( (<|>) )
import Control.Lens
    ( view, (%~), (&), (.~), (^.) )
import Crypto.Hash.Extra
    ( blake2b224 )
import Data.ByteString
    ( ByteString )
import Data.Maybe
    ( catMaybes, fromMaybe, mapMaybe )
import Data.Set
    ( Set )
import Numeric.Natural
    ( Natural )

import qualified Cardano.Address.Script as CA
import qualified Cardano.Api as Cardano
import qualified Cardano.Api.Byron as Byron
import qualified Cardano.Api.Shelley as Cardano
import Cardano.Chain.Common
    ( addrAttributesUnwrapped )
import qualified Cardano.Chain.Common as Byron
import qualified Cardano.Crypto as CC
import qualified Cardano.Crypto.Hash as Crypto
import qualified Cardano.Crypto.Wallet as Crypto.HD
import qualified Cardano.Ledger.Address as SL
import qualified Cardano.Ledger.Alonzo.Rules as Alonzo.Rules
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import qualified Cardano.Ledger.Alonzo.TxInfo as Alonzo
import qualified Cardano.Ledger.Api as Ledger
import Cardano.Ledger.Binary.Plain
    ( serialize' )
import qualified Cardano.Ledger.Keys.Bootstrap as SL
import qualified Cardano.Wallet.Primitive.Types.Address as W
import qualified Cardano.Wallet.Primitive.Types.Coin as W.Coin
import qualified Cardano.Wallet.Shelley.Compatibility.Ledger as Ledger
import qualified Cardano.Wallet.Write.Tx as Write
import qualified Codec.CBOR.Decoding as CBOR
import qualified Data.ByteString as BS
import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.Map as Map
import qualified Data.Set as Set

--------------------------------------------------------------------------------
-- Signing transactions
--------------------------------------------------------------------------------

type KeyHash' = KeyHash 'Witness StandardCrypto

keyHashToBytes :: KeyHash' -> ByteString
keyHashToBytes (KeyHash h) = hashToBytes h

keyHashFromBytes :: ByteString -> Maybe KeyHash'
keyHashFromBytes = fmap KeyHash . hashFromBytes

keyHashFromXPrv :: XPrv -> KeyHash'
keyHashFromXPrv = fromMaybe (error "keyStoreFromXPrv: unable to decode xprv")
    . keyHashFromBytes . blake2b224 . xpubPublicKey . toXPub

-- FIXME: Use a 'Map' instead?
data KeyStore = KeyStore
    { resolveKeyHash :: KeyHash 'Witness StandardCrypto -> Maybe XPrv

-- TODO: Change isOurs to take fingerprint instead, then we don't need
-- resolveAddress, and can rely on resolveKeyHash instead.
-- resolveBootstrapAddress is fine to keep.
    , resolveAddress :: Address -> Maybe XPrv

    , resolveBootstrapAddress :: BootstrapAddress StandardCrypto -> Maybe XPrv
    }

instance Semigroup KeyStore where
    (KeyStore f g h) <> (KeyStore f' g' h') =
        KeyStore (alt1 f f') (alt1 g g') (alt1 h h')
      where
        alt1 a b x = a x <|> b x

instance Monoid KeyStore where
    mempty = KeyStore (const Nothing) (const Nothing) (const Nothing)


-- | NOTE: 'XPrv' must be unencrypted!
keyStoreFromXPrv :: XPrv -> KeyStore
keyStoreFromXPrv xprv =
    let
        h = keyHashFromXPrv xprv
    in
        KeyStore
            (\h' -> if h == h' then Just xprv else Nothing)
            (const Nothing)
            (const Nothing)

keyStoreFromMaybeXPrv :: Maybe XPrv -> KeyStore
keyStoreFromMaybeXPrv = maybe mempty keyStoreFromXPrv


keyStoreFromBootstrapAddressLookup
    :: (BootstrapAddress StandardCrypto -> Maybe XPrv)
    -> KeyStore
keyStoreFromBootstrapAddressLookup = KeyStore (const Nothing) (const Nothing)

keyStoreFromAddressLookup
    :: (Address -> Maybe XPrv)
    -> KeyStore
keyStoreFromAddressLookup f =
    KeyStore (const Nothing) f (const Nothing)

keyStoreFromKeyHashLookup
    :: (KeyHash 'Witness StandardCrypto -> Maybe XPrv)
    -> KeyStore
keyStoreFromKeyHashLookup f =
    KeyStore
        f
        (const Nothing)
        (const Nothing)

signTx
    :: forall era. IsRecentEra era => RecentEra era
    -> KeyStore
    -> UTxO (ShelleyLedgerEra era)
    -> Tx (ShelleyLedgerEra era)
    -> Tx (ShelleyLedgerEra era)
signTx era keyStore utxo tx =
    let
        needed = timelockVKeyNeeded
            <> witsVKeyNeeded era utxo tx noGenDelegs
        availibleKeys = mconcat
            [ mapMaybe (resolveKeyHash keyStore) $ Set.toList needed
            , mapMaybe (resolveAddress keyStore) (shelleyInputAddrs tx)
            ]

        wits = Set.fromList $ map (mkShelleyWitness tx) availibleKeys

        bootWits = Set.fromList $ catMaybes $ do
            bootAddr <- byronInputAddrs tx
            case resolveBootstrapAddress keyStore bootAddr of
                Just xprv -> return $ Just $ mkByronWitness tx bootAddr xprv
                Nothing -> return Nothing

    in
        withConstraints era
            $ flip addKeyWitnesses wits
            $ addBootstrapWitnesses era tx bootWits
  where
    -- TODO: We need to add the logic for these ourselves
    timelockVKeyNeeded = mempty

    -- We probably don't need this?
    noGenDelegs = GenDelegs mempty

    -- TODO: Check for completeness.
    inputAddresses
        :: Tx (ShelleyLedgerEra era)
        -> [Address]
    inputAddresses tx = withConstraints era
        $ map (view addrTxOutL)
        $ mapMaybe (`txinLookup` utxo)
        $ Set.toList
        $ mconcat
            [ tx ^. bodyTxL . inputsTxBodyL
            , tx ^. bodyTxL . collateralInputsTxBodyL
            ]

    byronInputAddrs = mapMaybe asBootAddr . inputAddresses
      where
        asBootAddr :: Address -> Maybe (BootstrapAddress StandardCrypto)
        asBootAddr = \case
            AddrBootstrap bootAddr -> Just bootAddr
            _ -> Nothing

    shelleyInputAddrs = mapMaybe asAddr . inputAddresses
      where
        asAddr :: Address -> Maybe Address
        asAddr = \case
            a@Addr{} -> Just a
            _ -> Nothing

    mkShelleyWitness
        :: Tx (ShelleyLedgerEra era)
        -> XPrv
        -> WitVKey 'Witness StandardCrypto
    mkShelleyWitness tx key = withConstraints era $
        toLedgerWit $ Cardano.makeShelleyKeyWitness (toCardanoTxBody tx)
            $ Cardano.WitnessPaymentExtendedKey
            $ Cardano.PaymentExtendedSigningKey key
      where
        toLedgerWit (Cardano.ShelleyKeyWitness _ w) = w

        toCardanoTxBody :: Tx (ShelleyLedgerEra era) -> Cardano.TxBody era
        toCardanoTxBody tx = withConstraints era $
            let
                Cardano.Tx body _ = toCardanoTx tx
            in
                body
    mkByronWitness
        :: Tx (ShelleyLedgerEra era)
        -> BootstrapAddress StandardCrypto
        -> XPrv
        -> BootstrapWitness StandardCrypto
    mkByronWitness tx addr key = withConstraints era $
            SL.makeBootstrapWitness
                (Crypto.castHash $ Crypto.hashWith serialize' $ tx ^. bodyTxL)
                (CC.SigningKey key)
                attributes

      where
        toLedgerWit (Cardano.ShelleyKeyWitness _ w) = w

        networkMagic = Byron.aaNetworkMagic . addrAttributesUnwrapped . unBootstrapAddress $ addr

        attributes =
            Byron.mkAttributes Byron.AddrAttributes {
              Byron.aaVKDerivationPath = derivationPath,
              Byron.aaNetworkMagic     = networkMagic
            }

        derivationPath :: Maybe Byron.HDAddressPayload
        derivationPath =
            Byron.aaVKDerivationPath . addrAttributesUnwrapped . unBootstrapAddress $ addr


addBootstrapWitnesses
    :: RecentEra era
    -> Tx (ShelleyLedgerEra era)
    -> Set (SL.BootstrapWitness StandardCrypto)
    -> Tx (ShelleyLedgerEra era)
addBootstrapWitnesses era tx newWits = withConstraints era $
    tx & witsTxL . bootAddrTxWitsL %~ Set.union newWits

-- | Re-exposed version of 'Alonzo.Rules.witsVKeyNeeded'
witsVKeyNeeded
    :: forall era. RecentEra era
    -> UTxO (ShelleyLedgerEra era)
    -> Tx (ShelleyLedgerEra era)
    -> GenDelegs StandardCrypto
    -> Set (KeyHash 'Witness StandardCrypto)
witsVKeyNeeded era utxo tx gd = extra <> timelockVKeyNeeded <> case era of
    RecentEraBabbage -> Alonzo.Rules.witsVKeyNeeded utxo tx gd
    RecentEraConway -> Alonzo.Rules.witsVKeyNeeded utxo tx gd
  where
    -- Treated separately
    -- https://github.com/input-output-hk/cardano-ledger/blob/513857a0d802a6c0e96f8f79bd1af2337bd42e4e/eras/alonzo/impl/src/Cardano/Ledger/Alonzo/Rules/Utxow.hs#L373-L376
    extra = withConstraints era $ tx ^. (bodyTxL . reqSignerHashesTxBodyL)

    -- NOTE: ScriptPurpose is available if we wanted to display it to users.
    scriptsNeeded :: [(ScriptHash StandardCrypto)]
    scriptsNeeded = case era of
        RecentEraBabbage -> unwrap $ getScriptsNeeded utxo (tx ^. bodyTxL)
        RecentEraConway -> unwrap $ getScriptsNeeded utxo (tx ^. bodyTxL)
      where
        unwrap (AlonzoScriptsNeeded x) = map snd x

    scripts :: [Script (ShelleyLedgerEra era)]
    scripts = mapMaybe
        (`Map.lookup` (withConstraints era $ Alonzo.txscripts utxo tx))
        scriptsNeeded

    timelockScripts = mapMaybe (toTimelockScript era) scripts

    -- timelockVKeyNeeded = sum $ map estimateMaxWitnessRequiredPerInput timelockScripts

    -- FIXME: estimateKeyWitnessCount must not count all key hashes!
    -- Only signTx should count all! Move this there instead!
    timelockVKeyNeeded = timelockHashes <> Set.fromList (retrieveAllKeyHashes =<< timelockScripts)

    -- FIXME: HACK because shared wallets have 'ScriptHash -> addrXPrv' lookup,
    -- not 'KeyHash -> addrXPrv' lookup.
    timelockHashes = Set.fromList $ map (fromCAScriptHash . CA.toScriptHash) timelockScripts
      where
        fromCAScriptHash = fromMaybe err . keyHashFromBytes . CA.unScriptHash
          where
            err = error "fromCAKeyHash invalid hash"

    retrieveAllKeyHashes :: CA.Script CA.KeyHash -> [KeyHash']
    retrieveAllKeyHashes =
         map fromCAKeyHash . CA.foldScript (:) []
      where
        fromCAKeyHash = fromMaybe err . keyHashFromBytes . CA.digest
          where
            err = error "fromCAKeyHash invalid hash"

--------------------------------------------------------------------------------
-- Other
--------------------------------------------------------------------------------

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
    minfee witCount = toWalletCoin $ Write.evaluateMinimumFee
        era pparams unsignedTx witCount

    feePerByte :: W.Coin
    feePerByte = withConstraints era $ Ledger.toWalletCoin $
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
    -> Cardano.TxBody era
    -> KeyWitnessCount
estimateKeyWitnessCount utxo txbody@(Cardano.TxBody txbodycontent) =
    let txIns = map fst $ Cardano.txIns txbodycontent
        txInsCollateral =
            case Cardano.txInsCollateral txbodycontent of
                Cardano.TxInsCollateral _ ins -> ins
                Cardano.TxInsCollateralNone -> []
        vkInsUnique = L.nub $ filter (not . hasScriptCred utxo) $
            map Cardano.toShelleyTxIn $
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
            $ mapMaybe (toTimelockScript (recentEra @era)) scripts
        -- when wallets uses reference input it means script containing
        -- its policy key was already published in previous tx
        -- if so we need to add one witness that will stem from policy signing
        -- key. As it is not allowed to publish and consume in the same transaction
        -- we are not going to double count.
        txRefInpsWit = case Cardano.txInsReference txbodycontent of
            Cardano.TxInsReferenceNone -> 0
            Cardano.TxInsReference{} ->
                case Cardano.txMintValue txbodycontent of
                    Cardano.TxMintNone -> 0
                    Cardano.TxMintValue{} -> 1
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
        Cardano.ShelleyTxBody _ _ shelleyBodyScripts _ _ _ -> shelleyBodyScripts
        Byron.ByronTxBody {} -> error "estimateKeyWitnessCount: ByronTxBody"

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
    -- Partially related task: https://cardanofoundation.atlassian.net/browse/ADP-2676
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
    -- Partially related task: https://cardanofoundation.atlassian.net/browse/ADP-2676
    CA.ActiveFromSlot _     -> 0
    CA.ActiveUntilSlot _    -> 0


toTimelockScript
    :: forall era. RecentEra era
    -> Ledger.Script (Cardano.ShelleyLedgerEra era)
    -> Maybe (CA.Script CA.KeyHash)
toTimelockScript era anyScript = case era of
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

  where
    dummyKeyRole = CA.Payment
