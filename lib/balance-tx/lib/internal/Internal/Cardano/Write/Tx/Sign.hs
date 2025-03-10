{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

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
    , estimateSignedTxMinFee

    , KeyWitnessCounts (..)
    , TimelockKeyWitnessCounts (..)
    , estimateKeyWitnessCounts

    , estimateMaxWitnessRequiredPerInput
    , estimateMinWitnessRequiredPerInput
    )
    where

import Prelude

import Cardano.Api.Ledger
    ( Coin
    )
import Cardano.Ledger.Allegra.Scripts
    ( Timelock
    )
import Cardano.Ledger.Api
    ( Addr (..)
    , ScriptHash
    , StandardCrypto
    , addrTxOutL
    , addrTxWitsL
    , bodyTxL
    , bootAddrTxWitsL
    , scriptTxWitsL
    , sizeTxF
    , witsTxL
    )
import Cardano.Ledger.Credential
    ( Credential (..)
    , StakeCredential
    )
import Cardano.Ledger.Tools
    ( addDummyWitsTx
    )
import Cardano.Ledger.UTxO
    ( EraUTxO (getScriptsHashesNeeded, getScriptsNeeded)
    , getMinFeeTxUtxo
    , txinLookup
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
import Data.IntCast
    ( intCast
    , intCastMaybe
    )
import Data.Map.Strict
    ( Map
    )
import Data.Maybe
    ( fromMaybe
    , mapMaybe
    )
import Data.Set
    ( Set
    )
import Internal.Cardano.Write.Eras
    ( CardanoApiEra
    , IsRecentEra (..)
    )
import Internal.Cardano.Write.Tx
    ( KeyWitnessCounts (..)
    , PParams
    , Script
    , Tx
    , TxIn
    , UTxO
    , feeOfBytes
    , getFeePerByte
    , toCardanoApiTx
    )
import Numeric.Natural
    ( Natural
    )

import qualified Cardano.Address.Script as CA
import qualified Cardano.Api as CardanoApi
import qualified Cardano.Api.Shelley as CardanoApi
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import qualified Cardano.Ledger.Api as Ledger
import qualified Cardano.Ledger.Api.Tx.Cert as Conway
import qualified Cardano.Ledger.Shelley.TxCert as Shelley
import qualified Cardano.Wallet.Primitive.Ledger.Convert as Convert
import qualified Cardano.Wallet.Primitive.Types.Tx.Constraints as W
    ( TxSize (..)
    )
import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.Map as Map
import qualified GHC.IsList as IsList

-- | Estimate the size of the transaction when fully signed.
--
-- NOTE: Existing key witnesses in the tx are ignored.
estimateSignedTxSize
    :: forall era. IsRecentEra era
    => PParams era
    -> KeyWitnessCounts
    -> Tx era -- ^ existing wits in tx are ignored
    -> W.TxSize
estimateSignedTxSize pparams (KeyWitnessCounts nWit nBoot)tx =
    TxSize
    . (+ sizeOf_BootstrapWitnesses (intCast nBoot))
    . integerToNatural
    . view sizeTxF
    $ mockWitnesses nWit pparams tx
  where
    integerToNatural = fromMaybe (error "estimateSignedTxSize: negative size")
        . intCastMaybe

numberOfShelleyWitnesses :: Word -> KeyWitnessCounts
numberOfShelleyWitnesses n = KeyWitnessCounts n 0

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
estimateKeyWitnessCounts
    :: forall era. IsRecentEra era
    => UTxO era
    -- ^ Must contain all inputs from the 'TxBody' or
    -- 'estimateKeyWitnessCounts will 'error'.
    -> Tx era
    -> TimelockKeyWitnessCounts
    -- ^ Specifying the intended number of timelock script key witnesses may
    -- save space and fees when constructing a transaction.
    --
    -- Timelock scripts without entries in this map will have their key witness
    -- counts estimated according to 'estimateMaxWitnessRequiredPerInput'.
    -> KeyWitnessCounts
estimateKeyWitnessCounts utxo tx timelockKeyWitCounts =
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
            CardanoApi.TxCertificates _ certs ->
                sumVia estimateDelegSigningKeys $ fst <$> IsList.toList certs
        nonInputWits = numberOfShelleyWitnesses $ fromIntegral $
            length txExtraKeyWits' +
            length txWithdrawals' +
            txUpdateProposal' +
            fromIntegral txCerts +
            fromIntegral timelockTotalWitCount
        inputWits = KeyWitnessCounts
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
    CardanoApi.Tx body _keyWits
        = toCardanoApiTx tx
    txbodycontent = CardanoApi.getTxBodyContent body
    timelockTotalWitCount :: Natural
    timelockTotalWitCount = sum $ Map.elems $ Map.unionWith
        (\_est spec -> spec) -- Allow specified values to override
        upperBoundEstimatedTimelockKeyWitnessCounts
        specifiedTimelockKeyWitnessCounts
      where
        specifiedTimelockKeyWitnessCounts
            :: Map (ScriptHash StandardCrypto) Natural
        specifiedTimelockKeyWitnessCounts = Map.fromList $ mapMaybe resolve
            $ F.toList scriptsNeeded
          where
            resolve
                :: (ScriptHash StandardCrypto)
                -> Maybe (ScriptHash StandardCrypto, Natural)
            resolve h = (h,) <$> Map.lookup h
                (getTimelockKeyWitnessCounts timelockKeyWitCounts)

        upperBoundEstimatedTimelockKeyWitnessCounts
            :: Map (ScriptHash StandardCrypto) Natural
        upperBoundEstimatedTimelockKeyWitnessCounts = Map.mapMaybe
            (fmap (estimateMaxWitnessRequiredPerInput . toCAScript)
                . toTimelockScript)
            -- TODO [ADP-2675]
            -- https://cardanofoundation.atlassian.net/browse/ADP-2675
            -- Use `txscripts` restricted by `scriptsNeeded` instead. This would
            -- 1. take referenced scripts into account
            -- 2. ignore all non-needed scripts
            scriptsAvailableInBody

        scriptsNeeded :: Set (ScriptHash StandardCrypto)
        scriptsNeeded =
            getScriptsHashesNeeded
            $ getScriptsNeeded utxo
            $ view bodyTxL tx

        scriptsAvailableInBody :: Map (ScriptHash StandardCrypto) (Script era)
        scriptsAvailableInBody = tx ^. witsTxL . scriptTxWitsL

    estimateDelegSigningKeys
        :: CardanoApi.Certificate (CardanoApiEra era)
        -> Integer
    estimateDelegSigningKeys = \case
        CardanoApi.ShelleyRelatedCertificate s2b shelleyCert ->
            CardanoApi.shelleyToBabbageEraConstraints s2b $
                case shelleyCert of
                    Shelley.RegTxCert _ -> 0
                    Shelley.DelegStakeTxCert c _ -> estimateWitNumForCred c
                    Shelley.UnRegTxCert c -> estimateWitNumForCred c
                    _ -> 1
        CardanoApi.ConwayCertificate conway conwayCert ->
            CardanoApi.conwayEraOnwardsConstraints conway $
                case conwayCert of
                    Conway.RegTxCert _ -> 0
                    Conway.DelegStakeTxCert c _ -> estimateWitNumForCred c
                    Conway.UnRegTxCert c -> estimateWitNumForCred c
                    _ -> 1
      where
        -- Does not include the key witness needed for script credentials.
        -- They are accounted for separately in @scriptVkWitsUpperBound@.
        estimateWitNumForCred :: StakeCredential c -> Integer
        estimateWitNumForCred = \case
            KeyHashObj _ -> 1
            ScriptHashObj _ -> 0

    toCAScript = Convert.toWalletScript (const dummyKeyRole)
      where
        dummyKeyRole = CA.Payment

    toTimelockScript
        :: Ledger.Script era
        -> Maybe (Timelock era)
    toTimelockScript (Alonzo.TimelockScript timelock) = Just timelock
    toTimelockScript (Alonzo.PlutusScript _)          = Nothing

    hasScriptCred
        :: UTxO era
        -> TxIn
        -> Bool
    hasScriptCred u inp =
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
        :: UTxO era
        -> TxIn
        -> Bool
    hasBootstrapAddr u inp =
        case view addrTxOutL <$> txinLookup inp u of
            Just Addr{} -> False
            Just (AddrBootstrap _) -> True
            Nothing ->
                error $ unwords
                    [ "estimateMaxWitnessRequiredPerInput: input not in utxo."
                    , "Caller is expected to ensure this does not happen."
                    ]

-- | Used to specify the intended number of timelock script key witnesses.
--
-- The 'Semigroup' instance resolves conflicts using 'max'.
newtype TimelockKeyWitnessCounts = TimelockKeyWitnessCounts
    { getTimelockKeyWitnessCounts
        :: Map (ScriptHash StandardCrypto) Natural
    }
    deriving (Show, Eq)
    deriving newtype (Monoid)

instance Semigroup TimelockKeyWitnessCounts where
    (TimelockKeyWitnessCounts a) <> (TimelockKeyWitnessCounts b)
        = TimelockKeyWitnessCounts (Map.unionWith max a b)

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

estimateSignedTxMinFee
    :: forall era. IsRecentEra era
    => PParams era
    -> UTxO era
    -> Tx era
    -> KeyWitnessCounts
    -> Coin
estimateSignedTxMinFee pp utxo tx (KeyWitnessCounts nWit nBoot) =
    -- NOTE: We don't use mock bootstrap witnesses, but rely on the same
    -- size estimation as coin selection does through 'estimateTxSize'
    getMinFeeTxUtxo pp (mockWitnesses nWit pp tx) utxo
        <> bootWitFee
  where
    bootWitFee =
        feeOfBytes
            (getFeePerByte pp)
            (sizeOf_BootstrapWitnesses $ intCast nBoot)

-- Matching the corresponding implementation in "Cardano.Write.Tx.SizeEstimation".
-- Their equivalence is tested in a property test.
sizeOf_BootstrapWitnesses :: Natural -> Natural
sizeOf_BootstrapWitnesses 0 = 0
sizeOf_BootstrapWitnesses n = 4 + 180 * n

-- | Adds 'n' number of mock key witnesses to the tx. Any preexisting witnesses
-- will first be removed.
mockWitnesses
    :: forall era. IsRecentEra era
    => Word -- Key witnesses
    -> PParams era
    -> Tx era
    -> Tx era
mockWitnesses nWits pp tx =
    addDummyWitsTx
        pp
        (dropWits tx)
        (wordToInt nWits)
        [] -- no byron witnesses
  where
    wordToInt :: Word -> Int
    wordToInt = fromMaybe (error "addDummyKeyWitnesses") . intCastMaybe

    dropWits :: Tx era -> Tx era
    dropWits x = x
        & (witsTxL . bootAddrTxWitsL) .~ mempty
        & (witsTxL . addrTxWitsL) .~ mempty
