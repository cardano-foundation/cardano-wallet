{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Copyright: © 2018-2021 IOHK
-- License: Apache-2.0
--
-- An implementation of shared script state using
-- scheme specified in CIP-1854 Multi-signature Wallets.

module Cardano.Wallet.Address.Keys.Shared
    ( mkSharedStateFromAccountXPub
    , mkSharedStateFromRootXPrv
    , addCosignerAccXPub
    , validateScriptTemplates
    , toSharedWalletId
    ) where

import Prelude

import Cardano.Address.Script
    ( Cosigner (..)
    , ErrValidateScriptTemplate (..)
    , ScriptHash (..)
    , ScriptTemplate (..)
    , ValidationLevel (..)
    , toScriptHash
    , validateScriptTemplate
    )
import Cardano.Crypto.Wallet
    ( XPub
    , unXPub
    )
import Cardano.Wallet.Address.Derivation
    ( Depth (..)
    , DerivationPrefix (..)
    , DerivationType (..)
    , HardDerivation (..)
    , Index (..)
    )
import Cardano.Wallet.Address.Derivation.SharedKey
    ( SharedKey (..)
    , purposeCIP1854
    , replaceCosignersWithVerKeys
    )
import Cardano.Wallet.Address.Discovery
    ( ChangeAddressMode
    , coinTypeAda
    , emptyPendingIxs
    )
import Cardano.Wallet.Address.Discovery.Sequential
    ( AddressPoolGap (..)
    )
import Cardano.Wallet.Address.Discovery.Shared
    ( CredentialType (..)
    , ErrAddCosigner (..)
    , ErrScriptTemplate (..)
    , Readiness (Active, Pending)
    , SharedAddressPools (SharedAddressPools, externalPool, internalPool, pendingChangeIxs)
    , SharedState (..)
    , SupportsDiscovery
    , newSharedAddressPool
    , retrieveAllCosigners
    )
import Cardano.Wallet.Address.Keys.WalletKey
    ( getRawKey
    , publicKey
    )
import Cardano.Wallet.Flavor
    ( KeyFlavorS
    )
import Cardano.Wallet.Primitive.Types.RewardAccount
    ( RewardAccount (..)
    )
import Control.Monad
    ( unless
    )
import Cryptography.Hash.Blake
    ( Blake2b_160
    )
import Cryptography.Hash.Core
    ( Digest
    , hash
    )
import Data.Either
    ( isRight
    )
import Data.Either.Combinators
    ( mapLeft
    )

import Cardano.Address.Script.Parser
    ( scriptToText
    )
import qualified Cardano.Address.Style.Shelley as CA
import Cardano.Wallet.Primitive.Types.Credentials
    ( ClearCredentials
    , RootCredentials (..)
    )
import qualified Data.Foldable as F
import qualified Data.Map.Strict as Map
import qualified Data.Text.Encoding as T

-- | Create a new SharedState from public account key.
mkSharedStateFromAccountXPub
    :: (SupportsDiscovery n k, k ~ SharedKey)
    => KeyFlavorS k
    -> k 'AccountK XPub
    -> Index 'Hardened 'AccountK
    -> ChangeAddressMode
    -> AddressPoolGap
    -> ScriptTemplate
    -> Maybe ScriptTemplate
    -> SharedState n k
mkSharedStateFromAccountXPub kF accXPub accIx mode gap pTemplate dTemplateM =
    activate kF $ SharedState
        { derivationPrefix = DerivationPrefix (purposeCIP1854, coinTypeAda, accIx)
        , accountXPub = accXPub
        , paymentTemplate = pTemplate
        , delegationTemplate = dTemplateM
        , rewardAccountKey = Nothing
        , poolGap = gap
        , changeAddressMode = mode
        , ready = Pending
        }

-- | Create a new SharedState from root private key and password.
mkSharedStateFromRootXPrv
    :: (SupportsDiscovery n k, k ~ SharedKey)
    => KeyFlavorS k
    -> ClearCredentials k
    -> Index 'Hardened 'AccountK
    -> ChangeAddressMode
    -> AddressPoolGap
    -> ScriptTemplate
    -> Maybe ScriptTemplate
    -> SharedState n k
mkSharedStateFromRootXPrv kF (RootCredentials rootXPrv pwd) accIx =
    mkSharedStateFromAccountXPub kF accXPub accIx
  where
    accXPub = publicKey kF $ deriveAccountPrivateKey pwd rootXPrv accIx

-- | Turn a 'Pending' into an 'Active' state if all templates are complete.
activate
    :: forall n k. (SupportsDiscovery n k, k ~ SharedKey)
    => KeyFlavorS k
    -> SharedState n k -> SharedState n k
activate kF
    st@(SharedState{accountXPub,paymentTemplate=pT,delegationTemplate=dT
                   ,rewardAccountKey,poolGap,ready})
  = st { ready = updateReady ready, rewardAccountKey = updateRewardAccount ready }
  where
    updateReady Pending
        | templatesComplete kF accountXPub pT dT
            = Active $ SharedAddressPools
              { externalPool = newSharedAddressPool @n poolGap pT dT
              , internalPool = newSharedAddressPool @n poolGap pT dT
              , pendingChangeIxs = emptyPendingIxs
              }
    updateReady r = r

    updateRewardAccount Pending
        | templatesComplete kF accountXPub pT dT
            = FromScriptHash . unScriptHash . toScriptHash .
                  flip (replaceCosignersWithVerKeys CA.Stake) minBound <$> dT
    updateRewardAccount _ = rewardAccountKey

-- | The cosigner with his account public key is updated per template.
--
-- For each template the script is checked for presence of the cosigner:
--   * If present, then the key is inserted into the state.
--   * Otherwise, fail with 'NoSuchCosigner'.
-- If the key is already present it is going to be updated.
-- For a given template all keys must be unique. If already present key is tried to be added,
-- `KeyAlreadyPresent` error is produced. The updating works only with pending shared state,
--
-- When an active shared state is used `WalletAlreadyActive` error is triggered.
--
-- Updating the key for delegation script can be successful only if delegation script is
-- present. Otherwise, `NoDelegationTemplate` error is triggered.
addCosignerAccXPub
    :: (SupportsDiscovery n k, k ~ SharedKey)
    => KeyFlavorS k
    -> (Cosigner, k 'AccountK XPub)
    -> CredentialType
    -> SharedState n k
    -> Either ErrAddCosigner (SharedState n k)
addCosignerAccXPub kF (cosigner, cosignerXPub) cred st = case ready st of
    Active{} ->
        Left WalletAlreadyActive
    Pending ->
        case (cred, paymentTemplate st, delegationTemplate st) of
            (Payment, pt, _)
                | tryingUpdateWalletCosigner pt -> Left CannotUpdateSharedWalletKey
                | isCosignerMissing pt -> Left $ NoSuchCosigner cred cosigner
                | isKeyAlreadyPresent pt -> Left $ KeyAlreadyPresent cred
            (Delegation, _, Just dt)
                | tryingUpdateWalletCosigner dt -> Left CannotUpdateSharedWalletKey
                | isCosignerMissing dt -> Left $ NoSuchCosigner cred cosigner
                | isKeyAlreadyPresent dt -> Left $ KeyAlreadyPresent cred
            (Delegation, _, Nothing) -> Left NoDelegationTemplate
            _ -> Right $
                activate kF $ addCosignerPending kF (cosigner, cosignerXPub) cred st
  where
    walletKey = accountXPub st
    isKeyAlreadyPresent (ScriptTemplate cosignerKeys _) =
        getRawKey kF cosignerXPub `F.elem` cosignerKeys
    isCosignerMissing (ScriptTemplate _ script') =
        cosigner `notElem` retrieveAllCosigners script'
    tryingUpdateWalletCosigner (ScriptTemplate cosignerKeys _) =
        case Map.lookup cosigner cosignerKeys of
            Nothing -> False
            Just key' -> key' == getRawKey kF walletKey

addCosignerPending
    :: KeyFlavorS k
    -> (Cosigner, k 'AccountK XPub)
    -> CredentialType
    -> SharedState n k
    -> SharedState n k
addCosignerPending kF (cosigner, cosignerXPub) cred st = case cred of
    Payment ->
        st { paymentTemplate = updateScriptTemplate (paymentTemplate st) }
    Delegation ->
        st { delegationTemplate = updateScriptTemplate <$> (delegationTemplate st) }
  where
    updateScriptTemplate sc@(ScriptTemplate cosignerMap script')
        | cosigner `elem` retrieveAllCosigners script' =
            ScriptTemplate
                (Map.insert cosigner (getRawKey kF  cosignerXPub) cosignerMap)
                script'
        | otherwise = sc

{-------------------------------------------------------------------------------
    Template validation
-------------------------------------------------------------------------------}

-- | Is the given account public key among the cosigners?
accountXPubCondition
    :: KeyFlavorS k
    -> k 'AccountK XPub
    -> ScriptTemplate
    -> Bool
accountXPubCondition kF accXPub (ScriptTemplate cosignerKeys _) =
    getRawKey kF accXPub `F.elem` cosignerKeys

validateScriptTemplates
    :: KeyFlavorS k
    -> k 'AccountK XPub
    -> ValidationLevel
    -> ScriptTemplate
    -> Maybe ScriptTemplate
    -> Either ErrScriptTemplate ()
validateScriptTemplates kF accXPub level pTemplate dTemplateM = do
    checkTemplate Payment pTemplate
    unless (checkXPub pTemplate) $ Left $ ErrScriptTemplateMissingKey Payment accXPubErr
    case dTemplateM of
        Just dTemplate -> do
            checkTemplate Delegation dTemplate
            unless (checkXPub dTemplate) $ Left $ ErrScriptTemplateMissingKey Delegation accXPubErr
        Nothing -> pure ()
  where
      --when creating the shared wallet we can have cosigners in script with missing
      --account public key. They are supposed to be collected when patching.
      handleUnusedCosigner
          :: Either ErrValidateScriptTemplate ()
          -> Either ErrValidateScriptTemplate ()
      handleUnusedCosigner = \case
          Left MissingCosignerXPub -> Right ()
          rest -> rest
      checkTemplate cred template' =
          mapLeft (ErrScriptTemplateInvalid cred) $
          handleUnusedCosigner $
          validateScriptTemplate level template'
      checkXPub = accountXPubCondition kF accXPub
      accXPubErr = "The wallet's account key must be always present for the script template."

-- | Do we have all public keys in the templates?
templatesComplete
    :: KeyFlavorS k
    -> k 'AccountK XPub
    -> ScriptTemplate
    -> Maybe ScriptTemplate
    -> Bool
templatesComplete kF accXPub pTemplate dTemplate =
    isValid pTemplate && maybe True isValid dTemplate
  where
    isValid template' =
        isRight (validateScriptTemplate RequiredValidation template')
        && (accountXPubCondition kF accXPub template')

toSharedWalletId
    :: KeyFlavorS k
    -> k 'AccountK XPub
    -> ScriptTemplate
    -> Maybe ScriptTemplate
    -> Digest Blake2b_160
toSharedWalletId kF accXPub pTemplate dTemplateM =
    hash $
    (unXPub . getRawKey kF $ accXPub) <>
    serializeScriptTemplate pTemplate <>
    maybe mempty serializeScriptTemplate dTemplateM
  where
    serializeScriptTemplate (ScriptTemplate _ script) =
        T.encodeUtf8 $ scriptToText script
