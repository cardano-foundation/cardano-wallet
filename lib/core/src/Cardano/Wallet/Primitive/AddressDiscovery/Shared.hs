{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Copyright: © 2018-2021 IOHK
-- License: Apache-2.0
--
-- An implementation of shared script state using
-- scheme specified in CIP-1854 Multi-signature Wallets.

module Cardano.Wallet.Primitive.AddressDiscovery.Shared
    (
    -- ** State
      SharedState (..)
    , unsafePendingSharedState
    , newSharedState
    , addCosignerAccXPub
    , purposeCIP1854
    , isShared
    ) where

import Prelude

import Cardano.Address.Script
    ( Cosigner (..)
    , KeyHash (..)
    , Script (..)
    , ScriptTemplate (..)
    , ValidationLevel (..)
    , foldScript
    , validateScriptTemplate
    )
import Cardano.Crypto.Wallet
    ( XPub )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..)
    , DerivationPrefix (..)
    , DerivationType (..)
    , Index (..)
    , MkKeyFingerprint (..)
    , NetworkDiscriminant (..)
    , Role (..)
    , SoftDerivation
    , WalletKey (..)
    )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( coinTypeAda )
import Cardano.Wallet.Primitive.AddressDiscovery.Script
    ( keyHashFromAccXPubIx )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( AddressPool
    , AddressPoolGap
    , ParentContext (..)
    , context
    , lookupAddress
    , mkAddressPool
    )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..), AddressState (..) )
import Control.DeepSeq
    ( NFData )
import Data.Coerce
    ( coerce )
import Data.Either
    ( isRight )
import Data.Proxy
    ( Proxy (..) )
import GHC.Generics
    ( Generic )
import Type.Reflection
    ( Typeable )

import qualified Data.Map.Strict as Map

{-------------------------------------------------------------------------------
                                Shared State
-------------------------------------------------------------------------------}

-- | Shared wallet is a new kind of wallet owned by one or more co-signers.
-- | In this type of the wallet, addresses are defined by two monetary scripts
-- | (one for the ownership of assets and the other one for the ownership of stake).
-- | The two scripts can be the same, different, also the script for the ownership of stake
-- | can be absent.
--
-- | The shared wallet can be in two states: pending and ready. In a pending state the wallet
-- | does not have account public keys for all co-signers, and hence discovery of script addresses
-- | co-shared with other co-signers is not possible. In a ready state the wallet has account public
-- | keys for all co-signers and co-shared script address discovery is possible.
--
-- | The shared wallet is instantiated with an account public key, derivation path needed to
-- | recreate it for a holder of this shared wallet, ie., one of the co-signers.
-- | In order to construct corectly the wallet, ie., enable co-shared script addresses discovery
-- | the following is needed:
--
-- | - a way to determine what range of indices are checked on the
-- |   ledger. Mechanism of address pool, also adopted for sequential wallets,
-- |   is used. The idea is to track all indices starting from 0 and up to N.
-- |   N is variable as addresses are discovered (and marked as Used in consequence).
-- |   The pool of addresses is enlarged in such way that the number of consecutive
-- |   Unsed addresses equals to address pool gap of the address pool. Hence,
-- |   the address pool gap needs to be specified.
--
-- | - script template for payment credential contains information about all collected
-- |   account public keys for all parties engaged, here named co-signers. Also the skeleton
-- |   determining script structure is provided. In this sense script is predetermined from
-- |   the beginning and can variate only in verification key part that replaces co-signers in the
-- |   script skeleton. The places where a specific cosigner is present is to be replaced
-- |   with the derived verfication key using the co-signer's account public key and
-- |   the index that was chosen. This is the reason why we need complete set of account public keys for
-- |   each co-signer to realize address discovery. The script template can be translated into
-- |   a corresponding script, which hash is used in the address, only when account public keys
-- |   for all cosigners specified in script are collected.The index for derivation is the same
-- |   for each cosigner's derivation. The same index is used in both scripts that represent
-- |   payment or delegation credential. Verification keys are derived
-- |   using role=3 for payment credential.
--
-- | - optional script template for delegation credential contains all information as in case of
-- |   the script template for payment credential. One difference is that the verification keys are derived
-- |   using role=4 for delegation credential.
--
-- | When both script are present, the base address (with both credentials) is expected to be discovered.
-- | When script template for delegation credential is missing then enterprise address (non-stakable) is
-- | expected.
data SharedState (n :: NetworkDiscriminant) k =
      PendingSharedState
      { pendingSharedStateDerivationPrefix :: !DerivationPrefix
          -- ^ Derivation path prefix from a root key up to the account key
      , pendingSharedStateAccountKey :: !(k 'AccountK XPub)
          -- ^ The account public key of an initiator of the shared wallet
      , pendingSharedStatePaymentTemplate :: !ScriptTemplate
          -- ^ Script template together with a map of account keys and cosigners
          -- for payment credential
      , pendingSharedStateDelegationTemplate :: !(Maybe ScriptTemplate)
          -- ^ Script template together with a map of account keys and cosigners
          -- for staking credential. If not specified then the same template as for
          -- payment is used
      , pendingSharedStateAddressPoolGap :: !AddressPoolGap
          -- ^ Address pool gap to be used in the address pool of shared state
      }
    | SharedState
      { sharedStateDerivationPrefix :: !DerivationPrefix
        -- ^ Derivation path prefix from a root key up to the account key
      , sharedStateAddressPool :: !(AddressPool 'MultisigScript k)
        -- ^ Address pool tracking the shared addresses. Co-owning is based on
        -- payment credential only. Moreover, the parent context information is stored
        -- ie., validated script template for payment credential, optional
        -- validated script template for delegation credential and account public key with which
        -- the shared wallet was initiated
      } deriving (Generic)

deriving instance
    ( Show (k 'AccountK XPub)
    ) => Show (SharedState n k)

deriving instance
    ( Eq (k 'AccountK XPub)
    ) => Eq (SharedState n k)

instance
    ( NFData (k 'AccountK XPub)
    ) => NFData (SharedState n k)

-- | Purpose for shared wallets is a constant set to 1854' (or 0x8000073E) following the original
-- CIP-1854 Multi-signature Wallets.
--
-- It indicates that the subtree of this node is used according to this
-- specification.
--
-- Hardened derivation is used at this level.
purposeCIP1854 :: Index 'Hardened 'PurposeK
purposeCIP1854 = toEnum 0x8000073E

-- | Create a pending SharedState from the all needed ingredients.
-- There is no validation and it is unsafe way.
unsafePendingSharedState
    :: forall (n :: NetworkDiscriminant) k. k 'AccountK XPub
    -> Index 'Hardened 'AccountK
    -> AddressPoolGap
    -> ScriptTemplate
    -> Maybe ScriptTemplate
    -> SharedState n k
unsafePendingSharedState accXPub accIx g pTemplate dTemplateM =
    PendingSharedState
    { pendingSharedStateDerivationPrefix = DerivationPrefix ( purposeCIP1854, coinTypeAda, accIx )
    , pendingSharedStateAccountKey = accXPub
    , pendingSharedStatePaymentTemplate = pTemplate
    , pendingSharedStateDelegationTemplate = dTemplateM
    , pendingSharedStateAddressPoolGap = g
    }

-- | Create a new SharedState.
newSharedState
    :: forall (n :: NetworkDiscriminant) k.
    ( MkKeyFingerprint k Address
    , SoftDerivation k
    , Typeable n
    , MkKeyFingerprint k (Proxy n, k 'AddressK XPub) )
    => k 'AccountK XPub
    -> Index 'Hardened 'AccountK
    -> AddressPoolGap
    -> ScriptTemplate
    -> Maybe ScriptTemplate
    -> SharedState n k
newSharedState accXPub accIx g pTemplate dTemplateM =
    let pendingSharedState = unsafePendingSharedState accXPub accIx g pTemplate dTemplateM
    in trySharedState pendingSharedState

trySharedState
    :: forall (n :: NetworkDiscriminant) k.
     ( MkKeyFingerprint k (Proxy n, k 'AddressK XPub)
     , MkKeyFingerprint k Address
     , Typeable n
     , SoftDerivation k )
    => SharedState n k
    -> SharedState n k
trySharedState = \case
    st@(SharedState _ _) -> st
    st@(PendingSharedState derPath accXPub pT dT g) ->
        if templatesComplete pT dT then
            SharedState
            { sharedStateDerivationPrefix = derPath
            , sharedStateAddressPool =
                mkAddressPool @n (ParentContextMultisigScript accXPub pT dT) g []
            }
        else
            st

templatesComplete
    :: ScriptTemplate
    -> Maybe ScriptTemplate
    -> Bool
templatesComplete pTemplate dTemplate =
    isRight (validateScriptTemplate RequiredValidation pTemplate) &&
    case dTemplate of
        Nothing -> True
        Just dTemplate' ->
            isRight (validateScriptTemplate RequiredValidation dTemplate')

-- | The cosigner with his account public key is done per template.
-- For every template the script is checked if the cosigner is present.
-- If yes, then the key is inserted. If the key is already present it is going to be
-- updated. If there is no cosigner present is the script then the cosigner -
-- account public key map is not changed. The updating works with pending shared state,
-- and can unpend the shared state if all public keys are present. When already unpended
-- shared state is used it does not change it.
addCosignerAccXPub
    :: forall (n :: NetworkDiscriminant) k.
       ( MkKeyFingerprint k Address
       , MkKeyFingerprint k (Proxy n, k 'AddressK XPub)
       , SoftDerivation k
       , Typeable n
       , WalletKey k )
    => k 'AccountK XPub
    -> Cosigner
    -> SharedState n k
    -> SharedState n k
addCosignerAccXPub accXPub cosigner state = case state of
    st@(SharedState _ _) -> st
    PendingSharedState prefix accXPub' pT dT g ->
        let updateScriptTemplate sc@(ScriptTemplate cosignerMap script') =
                if cosigner `elem` retrieveAllCosigners script' then
                    ScriptTemplate (Map.insert cosigner (getRawKey accXPub) cosignerMap) script'
                else
                    sc
            st = PendingSharedState
                { pendingSharedStateDerivationPrefix = prefix
                , pendingSharedStateAccountKey = accXPub'
                , pendingSharedStatePaymentTemplate = updateScriptTemplate pT
                , pendingSharedStateDelegationTemplate = updateScriptTemplate <$> dT
                , pendingSharedStateAddressPoolGap = g
                }
        in trySharedState st

retrieveAllCosigners :: Script Cosigner -> [Cosigner]
retrieveAllCosigners = foldScript (:) []

isShared
    :: forall (n :: NetworkDiscriminant) k.
       ( SoftDerivation k
       , WalletKey k
       , Typeable n
       , MkKeyFingerprint k Address
       , MkKeyFingerprint k (Proxy n, k 'AddressK XPub) )
    => Address
    -> SharedState n k
    -> (Maybe (Index 'Soft 'ScriptK, KeyHash), SharedState n k)
isShared addr st = case st of
    SharedState prefix pool ->
        let (ixM, pool') = lookupAddress @n (const Used) addr pool
            (ParentContextMultisigScript accXPub _ _) = context pool
        in case ixM of
            Just ix ->
                (Just (coerce ix, keyHashFromAccXPubIx accXPub (coerce ix))
                , SharedState prefix pool')
            Nothing ->
                (Nothing, st)
    PendingSharedState {} ->
        (Nothing, st)
