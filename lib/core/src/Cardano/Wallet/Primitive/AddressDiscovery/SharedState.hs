{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright: © 2018-2021 IOHK
-- License: Apache-2.0
--
-- An implementation of shared script state using
-- scheme specified in CIP-1854 Multi-signature Wallets.

module Cardano.Wallet.Primitive.AddressDiscovery.SharedState
    (
    -- ** State
      SharedState (..)
    , SharedStateFields (..)
    , SharedStatePending (..)
    , SupportsSharedState
    , mkSharedStateFromAccountXPub
    , mkSharedStateFromRootXPrv
    , addCosignerAccXPub
    , purposeCIP1854
    , isShared
    , retrieveAllCosigners
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
    ( XPrv, XPub )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..)
    , DerivationPrefix (..)
    , DerivationType (..)
    , HardDerivation (..)
    , Index (..)
    , MkKeyFingerprint (..)
    , NetworkDiscriminant (..)
    , Passphrase
    , Role (..)
    , SoftDerivation
    , WalletKey (..)
    )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( IsOurs (..), coinTypeAda )
import Cardano.Wallet.Primitive.AddressDiscovery.Script
    ( CredentialType (..), keyHashFromAccXPubIx )
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
import Cardano.Wallet.Primitive.Types.RewardAccount
    ( RewardAccount )
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
-- In this type of the wallet, addresses are defined by two monetary scripts
-- (one for the ownership of assets and the other one for the ownership of stake).
-- The two scripts can be the same, different, also the script for the ownership of stake
-- can be absent.
--
-- The shared wallet can be in two states: pending or ready. In a pending state the wallet
-- does not have account public keys for all co-signers, and hence discovery of script addresses
-- co-shared with other co-signers is not possible. In a ready state the wallet has account public
-- keys for all co-signers and co-shared script address discovery is possible.
--
-- The shared wallet is instantiated with an account public key, derivation path needed to
-- recreate it for a holder of this shared wallet, ie., one of the co-signers.
-- In order to construct corectly the wallet, ie., enable co-shared script addresses discovery
-- the following is needed:
--
-- - a way to determine what range of indices are checked on the
--   ledger. Mechanism of address pool, also adopted for sequential wallets,
--   is used. The idea is to track all indices starting from 0 and up to N.
--   N is variable as addresses are discovered (and marked as Used in consequence).
--   The pool of addresses is enlarged in such way that the number of consecutive
--   Unsed addresses equals to address pool gap of the address pool. Hence,
--   the address pool gap needs to be specified.
--
-- - script template for payment credential contains information about all collected
--   account public keys for all parties engaged, here named co-signers. Also the skeleton
--   determining script structure is provided. In this sense script is predetermined from
--   the beginning and can variate only in verification key part that replaces co-signers in the
--   script skeleton. The places where a specific cosigner is present is to be replaced
--   with the derived verfication key using the co-signer's account public key and
--   the index that was chosen. This is the reason why we need complete set of account public keys for
--   each co-signer to realize address discovery. The script template can be translated into
--   a corresponding script, which hash is used in the address, only when account public keys
--   for all cosigners specified in script are collected.The index for derivation is the same
--   for each cosigner's derivation. The same index is used in both scripts that represent
--   payment or delegation credential. Verification keys are derived
--   using role=3 for payment credential.
--
-- - optional script template for delegation credential contains all information as in case of
--   the script template for payment credential. One difference is that the verification keys are derived
--   using role=4 for delegation credential.
--
-- When both script are present, the base address (with both credentials) is expected to be discovered.
-- When script template for delegation credential is missing then enterprise address (non-stakable) is
-- expected.
data SharedState (n :: NetworkDiscriminant) k = SharedState
    { derivationPrefix :: !DerivationPrefix
        -- ^ Derivation path prefix from a root key up to the account key
    , fields :: !(SharedStateFields (SharedStatePending k) (AddressPool 'MultisigScript k))
        -- ^ Address pool tracking the shared addresses. Co-owning is based on
        -- payment credential only. Moreover, the parent context information is
        -- stored ie., validated script template for payment credential,
        -- optional validated script template for delegation credential and
        -- account public key with which the shared wallet was initiated
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

data SharedStateFields pending ready
    = PendingFields !pending
    | ReadyFields !ready
    deriving (Generic, Show, Eq)

instance (NFData pending, NFData ready) => NFData (SharedStateFields pending ready)

data SharedStatePending k = SharedStatePending
    { pendingSharedStateAccountKey :: !(k 'AccountK XPub)
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
    } deriving (Generic)

deriving instance
    ( Show (k 'AccountK XPub)
    ) => Show (SharedStatePending k)

deriving instance
    ( Eq (k 'AccountK XPub)
    ) => Eq (SharedStatePending k)

instance
    ( NFData (k 'AccountK XPub)
    ) => NFData (SharedStatePending k)

-- | Purpose for shared wallets is a constant set to 1854' (or 0x8000073E) following the original
-- CIP-1854 Multi-signature Wallets.
--
-- It indicates that the subtree of this node is used according to this
-- specification.
--
-- Hardened derivation is used at this level.
purposeCIP1854 :: Index 'Hardened 'PurposeK
purposeCIP1854 = toEnum 0x8000073E

-- | Create a new SharedState from public account key.
mkSharedStateFromAccountXPub
    :: SupportsSharedState n k
    => k 'AccountK XPub
    -> Index 'Hardened 'AccountK
    -> AddressPoolGap
    -> ScriptTemplate
    -> Maybe ScriptTemplate
    -> SharedState n k
mkSharedStateFromAccountXPub accXPub accIx gap pTemplate dTemplateM =
    mkSharedState accIx $
    SharedStatePending accXPub pTemplate dTemplateM gap

mkSharedState
    :: SupportsSharedState n k
    => Index 'Hardened 'AccountK
    -> SharedStatePending k
    -> SharedState n k
mkSharedState accIx state = updateSharedState id $ SharedState
    { derivationPrefix
    , fields = PendingFields state
    }
  where
    derivationPrefix = DerivationPrefix (purposeCIP1854, coinTypeAda, accIx)

type SupportsSharedState (n :: NetworkDiscriminant) k =
    ( MkKeyFingerprint k Address
    , SoftDerivation k
    , Typeable n
    , MkKeyFingerprint k (Proxy n, k 'AddressK XPub)
    )

-- | Create a new SharedState from root private key and password.
mkSharedStateFromRootXPrv
    :: (SupportsSharedState n k, WalletKey k)
    => (k 'RootK XPrv, Passphrase "encryption")
    -> Index 'Hardened 'AccountK
    -> AddressPoolGap
    -> ScriptTemplate
    -> Maybe ScriptTemplate
    -> SharedState n k
mkSharedStateFromRootXPrv (rootXPrv, pwd) accIx gap pTemplate dTemplateM =
    mkSharedState accIx $
    SharedStatePending accXPub pTemplate dTemplateM gap
  where
    accXPub = publicKey $ deriveAccountPrivateKey pwd rootXPrv accIx

-- | Turn a "pending" into an "active" state or identity if already "active"
updateSharedState
    :: forall n k. SupportsSharedState n k
    => (SharedStatePending k -> SharedStatePending k)
    -> SharedState n k
    -> SharedState n k
updateSharedState f st = case fields st of
    ReadyFields _ -> st
    PendingFields pending -> case sharedStateFromPending @n (f pending) of
        Just ready -> st { fields = ReadyFields ready }
        Nothing -> st

sharedStateFromPending
    :: forall n k. SupportsSharedState n k
    => SharedStatePending k
    -> Maybe (AddressPool 'MultisigScript k)
sharedStateFromPending (SharedStatePending accXPub pT dT g)
    | templatesComplete pT dT = Just $
        mkAddressPool @n (ParentContextMultisigScript accXPub pT dT) g []
    | otherwise = Nothing

templatesComplete
    :: ScriptTemplate
    -> Maybe ScriptTemplate
    -> Bool
templatesComplete pTemplate dTemplate =
    isValid pTemplate && maybe True isValid dTemplate
  where
    isValid = isRight . validateScriptTemplate RequiredValidation

-- | The cosigner with his account public key is done per template.
-- For every template the script is checked if the cosigner is present.
-- If yes, then the key is inserted. If the key is already present it is going to be
-- updated. If there is no cosigner present is the script then the cosigner -
-- account public key map is not changed. The updating works with pending shared state,
-- and can unpend the shared state if all public keys are present. When already unpended
-- shared state is used it does not change it.
addCosignerAccXPub
    :: (SupportsSharedState n k, WalletKey k)
    => k 'AccountK XPub
    -> Cosigner
    -> CredentialType
    -> SharedState n k
    -> SharedState n k
addCosignerAccXPub accXPub cosigner cred =
    updateSharedState $ addCosignerAccXPubPending accXPub cosigner cred

addCosignerAccXPubPending
    :: WalletKey k
    => k 'AccountK XPub
    -> Cosigner
    -> CredentialType
    -> SharedStatePending k
    -> SharedStatePending k
addCosignerAccXPubPending accXPub cosigner cred (SharedStatePending accXPub' pT dTM g) = SharedStatePending
    { pendingSharedStateAccountKey = accXPub'
    , pendingSharedStatePaymentTemplate = case cred of
        Payment -> updateScriptTemplate pT
        Delegation -> pT
    , pendingSharedStateDelegationTemplate = case cred of
        Payment -> dTM
        Delegation -> updateScriptTemplate <$> dTM
    , pendingSharedStateAddressPoolGap = g
    }
  where
    updateScriptTemplate sc@(ScriptTemplate cosignerMap script')
        | cosigner `elem` retrieveAllCosigners script' =
            ScriptTemplate (Map.insert cosigner (getRawKey accXPub) cosignerMap) script'
        | otherwise = sc

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
isShared addr st = case fields st of
    ReadyFields pool ->
        let (ixM, pool') = lookupAddress @n (const Used) addr pool
            (ParentContextMultisigScript accXPub _ _) = context pool
        in case ixM of
            Just ix ->
                (Just (coerce ix, keyHashFromAccXPubIx accXPub (coerce ix))
                , st { fields = ReadyFields pool' })
            Nothing ->
                (Nothing, st)
    PendingFields _ ->
        (Nothing, st)

instance IsOurs (SharedState n k) Address where
    isOurs _addr state = (Nothing, state)

instance IsOurs (SharedState n k) RewardAccount where
    isOurs _account state = (Nothing, state)
