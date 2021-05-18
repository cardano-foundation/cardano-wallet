{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
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
    , SharedStateFields (..)
    , SharedStatePending (..)
    , SupportsSharedState
    , ErrAddCosigner (..)
    , mkSharedStateFromAccountXPub
    , mkSharedStateFromRootXPrv
    , addCosignerAccXPub
    , isShared
    , retrieveAllCosigners
    , walletCreationInvariant

    , CredentialType (..)
    , liftPaymentAddress
    , liftDelegationAddress
    , keyHashFromAccXPubIx
    ) where

import Prelude

import Cardano.Address.Script
    ( Cosigner (..)
    , KeyHash (..)
    , Script (..)
    , ScriptHash (..)
    , ScriptTemplate (..)
    , ValidationLevel (..)
    , foldScript
    , toScriptHash
    , validateScriptTemplate
    )
import Cardano.Address.Style.Shelley
    ( Credential (..), delegationAddress, paymentAddress )
import Cardano.Crypto.Wallet
    ( XPrv, XPub )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..)
    , DerivationPrefix (..)
    , DerivationType (..)
    , HardDerivation (..)
    , Index (..)
    , KeyFingerprint (..)
    , MkKeyFingerprint (..)
    , NetworkDiscriminant (..)
    , Passphrase
    , Role (..)
    , SoftDerivation
    , WalletKey (..)
    , deriveVerificationKey
    , hashVerificationKey
    )
import Cardano.Wallet.Primitive.AddressDerivation.SharedKey
    ( SharedKey (..)
    , purposeCIP1854
    , replaceCosignersWithVerKeys
    , toNetworkTag
    )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( CompareDiscovery (..)
    , GetAccount (..)
    , GetPurpose
    , IsOurs (..)
    , KnownAddresses (..)
    , coinTypeAda
    )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( AddressPool
    , AddressPoolGap
    , ParentContext (..)
    , addresses
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
import Data.Kind
    ( Type )
import Data.Proxy
    ( Proxy (..) )
import Data.Text.Class
    ( FromText (..), TextDecodingError (..), ToText (..) )
import GHC.Generics
    ( Generic )
import Type.Reflection
    ( Typeable )

import qualified Cardano.Address as CA
import qualified Cardano.Address.Style.Shelley as CA
import qualified Data.Foldable as F
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
    , fields :: !(SharedStateFields (SharedStatePending k) (AddressPool 'UtxoExternal k))
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

-- | Create a new SharedState from public account key.
mkSharedStateFromAccountXPub
    :: (SupportsSharedState n k, WalletKey k, k ~ SharedKey)
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
    :: (SupportsSharedState n k, WalletKey k, k ~ SharedKey)
    => Index 'Hardened 'AccountK
    -> SharedStatePending k
    -> SharedState n k
mkSharedState accIx pending = updateSharedState state id
  where
    state = SharedState
        { derivationPrefix = DerivationPrefix (purposeCIP1854, coinTypeAda, accIx)
        , fields = PendingFields pending
        }

type SupportsSharedState (n :: NetworkDiscriminant) k =
    ( MkKeyFingerprint k Address
    , SoftDerivation k
    , Typeable n
    , MkKeyFingerprint k (Proxy n, k 'AddressK XPub)
    )

-- | Create a new SharedState from root private key and password.
mkSharedStateFromRootXPrv
    :: (SupportsSharedState n k, WalletKey k, k ~ SharedKey)
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

-- | Turn a "pending" into an "active" state or identity if already "active".
updateSharedState
    :: forall n k. (SupportsSharedState n k, WalletKey k, k ~ SharedKey)
    => SharedState n k
    -> (SharedStatePending k -> SharedStatePending k)
    -> SharedState n k
updateSharedState st f = case fields st of
    ReadyFields _ -> st
    PendingFields pending -> case sharedStateFromPending @n (f pending) of
        Just ready -> st { fields = ReadyFields ready }
        Nothing -> st { fields = PendingFields (f pending) }

sharedStateFromPending
    :: forall n k. (SupportsSharedState n k, WalletKey k, k ~ SharedKey)
    => SharedStatePending k
    -> Maybe (AddressPool 'UtxoExternal k)
sharedStateFromPending (SharedStatePending accXPub pT dT g)
    | templatesComplete accXPub pT dT = Just $
        mkAddressPool @n (ParentContextShared accXPub pT dT) g []
    | otherwise = Nothing

accountXPubCondition
    :: WalletKey k
    => k 'AccountK XPub
    -> ScriptTemplate
    -> Bool
accountXPubCondition accXPub (ScriptTemplate cosignerKeys _) =
    getRawKey accXPub `F.elem` cosignerKeys

walletCreationInvariant
    :: WalletKey k
    => k 'AccountK XPub
    -> ScriptTemplate
    -> Maybe ScriptTemplate
    -> Bool
walletCreationInvariant accXPub pTemplate dTemplate =
    isValid pTemplate && maybe True isValid dTemplate
  where
    isValid template' =
        accountXPubCondition accXPub template'

templatesComplete
    :: WalletKey k
    => k 'AccountK XPub
    -> ScriptTemplate
    -> Maybe ScriptTemplate
    -> Bool
templatesComplete accXPub pTemplate dTemplate =
    isValid pTemplate && maybe True isValid dTemplate
  where
    isValid template' =
        isRight (validateScriptTemplate RequiredValidation template')
        && (accountXPubCondition accXPub template')

-- | Possible errors from adding a co-signer key to the shared wallet state.
data ErrAddCosigner
    = NoDelegationTemplate
        -- ^ Adding key for a cosigner for a non-existant delegation template is
        -- not allowed.
    | NoSuchCosigner CredentialType Cosigner
        -- ^ Adding key for a cosigners for a given script is possible for the
        -- cosigner present in the script template.
    | KeyAlreadyPresent CredentialType
        -- ^ Adding the same key for different cosigners for a given script is
        -- not allowed.
    | WalletAlreadyActive
        -- ^ Adding is possible only to pending shared wallet.
    | CannotUpdateSharedWalletKey
        -- ^ Updating key is possible only for other cosigners, not cosigner
        -- belonging to the shared wallet.
    deriving (Eq, Show)

-- | The cosigner with his account public key is updated per template.
--
-- For each template the script is checked for presence of the cosigner:
--   * If present, then the key is inserted into the staate.
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
    :: (SupportsSharedState n k, WalletKey k, k ~ SharedKey)
    => k 'AccountK XPub
    -> Cosigner
    -> CredentialType
    -> SharedState n k
    -> Either ErrAddCosigner (SharedState n k)
addCosignerAccXPub accXPub cosigner cred st = case fields st of
    ReadyFields _ ->
        Left WalletAlreadyActive
    PendingFields (SharedStatePending walletKey paymentTmpl delegTmpl _) ->
        case (cred, paymentTmpl, delegTmpl) of
            (Payment, pt, _)
                | tryingUpdateWalletCosigner walletKey pt -> Left CannotUpdateSharedWalletKey
                | isCosignerMissing pt -> Left $ NoSuchCosigner cred cosigner
                | isKeyAlreadyPresent pt -> Left $ KeyAlreadyPresent cred
            (Delegation, _, Just dt)
                | tryingUpdateWalletCosigner walletKey dt -> Left CannotUpdateSharedWalletKey
                | isCosignerMissing dt -> Left $ NoSuchCosigner cred cosigner
                | isKeyAlreadyPresent dt -> Left $ KeyAlreadyPresent cred
            (Delegation, _, Nothing) -> Left NoDelegationTemplate
            _ -> Right $
                 updateSharedState st $
                 addCosignerAccXPubPending accXPub cosigner cred
  where
    isKeyAlreadyPresent (ScriptTemplate cosignerKeys _) =
        getRawKey accXPub `F.elem` cosignerKeys
    isCosignerMissing (ScriptTemplate _ script') =
        cosigner `notElem` retrieveAllCosigners script'
    tryingUpdateWalletCosigner walletKey (ScriptTemplate cosignerKeys _) =
        case Map.lookup cosigner cosignerKeys of
            Nothing -> False
            Just key' -> key' == getRawKey walletKey

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
       , Typeable n
       , MkKeyFingerprint k Address
       , MkKeyFingerprint k (Proxy n, k 'AddressK XPub) )
    => Address
    -> SharedState n k
    -> (Maybe (Index 'Soft 'ScriptK), SharedState n k)
isShared addr st = case fields st of
    ReadyFields pool ->
        let (ixM, pool') = lookupAddress @n (const Used) addr pool
        in case ixM of
            Just ix ->
                ( Just $ coerce ix
                , st { fields = ReadyFields pool' })
            Nothing ->
                (Nothing, st)
    PendingFields _ ->
        (Nothing, st)

instance IsOurs (SharedState n k) Address where
    isOurs _addr state = (Nothing, state)

instance IsOurs (SharedState n k) RewardAccount where
    isOurs _account state = (Nothing, state)

instance GetAccount (SharedState n k) k where
    getAccount (SharedState _ (PendingFields pending)) =
        pendingSharedStateAccountKey pending
    getAccount (SharedState _ (ReadyFields pool)) =
        let (ParentContextShared accXPub _ _) = context pool
        in accXPub

instance
    ( MkKeyFingerprint k (Proxy n, k 'AddressK XPub)
    , MkKeyFingerprint k Address
    , SoftDerivation k
    , Typeable n
    ) => CompareDiscovery (SharedState n k) where
    compareDiscovery (SharedState _ state) a1 a2 = case state of
        PendingFields _ -> error "comparing addresses in pending shared state does not make sense"
        ReadyFields pool ->
            case (ix a1 pool, ix a2 pool) of
                (Nothing, Nothing) -> EQ
                (Nothing, Just _)  -> GT
                (Just _, Nothing)  -> LT
                (Just i1, Just i2) -> compare i1 i2
      where
        ix :: Address -> AddressPool 'UtxoExternal k -> Maybe (Index 'Soft 'AddressK)
        ix a = fst . lookupAddress @n id a

instance
    ( Typeable n
    , GetPurpose k
    ) => KnownAddresses (SharedState n k) where
    knownAddresses (SharedState _ s) = nonChangeAddresses
      where
          -- TODO - After enabling txs for shared wallets we will need to expand this
          nonChangeAddresses = case s of
              PendingFields _ -> []
              ReadyFields externalPool ->
                  addresses (liftPaymentAddress @n @k) externalPool

data CredentialType = Payment | Delegation
    deriving (Eq, Show, Generic)
    deriving anyclass NFData

instance ToText CredentialType where
    toText Payment = "payment"
    toText Delegation = "delegation"

instance FromText CredentialType where
    fromText = \case
        "payment" -> Right Payment
        "delegation" -> Right Delegation
        _ -> Left $ TextDecodingError $ unwords
            [ "Invalid credential type: expecting only following values:"
            , "'payment', 'delegation'."
            ]

keyHashFromAccXPubIx
    :: (SoftDerivation k, WalletKey k)
    => k 'AccountK XPub
    -> Role
    -> Index 'Soft 'ScriptK
    -> KeyHash
keyHashFromAccXPubIx accXPub r ix =
    hashVerificationKey r $ deriveVerificationKey accXPub r ix

liftPaymentAddress
    :: forall (n :: NetworkDiscriminant) (k :: Depth -> Type -> Type). Typeable n
    => KeyFingerprint "payment" k
  -> Address
liftPaymentAddress (KeyFingerprint fingerprint) =
    Address $ CA.unAddress $
    paymentAddress (toNetworkTag @n)
    (PaymentFromScript (ScriptHash fingerprint))

liftDelegationAddress
    :: forall (n :: NetworkDiscriminant) (k :: Depth -> Type -> Type). Typeable n
    => Index 'Soft 'ScriptK
    -> ScriptTemplate
    -> KeyFingerprint "payment" k
    -> Address
liftDelegationAddress ix dTemplate (KeyFingerprint fingerprint) =
    Address $ CA.unAddress $
    delegationAddress (toNetworkTag @n)
    (PaymentFromScript (ScriptHash fingerprint))
    (delegationCredential dScript)
  where
    delegationCredential = DelegationFromScript . toScriptHash
    dScript =
        replaceCosignersWithVerKeys CA.Stake dTemplate ix
