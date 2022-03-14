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
{-# LANGUAGE NamedFieldPuns #-}
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
      SupportsDiscovery

    -- ** State
    , SharedState (..)
    , Readiness (..)
    , newSharedAddressPool

    , ErrAddCosigner (..)
    , ErrScriptTemplate (..)
    , mkSharedStateFromAccountXPub
    , mkSharedStateFromRootXPrv
    , addCosignerAccXPub
    , isShared
    , retrieveAllCosigners
    , validateScriptTemplates

    , CredentialType (..)
    , liftPaymentAddress
    , liftDelegationAddress
    ) where

import Prelude

import Cardano.Address.Script
    ( Cosigner (..)
    , ErrValidateScriptTemplate (..)
    , Script (..)
    , ScriptHash (..)
    , ScriptTemplate (..)
    , ValidationLevel (..)
    , foldScript
    , prettyErrValidateScriptTemplate
    , toScriptHash
    , validateScriptTemplate
    )
import Cardano.Address.Style.Shelley
    ( Credential (..), delegationAddress, paymentAddress )
import Cardano.Crypto.Wallet
    ( XPrv, XPub )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..)
    , DerivationIndex (..)
    , DerivationPrefix (..)
    , DerivationType (..)
    , HardDerivation (..)
    , Index (..)
    , KeyFingerprint (..)
    , MkKeyFingerprint (..)
    , NetworkDiscriminant (..)
    , Passphrase
    , PersistPublicKey (..)
    , SoftDerivation
    , WalletKey (..)
    , utxoExternal
    )
import Cardano.Wallet.Primitive.AddressDerivation.SharedKey
    ( SharedKey (..)
    , constructAddressFromIx
    , purposeCIP1854
    , replaceCosignersWithVerKeys
    , toNetworkTag
    )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( CompareDiscovery (..)
    , GetAccount (..)
    , IsOurs (..)
    , KnownAddresses (..)
    , MaybeLight (..)
    , coinTypeAda
    )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( AddressPoolGap (..), unsafePaymentKeyFingerprint )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..) )
import Cardano.Wallet.Primitive.Types.RewardAccount
    ( RewardAccount )
import Control.Arrow
    ( first )
import Control.DeepSeq
    ( NFData )
import Control.Monad
    ( unless )
import Data.Either
    ( isRight )
import Data.Either.Combinators
    ( mapLeft )
import Data.Kind
    ( Type )
import Data.Proxy
    ( Proxy (..) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( FromText (..), TextDecodingError (..), ToText (..) )
import Fmt
    ( Buildable (..), indentF )
import GHC.Generics
    ( Generic )
import Type.Reflection
    ( Typeable )

import qualified Cardano.Address as CA
import qualified Cardano.Address.Style.Shelley as CA
import qualified Cardano.Wallet.Address.Pool as AddressPool
import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

-- | Convenient alias for commonly used class contexts on keys.
type SupportsDiscovery (n :: NetworkDiscriminant) k =
    ( MkKeyFingerprint k (Proxy n, k 'AddressK XPub)
    , MkKeyFingerprint k Address
    , SoftDerivation k
    , Typeable n
    )

{-------------------------------------------------------------------------------
    Address Pool
-------------------------------------------------------------------------------}
-- | An address pool which keeps track of shared addresses.
-- To create a new pool, see 'newSharedAddressPool'.
type SharedAddressPool (key :: Depth -> Type -> Type) =
        AddressPool.Pool
            (KeyFingerprint "payment" key)
            (Index 'Soft 'ScriptK)

-- | Create a new shared address pool from complete script templates.
newSharedAddressPool
    :: forall (n :: NetworkDiscriminant) key.
        ( key ~ SharedKey, SupportsDiscovery n key )
    => AddressPoolGap
    -> ScriptTemplate
    -> Maybe ScriptTemplate
    -> SharedAddressPool key
newSharedAddressPool g payment delegation =
    AddressPool.new generator gap
  where
    gap = fromIntegral $ getAddressPoolGap g
    generator
        = unsafePaymentKeyFingerprint @key
        . constructAddressFromIx @n payment delegation

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
-- In order to construct correctly the wallet, ie., enable co-shared script addresses discovery
-- the following is needed:
--
-- - a way to determine what range of indices are checked on the
--   ledger. Mechanism of address pool, also adopted for sequential wallets,
--   is used. The idea is to track all indices starting from 0 and up to N.
--   N is variable as addresses are discovered (and marked as Used in consequence).
--   The pool of addresses is enlarged in such way that the number of consecutive
--   Unused addresses equals to address pool gap of the address pool. Hence,
--   the address pool gap needs to be specified.
--
-- - script template for payment credential contains information about all collected
--   account public keys for all parties engaged, here named co-signers. Also the skeleton
--   determining script structure is provided. In this sense script is predetermined from
--   the beginning and can variate only in verification key part that replaces co-signers in the
--   script skeleton. The places where a specific cosigner is present is to be replaced
--   with the derived verification key using the co-signer's account public key and
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
    , accountXPub :: !(k 'AccountK XPub)
        -- ^ The account public key of an initiator of the shared wallet
    , paymentTemplate :: !ScriptTemplate
        -- ^ Script template together with a map of account keys and cosigners
        -- for payment credential.
    , delegationTemplate :: !(Maybe ScriptTemplate)
        -- ^ Script template together with a map of account keys and cosigners
        -- for staking credential. If not specified then the same template as for
        -- payment is used.
    , poolGap :: !AddressPoolGap
        -- ^ Address pool gap to be used in the address pool of shared state
    , ready :: !(Readiness (SharedAddressPool k))
        -- ^ Readiness status of the shared state.
        -- The state is ready if all cosigner public keys have been obtained.
        -- In this case, an address pool is allocated
    } deriving (Generic)

instance ( NFData (k 'AccountK XPub) ) => NFData (SharedState n k)

deriving instance ( Show (k 'AccountK XPub) ) => Show (SharedState n k)

-- We have to write the equality instance by hands,
-- because there is no general equality for address pools
-- (we cannot test the generators for equality).
instance Eq (k 'AccountK XPub) => Eq (SharedState n k) where
    SharedState a1 a2 a3 a4 a5 ap == SharedState b1 b2 b3 b4 b5 bp
        = and [a1 == b1, a2 == b2, a3 == b3, a4 == b4, a5 == b5, ap `match` bp]
      where
        match Pending Pending = True
        match (Active a) (Active b)
            = AddressPool.addresses a == AddressPool.addresses b
        match _ _ = False

instance PersistPublicKey (k 'AccountK) => Buildable (SharedState n k) where
    build st = "SharedState:\n"
        <> indentF 4 ("Derivation prefix: " <> build (toText $ derivationPrefix st))
        <> indentF 4 ("accountXPub:" <> build (accountXPub st))
        <> indentF 4 ("paymentTemplate:" <> build (paymentTemplate st))
        <> indentF 4 ("delegationTemplate:" <> build (delegationTemplate st))
        <> indentF 4 ("poolGap:" <> build (toText $ poolGap st))
        <> indentF 4 ("ready: " <> readyF (ready st))
      where
        readyF (Pending) = "Pending"
        readyF (Active pool) =
            "Active:" <> printIndex (derivationPrefix st) <> " " <> build pool
        printIndex (DerivationPrefix (_,_,ix)) =
            " hardened index: "<> build (getIndex ix)

-- | Readiness status of the shared state.
data Readiness a
    = Pending
    | Active !a
    deriving (Generic, Show, Eq)

instance (NFData a) => NFData (Readiness a)

-- | Create a new SharedState from public account key.
mkSharedStateFromAccountXPub
    :: (SupportsDiscovery n k, WalletKey k, k ~ SharedKey)
    => k 'AccountK XPub
    -> Index 'Hardened 'AccountK
    -> AddressPoolGap
    -> ScriptTemplate
    -> Maybe ScriptTemplate
    -> SharedState n k
mkSharedStateFromAccountXPub accXPub accIx gap pTemplate dTemplateM =
    activate $ SharedState
        { derivationPrefix = DerivationPrefix (purposeCIP1854, coinTypeAda, accIx)
        , accountXPub = accXPub
        , paymentTemplate = pTemplate
        , delegationTemplate = dTemplateM
        , poolGap = gap
        , ready = Pending
        }

-- | Create a new SharedState from root private key and password.
mkSharedStateFromRootXPrv
    :: (SupportsDiscovery n k, WalletKey k, k ~ SharedKey)
    => (k 'RootK XPrv, Passphrase "encryption")
    -> Index 'Hardened 'AccountK
    -> AddressPoolGap
    -> ScriptTemplate
    -> Maybe ScriptTemplate
    -> SharedState n k
mkSharedStateFromRootXPrv (rootXPrv, pwd) accIx =
    mkSharedStateFromAccountXPub accXPub accIx
  where
    accXPub = publicKey $ deriveAccountPrivateKey pwd rootXPrv accIx

-- | Turn a 'Pending' into an 'Active' state if all templates are complete.
activate
    :: forall n k. (SupportsDiscovery n k, WalletKey k, k ~ SharedKey)
    => SharedState n k -> SharedState n k
activate
    st@(SharedState{accountXPub,paymentTemplate=pT,delegationTemplate=dT,poolGap,ready})
  = st { ready = new ready }
  where
    new Pending
        | templatesComplete accountXPub pT dT
            = Active $ newSharedAddressPool @n poolGap pT dT
    new r   = r

-- | Possible errors from adding a co-signer key to the shared wallet state.
data ErrAddCosigner
    = NoDelegationTemplate
        -- ^ Adding key for a cosigner for a non-existent delegation template is
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
    :: (SupportsDiscovery n k, WalletKey k, k ~ SharedKey)
    => (Cosigner, k 'AccountK XPub)
    -> CredentialType
    -> SharedState n k
    -> Either ErrAddCosigner (SharedState n k)
addCosignerAccXPub (cosigner, cosignerXPub) cred st = case ready st of
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
                activate $ addCosignerPending (cosigner, cosignerXPub) cred st
  where
    walletKey = accountXPub st
    isKeyAlreadyPresent (ScriptTemplate cosignerKeys _) =
        getRawKey cosignerXPub `F.elem` cosignerKeys
    isCosignerMissing (ScriptTemplate _ script') =
        cosigner `notElem` retrieveAllCosigners script'
    tryingUpdateWalletCosigner (ScriptTemplate cosignerKeys _) =
        case Map.lookup cosigner cosignerKeys of
            Nothing -> False
            Just key' -> key' == getRawKey walletKey

addCosignerPending
    :: WalletKey k
    => (Cosigner, k 'AccountK XPub)
    -> CredentialType
    -> SharedState n k
    -> SharedState n k
addCosignerPending (cosigner, cosignerXPub) cred st = case cred of
    Payment ->
        st { paymentTemplate = updateScriptTemplate (paymentTemplate st) }
    Delegation ->
        st { delegationTemplate = updateScriptTemplate <$> (delegationTemplate st) }
  where
    updateScriptTemplate sc@(ScriptTemplate cosignerMap script')
        | cosigner `elem` retrieveAllCosigners script' =
            ScriptTemplate (Map.insert cosigner (getRawKey cosignerXPub) cosignerMap) script'
        | otherwise = sc

retrieveAllCosigners :: Script Cosigner -> [Cosigner]
retrieveAllCosigners = foldScript (:) []

{-------------------------------------------------------------------------------
    Template validation
-------------------------------------------------------------------------------}

-- | Is the given account public key among the cosigners?
accountXPubCondition
    :: WalletKey k
    => k 'AccountK XPub
    -> ScriptTemplate
    -> Bool
accountXPubCondition accXPub (ScriptTemplate cosignerKeys _) =
    getRawKey accXPub `F.elem` cosignerKeys

data ErrScriptTemplate =
      ErrScriptTemplateInvalid !CredentialType !ErrValidateScriptTemplate
    | ErrScriptTemplateMissingKey !CredentialType !Text
    deriving (Show, Eq)

instance ToText ErrValidateScriptTemplate where
    toText = T.pack . prettyErrValidateScriptTemplate

validateScriptTemplates
    :: WalletKey k
    => k 'AccountK XPub
    -> ValidationLevel
    -> ScriptTemplate
    -> Maybe ScriptTemplate
    -> Either ErrScriptTemplate ()
validateScriptTemplates accXPub level pTemplate dTemplateM = do
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
      checkXPub template' =
          accountXPubCondition accXPub template'
      accXPubErr = "The wallet's account key must be always present for the script template."

-- | Do we have all public keys in the templates?
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

{-------------------------------------------------------------------------------
    Address discovery
-------------------------------------------------------------------------------}

isShared
    :: SupportsDiscovery n k
    => Address
    -> SharedState n k
    -> (Maybe (Index 'Soft 'ScriptK), SharedState n k)
isShared addrRaw st = case ready st of
    Pending -> nop
    Active pool -> case paymentKeyFingerprint addrRaw of
        Left _ -> nop
        Right addr -> case AddressPool.lookup addr pool of
            Nothing -> nop
            Just ix -> let pool' = AddressPool.update addr pool in
                ( Just ix , st { ready = Active pool' } )
  where
    nop = (Nothing, st)
    -- FIXME: Check that the network discrimant of the type
    -- is compatible with the discriminant of the Address!

instance SupportsDiscovery n k => IsOurs (SharedState n k) Address
  where
    isOurs addr st = first (fmap (decoratePath st)) (isShared addr st)

-- | Decorate an index with the derivation prefix corresponding to the state.
decoratePath
    :: SharedState n key
    -> Index 'Soft 'ScriptK
    -> NE.NonEmpty DerivationIndex
decoratePath st ix = NE.fromList
    [ DerivationIndex $ getIndex purpose
    , DerivationIndex $ getIndex coinType
    , DerivationIndex $ getIndex accIx
    , DerivationIndex $ getIndex utxoExternal
    , DerivationIndex $ getIndex ix
    ] 
  where
    DerivationPrefix (purpose, coinType, accIx) = derivationPrefix st

instance IsOurs (SharedState n k) RewardAccount where
    isOurs _account st = (Nothing, st)

instance GetAccount (SharedState n k) k where
    getAccount = accountXPub

instance SupportsDiscovery n k => CompareDiscovery (SharedState n k) where
    compareDiscovery st a1 a2 = case ready st of
        Pending ->
            error "comparing addresses in pending shared state does not make sense"
        Active pool ->
            case (ix a1 pool, ix a2 pool) of
                (Nothing, Nothing) -> EQ
                (Nothing, Just _)  -> GT
                (Just _, Nothing)  -> LT
                (Just i1, Just i2) -> compare i1 i2
      where
        ix :: Address -> SharedAddressPool k -> Maybe (Index 'Soft 'ScriptK)
        ix a pool = case paymentKeyFingerprint a of
            Left _ -> Nothing
            Right addr -> AddressPool.lookup addr pool

instance Typeable n => KnownAddresses (SharedState n k) where
    knownAddresses st = nonChangeAddresses
      where
        -- TODO - After enabling txs for shared wallets we will need to expand this
        nonChangeAddresses = case ready st of
            Pending -> []
            Active pool -> map swivel $ Map.toList $ AddressPool.addresses pool
        swivel (k,(ix,s)) = (liftPaymentAddress @n k, s, decoratePath st ix)

instance MaybeLight (SharedState n k) where
    maybeDiscover = Nothing

{-------------------------------------------------------------------------------
    Address utilities
    Payment and Delegation parts
-------------------------------------------------------------------------------}

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

liftPaymentAddress
    :: forall (n :: NetworkDiscriminant) (k :: Depth -> Type -> Type).
       Typeable n
    => KeyFingerprint "payment" k
    -> Address
liftPaymentAddress (KeyFingerprint fingerprint) =
    Address $ CA.unAddress $
    paymentAddress (toNetworkTag @n)
    (PaymentFromScript (ScriptHash fingerprint))

liftDelegationAddress
    :: forall (n :: NetworkDiscriminant) (k :: Depth -> Type -> Type).
       Typeable n
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
