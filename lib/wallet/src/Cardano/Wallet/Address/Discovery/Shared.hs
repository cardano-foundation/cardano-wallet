{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
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

module Cardano.Wallet.Address.Discovery.Shared
    (
      SupportsDiscovery

    -- ** State
    , SharedState (..)
    , Readiness (..)
    , SharedAddressPools (..)
    , SharedAddressPool (..)
    , newSharedAddressPool
    , isOwned

    , ErrAddCosigner (..)
    , ErrScriptTemplate (..)
    , isShared
    , retrieveAllCosigners
    , estimateMinWitnessRequiredPerInput
    , estimateMaxWitnessRequiredPerInput

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
    , foldScript
    , prettyErrValidateScriptTemplate
    , toScriptHash
    )
import Cardano.Address.Style.Shelley
    ( Credential (..), delegationAddress, paymentAddress )
import Cardano.Crypto.Wallet
    ( XPrv, XPub )
import Cardano.Wallet.Address.Derivation
    ( AccountIxForStaking (..)
    , AddressParts (..)
    , Depth (..)
    , DerivationIndex (..)
    , DerivationPrefix (..)
    , DerivationType (..)
    , HardDerivation (..)
    , Index (..)
    , KeyFingerprint (..)
    , MkKeyFingerprint (..)
    , PersistPublicKey (..)
    , Role (..)
    , SoftDerivation
    , mutableAccount
    , roleVal
    , toAddressParts
    , unsafePaymentKeyFingerprint
    , utxoExternal
    , utxoInternal
    )
import Cardano.Wallet.Address.Derivation.Shared
    ( SharedKey )
import Cardano.Wallet.Address.Derivation.SharedKey
    ( constructAddressFromIx, replaceCosignersWithVerKeys, toNetworkTag )
import Cardano.Wallet.Address.Discovery
    ( CompareDiscovery (..)
    , GenChange (..)
    , GetAccount (..)
    , IsOurs (..)
    , KnownAddresses (..)
    , MaybeLight (..)
    , PendingIxs
    , nextChangeIndex
    , pendingIxsToList
    )
import Cardano.Wallet.Address.Discovery.Sequential
    ( AddressPoolGap (..) )
import Cardano.Wallet.Primitive.NetworkId
    ( HasSNetworkId (..), NetworkDiscriminant, networkDiscriminantBits )
import Cardano.Wallet.Primitive.Passphrase
    ( Passphrase )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..), AddressState (..) )
import Cardano.Wallet.Primitive.Types.RewardAccount
    ( RewardAccount (..) )
import Control.Applicative
    ( (<|>) )
import Control.Arrow
    ( first )
import Control.DeepSeq
    ( NFData )
import Control.Monad
    ( guard )
import Data.Data
    ( Data )
import Data.Kind
    ( Type )
import Data.Proxy
    ( Proxy (..) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( FromText (..), TextDecodingError (..), ToText (..) )
import Fmt
    ( Buildable (..), blockListF', indentF )
import GHC.Generics
    ( Generic )
import Numeric.Natural
    ( Natural )
import Type.Reflection
    ( Typeable )

import qualified Cardano.Address as CA
import qualified Cardano.Address.Style.Shelley as CA
import qualified Cardano.Wallet.Address.Pool as AddressPool
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

-- | Convenient alias for commonly used class contexts on keys.
type SupportsDiscovery (n :: NetworkDiscriminant) k =
    ( MkKeyFingerprint k (Proxy n, k 'CredFromScriptK XPub)
    , MkKeyFingerprint k Address
    , AddressIndexDerivationType SharedKey ~ 'Soft
    , AddressCredential k ~ 'CredFromScriptK
    , SoftDerivation k
    , HasSNetworkId n
    )

{-------------------------------------------------------------------------------
    Address Pool
-------------------------------------------------------------------------------}
data SharedAddressPools (key :: Depth -> Type -> Type) = SharedAddressPools
    { externalPool :: !(SharedAddressPool 'UtxoExternal key)
    , internalPool :: !(SharedAddressPool 'UtxoInternal key)
    , pendingChangeIxs :: !(PendingIxs 'CredFromScriptK)
    }
    deriving stock (Generic, Show)

instance NFData (SharedAddressPools key)

instance Eq (SharedAddressPools key) where
    (SharedAddressPools ext1 int1 pend1) == (SharedAddressPools ext2 int2 pend2)
        =  AddressPool.addresses (getPool int1) == AddressPool.addresses (getPool int2)
        && AddressPool.addresses (getPool ext1) == AddressPool.addresses (getPool ext2)
        && pend1 == pend2

instance Buildable (SharedAddressPools key) where
    build (SharedAddressPools extPool intPool pending) = "\n"
        <> indentF 6 ("External pool:" <> build extPool)
        <> indentF 6 ("Internal pool:" <> build intPool)
        <> indentF 6 ("Change indexes: " <> indentF 4 chgsF)
      where
        chgsF = blockListF' "-" build (pendingIxsToList pending)

-- | An address pool which keeps track of shared addresses.
-- To create a new pool, see 'newSharedAddressPool'.
newtype SharedAddressPool (c :: Role) (key :: Depth -> Type -> Type) =
    SharedAddressPool {
        getPool ::
            AddressPool.Pool
                (KeyFingerprint "payment" key)
                (Index 'Soft 'CredFromScriptK)
    } deriving (Generic, Show)

instance NFData (SharedAddressPool c k)

instance Buildable (SharedAddressPool c k) where
    build (SharedAddressPool pool) = build pool

-- | Create a new shared address pool from complete script templates.
newSharedAddressPool
    :: forall n c key
     . ( key ~ SharedKey
       , SupportsDiscovery n key
       , Typeable c
       )
    => AddressPoolGap
    -> ScriptTemplate
    -> Maybe ScriptTemplate
    -> SharedAddressPool c key
newSharedAddressPool g payment delegation =
    SharedAddressPool $ AddressPool.new addressFromIx gap
  where
    gap = fromIntegral $ getAddressPoolGap g
    addressFromIx
        = unsafePaymentKeyFingerprint @key
        . constructAddressFromIx @n (roleVal @c) payment delegation

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
    , rewardAccountKey :: !(Maybe RewardAccount)
        -- ^ Reward account script hash associated with this wallet
    , poolGap :: !AddressPoolGap
        -- ^ Address pool gap to be used in the address pool of shared state
    , ready :: !(Readiness (SharedAddressPools k))
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
    SharedState a1 a2 a3 a4 a5 a6 ap == SharedState b1 b2 b3 b4 b5 b6 bp
        = and
            [ a1 == b1
            , a2 == b2
            , a3 == b3
            , a4 == b4
            , a5 == b5
            , a6 == b6
            , ap `match` bp
            ]
      where
        match Pending Pending = True
        match (Active sharedAddressPools1) (Active sharedAddressPools2)
            = sharedAddressPools1 == sharedAddressPools2
        match _ _ = False

instance PersistPublicKey (k 'AccountK) => Buildable (SharedState n k) where
    build st = "SharedState:\n"
        <> indentF 4 ("Derivation prefix: " <> build (toText $ derivationPrefix st))
        <> indentF 4 ("accountXPub:" <> build (accountXPub st))
        <> indentF 4 ("paymentTemplate:" <> build (paymentTemplate st))
        <> indentF 4 ("delegationTemplate:" <> build (delegationTemplate st))
        <> indentF 4 ("rewardAccountKey:" <> build (toText <$> rewardAccountKey st))
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


retrieveAllCosigners :: Script Cosigner -> [Cosigner]
retrieveAllCosigners = foldScript (:) []

{-------------------------------------------------------------------------------
    Template validation
-------------------------------------------------------------------------------}

data ErrScriptTemplate =
      ErrScriptTemplateInvalid !CredentialType !ErrValidateScriptTemplate
    | ErrScriptTemplateMissingKey !CredentialType !Text
    deriving (Show, Eq)

instance ToText ErrValidateScriptTemplate where
    toText = T.pack . prettyErrValidateScriptTemplate


{-------------------------------------------------------------------------------
    Address discovery
-------------------------------------------------------------------------------}

isShared
    :: forall n k. SupportsDiscovery n k
    => Address
    -> SharedState n k
    -> (Maybe (Index 'Soft 'CredFromScriptK, Role), SharedState n k)
isShared addrRaw st = case ready st of
    Pending -> nop
    Active (SharedAddressPools extPool intPool pending) ->
        if networkTag == networkDiscriminantBits (sNetworkId @n) then
            case paymentKeyFingerprint addrRaw of
                Left _ -> nop
                Right addr -> case ( AddressPool.lookup addr (getPool extPool)
                                   , AddressPool.lookup addr (getPool intPool)) of
                    (Just ix, Nothing) ->
                        let pool' = AddressPool.update addr (getPool extPool) in
                        ( Just (ix, UtxoExternal)
                        , st { ready = Active
                                 ( SharedAddressPools
                                     (SharedAddressPool pool')
                                     intPool
                                     pending )
                             } )
                    (Nothing, Just ix) ->
                        let pool' = AddressPool.update addr (getPool intPool) in
                        ( Just (ix, UtxoInternal)
                        , st { ready = Active
                                 ( SharedAddressPools
                                     extPool
                                     (SharedAddressPool pool')
                                     pending )
                             } )
                    _ -> nop
        else
            (Nothing, st)
  where
    nop = (Nothing, st)
    AddressParts _ networkTag _ = toAddressParts addrRaw

instance SupportsDiscovery n k => IsOurs (SharedState n k) Address
  where
    isOurs addr st =
        first (fmap (decoratePath st utxoExternal . fst)) (isShared addr st)

-- | Decorate an index with the derivation prefix corresponding to the state.
decoratePath
    :: SharedState n key
    -> Index 'Soft 'RoleK
    -> Index 'Soft 'CredFromScriptK
    -> NE.NonEmpty DerivationIndex
decoratePath st role' ix = NE.fromList
    [ DerivationIndex $ getIndex purpose
    , DerivationIndex $ getIndex coinType
    , DerivationIndex $ getIndex accIx
    , DerivationIndex $ getIndex role'
    , DerivationIndex $ getIndex ix
    ]
  where
    DerivationPrefix (purpose, coinType, accIx) = derivationPrefix st

instance IsOurs (SharedState n k) RewardAccount where
    isOurs account state@SharedState{derivationPrefix, rewardAccountKey} =
        let
            DerivationPrefix (purpose, coinType, accountIx) = derivationPrefix
            path = NE.fromList
                [ DerivationIndex $ getIndex purpose
                , DerivationIndex $ getIndex coinType
                , DerivationIndex $ getIndex accountIx
                , DerivationIndex $ getIndex mutableAccount
                , DerivationIndex $ getIndex @'Soft minBound
                ]
        in
            case rewardAccountKey of
                Just rewardAcct ->
                    (guard (account == rewardAcct) *> Just path, state)
                Nothing ->
                    (Nothing, state)

instance GetAccount (SharedState n k) k where
    getAccount = accountXPub

instance SupportsDiscovery n k => CompareDiscovery (SharedState n k) where
    compareDiscovery st a1 a2 = case ready st of
        Pending ->
            error "comparing addresses in pending shared state does not make sense"
        Active pools ->
            case (ix a1 pools, ix a2 pools) of
                (Nothing, Nothing) -> EQ
                (Nothing, Just _)  -> GT
                (Just _, Nothing)  -> LT
                (Just i1, Just i2) -> compare i1 i2
      where
        ix :: Address -> SharedAddressPools k -> Maybe (Index 'Soft 'CredFromScriptK)
        ix a (SharedAddressPools extPool intPool _) =
            case paymentKeyFingerprint a of
                Left _ -> Nothing
                Right addr ->
                    AddressPool.lookup addr (getPool extPool) <|>
                    AddressPool.lookup addr (getPool intPool)

instance HasSNetworkId n => KnownAddresses (SharedState n k) where
    knownAddresses st = case ready st of
        Pending -> []
        Active (SharedAddressPools extPool intPool ixs) ->
            nonChangeAddresses extPool <>
            usedChangeAddresses intPool <>
            pendingChangeAddresses intPool (pendingIxsToList ixs)
      where
        nonChangeAddresses extPool =
            map (swivel utxoExternal) $ L.sortOn idx $ Map.toList $
            AddressPool.addresses (getPool extPool)

        idx (_,(ix,_)) = ix

        swivel role' (k,(ix,s)) =
            (liftPaymentAddress @n k, s, decoratePath st role' ix)

        changeAddresses intPool =
            map (swivel utxoInternal) $ L.sortOn idx $ Map.toList $
            AddressPool.addresses (getPool intPool)
        usedChangeAddresses intPool =
            filter (\(_, status, _) -> status == Used) $
            changeAddresses intPool

        -- pick as many unused change addresses as there are pending
        -- transactions. Note: the last `internalGap` addresses are all
        -- unused.
        pendingChangeAddresses intPool ixs =
            let internalGap = AddressPool.gap $ getPool intPool
                changeAddresses' = changeAddresses intPool
                edgeChangeAddresses =
                    drop (length changeAddresses' - internalGap) changeAddresses'
            in take (length ixs) edgeChangeAddresses

instance MaybeLight (SharedState n k) where
    maybeDiscover = Nothing

instance GenChange (SharedState n k) where
    type ArgGenChange (SharedState n k) =
        (ScriptTemplate -> Maybe ScriptTemplate -> Index 'Soft 'CredFromScriptK -> Address)

    genChange mkAddress st = case ready st of
        Pending ->
            error "generating change in pending shared state does not make sense"
        Active (SharedAddressPools extPool intPool pending) ->
            let (ix, pending') = nextChangeIndex (getPool intPool) pending
                addr = mkAddress (paymentTemplate st) (delegationTemplate st) ix
            in (addr, st{ ready = Active (SharedAddressPools extPool intPool pending') })

{-------------------------------------------------------------------------------
    Address utilities
    Payment and Delegation parts
-------------------------------------------------------------------------------}

data CredentialType = Payment | Delegation
    deriving (Bounded, Data, Enum, Eq, Show, Generic)
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
    :: forall n (k :: Depth -> Type -> Type).
       HasSNetworkId n
    => KeyFingerprint "payment" k
    -> Address
liftPaymentAddress (KeyFingerprint fingerprint) =
    Address $ CA.unAddress $
    paymentAddress (toNetworkTag @n)
    (PaymentFromScriptHash (ScriptHash fingerprint))

liftDelegationAddress
    :: forall n (k :: Depth -> Type -> Type).
       HasSNetworkId n
    => Index 'Soft 'CredFromScriptK
    -> ScriptTemplate
    -> KeyFingerprint "payment" k
    -> Address
liftDelegationAddress ix dTemplate (KeyFingerprint fingerprint) =
    Address $ CA.unAddress $
    delegationAddress (toNetworkTag @n)
    (PaymentFromScriptHash (ScriptHash fingerprint))
    (delegationCredential dScript)
  where
    delegationCredential =
        DelegationFromScriptHash . toScriptHash
    dScript =
        replaceCosignersWithVerKeys CA.Stake dTemplate ix

isOwned
    :: HasSNetworkId n
    => SharedState n SharedKey
    -> (SharedKey 'RootK XPrv, Passphrase "encryption")
    -> Address
    -> Maybe (SharedKey 'CredFromScriptK XPrv, Passphrase "encryption")
isOwned st (rootPrv, pwd) addr = case isShared addr st of
    (Just (ix, role'), _) ->
        let DerivationPrefix (_, _, accIx) = derivationPrefix st
            accXPrv = deriveAccountPrivateKey pwd rootPrv accIx
        in  Just
                ( deriveAddressPrivateKey pwd accXPrv role' ix
                , pwd
                )
    (Nothing, _) -> Nothing

estimateMinWitnessRequiredPerInput :: Script k -> Natural
estimateMinWitnessRequiredPerInput = \case
    RequireSignatureOf _ -> 1
    RequireAllOf xs      ->
        sum $ map estimateMinWitnessRequiredPerInput xs
    RequireAnyOf xs      ->
        optimumIfNotEmpty minimum $ map estimateMinWitnessRequiredPerInput xs
    RequireSomeOf m xs   ->
        let smallestReqFirst =
                L.sort $ map estimateMinWitnessRequiredPerInput xs
        in sum $ take (fromIntegral m) smallestReqFirst
    ActiveFromSlot _     -> 0
    ActiveUntilSlot _    -> 0

optimumIfNotEmpty :: (Foldable t, Num p) => (t a -> p) -> t a -> p
optimumIfNotEmpty f xs =
    if null xs then
        0
    else f xs

estimateMaxWitnessRequiredPerInput :: Script k -> Natural
estimateMaxWitnessRequiredPerInput = \case
    RequireSignatureOf _ -> 1
    RequireAllOf xs      ->
        sum $ map estimateMaxWitnessRequiredPerInput xs
    RequireAnyOf xs      ->
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
    RequireSomeOf _m xs   ->
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
    ActiveFromSlot _     -> 0
    ActiveUntilSlot _    -> 0

instance AccountIxForStaking (SharedState n SharedKey) where
    getAccountIx st =
        let DerivationPrefix (_, _, ix) = derivationPrefix st
        in Just ix
