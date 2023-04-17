{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- We intentionally specify the constraint  (k == SharedKey) ~ 'False
-- in some exports.

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- An implementation of address discovery for the sequential address derivation
-- scheme specified in BIP-0044.
--
-- The management of _accounts_ is left-out for this implementation focuses on
-- a single account. In practice, one wants to manage a set of pools, one per
-- account.

module Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    (
      SupportsDiscovery

    -- * Sequential Derivation
    -- ** Address Pool Gap
    , AddressPoolGap
    , MkAddressPoolGapError (..)
    , defaultAddressPoolGap
    , getAddressPoolGap
    , mkAddressPoolGap
    , mkUnboundedAddressPoolGap

    -- ** Address Pool
    , SeqAddressPool (..)
    , getGap
    , newSeqAddressPool

    -- ** State
    , SeqState (..)
    , DerivationPrefix (..)
    , purposeBIP44
    , purposeCIP1852
    , coinTypeAda
    , mkSeqStateFromRootXPrv
    , mkSeqStateFromAccountXPub
    , discoverSeq
    , discoverSeqWithRewards

    -- ** Benchmarking
    , SeqAnyState (..)
    , mkSeqAnyState
    ) where

import Prelude

import Cardano.Address.Derivation
    ( xpubPublicKey, xpubToBytes )
import Cardano.Address.Script
    ( Cosigner (..), KeyHash (..), KeyRole (..), ScriptTemplate (..) )
import Cardano.Crypto.Wallet
    ( XPrv, XPub )
import Cardano.Wallet.Primitive.AddressDerivation
    ( AddressParts (..)
    , DelegationAddress (..)
    , Depth (..)
    , DerivationIndex (..)
    , DerivationPrefix (..)
    , DerivationType (..)
    , HardDerivation (..)
    , Index (..)
    , KeyFingerprint (..)
    , MkKeyFingerprint (..)
    , PaymentAddress (..)
    , PersistPublicKey (..)
    , Role (..)
    , SoftDerivation (..)
    , ToRewardAccount (..)
    , WalletKey (..)
    , liftDelegationAddressS
    , liftPaymentAddressS
    , roleVal
    , toAddressParts
    , unsafePaymentKeyFingerprint
    )
import Cardano.Wallet.Primitive.AddressDerivation.MintBurn
    ( derivePolicyPrivateKey )
import Cardano.Wallet.Primitive.AddressDerivation.SharedKey
    ( SharedKey (..) )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( CompareDiscovery (..)
    , GenChange (..)
    , GetAccount (..)
    , IsOurs (..)
    , IsOwned (..)
    , KnownAddresses (..)
    , MaybeLight (..)
    , PendingIxs
    , coinTypeAda
    , dropLowerPendingIxs
    , emptyPendingIxs
    , nextChangeIndex
    , pendingIxsToList
    )
import Cardano.Wallet.Primitive.BlockSummary
    ( ChainEvents )
import Cardano.Wallet.Primitive.Passphrase.Types
    ( Passphrase )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..), AddressState (..) )
import Cardano.Wallet.Primitive.Types.RewardAccount
    ( RewardAccount )
import Cardano.Wallet.Read.NetworkId
import Cardano.Wallet.Transaction
    ( ToWitnessCountCtx (..), WitnessCountCtx (..) )
import Codec.Binary.Encoding
    ( AbstractEncoding (..), encode )
import Control.Applicative
    ( (<|>) )
import Control.DeepSeq
    ( NFData (..), deepseq )
import Control.Monad
    ( unless )
import Data.Bifunctor
    ( first, second )
import Data.Digest.CRC32
    ( crc32 )
import Data.Kind
    ( Type )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Proxy
    ( Proxy (..) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( FromText (..), TextDecodingError (..), ToText (..) )
import Data.Text.Read
    ( decimal )
import Data.Type.Equality
    ( type (==) )
import Data.Word
    ( Word32 )
import Fmt
    ( Buildable (..), blockListF', hexF, indentF, prefixF, suffixF )
import GHC.Generics
    ( Generic )
import GHC.TypeLits
    ( KnownNat, Nat, natVal )
import Type.Reflection
    ( Typeable )

import qualified Cardano.Wallet.Address.Pool as AddressPool
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

-- | Convenient constraint alias for commonly used class contexts on keys.
type SupportsDiscovery n k =
    ( MkKeyFingerprint k (Proxy n, k 'CredFromKeyK XPub)
    , MkKeyFingerprint k Address
    , AddressCredential k ~ 'CredFromKeyK
    , SoftDerivation k
    , NetworkDiscriminantCheck k
    , HasSNetworkId n
    )

{-------------------------------------------------------------------------------
                              Address Pool Gap
-------------------------------------------------------------------------------}

-- | Maximum number of consecutive undiscovered addresses allowed
newtype AddressPoolGap = AddressPoolGap { getAddressPoolGap :: Word32 }
    deriving stock (Generic, Show, Eq, Ord)

instance NFData AddressPoolGap

instance FromText AddressPoolGap where
    fromText t = do
        (g, txt) <- first (const err) $ decimal t
        unless (T.null txt) $ Left err
        first (\case ErrGapOutOfRange{} -> err) (mkAddressPoolGap g)
      where
        err = TextDecodingError $
            "An address pool gap must be a natural number between "
                <> show (fromEnum $ minBound @AddressPoolGap)
                <> " and "
                <> show (fromEnum $ maxBound @AddressPoolGap)
                <> "."

instance ToText (AddressPoolGap) where
    toText = T.pack . show . getAddressPoolGap

instance Bounded AddressPoolGap where
    minBound = AddressPoolGap 10
    maxBound = AddressPoolGap 100_000

instance Enum AddressPoolGap where
    fromEnum (AddressPoolGap g) = fromEnum g
    toEnum g
        | AddressPoolGap (toEnum g) < minBound @AddressPoolGap =
            error "AddressPoolGap.toEnum: bad argument"
        | AddressPoolGap (toEnum g) > maxBound @AddressPoolGap =
            error "AddressPoolGap.toEnum: bad argument"
        | otherwise =
            AddressPoolGap (toEnum g)

-- | Smart constructor for 'AddressPoolGap'
mkAddressPoolGap :: Integer -> Either MkAddressPoolGapError AddressPoolGap
mkAddressPoolGap !g
    | g >= fromIntegral (getAddressPoolGap minBound) &&
      g <= fromIntegral (getAddressPoolGap maxBound) =
        Right $ AddressPoolGap (fromIntegral g)
    | otherwise =
        Left $ ErrGapOutOfRange g

-- | Constructor which allows by-passing the address pool gap boundary
-- limitations.
-- A practical use-case for this are sequential wallets for which we don't have
-- access to the whole history which therefore require using arbitrary big gaps
-- in order to discover addresses with indexes separated by possible huge gaps.
--
-- This defies a bit the purpose of this type though.
mkUnboundedAddressPoolGap :: Word32 -> AddressPoolGap
mkUnboundedAddressPoolGap = AddressPoolGap

-- | Possible errors when casting to an 'AddressPoolGap'
newtype MkAddressPoolGapError = ErrGapOutOfRange Integer
    deriving (Eq, Show)

-- | A default 'AddressPoolGap', as suggested in BIP-0044
defaultAddressPoolGap :: AddressPoolGap
defaultAddressPoolGap =
    AddressPoolGap 20

{-------------------------------------------------------------------------------
    Sequential address pools
-------------------------------------------------------------------------------}
-- | An address pool which keeps track of sequential addresses.
-- To create a new pool, see 'newSeqAddressPool'.
newtype SeqAddressPool (c :: Role) (key :: Depth -> Type -> Type) =
    SeqAddressPool {
        getPool ::
            AddressPool.Pool
                (KeyFingerprint "payment" key)
                (Index 'Soft 'CredFromKeyK)
    } deriving (Generic, Show)

instance NFData (SeqAddressPool c k)

instance Buildable (SeqAddressPool c k) where
    build (SeqAddressPool pool) = build pool

-- | Create a new Address pool from a list of addresses. Note that, the list is
-- expected to be ordered in sequence (first indexes, first in the list).
newSeqAddressPool
    :: forall n c key
     . ( SupportsDiscovery n key
       , Typeable c
       )
    => key 'AccountK XPub
    -> AddressPoolGap
    -> SeqAddressPool c key
newSeqAddressPool account g =
    SeqAddressPool $ AddressPool.new addressFromIx gap
  where
    gap = fromIntegral $ getAddressPoolGap g
    addressFromIx ix =
        unsafePaymentKeyFingerprint @key
            ( Proxy @n
            , deriveAddressPublicKey @key account (roleVal @c) ix
            )

getGap :: SeqAddressPool c k -> AddressPoolGap
getGap = AddressPoolGap . fromIntegral . AddressPool.gap . getPool

{-------------------------------------------------------------------------------
    Pretty printing
-------------------------------------------------------------------------------}

instance PersistPublicKey (key 'AccountK) =>
    Buildable (key 'AccountK XPub) where
    build key = prefixF 8 xpubF <> "..." <> suffixF 8 xpubF
      where
        xpubF = hexF $ serializeXPub key

instance PersistPublicKey (key 'PolicyK) => Buildable (key 'PolicyK XPub) where
    build key = prefixF 8 xpubF <> "..." <> suffixF 8 xpubF
      where
        xpubF = hexF $ serializeXPub key

instance Buildable (ScriptTemplate, Maybe ScriptTemplate) where
    build (paymentTemplate, delegationTemplateM) =
        " payment script credential: " <> scriptPaymentF <>
        " delegation script credential: " <> scriptDelegationF
      where
        scriptPaymentF = build paymentTemplate
        scriptDelegationF = maybe "absent" build delegationTemplateM

accXPubTxt :: XPub -> Text
accXPubTxt xpub =
    let keyFormatted = T.decodeUtf8 $ encode EBase16 $ xpubPublicKey xpub
    in T.take 8 keyFormatted <> "..." <> T.takeEnd 8 keyFormatted

instance Buildable ScriptTemplate where
    build (ScriptTemplate cosignersMap script') =
        "Cosigners:" <> build (presentCosigners cosignersMap) <>
        " Script:" <> build (T.pack (show script'))
      where
        printCosigner (Cosigner ix) = "cosigner#" <> T.pack (show ix)
        presentCosigners = (`Map.foldrWithKey` mempty) $ \c k acc ->
            acc <> "| " <> printCosigner c <> " " <> accXPubTxt k

{-------------------------------------------------------------------------------
    SeqState
-------------------------------------------------------------------------------}

-- | A state to keep track of sequential addresses as described in
-- [BIP-44](https://github.com/bitcoin/bips/blob/master/bip-0044.mediawiki)
--
-- Internally, the state keeps track of a few things for us and is it is
-- parameterized by a type @n@ which captures a particular network discrimination.
-- This enables the state to be agnostic to the underlying address format.
data SeqState (n :: NetworkDiscriminant) k = SeqState
    { internalPool :: !(SeqAddressPool 'UtxoInternal k)
        -- ^ Addresses living on the 'UtxoInternal'
    , externalPool :: !(SeqAddressPool 'UtxoExternal k)
        -- ^ Addresses living on the 'UtxoExternal'
    , pendingChangeIxs :: !(PendingIxs 'CredFromKeyK)
        -- ^ Indexes from the internal pool that have been used in pending
        -- transactions. The list is maintained sorted in descending order
        -- (cf: 'PendingIxs')
    , accountXPub :: k 'AccountK XPub
        -- ^ The account public key associated with this state
    , policyXPub :: Maybe (k 'PolicyK XPub)
        -- ^ The policy public key associated with this state derived for
        -- policy key hardened index=0
    , rewardAccountKey :: k 'CredFromKeyK XPub
        -- ^ Reward account public key associated with this wallet
    , derivationPrefix :: DerivationPrefix
        -- ^ Derivation path prefix from a root key up to the internal account
    }
    deriving stock (Generic)

deriving instance
    ( Show (k 'AccountK XPub)
    , Show (k 'CredFromKeyK XPub)
    , Show (k 'PolicyK XPub)
    , Show (KeyFingerprint "payment" k)
    ) => Show (SeqState n k)

instance
    ( NFData (k 'AccountK XPub)
    , NFData (k 'CredFromKeyK XPub)
    , NFData (k 'PolicyK XPub)
    , NFData (KeyFingerprint "payment" k)
    )
    => NFData (SeqState n k)

-- Hand-written, because 'AddressPool.Pool' is not an instance of 'Eq'.
instance
    ( Eq (k 'AccountK XPub)
    , Eq (k 'CredFromKeyK XPub)
    , Eq (k 'PolicyK XPub)
    , Eq (KeyFingerprint "payment" k)
    ) => Eq (SeqState n k)
  where
    SeqState ai ae a1 a2 a3 a4 a5 == SeqState bi be b1 b2 b3 b4 b5
        = and
            [a1 == b1, a2 == b2, a3 == b3, a4 == b4, a5 == b5
            , ae `match` be, ai `match` bi
            ]
      where
        match (SeqAddressPool a) (SeqAddressPool b)
            = AddressPool.addresses a == AddressPool.addresses b
            && AddressPool.gap a == AddressPool.gap b

instance Buildable (SeqState n k) where
    build st = "SeqState:\n"
        <> indentF 4 ("Derivation prefix: " <>
            build (toText (derivationPrefix st)))
        <> indentF 4 (build $ internalPool st)
        <> indentF 4 (build $ externalPool st)
        <> indentF 4 ("Change indexes: " <> indentF 4 chgsF)
      where
        chgsF = blockListF' "-" build (pendingIxsToList $ pendingChangeIxs st)

-- | Purpose is a constant set to 44' (or 0x8000002C) following the original
-- BIP-44 specification.
--
-- It indicates that the subtree of this node is used according to this
-- specification.
--
-- Hardened derivation is used at this level.
purposeBIP44 :: Index 'Hardened 'PurposeK
purposeBIP44 = toEnum 0x8000_002C

-- | Purpose is a constant set to 1852' (or 0x8000073c) following the BIP-44
-- extension for Cardano:
--
-- https://github.com/input-output-hk/implementation-decisions/blob/
-- 2d1bed5e617f0907bc5e12cf1c3f3302a4a7c42/text/1852-hd-chimeric.md
--
-- It indicates that the subtree of this node is used according to this
-- specification.
--
-- Hardened derivation is used at this level.
purposeCIP1852 :: Index 'Hardened 'PurposeK
purposeCIP1852 = toEnum 0x8000_073c

-- | Construct a Sequential state for a wallet
-- from root private key and password.
mkSeqStateFromRootXPrv
    :: forall n k.
        ( WalletKey k
        , SupportsDiscovery n k
        , (k == SharedKey) ~ 'False
        )
    => (k 'RootK XPrv, Passphrase "encryption")
    -> Index 'Hardened 'PurposeK
    -> AddressPoolGap
    -> SeqState n k
mkSeqStateFromRootXPrv (rootXPrv, pwd) =
    mkSeqStateFromAccountXPub
        (publicKey $ deriveAccountPrivateKey pwd rootXPrv minBound)
            $ Just
            $ publicKey
            $ liftRawKey
            $ derivePolicyPrivateKey pwd (getRawKey rootXPrv) minBound

-- | Construct a Sequential state for a wallet from public account key.
mkSeqStateFromAccountXPub
    :: forall (n :: NetworkDiscriminant) k.
        ( SupportsDiscovery n k
        , (k == SharedKey) ~ 'False
        )
    => k 'AccountK XPub
    -> Maybe (k 'PolicyK XPub)
    -> Index 'Hardened 'PurposeK
    -> AddressPoolGap
    -> SeqState n k
mkSeqStateFromAccountXPub accXPub policyXPubM purpose g = SeqState
    { internalPool = newSeqAddressPool @n accXPub g
    , externalPool = newSeqAddressPool @n accXPub g
    , accountXPub = accXPub
    , policyXPub = policyXPubM
    , rewardAccountKey = rewardXPub
    , pendingChangeIxs = emptyPendingIxs
    , derivationPrefix = DerivationPrefix ( purpose, coinTypeAda, minBound )
    }
  where
    -- This matches the reward address for "normal wallets". The accountXPub
    -- is the first account, minBound being the first Soft index
    rewardXPub = deriveAddressPublicKey accXPub MutableAccount minBound

-- | Decorate an index with the derivation prefix corresponding to the state.
decoratePath
    :: SeqState n k -> Role -> Index 'Soft 'CredFromKeyK
    -> NE.NonEmpty DerivationIndex
decoratePath st r ix = NE.fromList
    [ DerivationIndex $ getIndex purpose
    , DerivationIndex $ getIndex coinType
    , DerivationIndex $ getIndex accountIx
    , DerivationIndex $ fromIntegral $ fromEnum r
    , DerivationIndex $ getIndex ix
    ]
  where
    SeqState
        { derivationPrefix = DerivationPrefix (purpose, coinType, accountIx)
        } = st



-- NOTE
-- We have to scan both the internal and external chain. Note that, the
-- BIP-44 account discovery algorithm is only specified for the external
-- chain so in theory, there's nothing forcing a wallet to generate change
-- addresses on the internal chain anywhere in the available range.
instance SupportsDiscovery n k => IsOurs (SeqState n k) Address where
    isOurs addrRaw st =
        if networkDiscriminantCheck @k (sNetworkId @n) networkTag then
            case paymentKeyFingerprint addrRaw of
                Left _ -> (Nothing, st)
                Right addr ->
                    let (internalIndex, !internalPool') =
                            lookupAddress addr (internalPool st)
                        (externalIndex, !externalPool') =
                            lookupAddress addr (externalPool st)

                        ours = (decoratePath st UtxoExternal <$> externalIndex)
                           <|> (decoratePath st UtxoInternal <$> internalIndex)

                        pendingChangeIxs' =
                            case internalIndex of
                                Nothing -> pendingChangeIxs st
                                Just ix ->
                                    dropLowerPendingIxs ix (pendingChangeIxs st)
                    in
                        ( ours `deepseq` ours
                        , st { internalPool = internalPool'
                             , externalPool = externalPool'
                             , pendingChangeIxs =
                                   pendingChangeIxs' `deepseq` pendingChangeIxs'
                             }
                        )
        else
            (Nothing, st)
      where
        AddressParts _ networkTag _ = toAddressParts addrRaw
        lookupAddress addr (SeqAddressPool pool) =
            case AddressPool.lookup addr pool of
                Nothing ->
                    (Nothing, SeqAddressPool pool)
                Just ix ->
                    (Just ix, SeqAddressPool $ AddressPool.update addr pool)

instance
    ( SoftDerivation k, AddressCredential k ~ 'CredFromKeyK
    ) => GenChange (SeqState n k) where
    -- | We pick indexes in sequence from the first known available index (i.e.
    -- @length addrs - gap@) but we do not generate _new change addresses_. As a
    -- result, we can't generate more than @gap@ _pending_ change addresses and
    -- therefore, rotate the change addresses when we need extra change outputs.
    --
    -- See also: 'nextChangeIndex'
    type ArgGenChange (SeqState n k) =
        (k 'CredFromKeyK XPub -> k 'CredFromKeyK XPub -> Address)

    genChange mkAddress st = (addr, st{pendingChangeIxs=pending'})
      where
        (ix, pending') =
            nextChangeIndex (getPool $ internalPool st) (pendingChangeIxs st)
        addressXPub = deriveAddressPublicKey (accountXPub st) UtxoInternal ix
        addr = mkAddress addressXPub (rewardAccountKey st)

instance
    ( IsOurs (SeqState n k) Address
    , SupportsDiscovery n k
    , AddressIndexDerivationType k ~ 'Soft
    )
    => IsOwned (SeqState n k) k 'CredFromKeyK where
    isOwned st (rootPrv, pwd) addrRaw =
        case paymentKeyFingerprint addrRaw of
            Left _ -> Nothing
            Right addr ->
                let
                    xPrv1 = lookupAndDeriveXPrv addr (internalPool st)
                    xPrv2 = lookupAndDeriveXPrv addr (externalPool st)
                    xPrv = xPrv1 <|> xPrv2
                in
                    (,pwd) <$> xPrv
      where
        -- We are assuming there is only one account
        accountPrv = deriveAccountPrivateKey pwd rootPrv minBound

        lookupAndDeriveXPrv
            :: forall c. (Typeable c)
            => KeyFingerprint "payment" k
            -> SeqAddressPool c k
            -> Maybe (k 'CredFromKeyK XPrv)
        lookupAndDeriveXPrv addr (SeqAddressPool pool) =
                deriveAddressPrivateKey pwd accountPrv (roleVal @c)
            <$> AddressPool.lookup addr pool

instance SupportsDiscovery n k => CompareDiscovery (SeqState n k) where
    compareDiscovery (SeqState !s1 !s2 _ _ _ _ _) a1 a2 =
        case (ix a1 s1 <|> ix a1 s2, ix a2 s1 <|> ix a2 s2) of
            (Nothing, Nothing) -> EQ
            (Nothing, Just _)  -> GT
            (Just _, Nothing)  -> LT
            (Just i1, Just i2) -> compare i1 i2
      where
        ix :: Address -> SeqAddressPool c k -> Maybe (Index 'Soft 'CredFromKeyK)
        ix a (SeqAddressPool pool) = case paymentKeyFingerprint a of
            Left _ -> Nothing
            Right addr -> AddressPool.lookup addr pool

instance (PaymentAddress k 'CredFromKeyK, HasSNetworkId n)
    => KnownAddresses (SeqState n k) where
    knownAddresses st =
        nonChangeAddresses <> usedChangeAddresses <> pendingChangeAddresses
      where
        -- | List addresses in order of increasing indices.
        listAddresses
            :: forall c
             . Typeable c
            => SeqAddressPool c k
            -> [(Address, AddressState, NonEmpty DerivationIndex)]
        listAddresses (SeqAddressPool pool) =
            map shuffle . L.sortOn idx . Map.toList $ AddressPool.addresses pool
          where
            idx (_, (ix, _)) = ix
            shuffle (k, (ix, s)) =
                ( liftPaymentAddressS @n @k @'CredFromKeyK k
                , s
                , decoratePath st (roleVal @c) ix
                )
        nonChangeAddresses = listAddresses $ externalPool st
        changeAddresses = listAddresses $ internalPool st
        usedChangeAddresses = [addr | addr@(_, Used, _) <- changeAddresses]
        -- pick as many unused change addresses as there are pending
        -- transactions. Note: the last `internalGap` addresses are all
        -- unused.
        pendingChangeAddresses = take (length ixs) edgeChangeAddresses
          where
            ixs = pendingIxsToList $ pendingChangeIxs st
            internalGap = AddressPool.gap $ getPool $ internalPool st
            edgeChangeAddresses =
                drop (length changeAddresses - internalGap) changeAddresses

instance GetAccount (SeqState n k) k where
    getAccount = accountXPub

-- | Discover addresses and transactions using an
-- efficient query @addr -> m txs@.
-- Does /not/ take 'RewardAccount' into account.
discoverSeq
    :: forall n k m. (PaymentAddress k 'CredFromKeyK, Monad m, HasSNetworkId n)
    => (Either Address RewardAccount -> m ChainEvents)
    -> SeqState n k -> m (ChainEvents, SeqState n k)
discoverSeq query state = do
    (eventsInternal, internalPool') <- discover (internalPool state)
    (eventsExternal, externalPool') <- discover (externalPool state)
    let discoveredEvents = eventsInternal <> eventsExternal
        state' = state
            { internalPool = internalPool'
            , externalPool = externalPool'
            , pendingChangeIxs =
                dropLowerPendingIxs
                    (AddressPool.nextIndex (getPool internalPool'))
                    (pendingChangeIxs state)
            }
    pure (discoveredEvents, state')
  where
    -- Only enterprise address (for legacy Icarus keys)
    fromPayment = liftPaymentAddressS @n @k @'CredFromKeyK
    discover :: SeqAddressPool r k -> m (ChainEvents, SeqAddressPool r k)
    discover = fmap (second SeqAddressPool)
        . AddressPool.discover (query . Left . fromPayment) . getPool

-- | Discover addresses and transactions using an
-- efficient query @addr -> m txs@.
-- Does take 'RewardAccount' into account.
discoverSeqWithRewards
    :: forall n k m
     . ( DelegationAddress k 'CredFromKeyK
       , ToRewardAccount k
       , Monad m
       , HasSNetworkId n
       )
    => (Either Address RewardAccount -> m ChainEvents)
    -> SeqState n k
    -> m (ChainEvents, SeqState n k)
discoverSeqWithRewards query state = do
    eventsReward <- query . Right $ toRewardAccount (rewardAccountKey state)
    (eventsInternal, internalPool') <- discover (internalPool state)
    (eventsExternal, externalPool') <- discover (externalPool state)
    let discoveredEvents = eventsReward <> eventsInternal <> eventsExternal
        state' = state
            { internalPool = internalPool'
            , externalPool = externalPool'
            , pendingChangeIxs =
                dropLowerPendingIxs
                    (AddressPool.nextIndex (getPool internalPool'))
                    (pendingChangeIxs state)
            }
    pure (discoveredEvents, state')
  where
    -- Every 'Address' is composed of a payment part and a staking part.
    -- Ideally, we would want 'query' to give us all transactions
    -- belonging to a given payment part, regardless of the staking parts
    -- that are paired with that payment part.
    -- Unfortunately, this is not possible at the moment.
    -- However, fortunately, the staking part is always the same,
    -- so we supply it here in order to obtain an 'Address' that we can query.
    fromPayment hash = liftDelegationAddressS @n hash (rewardAccountKey state)

    discover :: SeqAddressPool r k -> m (ChainEvents, SeqAddressPool r k)
    discover = fmap (second SeqAddressPool)
        . AddressPool.discover (query . Left . fromPayment) . getPool

{-------------------------------------------------------------------------------
    SeqAnyState

    For benchmarking and testing arbitrary large sequential wallets.
-------------------------------------------------------------------------------}

-- | An "unsound" alternative that can be used for benchmarking and stress
-- testing. It re-uses the same underlying structure as the `SeqState` but
-- it discovers addresses based on an arbitrary ratio instead of respecting
-- BIP-44 discovery.
--
-- The proportion is stored as a type-level parameter so that we don't have to
-- alter the database schema to store it. It simply exists and depends on the
-- caller creating the wallet to define it.
newtype SeqAnyState (network :: NetworkDiscriminant) key (p :: Nat) =
    SeqAnyState { innerState :: SeqState network key }
    deriving (Generic)

deriving instance
    ( Show (k 'AccountK XPub)
    , Show (k 'CredFromKeyK XPub)
    , Show (k 'PolicyK XPub)
    , Show (KeyFingerprint "payment" k)
    )
    => Show (SeqAnyState n k p)

instance
    ( NFData (k 'AccountK XPub)
    , NFData (k 'CredFromKeyK XPub)
    , NFData (k 'PolicyK XPub)
    , NFData (KeyFingerprint "payment" k)
    )
    => NFData (SeqAnyState n k p)

-- | Initialize the HD random address discovery state from a root key and RNG
-- seed.
--
-- The type parameter is expected to be a ratio of addresses we ought to simply
-- recognize as ours. It is expressed in per-myriad, so "1" means 0.01%,
-- "100" means 1% and 10000 means 100%.
mkSeqAnyState
    :: forall (p :: Nat) n k.
        ( SupportsDiscovery n k
        , WalletKey k
        , (k == SharedKey) ~ 'False
        )
    => (k 'RootK XPrv, Passphrase "encryption")
    -> Index 'Hardened 'PurposeK
    -> AddressPoolGap
    -> SeqAnyState n k p
mkSeqAnyState credentials purpose poolGap =
    SeqAnyState{innerState = mkSeqStateFromRootXPrv credentials purpose poolGap}

instance KnownNat p => IsOurs (SeqAnyState n k p) Address where
    isOurs (Address bytes) st@(SeqAnyState inner)
        | crc32 bytes < p =
            let
                pool = getPool $ externalPool inner
                ix = toEnum $ AddressPool.size pool - AddressPool.gap pool
                addr = AddressPool.addressFromIx pool ix
                pool' = AddressPool.update addr pool
                path = DerivationIndex (getIndex ix) :| []
            in
                ( Just path
                , SeqAnyState $ inner{externalPool = SeqAddressPool pool'}
                )
        | otherwise =
            (Nothing, st)
      where
        p = floor (double sup * double (natVal (Proxy @p)) / 10_000)
          where
            sup = maxBound :: Word32

        double :: Integral a => a -> Double
        double = fromIntegral

instance IsOurs (SeqAnyState n k p) RewardAccount where
    isOurs _account state = (Nothing, state)

instance
    ( AddressIndexDerivationType k ~ 'Soft
    , KnownNat p
    ) => IsOwned (SeqAnyState n k p) k 'CredFromKeyK
  where
    isOwned _ _ _ = Nothing

instance
    ( SoftDerivation k
    , AddressCredential k ~ 'CredFromKeyK
    ) => GenChange (SeqAnyState n k p)
  where
    type ArgGenChange (SeqAnyState n k p) = ArgGenChange (SeqState n k)
    genChange a (SeqAnyState s) = SeqAnyState <$> genChange a s

instance SupportsDiscovery n k => CompareDiscovery (SeqAnyState n k p) where
    compareDiscovery (SeqAnyState s) = compareDiscovery s

instance (PaymentAddress k 'CredFromKeyK, HasSNetworkId n)
    => KnownAddresses (SeqAnyState n k p) where
    knownAddresses (SeqAnyState s) = knownAddresses s

instance MaybeLight (SeqAnyState n k p) where
    maybeDiscover = Nothing

instance WalletKey k => ToWitnessCountCtx (SeqState n k) where
    toWitnessCountCtx s =
        let keyM = policyXPub s
        in case keyM of
            Just key ->
                ShelleyWalletCtx $ KeyHash Policy (xpubToBytes $ getRawKey key)
            Nothing -> AnyWitnessCountCtx
