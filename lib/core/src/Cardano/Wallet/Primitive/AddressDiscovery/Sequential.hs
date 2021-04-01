{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
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
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

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
    -- * Sequential Derivation
    -- ** Address Pool Gap
      AddressPoolGap
    , MkAddressPoolGapError (..)
    , defaultAddressPoolGap
    , getAddressPoolGap
    , mkAddressPoolGap
    , mkUnboundedAddressPoolGap

    -- ** Address Pool
    , AddressPool
    , ParentContext (..)
    , gap
    , addresses
    , role
    , context
    , mkAddressPool
    , lookupAddress
    , shrinkPool
    , unsafePaymentKeyFingerprint

    -- * Pending Change Indexes
    , PendingIxs
    , emptyPendingIxs
    , pendingIxsToList
    , pendingIxsFromList

    -- ** State
    , SeqState (..)
    , DerivationPrefix (..)
    , purposeBIP44
    , purposeCIP1852
    , coinTypeAda
    , mkSeqStateFromRootXPrv
    , mkSeqStateFromAccountXPub

    -- ** Benchmarking
    , SeqAnyState (..)
    , mkSeqAnyState
    ) where

import Prelude

import Cardano.Address.Derivation
    ( xpubPublicKey )
import Cardano.Address.Script
    ( Cosigner (..), ScriptTemplate (..) )
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
    , Passphrase (..)
    , PaymentAddress (..)
    , PersistPublicKey (..)
    , Role (..)
    , SoftDerivation (..)
    , WalletKey (..)
    , deriveRewardAccount
    , utxoExternal
    , utxoInternal
    )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( CompareDiscovery (..)
    , GenChange (..)
    , IsOurs (..)
    , IsOwned (..)
    , KnownAddresses (..)
    , coinTypeAda
    )
import Cardano.Wallet.Primitive.AddressDiscovery.Script
    ( constructAddressFromIx )
import Cardano.Wallet.Primitive.Types
    ( invariant )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..), AddressState (..) )
import Cardano.Wallet.Primitive.Types.RewardAccount
    ( RewardAccount )
import Codec.Binary.Encoding
    ( AbstractEncoding (..), encode )
import Control.Applicative
    ( (<|>) )
import Control.DeepSeq
    ( NFData (..), deepseq )
import Control.Monad
    ( unless )
import Data.Bifunctor
    ( first )
import Data.Coerce
    ( coerce )
import Data.Digest.CRC32
    ( crc32 )
import Data.Function
    ( (&) )
import Data.Kind
    ( Type )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Map.Strict
    ( Map )
import Data.Maybe
    ( fromMaybe )
import Data.Proxy
    ( Proxy (..) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( FromText (..), TextDecodingError (..), ToText (..) )
import Data.Text.Read
    ( decimal )
import Data.Type.Equality
    ( (:~:) (..), testEquality )
import Data.Word
    ( Word32 )
import Fmt
    ( Buildable (..), blockListF', hexF, indentF, prefixF, suffixF )
import GHC.Generics
    ( Generic )
import GHC.Stack
    ( HasCallStack )
import GHC.TypeLits
    ( KnownNat, Nat, natVal )
import Type.Reflection
    ( Typeable, typeRep )

import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

{-------------------------------------------------------------------------------
                              Address Pool Gap
-------------------------------------------------------------------------------}

-- | Maximum number of consecutive undiscovered addresses allowed
newtype AddressPoolGap = AddressPoolGap
    { getAddressPoolGap :: Word32 }
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
                                 Address Pool
-------------------------------------------------------------------------------}

data ParentContext (chain :: Role) (key :: Depth -> Type -> Type) where
    ParentContextUtxoExternal
        :: key 'AccountK XPub
        -> ParentContext 'UtxoExternal key

    ParentContextUtxoInternal
        :: key 'AccountK XPub
        -> ParentContext 'UtxoInternal key

    ParentContextMultisigScript
        :: key 'AccountK XPub
        -> ScriptTemplate
        -> Maybe ScriptTemplate
        -> ParentContext 'MultisigScript key

deriving instance Eq   (key 'AccountK XPub) => Eq   (ParentContext chain key)
deriving instance Show (key 'AccountK XPub) => Show (ParentContext chain key)

instance (WalletKey key) => Buildable (ParentContext chain key) where
    build (ParentContextUtxoExternal acct) =
        mempty <> "(ParentContext : "<> build (accXPubTxt (getRawKey acct)) <>")"
    build (ParentContextUtxoInternal acct) =
        mempty <> "(ParentContext : "<> build (accXPubTxt (getRawKey acct)) <>")"
    build (ParentContextMultisigScript acct p dM) =
        mempty <> "(ParentContext : "<> build (accXPubTxt (getRawKey acct))
        <> ", " <> build (p, dM) <> ")"

instance NFData (key 'AccountK XPub) => NFData (ParentContext chain key) where
    rnf = \case
        ParentContextUtxoExternal acct  -> rnf acct
        ParentContextUtxoInternal acct  -> rnf acct
        ParentContextMultisigScript acct p d -> rnf (acct, p, d)

-- | An 'AddressPool' which keeps track of sequential addresses within a given
-- Account and change chain. See 'mkAddressPool' to create a new or existing
-- pool:
--
-- >>> mkAddressPool xpub gap role mempty
-- AddressPool { }
data AddressPool
    (chain :: Role)
    (key :: Depth -> Type -> Type) = AddressPool
    { context
        :: ParentContext chain key
        -- ^ Context of given address pool
    , gap
        :: !AddressPoolGap
        -- ^ The actual gap for the pool. This can't change for a given pool.
    , indexedKeys
        :: !(Map
                (KeyFingerprint "payment" key)
                (Index 'Soft 'AddressK, AddressState)
            )
    } deriving (Generic)

deriving instance (Show (key 'AccountK XPub), Show (ParentContext chain key))
    => Show (AddressPool chain key)

deriving instance (Eq (key 'AccountK XPub), Eq (ParentContext chain key))
    => Eq (AddressPool chain key)

instance (NFData (key 'AccountK XPub), NFData (ParentContext chain key))
    => NFData (AddressPool chain key)

instance PersistPublicKey (key 'AccountK) => Buildable (key 'AccountK XPub) where
    build key = prefixF 8 xpubF <> "..." <> suffixF 8 xpubF
      where
        xpubF = hexF $ serializeXPub key

instance Buildable (ScriptTemplate, Maybe ScriptTemplate) where
    build (paymentTemplate, delegationTemplateM) =
        mempty <> " payment script credential: " <> scriptPaymentF <>
        " delegation script credential: " <> scriptDelegationF
      where
        scriptPaymentF = build paymentTemplate
        scriptDelegationF = maybe "absent" build delegationTemplateM

accXPubTxt :: XPub -> Text
accXPubTxt xpub =
    let keyFormatted = T.decodeUtf8 $ encode EBase16 $ xpubPublicKey xpub
    in T.take 8 keyFormatted <> "..." <> T.takeEnd 8 keyFormatted

instance Buildable ScriptTemplate where
    build (ScriptTemplate cosignersMap script') = mempty <>
        "Cosigners:" <> build (presentCosigners cosignersMap) <>
        " Script:" <> build (T.pack (show script'))
      where
        printCosigner (Cosigner ix) =
            "cosigner#"<> T.pack (show ix)
        presentCosigners =
            Map.foldrWithKey (\c k acc -> acc <> "| " <> printCosigner c <> " " <> accXPubTxt k ) mempty

instance (Typeable chain, WalletKey key) => Buildable (AddressPool chain key) where
    build (AddressPool ctx (AddressPoolGap g) _) = mempty
        <> ccF <> " " <> build ctx <> " (gap=" <> build g <> ")\n"
      where
        ccF = build $ toText $ role @chain

-- | Bring a 'Role' type back to the term-level. This requires a type
-- application and either a scoped type variable, or an explicit passing of a
-- 'Role'.
--
-- >>> role @'UtxoExternal
-- UtxoExternal
--
-- >>> role @chain
-- ...
role :: forall (c :: Role). Typeable c => Role
role = fromMaybe (error $ "role: unmatched type" <> show (typeRep @c))
       (tryUtxoExternal <|> tryUtxoInternal <|> tryMultisigScript <|> tryMutableAccount)
  where
    tryUtxoExternal =
        case testEquality (typeRep @c) (typeRep @'UtxoExternal) of
            Just Refl  -> Just UtxoExternal
            Nothing -> Nothing
    tryUtxoInternal =
        case testEquality (typeRep @c) (typeRep @'UtxoInternal) of
            Just Refl  -> Just UtxoInternal
            Nothing -> Nothing
    tryMultisigScript =
        case testEquality (typeRep @c) (typeRep @'MultisigScript) of
            Just Refl  -> Just MultisigScript
            Nothing -> Nothing
    tryMutableAccount =
        case testEquality (typeRep @c) (typeRep @'MutableAccount) of
            Just Refl  -> Just MutableAccount
            Nothing -> Nothing

roleIndex :: forall (c :: Role). Typeable c => DerivationIndex
roleIndex = fromMaybe (error $ "roleIndex: unmatched type" <> show (typeRep @c))
       (tryUtxoExternal <|> tryUtxoInternal <|> tryMutableAccount)
  where
    tryUtxoExternal =
        case testEquality (typeRep @c) (typeRep @'UtxoExternal) of
            Just Refl  -> Just (DerivationIndex 0)
            Nothing -> Nothing
    tryUtxoInternal =
        case testEquality (typeRep @c) (typeRep @'UtxoInternal) of
            Just Refl  -> Just (DerivationIndex 1)
            Nothing -> Nothing
    tryMutableAccount =
        case testEquality (typeRep @c) (typeRep @'MutableAccount) of
            Just Refl  -> Just (DerivationIndex 2)
            Nothing -> Nothing

-- | Get all addresses in the pool, sorted from the first address discovered,
-- up until the next one.
--
-- In practice, we always have:
--
-- > mkAddressPool key g cc (map (\(a,s,_,_) -> (a,s)) $ addresses pool) == pool
addresses
    :: forall c k. Typeable c
    => (KeyFingerprint "payment" k -> Address)
    -> AddressPool c k
    -> [(Address, AddressState, DerivationIndex, DerivationIndex)]
addresses mkAddress =
    map (\(k, (ix, st)) -> (mkAddress k, st, roleIndex @c, DerivationIndex $ getIndex ix))
    . L.sortOn (fst . snd)
    . Map.toList
    . indexedKeys

-- | Create a new Address pool from a list of addresses. Note that, the list is
-- expected to be ordered in sequence (first indexes, first in the list).
--
-- The pool will grow from the start if less than @g :: AddressPoolGap@ are
-- given, such that, there are always @g@ undiscovered addresses in the pool.
--
-- FIXME:
-- Don't construct from addresses but from fingerprints!
mkAddressPool
    :: forall (n :: NetworkDiscriminant) c k.
        ( MkKeyFingerprint k (Proxy n, k 'AddressK XPub)
        , MkKeyFingerprint k Address
        , SoftDerivation k
        , Typeable c
        , Typeable n
        )
    => ParentContext c k
    -> AddressPoolGap
    -> [(Address, AddressState)]
    -> AddressPool c k
mkAddressPool ctx g addrs = AddressPool
    { context = ctx
    , gap = g
    , indexedKeys = mconcat
        [ Map.fromList $ zipWith (\(addr, status) ix -> (addr, (ix, status)))
            (first (unsafePaymentKeyFingerprint @k) <$> addrs)
            [minBound..maxBound]
        , nextAddresses @n @c
            ctx
            g
            minBound
        ]
    }

-- When discovering sequential wallets from a snapshot, we have to use
-- arbitrarily big pools for scanning the genesis block. However, keeping such
-- big pools with huge gaps makes for poor storage, memory and time performances.
-- So, once the genesis block has been scanned, we try to shrink the pool back
-- to something sensible.
shrinkPool
    :: forall (n :: NetworkDiscriminant) c key.
        ( Typeable c
        , MkKeyFingerprint key Address
        , MkKeyFingerprint key (Proxy n, key 'AddressK XPub)
        , SoftDerivation key
        , Typeable n
        )
    => (KeyFingerprint "payment" key -> Address)
        -- ^ A way to lift fingerprint back into an 'Address'
    -> [Address]
        -- ^ A set of known addresses. Will shrink the pool down to the latest
        -- known address from this list, while respecting the new gap.
    -> AddressPoolGap
        -- ^ A new address pool gap for this pool
    -> AddressPool c key
        -- ^ Original pool
    -> AddressPool c key
shrinkPool mkAddress knownAddrs newGap pool =
    let
        keys  = indexedKeys pool
        maxV  = maximumValue
            (unsafePaymentKeyFingerprint <$> knownAddrs)
            (Map.map fst keys)
        addrs = map (withAddressState . fst)
            . L.sortOn (fst . snd)
            . Map.toList
            . Map.filter (\(v, _) -> Just v <= maxV)
            $ keys
    in
        mkAddressPool @n (context pool) newGap addrs
  where
    withAddressState :: KeyFingerprint "payment" key -> (Address, AddressState)
    withAddressState fingerprint =
        (addr, if addr `elem` knownAddrs then Used else Unused)
      where
        addr = mkAddress fingerprint

    maximumValue
        :: (Ord k, Ord v)
        => [k]
        -> Map k v
        -> Maybe v
    maximumValue ks =
        Map.foldl' keepHighest Nothing . (`Map.restrictKeys` (Set.fromList ks))
      where
        keepHighest highest v = if Just v > highest then Just v else highest

-- | Lookup an address in the pool. When we find an address in a pool, the pool
-- may be amended if the address was discovered near the edge. It is also
-- possible that the pool is not amended at all - this happens in the case that
-- an address is discovered 'far' from the edge.
lookupAddress
    :: forall (n :: NetworkDiscriminant) c k.
        ( MkKeyFingerprint k (Proxy n, k 'AddressK XPub)
        , MkKeyFingerprint k Address
        , SoftDerivation k
        , Typeable c
        , Typeable n
        )
    => (AddressState -> AddressState)
    -> Address
    -> AddressPool c k
    -> (Maybe (Index 'Soft 'AddressK), AddressPool c k)
lookupAddress alterSt !target !pool =
    case paymentKeyFingerprint @k target of
        Left _ ->
            (Nothing, pool)
        Right fingerprint ->
            case Map.alterF lookupF fingerprint (indexedKeys pool) of
                (Just ix, keys') ->
                    ( Just ix
                    , extendAddressPool @n ix (pool { indexedKeys = keys'})
                    )
                (Nothing, _) ->
                    ( Nothing
                    , pool
                    )
  where
    lookupF = \case
        Nothing -> (Nothing, Nothing)
        Just (ix, st) -> (Just ix, Just (ix, alterSt st))

-- | If an address is discovered near the edge, we extend the address sequence,
-- otherwise we return the pool untouched.
extendAddressPool
    :: forall (n :: NetworkDiscriminant) c k.
        ( MkKeyFingerprint k (Proxy n, k 'AddressK XPub)
        , MkKeyFingerprint k Address
        , SoftDerivation k
        , Typeable c
        , Typeable n
        )
    => Index 'Soft 'AddressK
    -> AddressPool c k
    -> AddressPool c k
extendAddressPool !ix !pool
    | isOnEdge  = pool { indexedKeys = indexedKeys pool <> next }
    | otherwise = pool
  where
    edge = Map.size (indexedKeys pool)
    isOnEdge = edge - fromEnum ix <= fromEnum (gap pool)
    next = if ix == maxBound then mempty else nextAddresses @n @c
        (context pool)
        (gap pool)
        (succ ix)

-- | Compute the pool extension from a starting index
nextAddresses
    :: forall (n :: NetworkDiscriminant) (c :: Role) k.
        ( MkKeyFingerprint k (Proxy n, k 'AddressK XPub)
        , MkKeyFingerprint k Address
        , SoftDerivation k
        , Typeable c
        , Typeable n
        )
    => ParentContext c k
    -> AddressPoolGap
    -> Index 'Soft 'AddressK
    -> Map (KeyFingerprint "payment" k) (Index 'Soft 'AddressK, AddressState)
nextAddresses !ctx (AddressPoolGap !g) !fromIx =
    [fromIx .. min maxBound toIx]
        & map (\ix -> (mkPaymentKey ix ctx, (ix, Unused)))
        & Map.fromList
  where
    toIx = invariant
        "nextAddresses: toIx should be greater than fromIx"
        (toEnum $ fromEnum fromIx + fromEnum g - 1)
        (>= fromIx)

    mkPaymentKey ix = \case
        ParentContextUtxoExternal acct ->
            mkPaymentKeyFromAccXPub acct
        ParentContextUtxoInternal acct ->
            mkPaymentKeyFromAccXPub acct
        ParentContextMultisigScript _ payment delegation ->
            mkPaymentKeyFromTemplates payment delegation
      where
        mkPaymentKeyFromAccXPub acct =
            unsafePaymentKeyFingerprint @k
                ( Proxy @n
                , deriveAddressPublicKey @k acct (role @c) ix
                )

        mkPaymentKeyFromTemplates payment delegation =
            unsafePaymentKeyFingerprint @k $
                constructAddressFromIx @n payment delegation (coerce ix)

{-------------------------------------------------------------------------------
                            Pending Change Indexes
-------------------------------------------------------------------------------}

-- | An ordered set of pending indexes. This keep track of indexes used
newtype PendingIxs = PendingIxs
    { pendingIxsToList :: [Index 'Soft 'AddressK] }
    deriving stock (Generic, Show, Eq)

instance NFData PendingIxs

-- | An empty pending set of change indexes.
--
-- NOTE: We do not define a 'Monoid' instance here because there's no rational
-- of combining two pending sets.
emptyPendingIxs :: PendingIxs
emptyPendingIxs = PendingIxs mempty

-- | Update the set of pending indexes by discarding every indexes _below_ the
-- given index.
--
-- Why is that?
--
-- Because we really do care about the higher index that was last used in order
-- to know from where we can generate new indexes.
updatePendingIxs
    :: Index 'Soft 'AddressK
    -> PendingIxs
    -> PendingIxs
updatePendingIxs ix (PendingIxs ixs) =
    PendingIxs $ L.filter (> ix) ixs

-- | Construct a 'PendingIxs' from a list, ensuring that it is a set of indexes
-- in descending order.
pendingIxsFromList :: [Index 'Soft 'AddressK] -> PendingIxs
pendingIxsFromList = PendingIxs . reverse . map head . L.group . L.sort

-- | Get the next change index; If every available indexes have already been
-- taken, we'll rotate the pending set and re-use already provided indexes.
--
-- This should not be a problem for users in practice, and remain okay for
-- exchanges who care less about privacy / not-reusing addresses than
-- regular users.
nextChangeIndex
    :: AddressPool c k
    -> PendingIxs
    -> (Index 'Soft 'AddressK, PendingIxs)
nextChangeIndex pool (PendingIxs ixs) =
    let
        poolLen = Map.size (indexedKeys pool)
        (firstUnused, lastUnused) =
            ( toEnum $ poolLen - fromEnum (gap pool)
            , toEnum $ poolLen - 1
            )
        (ix, ixs') = case ixs of
            [] ->
                (firstUnused, PendingIxs [firstUnused])
            h:_ | length ixs < fromEnum (gap pool) ->
                (succ h, PendingIxs (succ h:ixs))
            h:q ->
                (h, PendingIxs (q++[h]))
    in
        invariant "index is within first unused and last unused" (ix, ixs')
            (\(i,_) -> i >= firstUnused && i <= lastUnused)

-- Extract the fingerprint from an 'Address', we expect the caller to
-- provide addresses that are compatible with the key scheme being used.
--
-- Actually, addresses passed as asgument should have been "generated" by
-- the address pool itself in the past, so they ought to be valid!
unsafePaymentKeyFingerprint
    :: forall k from. (HasCallStack, MkKeyFingerprint k from)
    => from
    -> KeyFingerprint "payment" k
unsafePaymentKeyFingerprint from = case paymentKeyFingerprint @k from of
    Right a -> a
    Left err -> error $ unwords
        [ "unsafePaymentKeyFingerprint was given a source invalid with its"
        , "key type:"
        , show err
        ]

{-------------------------------------------------------------------------------
                                 State
-------------------------------------------------------------------------------}

-- | A state to keep track of sequential addresses as described in
-- [BIP-44](https://github.com/bitcoin/bips/blob/master/bip-0044.mediawiki)
--
-- Internally, the state keeps track of a few things for us and is it is
-- parameterized by a type @n@ which captures a particular network discrimination.
-- This enables the state to be agnostic to the underlying address format.
data SeqState (n :: NetworkDiscriminant) k = SeqState
    { internalPool :: !(AddressPool 'UtxoInternal k)
        -- ^ Addresses living on the 'UtxoInternal'
    , externalPool :: !(AddressPool 'UtxoExternal k)
        -- ^ Addresses living on the 'UtxoExternal'
    , pendingChangeIxs :: !PendingIxs
        -- ^ Indexes from the internal pool that have been used in pending
        -- transactions. The list is maintained sorted in descending order
        -- (cf: 'PendingIxs')
    , rewardAccountKey :: k 'AddressK XPub
        -- ^ Reward account public key associated with this wallet
    , derivationPrefix :: DerivationPrefix
        -- ^ Derivation path prefix from a root key up to the internal account
    }
    deriving stock (Generic)

deriving instance
    ( Show (k 'AccountK XPub)
    , Show (k 'AddressK XPub)
    , Show (KeyFingerprint "payment" k)
    ) => Show (SeqState n k)

deriving instance
    ( Eq (k 'AccountK XPub)
    , Eq (k 'AddressK XPub)
    , Eq (KeyFingerprint "payment" k)
    ) => Eq (SeqState n k)

instance
    ( NFData (k 'AccountK XPub)
    , NFData (k 'AddressK XPub)
    , NFData (KeyFingerprint "payment" k)
    )
    => NFData (SeqState n k)

instance (WalletKey k) => Buildable (SeqState n k) where
    build (SeqState intP extP chgs _ path) = "SeqState:\n"
        <> indentF 4 ("Derivation prefix: " <> build (toText path))
        <> indentF 4 (build intP)
        <> indentF 4 (build extP)
        <> indentF 4 ("Change indexes: " <> indentF 4 chgsF)
      where
        chgsF = blockListF' "-" build (pendingIxsToList chgs)

-- | Purpose is a constant set to 44' (or 0x8000002C) following the original
-- BIP-44 specification.
--
-- It indicates that the subtree of this node is used according to this
-- specification.
--
-- Hardened derivation is used at this level.
purposeBIP44 :: Index 'Hardened 'PurposeK
purposeBIP44 = toEnum 0x8000002C

-- | Purpose is a constant set to 1852' (or 0x8000073c) following the BIP-44
-- extension for Cardano:
--
-- https://github.com/input-output-hk/implementation-decisions/blob/e2d1bed5e617f0907bc5e12cf1c3f3302a4a7c42/text/1852-hd-chimeric.md
--
-- It indicates that the subtree of this node is used according to this
-- specification.
--
-- Hardened derivation is used at this level.
purposeCIP1852 :: Index 'Hardened 'PurposeK
purposeCIP1852 = toEnum 0x8000073c


-- | Construct a Sequential state for a wallet from root private key and password.
mkSeqStateFromRootXPrv
    :: forall n k.
        ( SoftDerivation k
        , MkKeyFingerprint k (Proxy n, k 'AddressK XPub)
        , MkKeyFingerprint k Address
        , WalletKey k
        , Bounded (Index (AddressIndexDerivationType k) 'AddressK)
        , Typeable n
        )
    => (k 'RootK XPrv, Passphrase "encryption")
    -> Index 'Hardened 'PurposeK
    -> AddressPoolGap
    -> SeqState n k
mkSeqStateFromRootXPrv (rootXPrv, pwd) purpose g =
    let
        accXPrv =
            deriveAccountPrivateKey pwd rootXPrv minBound
        rewardXPub =
            publicKey $ deriveRewardAccount pwd rootXPrv
        extPool =
            mkAddressPool @n (ParentContextUtxoExternal $ publicKey accXPrv) g []
        intPool =
            mkAddressPool @n (ParentContextUtxoInternal $ publicKey accXPrv) g []
        prefix =
            DerivationPrefix ( purpose, coinTypeAda, minBound )
    in
        SeqState intPool extPool emptyPendingIxs rewardXPub prefix

-- | Construct a Sequential state for a wallet from public account key.
mkSeqStateFromAccountXPub
    :: forall (n :: NetworkDiscriminant) k.
        ( SoftDerivation k
        , MkKeyFingerprint k (Proxy n, k 'AddressK XPub)
        , MkKeyFingerprint k Address
        , Typeable n
        )
    => k 'AccountK XPub
    -> Index 'Hardened 'PurposeK
    -> AddressPoolGap
    -> SeqState n k
mkSeqStateFromAccountXPub accXPub purpose g =
    let
        -- This matches the reward address for "normal wallets". The accountXPub
        -- is the first account, minBound being the first Soft index
        rewardXPub =
            deriveAddressPublicKey accXPub MutableAccount minBound
        extPool =
            mkAddressPool @n (ParentContextUtxoExternal accXPub) g []
        intPool =
            mkAddressPool @n (ParentContextUtxoInternal accXPub) g []
        prefix =
            DerivationPrefix ( purpose, coinTypeAda, minBound )
    in
        SeqState intPool extPool emptyPendingIxs rewardXPub prefix

-- NOTE
-- We have to scan both the internal and external chain. Note that, the
-- account discovery algorithm is only specified for the external chain so
-- in theory, there's nothing forcing a wallet to generate change
-- addresses on the internal chain anywhere in the available range.
instance
    ( SoftDerivation k
    , MkKeyFingerprint k (Proxy n, k 'AddressK XPub)
    , MkKeyFingerprint k Address
    , Typeable n
    ) => IsOurs (SeqState n k) Address
  where
    isOurs addr (SeqState !s1 !s2 !ixs !rpk !prefix) =
        let
            DerivationPrefix (purpose, coinType, accountIx) = prefix
            (internal, !s1') = lookupAddress @n (const Used) addr s1
            (external, !s2') = lookupAddress @n (const Used) addr s2

            !ixs' = case internal of
                Nothing -> ixs
                Just ix -> updatePendingIxs ix ixs

            ours = case (external, internal) of
                (Just addrIx, _) -> Just $ NE.fromList
                    [ DerivationIndex $ getIndex purpose
                    , DerivationIndex $ getIndex coinType
                    , DerivationIndex $ getIndex accountIx
                    , DerivationIndex $ getIndex utxoExternal
                    , DerivationIndex $ getIndex addrIx
                    ]

                (_, Just addrIx) -> Just $ NE.fromList
                    [ DerivationIndex $ getIndex purpose
                    , DerivationIndex $ getIndex coinType
                    , DerivationIndex $ getIndex accountIx
                    , DerivationIndex $ getIndex utxoInternal
                    , DerivationIndex $ getIndex addrIx
                    ]

                _ -> Nothing
        in
            (ixs' `deepseq` ours `deepseq` ours, SeqState s1' s2' ixs' rpk prefix)

instance
    ( SoftDerivation k
    ) => GenChange (SeqState n k) where
    -- | We pick indexes in sequence from the first known available index (i.e.
    -- @length addrs - gap@) but we do not generate _new change addresses_. As a
    -- result, we can't generate more than @gap@ _pending_ change addresses and
    -- therefore, rotate the change addresses when we need extra change outputs.
    --
    -- See also: 'nextChangeIndex'
    type ArgGenChange (SeqState n k) =
        (k 'AddressK XPub -> k 'AddressK XPub -> Address)

    genChange mkAddress (SeqState intPool extPool pending rpk path) =
        let
            (ix, pending') = nextChangeIndex intPool pending
            ParentContextUtxoInternal accountXPub' = context intPool
            addressXPub = deriveAddressPublicKey accountXPub' UtxoInternal ix
            addr = mkAddress addressXPub rpk
        in
            (addr, SeqState intPool extPool pending' rpk path)

instance
    ( IsOurs (SeqState n k) Address
    , SoftDerivation k
    , MkKeyFingerprint k (Proxy n, k 'AddressK XPub)
    , MkKeyFingerprint k Address
    , AddressIndexDerivationType k ~ 'Soft
    , Typeable n
    )
    => IsOwned (SeqState n k) k where
    isOwned (SeqState !s1 !s2 _ _ _) (rootPrv, pwd) addr =
        let
            xPrv1 = lookupAndDeriveXPrv s1
            xPrv2 = lookupAndDeriveXPrv s2
            xPrv = xPrv1 <|> xPrv2
        in
            (,pwd) <$> xPrv
      where
        lookupAndDeriveXPrv
            :: forall c. (Typeable c)
            => AddressPool c k
            -> Maybe (k 'AddressK XPrv)
        lookupAndDeriveXPrv pool =
            let
                -- We are assuming there is only one account
                accountPrv = deriveAccountPrivateKey pwd rootPrv minBound
                (addrIx, _) = lookupAddress @n (const Used) addr pool
                cc = role @c
            in
                deriveAddressPrivateKey pwd accountPrv cc <$> addrIx

instance
    ( MkKeyFingerprint k (Proxy n, k 'AddressK XPub)
    , MkKeyFingerprint k Address
    , SoftDerivation k
    , Typeable n
    ) => CompareDiscovery (SeqState n k) where
    compareDiscovery (SeqState !s1 !s2 _ _ _) a1 a2 =
        case (ix a1 s1 <|> ix a1 s2, ix a2 s1 <|> ix a2 s2) of
            (Nothing, Nothing) -> EQ
            (Nothing, Just _)  -> GT
            (Just _, Nothing)  -> LT
            (Just i1, Just i2) -> compare i1 i2
      where
        ix :: Typeable c => Address -> AddressPool c k -> Maybe (Index 'Soft 'AddressK)
        ix a = fst . lookupAddress @n id a

instance
    ( PaymentAddress n k
    ) => KnownAddresses (SeqState n k) where
    knownAddresses s =
        nonChangeAddresses <> usedChangeAddresses <> pendingChangeAddresses
      where
            nonChangeAddresses =
                addresses (liftPaymentAddress @n @k) (externalPool s)

            changeAddresses =
                addresses (liftPaymentAddress @n @k) (internalPool s)

            usedChangeAddresses =
                filter (\(_, state, _, _) -> state == Used ) changeAddresses

            -- pick as many unused change addresses as there are pending
            -- transactions. Note: the last `internalGap` addresses are all
            -- unused.
            pendingChangeAddresses =
                let
                    (PendingIxs ixs) =
                        pendingChangeIxs s

                    internalGap =
                        fromEnum . getAddressPoolGap . gap . internalPool $ s

                    edgeChangeAddresses =
                        drop (length changeAddresses - internalGap) changeAddresses
                in
                    take (length ixs) edgeChangeAddresses

--------------------------------------------------------------------------------
--
-- SeqAnyState
--
-- For benchmarking and testing arbitrary large sequential wallets.

-- | An "unsound" alternative that can be used for benchmarking and stress
-- testing. It re-uses the same underlying structure as the `SeqState` but
-- it discovers addresses based on an arbitrary ratio instead of respecting
-- BIP-44 discovery.
--
-- The proportion is stored as a type-level parameter so that we don't have to
-- alter the database schema to store it. It simply exists and depends on the
-- caller creating the wallet to define it.
newtype SeqAnyState (network :: NetworkDiscriminant) key (p :: Nat) = SeqAnyState
    { innerState :: SeqState network key
    } deriving (Generic)

deriving instance
    ( Show (k 'AccountK XPub)
    , Show (k 'AddressK XPub)
    , Show (KeyFingerprint "payment" k)
    ) => Show (SeqAnyState n k p)

instance
    ( NFData (k 'AccountK XPub)
    , NFData (k 'AddressK XPub)
    , NFData (KeyFingerprint "payment" k)
    )
    => NFData (SeqAnyState n k p)

-- | Initialize the HD random address discovery state from a root key and RNG
-- seed.
--
-- The type parameter is expected to be a ratio of addresses we ought to simply
-- recognize as ours. It is expressed in tenths of percent, so "1" means 0.1%,
-- "10" means 1% and 1000 means 100%.
mkSeqAnyState
    :: forall (p :: Nat) n k.
        ( SoftDerivation k
        , MkKeyFingerprint k (Proxy n, k 'AddressK XPub)
        , MkKeyFingerprint k Address
        , WalletKey k
        , Bounded (Index (AddressIndexDerivationType k) 'AddressK)
        , Typeable n
        )
    => (k 'RootK XPrv, Passphrase "encryption")
    -> Index 'Hardened 'PurposeK
    -> AddressPoolGap
    -> SeqAnyState n k p
mkSeqAnyState credentials purpose poolGap = SeqAnyState
    { innerState = mkSeqStateFromRootXPrv credentials purpose poolGap
    }

instance
    ( SoftDerivation k
    , MkKeyFingerprint k (Proxy n, k 'AddressK XPub)
    , MkKeyFingerprint k Address
    , Typeable n
    , KnownNat p
    ) => IsOurs (SeqAnyState n k p) Address
  where
    isOurs (Address bytes) st@(SeqAnyState inner)
        | crc32 bytes < p =
            let
                edge = Map.size (indexedKeys $ externalPool inner)
                ix = toEnum (edge - fromEnum (gap $ externalPool inner))
                pool' = extendAddressPool @n ix (externalPool inner)
                path = DerivationIndex (getIndex ix) :| []
            in
                (Just path, SeqAnyState (inner { externalPool = pool' }))
        | otherwise =
            (Nothing, st)
      where
        p = floor (double sup * double (natVal (Proxy @p)) / 1000)
          where
            sup = maxBound :: Word32

        double :: Integral a => a -> Double
        double = fromIntegral

instance IsOurs (SeqAnyState n k p) RewardAccount
  where
    isOurs _account state = (Nothing, state)

instance
    ( SoftDerivation k
    , MkKeyFingerprint k (Proxy n, k 'AddressK XPub)
    , MkKeyFingerprint k Address
    , AddressIndexDerivationType k ~ 'Soft
    , KnownNat p
    , Typeable n
    ) => IsOwned (SeqAnyState n k p) k
  where
    isOwned _ _ _ = Nothing

instance
    ( SoftDerivation k
    ) => GenChange (SeqAnyState n k p)
  where
    type ArgGenChange (SeqAnyState n k p) = ArgGenChange (SeqState n k)
    genChange a (SeqAnyState s) = SeqAnyState <$> genChange a s

instance
    ( MkKeyFingerprint k (Proxy n, k 'AddressK XPub)
    , MkKeyFingerprint k Address
    , SoftDerivation k
    , Typeable n
    ) => CompareDiscovery (SeqAnyState n k p)
  where
    compareDiscovery (SeqAnyState s) = compareDiscovery s

instance
    ( PaymentAddress n k
    ) => KnownAddresses (SeqAnyState n k p)
  where
    knownAddresses (SeqAnyState s) = knownAddresses s
