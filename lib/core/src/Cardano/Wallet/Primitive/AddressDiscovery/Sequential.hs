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
{-# LANGUAGE NamedFieldPuns #-}
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
    -- * Sequential Derivation
    -- ** Address Pool Gap
      AddressPoolGap
    , MkAddressPoolGapError (..)
    , defaultAddressPoolGap
    , getAddressPoolGap
    , mkAddressPoolGap
    , mkUnboundedAddressPoolGap

    -- ** Address Pool
    , SeqAddressPool (..)
    , role
    , getGap
    , newSeqAddressPool
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
    )
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
    , coinTypeAda
    )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..), AddressState (..) )
import Cardano.Wallet.Primitive.Types.RewardAccount
    ( RewardAccount )
import Cardano.Wallet.Util
    ( invariant )
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
import Data.Digest.CRC32
    ( crc32 )
import Data.Kind
    ( Type )
import Data.List.NonEmpty
    ( NonEmpty (..) )
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
    ( (:~:) (..), type (==), testEquality )
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

import qualified Cardano.Wallet.Address.Pool as AddressPool
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

-- | Convenient constraint alias for commonly used class contexts on keys.
type SupportsDiscovery n k =
    ( MkKeyFingerprint k (Proxy n, k 'AddressK XPub)
    , MkKeyFingerprint k Address
    , SoftDerivation k
    , Typeable n
    )

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
    Sequential address pools
-------------------------------------------------------------------------------}
-- | An address pool which keeps track of sequential addresses.
-- To create a new pool, see 'newSeqAddressPool'.
newtype SeqAddressPool (c :: Role) (key :: Depth -> Type -> Type) =
    SeqAddressPool {
        getPool ::
            AddressPool.Pool
                (KeyFingerprint "payment" key)
                (Index 'Soft 'AddressK)
    } deriving (Generic, Show)

instance NFData (SeqAddressPool c k)

instance Buildable (SeqAddressPool c k) where
    build (SeqAddressPool pool) = build pool

-- | Create a new Address pool from a list of addresses. Note that, the list is
-- expected to be ordered in sequence (first indexes, first in the list).
newSeqAddressPool
    :: forall (n :: NetworkDiscriminant) c key.
        ( SupportsDiscovery n key
        , Typeable c
        )
    => key 'AccountK XPub
    -> AddressPoolGap
    -> SeqAddressPool c key
newSeqAddressPool account g = SeqAddressPool $ AddressPool.new generator gap
  where
    gap = fromIntegral $ getAddressPoolGap g
    generator ix =
        unsafePaymentKeyFingerprint @key
            ( Proxy @n
            , deriveAddressPublicKey @key account (role @c) ix
            )

getGap :: SeqAddressPool c k -> AddressPoolGap
getGap = AddressPoolGap . fromIntegral . AddressPool.gap . getPool

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
    Pretty printing
-------------------------------------------------------------------------------}

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
       (tryUtxoExternal <|> tryUtxoInternal <|> tryMutableAccount)
  where
    tryUtxoExternal =
        case testEquality (typeRep @c) (typeRep @'UtxoExternal) of
            Just Refl  -> Just UtxoExternal
            Nothing -> Nothing
    tryUtxoInternal =
        case testEquality (typeRep @c) (typeRep @'UtxoInternal) of
            Just Refl  -> Just UtxoInternal
            Nothing -> Nothing
    tryMutableAccount =
        case testEquality (typeRep @c) (typeRep @'MutableAccount) of
            Just Refl  -> Just MutableAccount
            Nothing -> Nothing

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
    :: SeqAddressPool c k
    -> PendingIxs
    -> (Index 'Soft 'AddressK, PendingIxs)
nextChangeIndex (SeqAddressPool pool) (PendingIxs ixs) =
    let
        poolLen = AddressPool.size  pool
        (firstUnused, lastUnused) =
            ( toEnum $ poolLen - AddressPool.gap pool
            , toEnum $ poolLen - 1
            )
        (ix, ixs') = case ixs of
            [] ->
                (firstUnused, PendingIxs [firstUnused])
            h:_ | length ixs < AddressPool.gap pool ->
                (succ h, PendingIxs (succ h:ixs))
            h:q ->
                (h, PendingIxs (q++[h]))
    in
        invariant "index is within first unused and last unused" (ix, ixs')
            (\(i,_) -> i >= firstUnused && i <= lastUnused)

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
    , pendingChangeIxs :: !PendingIxs
        -- ^ Indexes from the internal pool that have been used in pending
        -- transactions. The list is maintained sorted in descending order
        -- (cf: 'PendingIxs')
    , accountXPub :: k 'AccountK XPub
        -- ^ The account public key associated with this state
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

instance
    ( NFData (k 'AccountK XPub)
    , NFData (k 'AddressK XPub)
    , NFData (KeyFingerprint "payment" k)
    )
    => NFData (SeqState n k)

-- Hand-written, because 'AddressPool.Pool' is not an instance of 'Eq'.
instance
    ( Eq (k 'AccountK XPub)
    , Eq (k 'AddressK XPub)
    , Eq (KeyFingerprint "payment" k)
    ) => Eq (SeqState n k)
  where
    SeqState ai ae a1 a2 a3 a4 == SeqState bi be b1 b2 b3 b4
        = and
            [a1 == b1, a2 == b2, a3 == b3, a4 == b4
            , ae `match` be, ai `match` bi
            ]
      where
        match (SeqAddressPool a) (SeqAddressPool b)
            = AddressPool.addresses a == AddressPool.addresses b
            && AddressPool.gap a == AddressPool.gap b

instance Buildable (SeqState n k) where
    build st = "SeqState:\n"
        <> indentF 4 ("Derivation prefix: " <> build (toText (derivationPrefix st)))
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
        ( WalletKey k
        , SupportsDiscovery n k
        , (k == SharedKey) ~ 'False
        )
    => (k 'RootK XPrv, Passphrase "encryption")
    -> Index 'Hardened 'PurposeK
    -> AddressPoolGap
    -> SeqState n k
mkSeqStateFromRootXPrv (rootXPrv, pwd) =
    mkSeqStateFromAccountXPub $
        publicKey $ deriveAccountPrivateKey pwd rootXPrv minBound

-- | Construct a Sequential state for a wallet from public account key.
mkSeqStateFromAccountXPub
    :: forall (n :: NetworkDiscriminant) k.
        ( SupportsDiscovery n k
        , (k == SharedKey) ~ 'False
        )
    => k 'AccountK XPub
    -> Index 'Hardened 'PurposeK
    -> AddressPoolGap
    -> SeqState n k
mkSeqStateFromAccountXPub accXPub purpose g = SeqState
    { internalPool = newSeqAddressPool @n accXPub g
    , externalPool = newSeqAddressPool @n accXPub g
    , accountXPub = accXPub
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
    :: SeqState n k -> Role -> Index 'Soft 'AddressK
    -> NE.NonEmpty DerivationIndex
decoratePath SeqState{derivationPrefix} r ix = NE.fromList
    [ DerivationIndex $ getIndex purpose
    , DerivationIndex $ getIndex coinType
    , DerivationIndex $ getIndex accountIx
    , DerivationIndex $ fromIntegral $ fromEnum r
    , DerivationIndex $ getIndex ix
    ]
  where
    DerivationPrefix (purpose, coinType, accountIx) = derivationPrefix

-- NOTE
-- We have to scan both the internal and external chain. Note that, the
-- BIP-44 account discovery algorithm is only specified for the external
-- chain so in theory, there's nothing forcing a wallet to generate change
-- addresses on the internal chain anywhere in the available range.
instance SupportsDiscovery n k => IsOurs (SeqState n k) Address where
    isOurs addrRaw st@SeqState{pendingChangeIxs=ixs} =
        -- FIXME LATER: Check that the network discrimant of the type
        -- is compatible with the discriminant of the Address!
        case paymentKeyFingerprint addrRaw of
            Left _ -> (Nothing, st)
            Right addr ->
                let (internal, !int) = lookupAddress addr (internalPool st)
                    (external, !ext) = lookupAddress addr (externalPool st)

                    !ixs' = case internal of
                        Nothing -> ixs
                        Just ix -> updatePendingIxs ix ixs

                    ours = case (external, internal) of
                        (Just ix, _) -> Just $ decoratePath st UtxoExternal ix
                        (_, Just ix) -> Just $ decoratePath st UtxoInternal ix
                        _ -> Nothing
                in
                    ( ixs' `deepseq` ours `deepseq` ours
                    , st
                        { internalPool = int
                        , externalPool = ext
                        , pendingChangeIxs = ixs'
                        }
                    )
      where
        lookupAddress addr (SeqAddressPool pool) =
            case AddressPool.lookup addr pool of
                Nothing -> (Nothing, SeqAddressPool pool)
                Just ix -> (Just ix, SeqAddressPool $ AddressPool.update addr pool)

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
    
    genChange mkAddress st =
        (addr, st{ pendingChangeIxs = pending' })
      where
        (ix, pending') = nextChangeIndex (internalPool st) (pendingChangeIxs st)
        addressXPub = deriveAddressPublicKey (accountXPub st) UtxoInternal ix
        addr = mkAddress addressXPub (rewardAccountKey st)

instance
    ( IsOurs (SeqState n k) Address
    , SupportsDiscovery n k
    , AddressIndexDerivationType k ~ 'Soft
    )
    => IsOwned (SeqState n k) k where
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
            -> Maybe (k 'AddressK XPrv)
        lookupAndDeriveXPrv addr (SeqAddressPool pool) =
                deriveAddressPrivateKey pwd accountPrv (role @c)
            <$> AddressPool.lookup addr pool 

instance SupportsDiscovery n k => CompareDiscovery (SeqState n k) where
    compareDiscovery (SeqState !s1 !s2 _ _ _ _) a1 a2 =
        case (ix a1 s1 <|> ix a1 s2, ix a2 s1 <|> ix a2 s2) of
            (Nothing, Nothing) -> EQ
            (Nothing, Just _)  -> GT
            (Just _, Nothing)  -> LT
            (Just i1, Just i2) -> compare i1 i2
      where
        ix :: Address -> SeqAddressPool c k -> Maybe (Index 'Soft 'AddressK)
        ix a (SeqAddressPool pool) = case paymentKeyFingerprint a of
            Left _ -> Nothing
            Right addr -> AddressPool.lookup addr pool

instance
    ( PaymentAddress n k
    ) => KnownAddresses (SeqState n k) where
    knownAddresses st =
        nonChangeAddresses <> usedChangeAddresses <> pendingChangeAddresses
      where
        -- | List addresses in order of increasing indices.
        listAddresses
            :: forall c. (Typeable c)
            => SeqAddressPool c k
            -> [(Address, AddressState, NonEmpty DerivationIndex)]
        listAddresses (SeqAddressPool pool) =
            map shuffle . L.sortOn idx . Map.toList
            $ AddressPool.addresses pool
          where
            idx (_,(ix,_)) = ix
            shuffle (k,(ix,s)) =
                (liftPaymentAddress @n k, s, decoratePath st (role @c) ix)

        nonChangeAddresses = listAddresses $ externalPool st

        changeAddresses = listAddresses $ internalPool st
        usedChangeAddresses =
            filter (\(_, status, _) -> status == Used) changeAddresses

        -- pick as many unused change addresses as there are pending
        -- transactions. Note: the last `internalGap` addresses are all
        -- unused.
        pendingChangeAddresses = take (length ixs) edgeChangeAddresses
          where
            PendingIxs ixs = pendingChangeIxs st
            internalGap = AddressPool.gap $ getPool $ internalPool st
            edgeChangeAddresses =
                drop (length changeAddresses - internalGap) changeAddresses

instance GetAccount (SeqState n k) k where
    getAccount = accountXPub

instance MaybeLight (SeqState n k) where
    maybeDiscover = Nothing

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
mkSeqAnyState credentials purpose poolGap = SeqAnyState
    { innerState = mkSeqStateFromRootXPrv credentials purpose poolGap
    }

instance KnownNat p => IsOurs (SeqAnyState n k p) Address where
    isOurs (Address bytes) st@(SeqAnyState inner)
        | crc32 bytes < p =
            let
                pool = getPool $ externalPool inner
                ix = toEnum $ AddressPool.size pool - AddressPool.gap pool
                addr = AddressPool.generator pool ix
                pool' = AddressPool.update addr pool
                path = DerivationIndex (getIndex ix) :| []
            in
                ( Just path
                , SeqAnyState $ inner{ externalPool = SeqAddressPool pool' }
                )
        | otherwise =
            (Nothing, st)
      where
        p = floor (double sup * double (natVal (Proxy @p)) / 10000)
          where
            sup = maxBound :: Word32

        double :: Integral a => a -> Double
        double = fromIntegral

instance IsOurs (SeqAnyState n k p) RewardAccount where
    isOurs _account state = (Nothing, state)

instance
    ( AddressIndexDerivationType k ~ 'Soft
    , KnownNat p
    ) => IsOwned (SeqAnyState n k p) k
  where
    isOwned _ _ _ = Nothing

instance SoftDerivation k => GenChange (SeqAnyState n k p) where
    type ArgGenChange (SeqAnyState n k p) = ArgGenChange (SeqState n k)
    genChange a (SeqAnyState s) = SeqAnyState <$> genChange a s

instance SupportsDiscovery n k => CompareDiscovery (SeqAnyState n k p) where
    compareDiscovery (SeqAnyState s) = compareDiscovery s

instance PaymentAddress n k => KnownAddresses (SeqAnyState n k p) where
    knownAddresses (SeqAnyState s) = knownAddresses s

instance MaybeLight (SeqAnyState n k p) where
    maybeDiscover = Nothing
