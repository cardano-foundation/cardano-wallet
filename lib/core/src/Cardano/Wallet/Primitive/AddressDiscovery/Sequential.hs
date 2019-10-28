{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright: © 2018-2019 IOHK
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

    -- ** Address Pool
    , AddressPool
    , gap
    , addresses
    , changeChain
    , accountPubKey
    , mkAddressPool
    , lookupAddress

    -- * Pending Change Indexes
    , PendingIxs
    , emptyPendingIxs
    , pendingIxsToList
    , pendingIxsFromList

    -- ** State
    , SeqState (..)
    , mkSeqState
    ) where

import Prelude

import Cardano.Crypto.Wallet
    ( XPrv, XPub )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..)
    , DerivationType (..)
    , Index
    , PaymentAddress (..)
    , NetworkDiscriminant
    , Passphrase (..)
    , WalletKey (..)
    )
import Cardano.Wallet.Primitive.AddressDerivation.Sequential
    ( ChangeChain (..)
    , SeqKey (..)
    , deriveAccountPrivateKey
    , deriveAddressPrivateKey
    , deriveAddressPublicKey
    , serializeXPubSeq
    )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( CompareDiscovery (..)
    , GenChange (..)
    , IsOurs (..)
    , IsOwned (..)
    , KnownAddresses (..)
    )
import Cardano.Wallet.Primitive.Types
    ( Address, invariant )
import Control.Applicative
    ( (<|>) )
import Control.DeepSeq
    ( NFData, deepseq )
import Control.Monad
    ( unless )
import Data.Bifunctor
    ( first )
import Data.Function
    ( (&) )
import Data.Map.Strict
    ( Map )
import Data.Maybe
    ( isJust )
import Data.Proxy
    ( Proxy (..) )
import Data.Text.Class
    ( FromText (..), TextDecodingError (..), ToText (..) )
import Data.Text.Read
    ( decimal )
import Data.Typeable
    ( Typeable, typeRep )
import Data.Word
    ( Word8 )
import Fmt
    ( Buildable (..)
    , blockListF'
    , blockMapF'
    , hexF
    , indentF
    , prefixF
    , suffixF
    )
import GHC.Generics
    ( Generic )

import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

{-------------------------------------------------------------------------------
                              Address Pool Gap
-------------------------------------------------------------------------------}

-- | Maximum number of consecutive undiscovered addresses allowed
newtype AddressPoolGap = AddressPoolGap
    { getAddressPoolGap :: Word8 }
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
    maxBound = AddressPoolGap 100

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

-- | An 'AddressPool' which keeps track of sequential addresses within a given
-- Account and change chain. See 'mkAddressPool' to create a new or existing
-- pool:
--
-- >>> mkAddressPool xpub gap changeChain mempty
-- AddressPool { }
data AddressPool (network :: NetworkDiscriminant) (chain :: ChangeChain) = AddressPool
    { accountPubKey
        :: !(SeqKey 'AccountK XPub)
        -- ^ Corresponding key for the pool (a pool is tied to only one account)
    , gap
        :: !AddressPoolGap
        -- ^ The actual gap for the pool. This can't change for a given pool.
    , indexedAddresses
        :: !(Map Address (Index 'Soft 'AddressK))
    } deriving (Generic, Show, Eq)

instance NFData (AddressPool network chain)

instance Typeable chain => Buildable (AddressPool network chain) where
    build (AddressPool acct (AddressPoolGap g) addrs) = mempty
        <> ccF <> " " <> acctF <> " (gap=" <> build g <> ")\n"
        <> indentF 4 (blockMapF' build build addrs)
      where
        ccF = build $ toText $ changeChain @chain
        xpubF = hexF $ serializeXPubSeq acct
        acctF = prefixF 8 xpubF <> "..." <> suffixF 8 xpubF

-- | Bring a 'ChangeChain' type back to the term-level. This requires a type
-- application and either a scoped type variable, or an explicit passing of a
-- 'ChangeChain'.
--
-- >>> changeChain @'ExternalChain
-- ExternalChain
--
-- >>> changeChain @chain
-- ...
changeChain :: forall (c :: ChangeChain). Typeable c => ChangeChain
changeChain =
    case typeRep (Proxy :: Proxy c) of
        t | t == typeRep (Proxy :: Proxy 'InternalChain) ->
            InternalChain
        _ ->
            ExternalChain

-- | Get all addresses in the pool, sorted from the first address discovered,
-- up until the next one.
--
-- In practice, we always have:
--
-- > mkAddressPool key g cc (addresses pool) == pool
addresses :: AddressPool n chain -> [Address]
addresses = map fst . L.sortOn snd . Map.toList . indexedAddresses

-- | Create a new Address pool from a list of addresses. Note that, the list is
-- expected to be ordered in sequence (first indexes, first in the list).
--
-- The pool will grow from the start if less than @g :: AddressPoolGap@ are
-- given, such that, there are always @g@ undiscovered addresses in the pool.
mkAddressPool
    :: forall n c. (PaymentAddress n SeqKey, Typeable c)
    => SeqKey 'AccountK XPub
    -> AddressPoolGap
    -> [Address]
    -> AddressPool n c
mkAddressPool key g addrs = AddressPool
    { accountPubKey = key
    , gap = g
    , indexedAddresses =
        nextAddresses
            (Proxy @n)
            key
            g
            (changeChain @c)
            minBound
          <>
            Map.fromList (zip addrs [minBound..maxBound])
    }

-- | Lookup an address in the pool. When we find an address in a pool, the pool
-- may be amended if the address was discovered near the edge. It is also
-- possible that the pool is not amended at all - this happens in the case that
-- an address is discovered 'far' from the edge.
lookupAddress
    :: forall n c. (PaymentAddress n SeqKey, Typeable c)
    => Address
    -> AddressPool n c
    -> (Maybe (Index 'Soft 'AddressK), AddressPool n c)
lookupAddress !target !pool =
    case Map.lookup target (indexedAddresses pool) of
        Just ix ->
            (Just ix, extendAddressPool ix pool)
        Nothing ->
            (Nothing, pool)

-- | If an address is discovered near the edge, we extend the address sequence,
-- otherwise we return the pool untouched.
extendAddressPool
    :: forall n c. (PaymentAddress n SeqKey, Typeable c)
    => Index 'Soft 'AddressK
    -> AddressPool n c
    -> AddressPool n c
extendAddressPool !ix !pool
    | isOnEdge  = pool { indexedAddresses = indexedAddresses pool <> next }
    | otherwise = pool
  where
    edge = Map.size (indexedAddresses pool)
    isOnEdge = edge - fromEnum ix <= fromEnum (gap pool)
    next = if ix == maxBound then mempty else nextAddresses
        (Proxy @n)
        (accountPubKey pool)
        (gap pool)
        (changeChain @c)
        (succ ix)

-- | Compute the pool extension from a starting index
nextAddresses
    :: forall n. PaymentAddress n SeqKey
    => Proxy n
    -> SeqKey 'AccountK XPub
    -> AddressPoolGap
    -> ChangeChain
    -> Index 'Soft 'AddressK
    -> Map Address (Index 'Soft 'AddressK)
nextAddresses _ !key (AddressPoolGap !g) !cc !fromIx =
    [fromIx .. min maxBound toIx]
        & map (\ix -> (newAddress ix, ix))
        & Map.fromList
  where
    toIx = invariant
        "nextAddresses: toIx should be greater than fromIx"
        (toEnum $ fromEnum fromIx + fromEnum g - 1)
        (>= fromIx)
    newAddress = paymentAddress @n . deriveAddressPublicKey key cc

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
    :: AddressPool n 'InternalChain
    -> PendingIxs
    -> (Index 'Soft 'AddressK, PendingIxs)
nextChangeIndex pool (PendingIxs ixs) =
    let
        poolLen = length (addresses pool)
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

{-------------------------------------------------------------------------------
                                 State
-------------------------------------------------------------------------------}

-- | A state to keep track of sequential addresses as described in
-- [BIP-44](https://github.com/bitcoin/bips/blob/master/bip-0044.mediawiki)
--
-- Internally, the state keeps track of a few things for us and is it is
-- parameterized by a type @t@ which represents a target backend. This enables
-- the state to be agnostic to the underlying address format.
data SeqState n = SeqState
    { internalPool :: !(AddressPool n 'InternalChain)
        -- ^ Addresses living on the 'InternalChain'
    , externalPool :: !(AddressPool n 'ExternalChain)
        -- ^ Addresses living on the 'ExternalChain'
    , pendingChangeIxs :: !PendingIxs
        -- ^ Indexes from the internal pool that have been used in pending
        -- transactions. The list is maintained sorted in descending order
        -- (cf: 'PendingIxs')
    }
    deriving stock (Generic, Show)

instance NFData (SeqState n)

instance Buildable (SeqState n) where
    build (SeqState intP extP chgs) = "SeqState:\n"
        <> indentF 4 (build intP)
        <> indentF 4 (build extP)
        <> indentF 4 ("Change indexes: " <> indentF 4 chgsF)
      where
        chgsF = blockListF' "-" build (pendingIxsToList chgs)

-- | Construct a Sequential state for a wallet.
mkSeqState
    :: PaymentAddress n SeqKey
    => (SeqKey 'RootK XPrv, Passphrase "encryption")
    -> AddressPoolGap
    -> SeqState n
mkSeqState (rootXPrv, pwd) g =
    let
        accXPrv =
            deriveAccountPrivateKey pwd rootXPrv minBound
        extPool =
            mkAddressPool (publicKey accXPrv) g []
        intPool =
            mkAddressPool (publicKey accXPrv) g []
    in
        SeqState intPool extPool emptyPendingIxs

-- NOTE
-- We have to scan both the internal and external chain. Note that, the
-- account discovery algorithm is only specified for the external chain so
-- in theory, there's nothing forcing a wallet to generate change
-- addresses on the internal chain anywhere in the available range.
instance PaymentAddress n SeqKey => IsOurs (SeqState n) where
    isOurs addr (SeqState !s1 !s2 !ixs) =
        let
            (internal, !s1') = lookupAddress addr s1
            (external, !s2') = lookupAddress addr s2
            !ixs' = case internal of
                Nothing -> ixs
                Just ix -> updatePendingIxs ix ixs
            ours = isJust (internal <|> external)
        in
            (ixs' `deepseq` ours `deepseq` ours, SeqState s1' s2' ixs')

instance PaymentAddress n SeqKey => GenChange (SeqState n) where
    -- | We pick indexes in sequence from the first known available index (i.e.
    -- @length addrs - gap@) but we do not generate _new change addresses_. As a
    -- result, we can't generate more than @gap@ _pending_ change addresses and
    -- therefore, rotate the change addresses when we need extra change outputs.
    --
    -- See also: 'nextChangeIndex'
    type ArgGenChange (SeqState n) = ()
    genChange () (SeqState intPool extPool pending) =
        let
            (ix, pending') = nextChangeIndex intPool pending
            accountXPub = accountPubKey intPool
            addressXPub = deriveAddressPublicKey accountXPub InternalChain ix
            addr = paymentAddress @n addressXPub
        in
            (addr, SeqState intPool extPool pending')

instance PaymentAddress n SeqKey => IsOwned (SeqState n) SeqKey where
    isOwned (SeqState !s1 !s2 _) (rootPrv, pwd) addr =
        let
            xPrv1 = lookupAndDeriveXPrv s1
            xPrv2 = lookupAndDeriveXPrv s2
            xPrv = xPrv1 <|> xPrv2
        in
            (,pwd) <$> xPrv
      where
        lookupAndDeriveXPrv
            :: forall c. (Typeable c)
            => AddressPool n c
            -> Maybe (SeqKey 'AddressK XPrv)
        lookupAndDeriveXPrv pool =
            let
                -- We are assuming there is only one account
                accountPrv = deriveAccountPrivateKey pwd rootPrv minBound
                (addrIx, _) = lookupAddress addr pool
                cc = changeChain @c
            in
                deriveAddressPrivateKey pwd accountPrv cc <$> addrIx

instance PaymentAddress n SeqKey => CompareDiscovery (SeqState n) where
    compareDiscovery (SeqState !s1 !s2 _) a1 a2 =
        let
            ix a = fst . lookupAddress a
        in
            case (ix a1 s1 <|> ix a1 s2, ix a2 s1 <|> ix a2 s2) of
                (Nothing, Nothing) -> EQ
                (Nothing, Just _)  -> GT
                (Just _, Nothing)  -> LT
                (Just i1, Just i2) -> compare i1 i2

instance KnownAddresses (SeqState n) where
    knownAddresses s =
        let
            (PendingIxs ixs) =
                pendingChangeIxs s
            internalGap =
                fromEnum . getAddressPoolGap . gap . internalPool $ s
            discardUndiscoveredChange xs =
                take (length ixs) $ drop (length xs - internalGap) xs
        in
            addresses (externalPool s)
            <> discardUndiscoveredChange (addresses $ internalPool s)
