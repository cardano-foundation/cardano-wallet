{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Wallet.Primitive.AddressDiscovery.Delegation
    ( -- * Creation
      DelegationState (..)
    , mkEmptyDelegationState
    , mkDelegationState

      -- * Operations
    , assignStakeKeys
    , allStakeKeys
    , takeStakeKey
    , isOurStakeKey
    , allRewardAccountsWithPaths
    )
    where

import Prelude

import Cardano.Crypto.Wallet
    ( XPub )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..)
    , DerivationIndex (..)
    , DerivationPrefix
    , DerivationType (..)
    , Index (..)
    , Role (..)
    , SoftDerivation (..)
    , ToRewardAccount (..)
    , stakeDerivationPath
    )
import Cardano.Wallet.Primitive.Types.RewardAccount
    ( RewardAccount )
import Control.DeepSeq
    ( NFData )
import Data.List.NonEmpty
    ( NonEmpty )
import Data.Word
    ( Word8 )
import GHC.Generics
    ( Generic )

import Data.Map.Strict
    ( Map )

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map

--------------------------------------------------------------------------------
-- Delegation State
--------------------------------------------------------------------------------

-- | @DelegationState@ keeps track of stake keys on-chain. It allows you to
-- delegate to multiple pools, by allowing multiple stake keys to be registered.
--
-- == Deriving set of keys from chain data
--
-- Normal wallets start with a single stake key. It may or may not be registered
-- in the ledger.
--
-- Using pseudo-syntax:
--
-- >>> s0 = { n=1, indexedKeys=[0] }
--
-- -- FIXME: What happens if the keys for some reason are registered out of
-- order?
--
-- If we see a stake key registration for the edge, we add it. This can be done
-- repeatedly in a single tx, e.g. if the user wants to delegate to 10 different
-- pools.
--
-- >>> stake key registration certificates for 1,2,3,4,5,6,7,8,9,10
--
-- >>> s1 = { n=10, indexedKeys=[0,1,2,3,4,5,6,7,8,9,10] }
--
-- The state can only be shrunk from de-registering the last indexed key. To go
-- to 5 keys, we would now de-register key 10,9,8,6 and 5 in order, leaving us
-- with:
--
-- >>> s1 = { n=5, indexedKeys=[0,1,2,3,4] }
--
-- == Assigning stake keys to addresses
--
-- There are two ways of generating addresses: users can either have funds sent
-- to addresses listed by the wallet, or change addresses can be generated when
-- the users themselves make transactions.
--
-- ==== Recieving funds in listed addresses
--
-- See @assignStakeKeys@.
--
-- ==== Generating change addresses
--
-- See @takeStakeKey@.
-- -- FIXME: How does this ensure balancing?
-- -- FIXME: How does this interact with the address listing?
-- -- FIXME: Describe how the cursor should work.
--
-- There are two ways of generating address
--
-- NOTE: Assuming delegation to n stake pools with equal weights.
data DelegationState k = DelegationState
    { -- | Availible number of stake keys
      numberOfStakeKeys :: Word8

      -- | Points to the stake key to be assigned to the next address.
      --
      -- Invariant: cursor \in [0 .. numberOfStakeKeys - 1]
    , cursor :: Word8

      -- | The account public key from which the stake keys should be derived.
    , rewardAccountKey :: k 'AccountK XPub

      -- | Index of known stake keys
      --
      -- Invariant: indices should form a consecutive range.
    , dsIndexedKeys
        :: !(Map
                (RewardAccount)
                ((Index 'Soft 'AddressK), k 'AddressK XPub)
            )
    } deriving (Generic)

--------------------------------------------------------------------------------
-- Constructors
--------------------------------------------------------------------------------

-- | Construct a empty @DelegationState@. It has no stake keys, and is not
-- capable of delegation.
--
-- NOTE: The reward account key is needed for the DB. This could be changed.
mkEmptyDelegationState :: k 'AccountK XPub -> DelegationState k
mkEmptyDelegationState accK = DelegationState 0 0 accK Map.empty

-- | Construct a @DelegationState@.
mkDelegationState
    :: (ToRewardAccount k, SoftDerivation k)
    => Word8 -- Number of stake keys
    -> Word8 -- Current cursor (0, unless deserializing from DB)
    -> k 'AccountK XPub
    -> DelegationState k
mkDelegationState n c accountK =
    let
        -- FIXME: Do nicer
        indexes = map (toEnum . fromEnum) [0 .. (n - 1)] -- FIXME: Unsafe!
        m = Map.fromList $ map mkEntry indexes
    in
        DelegationState n c accountK m
  where
    mkEntry ix =
        let
            xpub = deriveAddressPublicKey accountK MutableAccount ix
            acct = toRewardAccount xpub
        in (acct, (ix, xpub))

--------------------------------------------------------------------------------
-- Operations
--------------------------------------------------------------------------------

-- | @withStakeKeys@ allows you to assign stake keys to a list of payment keys,
-- to construct a list of addresses.
--
-- Respects the cursor of the delegation state. If a stake key was used in a
-- change output via @takeStakeKey@, it won't immediately be used again here.
--
-- TODO: Test.
assignStakeKeys
    :: DelegationState k
    -> (a -> Maybe (k 'AddressK XPub) -> b)
    -> [a]
    -> [b]
assignStakeKeys s f as = zipWith f as (drop (fromEnum $ cursor s) $ safeCycle stakeKeys)
  where
    stakeKeys = map (snd . snd) $ Map.toList $ dsIndexedKeys s
    safeCycle [] = repeat Nothing
    safeCycle xs = map Just $ cycle xs

-- Repeated application should yield:
-- (n, i)
-- 3 0
-- 3 1
-- 3 2
-- 3 0
-- 3 1
-- ...
takeStakeKeyIndex :: DelegationState k -> (Word8, DelegationState k)
takeStakeKeyIndex (DelegationState n i k ix)
    | (i + 1) < n     = (i, DelegationState n (i + 1) k ix)
    | otherwise = (i, DelegationState n 0 k ix)

-- Take one of the stake keys in the "pool".
--
-- If called to retrieve stake keys for each generated address, the addresses
-- will get a even distribution of @numberOfStakeKeys@ stake keys.
takeStakeKey
    :: SoftDerivation k
    => DelegationState k
    -> (k 'AddressK XPub, DelegationState k)
takeStakeKey s =
    let
      (ix, s') = takeStakeKeyIndex s
      ix' = toEnum $ fromEnum ix -- FIXME: Use better types!
    in (deriveAddressPublicKey (rewardAccountKey s) MutableAccount ix', s')
    -- FIXME: This way of deriving the stake key is probably wrong!

allStakeKeys
    :: DelegationState k
    -> [k 'AddressK XPub]
allStakeKeys = map (snd . snd) . Map.toList . dsIndexedKeys

allRewardAccountsWithPaths
    :: DerivationPrefix
    -> DelegationState k
    -> [(RewardAccount, NonEmpty DerivationIndex)]
allRewardAccountsWithPaths prefix = map (\(r,(d,_)) -> (r, toFullPath d) ) . Map.toList . dsIndexedKeys
  where
    toFullPath = stakeDerivationPath prefix

-- | Helper to define @IsOurs@
isOurStakeKey
    :: RewardAccount
    -> [DerivationIndex]
    -> DelegationState k
    -> (Maybe (NonEmpty DerivationIndex), DelegationState k)
isOurStakeKey acc basePath s@DelegationState{dsIndexedKeys} =
    let
        ix = fst <$> Map.lookup acc dsIndexedKeys
        -- FIXME: Allow extending the map.
        --
        -- FIXME: We should write nice properties, showing that whatever certs
        -- appear on-chain, it works correctly.
        path x = append (DerivationIndex $ getIndex @'Soft x) basePath

        -- | Like :|, but the element is put last.
        append :: x -> [x] -> NonEmpty x
        append x xs = NE.reverse (x NE.:| reverse xs)
    in
        (path <$> ix, s)

instance (NFData (k 'AccountK XPub), NFData (k 'AddressK XPub))
    => NFData (DelegationState k)

deriving instance
    ( Show (k 'AccountK XPub)
    , Show (k 'AddressK XPub)
    ) => Show (DelegationState k)

deriving instance
    ( Eq (k 'AccountK XPub)
    , Eq (k 'AddressK XPub)
    ) => Eq (DelegationState k)
