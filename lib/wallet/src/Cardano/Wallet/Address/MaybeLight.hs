{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Address.MaybeLight
    ( MaybeLight (..)
    , LightDiscoverTxs
    , DiscoverTxs (..)
    )
    where

import Prelude

import Cardano.Wallet.Address.Derivation
    ( DelegationAddress
    , Depth (..)
    , PaymentAddress
    , ToRewardAccount (toRewardAccount)
    , liftDelegationAddressS
    , liftPaymentAddressS
    )
import Cardano.Wallet.Address.Derivation.Icarus
    ( IcarusKey
    )
import Cardano.Wallet.Address.Derivation.Shelley
    ( ShelleyKey
    )
import Cardano.Wallet.Address.Discovery
    ( dropLowerPendingIxs
    )
import Cardano.Wallet.Address.Discovery.Random
    ( RndState
    )
import Cardano.Wallet.Address.Discovery.RandomAny
    ( RndAnyState
    )
import Cardano.Wallet.Address.Discovery.Sequential
    ( SeqAddressPool (..)
    , SeqState (..)
    )
import Cardano.Wallet.Address.Discovery.SequentialAny
    ( SeqAnyState
    )
import Cardano.Wallet.Address.Discovery.Shared
    ( SharedState
    )
import Cardano.Wallet.Primitive.BlockSummary
    ( ChainEvents
    )
import Cardano.Wallet.Primitive.NetworkId
    ( HasSNetworkId
    )
import Cardano.Wallet.Primitive.Types.Address
    ( Address
    )
import Cardano.Wallet.Primitive.Types.RewardAccount
    ( RewardAccount
    )
import Data.Bifunctor
    ( second
    )

import qualified Cardano.Wallet.Address.Pool as AddressPool

-- | Checks whether the address discovery state @s@ works in light-mode
-- and returns a procedure for discovering addresses
-- if that is indeed the case.
class MaybeLight s where
    maybeDiscover :: Maybe (LightDiscoverTxs s)

type LightDiscoverTxs s =
    DiscoverTxs (Either Address RewardAccount) ChainEvents s

-- | Function that discovers transactions based on an address.
newtype DiscoverTxs addr txs s = DiscoverTxs
    { discoverTxs
        :: forall m. Monad m
        => (addr -> m txs) -> s -> m (txs, s)
    }

instance MaybeLight (RndState n) where
    maybeDiscover = Nothing

instance MaybeLight (RndAnyState n p) where
    maybeDiscover = Nothing


instance MaybeLight (SeqAnyState n k p) where
    maybeDiscover = Nothing

instance HasSNetworkId n => MaybeLight (SeqState n IcarusKey)
  where
    maybeDiscover = Just $ DiscoverTxs discoverSeq

instance HasSNetworkId n => MaybeLight (SeqState n ShelleyKey)
  where
    maybeDiscover = Just $ DiscoverTxs discoverSeqWithRewards

instance MaybeLight (SharedState n k) where
    maybeDiscover = Nothing

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
