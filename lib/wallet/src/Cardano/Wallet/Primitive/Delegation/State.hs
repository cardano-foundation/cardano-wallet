{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Module for `DelegationState`.
module Cardano.Wallet.Primitive.Delegation.State
    ( -- * Creation
      DelegationState (..)
    , initialDelegationState

      -- * Operations
    , presentableKeys
    , usableKeys
    , activeKeys

      -- * For Testing
    , keyAtIx
    , lastActiveIx
    , PointerUTxO (..)
    , State (..)
    , Key0Status (..)

      -- * Chain following model
    , Tx (..)
    , Cert (..)
    , applyTx
    , setPortfolioOf
    )
where

import Prelude

import Cardano.Crypto.Wallet
    ( XPub
    )
import Cardano.Wallet.Address.Derivation
    ( Depth (..)
    , DerivationType (..)
    , HardDerivation (..)
    , Index (..)
    , MkKeyFingerprint (paymentKeyFingerprint)
    , Role (..)
    , SoftDerivation (..)
    , ToRewardAccount (..)
    )
import Cardano.Wallet.Primitive.Types.Address
    ( Address
    )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..)
    )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..)
    )
import Cardano.Wallet.Primitive.Types.RewardAccount
    ( RewardAccount
    )
import Cardano.Wallet.Primitive.Types.Tx.TxIn
    ( TxIn (..)
    )
import Cardano.Wallet.Primitive.Types.Tx.TxOut
    ( TxOut (..)
    )
import Control.DeepSeq
    ( NFData
    )
import Data.Maybe
    ( maybeToList
    )
import GHC.Generics
    ( Generic
    )
import Quiet
    ( Quiet (..)
    )

import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TB

--------------------------------------------------------------------------------
-- Delegation State
--------------------------------------------------------------------------------

-- | Delegation state
--
-- === Goals
-- 1. Allow a wallet to have an arbitrary number of stake keys
-- 2. Ensure those stake keys can be automatically discovered on-chain
-- 3. Ensure the wallet is always aware of all stake keys it registers, even in
-- the case of concurrent user actions on multiple wallet instances, and old
-- wallet software.
--
--
-- === How
--
-- We track a consecutive range of keys that is extended with delegation of the
-- next unused key, and shrunk with key de-registration. We use a `PointerUTxO`
-- to ensure that transactions changing the state can't be accepted to the
-- ledger in any other order than intended. We also need some special care
-- regarding stake key 0, which old wallet software could try to de-register.
--
-- === Diagram
-- Diagram of states, where the list denotes active (registered /and/ delegating)
-- keys.
--
-- Here we assume the minUTxOValue is 1 ada.
--
-- Note that intermediate steps for the `PointerUTxO` should be skipped within a
-- transaction.
-- E.g. to transition from [] to [0,1,2] we should deposit 1 ada to key 3,
-- skipping key 2.
--
-- See the implementation of `setPortfolioOf` and `applyTx` for more details.
--
-- @
-- ┌────────────────────┐           ┌────────────────────┐                     ┌────────────────────┐            ┌─────────────────────┐
-- │                    │           │                    │                     │                    │            │                     │
-- │                    │           │                    │       Pointer       │                    │            │                     │
-- │                    │──────────▶│                    │ ──────deposit──────▶│                    │ ─────────▶ │                     │ ─────────▶
-- │                    │           │                    │                     │       [0,1]        │            │       [0,1,2]       │
-- │         []         │           │        [0]         │                     │1 ada held by key 2 │            │ 1 ada held by key 3 │
-- │                    │           │                    │                     │                    │            │                     │
-- │                    │           │                    │       Pointer       │                    │            │                     │
-- │                    │◀──────────│                    │ ◀─────deposit ──────│                    │◀────────── │                     │◀──────────
-- │                    │           │                    │       returned      │                    │            │                     │
-- └────────────────────┘◀──┐       └────────────────────┘                     └────────────────────▲            ▲─────────────────────▲            ▲
--                          └───┐                                                     │       ▲     └─┐         ┌┘      │       ▲      └─┐         ┌┘
-- Normal states                └───┐                                                 │       │       └─┐     ┌─┘       │       │        └─┐     ┌─┘
--                                  └───┐                                             │       │         └─┐ ┌─┘         │       │          └─┐ ┌─┘
-- ╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳└───┐╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳│╳╳╳╳╳╳╳│╳╳╳╳╳╳╳╳╳╳╳│ │╳╳╳╳╳╳╳╳╳╳╳│╳╳╳╳╳╳╳│╳╳╳╳╳╳▶     ├─┤
--                                          └───┐                                     │       │         ┌─┘ └─┐         │       │          ┌─┘ └─┐
-- States caused by                             └────┐Pointer                         ▼       │       ┌─┘     └─┐       ▼       │        ┌─┘     └─┐
-- old wallet                                        └deposit                  ┌────────────────────┐─┘         └┬─────────────────────┐─┘         └─
-- de-registering                                     returned─┐               │                    │            │                     │
-- stake-key 0                                                 └────┐          │                    │ ─────────▶ │                     │ ─────────▶
-- of multi-stake                                                   └────┐     │                    │            │                     │
-- key wallet                                                            └──── │        [1]         │            │        [1,2]        │
--                                                                             │1 ada held by key 2 │            │ 1 ada held by key 3 │
--                                                                             │                    │            │                     │
--                                                                             │                    │◀────────── │                     │◀──────────
--                                                                             │                    │            │                     │
--                                                                             │                    │            │                     │
--                                                                             └────────────────────┘            └─────────────────────┘
-- @
data DelegationState k = DelegationState
    { rewardAccountKey :: k 'AccountK XPub
    -- ^ The account public key from which the stake keys should be derived.
    , state :: State
    }
    deriving (Generic)

-- | Construct the initial delegation state.
initialDelegationState
    :: k 'AccountK XPub
    -> DelegationState k
initialDelegationState accK = DelegationState accK Zero

-- | The internal state, without the account key.
--
-- TODO: Perhaps we should model this as
-- @S = S1 * S2, where S1 = Bool, S2 = ix * UTxOPointer@ - having two
-- "concurrent" states tracking stake keys, where the first one is identical to
-- legacy single-stake key wallets.
--
-- Maybe that would help simplify `applyTx` and `setPortfolioOf`...
data State
    = -- | No active stake keys. The initial state of a new wallet.
      Zero
    | -- | The first stake-key (index 0) is registered and either delegating or
      -- about to be delegating.
      One
    | -- | There is more than one active stake keys. Can only be reached using
      -- wallets with support for multiple stake keys.
      More
        !(Index 'Soft 'CredFromKeyK)
        -- nextKeyIx - the ix of the next unused key
        PointerUTxO
        -- ^ pointer utxo that need to be spent when changing state.
        !Key0Status
    deriving (Eq, Show, Generic)

instance NFData State

-- | Is key 0 still registered? For compatibility with
-- single-stake-key wallets, we need to track this.
--
-- >>> activeKeys (More (toEnum 3) p ValidKey0)
-- [0, 1, 2]
--
-- >>> activeKeys (More (toEnum 3) p MissingKey0)
-- [1, 2]
--
-- (pseudocode; requires a bit more boilerplate to compile)
--
-- See the implementation of @applyTx@ for how it is used.
data Key0Status = ValidKey0 | MissingKey0
    deriving (Eq, Show, Generic)

instance NFData Key0Status

instance
    (NFData (k 'AccountK XPub), NFData (k 'CredFromKeyK XPub))
    => NFData (DelegationState k)

deriving instance
    ( Show (k 'AccountK XPub)
    , Show (k 'CredFromKeyK XPub)
    )
    => Show (DelegationState k)

deriving instance
    ( Eq (k 'AccountK XPub)
    , Eq (k 'CredFromKeyK XPub)
    )
    => Eq (DelegationState k)

keyAtIx
    :: (SoftDerivation k, AddressCredential k ~ 'CredFromKeyK)
    => DelegationState k
    -> Index 'Soft 'CredFromKeyK
    -> k 'CredFromKeyK XPub
keyAtIx s = deriveAddressPublicKey (rewardAccountKey s) MutableAccount

nextKeyIx
    :: DelegationState k
    -> Index 'Soft 'CredFromKeyK
nextKeyIx s = case state s of
    Zero -> minBound
    One -> succ minBound
    More ix _ _ -> ix

lastActiveIx
    :: DelegationState k
    -> Maybe (Index 'Soft 'CredFromKeyK)
lastActiveIx s
    | nextKeyIx s == minBound = Nothing
    | otherwise = Just $ pred $ nextKeyIx s

data PointerUTxO = PointerUTxO {pTxIn :: TxIn, pCoin :: Coin}
    deriving (Generic, Eq, Show)
    deriving anyclass (NFData)

-- | Returns the index corresponding to the payment key the `PointerUTxO`
-- should be locked with for a portfolio of a given size @n@.
--
-- In our current implementation we require the `PointerUTxO` to be created in
-- the @[0] -> [0,1] transition@, i.e. @nextKeyIx 1 -> nextKeyIx 2@.
pointerIx
    :: Int
    -> Maybe (Index 'Soft 'CredFromKeyK)
pointerIx 0 = Nothing
pointerIx 1 = Nothing
pointerIx n = Just $ toEnum n

-- | Retrieve the `PointerUTxO` from a `DelegationState` if it has one.
pointer :: DelegationState k -> Maybe PointerUTxO
pointer (DelegationState _ (More _ p _)) = Just p
pointer _ = Nothing

--------------------------------------------------------------------------------
-- Chain
--------------------------------------------------------------------------------

-- | A transaction type specific to `DelegationState`.
--
-- Intended to be converted both from and to a more real transaction type.
--
-- When constructing a real transaction from `Tx`, these `outputs`
-- should appear before other outputs. In the theoretical event that there's
-- also a user-specified output with the same payment key as the pointer output,
-- `applyTx` will track the first one as the new pointer.
data Tx = Tx
    { certs :: [Cert]
    , inputs :: [(TxIn, Coin)]
    , outputs :: [TxOut]
    }
    deriving (Eq, Generic)
    deriving (Show) via (Quiet Tx)

instance Semigroup Tx where
    (Tx cs1 is1 os1) <> (Tx cs2 is2 os2) =
        Tx (cs1 <> cs2) (is1 <> is2) (os1 <> os2)

data Cert
    = RegisterKey RewardAccount
    | -- | Which pool we're delegating to is here (and for now) irrelevant.
      -- The main thing is that there exists a witness on-chain for this stake
      -- key (registration certs don't require witnesses)
      --
      -- TODO: We may also want to add the PoolId here.
      Delegate RewardAccount
    | DeRegisterKey RewardAccount
    deriving (Eq, Show, Generic)

-- | Given a `DelegationState`, produce a `Tx` registering or de-registering
-- stake-keys, in order to have @n@ stake-keys.
--
-- E.g. @setPortfolioOf s0 _ _ 2@ creates a tx which after application causes
-- the state to have @activeKeys == [0,1]@
--
-- Returns @Nothing@ if the target @n@ is already reached.
setPortfolioOf
    :: ( SoftDerivation k
       , ToRewardAccount k
       , AddressCredential k ~ 'CredFromKeyK
       )
    => DelegationState k
    -> Coin
    -- ^ minUTxOVal
    -> (k 'CredFromKeyK XPub -> Address)
    -- ^ A way to construct an Address
    -> (RewardAccount -> Bool)
    -- ^ Whether or not the key is registered.
    --
    -- TODO: Need a Set or Map for the real implementation with LSQ.
    -> Int
    -- ^ Target number of stake keys.
    -> Maybe Tx
setPortfolioOf ds minUTxOVal mkAddress isReg n =
    repairKey0IfNeededTx <> changeStateTx
  where
    -- The `changeStateTx` calculation assumes that key 0 is registered. If it
    -- is not, we fix it here.
    --
    -- At some point we will need to take a `Set PoolId` instead of `n`. If
    -- we're in the state of key 1 and 2 delegating to pools B and C
    -- respectively, we likely want:
    -- - `setPortfolio [A,B,C]` to delegate key 0 to A, instead of key 3.
    -- - `setPortfolio [B,C]` to replace key 2 with key 0
    repairKey0IfNeededTx :: Maybe Tx
    repairKey0IfNeededTx = case repairKey0 $ state ds of
        [] -> Nothing
        cs -> Just $ Tx cs [] []
      where
        repairKey0 (More _ _ MissingKey0) | n > 0 = deleg [minBound]
        repairKey0 _ = []

    changeStateTx :: Maybe Tx
    changeStateTx = txWithCerts $ case compare (toEnum n) (nextKeyIx ds) of
        GT -> deleg [nextKeyIx ds .. toEnum (n - 1)]
        EQ -> []
        LT -> dereg $ reverse [toEnum n .. (pred $ nextKeyIx ds)]
      where
        txWithCerts [] = Nothing
        txWithCerts cs =
            Just
                $ Tx
                    { certs = cs
                    , inputs = maybeToList (mkTxIn <$> pointer ds)
                    , outputs =
                        maybeToList
                            $
                            -- Note that this is the only place where we move the pointer.
                            -- I.e. repairKey0IfNeededTx won't do it on its own.
                            ( \i ->
                                TxOut
                                    (mkAddress $ keyAtIx ds i)
                                    (TB.fromCoin minUTxOVal)
                            )
                                <$> pointerIx n
                    }
          where
            mkTxIn (PointerUTxO txIx coin) = (txIx, coin)
    -- Note: If c > minUTxOVal we need to rely on the wallet to return the
    -- difference to the user as change.

    deleg :: [Index 'Soft 'CredFromKeyK] -> [Cert]
    deleg =
        ( >>=
            \ix ->
                if isReg (acct ix)
                    then [Delegate (acct ix)]
                    else [RegisterKey (acct ix), Delegate (acct ix)]
        )

    dereg :: [Index 'Soft 'CredFromKeyK] -> [Cert]
    dereg ixs =
        [ DeRegisterKey (acct ix)
        | ix <- ixs
        , isReg . acct $ ix
        -- We need to /at least/ check @isReg (key 0)@, because the user could
        -- have deregistered it using old wallet software.
        ]

    acct = toRewardAccount . keyAtIx ds

-- | Apply a `Tx` to a `DelegationState`.
--
-- Expects the @PointerUTxO@ to be correctly managed, and will panic otherwise.
applyTx
    :: forall k
     . ( SoftDerivation k
       , ToRewardAccount k
       , MkKeyFingerprint k Address
       , MkKeyFingerprint k (k 'CredFromKeyK XPub)
       , AddressCredential k ~ 'CredFromKeyK
       )
    => Tx
    -> Hash "Tx"
    -> DelegationState k
    -> DelegationState k
applyTx (Tx cs _ins outs) h ds0 = foldl applyCert ds0 cs
  where
    applyCert ds cert = flip modifyState ds $ case cert of
        RegisterKey _ -> id
        Delegate k
            | k == nextKey ds -> inc
            | otherwise -> modifyKey0 cert
        DeRegisterKey k
            | Just k == lastActiveKey ds -> dec
            | otherwise -> modifyKey0 cert
      where
        inc s = case s of
            Zero -> One
            One -> More (toEnum 2) (findOut $ toEnum 2) ValidKey0
            More ix _ is0Reg -> let ix' = succ ix in More ix' (findOut ix') is0Reg
        dec s = case s of
            Zero -> error "impossible: can't decrement beyond zero"
            One -> Zero
            More ix _ is0Reg
                | fromEnum ix > 2 -> let ix' = pred ix in More ix' (findOut ix') is0Reg
                | otherwise -> case is0Reg of
                    ValidKey0 -> One
                    MissingKey0 -> Zero

        findOut ix = case map mkPointer pointerOuts of
            (x : _) -> x
            _ ->
                error
                    $ mconcat
                        [ "couldn't find pointer output for ix "
                        , show ix
                        , " with state "
                        , show $ state ds
                        ]
          where
            isOurOut (TxOut addr _b) =
                case (paymentKeyFingerprint @k $ keyAtIx ds ix, paymentKeyFingerprint addr) of
                    (Right fp, Right fp')
                        | fp == fp' -> True
                        | otherwise -> False
                    _ -> False
            mkPointer (txIx, TxOut _ tb) = PointerUTxO (TxIn h txIx) (TB.getCoin tb)
            pointerOuts = filter (isOurOut . snd) $ zip [0 ..] outs

    modifyState
        :: (State -> State)
        -> DelegationState k
        -> DelegationState k
    modifyState f s = s{state = f (state s)}

    -- \| Modifies the "isKey0Reg" of the `More` constructor.
    modifyKey0 cert s@(More i p _) = case cert of
        Delegate k
            | k == acct 0 -> More i p ValidKey0
            | otherwise -> s
        DeRegisterKey k
            | k == acct 0 -> More i p MissingKey0
            | otherwise -> s
        _ -> s
      where
        acct = toRewardAccount . keyAtIx ds0 . toEnum
    modifyKey0 _ s = s

    lastActiveKey ds' = toRewardAccount . keyAtIx ds' <$> lastActiveIx ds'
    nextKey ds' = toRewardAccount . keyAtIx ds' $ nextKeyIx ds'

--------------------------------------------------------------------------------
-- Operations
--------------------------------------------------------------------------------

-- | All stake keys worth listing to the user.
--
-- May include:
-- 1. Active stake keys
-- 2. The next un-active key
--
-- NOTE: In theory we might want also present stake keys that are unexpectedly
-- registered, as they could be de-registered to reclaim the deposit, but this
-- should in-practice never happen.
--
-- If @sn@ denotes the state with @n@ registered and delegating keys:
-- >>> presentableKeys s0
-- [0]
-- >>> presentableKeys s1
-- [0, 1]
-- >>> presentableKeys s2
-- [0, 1, 2]
presentableKeys
    :: ( SoftDerivation k
       , AddressCredential k ~ 'CredFromKeyK
       )
    => DelegationState k
    -> [k 'CredFromKeyK XPub]
presentableKeys s = case lastActiveIx s of
    Just i -> map (keyAtIx s) [minBound .. (succ i)]
    Nothing -> [keyAtIx s minBound]

-- Keys meant to be used in addresses.
--
-- If @sn@ denotes the state with @n@ registered and delegating keys:
-- >>> usableKeys s0
-- [0]
-- >>> usableKeys s1
-- [0]
-- >>> usableKeys s2
-- [0, 1]
--
-- Note that for @s0@, we have no active stake keys, but we still want to use
-- key 0 as part of addresses.
--
-- Also note that old wallet software may unregister the first stake key 0
-- despite stake key 1 being active. This doesn't affect `usableKeys`
-- (it still includes key 0), as we view the state as incorrect and temporary.
usableKeys
    :: ( SoftDerivation k
       , AddressCredential k ~ 'CredFromKeyK
       )
    => DelegationState k
    -> [k 'CredFromKeyK XPub]
usableKeys s = case lastActiveIx s of
    Just i -> map (keyAtIx s) [minBound .. i]
    Nothing -> [keyAtIx s minBound]

-- | For testing. Returns all registered and delegating stake keys.
activeKeys
    :: ( SoftDerivation k
       , AddressCredential k ~ 'CredFromKeyK
       )
    => DelegationState k
    -> [k 'CredFromKeyK XPub]
activeKeys ds = map (keyAtIx ds) $ case state ds of
    Zero -> []
    One -> [minBound]
    More nextIx _ ValidKey0 -> [minBound .. pred nextIx]
    More nextIx _ MissingKey0 -> [succ minBound .. pred nextIx]
