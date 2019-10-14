{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: Apache-2.0
--
-- Here we find the "business logic" to manage a Cardano wallet. This is a
-- direct implementation of the model from the [Formal Specification for a Cardano Wallet](https://github.com/input-output-hk/cardano-wallet/blob/master/specifications/wallet/formal-specification-for-a-cardano-wallet.pdf)
-- Note that, this module is purposedly agnostic to how blocks are retrieved or
-- how various types are serialized.
--
-- This is really about how the wallet keep track of its internal state, and its
-- UTxO (where the relationship is defined via the 'IsOurs' abstraction to allow
-- this core code to be used with any sort of derivation scheme).
--
-- All those functions are pure and there's no reason to shove in any sort of
-- side-effects in here :)

module Cardano.Wallet.Primitive.Model
    (
    -- * Type
      Wallet
    , BlockchainParameters (..)

    -- * Construction & Modification
    , initWallet
    , updateState
    , applyBlock
    , applyBlocks
    , unsafeInitWallet

    -- * Accessors
    , currentTip
    , getState
    , availableBalance
    , totalBalance
    , totalUTxO
    , availableUTxO
    , utxo
    , blockchainParameters

    -- * Auxiliary
    , slotParams

    ) where

import Prelude

import Cardano.Wallet.Primitive.AddressDiscovery
    ( IsOurs (..) )
import Cardano.Wallet.Primitive.Types
    ( Block (..)
    , BlockHeader (..)
    , DefineTx (..)
    , Direction (..)
    , Dom (..)
    , EpochLength (..)
    , FeePolicy (..)
    , Hash (..)
    , SlotLength (..)
    , SlotParameters (SlotParameters)
    , StartTime (..)
    , TxIn (..)
    , TxMeta (..)
    , TxOut (..)
    , TxStatus (..)
    , UTxO (..)
    , balance
    , excluding
    , restrictedBy
    , txIns
    )
import Control.DeepSeq
    ( NFData (..), deepseq )
import Control.Monad
    ( foldM, forM )
import Control.Monad.Trans.State.Strict
    ( State, evalState, runState, state )
import Data.ByteArray.Encoding
    ( Base (Base16), convertToBase )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.Generics.Labels
    ()
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Maybe
    ( catMaybes )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Quantity (..) )
import Data.Set
    ( Set )
import Data.Text.Class
    ( toText )
import Data.Word
    ( Word16, Word32 )
import Fmt
    ( Buildable (..), blockListF', indentF )
import GHC.Generics
    ( Generic )
import Numeric.Natural
    ( Natural )

import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text.Encoding as T

{-------------------------------------------------------------------------------
                                     Type
-------------------------------------------------------------------------------}

-- | An opaque wallet type, see 'initWallet', 'updateState', 'applyBlock', and
-- 'applyBlocks' to construct and update wallets.
--
-- Internally, this keeps track or a few things including:
--
--  - UTxOs
--  - Transaction history
--  - Known & used addresses, via address discovery state
--  - Blockchain parameters
--
-- The 'Wallet' is parameterized over two types:
--
-- - @s@: A _state_ used to keep track of known addresses. The business logic
--   doesn't know how to answer the question 'Is this address ours?', so we
--   expect this state to be able to answer that for us.
--
-- - @t@: A target backend. This makes the wallet fairly agnostic to the type
--   of binary representation used by the underlying target network and it
--   allows us to re-use the same logic to provide a wallet backend for multiple
--   backends (for instance, Byron or Shelley) which may have divergence in
--   their binary formats. For the sake of this module, we only care about one
--   particular super-power, and its the ability to compute transaction id
--   (which is intrinsically linked to the transaction's binary format).
--
-- A few examples to make it concrete:
--
-- @
-- Wallet RndState Byron
-- Wallet SeqState Shelley
-- Wallet SeqState Bitcoin
-- @
data Wallet s t where
    Wallet :: (IsOurs s, NFData s, Show s, DefineTx t)
        => UTxO -- Unspent tx outputs belonging to this wallet
        -> BlockHeader -- Header of the latest applied block (current tip)
        -> s -- Address discovery state
        -> BlockchainParameters
        -> Wallet s t

deriving instance Show (Wallet s t)
deriving instance Eq s => Eq (Wallet s t)
instance NFData (Wallet s t) where
    rnf (Wallet u sl s bp) =
        deepseq (rnf u) $
        deepseq (rnf sl) $
        deepseq (rnf s) $
        deepseq (rnf bp) ()

instance Buildable s => Buildable (Wallet s t) where
    build (Wallet u tip s bp) = "Wallet s t\n"
        <> indentF 4 ("Tip: " <> build tip)
        <> indentF 4 ("Parameters:\n" <> indentF 4 (build bp))
        <> indentF 4 ("UTxO: " <> build u)
        <> indentF 4 (build s)

data BlockchainParameters = BlockchainParameters
    { getGenesisBlockHash :: Hash "Genesis"
        -- ^ Hash of the very first block
    , getGenesisBlockDate :: StartTime
        -- ^ Start time of the chain.
    , getFeePolicy :: FeePolicy
        -- ^ Policy regarding transaction fee.
    , getSlotLength :: SlotLength
        -- ^ Length, in seconds, of a slot.
    , getEpochLength :: EpochLength
        -- ^ Number of slots in a single epoch.
    , getTxMaxSize :: Quantity "byte" Word16
        -- ^ Maximum size of a transaction (soft or hard limit).
    , getEpochStability :: Quantity "block" Word32
        -- ^ Length of the suffix of the chain considered unstable
    } deriving (Generic, Show, Eq)

instance NFData BlockchainParameters

instance Buildable BlockchainParameters where
    build bp = blockListF' "" id
        [ "Genesis block hash: " <> genesisF (getGenesisBlockHash bp)
        , "Genesis block date: " <> startTimeF (getGenesisBlockDate bp)
        , "Fee policy:         " <> feePolicyF (getFeePolicy bp)
        , "Slot length:        " <> slotLengthF (getSlotLength bp)
        , "Epoch length:       " <> epochLengthF (getEpochLength bp)
        , "Tx max size:        " <> txMaxSizeF (getTxMaxSize bp)
        , "Epoch stability:    " <> epochStabilityF (getEpochStability bp)
        ]
      where
        genesisF = build . T.decodeUtf8 . convertToBase Base16 . getHash
        startTimeF (StartTime s) = build s
        feePolicyF = build . toText
        slotLengthF (SlotLength s) = build s
        epochLengthF (EpochLength s) = build s
        txMaxSizeF (Quantity s) = build s
        epochStabilityF (Quantity s) = build s

slotParams :: BlockchainParameters -> SlotParameters
slotParams bp =
    SlotParameters
        (bp ^. #getEpochLength)
        (bp ^. #getSlotLength)
        (bp ^. #getGenesisBlockDate)

{-------------------------------------------------------------------------------
                          Construction & Modification
-------------------------------------------------------------------------------}

-- | Create a an empty wallet and apply the given genesis block
--
-- The wallet tip will be set to the header of the applied genesis block.
initWallet
    :: forall s t. (IsOurs s, NFData s, Show s, DefineTx t)
    => Block (Tx t)
        -- ^ The genesis block
    -> BlockchainParameters
        -- ^ Initial blockchain parameters
    -> s
        -- ^ Initial address discovery state
    -> ([(Tx t, TxMeta)], Wallet s t)
initWallet block bp s =
    let
        ((txs, u), s') = prefilterBlock (Proxy @t) block mempty s
    in
        (txs, Wallet u (header block) s' bp)

-- | Constructs a wallet from the exact given state. Using this function instead
-- of 'initWallet' and 'applyBlock' allows the wallet invariants to be
-- broken. Therefore it should only be used in the special case of loading
-- wallet checkpoints from the database (where it is assumed a valid wallet was
-- stored into the database).
unsafeInitWallet
    :: (IsOurs s, NFData s, Show s, DefineTx t)
    => UTxO
       -- ^ Unspent tx outputs belonging to this wallet
    -> BlockHeader
    -- ^ Header of the latest applied block (current tip)
    -> s
    -- ^ Address discovery state
    -> BlockchainParameters
    -- ^ Blockchain parameters
    -> Wallet s t
unsafeInitWallet = Wallet

-- | Update the state of an existing Wallet model
updateState
    :: (IsOurs s, NFData s, Show s)
    => s
    -> Wallet s t
    -> Wallet s t
updateState s (Wallet u tip _ bp) = Wallet u tip s bp

-- | Apply Block is the primary way of making the wallet evolve. It returns the
-- updated wallet state, as well as a set of all transactions belonging to the
-- wallet discovered while applying the block.
applyBlock
    :: forall s t. (DefineTx t)
    => Block (Tx t)
    -> Wallet s t
    -> ([(Tx t, TxMeta)], Wallet s t)
applyBlock !b (Wallet !u _ s bp) =
    let
        ((txs, u'), s') = prefilterBlock (Proxy @t) b u s
    in
        ( txs
        , Wallet u' (b ^. #header) s' bp
        )

-- | Apply multiple blocks in sequence to an existing wallet, returning a list
--   of intermediate wallet states.
--
-- Each intermediate wallet state is paired with the set of transactions that
-- belong to that state but not the previous state.
--
-- For an original wallet state __@w@__ and a list of blocks __@b@__ such that:
--
-- > b = [b1, b2, ..., bn]
--
-- Returns the following list of updates:
--
-- > [ (t b1, w + b1)
-- > , (t b2, w + b1 + b2)
-- > , ...
-- > , (t bn, w + b1 + b2 + ... + bn) ]
--
-- Where:
--
-- * __@(t bi)@__   is the set of transactions contained within block __@bi@__.
-- * __@(w + bi)@__ is the wallet state after applying block __@bi@__ to wallet
--   __@w@__.
--
applyBlocks
    :: forall s t. (DefineTx t)
    => NonEmpty (Block (Tx t))
    -> Wallet s t
    -> NonEmpty ([(Tx t, TxMeta)], Wallet s t)
applyBlocks (block0 :| blocks) cp =
    NE.scanl (flip applyBlock . snd) (applyBlock block0 cp) blocks

{-------------------------------------------------------------------------------
                                   Accessors
-------------------------------------------------------------------------------}

-- | Get the wallet current tip
currentTip :: Wallet s t -> BlockHeader
currentTip (Wallet _ tip _ _) = tip

-- | Get the wallet current state
getState :: Wallet s t -> s
getState (Wallet _ _ s _) = s

-- | Available balance = 'balance' . 'availableUTxO'
availableBalance :: DefineTx t => Set (Tx t) -> Wallet s t -> Natural
availableBalance pending =
    balance . availableUTxO pending

-- | Total balance = 'balance' . 'totalUTxO'
totalBalance :: DefineTx t => Set (Tx t) -> Wallet s t -> Natural
totalBalance pending =
    balance . totalUTxO pending

-- | Available UTxO = @pending ⋪ utxo@
availableUTxO
    :: forall s t. DefineTx t
    => Set (Tx t)
    -> Wallet s t
    -> UTxO
availableUTxO pending (Wallet u _ _ _) =
    u  `excluding` txIns @t pending

-- | Total UTxO = 'availableUTxO' @<>@ 'changeUTxO'
totalUTxO
    :: forall s t. DefineTx t
    => Set (Tx t)
    -> Wallet s t
    -> UTxO
totalUTxO pending wallet@(Wallet _ _ s _) =
    availableUTxO pending wallet <> changeUTxO (Proxy @t) pending s

-- | Actual utxo
utxo :: Wallet s t -> UTxO
utxo (Wallet u _ _ _) = u

-- | Get the current chain parameters.
--
-- Parameters may change over time via protocol updates, so we keep track of
-- them as part of the wallet checkpoints.
blockchainParameters :: Wallet s t -> BlockchainParameters
blockchainParameters (Wallet _ _ _ bp) = bp

{-------------------------------------------------------------------------------
                               Internals
-------------------------------------------------------------------------------}

-- | Prefiltering returns all transactions of interest for the wallet. A
-- transaction is a matter of interest for the wallet if:
--
--    - It has known input(s)
--    - and/or It has known output(s)
--
-- In practice, most transactions that are of interest have an output to the
-- wallet but some may actually have no change output whatsoever and be only
-- linked to the wallet by their inputs.
--
-- In order to identify transactions that are ours, we do therefore look for
-- known inputs and known outputs. However, we can't naievely look at the domain
-- of the utxo constructed from all outputs that are ours (as the specification
-- would suggest) because some transactions may use outputs of a previous
-- transaction within the same block as an input. Therefore, Looking solely at
-- the final 'dom (UTxO ⊳ oursOuts)', we would be missing all intermediate txs
-- that happen from _within_ the block itself.
--
-- As a consequence, we do have to traverse the block, and look at transaction
-- in order, starting from the known inputs that can be spent (from the previous
-- UTxO) and, collect resolved tx outputs that are ours as we apply transactions.
prefilterBlock
    :: forall s t. (IsOurs s, DefineTx t)
    => Proxy t
    -> Block (Tx t)
    -> UTxO
    -> s
    -> (([(Tx t, TxMeta)], UTxO), s)
prefilterBlock proxy b u0 = runState $ do
    (ourTxs, ourU) <- foldM applyTx (mempty, u0) (transactions b)
    return (ourTxs, ourU)
  where
    mkTxMeta :: Natural -> Direction -> TxMeta
    mkTxMeta amt dir = TxMeta
        { status = InLedger
        , direction = dir
        , slotId = b ^. #header . #slotId
        , amount = Quantity amt
        }
    applyTx
        :: ([(Tx t, TxMeta)], UTxO)
        -> Tx t
        -> State s ([(Tx t, TxMeta)], UTxO)
    applyTx (!txs, !u) tx = do
        ourU <- state $ utxoOurs proxy tx
        let ourIns = Set.fromList (inputs @t tx) `Set.intersection` dom (u <> ourU)
        let u' = (u <> ourU) `excluding` ourIns
        let received = fromIntegral @_ @Integer $ balance ourU
        let spent = fromIntegral @_ @Integer $ balance (u `restrictedBy` ourIns)
        let amt = fromIntegral $ abs (received - spent)
        let hasKnownInput = ourIns /= mempty
        let hasKnownOutput = ourU /= mempty
        return $ if hasKnownOutput && not hasKnownInput then
            ( (tx, mkTxMeta amt Incoming) : txs
            , u'
            )
        else if hasKnownInput then
            ( (tx, mkTxMeta amt Outgoing) : txs
            , u'
            )
        else
            (txs, u)

-- | Get the change UTxO
--
-- NOTE
-- We _safely_ discard the state here because we aren't intending to
-- discover any new addresses through this operation. In practice, we
-- can only discover new addresses when applying blocks. The state is
-- therefore use in a read-only mode here.
changeUTxO
    :: forall s t. (IsOurs s, DefineTx t)
    => Proxy t
    -> Set (Tx t)
    -> s
    -> UTxO
changeUTxO proxy pending = evalState $
    mconcat <$> mapM (state . utxoOurs proxy) (Set.toList pending)

-- | Construct our _next_ UTxO (possible empty) from a transaction by selecting
-- outputs that are ours. It is important for the transaction outputs to be
-- ordered correctly, since they become available inputs for the subsequent
-- blocks.
utxoOurs
    :: forall s t. (IsOurs s, DefineTx t)
    => Proxy t
    -> (Tx t)
    -> s
    -> (UTxO, s)
utxoOurs _ tx = runState $ toUtxo <$> forM (zip [0..] (outputs @t tx)) filterOut
  where
    toUtxo = UTxO . Map.fromList . catMaybes
    filterOut (ix, out) = do
        predicate <- state $ isOurs $ address out
        return $ if predicate
            then Just (TxIn (txId @t tx) ix, out)
            else Nothing
