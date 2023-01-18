{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- This module implements the "business logic" to manage a Cardano wallet.
-- It is a direct implementation of the model, with extensions, from the
-- [Formal Specification for a Cardano Wallet](https://github.com/input-output-hk/cardano-wallet/blob/master/specifications/wallet/formal-specification-for-a-cardano-wallet.pdf).
--
-- In other words, this module is about how the wallet keeps track of its
-- internal state, specifically the 'UTxO' set and the address discovery state.
-- This module is intentionally agnostic to specific address formats, and
-- instead relies on the 'IsOurs' abstraction.  It is also agnostic to issues
-- such as how blocks are retrieved from the network, or how the state is
-- serialized and cached in the local database.
--
-- All those functions are pure and there's no reason to shove in any sort of
-- side-effects in here. 🙂

module Cardano.Wallet.Primitive.Model
    (
    -- * Type
      Wallet

    -- * Construction & Modification
    , initWallet
    , updateState
    , FilteredBlock (..)
    , applyBlock
    , applyBlocks
    , applyBlockData

    , BlockData (..)
    , firstHeader
    , lastHeader

    -- * Accessors
    , currentTip
    , getState
    , availableBalance
    , totalBalance
    , totalUTxO
    , availableUTxO
    , utxo

    -- * Delta Type
    , DeltaWallet

    -- * Internal
    , unsafeInitWallet
    -- ** Exported for testing
    , spendTx
    , utxoFromTx
    , utxoFromTxOutputs
    , utxoFromTxCollateralOutputs
    , applyTxToUTxO
    , applyOurTxToUTxO
    , changeUTxO
    , discoverAddressesBlock
    , discoverFromBlockData
    , updateOurs
    ) where

import Prelude

import Cardano.Wallet.Primitive.AddressDiscovery
    ( DiscoverTxs (..), IsOurs (..) )
import Cardano.Wallet.Primitive.BlockSummary
    ( BlockEvents (..)
    , BlockSummary (..)
    , ChainEvents
    , fromBlockEvents
    , fromEntireBlock
    , toAscBlockEvents
    )
import Cardano.Wallet.Primitive.Types
    ( Block (..)
    , BlockHeader (..)
    , DelegationCertificate (..)
    , Slot
    , WithOrigin (..)
    , chainPointFromBlockHeader
    , dlgCertAccount
    , toSlot
    )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..) )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..), distance )
import Cardano.Wallet.Primitive.Types.RewardAccount
    ( RewardAccount (..) )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle )
import Cardano.Wallet.Primitive.Types.Tx
    ( Direction (..)
    , Tx (..)
    , TxMeta (..)
    , TxStatus (..)
    , collateralInputs
    , inputs
    , txScriptInvalid
    )
import Cardano.Wallet.Primitive.Types.Tx.TxIn
    ( TxIn (..) )
import Cardano.Wallet.Primitive.Types.UTxO
    ( DeltaUTxO, UTxO (..), balance, excluding, excludingD, receiveD )
import Control.DeepSeq
    ( NFData (..), deepseq )
import Control.Monad.Trans.State.Strict
    ( State, evalState, state )
import Data.Bifunctor
    ( first )
import Data.Delta
    ( Delta (..) )
import Data.Foldable
    ( Foldable (toList) )
import Data.Functor.Identity
    ( Identity (..) )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Maybe
    ( isJust )
import Data.Quantity
    ( Quantity )
import Data.Set
    ( Set )
import Data.Word
    ( Word32 )
import Fmt
    ( Buildable (..), indentF )
import GHC.Generics
    ( Generic )

import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TB
import qualified Cardano.Wallet.Primitive.Types.Tx.TxOut as TxOut
import qualified Cardano.Wallet.Primitive.Types.UTxO as UTxO
import qualified Data.Delta as Delta
import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import qualified Data.Set as Set

{-------------------------------------------------------------------------------
                                      Type
-------------------------------------------------------------------------------}

-- | Abstract data type representing a wallet state.
--
-- A 'Wallet' keeps track of transaction outputs and associated addresses that
-- belong to /us/ -- we are able to spend these outputs because we know the
-- corresponding signing key belonging to the output. Hence, we are able to
-- produce witness engaging those outputs as they become inputs in forthcoming
-- transactions according to UTxO model.  This information is associated to a
-- particular point on the blockchain.
--
-- Internally, a 'Wallet' keeps track of
--
--  - UTxOs
--  - Known & used addresses, via address discovery state
--  - The associated 'BlockHeader' indicating the point on the chain.
--
-- The 'Wallet' is parameterized over a single type:
--
-- - @s@ is a /state/ used to keep track of known addresses.
--   Typically, this state will be an instance of the 'IsOurs' class,
--   e.g. @'IsOurs' s 'Address'@.
--
-- A few examples to make it concrete:
--
-- @
-- Wallet (RndState k n)
-- Wallet (SeqState n ShelleyKey)
-- @
data Wallet s = Wallet
    { -- | Unspent tx outputs belonging to this wallet
      utxo :: UTxO

      -- | Header of the latest applied block (current tip)
    , currentTip :: BlockHeader

      -- | Address discovery state
    , getState :: s
    } deriving (Generic, Eq, Show)

instance NFData s => NFData (Wallet s) where
    rnf (Wallet u sl s) =
        deepseq (rnf u) $
        deepseq (rnf sl) $
        deepseq (rnf s)
        ()

instance Buildable s => Buildable (Wallet s) where
    build (Wallet u tip s) = "Wallet s\n"
        <> indentF 4 ("Tip: " <> build tip)
        <> indentF 4 ("UTxO:\n" <> indentF 4 (build u))
        <> indentF 4 (build s)

-- | Delta encoding for 'Wallet'.
data DeltaWallet s = DeltaWallet
    { deltaUTxO :: DeltaUTxO
    , deltaCurrentTip :: Delta.Replace BlockHeader
    , deltaAddressBook :: DeltaAddressBook s
    } deriving (Show)

type DeltaAddressBook s = Delta.Replace s

instance Delta (DeltaWallet s) where
    type Base (DeltaWallet s) = Wallet s
    dw `apply` w = w
        { utxo = deltaUTxO dw `apply` utxo w
        , currentTip = deltaCurrentTip dw `apply` currentTip w
        , getState = deltaAddressBook dw `apply` getState w
        }

{-------------------------------------------------------------------------------
                          Construction & Modification
-------------------------------------------------------------------------------}

-- | Create an empty wallet and apply the given genesis block.
--
-- The wallet tip will be the genesis block header.
initWallet
    :: (IsOurs s Address, IsOurs s RewardAccount)
    => Block
        -- ^ The genesis block
    -> s
        -- ^ Initial address discovery state
    -> ([(Tx, TxMeta)], Wallet s)
initWallet block0 s = (transactions, w1)
  where
    w0 = Wallet mempty undefined s
    (FilteredBlock{transactions}, (_, w1)) = applyBlock block0 w0

-- | Construct a wallet from the exact given state.
--
-- Using this function instead of 'initWallet' and 'applyBlock' allows the
-- wallet invariants to be broken. Therefore it should only be used in the
-- special case of loading wallet checkpoints from the database (where it is
-- assumed a valid wallet was stored into the database).
unsafeInitWallet
    :: UTxO
       -- ^ Unspent tx outputs belonging to this wallet
    -> BlockHeader
    -- ^ Header of the latest applied block (current tip)
    -> s
    -- ^ Address discovery state
    -> Wallet s
unsafeInitWallet = Wallet

-- | Update the address discovery state of a wallet.
updateState
    :: s
    -> Wallet s
    -> Wallet s
updateState s (Wallet u tip _) = Wallet u tip s

{-------------------------------------------------------------------------------
                    Applying Blocks to the wallet state
-------------------------------------------------------------------------------}

-- | Represents the subset of data from a single block that are relevant to a
-- particular wallet, discovered when applying a block to that wallet.
data FilteredBlock = FilteredBlock
    { slot :: !Slot
        -- ^ The slot of this block.
    , transactions :: ![(Tx, TxMeta)]
        -- ^ The set of transactions that affect the wallet,
        -- list in the same order which they appeared in the block.
    , delegations :: ![DelegationCertificate]
        -- ^ Stake delegations made on behalf of the wallet,
        -- listed in the order in which they appear on the chain.
        -- If the list contains more than element, those that appear
        -- later in the list supersede those that appear earlier on.
    } deriving (Generic, Show, Eq)

-- | Apply a single block to a wallet.
--
-- This is the most fundamental way of making a wallet evolve.
--
-- Returns an updated wallet, as well as the address data relevant to the wallet
-- that were discovered while applying the block.
applyBlock
    :: (IsOurs s Address, IsOurs s RewardAccount)
    => Block
    -> Wallet s
    -> (FilteredBlock, (DeltaWallet s, Wallet s))
applyBlock block =
    first fromFiltered . runIdentity . applyBlockData (List $ block :| [])
  where
    fromFiltered [] = FilteredBlock
        { slot = toSlot $ chainPointFromBlockHeader (block ^. #header)
        , transactions = []
        , delegations = []
        }
    fromFiltered (fblock:_) = fblock

-- | Apply multiple blocks in sequence to an existing wallet and
-- return a list of intermediate wallet states.
--
-- If the input blocks are a 'List', then one intermediate wallet
-- state is returned for each block in the list.
-- If the input blocks are a 'Summary', then only one final wallet state
-- is returned.
--
-- More specifically, for an initial wallet state @w0@ and a 'List' of
-- of blocks
--
-- > bs = [b1, b2, …, bn]@
--
-- , the function returns
--
-- @
-- [ (filtered b1, (delta w0 -> w1 , w1 = w0 + b1))
-- , (filtered b2, (delta w1 -> w2 , w2 = w1 + b2))
-- , …
-- , (filtered bn, (delta w(n-1) -> wn, wn = w(n-1)+bn))
-- ]
--
-- Here:
--
-- * @filtered bj@ refers to the set of transactions contained in the
--   block @bj@ that were actually applied to the wallet state.
-- * @wi + bj@ refers to the wallet state obtained after applying
--   the block @bi@ to the wallet @wj@.
-- * delta wi -> wj@ refers to the delta that was applied in order
--   to obtain @wj@ from @wi@.
--
applyBlocks
    :: (IsOurs s Address, IsOurs s RewardAccount, Monad m)
    => BlockData m (Either Address RewardAccount) ChainEvents s
    -> Wallet s
    -> m (NonEmpty ([FilteredBlock], (DeltaWallet s, Wallet s)))
applyBlocks (List (block0 :| blocks)) w0 = pure $
    NE.scanl applyBlock' (first (:[]) $ applyBlock block0 w0) blocks
  where
    applyBlock' (_,(_,w)) block = first (:[]) $ applyBlock block w
applyBlocks summary@(Summary _ _) w =
    (NE.:| []) <$> applyBlockData summary w

-- | Apply multiple blocks in sequence to an existing wallet
-- and return the final wallet state as well as the transactions
-- that were applied.
applyBlockData
    :: (IsOurs s Address, IsOurs s RewardAccount, Monad m)
    => BlockData m (Either Address RewardAccount) ChainEvents s
    -> Wallet s
    -> m ([FilteredBlock], (DeltaWallet s, Wallet s))
applyBlockData blocks (Wallet !u0 _ s0) = do
    (chainEvents, s1) <- discoverFromBlockData blocks s0
    let blockEvents = toAscBlockEvents chainEvents
        applies u blockEvent = applyBlockEventsToUTxO blockEvent s1 u
        (processedBlocks, u1) = mapAccumL' applies u0 blockEvents
        filteredBlocks = map fst processedBlocks
        tip1 = lastHeader blocks
        dtip = Delta.Replace tip1
        ds = Delta.Replace s1
        du = mconcat (reverse $ map snd processedBlocks)
        dw = DeltaWallet
            { deltaUTxO = du , deltaAddressBook = ds, deltaCurrentTip = dtip }
    pure (filteredBlocks, (dw, Wallet u1 tip1 s1))

-- | Strict variant of 'mapAccumL'.
mapAccumL' :: (s -> a -> (o,s)) -> s -> [a] -> ([o],s)
mapAccumL' f = go []
  where
    go os !s0 []     = (reverse os, s0)
    go os !s0 (x:xs) = case f s0 x of
        (!o,!s1) -> go (o:os) s1 xs

-- | BlockData which has been paired with discovery facilities.
data BlockData m addr tx s
    = List (NonEmpty Block)
    | Summary (DiscoverTxs addr tx s) (BlockSummary m addr tx)

-- | First 'BlockHeader' of the blocks represented
-- by 'BlockData'.
firstHeader :: BlockData m addr txs s -> BlockHeader
firstHeader (List xs) = header $ NE.head xs
firstHeader (Summary _ BlockSummary{from}) = from

-- | Last 'BlockHeader' of the blocks represented
-- by 'BlockData'.
lastHeader :: BlockData m addr txs s -> BlockHeader
lastHeader (List xs) = header $ NE.last xs
lastHeader (Summary _ BlockSummary{to}) = to

{-------------------------------------------------------------------------------
                                   Accessors
-------------------------------------------------------------------------------}

-- | Available balance = 'balance' . 'availableUTxO'
availableBalance :: Set Tx -> Wallet s -> TokenBundle
availableBalance pending =
    balance . availableUTxO pending

-- | Total balance = 'balance' . 'totalUTxO' +? rewards
totalBalance
    :: (IsOurs s Address, IsOurs s RewardAccount)
    => Set Tx
    -> Coin
    -> Wallet s
    -> TokenBundle
totalBalance pending rewards wallet@(Wallet _ _ s) =
    balance (totalUTxO pending wallet) `TB.add` rewardsBalance
  where
    rewardsBalance
        | hasPendingWithdrawals = mempty
        | otherwise = TB.fromCoin rewards

    hasPendingWithdrawals =
        any (any (ours s) . Map.keys . withdrawals) pending

-- | Available UTxO = @pending ⋪ utxo@
availableUTxO
    :: Set Tx
    -> Wallet s
    -> UTxO
availableUTxO pending (Wallet u _ _) = u `excluding` used
  where
    used :: Set TxIn
    used = F.foldMap' getUsedTxIn pending

    -- UTxO which have been spent or committed as collateral in a pending
    -- transaction are not available to use in future transactions.
    getUsedTxIn :: Tx -> Set TxIn
    getUsedTxIn tx = Set.fromList $ mconcat
        [ fst <$> tx ^. #resolvedInputs
        , fst <$> tx ^. #resolvedCollateralInputs
        ]

-- | Computes the total 'UTxO' set of a wallet.
--
-- This total 'UTxO' set is a projection of how the wallet's UTxO set would look
-- if all pending transactions were applied successfully.
--
-- >>> totalUTxO pendingTxs wallet
-- >>>     = utxo wallet
-- >>>     − inputs pendingTxs
-- >>>     ∪ change pendingTxs
--
totalUTxO
    :: IsOurs s Address
    => Set Tx
    -> Wallet s
    -> UTxO
totalUTxO pending (Wallet u _ s) =
    (u `excluding` spent) <> changeUTxO pending s
  where
    spent :: Set TxIn
    spent = F.foldMap' getSpentTxIn pending

    -- NOTE: In 'availableUTxO', we exclude UTxO committed as collateral, but
    -- not here. Since the total UTxO set is indented to be a projection of how
    -- the UTxO set would look if all pending transactions are applied
    -- successfully: if a transaction is applied successfully, then its
    -- collateral inputs cannot be consumed.
    getSpentTxIn :: Tx -> Set TxIn
    getSpentTxIn tx = Set.fromList $ fst <$> tx ^. #resolvedInputs

-- | Retrieve the change 'UTxO' contained in a set of pending transactions.
--
-- We perform /some/ address discovery within the list of pending addresses,
-- but we do not store the result.
-- Instead, we essentially assume that the address discovery state @s@ contains
-- enough information to collect the change addresses in the pending
-- transactions.
--
-- Caveats:
-- * Rollbacks can invalidate this assumption. 🙈
-- * The order of pending transactions is based on transaction hashes,
--   and typically does not agree with the order in which we have submitted
--   them onto the chain. Hence, the address discovery phase is not really
--   very effective.
--   TODO: Add slot to 'Tx' and sort the pending set by slot.
changeUTxO
    :: IsOurs s Address
    => Set Tx
    -> s
    -> UTxO
changeUTxO pending = evalState $
    mconcat <$> mapM
        (UTxO.filterByAddressM isOursState . utxoFromTx)
        (Set.toList pending)

{-------------------------------------------------------------------------------
                                UTxO operations
-------------------------------------------------------------------------------}

-- | Applies a transaction to a UTxO, moving it from one state to another.
--
-- When applying a transaction to a UTxO:
--   1. We need to remove any unspents that have been spent in the transaction.
--   2. Add any unspents that we've received via the transaction.
--      In this function, we assume that all outputs belong to us.
--
-- Properties:
--
-- @
-- balance (applyTxToUTxO tx u) = balance u
--                              + balance (utxoFromTx tx)
--                              - balance (u `restrictedBy` inputs tx)
-- unUTxO (applyTxToUTxO tx u) = unUTxO u
--     `Map.union` unUTxO (utxoFromTx tx)
--     `Map.difference` unUTxO (u `restrictedBy` inputs tx)
-- applyTxToUTxO tx u = spend tx u <> utxoFromTx tx
-- applyTxToUTxO tx u = spend tx (u <> utxoFromTx tx)
-- @
applyTxToUTxO
    :: Tx
    -> UTxO
    -> UTxO
applyTxToUTxO tx !u = spendTx tx u <> utxoFromTx tx

-- | Remove unspent outputs that are consumed by the given transaction.
--
-- @
-- spendTx tx u `isSubsetOf` u
-- balance (spendTx tx u) <= balance u
-- balance (spendTx tx u) = balance u - balance (u `restrictedBy` inputs tx)
-- spendTx tx u = u `excluding` inputs tx
-- spendTx tx (filterByAddress f u) = filterByAddress f (spendTx tx u)
-- spendTx tx (u <> utxoFromTx tx) = spendTx tx u <> utxoFromTx tx
-- @
spendTx :: Tx -> UTxO -> UTxO
spendTx tx = snd . spendTxD tx

-- | Remove unspent outputs that are consumed by the given transaction.
spendTxD :: Tx -> UTxO -> (DeltaUTxO, UTxO)
spendTxD tx !u =
    u `excludingD` Set.fromList inputsToExclude
  where
    inputsToExclude =
        if txScriptInvalid tx
        then collateralInputs tx
        else inputs tx

-- | Generates a UTxO set from a transaction.
--
-- The generated UTxO set corresponds to the value provided by the transaction.
--
-- It is important for transaction outputs to be ordered correctly, as their
-- indices within this ordering will determine how they are referenced as
-- transaction inputs in subsequent blocks.
--
-- Assuming the transaction is not marked as having an invalid script, the
-- following properties should hold:
--
-- prop> balance (utxoFromTx tx) == foldMap tokens (outputs tx)
-- prop> size    (utxoFromTx tx) == length         (outputs tx)
-- prop> toList  (utxoFromTx tx) == toList         (outputs tx)
--
-- However, if the transaction is marked as having an invalid script, then the
-- following properties should hold:
--
-- prop> balance (utxoFromTx tx) == foldMap tokens (collateralOutput tx)
-- prop> size    (utxoFromTx tx) == length         (collateralOutput tx)
-- prop> toList  (utxoFromTx tx) == toList         (collateralOutput tx)
--
utxoFromTx :: Tx -> UTxO
utxoFromTx tx =
    if txScriptInvalid tx
    then utxoFromTxCollateralOutputs tx
    else utxoFromTxOutputs tx

-- | Generates a UTxO set from the ordinary outputs of a transaction.
--
-- This function ignores the transaction's script validity.
--
utxoFromTxOutputs :: Tx -> UTxO
utxoFromTxOutputs Tx {txId, outputs} =
    UTxO $ Map.fromList $ zip (TxIn txId <$> [0..]) outputs

-- | Generates a UTxO set from the collateral outputs of a transaction.
--
-- This function ignores the transaction's script validity.
--
utxoFromTxCollateralOutputs :: Tx -> UTxO
utxoFromTxCollateralOutputs Tx {txId, outputs, collateralOutput} =
    UTxO $ Map.fromList $ F.toList $ (TxIn txId index,) <$> collateralOutput
  where
    -- To reference a collateral output within transaction t, we specify an
    -- output index that is equal to the number of ordinary outputs within t.
    --
    -- See definition of function "collOuts" within "Formal Specification of
    -- the Cardano Ledger for the Babbage era".
    --
    -- https://hydra.iohk.io/build/14336206/download/1/babbage-changes.pdf
    --
    index :: Word32
    index = fromIntegral (length outputs)

{-------------------------------------------------------------------------------
                        Address ownership and discovery
-------------------------------------------------------------------------------}

-- | Perform address discovery on a 'Block' by going through all transactions
-- and delegation certificates in the block.
discoverAddressesBlock
    :: (IsOurs s Address, IsOurs s RewardAccount)
    => Block -> s -> (DeltaAddressBook s, s)
discoverAddressesBlock block s0 = (Delta.Replace s2, s2)
  where
    -- NOTE: Order in which we perform discovery is important.
    s1 = L.foldl' discoverCert s0 (block ^. #delegations)
    s2 = L.foldl' discoverTx   s1 (block ^. #transactions)

    discoverCert s cert = updateOurs s (dlgCertAccount cert)

    -- NOTE: Only outputs and withdrawals can potentially
    -- result in the extension of the address pool and
    -- the learning of new addresses.
    --
    -- Inputs and collateral are forced to use existing addresses.
    discoverTx s tx = discoverWithdrawals (discoverOutputs s tx) tx
    discoverOutputs s tx =
        L.foldl' (\s_ out -> updateOurs s_ (out ^. #address)) s (tx ^. #outputs)
    discoverWithdrawals s tx =
        L.foldl' updateOurs s $ Map.keys (tx ^. #withdrawals)

-- | Perform address and transaction discovery on 'BlockData',
discoverFromBlockData
    :: (IsOurs s Address, IsOurs s RewardAccount, Monad m)
    => BlockData m (Either Address RewardAccount) ChainEvents s
    -> s
    -> m (ChainEvents, s)
discoverFromBlockData (List blocks) !s0 =
    pure (fromBlockEvents . map fromEntireBlock $ NE.toList blocks , s1)
  where
    s1 = L.foldl' (\s bl -> snd $ discoverAddressesBlock bl s) s0 $ NE.toList blocks
discoverFromBlockData (Summary dis summary) !s0 =
    discoverTxs dis (summary ^. #query) s0

-- | Indicates whether an address is known to be ours, without updating the
-- address discovery state.
ours :: IsOurs s addr => s -> addr -> Bool
ours s x = isJust . fst $ isOurs x s

-- | Add an address to the address discovery state, iff it belongs to us.
updateOurs :: IsOurs s addr => s -> addr -> s
updateOurs s x = snd $ isOurs x s

-- | Perform stateful address discovery, and return whether the given address
-- belongs to us.
isOursState :: IsOurs s addr => addr -> State s Bool
isOursState x = isJust <$> state (isOurs x)

{-------------------------------------------------------------------------------
                              Modification of UTxO
-------------------------------------------------------------------------------}

-- | Apply a 'Block' to the 'UTxO'.
--
-- Here, we assume that address discovery has already been performed and
-- that the address discovery state @s@ identifies all our output addresses
-- in the given 'Block'.
--
-- A 'FilteredBlock' is returned in addition to the new 'UTxO' set.
-- This 'FilteredBlock' includes those transactions and delegations
-- that are in the given 'Block' and that are also relevant to the wallet,
-- i.e. they have
--
-- * Outputs with known addresses
-- * Inputs referring to known outputs of previous transactions.
--
-- In practice, most transactions that are relevant to the wallet
-- have at least one output that belongs to the wallet:
-- either because we have received funds from another party,
-- or because the wallet has created a change output when sending
-- funds to another party.
-- But some transactions may actually have no relevant outputs whatsoever
-- and be only linked to the wallet by their inputs.
--
-- As inputs are given as references to outputs (no address, no coin value),
-- we have to traverse all transactions in the block in order to
-- discover the outputs that belong to us and be able to infer that the
-- corresponding inputs belong to us as well.
applyBlockEventsToUTxO
    :: (IsOurs s Address, IsOurs s RewardAccount)
    => BlockEvents
    -> s
    -> UTxO
    -> ((FilteredBlock, DeltaUTxO), UTxO)
applyBlockEventsToUTxO BlockEvents{slot,blockHeight,transactions,delegations} s u0 =
    ((fblock, du1), u1)
  where
    fblock = FilteredBlock
      { slot
      , transactions = reverse rtxs1
      , delegations = filter (ours s . dlgCertAccount) $ toList delegations
      }
    (rtxs1, du1, u1) = L.foldl' applyOurTx (mempty, mempty, u0)
        $ toList transactions

    applyOurTx
        :: ([(Tx, TxMeta)], DeltaUTxO, UTxO)
        -> Tx
        -> ([(Tx, TxMeta)], DeltaUTxO, UTxO)
    applyOurTx (!txs, !du, !u) !tx =
        case applyOurTxToUTxO slot blockHeight s tx u of
            Nothing -> (txs, du, u)
            Just (tx', du', u') -> (tx' : txs, du' <> du, u')

-- | Apply the given transaction to the 'UTxO'.
-- Return 'Just' if and only if the transaction is relevant to the wallet
-- (changes the 'UTxO' set or makes a withdrawal).
--
-- It satisfies the following property:
--
-- > isJust (applyOurTxToUTxO slot bh state1 tx u) = b
-- >   where (b, state1) = runState (isOurTx tx u) state0
applyOurTxToUTxO
    :: (IsOurs s Address, IsOurs s RewardAccount)
    => Slot
    -> Quantity "block" Word32
    -> s
    -> Tx
    -> UTxO
    -> Maybe ((Tx, TxMeta), DeltaUTxO, UTxO)
applyOurTxToUTxO !slot !blockHeight !s !tx !u0 =
    if hasKnownWithdrawal || not isUnchangedUTxO
        then Just ((tx {fee = actualFee dir}, txmeta), du, u)
        else Nothing
  where
    -- The next UTxO state (apply a state transition) (e.g. remove
    -- transaction outputs we've spent)
    (du, u) = (du21 <> du10, u2)

    -- Note [Naming of Deltas]
    -- The identifiers for delta encodings carry two indices
    -- which indicate the "to" and "from" value of the delta.
    -- For example, the delta du10 maps the value u0 to the value u1,
    -- and the delta du21 maps the value u1 to the value u2.
    -- In general, the naming convention is  ui = duij `apply` uj
    (du10, u1)   = spendTxD tx u0
    receivedUTxO = UTxO.filterByAddress (ours s) (utxoFromTx tx)
    (du21, u2)   = receiveD u1 receivedUTxO

    -- NOTE: Performance.
    -- This function is part of a tight loop that inspects all transactions
    -- (> 30M Txs as of Feb 2022).
    -- Thus, we make a small performance optimization here.
    -- Specifically, we want to reject a transaction as soon as possible
    -- if it does not change the 'UTxO' set. The test
    isUnchangedUTxO = UTxO.null receivedUTxO && mempty == du10
    -- allocates slightly fewer new Set/Map than the definition
    --   isUnchangedUTxO =  mempty == du

    ourWithdrawalSum = ourWithdrawalSumFromTx s tx

    -- Balance of the UTxO that we received and that we spent
    received = balance receivedUTxO
    spent = balance (u0 `UTxO.restrictedBy` UTxO.excluded du10)
        `TB.add` TB.fromCoin ourWithdrawalSum

    adaSpent = TB.getCoin spent
    adaReceived = TB.getCoin received
    dir = if adaSpent > adaReceived then Outgoing else Incoming
    amount = distance adaSpent adaReceived

    -- Transaction metadata computed from the above information
    txmeta = TxMeta
        { status = InLedger
        , direction = dir
        , slotNo = pseudoSlotNo slot
        , blockHeight
        , amount = amount
        , expiry = Nothing
        }
      where
        pseudoSlotNo Origin = 0
        pseudoSlotNo (At sl) = sl

    hasKnownWithdrawal = ourWithdrawalSum /= mempty

    -- NOTE 1: The only case where fees can be 'Nothing' is when dealing with
    -- a Byron transaction. In which case fees can actually be calculated as
    -- the delta between inputs and outputs.
    --
    -- NOTE 2: We do not have in practice the actual input amounts, yet we
    -- do make the assumption that if one input is ours, then all inputs are
    -- necessarily ours and therefore, known as part of our current UTxO.
    actualFee direction = case (tx ^. #fee, direction) of
        (Just x, Outgoing) ->
            -- Shelley and beyond:
            Just x
        (Nothing, Outgoing) ->
            -- Byron:
            let totalOut = F.fold (TxOut.coin <$> outputs tx)
                totalIn = TB.getCoin spent
            in
            Just $ distance totalIn totalOut
        (_, Incoming) ->
            Nothing

ourWithdrawalSumFromTx
    :: IsOurs s RewardAccount
    => s -> Tx -> Coin
ourWithdrawalSumFromTx s tx
    -- If a transaction has failed script validation, then the ledger rules
    -- require that applying the transaction shall have no effect other than
    -- to fully spend the collateral inputs included within that transaction.
    --
    -- Therefore, any reward withdrawals included in such a transaction should
    -- also have no effect.
    --
    | txScriptInvalid tx = Coin 0
    | otherwise = Map.foldlWithKey' add (Coin 0) (tx ^. #withdrawals)
  where
    add total account coin
        | ours s account = total <> coin
        | otherwise      = total
