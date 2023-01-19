{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{- HLINT ignore "Move brackets to avoid $" -}

module Cardano.Wallet.Primitive.ModelSpec
    ( spec
    ) where

import Prelude

import Algebra.PartialOrd
    ( PartialOrd (..) )
import Cardano.Wallet.DummyTarget.Primitive.Types
    ( block0 )
import Cardano.Wallet.Gen
    ( genSlot )
import Cardano.Wallet.Primitive.AddressDerivation
    ( DerivationIndex (..) )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( DiscoverTxs (..), IsOurs (..), MaybeLight (..) )
import Cardano.Wallet.Primitive.BlockSummary
    ( ChainEvents, summarizeOnTxOut )
import Cardano.Wallet.Primitive.Model
    ( BlockData (..)
    , DeltaWallet
    , FilteredBlock
    , Wallet
    , applyBlock
    , applyBlockData
    , applyBlocks
    , applyOurTxToUTxO
    , applyTxToUTxO
    , availableBalance
    , availableUTxO
    , changeUTxO
    , currentTip
    , discoverAddressesBlock
    , discoverFromBlockData
    , getState
    , initWallet
    , spendTx
    , totalBalance
    , totalUTxO
    , unsafeInitWallet
    , updateOurs
    , utxo
    , utxoFromTx
    , utxoFromTxCollateralOutputs
    , utxoFromTxOutputs
    )
import Cardano.Wallet.Primitive.Slotting.Legacy
    ( flatSlot )
import Cardano.Wallet.Primitive.Types
    ( Block (..)
    , BlockHeader (..)
    , EpochLength (..)
    , Slot
    , SlotId (..)
    , SlotNo (..)
    )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..) )
import Cardano.Wallet.Primitive.Types.Address.Gen
    ( Parity (..), addressParity, genAddress )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.Coin.Gen
    ( genCoin, shrinkCoin )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Cardano.Wallet.Primitive.Types.RewardAccount
    ( RewardAccount (..) )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle )
import Cardano.Wallet.Primitive.Types.Tx
    ( Direction (..)
    , Tx (..)
    , TxMeta (direction)
    , TxScriptValidity (..)
    , collateralInputs
    , inputs
    , txIns
    , txScriptInvalid
    )
import Cardano.Wallet.Primitive.Types.Tx.Gen
    ( genTx, genTxScriptValidity, shrinkTx )
import Cardano.Wallet.Primitive.Types.Tx.TxIn
    ( TxIn (..) )
import Cardano.Wallet.Primitive.Types.Tx.TxIn.Gen
    ( genTxIn, shrinkTxIn )
import Cardano.Wallet.Primitive.Types.Tx.TxOut
    ( TxOut (..) )
import Cardano.Wallet.Primitive.Types.Tx.TxOut.Gen
    ( genTxOut, shrinkTxOut )
import Cardano.Wallet.Primitive.Types.Tx.TxSeq
    ( TxSeq )
import Cardano.Wallet.Primitive.Types.Tx.TxSeq.Gen
    ( ShrinkableTxSeq, genTxSeq, getTxSeq, shrinkTxSeq )
import Cardano.Wallet.Primitive.Types.UTxO
    ( UTxO (..), balance, dom, excluding, filterByAddress, restrictedTo )
import Cardano.Wallet.Primitive.Types.UTxO.Gen
    ( genUTxO, shrinkUTxO )
import Cardano.Wallet.Util
    ( ShowFmt (..), invariant )
import Control.Applicative
    ( ZipList (..) )
import Control.DeepSeq
    ( NFData (..) )
import Control.Monad
    ( filterM, foldM, guard )
import Control.Monad.Trans.State.Strict
    ( State, evalState, execState, runState, state )
import Data.Delta
    ( apply )
import Data.Function
    ( (&) )
import Data.Functor
    ( ($>) )
import Data.Functor.Identity
    ( Identity (..) )
import Data.Generics.Internal.VL.Lens
    ( over, set, view, (^.) )
import Data.Generics.Labels
    ()
import Data.List
    ( elemIndex )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Maybe
    ( catMaybes, isJust )
import Data.Quantity
    ( Quantity (..) )
import Data.Set
    ( Set, (\\) )
import Data.Traversable
    ( for )
import Data.Word
    ( Word32, Word64 )
import Fmt
    ( Buildable, blockListF, pretty )
import Generics.SOP
    ( NP (..) )
import GHC.Generics
    ( Generic )
import Test.Hspec
    ( Spec, describe, it, shouldSatisfy )
import Test.Hspec.Extra
    ( parallel )
import Test.QuickCheck
    ( Arbitrary (..)
    , CoArbitrary (..)
    , Fun (..)
    , Function (..)
    , Gen
    , Positive (..)
    , Property
    , Testable
    , applyFun
    , arbitrarySizedBoundedIntegral
    , checkCoverage
    , choose
    , classify
    , conjoin
    , counterexample
    , cover
    , elements
    , forAllShrink
    , frequency
    , genericShrink
    , label
    , liftArbitrary
    , listOf
    , oneof
    , property
    , scale
    , shrinkIntegral
    , shrinkList
    , shrinkMap
    , shrinkMapBy
    , shrinkNothing
    , vector
    , withMaxSuccess
    , (.&&.)
    , (===)
    )
import Test.QuickCheck.Extra
    ( Pretty (..)
    , chooseNatural
    , genericRoundRobinShrink
    , labelInterval
    , report
    , verify
    , (<:>)
    , (<@>)
    )
import Test.QuickCheck.Instances.ByteString
    ()
import Test.Utils.Pretty
    ( (====) )

import qualified Cardano.Wallet.Primitive.Types.Coin as Coin
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.Tx.TxOut as TxOut
import qualified Cardano.Wallet.Primitive.Types.Tx.TxSeq as TxSeq
import qualified Cardano.Wallet.Primitive.Types.UTxO as UTxO
import qualified Data.ByteString as BS
import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T

spec :: Spec
spec = do
    parallel $ describe "Buildable instances examples" $ do
        let block = blockchain !! 1
        let utxo = utxoFromTx $ head $ transactions block
        it (show $ ShowFmt utxo) True

    parallel $ describe "Compare Wallet impl. with Specification" $ do
        it "Lemma 3.2 - dom u ⋪ updateUTxO b u = new b"
            (checkCoverage prop_3_2)

        it "applyBlock matches the basic model from the specification"
            (checkCoverage prop_applyBlockBasic)

    parallel $ describe "Extra Properties" $ do
        it "Incoming transactions have output addresses that belong to the wallet"
            (property prop_applyBlockTxHistoryIncoming)

        it "applyBlock moves the current tip forward"
            (property prop_applyBlockCurrentTip)

        it "Wallet starts with a block height of 0"
            (property prop_initialBlockHeight)

        it "applyBlocks increases the blockHeight"
            (property prop_applyBlocksBlockHeight)

        it "only counts rewards once."
            (property prop_countRewardsOnce)

        describe "coverage" $ do
            it "utxo and tx generators have expected coverage"
                (property prop_tx_utxo_coverage)

        describe "applyTxToUTxO" $ do
            it "has expected balance"
                (property prop_applyTxToUTxO_balance)
            it "has expected entries"
                (property prop_applyTxToUTxO_entries)
            it "consumes inputs"
                (property unit_applyTxToUTxO_spends_input)
            it "produces expected UTxO set when script validity is valid"
                (property unit_applyTxToUTxO_scriptValidity_Valid)
            it "produces expected UTxO set when script validity is invalid"
                (property unit_applyTxToUTxO_scriptValidity_Invalid)
            it "produces expected UTxO set when script validity is unknown"
                (property unit_applyTxToUTxO_scriptValidity_Unknown)
            it "applyTxToUTxO then filterByAddress"
                (property prop_filterByAddress_balance_applyTxToUTxO)
            it "spendTx/applyTxToUTxO/utxoFromTx"
                (property prop_applyTxToUTxO_spendTx_utxoFromTx)

        describe "utxoFromTx" $ do
            it "has expected balance"
                (property prop_utxoFromTx_balance)
            it "has expected size"
                (property prop_utxoFromTx_size)
            it "has expected values"
                (property prop_utxoFromTx_values)
            it "prop_utxoFromTx_disjoint"
                (property prop_utxoFromTx_disjoint)
            it "prop_utxoFromTx_union"
                (property prop_utxoFromTx_union)

        describe "spendTx" $ do
            it "is subset of UTxO"
                (property prop_spendTx_isSubset)
            it "balance is <= balance of UTxO"
                (property prop_spendTx_balance_inequality)
            it "has expected balance"
                (property prop_spendTx_balance)
            it "definition"
                (property prop_spendTx)
            it "commutative with filterByAddress"
                (property prop_spendTx_filterByAddress)
            it "spendTx/utxoFromTx"
                (property prop_spendTx_utxoFromTx)

    parallel $ describe "DeltaWallet" $ do
        it "applyBlock gives deltas that match final wallet state" $
            property prop_applyBlock_DeltaWallet

    parallel $ describe "Available UTxO" $ do
        it "prop_availableUTxO_isSubmap" $
            property prop_availableUTxO_isSubmap
        it "prop_availableUTxO_notMember" $
            property prop_availableUTxO_notMember
        it "prop_availableUTxO_withoutKeys" $
            property prop_availableUTxO_withoutKeys
        it "prop_availableUTxO_availableBalance" $
            property prop_availableUTxO_availableBalance

    parallel $ describe "Change UTxO" $ do
        it "prop_changeUTxO" $
            property prop_changeUTxO

    parallel $ describe "Total UTxO" $ do
        it "prop_totalUTxO_pendingChangeIncluded" $
            property prop_totalUTxO_pendingChangeIncluded
        it "prop_totalUTxO_pendingCollateralInputsIncluded" $
            property prop_totalUTxO_pendingCollateralInputsIncluded
        it "prop_totalUTxO_pendingInputsExcluded" $
            property prop_totalUTxO_pendingInputsExcluded

    parallel $ describe "Applying transactions to UTxO sets" $ do
        it "prop_applyOurTxToUTxO_allOurs" $
            property prop_applyOurTxToUTxO_allOurs
        it "prop_applyOurTxToUTxO_someOurs" $
            property prop_applyOurTxToUTxO_someOurs

    parallel $ describe "Address discovery" $ do
        it "discoverAddressesBlock ~ isOurTx" $
            property prop_discoverAddressesBlock

    parallel $ describe "Light-mode" $ do
        it "discovery on blocks = discovery on summary" $
            property prop_discoverFromBlockData

    parallel $ describe "applyBlocks" $ do

        describe "filteredTxs" $ do
            it "prop_applyBlocks_filteredTxs_someOurs" $
                prop_applyBlocks_filteredTxs_someOurs
                    & property
            it "prop_applyBlocks_filteredTxs_allOurs" $
                prop_applyBlocks_filteredTxs_allOurs
                    & property
            it "prop_applyBlocks_filteredTxs_noneOurs" $
                prop_applyBlocks_filteredTxs_noneOurs
                    & property

        describe "lastUTxO" $ do
            it "prop_applyBlocks_lastUTxO_someOurs" $
                prop_applyBlocks_lastUTxO_someOurs
                    & property
            it "prop_applyBlocks_lastUTxO_allOurs" $
                prop_applyBlocks_lastUTxO_allOurs
                    & property
            it "prop_applyBlocks_lastUTxO_noneOurs" $
                prop_applyBlocks_lastUTxO_noneOurs
                    & property

{-------------------------------------------------------------------------------
                                Properties
-------------------------------------------------------------------------------}
applyBlocksOld
    :: (IsOurs s Address, IsOurs s RewardAccount)
    => NonEmpty Block
    -> Wallet s
    -> ([FilteredBlock], (DeltaWallet s, Wallet s))
applyBlocksOld bs = runIdentity . applyBlockData (List bs)

prop_3_2
    :: ApplyBlock
    -> Property
prop_3_2 (ApplyBlock s utxo block) =
    cover 75 cond "utxo ≠ ∅ " (property prop)
  where
    cond = utxo /= mempty
    prop =
        ShowFmt (updateUTxO' block utxo `excluding` dom utxo)
            ===
        ShowFmt (new block)
    new b = flip evalState s $ do
        let txs = Set.fromList $ transactions b
        utxo' <- (foldMap utxoFromTx txs `restrictedTo`) . Set.map snd
            <$> state (txOutsOurs txs)
        return $ utxo' `excluding` txIns txs
    updateUTxO' b u = evalState (updateUTxO b u) s

prop_applyBlockBasic
    :: WalletState
    -> Property
prop_applyBlockBasic s =
    cover 90 cond0 "ours ≠ ∅ " $
    cover 90 cond1 "addresses \\ ours ≠ ∅ " $
        property prop
  where
    cond0 = not $ null $ ourAddresses s
    cond1 = not $ null $ (Set.fromList addresses) \\ (ourAddresses s)
    prop =
        let
            (_, cp0) = initWallet @_ block0 s
            wallet = foldl (\cp b -> snd . snd $ applyBlock b cp) cp0 blockchain
            utxo = totalUTxO mempty wallet
            utxo' = evalState (foldM (flip updateUTxO) mempty blockchain) s
        in
            (ShowFmt utxo === ShowFmt utxo') .&&.
            (availableBalance mempty wallet === balance utxo') .&&.
            (totalBalance mempty (Coin 0) wallet === balance utxo')

prop_applyBlock_DeltaWallet :: ApplyBlock -> Property
prop_applyBlock_DeltaWallet (ApplyBlock s utxo0 block) =
    counterexample ("DeltaUTxO:\n" <> show dw)
    $ w1 === apply dw w0
  where
    w0 = unsafeInitWallet utxo0 (view #header block0) s
    (_, (dw, w1)) = applyBlock block w0

-- Each transaction must have at least one output belonging to us
prop_applyBlockTxHistoryIncoming :: WalletState -> Property
prop_applyBlockTxHistoryIncoming s =
    property (outs (filter isIncoming txs) `overlaps` ourAddresses s')
  where
    (_, cp0) = initWallet @_ block0 s
    bs = NE.fromList blockchain
    (filteredBlocks, (_, cp)) = applyBlocksOld bs cp0
    txs = foldMap (view #transactions) filteredBlocks
    s' = getState cp
    isIncoming (_, m) = direction m == Incoming
    outs = Set.fromList . concatMap (map address . outputs . fst)
    overlaps a b
        | a == mempty && b == mempty = True
        | otherwise = not (Set.disjoint a b)

-- | Apply blocks move current tip forward
prop_applyBlockCurrentTip :: ApplyBlock -> Property
prop_applyBlockCurrentTip (ApplyBlock s _ b) =
    property $ currentTip wallet' > currentTip wallet
  where
    (_, wallet) = initWallet @_ block0 s
    wallet' = snd $ snd $ applyBlock b wallet

-- | applyBlocks increases the block height.
prop_applyBlocksBlockHeight :: WalletState -> Positive Int -> Property
prop_applyBlocksBlockHeight s (Positive n) =
    counterexample (show (wallet, wallet')) $
    bh wallet' - bh wallet `shouldSatisfy` (> 0)
  where
    bs = NE.fromList (take n blockchain)
    (_, wallet) = initWallet block0 s
    (_,(_,wallet')) = applyBlocksOld bs wallet
    bh = unQuantity . blockHeight . currentTip
    unQuantity (Quantity a) = a

prop_initialBlockHeight :: WalletState -> Property
prop_initialBlockHeight s =
    property $ blockHeight (currentTip wallet) === Quantity 0
  where
    (_, wallet) = initWallet block0 s

-- Rationale here is that pending transactions contributes towards the total
-- balance but also _cost_ something as fee.
--
-- In particular, this tests that the reward amount is correctly reported in the
-- context of withdrawals. Before this property was introduced, we would simply
-- add the wallet's reward to the total balance every time so when a withdrawal
-- was pending, the rewards would be counted almost twice (once as raw rewards,
-- and once as part of change output on the pending withdrawal!).
prop_countRewardsOnce :: WithPending WalletState -> Property
prop_countRewardsOnce (WithPending wallet pending rewards)
    = withMaxSuccess 1000
    $ counterexample ("Total UTxO:\n" <> pretty' (totalUTxO pending wallet))
    $ counterexample ("Available UTxO:\n" <> pretty' (availableUTxO pending wallet))
    $ counterexample ("Pending Transactions:\n" <> pretty' (blockListF pending))
    $ counterexample ("Pending balance:            " <> show pendingBalance)
    $ counterexample ("Total Rewards:              " <> show (unCoin rewards))
    $ counterexample ("Total Balance (w/ pending): " <> show totalWithPending)
    $ counterexample ("Total Balance (no pending): " <> show totalWithoutPending)
    $ classify hasPending "has pending"
    $ classify hasWithdrawals "has withdrawals"
    $ if hasPending
      then property (totalWithPending < totalWithoutPending)
      else property (totalWithPending == totalWithoutPending)
  where
    pendingBalance =
        sum $ (unCoin . TxOut.coin) <$> concatMap outputs (Set.elems pending)
    totalWithPending =
        TokenBundle.getCoin $ totalBalance pending rewards wallet
    totalWithoutPending =
        TokenBundle.getCoin $ totalBalance Set.empty rewards wallet

    hasPending =
        not $ Set.null pending
    hasWithdrawals =
        not $ null $ Set.filter (not . null . withdrawals) pending

    pretty' :: Buildable a => a -> String
    pretty' = T.unpack . pretty

--------------------------------------------------------------------------------
-- Available UTxO properties
--------------------------------------------------------------------------------

allInputsOfTxs :: Set Tx -> Set TxIn
allInputsOfTxs = F.foldMap allInputsOfTx
  where
    allInputsOfTx :: Tx -> Set TxIn
    allInputsOfTx tx = Set.fromList $ mconcat
        [ fst <$> resolvedInputs tx
        , fst <$> resolvedCollateralInputs tx
        ]

prop_availableUTxO_isSubmap :: Property
prop_availableUTxO_isSubmap =
    prop_availableUTxO $ \_pendingTxs wallet result ->
    unUTxO result `Map.isSubmapOf` unUTxO (utxo wallet)

prop_availableUTxO_notMember :: Property
prop_availableUTxO_notMember =
    prop_availableUTxO $ \pendingTxs _wallet result ->
    all (`Map.notMember` unUTxO result)
        (allInputsOfTxs pendingTxs)

prop_availableUTxO_withoutKeys :: Property
prop_availableUTxO_withoutKeys =
    prop_availableUTxO $ \pendingTxs wallet result ->
    unUTxO (utxo wallet) `Map.withoutKeys` allInputsOfTxs pendingTxs
        === unUTxO result

prop_availableUTxO_availableBalance :: Property
prop_availableUTxO_availableBalance =
    prop_availableUTxO $ \pendingTxs wallet result ->
    availableBalance pendingTxs wallet
        === F.foldMap (view #tokens) (unUTxO result)

prop_availableUTxO
    :: Testable prop
    => (Set Tx -> Wallet s -> UTxO -> prop)
    -> Property
prop_availableUTxO makeProperty =
    forAllShrink (scale (* 4) genUTxO) shrinkUTxO
        $ \utxo ->
    forAllShrink (scale (`div` 2) $ listOf genTx) (shrinkList shrinkTx)
        $ \pendingTxs ->
    inner utxo pendingTxs
  where
    inner utxo pendingTxs =
        cover 5 (result /= mempty && result == utxo)
            "result /= mempty && result == utxo" $
        cover 5 (result /= mempty && result /= utxo)
            "result /= mempty && result /= utxo" $
        cover 5 (balance result /= TokenBundle.empty)
            "balance result /= TokenBundle.empty" $
        property $ makeProperty pendingTxSet wallet result
      where
        pendingTxSet = Set.fromList pendingTxs
        wallet = walletFromUTxO utxo
        result = availableUTxO pendingTxSet wallet

    -- Creates a wallet object from a UTxO set, and asserts that the other
    -- parts of the wallet state are not used in any way.
    --
    walletFromUTxO :: UTxO -> Wallet s
    walletFromUTxO utxo = unsafeInitWallet utxo
        (shouldNotEvaluate "currentTip")
        (shouldNotEvaluate "addressDiscoveryState")
      where
        shouldNotEvaluate :: String -> a
        shouldNotEvaluate fieldName = error $ unwords
            [fieldName, "was unexpectedly evaluated"]

--------------------------------------------------------------------------------
-- Change UTxO properties
--------------------------------------------------------------------------------

-- | Tests the 'changeUTxO' function with custom filter conditions to identify
--   addresses that belong to the wallet.
--
-- This test divides all addresses into two disjoint sets:
--
--    - even-parity addresses:
--      addresses with a pop count (Hamming weight) that is even.
--
--    - odd-parity addresses:
--      addresses with a pop count (Hamming weight) that is odd.
--
-- The choice to categorize addresses by parity has two advantages:
--
--    - The parity of an address can be determined by a pure function.
--
--    - Addresses with even parity and odd parity are equally frequent.
--
-- A custom 'IsOurs' instance is used to categorize output addresses according
-- to their parity.
--
-- Since a given address can either be even, or odd, but not both, it follows
-- that the following results must be disjoint:
--
--    - 'changeUTxO pendingTxs (IsOursIf (== Even) . addressParity)'
--    - 'changeUTxO pendingTxs (IsOursIf (==  Odd) . addressParity)'
--
-- This test applies the above filter conditions to 'changeUTxO' and verifies
-- that all entries match the condition that was provided.
--
prop_changeUTxO :: Property
prop_changeUTxO =
    forAllShrink (scale (`div` 4) $ listOf genTx) (shrinkList shrinkTx)
        prop_changeUTxO_inner

prop_changeUTxO_inner :: [Tx] -> Property
prop_changeUTxO_inner pendingTxs
    = checkCoverage
    $ cover 50 (not (UTxO.null utxoEven) && not (UTxO.null utxoOdd))
        "UTxO sets not null"
    $ cover 10
        (all txScriptInvalid pendingTxs)
        "all txScriptInvalid pendingTxs"
    $ cover 10
        (not (any txScriptInvalid pendingTxs))
        "not (any txScriptInvalid pendingTxs)"
    $ cover 10
        (any txScriptInvalid pendingTxs && not (all txScriptInvalid pendingTxs))
        "any txScriptInvalid pendingTxs && not (all txScriptInvalid pendingTxs)"
    $ conjoin
        [ prop_parityEven
        , prop_parityOdd
        , prop_disjoint
        , prop_complete
        , prop_everything
        ]
    & report
        (UTxO.size utxoAll)
        "UTxO.size utxoAll"
    & report
        (F.sum (F.length . view #outputs <$> pendingTxs))
        "F.sum (F.length . view #outputs <$> pendingTxs)"
  where
    -- Verify that all addresses in the even-parity UTxO set have even parity.
    prop_parityEven = counterexample "prop_parityEven" $
        F.all ((== Even) . txOutParity) (unUTxO utxoEven)

    -- Verify that all addresses in the odd-parity UTxO set have odd parity.
    prop_parityOdd = counterexample "prop_parityOdd" $
        F.all ((== Odd) . txOutParity) (unUTxO utxoOdd)

    -- Verify that the even-parity and odd-parity UTxO sets are disjoint.
    prop_disjoint = counterexample "prop_disjoint" $
        Map.null $ Map.intersection (unUTxO utxoEven) (unUTxO utxoOdd)

    -- Verify that the even-parity and odd-parity UTxO sets are complete.
    prop_complete = counterexample "prop_complete" $
        Map.union (unUTxO utxoEven) (unUTxO utxoOdd) == unUTxO utxoAll

    -- Verify that no outputs are omitted when we select everything.
    prop_everything = counterexample "prop_everything" $
        utxoAll == F.foldMap utxoFromTx pendingTxs

    -- Computes the parity of an output based on its address parity.
    txOutParity :: TxOut -> Parity
    txOutParity = addressParity . view #address

    -- The UTxO set that contains all available output addresses.
    utxoAll :: UTxO
    utxoAll = changeUTxO pendingTxSet $ IsOursIf @Address (const True)

    -- The UTxO set that contains only even-parity output addresses.
    utxoEven :: UTxO
    utxoEven = changeUTxO pendingTxSet $ IsOursIf ((== Even) . addressParity)

    -- The UTxO set that contains only odd-parity output addresses.
    utxoOdd :: UTxO
    utxoOdd  = changeUTxO pendingTxSet $ IsOursIf ((== Odd) . addressParity)

    pendingTxSet :: Set Tx
    pendingTxSet = Set.fromList pendingTxs

--------------------------------------------------------------------------------
-- Matching entities with 'IsOurs'
--------------------------------------------------------------------------------

-- | A simplified wallet state that marks all entities as "ours".
--
data AllOurs = AllOurs

instance IsOurs AllOurs a where
    isOurs _ = (Just shouldNotEvaluate,)
      where
        shouldNotEvaluate = error "AllOurs: unexpected evaluation"

-- | A simplified wallet state that marks no entities as "ours".
--
data NoneOurs = NoneOurs

instance IsOurs NoneOurs a where
    isOurs _ = (Nothing,)

-- | Encapsulates a filter condition for matching entities with 'IsOurs'.
--
newtype IsOursIf a = IsOursIf {condition :: a -> Bool}

instance IsOurs (IsOursIf a) a where
    isOurs a s@IsOursIf {condition} = isOursIf condition a s

-- | Encapsulates a pair of filter conditions for matching entities with
--   'IsOurs'.
--
-- This is useful in contexts that require matching two different types of
-- entity.
--
data IsOursIf2 a b = IsOursIf2
    { conditionA :: Fun a Bool
    , conditionB :: Fun b Bool
    }

deriving instance (Eq (Fun a Bool), Eq (Fun b Bool)) => Eq (IsOursIf2 a b)
deriving instance (Show (Fun a Bool), Show (Fun b Bool)) => Show (IsOursIf2 a b)

instance IsOurs (IsOursIf2 a b) a where
    isOurs entity s@(IsOursIf2 {conditionA}) =
        isOursIf (applyFun conditionA) entity s

instance IsOurs (IsOursIf2 a b) b where
    isOurs entity s@(IsOursIf2 {conditionB}) =
        isOursIf (applyFun conditionB) entity s

isOursIf :: (a -> Bool) -> a -> s -> (Maybe (NonEmpty DerivationIndex), s)
isOursIf condition a s
    | condition a = (Just dummyDerivationIndex, s)
    | otherwise   = (Nothing                  , s)
  where
    dummyDerivationIndex :: NonEmpty DerivationIndex
    dummyDerivationIndex = DerivationIndex shouldNotEvaluate :| []
      where
        shouldNotEvaluate = error "Derivation index unexpectedly evaluated"

--------------------------------------------------------------------------------
-- Total UTxO properties
--------------------------------------------------------------------------------

data TestStateForTotalUTxO = TestStateForTotalUTxO

instance IsOurs TestStateForTotalUTxO Address where
    isOurs = isOursIf ((== Even) . addressParity)

prop_totalUTxO_pendingChangeIncluded :: Property
prop_totalUTxO_pendingChangeIncluded =
    prop_totalUTxO prop
  where
    prop pendingTxs wallet result =
        cover 20
            (not (UTxO.null pendingChange))
            "not (UTxO.null pendingChange)" $
        cover 20
            (unUTxO pendingChange `Map.isProperSubmapOf` unUTxO result)
            "unUTxO pendingChange `Map.isProperSubmapOf` unUTXO result" $
        unUTxO pendingChange `Map.isSubmapOf` unUTxO result
      where
        pendingChange :: UTxO
        pendingChange = changeUTxO pendingTxs (getState wallet)

prop_totalUTxO_pendingCollateralInputsIncluded :: Property
prop_totalUTxO_pendingCollateralInputsIncluded =
    prop_totalUTxO prop
  where
    prop pendingTxs _wallet result =
        cover 1
            (not (Set.null pendingCollateralInputs))
            "not (Set.null pendingCollateralInputs)" $
        all (`Map.member` unUTxO result) pendingCollateralInputs
      where
        pendingInputs :: Set TxIn
        pendingInputs = pendingTxs
            & F.foldMap (fmap fst . view #resolvedInputs)
            & Set.fromList

        -- In any given transaction, the sets of ordinary inputs and collateral
        -- inputs can intersect. Since ordinary inputs of pending transactions
        -- must be excluded from the result, we only consider collateral inputs
        -- that are not also used as ordinary inputs:
        pendingCollateralInputs :: Set TxIn
        pendingCollateralInputs = pendingTxs
            & F.foldMap (fmap fst . view #resolvedCollateralInputs)
            & L.filter (`Set.notMember` pendingInputs)
            & Set.fromList

prop_totalUTxO_pendingInputsExcluded :: Property
prop_totalUTxO_pendingInputsExcluded =
    prop_totalUTxO prop
  where
    prop pendingTxs _wallet result =
        cover 20
            (not (Set.null pendingInputs))
            "not (Set.null pendingInputs)" $
        all (`Map.notMember` unUTxO result) pendingInputs
      where
        pendingInputs :: Set TxIn
        pendingInputs = pendingTxs
            & F.foldMap (fmap fst . view #resolvedInputs)
            & Set.fromList

prop_totalUTxO
    :: Testable prop
    => (Set Tx -> Wallet TestStateForTotalUTxO -> UTxO -> prop)
    -> Property
prop_totalUTxO makeProperty =
    checkCoverage $
    forAllShrink (scale (* 2) genUTxO) shrinkUTxO
        $ \utxo ->
    forAllShrink (listOf (scale (`div` 2) genTx)) (shrinkList shrinkTx)
        $ \pendingTxs ->
    inner utxo pendingTxs
  where
    inner utxo pendingTxs =
        property $ makeProperty pendingTxSet wallet result
      where
        pendingTxSet = Set.fromList $ restrictTxInputs utxo <$> pendingTxs
        wallet = walletFromUTxO utxo
        result = totalUTxO pendingTxSet wallet

    -- Restricts a transaction so that its ordinary inputs and collateral
    -- inputs are all members of the given UTxO set.
    --
    restrictTxInputs :: UTxO -> Tx -> Tx
    restrictTxInputs utxo
        = over #resolvedInputs
            (filter ((`Set.member` utxoInputs) . fst))
        . over #resolvedCollateralInputs
            (filter ((`Set.member` utxoInputs) . fst))
      where
        utxoInputs = Map.keysSet (unUTxO utxo)

    -- Creates a wallet object from a UTxO set, and asserts that the other
    -- parts of the wallet state are not used in any way.
    --
    walletFromUTxO :: UTxO -> Wallet TestStateForTotalUTxO
    walletFromUTxO utxo = unsafeInitWallet utxo
        (shouldNotEvaluate "currentTip")
        TestStateForTotalUTxO
      where
        shouldNotEvaluate :: String -> a
        shouldNotEvaluate fieldName = error $ unwords
            [fieldName, "was unexpectedly evaluated"]

--------------------------------------------------------------------------------
-- Applying transactions to UTxO sets
--------------------------------------------------------------------------------

-- Verifies that 'applyOurTxToUTxO' updates the UTxO set in an identical
-- way to 'applyTxToUTxO' in the case that all entities are marked as ours.
--
prop_applyOurTxToUTxO_allOurs
    :: Slot
    -> Quantity "block" Word32
    -> Tx
    -> UTxO
    -> Property
prop_applyOurTxToUTxO_allOurs slot blockHeight tx utxo =
    checkCoverage $
    cover 50 haveResult "have result" $
    cover 0.1 (not haveResult) "do not have result" $
    report utxo "utxo" $
    report (utxoFromTx tx) "utxoFromTx tx" $
    report haveResult "haveResult" $
    report shouldHaveResult "shouldHaveResult" $
    case maybeResult of
        Nothing ->
            verify (not shouldHaveResult) "not shouldHaveResult" $
            property True
        Just utxo' ->
            cover 10 (utxo /= utxo') "utxo /= utxo'" $
            verify shouldHaveResult "shouldHaveResult" $
            verify (utxo' == applyTxToUTxO tx utxo)
                "utxo' == applyTxToUTxO tx utxo" $
            property True
  where
    haveResult :: Bool
    haveResult = isJust maybeResult
    maybeResult :: Maybe UTxO
    maybeResult = get <$> applyOurTxToUTxO slot blockHeight AllOurs tx utxo
      where get (_,_,a) = a
    shouldHaveResult :: Bool
    shouldHaveResult = evalState (isOurTx tx utxo) AllOurs

-- Verifies that 'applyOurTxToUTxO' returns a result only when it is
-- appropriate to do so.
--
-- Within this property, only some addresses and reward accounts are marked as
-- being "ours".
--
prop_applyOurTxToUTxO_someOurs
    :: IsOursIf2 Address RewardAccount
    -> Slot
    -> Quantity "block" Word32
    -> Tx
    -> UTxO
    -> Property
prop_applyOurTxToUTxO_someOurs ourState slot blockHeight tx utxo =
    checkCoverage $
    cover 50 haveResult "have result" $
    cover 0.1 (not haveResult) "do not have result" $
    report utxo "utxo" $
    report (utxoFromTx tx) "utxoFromTx tx" $
    report haveResult "haveResult" $
    report shouldHaveResult "shouldHaveResult" $
    case maybeResult of
        Nothing ->
            verify (not shouldHaveResult) "not shouldHaveResult" $
            property True
        Just utxo' ->
            cover 10 (utxo /= utxo') "utxo /= utxo'" $
            verify shouldHaveResult "shouldHaveResult" $
            property True
  where
    haveResult :: Bool
    haveResult = isJust maybeResult
    maybeResult :: Maybe UTxO
    maybeResult = get <$> applyOurTxToUTxO slot blockHeight ourState tx utxo
      where get (_,_,a) = a
    shouldHaveResult :: Bool
    shouldHaveResult = evalState (isOurTx tx utxo) ourState

{-------------------------------------------------------------------------------
                               Address discovery
-------------------------------------------------------------------------------}

{- HLINT ignore prop_discoverAddressesBlock  "Avoid lambda using `infix`" -}
prop_discoverAddressesBlock :: ApplyBlock -> Property
prop_discoverAddressesBlock (ApplyBlock s utxo block) =
    snd (discoverAddressesBlock block s)
    ===
    execState (mapM (\tx -> isOurTx tx utxo) txs) s
  where
    txs = view #transactions block

-- | Performs address discovery and indicates whether a given transaction is
-- relevant to the wallet.
--
-- This function is only used for unit tests -- wallet model code uses the
-- 'ours' and 'updateOurs' variants.
isOurTx
    :: (IsOurs s Address, IsOurs s RewardAccount)
    => Tx
    -> UTxO
    -> State s Bool
isOurTx tx u
    -- If a transaction has failed script validation, then the ledger rules
    -- require that applying the transaction shall have no effect other than
    -- to fully spend the collateral inputs included within that transaction.
    --
    -- Therefore, such a transaction is only relevant to the wallet if it has
    -- one more collateral inputs that belong to the wallet.
    --
    | txScriptInvalid tx =
        F.or <$> sequence
            [ txHasRelevantCollateralInput
            , txHasRelevantCollateralOutput
            ]
    | otherwise =
        F.or <$> sequence
            [ txHasRelevantInput
            , txHasRelevantOutput
            , txHasRelevantWithdrawal
            ]
  where
    txHasRelevantInput =
        pure . not . UTxO.null $
        u `UTxO.restrictedBy` Set.fromList
            (fst <$> tx ^. #resolvedInputs)
    txHasRelevantCollateralInput =
        pure . not . UTxO.null $
        u `UTxO.restrictedBy` Set.fromList
            (fst <$> tx ^. #resolvedCollateralInputs)
    txHasRelevantOutput =
        F.or <$> mapM (isOursState . (view #address)) (tx ^. #outputs)
    txHasRelevantCollateralOutput =
        F.or <$> mapM (isOursState . (view #address)) (tx ^. #collateralOutput)
    txHasRelevantWithdrawal =
        F.or <$> mapM (isOursState . fst) (Map.toList (tx ^. #withdrawals))

    isOursState :: IsOurs s addr => addr -> State s Bool
    isOursState = fmap isJust . state . isOurs

prop_discoverFromBlockData
    :: ApplyBlocks -> Property
prop_discoverFromBlockData (ApplyBlocks s _ blocks) =
    case maybeDiscover of
        Nothing ->
            label "Address discovery state does not support light-mode." False
        Just discover ->
            discoverState (Summary discover summary) s
            === discoverState (List blocks) s
  where
    -- TODO: The `summary` here is incomplete and may miss transactions
    -- because `summarizeOnTxOut` does not consider transaction inputs.
    -- This likely won't happen in the test cases generated by `ApplyBlocks`,
    -- but more rigorous testing of randomly generated block chains
    -- may reveal this.
    summary = summarizeOnTxOut blocks
    discoverState
        :: (IsOurs s Address, IsOurs s RewardAccount, MaybeLight s)
        => BlockData Identity (Either Address RewardAccount) ChainEvents s
        -> s -> s
    discoverState bs = snd . runIdentity . discoverFromBlockData bs

{-------------------------------------------------------------------------------
               Basic Model - See Wallet Specification, section 3

   Our implementation of 'applyBlock' is a bit more complex than the basic
   model. In practice, we do not want to compute intersection and tx id of a
   whole block of transactions, but we only do it for the one that are relevant
   to us.
   Plus, we are tracking more than just the UTxO. However, when it comes to UTxO
   the basic model and our implementation should be "on-par" and therefore,
   given a few blocks, we should be able to control that they are indeed.
-------------------------------------------------------------------------------}

-- Update UTxO as described in the formal specification, Fig 3. The basic model
updateUTxO
    :: IsOurs s Address
    => Block
    -> UTxO
    -> State s UTxO
updateUTxO !b utxo = do
    let txs = Set.fromList $ transactions b
    utxo' <- (foldMap utxoFromTx txs `restrictedTo`) . Set.map snd
        <$> state (txOutsOurs txs)
    return $
        (utxo <> utxo') `excluding` foldMap inputsSpentByTx txs

-- | Return all transaction outputs that are ours. This plays well within a
-- 'State' monad.
--
-- @
-- myFunction :: Block -> State s Result
-- myFunction b = do
--    ours <- state $ txOutsOurs (transaction b)
--    return $ someComputation ours
-- @
txOutsOurs
    :: forall s. (IsOurs s Address)
    => Set Tx
    -> s
    -> (Set (Tx, TxOut), s)
txOutsOurs txs =
    runState $ Set.fromList <$> forMaybe
        (foldMap (\tx -> map (tx,) (outputsCreatedByTx tx)) txs) pick
  where
    pick :: (Tx, TxOut) -> State s (Maybe (Tx, TxOut))
    pick (tx, out) = do
        predicate <- state $ isOurs (address out)
        return $ case predicate of
            Just{}  -> Just (tx, out)
            Nothing -> Nothing
    forMaybe :: Monad m => [a] -> (a -> m (Maybe b)) -> m [b]
    forMaybe xs = fmap catMaybes . for xs

{-------------------------------------------------------------------------------
                                  Test Data

    In practice, we may want to generate arbitrary valid sequences of block.
    This isn't trivial though because we would need to generate _valid_ chains
    for various invariants and preconditions to hold. Work have been done in
    cardano-sl to generate such chains, and we may want to use that at some
    point. For now, a valid chain coming from the testnet will do

-------------------------------------------------------------------------------}

-- | An arbitrary wallet state that can recognize some hard-coded addresses from
-- our chain. This allows us to control that the UTxO gets updated accordingly
-- for some arbitrary instances of that state.
data WalletState = WalletState
    { _ourAddresses :: Set (ShowFmt Address)
    , _discoveredAddresses :: Set (ShowFmt Address)
    } deriving (Generic, Show, Eq)

ourAddresses :: WalletState -> Set Address
ourAddresses =
    Set.map (\(ShowFmt a) -> a) . _ourAddresses

ourRewardAccount :: RewardAccount
ourRewardAccount = RewardAccount $ BS.replicate 28 0

instance NFData WalletState

instance Semigroup WalletState where
    (WalletState ours a) <> (WalletState ours' b) =
        invariant "Semigroup WalletState must be defined on same addresses"
            (WalletState ours (a <> b))
            (\_ -> ours == ours')

instance IsOurs WalletState Address where
    isOurs addr s@(WalletState ours discovered) =
        case ShowFmt addr `elemIndex` Set.toList ours of
            Just ix ->
                let path = DerivationIndex (fromIntegral ix) :| []
                in (Just path, WalletState ours (Set.insert (ShowFmt addr) discovered))
            Nothing ->
                (Nothing, s)

instance IsOurs WalletState RewardAccount where
    isOurs account s =
        ( guard (account == ourRewardAccount) $> (DerivationIndex 0 :| [])
        , s
        )

instance MaybeLight WalletState where
    maybeDiscover = Just $ DiscoverTxs discover
      where
        discover query s0 = go (Set.toList $ ourAddresses s0) s0
          where
            go []     s = pure (mempty, s) -- TODO: Test transactions as well.
            go (a:as) s = do
                txs <- query (Left a)
                if mempty == txs
                    then go as s
                    else go as (updateOurs s a)

instance (CoArbitrary a, CoArbitrary b, Function a, Function b) =>
    Arbitrary (IsOursIf2 a b)
  where
    arbitrary = IsOursIf2
        <$> arbitrary
        <*> arbitrary
    shrink (IsOursIf2 a b) = uncurry IsOursIf2 <$> shrink (a, b)

instance Arbitrary Coin where
    shrink = shrinkCoin
    arbitrary = genCoin

instance Arbitrary (Quantity "block" Word32) where
    arbitrary = Quantity <$> arbitrarySizedBoundedIntegral @Word32
    shrink = shrinkMapBy Quantity getQuantity shrinkIntegral

instance Arbitrary Slot where
    arbitrary = genSlot
    shrink = shrinkNothing

instance Arbitrary Tx where
    shrink = shrinkTx
    arbitrary = genTx

instance Arbitrary TxIn where
    arbitrary = genTxIn
    shrink = shrinkTxIn

instance Arbitrary TxOut where
    arbitrary = genTxOut
    shrink = shrinkTxOut

instance Arbitrary UTxO where
    shrink = shrinkUTxO
    arbitrary = genUTxO

instance Arbitrary WalletState where
    shrink = genericShrink
    arbitrary = do
        knownAddresses <- Set.fromList <$> listOf arbitrary
        return $ WalletState knownAddresses mempty

instance Arbitrary (ShowFmt Address) where
    shrink _ = []
    arbitrary = ShowFmt <$> elements addresses

data WithPending s = WithPending
    { _wallet :: Wallet s
    , _pendingTxs :: Set Tx
    , _rewards :: Coin
    } deriving (Generic, Show)

instance Arbitrary (Hash "Tx") where
    arbitrary = Hash . BS.pack <$> vector 32

instance Arbitrary (WithPending WalletState) where
    shrink _  = []
    arbitrary = do
        (_, cp0) <- initWallet @_ block0 <$> arbitrary
        subChain <- flip take blockchain <$> choose (1, length blockchain)
        let wallet = foldl (\cp b -> snd . snd $ applyBlock b cp) cp0 subChain
        rewards <- Coin <$> oneof [pure 0, chooseNatural (1, 10000)]
        pending <- genPendingTxs (totalUTxO Set.empty wallet) rewards
        pure $ WithPending wallet pending rewards
      where
        genPendingTxs :: UTxO -> Coin -> Gen (Set Tx)
        genPendingTxs (UTxO u) rewards
            | Map.null u = pure Set.empty
            | otherwise  = do
                (inp, out) <-
                    (\i -> Map.toList u !! i) <$> choose (0, Map.size u - 1)

                arbitraryHash <- arbitrary

                withWithdrawal <- frequency
                    [ (3, pure id)
                    , (1, pure $ \tx ->
                        if rewards == Coin 0
                        then tx
                        else tx
                            { withdrawals =
                                Map.singleton ourRewardAccount rewards
                            , outputs =
                                out { tokens = TokenBundle.fromCoin rewards }
                                    : outputs tx
                            }
                      )
                    ]

                -- NOTE:
                --
                -- - We simply re-use the same output to send money to it, since
                --   we know it's already ours.
                --
                -- - We send less than the input value, to simulate some fees
                --
                -- - Sometimes, we also add a withdrawal by creating another
                --   change output and an explicit withdrawal in the
                --   transaction.
                let tokens = TokenBundle.fromCoin $ simulateFee $ TxOut.coin out
                scriptValidity <- liftArbitrary genTxScriptValidity
                let pending = withWithdrawal $ Tx
                        { txId = arbitraryHash
                        , txCBOR = Nothing
                        , fee = Nothing
                        , resolvedInputs = [(inp, Just out)]
                        -- TODO: (ADP-957)
                        , resolvedCollateralInputs = []
                        -- TODO: [ADP-1670]
                        , collateralOutput = Nothing
                        , outputs = [out {tokens}]
                        , withdrawals = mempty
                        , metadata = Nothing
                        , scriptValidity
                        }

                elements [Set.singleton pending, Set.empty]
          where
            simulateFee (Coin amt) = Coin (amt - 5000)

-- | Since it's quite tricky to generate a valid Arbitrary chain and
-- corresponding initial UTxO, instead, we take subset of our small valid
-- blockchain and reconstruct a valid initial UTxO by applying a prefix
-- of the blocks in the chain. Then, we consider additional blocks in the
-- chain for application.
data ApplyBlocks = ApplyBlocks WalletState UTxO (NonEmpty Block)
    deriving Show

shrinkBlocks :: NonEmpty Block -> [NonEmpty Block]
shrinkBlocks bs = (\n -> NE.fromList $ NE.take n bs) <$> [1, 2, NE.length bs]

instance Arbitrary ApplyBlocks where
    shrink (ApplyBlocks s u bs) =
        ApplyBlocks s <$> shrinkUTxO u <*> shrinkBlocks bs
    arbitrary = do
        n <- choose (1, length blockchain)
        m <- choose (0, n-1)
        s <- arbitrary
        let (prefix, suffix) = splitAt m (take n blockchain)
            utxo = evalState (foldM (flip updateUTxO) mempty prefix) s
            blocks = NE.fromList suffix
        return $ ApplyBlocks s utxo blocks

-- | Same as 'ApplyBlocks', but with a single next block.
data ApplyBlock = ApplyBlock WalletState UTxO Block
    deriving Show

instance Arbitrary ApplyBlock where
    shrink (ApplyBlock s u b) = ApplyBlock s <$> shrinkUTxO u <*> pure b
    arbitrary = headBlock <$> arbitrary
      where
        headBlock (ApplyBlocks s u (b :| _)) = ApplyBlock s u b

addresses :: [Address]
addresses = map address
    $ concatMap outputs
    $ concatMap transactions
    blockchain

coinToBundle :: Word64 -> TokenBundle
coinToBundle = TokenBundle.fromCoin . Coin.fromWord64

-- A excerpt of mainnet, epoch #14, first 20 blocks; plus a few previous blocks
-- which contains transactions referred to in the former. This is useful to test
-- correct resolution of the tx history.
blockchain :: [Block]
blockchain =
    [ Block
        { header = BlockHeader
            { slotNo = slot 2 19218
            , blockHeight = Quantity 62392
            , headerHash = Hash "unused"
            , parentHeaderHash = Just $ Hash "y\130\145\211\146\234S\221\150\GS?\212>\167B\134C\r\160J\230\173\SOHn\188\245\141\151u\DC4\236\154"
            }
        , transactions =
            [ Tx
                { txId = Hash "97928516bb05fce234d26e99b22b2e68c81841730fb5bd1d835b67374f1de8d7"
                , fee = Nothing
                , txCBOR = Nothing
                , resolvedInputs =
                    [ (TxIn
                        { inputId = Hash "\199D\198\229\227\196\204\231\178\166m\226\134\211\DC1}\243[\204\DC4\171\213\230\246\SOHy\229\t\167\184\235g"
                        , inputIx = 0
                        }, Nothing)
                    , (TxIn
                        { inputId = Hash "\a\241.\180(\a\148\201u$\229\251\147\224\f\166\159\EOT\166m\US\178dN\242\227\b\254\227G\169\RS"
                        , inputIx = 0
                        }, Nothing)
                    ]
                , resolvedCollateralInputs = []
                , outputs =
                    [ TxOut
                        { address = Address "\130\216\CANXB\131X\FS\251\STX\v\235\129\179\243k\185\131Eq\190\239\137\143\ETB\167\&7\GS\131\&1\215R\202!\US\205\161\SOHX\RSX\FSq=\137+\197\151g\151-\158\222\RS\246\190\155\EOTz\242\202H\SUB\237\227\167)\fo\198\NUL\SUBw\218X/"
                        , tokens = coinToBundle 21063
                        }
                    , TxOut
                        { address = Address "\130\216\CANXB\131X\FS\132X0\144p\144\ENQ\145\&2\224\&3\149hLk\221\152l\142>O\154\210\133\148\211\152\138\161\SOHX\RSX\FS\202>U<\156c\197o\203\t\188C_\254\205\ETXj\237\193\192\144\210KJyU\DEL\240\NUL\SUB\139\185\251\n"
                        , tokens = coinToBundle 3844423800000
                        }
                    ]
                , collateralOutput = Nothing
                , withdrawals = mempty
                , metadata = Nothing
                , scriptValidity = Nothing
                }
            ]
        , delegations = []
        }
    , Block
        { header = BlockHeader
            { slotNo = slot 13 20991
            , blockHeight = Quantity 301749
            , headerHash = Hash "unused"
            , parentHeaderHash = Just $ Hash "m\FS\235\ETB6\151'\250M\SUB\133\235%\172\196B_\176n\164k\215\236\246\152\214cc\214\&9\207\142"
            }
        , transactions =
            [ Tx
                { txId = Hash "cd6f2081fdd619c623c71e5cd2a3b5f22289e598b727f325dbba9681ea723079"
                , txCBOR = Nothing
                , fee = Nothing
                , resolvedInputs =
                    [ (TxIn
                        { inputId = Hash "+\253\232\DC3\132\"M\NULf\EM\228\bh)\STX\171W\215@#\198\a\228\229Z2]\156_fjg"
                        , inputIx = 0
                        }, Nothing)
                    ]
                , resolvedCollateralInputs = []
                , outputs =
                    [ TxOut
                        { address = Address "\130\216\CANXB\131X\FS\211Yn9s*R\243\193x\166T\178\189%i\182X\179!\ESC\tf\t;\CAN8\GS\161\SOHX\RSX\FS\202>U<\156c\197M\234W\ETBC\f\177\235\163\254\194\RS\225\ESC\\\244\b\255\164\CAN\201\NUL\SUB\166\230\137["
                        , tokens = coinToBundle 3860802399001
                        }
                    , TxOut
                        { address = Address "\130\216\CANXB\131X\FS\149\244~\254\146>\133\160ic\137LqZ\152|N\185\207\CANun\252*\158\\\ACK\NUL\161\SOHX\RSX\FSR\128\f\225\232\SO\196\204\225Dz\SOH\145\129)t\175k\191\148Am\NAK\156\&4\DC2\166q\NUL\SUB\238\180t\198"
                        , tokens = coinToBundle 3351830178
                        }
                    ]
                , collateralOutput = Nothing
                , withdrawals = mempty
                , metadata = Nothing
                , scriptValidity = Nothing
                }
            , Tx
                { txId = Hash "b17ca3d2b8a991ea4680d1ebd9940a03449b1b6261fbe625d5cae6599726ea41"
                , txCBOR = Nothing
                , fee = Nothing
                , resolvedInputs =
                    [ (TxIn
                        { inputId = Hash "\137\150\&8\141\164l\v\ACK\132\198\SI\GS7\201\&3Dd\177fM,\GS)\EM\DC4\242#\211'3\233\163"
                        , inputIx = 0
                        }, Nothing)
                    ]
                , resolvedCollateralInputs = []
                , outputs =
                     [ TxOut
                        { address = Address "\130\216\CANXB\131X\FS)/\216\137\&7\187\235\136\159m[g\DC2\156\193v\EM\169^\GS\176\128\rh\186\234\EM\NUL\161\SOHX\RSX\FS\202>U<\156c\197\SYN!\161_C\135\ACK\210/\193|\STX\158f\138C\234\221\RS\134\231\NUL\SUB\190\&2?C"
                        , tokens = coinToBundle 3844424216795
                        }
                    , TxOut
                        { address = Address "\130\216\CANXB\131X\FS\ACK\218k\189\250\189\129\229A\128>`V\153\144EyN\187T\\\151 \171;\251(\t\161\SOHX\RSX\FS\197\217I\176.##'\217l\226i{\200'\176\&32I\150\166\SI+\143\138\GS\SOH+\NUL\SUB7\206\156`"
                        , tokens = coinToBundle 19999800000
                        }
                    ]
                , collateralOutput = Nothing
                , withdrawals = mempty
                , metadata = Nothing
                , scriptValidity = Nothing
                }
            ]
        , delegations = []
        }
    , Block
        { header = BlockHeader
            { slotNo = slot 13 21458
            , blockHeight = Quantity 302216
            , headerHash = Hash "unused"
            , parentHeaderHash = Just $ Hash "hA\130\182\129\161\&7u8\CANx\218@S{\131w\166\192Bo\131) 2\190\217\134\&7\223\&2>"
            }
        , transactions =
            [ Tx
                { txId = Hash "73a5d20740d511e01090247f8aca90e1e550564244173235a1a47589007b9e76"
                , txCBOR = Nothing
                , fee = Nothing
                , resolvedInputs =
                    [ (TxIn
                        { inputId = Hash "(\EM#\f\165\236\169=\227\163>MY\225ts\192\SYN\137=\145\155~\212.\252\130l\166v0\SOH"
                        , inputIx = 0
                        }, Nothing)
                    ]
                , resolvedCollateralInputs = []
                , outputs =
                    [ TxOut
                        { address = Address "\130\216\CANXB\131X\FS\ACK\142\129o\164[teM\222\&2`\153\STX'\DC4\190\n\194\156:6\DC3\223\184\150[\249\161\SOHX\RSX\FS\202>U<\156c\197\f\132y\163C>\252]w\f\STXb\GS\150\130\255\215`\140\212\CAN\NUL\SUB\135\214\245\224"
                        , tokens = coinToBundle 3844425617319
                        }
                    , TxOut
                        { address = Address "\130\216\CANXB\131X\FS\184&\170\193\237\196\242-9\168)Pg\NUL\217\149\&6\169\158U\177c'/\172\221\148\232\161\SOHX\RSX\FS\202>U<\156c\197{\209\173\167C\204n~C\188\169&\217c\212'\131Nm<\150\NUL\SUB=\147\148z"
                        , tokens = coinToBundle 3495800000
                        }
                    ]
                , collateralOutput = Nothing
                , withdrawals = mempty
                , metadata = Nothing
                , scriptValidity = Nothing
                }
            , Tx
                { txId = Hash "6ed51b05821f0dc130a9411f0d63a241a624fbc8a9c8a2a13da8194ce3c463f4"
                , txCBOR = Nothing
                , fee = Nothing
                , resolvedInputs =
                    [ ( TxIn
                        { inputId = Hash "\128\168muc\212\EMP\238\\\173w\203\159N\205T:\230V\134\164w\143>\192\134\153\SUB$cD"
                        , inputIx = 0
                        }, Nothing)
                    ]
                , resolvedCollateralInputs = []
                , outputs =
                    [ TxOut
                        { address = Address "\130\216\CANXB\131X\FSY\128\ETX4\191\170\EOT\144\195#\f]\ESCy\nSe\216+f\132\210\232x\168\160''\161\SOHX\RSX\FS\202>U<\156c\197E\160\162\181C\f|\SO\223\170\DC4\253.R\248R+'\162\172\166\NUL\SUB\220\192\171)"
                        , tokens = coinToBundle 3817943388680
                        }
                    , TxOut
                        { address = Address "\130\216\CANXB\131X\FScS\243q\152\237Vv\197\162\RS\168\238\130}\172\&0_=\142n\170]\198EH@l\161\SOHX\RSX\FS(}\ETB\129*k\253\173\&2\177\131V0`\219\243\212*\153\212\159@\128\149\209s\143(\NUL\SUB\"\175\195<"
                        , tokens = coinToBundle 29999800000
                        }
                    ]
                , collateralOutput = Nothing
                , withdrawals = mempty
                , metadata = Nothing
                , scriptValidity = Nothing
                }
            ]
        , delegations = []
        }
    , Block
        { header = BlockHeader
            { slotNo = slot 13 21586
            , blockHeight = Quantity 1321586
            , headerHash = Hash "unused"
            , parentHeaderHash = Just $ Hash "D\152\178<\174\160\225\230w\158\194-$\221\212:z\DC1\255\239\220\148Q!\220h+\134\220\195e5"
            }
        , transactions =
            [ Tx
                { txId = Hash "bbc7a1f0de24085ac48a52ee6f89d1815145845a8712547350a7e492385974ab"
                , txCBOR = Nothing
                , fee = Nothing
                , resolvedInputs =
                    [ (TxIn
                        { inputId = Hash "\164\254\137\218h\f\DLE\245\141u\SYN\248~\253n;\202\144\150\v\229\177\218\195\238\157\230\158\241O\153\215"
                        , inputIx = 0
                        }, Nothing)
                    ]
                , resolvedCollateralInputs = []
                , outputs =
                    [ TxOut
                        { address = Address "\130\216\CANXB\131X\FSJ:Kh-\227hW$\139\165\194\192\249\251f\250\NAKf\207\146\131\193\248\242%\153\180\161\SOHX\RSX\FS\202>U<\156c\197\US\196\DC3\208C*1\176\172\138(\EOTd\b\179\157\135e\171#\136\NUL\SUB)\228M*"
                        , tokens = coinToBundle 3844435857860
                        }
                    , TxOut
                        { address = Address "\130\216\CANXB\131X\FS\135:\161F\145\151\189z\134\231\254\143\134\129\227I\251\193\129\&8\161\208\236\US[\203e\211\161\SOHX\RSX\FS\142\&2M\NAK\156,\206r\v\237\129;u\168\&3\215\243Kyd\143\EM0\240\182\DC4dE\NUL\SUB\195\DEL\204\176"
                        , tokens = coinToBundle 500000000
                        }
                    ]
                , collateralOutput = Nothing
                , withdrawals = mempty
                , metadata = Nothing
                , scriptValidity = Nothing
                }
            ]
        , delegations = []
        }
    , Block
        { header = BlockHeader
            { slotNo = slot 14 0
            , blockHeight = Quantity 302358
            , headerHash = Hash "unused"
            , parentHeaderHash = Just $ Hash "39d89a1e837e968ba35370be47cdfcbfd193cd992fdeed557b77c49b77ee59cf"
            }
        , transactions = []
        , delegations = []
        }
    , Block
        { header = BlockHeader
            { slotNo = slot 14 1
            , blockHeight = Quantity 302359
            , headerHash = Hash "unused"
            , parentHeaderHash = Just $ Hash "2d04732b41d07e45a2b87c05888f956805f94b108f59e1ff3177860a17c292db"
            }
        , transactions =
            [ Tx
                { txId = Hash "fd545d055c2241802d1518b7fd9f8842c217b54c73364e519ccada5cda07612a"
                , txCBOR = Nothing
                , fee = Nothing
                , resolvedInputs =
                    [ (TxIn
                        { inputId = Hash "\187\199\161\240\222$\bZ\196\138R\238o\137\209\129QE\132Z\135\DC2TsP\167\228\146\&8Yt\171"
                        , inputIx = 0
                        }, Nothing)
                    ]
                , resolvedCollateralInputs = []
                , outputs =
                    [ TxOut
                      { address = Address "\130\216\CANXB\131X\FSUh\206'\198\237\161R3\214L\145\245P'\197\230\&6\206\152\173\EOTI:\152\vX&\161\SOHX\RSX\FS\202>U<\156c\197|\227M\202Cv\136\\\253\176\130\185b9G\188_\179\&4\253Y\NUL\SUB\176\EOT\165s"
                      , tokens = coinToBundle 3834435886614
                      }
                    , TxOut
                      { address = Address "\130\216\CANXB\131X\FSq4\137\215\171\175Z\ENQ\242\216^\239\197\244^s\230\170}\183}\136\143\218\150\ENQ\137\255\161\SOHX\RSX\FS\173y\SI\234\169\ETB\\\251\238\175\128\178\191a\128\142?(\FSD\148\182\192\250\221\&5;7\NUL\SUB\241\244w\194"
                      , tokens = coinToBundle 9999800000
                      }
                    ]
                , collateralOutput = Nothing
                , withdrawals = mempty
                , metadata = Nothing
                , scriptValidity = Nothing
                }
            ]
        , delegations = []
        }
    , Block
        { header = BlockHeader
            { slotNo = slot 14 2
            , blockHeight = Quantity 302360
            , headerHash = Hash "unused"
            , parentHeaderHash = Just $ Hash "e95a6e7da3cd61e923e30b1998b135d40958419e4157a9f05d2f0f194e4d7bba"
            }
        , transactions =
            [ Tx
                { txId = Hash "dffbcd7ca494c4695cc2fd4ec525ffca0861bafb221ee185711f99ca49ae7c11"
                , txCBOR = Nothing
                , fee = Nothing
                , resolvedInputs =
                    [ (TxIn
                        { inputId = Hash "s\165\210\a@\213\DC1\224\DLE\144$\DEL\138\202\144\225\229PVBD\ETB25\161\164u\137\NUL{\158v"
                        , inputIx = 0
                        }, Nothing) ]
                , resolvedCollateralInputs = []
                , outputs =
                    [ TxOut
                        { address = Address "\130\216\CANXB\131X\FS\255-+\179k\202\194\212\206\224\248\243\158\b\188 \212\141$\189\194&\252\162\166\162jq\161\SOHX\RSX\FS\202>U<\156c\197QM\140\ACKCk=\238\239\134^w\CAN$\253\FSqL\198\128\200\NUL\SUB\f\219\163/"
                        , tokens = coinToBundle 3841151724910
                        }
                    , TxOut
                        { address = Address "\130\216\CANXB\131X\FS\n\aGD6\206\202\&2K\n\203%\180\249\227\229\216n\130\218\&6\147\SYN/\SUBq\231\210\161\SOHX\RSX\FS?\DLE\204\131\217-\176\181^\169#?Jn~\137\153\ENQc0<\225\SOH)\DEL\150\163\136\NUL\SUB\b\215\236\238"
                        , tokens = coinToBundle 3273721339
                        }
                    ]
                , collateralOutput = Nothing
                , withdrawals = mempty
                , metadata = Nothing
                , scriptValidity = Nothing
                }
            ]
        , delegations = []
        }
    , Block
        { header = BlockHeader
            { slotNo = slot 14 3
            , blockHeight = Quantity 302361
            , headerHash = Hash "unused"
            , parentHeaderHash = Just $ Hash "b5d970285a2f8534e94119cd631888c20b3a4ec0707a821f6df5c96650fe01dd"
            }
        , transactions =
            [ Tx
                { txId = Hash "c430d9ae438b9fe1c0898e9c131f3ca2c64c34ef75b202a834b6eabe248eac88"
                , txCBOR = Nothing
                , fee = Nothing
                , resolvedInputs =
                    [ (TxIn
                        { inputId = Hash "\177|\163\210\184\169\145\234F\128\209\235\217\148\n\ETXD\155\ESCba\251\230%\213\202\230Y\151&\234A"
                        , inputIx = 0
                        }, Nothing)
                    ]
                , resolvedCollateralInputs = []
                , outputs =
                    [ TxOut
                        { address = Address { unAddress = "\130\216\CANXB\131X\FS!\148\NULDcB\r\237\202\255)\DLEe`\159\a\\-IG\"P\218\136\219i\244\134\161\SOHX\RSX\FS\202>U<\156c\197;\236\EOT\STXC\209\173\138\205B\EOT.\ENQ\ACKG@\174\206\185\ESC\206\NUL\SUB\230\150\192\165" }
                        , tokens = coinToBundle 3824424245549
                        }
                    , TxOut
                        { address = Address "\130\216\CANXB\131X\FS\ACK\218k\189\250\189\129\229A\128>`V\153\144EyN\187T\\\151 \171;\251(\t\161\SOHX\RSX\FS\197\217I\176.##'\217l\226i{\200'\176\&32I\150\166\SI+\143\138\GS\SOH+\NUL\SUB7\206\156`"
                        , tokens = coinToBundle 19999800000
                        }
                    ]
                , collateralOutput = Nothing
                , withdrawals = mempty
                , metadata = Nothing
                , scriptValidity = Nothing
                }
            ]
        , delegations = []
        }
    , Block
        { header = BlockHeader
              { slotNo = slot 14 4
              , blockHeight = Quantity 302362
              , headerHash = Hash "unused"
              , parentHeaderHash = Just $ Hash "cb96ff923728a67e52dfad54df01fc5a20c7aaf386226a0564a1185af9798cb1"
              }
        , transactions = []
        , delegations = []
        }
    , Block
        { header = BlockHeader
              { slotNo = slot 14 5
              , blockHeight = Quantity 302363
              , headerHash = Hash "unused"
              , parentHeaderHash = Just $ Hash "63040af5ed7eb2948e2c09a43f946c91d5dd2efaa168bbc5c4f3e989cfc337e6"
              }
        , transactions =
            [ Tx
                { txId = Hash "b5af444a0d95ebd1a55185a0aee2b19835da1c86fc2b43f453f04c002bbc708e"
                , txCBOR = Nothing
                , fee = Nothing
                , resolvedInputs =
                    [ ( TxIn
                        { inputId = Hash "\195\242\DEL-\232v(c\SI+\172\163\245\142\189\214aiB#4\139\172\166\237\167\ETB9\246\150\185\219"
                        , inputIx = 1
                        }, Nothing)
                    , ( TxIn
                        { inputId = Hash "8O\137\193\224w\243\252s\198\250\201\&04\169\129E\155{\n\DC3H<\199\208\154\214\237\141\128<+"
                        , inputIx = 1
                        }, Nothing)
                    ]
                , resolvedCollateralInputs = []
                , outputs =
                    [ TxOut
                        { address = Address "\130\216\CANXB\131X\FS%\ENQ\163x'\DC3\DC1\222\157\197 4*\200v\219\f\201\215\197\136\188\128\243\216\NAKe\214\161\SOHX\RSX\FS\197\217I\176.##LD\224\179i\142\&3\220\162\250\221:F\227\NAK$\156|\EOTY\228\NUL\SUBr\a\134\146"
                        , tokens = coinToBundle 15908
                        }
                    , TxOut
                        { address = Address "\130\216\CANXB\131X\FS\SI\DC4f\168\210\188\164\SUBF\239\212\201,\DLE\238\230<r\187A+w\b\222\155\ETB\226m\161\SOHX\RSX\FS\234\DC4%\204\221d\155\200\136\211o~\SOH\229t\229\178p\146\188\214X\237\151T\183\&4\247\NUL\SUBx\242\186\182"
                        , tokens = coinToBundle 12999433909
                        }
                    ]
                , collateralOutput = Nothing
                , withdrawals = mempty
                , metadata = Nothing
                , scriptValidity = Nothing
                }
            ]
        , delegations = []
        }
    , Block
        { header = BlockHeader
            { slotNo = slot 14 6
            , blockHeight = Quantity 302364
            , headerHash = Hash "unused"
            , parentHeaderHash = Just $ Hash "1a32e01995225c7cd514e0fe5087f19a6fd597a6071ad4ad1fbf5b20de39670b"
            }
        , transactions = []
        , delegations = []
        }
    , Block
        { header = BlockHeader
            { slotNo = slot 14 7
            , blockHeight = Quantity 302365
            , headerHash = Hash "unused"
            , parentHeaderHash = Just $ Hash "7855c0f101b6761b234058e7e9fd19fbed9fee90a202cca899da1f6cbf29518d"
            }
        , transactions = []
        , delegations = []
        }
    , Block
        { header = BlockHeader
            { slotNo = slot 14 8
            , blockHeight = Quantity 302366
            , headerHash = Hash "unused"
            , parentHeaderHash = Just $ Hash "9007e0513b9fea848034a7203b380cdbbba685073bcfb7d8bb795130d92e7be8"
            }
        , transactions = []
        , delegations = []
        }
    , Block
        { header = BlockHeader
            { slotNo = slot 14 9
            , blockHeight = Quantity 302367
            , headerHash = Hash "unused"
            , parentHeaderHash = Just $ Hash "0af8082504f59eb1b7114981b7dee9009064638420382211118730b45ad385ae"
            }
        , transactions = []
        , delegations = []
        }
    , Block
        { header = BlockHeader
            { slotNo = slot 14 10
            , blockHeight = Quantity 302368
            , headerHash = Hash "unused"
            , parentHeaderHash = Just $ Hash "adc8c71d2c85cee39fbb34cdec6deca2a4d8ce6493d6d28f542d891d5504fc38"
            }
        , transactions =
            [ Tx
                { txId = Hash "e65862828230aa340878c8d593f84d59397f10dfa0b183d2d15bea0a5db8ccaf"
                , txCBOR = Nothing
                , fee = Nothing
                , resolvedInputs =
                    [ ( TxIn
                        { inputId = Hash "\ETXX\189\235\195q81{D\DC3\DLE\228\237(\251\184`l\226\229\184\FSG\132\217\224\202\222\249\246J"
                        , inputIx = 1
                        }, Nothing)
                    ]
                , resolvedCollateralInputs = []
                , outputs =
                    [ TxOut
                        { address = Address "\130\216\CANXB\131X\FS\DC3\136\135t\199V\160\217\173\r\235\229\193\&03q{\178'\f\DLE\137k\222P\180\DC3\224\161\SOHX\RSX\FS\202>U<\156c\197<\211\197>C_\207\225?\146\134\160\ETB\207!X\139\250N\220\ESC\NUL\SUB\186\217]\175"
                        , tokens = coinToBundle 3827577253906
                        }
                    , TxOut
                        { address = Address "\130\216\CANXB\131X\FS\167\219!{\ETX\157lP>i~\158\225\DEL\141!.I\248\"\183(\DC13\231\185pU\161\SOHX\RSX\FS\SOH\131\136&\ESC\236\240\200\rw\255.\153\252\&6'\174\159vs\CAN\255\153\USf\155\173\223\NUL\SUB\214\237\RS\248"
                        , tokens = coinToBundle 16837395907
                        }
                    ]
                , collateralOutput = Nothing
                , withdrawals = mempty
                , metadata = Nothing
                , scriptValidity = Nothing
                }
            , Tx
                { txId = Hash "611ce641f0f9282a35b1678fcd996016833c0de9e83a04bfa1178c8f045196ea"
                , txCBOR = Nothing
                , fee = Nothing
                , resolvedInputs =
                    [ ( TxIn
                        { inputId = Hash "\151\146\133\SYN\187\ENQ\252\226\&4\210n\153\178+.h\200\CANAs\SI\181\189\GS\131[g7O\GS\232\215"
                        , inputIx = 1
                        }, Nothing)
                    ]
                , resolvedCollateralInputs = []
                , outputs =
                    [ TxOut
                        { address = Address "\130\216\CANXB\131X\FS!f\151\SYN\189\218\167\236\206\253\&9UW%\CAN\238\139\205<\246\132\&1\SOH\164\SUBR\237\DC4\161\SOHX\RSX\FS\202>U<\156c\197T\188\198\219C5_\246\194@\227\217\151\235\139\216(2p\173\236\NUL\SUB0\147sX"
                        , tokens = coinToBundle 3843675297120
                        }
                    , TxOut
                        { address = Address "\130\216\CANXB\131X\FS\ETBb\215X\172)\244\139Tp\DC4b\194\DC3\SUB\157\STXqr\172/\175q\244\153\140\214`\161\SOHX\RSX\FSCGQbc\253u+\vF\192XT\185\233e\150}\173\139\199\CAN\215\134\159\166\GS\216\NUL\SUBA}\137A"
                        , tokens = coinToBundle 748331810
                        }
                    ]
                , collateralOutput = Nothing
                , withdrawals = mempty
                , metadata = Nothing
                , scriptValidity = Nothing
                }
            ]
        , delegations = []
        }
    , Block
        { header = BlockHeader
            { slotNo = slot 14 11
            , blockHeight = Quantity 302369
            , headerHash = Hash "unused"
            , parentHeaderHash = Just $ Hash "4fdff9f1d751dba5a48bc2a14d6dfb21709882a13dad495b856bf76d5adf4bd1"
            }
        , transactions =
            [ Tx
                { txId = Hash "0a5232f1683aaba994fb3774a5e123e2ed4f2842457b67b5309b825550a5f55d"
                , txCBOR = Nothing
                , fee = Nothing
                , resolvedInputs =
                    [ ( TxIn
                        { inputId = Hash "_>\240.\159\145\US\NUL1\158r\231\&8\214\241\134\&2\DC4\ETB\160\134\237z\143D\229d\DC4\245\208\DC3?"
                        , inputIx = 0
                        }, Nothing)
                    ]
                , resolvedCollateralInputs = []
                , outputs =
                    [ TxOut
                        { address = Address "\130\216\CANXB\131X\FS\233\219\220^Zp\135\EOT\205&#\226S\232\&0\160\252\164\&9\224\&2\152\RS\197F\191\193\223\161\SOHX\RSX\FS\202>U<\156c\197\&5\201\210\140C\v\216\253\150\235\177\189*\211E\241\201;L;t\NUL\SUB||\158\&1"
                        , tokens = coinToBundle 3842710635646
                        }
                    , TxOut
                        { address = Address "\130\216\CANXB\131X\FS\197\251\223.\192>\179\168\236}\242\180\188$\173\161\229\165\157#\190\USo{]BO\191\161\SOHX\RSX\FSA\162\195Z4\CANj\174\148\160\&34\USo\ETB\179\a\133Te\ACK\131\182y\248\236\211c\NUL\SUB\225\153\247\212"
                        , tokens = coinToBundle 1499800000
                        }
                    ]
                , collateralOutput = Nothing
                , withdrawals = mempty
                , metadata = Nothing
                , scriptValidity = Nothing
                }
            , Tx
                { txId = Hash "b8e9699ffff40c993d6778f586110b78cd30826feaa5314adf3a2e9894b9313a"
                , txCBOR = Nothing
                , fee = Nothing
                , resolvedInputs =
                    [ ( TxIn
                        { inputId = Hash "\187\177J\189\132K\n\175\130\148\&3[\150\193zL\153\191Qjcl\n\162B\241G)>\151\DC4\225"
                        , inputIx = 0
                        }, Nothing)
                    ]
                , resolvedCollateralInputs = []
                , outputs =
                    [ TxOut
                        { address = Address "\130\216\CANXB\131X\FS\203\242{\247\221*[\182a\171/`\151,\130\&4\246\219\245I\t\240\&6\ACK\159wg\186\161\SOHX\RSX\FS\202>U<\156c\197\CAN\250\154\238C \170\214\202\244y\140!\189\SYN]\157\132\ETXt\245\NUL\SUB\155\210\\\173"
                        , tokens = coinToBundle 3842940911894
                        }
                    , TxOut
                        { address = Address "\130\216\CANXB\131X\FS~\133V\SYN\DEL\211\165\ACK\239\a\182\131\143'\253On\210d\169kc\145\179\156\142\230\140\161\SOHX\RSX\FS\179@nvQ\155\209\149n\214\226y\166\133\170\207\134\131t\219\&7&\246m_Jv\DC2\NUL\SUB\218\132l\235"
                        , tokens = coinToBundle 1345293520
                        }
                    ]
                , collateralOutput = Nothing
                , withdrawals = mempty
                , metadata = Nothing
                , scriptValidity = Nothing
                }
            ]
        , delegations = []
        }
    , Block
        { header = BlockHeader
            { slotNo = slot 14 12
            , blockHeight = Quantity 302370
            , headerHash = Hash "unused"
            , parentHeaderHash = Just $ Hash "96a31a7cdb410aeb5756ddb43ee2ddb4c682f6308db38310ab54bf38b89d6b0d"
            }
        , transactions = []
        , delegations = []
        }
    , Block
        { header = BlockHeader
            { slotNo = slot 14 13
            , blockHeight = Quantity 302371
            , headerHash = Hash "unused"
            , parentHeaderHash = Just $ Hash "47c08c0a11f66aeab915e5cd19362e8da50dc2523e629b230b73ec7b6cdbeef8"
            }
        , delegations = []
        , transactions = []
        }
    , Block
        { header = BlockHeader
            { slotNo = slot 14 14
            , blockHeight = Quantity 302372
            , headerHash = Hash "unused"
            , parentHeaderHash = Just $ Hash "d6d7e79e2a25f53e6fb771eebd1be05274861004dc62c03bf94df03ff7b87198"
            }
        , delegations = []
        , transactions = []
        }
    , Block
        { header = BlockHeader
            { slotNo = slot 14 15
            , blockHeight = Quantity 302373
            , headerHash = Hash "unused"
            , parentHeaderHash = Just $ Hash "647e62b29ebcb0ecfa0b4deb4152913d1a669611d646072d2f5898835b88d938"
            }
        , delegations = []
        , transactions = []
        }
    , Block
        { header = BlockHeader
            { slotNo = slot 14 16
            , blockHeight = Quantity 302374
            , headerHash = Hash "unused"
            , parentHeaderHash = Just $ Hash "02f38ce50c9499f2526dd9c5f9e8899e65c0c40344e14ff01dc6c31137978efb"
            }
        , delegations = []
        , transactions = []
        }
    , Block
        { header = BlockHeader
            { slotNo = slot 14 17
            , blockHeight = Quantity 302375
            , headerHash = Hash "unused"
            , parentHeaderHash = Just $ Hash "528492ded729ca77a72b1d85654742db85dfd3b68e6c4117ce3c253e3e86616d"
            }
        , delegations = []
        , transactions = []
        }
    , Block
        { header = BlockHeader
            { slotNo = slot 14 18
            , blockHeight = Quantity 302376
            , headerHash = Hash "unused"
            , parentHeaderHash = Just $ Hash "f4283844eb78ca6f6333b007f5a735d71499d6ce7cc816846a033a36784bd299"
            }
        , transactions =
            [ Tx
                { txId = Hash "387b98d4cde710b78ce19e9077b61d6b7a2dff52fd16dcb884b31cd576904c86"
                , txCBOR = Nothing
                , fee = Nothing
                , resolvedInputs =
                    [ ( TxIn
                        { inputId = Hash "\150\225pI\SUB\251n\189W\159\213|v\198\132\242$6\248\204:\145#\151\221\177\201\197\ESC\134\251S"
                        , inputIx = 0
                        }, Nothing)
                    ]
                , resolvedCollateralInputs = []
                , outputs =
                    [ TxOut
                        { address = Address "\130\216\CANXB\131X\FS\197\CAN\DELP\160W\144\&8\GSW\189\&7m\b\233Y\216I\176\159\250\144\EM\155|\219\n\231\161\SOHX\RSX\FS\202>U<\156c\197\&6\149=XC\217L\SOH\255\166\228\138\221\157\&0\ACK&]`z\DC2\NUL\SUB\149\157\191\162"
                        , tokens = coinToBundle 3832107959251
                        }
                    , TxOut
                        { address = Address "\130\216\CANXB\131X\FSI\SI\165\f\DLE\223\214\209\206\187y\128F\SUB\248.\203\186/\244\143m1]\n\132\234\"\161\SOHX\RSX\FSv\SI\240\133L\130\194\DC2\191}\189;5\141\252t]\132}[\244\ESC&\SI\EOT[{\238\NUL\SUB\159\236eZ"
                        , tokens = coinToBundle 11823271860
                        }
                    ]
                , collateralOutput = Nothing
                , withdrawals = mempty
                , metadata = Nothing
                , scriptValidity = Nothing
                }
            , Tx
                { txId = Hash "7726526b5cc003f71d9629c611397285004b5438eac9a118c2b20e2810e0783e"
                , txCBOR = Nothing
                , fee = Nothing
                , resolvedInputs =
                    [ ( TxIn
                        { inputId = Hash "\249\DC2\146\&0\GSK\177\182\224@\206\205\255@0\149\155I\201^}\174\bw\130\221U\139\235\182f\138"
                        , inputIx = 0
                        }, Nothing)
                    ]
                , resolvedCollateralInputs = []
                , outputs =
                    [ TxOut
                        { address = Address "\130\216\CANXB\131X\FSe$;\SO\178g\161\226>1w\159M\NAK\141d\173\210\202\192Bn\250\176C(\DC2\ENQ\161\SOHX\RSX\FS\202>U<\156c\197\SUB\225\157\&1C\209\253\183\USuz\163\193\209\196\217:\155!\167!\NUL\SUB\137\240\187\159"
                        , tokens = coinToBundle 3841254542346
                        }
                    , TxOut
                        { address = Address "\130\216\CANXB\131X\FS\161\243^\nQ`\DLE\151\147n\153j\STX\215]\SOr7\136\211\222y\US*\157%\DEL\ETB\161\SOHX\RSX\FS\201\SUB\170\156Oe\155)D\US\143\CAN\237\193\244vKM\160\SOH\166&\161\213\188KD\142\NUL\SUB\144\192\240\146"
                        , tokens = coinToBundle 2700667457
                        }
                    ]
                , collateralOutput = Nothing
                , withdrawals = mempty
                , metadata = Nothing
                , scriptValidity = Nothing
                }
            ]
        , delegations = []
        }
    , Block
        { header = BlockHeader
            { slotNo = slot 14 19
            , blockHeight = Quantity 302377
            , headerHash = Hash "unused"
            , parentHeaderHash = Just $ Hash "dffc3506d381361468376227e1c9323a2ffc76011103e3225124f08e6969a73b"
            }
        , transactions =
            [ Tx
                { txId = Hash "9c6fed8fef3b296d4dee6e62ca72b180bf0ed1c13eb5f0445099b2a146235e77"
                , txCBOR = Nothing
                , fee = Nothing
                , resolvedInputs =
                    [ (TxIn
                        { inputId = Hash "\194\157>\160\221\163\&4\218\149\215\178\161]p\185\246\208\198\ENQ \188\216\242\160\190\236\137\151\DC3\134\"\DC4"
                        , inputIx = 0
                        }, Nothing)
                    ]
                , resolvedCollateralInputs = []
                , outputs =
                    [ TxOut
                        { address = Address "\130\216\CANXB\131X\FS\147\ACKn\246.n\DLE\233Y\166)\207c\v\248\183\235\212\EOTV\243h\192\190T\150'\196\161\SOHX\RSX\FS\202>U<\156c\197&\DC3S\235C\198\245\163\204=\214fa\201\t\205\248\204\226r%\NUL\SUB\174\187\&7\t"
                        , tokens = coinToBundle 3823755953610
                        }
                    , TxOut
                        { address = Address "\130\216\CANXB\131X\FS\ACK\218k\189\250\189\129\229A\128>`V\153\144EyN\187T\\\151 \171;\251(\t\161\SOHX\RSX\FS\197\217I\176.##'\217l\226i{\200'\176\&32I\150\166\SI+\143\138\GS\SOH+\NUL\SUB7\206\156`"
                        , tokens = coinToBundle 19999800000
                        }
                    ]
                , collateralOutput = Nothing
                , withdrawals = mempty
                , metadata = Nothing
                , scriptValidity = Nothing
                }
            ]
        , delegations = []
        }

    -- After this point, all blocks and transactions are constructed by hand,
    -- in order to simulate various interesting scenarios:

    , Block
        { header = BlockHeader
            { slotNo = slot 14 20
            , blockHeight = Quantity 302378
            , headerHash = Hash "unused"
            , parentHeaderHash = Just $ Hash "unused"
            }
        , transactions =
            -- This transaction is marked as having an invalid script.
            -- It spends a single collateral input and creates a single
            -- collateral output:
            [ Tx
                { txId = Hash "tx-create-collateral-output"
                , txCBOR = Nothing
                , fee = Just (Coin 1)
                , resolvedInputs =
                    [   ( TxIn
                            { inputId = Hash "9c6fed8fef3b296d4dee6e62ca72b180bf0ed1c13eb5f0445099b2a146235e77"
                            , inputIx = 0
                            }
                        , Just $ TxOut
                            { address = Address "\130\216\CANXB\131X\FS\147\ACKn\246.n\DLE\233Y\166)\207c\v\248\183\235\212\EOTV\243h\192\190T\150'\196\161\SOHX\RSX\FS\202>U<\156c\197&\DC3S\235C\198\245\163\204=\214fa\201\t\205\248\204\226r%\NUL\SUB\174\187\&7\t"
                            , tokens = coinToBundle 3823755953610
                            }
                        )
                    ]
                , resolvedCollateralInputs =
                    [   ( TxIn
                            { inputId = Hash "9c6fed8fef3b296d4dee6e62ca72b180bf0ed1c13eb5f0445099b2a146235e77"
                            , inputIx = 1
                            }
                        , Nothing
                        )
                    ]
                , outputs =
                    [ TxOut
                        { address = Address "\130\216\CANXB\131X\FS\147\ACKn\246.n\DLE\233Y\166)\207c\v\248\183\235\212\EOTV\243h\192\190T\150'\196\161\SOHX\RSX\FS\202>U<\156c\197&\DC3S\235C\198\245\163\204=\214fa\201\t\205\248\204\226r%\NUL\SUB\174\187\&7\t"
                        , tokens = coinToBundle (3823755953610 - 1)
                        }
                    ]
                , collateralOutput = Just
                    TxOut
                        { address = Address "\130\216\CANXB\131X\FS\147\ACKn\246.n\DLE\233Y\166)\207c\v\248\183\235\212\EOTV\243h\192\190T\150'\196\161\SOHX\RSX\FS\202>U<\156c\197&\DC3S\235C\198\245\163\204=\214fa\201\t\205\248\204\226r%\NUL\SUB\174\187\&7\t"
                        , tokens = coinToBundle (19999800000 - 1)
                        }
                , withdrawals = mempty
                , metadata = Nothing
                , scriptValidity = Just TxScriptInvalid
                }
            ]
        , delegations = []
        }

    , Block
        { header = BlockHeader
            { slotNo = slot 14 21
            , blockHeight = Quantity 302379
            , headerHash = Hash "unused"
            , parentHeaderHash = Just $ Hash "unused"
            }
        , transactions =
            -- This transaction spends a single collateral output that was
            -- created in the previous transaction:
            [ Tx
                { txId = Hash "tx-spend-collateral-output"
                , txCBOR = Nothing
                , fee = Just (Coin 1)
                , resolvedInputs =
                    [   ( TxIn
                            { inputId = Hash "tx-create-collateral-output"
                            -- The previous transaction defined exactly one
                            -- ordinary output, so we use 1 as the index of
                            -- the collateral output:
                            , inputIx = 1
                            }
                        , Just $ TxOut
                            { address = Address "\130\216\CANXB\131X\FS\147\ACKn\246.n\DLE\233Y\166)\207c\v\248\183\235\212\EOTV\243h\192\190T\150'\196\161\SOHX\RSX\FS\202>U<\156c\197&\DC3S\235C\198\245\163\204=\214fa\201\t\205\248\204\226r%\NUL\SUB\174\187\&7\t"
                            , tokens = coinToBundle (19999800000 - 1)
                            }
                        )
                    ]
                , resolvedCollateralInputs = []
                , outputs =
                    [ TxOut
                        { address = Address "\130\216\CANXB\131X\FS\147\ACKn\246.n\DLE\233Y\166)\207c\v\248\183\235\212\EOTV\243h\192\190T\150'\196\161\SOHX\RSX\FS\202>U<\156c\197&\DC3S\235C\198\245\163\204=\214fa\201\t\205\248\204\226r%\NUL\SUB\174\187\&7\t"
                        , tokens = coinToBundle (19999800000 - 2)
                        }
                    ]
                , collateralOutput = Nothing
                , withdrawals = mempty
                , metadata = Nothing
                , scriptValidity = Just TxScriptValid
                }
            ]
        , delegations = []
        }
    ]
  where
    slot e s = SlotNo $ flatSlot (EpochLength 21600) (SlotId e s)

prop_tx_utxo_coverage :: Tx -> UTxO -> Property
prop_tx_utxo_coverage tx u =
    checkCoverage $
    cover 2 (UTxO.null u)
        "UTxO empty" $
    cover 30 (not $ UTxO.null u)
        "UTxO not empty" $
    cover 30 (not $ Set.disjoint (dom u) (Set.fromList $ inputs tx))
        "UTxO and Tx not disjoint" $
    cover 10 (Set.disjoint (dom u) (Set.fromList $ inputs tx))
        "UTxO and Tx disjoint" $
    cover 4 (length (inputs tx) > 3)
        "Number of tx inputs > 3" $
    cover 4 (length (inputs tx) < 3)
        "Number of tx inputs < 3" $
    cover 4 (length (outputs tx) > 3)
        "Number of tx outputs > 3" $
    cover 4 (length (outputs tx) < 3)
        "Number of tx outputs < 3" $
    property True

prop_applyTxToUTxO_balance :: Tx -> UTxO -> Property
prop_applyTxToUTxO_balance tx u =
    checkCoverage $
    cover 0.1
        (applyTxToUTxO tx u == u)
        "applyTxToUTxO tx u == u" $
    cover 10
        (applyTxToUTxO tx u /= u)
        "applyTxToUTxO tx u /= u" $
    cover 10
        (txScriptInvalid tx)
        "txScriptInvalid tx" $
    cover 10
        (not $ txScriptInvalid tx)
        "not $ txScriptInvalid tx" $
    balance (applyTxToUTxO tx u) === expectedBalance
  where
    expectedBalance =
        balance (utxoFromTx tx) <>
        balance (u `excluding` inputsSpentByTx tx)

prop_applyTxToUTxO_entries :: Tx -> UTxO -> Property
prop_applyTxToUTxO_entries tx u =
    checkCoverage $
    cover 0.1
        (applyTxToUTxO tx u == u)
        "applyTxToUTxO tx u == u" $
    cover 10
        (applyTxToUTxO tx u /= u)
        "applyTxToUTxO tx u /= u" $
    cover 10
        (txScriptInvalid tx)
        "txScriptInvalid tx" $
    cover 10
        (not $ txScriptInvalid tx)
        "not $ txScriptInvalid tx" $
    applyTxToUTxO tx u === expectedResult
  where
    expectedResult = (u `excluding` inputsSpentByTx tx) <> utxoFromTx tx

prop_filterByAddress_balance_applyTxToUTxO
    :: (Address -> Bool) -> Tx -> Property
prop_filterByAddress_balance_applyTxToUTxO f tx =
    checkCoverage $
    cover 0.1
        (filterByAddress f (applyTxToUTxO tx mempty) == mempty)
        "filterByAddress f (applyTxToUTxO tx mempty) == mempty" $
    cover 10
        (filterByAddress f (applyTxToUTxO tx mempty) /= mempty)
        "filterByAddress f (applyTxToUTxO tx mempty) /= mempty" $
    cover 10
        (txScriptInvalid tx)
        "txScriptInvalid tx" $
    cover 10
        (not $ txScriptInvalid tx)
        "not $ txScriptInvalid tx" $
    balance (filterByAddress f (applyTxToUTxO tx mempty))
    ===
    expectedResult
  where
    expectedResult = F.foldMap m (outputsCreatedByTx tx)
      where
        m output =
            if f (address output)
            then tokens output
            else mempty

unit_applyTxToUTxO_spends_input :: Tx -> TxIn -> TxOut -> Maybe TxOut -> Property
unit_applyTxToUTxO_spends_input tx txin txout resolvedOut =
    let
        tx' = tx
            { resolvedInputs = [(txin, resolvedOut)]
            , scriptValidity = Nothing
            }
    in
        applyTxToUTxO tx' (UTxO $ Map.fromList [(txin, txout)])
        === utxoFromTx tx' `excluding` Set.singleton txin

unit_applyTxToUTxO_scriptValidity_Valid
    :: Tx -> (TxIn, TxOut) -> (TxIn, TxOut) -> Property
unit_applyTxToUTxO_scriptValidity_Valid tx'
    (sIn, sOut) (cIn, cOut) =
    let tx = tx'
            { resolvedCollateralInputs = [(cIn, Nothing)]
            , resolvedInputs = [(sIn, Nothing)]
            , scriptValidity = Just TxScriptValid
            }
        utxo = UTxO $ Map.fromList [(sIn, sOut), (cIn, cOut)]
    in
    applyTxToUTxO tx utxo
        ===
        (utxo `excluding` Set.singleton sIn) <> utxoFromTxOutputs tx

unit_applyTxToUTxO_scriptValidity_Invalid
    :: Tx -> (TxIn, TxOut) -> (TxIn, TxOut) -> Property
unit_applyTxToUTxO_scriptValidity_Invalid tx' (sIn, sOut) (cIn, cOut) =
    let tx = tx'
            { resolvedCollateralInputs = [(cIn, Nothing)]
            , resolvedInputs = [(sIn, Nothing)]
            , scriptValidity = Just TxScriptInvalid
            }
        utxo = UTxO $ Map.fromList [(sIn, sOut), (cIn, cOut)]
    in
    applyTxToUTxO tx utxo
        ===
        (utxo `excluding` Set.singleton cIn) <> utxoFromTxCollateralOutputs tx

unit_applyTxToUTxO_scriptValidity_Unknown
    :: Tx -> (TxIn, TxOut) -> (TxIn, TxOut) -> Property
unit_applyTxToUTxO_scriptValidity_Unknown tx' (sIn, sOut) (cIn, cOut) =
    let tx = tx'
            { resolvedCollateralInputs = [(cIn, Nothing)]
            , resolvedInputs = [(sIn, Nothing)]
            , scriptValidity = Nothing
            }
        utxo = UTxO $ Map.fromList [(sIn, sOut), (cIn, cOut)]
    in
    applyTxToUTxO tx utxo
        ===
        (utxo `excluding` Set.singleton sIn) <> utxoFromTxOutputs tx

--------------------------------------------------------------------------------
-- utxoFromTx
--------------------------------------------------------------------------------

prop_utxoFromTx_balance :: Tx -> Property
prop_utxoFromTx_balance tx =
    checkCoverage $
    cover 10
        (txHasOutputsAndCollateralOutputs tx)
        "txHasOutputsAndCollateralOutputs tx" $
    cover 10
        (txScriptInvalid tx)
        "txScriptInvalid tx)" $
    cover 10
        (not $ txScriptInvalid tx)
        "not $ txScriptInvalid tx)" $
    balance (utxoFromTx tx) === F.foldMap tokens (outputsCreatedByTx tx)

prop_utxoFromTx_size :: Tx -> Property
prop_utxoFromTx_size tx =
    checkCoverage $
    cover 10
        (txHasOutputsAndCollateralOutputs tx)
        "txHasOutputsAndCollateralOutputs tx" $
    cover 10
        (txScriptInvalid tx)
        "txScriptInvalid tx)" $
    cover 10
        (not $ txScriptInvalid tx)
        "not $ txScriptInvalid tx)" $
    UTxO.size (utxoFromTx tx) === F.length (outputsCreatedByTx tx)

prop_utxoFromTx_values :: Tx -> Property
prop_utxoFromTx_values tx =
    checkCoverage $
    cover 10
        (txHasOutputsAndCollateralOutputs tx)
        "txHasOutputsAndCollateralOutputs tx" $
    cover 10
        (txScriptInvalid tx)
        "txScriptInvalid tx)" $
    cover 10
        (not $ txScriptInvalid tx)
        "not $ txScriptInvalid tx)" $
    F.toList (unUTxO (utxoFromTx tx)) === outputsCreatedByTx tx

prop_utxoFromTx_disjoint :: Tx -> Property
prop_utxoFromTx_disjoint tx =
    checkCoverage $
    cover 10
        (txHasOutputsAndCollateralOutputs tx)
        "txHasOutputsAndCollateralOutputs tx" $
    UTxO.disjoint
        (utxoFromTxOutputs tx)
        (utxoFromTxCollateralOutputs tx)

prop_utxoFromTx_union :: Tx -> Property
prop_utxoFromTx_union tx =
    checkCoverage $
    cover 10
        (txHasOutputsAndCollateralOutputs tx)
        "txHasOutputsAndCollateralOutputs tx" $
    mappend
        (utxoFromTxOutputs tx)
        (utxoFromTxCollateralOutputs tx)
        ===
        (utxoFromTxOutputsAndCollateralOutputs tx)

txHasOutputs :: Tx -> Bool
txHasOutputs = not . null . outputs

txHasCollateralOutputs :: Tx -> Bool
txHasCollateralOutputs = not . null . collateralOutput

txHasOutputsAndCollateralOutputs :: Tx -> Bool
txHasOutputsAndCollateralOutputs tx =
    txHasOutputs tx && txHasCollateralOutputs tx

utxoFromTxOutputsAndCollateralOutputs :: Tx -> UTxO
utxoFromTxOutputsAndCollateralOutputs Tx {txId, outputs, collateralOutput} =
    UTxO $ Map.fromList $ zip
        (TxIn txId <$> [0..])
        (outputs <> F.toList collateralOutput)

--------------------------------------------------------------------------------
-- spendTx
--------------------------------------------------------------------------------

-- spendTx tx u `isSubsetOf` u
prop_spendTx_isSubset :: Tx -> UTxO -> Property
prop_spendTx_isSubset tx u =
    checkCoverage $
    cover 10 isNonEmptyProperSubmap "isNonEmptyProperSubmap" $
    property $ spendTx tx u `UTxO.isSubsetOf` u
  where
    isNonEmptyProperSubmap = (&&)
        (spendTx tx u /= mempty)
        (unUTxO (spendTx tx u) `Map.isProperSubmapOf` unUTxO u)

-- balance (spendTx tx u) <= balance u
prop_spendTx_balance_inequality :: Tx -> UTxO -> Property
prop_spendTx_balance_inequality tx u =
    checkCoverage $
    cover 10
        (lhs /= mempty && lhs `leq` rhs && lhs /= rhs)
        "lhs /= mempty && lhs `leq` rhs && lhs /= rhs" $
    isJust (rhs `TokenBundle.subtract` lhs)
        & counterexample ("balance (spendTx tx u) = " <> show lhs)
        & counterexample ("balance u = " <> show rhs)
  where
    lhs = balance (spendTx tx u)
    rhs = balance u

prop_spendTx_balance :: Tx -> UTxO -> Property
prop_spendTx_balance tx u =
    checkCoverage $
    cover 10
        (lhs /= mempty && rhs /= mempty)
        "lhs /= mempty && rhs /= mempty" $
    cover 10
        (txScriptInvalid tx)
        "txScriptInvalid tx" $
    cover 10
        (not $ txScriptInvalid tx)
        "not $ txScriptInvalid tx" $
    lhs === rhs
  where
    lhs = balance (spendTx tx u)
    rhs = TokenBundle.unsafeSubtract
        (balance u)
        (balance (u `UTxO.restrictedBy` inputsSpentByTx tx))

prop_spendTx :: Tx -> UTxO -> Property
prop_spendTx tx u =
    checkCoverage $
    cover 10
        (spendTx tx u /= mempty)
        "spendTx tx u /= mempty" $
    cover 10
        (txScriptInvalid tx)
        "txScriptInvalid tx" $
    cover 10
        (not $ txScriptInvalid tx)
        "not $ txScriptInvalid tx" $
    spendTx tx u === u `excluding` inputsSpentByTx tx

prop_spendTx_utxoFromTx :: Tx -> UTxO -> Property
prop_spendTx_utxoFromTx tx u =
    spendTx tx (u <> utxoFromTx tx) === spendTx tx u <> utxoFromTx tx

prop_applyTxToUTxO_spendTx_utxoFromTx :: Tx -> UTxO -> Property
prop_applyTxToUTxO_spendTx_utxoFromTx tx u =
    checkCoverage $
    cover 10
        (spendTx tx u /= mempty && utxoFromTx tx /= mempty)
        "spendTx tx u /= mempty && utxoFromTx tx /= mempty" $
    applyTxToUTxO tx u === spendTx tx u <> utxoFromTx tx

prop_spendTx_filterByAddress :: (Address -> Bool) -> Tx -> UTxO -> Property
prop_spendTx_filterByAddress f tx u =
    checkCoverage $
    cover 10
        (spendTx tx u /= mempty && filterByAddress f u /= mempty)
        "spendTx tx u /= mempty && filterByAddress f u /= mempty" $
    filterByAddress f (spendTx tx u) === spendTx tx (filterByAddress f u)

deriving anyclass instance CoArbitrary Address
deriving anyclass instance CoArbitrary RewardAccount
deriving anyclass instance Function Address
deriving anyclass instance Function RewardAccount

instance Show (Address -> Bool) where
    show = const "(Address -> Bool)"

instance Show (RewardAccount -> Bool) where
    show = const "(RewardAccount -> Bool)"

--------------------------------------------------------------------------------
-- Utility functions
--------------------------------------------------------------------------------

-- | Returns the inputs that a transaction should spend, based on the
--   transaction's script validation status.
--
inputsSpentByTx :: Tx -> Set TxIn
inputsSpentByTx tx
    | txScriptInvalid tx =
        Set.fromList (collateralInputs tx)
    | otherwise =
        Set.fromList (inputs tx)

-- | Returns the outputs that a transaction should create, based on the
--   transaction's script validation status.
--
-- Note that the indices are not returned. If it's important to obtain the
-- indices, then use function 'utxoFromTx'.
--
outputsCreatedByTx :: Tx -> [TxOut]
outputsCreatedByTx tx
    | txScriptInvalid tx =
        F.toList (collateralOutput tx)
    | otherwise =
        outputs tx

--------------------------------------------------------------------------------
-- Generating and shrinking arbitrary sequences of blocks
--------------------------------------------------------------------------------

-- | A sequence of blocks that starts at a given block height and slot number.
--
-- Values of this type model a sequence of state transitions affecting the
-- global UTxO set:
--
-- @
--    global_utxo_0 -> block_0_1 ->
--    global_utxo_1 -> block_1_2 ->
--    ...
--    global_utxo_i -> block_i_j ->
--    global_utxo_j
-- @
--
-- In turn, each block contains a series of transactions that each affect the
-- global UTxO set. The effect of applying a single block to the global UTxO
-- set is equivalent to applying all of the contained transactions in order
-- to the global UTxO set.
--
-- In general, only a subset of transactions within a block sequence will be
-- relevant to a particular wallet, and only a subset of entries within each
-- successive iteration of the global UTxO set will be relevant to that wallet.
--
-- Values of 'BlockSeq' are more convenient to generate and shrink than values
-- of 'BlockData', but can easily be transformed into values of 'BlockData' in
-- order to test properties of the 'applyBlocks' function.
--
-- Usage:
--
--  1. Use function 'genBlockSeq' to generate a 'BlockSeq' value.
--  2. Use function 'blockSeqToBlockData' to convert a 'BlockSeq' value to a
--     value of 'BlockData' suitable for use with the 'applyBlocks' function.
--  3. Use function 'shrinkBlockSeq' to shrink to a minimal counterexample.
--
data BlockSeq = BlockSeq
    { initialBlockHeight
        :: Quantity "block" Word32
    , initialSlotNo
        :: SlotNo
    , shrinkableTxSeq
        :: ShrinkableTxSeq
    }
    deriving (Eq, Generic, Show)

instance Arbitrary BlockSeq where
    arbitrary = genBlockSeq
    shrink = shrinkBlockSeq

genBlockSeq :: Gen BlockSeq
genBlockSeq = BlockSeq
    <$> fmap toEnum (choose (0, 100))
    <*> fmap toEnum (choose (0, 100))
    <*> genTxSeq genUTxO genAddress

shrinkBlockSeq :: BlockSeq -> [BlockSeq]
shrinkBlockSeq = genericRoundRobinShrink
    <@> shrinkMap toEnum fromEnum
    <:> shrinkMap toEnum fromEnum
    <:> shrinkTxSeq
    <:> Nil

-- | Converts a 'BlockSeq' to a value of 'BlockData' that is suitable for use
--   with the 'applyBlocks' function.
--
-- This function fills every slot with exactly one block, leaving no gaps.
--
blockSeqToBlockData :: BlockSeq -> BlockData m addr tx state
blockSeqToBlockData = List . blockSeqToBlockList
  where
    blockSeqToBlockList :: BlockSeq -> NonEmpty Block
    blockSeqToBlockList blockSeq =
        NE.fromList $ getZipList $ makeBlock
            <$> ZipList (enumFrom (blockSeq & initialBlockHeight))
            <*> ZipList (enumFrom (blockSeq & initialSlotNo))
            <*> ZipList (NE.toList (TxSeq.toTxGroupList txSeq))
      where
        txSeq :: TxSeq
        txSeq = blockSeqToTxSeq blockSeq

        -- Makes a block using dummy values for fields that are not relevant
        -- to our expectations of 'applyBlocks'.
        --
        makeBlock :: Quantity "block" Word32 -> SlotNo -> [Tx] -> Block
        makeBlock blockHeight slotNo transactions = Block
            { header = BlockHeader
                { blockHeight
                , slotNo
                , headerHash = Hash ""
                , parentHeaderHash = Nothing
                }
            , transactions
            , delegations = []
            }

-- | Retrieves the head UTxO of a block sequence.
--
-- The head UTxO represents the initial state of the /global/ UTxO set before
-- any of the blocks in the given block sequence are applied.
--
blockSeqHeadUTxO :: BlockSeq -> UTxO
blockSeqHeadUTxO = TxSeq.headUTxO . blockSeqToTxSeq

-- | Retrieves the last UTxO of a block sequence.
--
-- The last UTxO represents the final state of the /global/ UTxO set after
-- all of the blocks in the given block sequence have been applied.
--
blockSeqLastUTxO :: BlockSeq -> UTxO
blockSeqLastUTxO = TxSeq.lastUTxO . blockSeqToTxSeq

-- | Retrieves, from a block sequence, the complete list of transactions that
--   are expected to be relevant to a particular wallet.
--
-- The returned transactions are listed in the same order that they appear
-- within the block sequence.
--
blockSeqOurTxs
    :: forall s. (IsOurs s Address, IsOurs s RewardAccount)
    => s
    -> BlockSeq
    -> [Tx]
blockSeqOurTxs s0 blockSeq = blockSeq
    & blockSeqToTxSeq
    & TxSeq.toTransitionList
    & filterM isOurTransitionM
    & flip evalState s0
    & fmap (\(_, tx, _) -> tx)
  where
    isOurAddressM :: Address -> State s Bool
    isOurAddressM = fmap isJust . state . isOurs

    isOurTransitionM :: (UTxO, Tx, UTxO) -> State s Bool
    isOurTransitionM (u, tx, _) =
        -- Here we use the test function 'isOurTx', which is not used by the
        -- implementation of 'applyBlocks'.
        isOurTx tx =<< UTxO.filterByAddressM isOurAddressM u

-- | Extracts a 'TxSeq' from a 'BlockSeq'.
--
-- The resulting 'TxSeq' does retain knowledge of block boundaries, but does
-- not have any knowledge of block heights or slot numbers.
--
blockSeqToTxSeq :: BlockSeq -> TxSeq
blockSeqToTxSeq BlockSeq {shrinkableTxSeq} = getTxSeq shrinkableTxSeq

--------------------------------------------------------------------------------
-- Convenience functions for calling 'applyBlocks' and processing the result
--------------------------------------------------------------------------------

-- | Embeds a 'UTxO' set within a 'Wallet' suitable for use with 'applyBlocks'.
--
utxoToWallet :: s -> UTxO -> Wallet s
utxoToWallet s u = unsafeInitWallet u currentTip s
  where
    -- The 'currentTip' field is not currently used by 'applyBlocks'.
    --
    currentTip :: BlockHeader
    currentTip = shouldNotEvaluate "currentTip"

    shouldNotEvaluate :: String -> a
    shouldNotEvaluate name = error $ unwords
        [name, "was unexpectedly evaluated"]

-- | Extracts a list of filtered transactions from the result of 'applyBlocks'.
--
-- The returned transactions are listed in the same order that they appear
-- within the filtered blocks.
--
applyBlocksFilteredTxs
    :: NonEmpty ([FilteredBlock], (DeltaWallet s, Wallet s)) -> [Tx]
applyBlocksFilteredTxs = mconcat . mconcat . NE.toList
    . fmap (fmap (fmap fst . view #transactions) . fst)

-- | Extracts out the last UTxO set returned by 'applyBlocks'.
--
-- This UTxO set represents the final state of the wallet after applying
-- just those transactions that are relevant to the wallet.
--
applyBlocksLastUTxO
    :: NonEmpty ([FilteredBlock], (DeltaWallet s, Wallet s)) -> UTxO
applyBlocksLastUTxO = utxo . snd . snd . NE.last

--------------------------------------------------------------------------------
-- Convenience functions for manipulating UTxO sets
--------------------------------------------------------------------------------

-- | Filters a UTxO set for entries that are expected to be relevant to a
--   particular wallet.
--
ourUTxO :: forall s. (IsOurs s Address) => s -> UTxO -> UTxO
ourUTxO s u = evalState (UTxO.filterByAddressM isOurAddressM u) s
  where
    isOurAddressM :: Address -> State s Bool
    isOurAddressM = fmap isJust . state . isOurs

--------------------------------------------------------------------------------
-- Verifying transactions returned by 'applyBlocks'
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- The following properties verify that 'applyBlocks':
--
-- - returns all transactions that are relevant to the wallet, and no others.
-- - returns transactions in the same order that they appear on the blockchain.
--------------------------------------------------------------------------------

-- Scenario: some transactions within the block sequence are expected to be
-- relevant to the wallet, but not all.
--
prop_applyBlocks_filteredTxs_someOurs
    :: Pretty (IsOursIf2 Address RewardAccount)
    -> Pretty BlockSeq
    -> Property
prop_applyBlocks_filteredTxs_someOurs (Pretty someOurs) (Pretty blockSeq)
    -- TODO: we currently ignore transaction fees, as under some circumstances
    -- the 'applyBlocks' function can modify fee values:
    = fmap (set #fee Nothing) ourTxsReturned ====
      fmap (set #fee Nothing) ourTxsExpected
    & labelInterval 10
        "length allTxsProvided"
        (length allTxsProvided)
    & labelInterval 10
        "length ourTxsExpected"
        (length ourTxsExpected)
    & cover 10
        (ourTxsExpected `nonNullProperSubsequenceOf` allTxsProvided)
        "ourTxsExpected `nonNullProperSubsequenceOf` allTxsProvided"
    & checkCoverage
  where
    ourHeadUTxOProvided = ourUTxO someOurs $ blockSeqHeadUTxO blockSeq
    allTxsProvided = TxSeq.toTxList $ blockSeqToTxSeq blockSeq
    ourTxsExpected = blockSeqOurTxs someOurs blockSeq
    ourTxsReturned = applyBlocksFilteredTxs $ runIdentity $ applyBlocks
        (blockSeqToBlockData blockSeq)
        (utxoToWallet someOurs ourHeadUTxOProvided)

-- Scenario: all transactions within the block sequence are expected to be
-- relevant to the wallet.
--
prop_applyBlocks_filteredTxs_allOurs :: Pretty BlockSeq -> Property
prop_applyBlocks_filteredTxs_allOurs (Pretty blockSeq) =
    -- TODO: we currently ignore transaction fees, as under some circumstances
    -- the 'applyBlocks' function can modify fee values:
    fmap (set #fee Nothing) ourTxsReturned ====
    fmap (set #fee Nothing) ourTxsExpected
  where
    ourHeadUTxOProvided = blockSeqHeadUTxO blockSeq
    ourTxsExpected = blockSeqOurTxs AllOurs blockSeq
    ourTxsReturned = applyBlocksFilteredTxs $ runIdentity $ applyBlocks
        (blockSeqToBlockData blockSeq)
        (utxoToWallet AllOurs ourHeadUTxOProvided)

-- Scenario: no transactions within the block sequence are expected to be
-- relevant to the wallet.
--
prop_applyBlocks_filteredTxs_noneOurs :: Pretty BlockSeq -> Property
prop_applyBlocks_filteredTxs_noneOurs (Pretty blockSeq) =
    ourTxsReturned ==== ourTxsExpected
  where
    ourHeadUTxOProvided = UTxO.empty
    ourTxsExpected = blockSeqOurTxs NoneOurs blockSeq
    ourTxsReturned = applyBlocksFilteredTxs $ runIdentity $ applyBlocks
        (blockSeqToBlockData blockSeq)
        (utxoToWallet NoneOurs ourHeadUTxOProvided)

--------------------------------------------------------------------------------
-- Verifying UTxO sets returned by 'applyBlocks'
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- The following properties verify the correctness of the /last/ UTxO set
-- returned by 'applyBlocks'.
--
-- The last UTxO set has special significance, as it represents the final
-- state of the wallet after executing all /relevant/ transactions within
-- the given block sequence.
--------------------------------------------------------------------------------

-- Scenario: some transactions within the block sequence are expected to be
-- relevant to the wallet, but not all.
--
prop_applyBlocks_lastUTxO_someOurs
    :: Pretty (IsOursIf2 Address RewardAccount)
    -> Pretty BlockSeq
    -> Property
prop_applyBlocks_lastUTxO_someOurs (Pretty someOurs) (Pretty blockSeq)
    = ourLastUTxOReturned ==== ourLastUTxOExpected
    & labelInterval 10
        "UTxO.size globalHeadUTxO"
        (UTxO.size globalHeadUTxO)
    & labelInterval 10
        "UTxO.size globalLastUTxO"
        (UTxO.size globalLastUTxO)
    & labelInterval 10
        "UTxO.size ourHeadUTxOProvided"
        (UTxO.size ourHeadUTxOProvided)
    & labelInterval 10
        "UTxO.size ourLastUTxOExpected"
        (UTxO.size ourLastUTxOExpected)
    & cover 10
        (ourHeadUTxOProvided `utxoNonNullProperSubsetOf` globalHeadUTxO)
        "ourHeadUTxOProvided `utxoNonNullProperSubsetOf` globalHeadUTxO"
    & cover 10
        (ourLastUTxOReturned `utxoNonNullProperSubsetOf` globalLastUTxO)
        "ourLastUTxOReturned `utxoNonNullProperSubsetOf` globalLastUTxO"
    & cover 10
        (all utxoNonNull [ourUTxOAdded, ourUTxORemoved, ourUTxOPreserved])
        "all utxoNonNull [ourUTxOAdded, ourUTxORemoved, ourUTxOPreserved]"
    & checkCoverage
  where
    globalHeadUTxO = blockSeqHeadUTxO blockSeq
    globalLastUTxO = blockSeqLastUTxO blockSeq

    ourHeadUTxOProvided = ourUTxO someOurs globalHeadUTxO
    ourLastUTxOExpected = ourUTxO someOurs globalLastUTxO
    ourLastUTxOReturned = applyBlocksLastUTxO $ runIdentity $ applyBlocks
        (blockSeqToBlockData blockSeq)
        (utxoToWallet someOurs ourHeadUTxOProvided)

    ourUTxOAdded =
        ourLastUTxOReturned `UTxO.difference` ourHeadUTxOProvided
    ourUTxORemoved =
        ourHeadUTxOProvided `UTxO.difference` ourLastUTxOReturned
    ourUTxOPreserved =
        ourHeadUTxOProvided `UTxO.intersection` ourLastUTxOReturned

-- Scenario: all transactions within the block sequence are expected to be
-- relevant to the wallet.
--
prop_applyBlocks_lastUTxO_allOurs :: Pretty BlockSeq -> Property
prop_applyBlocks_lastUTxO_allOurs (Pretty blockSeq) =
    ourLastUTxOReturned ==== ourLastUTxOExpected
  where
    ourHeadUTxOProvided = blockSeqHeadUTxO blockSeq
    ourLastUTxOExpected = blockSeqLastUTxO blockSeq
    ourLastUTxOReturned = applyBlocksLastUTxO $ runIdentity $ applyBlocks
        (blockSeqToBlockData blockSeq)
        (utxoToWallet AllOurs ourHeadUTxOProvided)

-- Scenario: no transactions within the block sequence are expected to be
-- relevant to the wallet.
--
prop_applyBlocks_lastUTxO_noneOurs :: Pretty BlockSeq -> Property
prop_applyBlocks_lastUTxO_noneOurs (Pretty blockSeq) =
    ourLastUTxOReturned ==== ourLastUTxOExpected
  where
    ourHeadUTxOProvided = UTxO.empty
    ourLastUTxOExpected = UTxO.empty
    ourLastUTxOReturned = applyBlocksLastUTxO $ runIdentity $ applyBlocks
        (blockSeqToBlockData blockSeq)
        (utxoToWallet NoneOurs ourHeadUTxOProvided)

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

nonNullProperSubsequenceOf :: Eq a => [a] -> [a] -> Bool
nonNullProperSubsequenceOf s1 s2 = (&&)
    (s1 `isProperSubsequenceOf` s2)
    (not (null s1))

isProperSubsequenceOf :: Eq a => [a] -> [a] -> Bool
isProperSubsequenceOf s1 s2 = (&&)
    (s1 `L.isSubsequenceOf` s2)
    (s1 /= s2)

utxoNonNull :: UTxO -> Bool
utxoNonNull = not . UTxO.null

utxoNonNullProperSubsetOf :: UTxO -> UTxO -> Bool
utxoNonNullProperSubsetOf u1 u2 = (&&)
    (u1 `UTxO.isProperSubsetOf` u2)
    (not (UTxO.null u1))
