{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TupleSections #-}

module Cardano.Wallet.Primitive.ModelSpec
    ( spec
    ) where

import Prelude

import Algebra.PartialOrd
    ( PartialOrd (..) )
import Cardano.Api.Gen
    ( genSlotNo )
import Cardano.Wallet.DummyTarget.Primitive.Types
    ( block0 )
import Cardano.Wallet.Primitive.AddressDerivation
    ( DerivationIndex (..) )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( IsOurs (..) )
import Cardano.Wallet.Primitive.Model
    ( BlockData (..)
    , DeltaWallet
    , FilteredBlock
    , Wallet
    , applyBlock
    , applyBlocks
    , applyOurTxToUTxO
    , applyTxToUTxO
    , availableBalance
    , availableUTxO
    , changeUTxO
    , currentTip
    , discoverAddresses
    , getState
    , initWallet
    , spendTx
    , totalBalance
    , totalUTxO
    , unsafeInitWallet
    , utxo
    , utxoFromTx
    )
import Cardano.Wallet.Primitive.Slotting.Legacy
    ( flatSlot )
import Cardano.Wallet.Primitive.Types
    ( Block (..)
    , BlockHeader (..)
    , EpochLength (..)
    , SlotId (..)
    , SlotNo (..)
    )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..) )
import Cardano.Wallet.Primitive.Types.Address.Gen
    ( Parity (..), addressParity, coarbitraryAddress )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.Coin.Gen
    ( genCoin, shrinkCoin )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Cardano.Wallet.Primitive.Types.RewardAccount
    ( RewardAccount (..) )
import Cardano.Wallet.Primitive.Types.RewardAccount.Gen
    ( coarbitraryRewardAccount )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle )
import Cardano.Wallet.Primitive.Types.Tx
    ( Direction (..)
    , Tx (..)
    , TxIn (..)
    , TxMeta (direction)
    , TxOut (..)
    , TxScriptValidity (..)
    , collateralInputs
    , failedScriptValidation
    , inputs
    , txIns
    , txOutCoin
    )
import Cardano.Wallet.Primitive.Types.Tx.Gen
    ( genTx, genTxIn, genTxOut, shrinkTx, shrinkTxIn, shrinkTxOut )
import Cardano.Wallet.Primitive.Types.UTxO
    ( UTxO (..), balance, dom, excluding, filterByAddress, restrictedTo )
import Cardano.Wallet.Primitive.Types.UTxO.Gen
    ( genUTxO, shrinkUTxO )
import Cardano.Wallet.Util
    ( ShowFmt (..), invariant )
import Control.DeepSeq
    ( NFData (..) )
import Control.Monad
    ( foldM, guard )
import Control.Monad.Trans.State.Strict
    ( State, evalState, execState, runState, state )
import Data.Delta
    ( apply )
import Data.Foldable
    ( fold )
import Data.Function
    ( (&) )
import Data.Functor
    ( ($>) )
import Data.Functor.Identity
    ( Identity (..) )
import Data.Generics.Internal.VL.Lens
    ( over, view, (^.) )
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
import GHC.Generics
    ( Generic )
import Test.Hspec
    ( Spec, describe, it, shouldSatisfy )
import Test.Hspec.Extra
    ( parallel )
import Test.QuickCheck
    ( Arbitrary (..)
    , CoArbitrary (..)
    , Gen
    , Positive (..)
    , Property
    , Testable
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
    , listOf
    , oneof
    , property
    , scale
    , shrinkIntegral
    , shrinkList
    , shrinkMapBy
    , shrinkNothing
    , vector
    , withMaxSuccess
    , (.&&.)
    , (===)
    )
import Test.QuickCheck.Extra
    ( chooseNatural, report, verify )

import qualified Cardano.Wallet.Primitive.Types.Coin as Coin
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
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
            it "loses collateral"
                (property unit_applyTxToUTxO_loses_collateral)
            it "applyTxToUTxO then filterByAddress"
                (property prop_filterByAddress_balance_applyTxToUTxO)
            it "spendTx/applyTxToUTxO/utxoFromTx"
                (property prop_applyTxToUTxO_spendTx_utxoFromTx)

        describe "utxoFromTx" $ do
            it "has expected balance"
                (property prop_utxoFromTx_balance)
            it "is unspent"
                (property prop_utxoFromTx_is_unspent)

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
        it "prop_totalUTxO_pendingCollateralIncluded" $
            property prop_totalUTxO_pendingCollateralIncluded
        it "prop_totalUTxO_pendingInputsExcluded" $
            property prop_totalUTxO_pendingInputsExcluded

    parallel $ describe "Applying transactions to UTxO sets" $ do
        it "prop_applyOurTxToUTxO_allOurs" $
            property prop_applyOurTxToUTxO_allOurs
        it "prop_applyOurTxToUTxO_someOurs" $
            property prop_applyOurTxToUTxO_someOurs

    parallel $ describe "Address discovery" $ do
        it "discoverAddresses ~ isOurTx" $
            property prop_discoverAddresses

{-------------------------------------------------------------------------------
                                Properties
-------------------------------------------------------------------------------}
applyBlocksOld
    :: (IsOurs s Address, IsOurs s RewardAccount)
    => NonEmpty Block
    -> Wallet s
    -> NonEmpty (FilteredBlock, (DeltaWallet s, Wallet s))
applyBlocksOld bs = runIdentity . applyBlocks (List bs)

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
    (filteredBlocks, cps') = NE.unzip $ applyBlocksOld bs cp0
    cps = NE.map snd cps'
    txs = fold $ (view #transactions) <$> filteredBlocks
    s' = getState $ NE.last cps
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
    wallet' = NE.last $ snd . snd <$> applyBlocksOld bs wallet
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
        sum $ (unCoin . txOutCoin) <$> concatMap outputs (Set.elems pending)
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
    allInputsOfTx tx = Set.fromList $ fst <$> mconcat
        [ tx & resolvedInputs
        , tx & resolvedCollateral
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
prop_changeUTxO_inner pendingTxs =
    checkCoverage $
    cover 50 (not (UTxO.null utxoEven) && not (UTxO.null utxoOdd))
        "UTxO sets not null" $
    conjoin
        [ -- All addresses in the even-parity UTxO set have even parity:
          F.all ((== Even) . txOutParity) (unUTxO utxoEven)
          -- All addresses in the odd-parity UTxO set have odd parity:
        , F.all ((== Odd) . txOutParity) (unUTxO utxoOdd)
          -- The even-parity and odd-parity UTxO sets are disjoint:
        , Map.null $ Map.intersection (unUTxO utxoEven) (unUTxO utxoOdd)
          -- The even-parity and odd-parity UTxO sets are complete:
        , Map.union (unUTxO utxoEven) (unUTxO utxoOdd) == unUTxO utxoAll
          -- No outputs are omitted when we select everything:
        , UTxO.size utxoAll == F.sum (F.length . view #outputs <$> pendingTxs)
        ]
    & report
        (UTxO.size utxoAll)
        "UTxO.size utxoAll"
    & report
        (F.sum (F.length . view #outputs <$> pendingTxs))
        "F.sum (F.length . view #outputs <$> pendingTxs)"
  where
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
    { conditionA :: a -> Bool
    , conditionB :: b -> Bool
    }

instance Eq (IsOursIf2 a b) where
    _ == _ = False

instance Show (IsOursIf2 a b) where
    show _ = "IsOursIf2"

instance IsOurs (IsOursIf2 a b) a where
    isOurs entity s@(IsOursIf2 {conditionA}) =
        isOursIf conditionA entity s

instance IsOurs (IsOursIf2 a b) b where
    isOurs entity s@(IsOursIf2 {conditionB}) =
        isOursIf conditionB entity s

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

prop_totalUTxO_pendingCollateralIncluded :: Property
prop_totalUTxO_pendingCollateralIncluded =
    prop_totalUTxO prop
  where
    prop pendingTxs _wallet result =
        cover 1
            (not (Set.null pendingCollateral))
            "not (Set.null pendingCollateral)" $
        all (`Map.member` unUTxO result) pendingCollateral
      where
        -- In any given transaction, the sets of ordinary inputs and collateral
        -- inputs can intersect. Since ordinary inputs of pending transactions
        -- must be excluded from the result, we only consider collateral inputs
        -- that are not also used as ordinary inputs:
        pendingCollateral :: Set TxIn
        pendingCollateral = pendingTxs
            & F.foldMap (fmap fst . view #resolvedCollateral)
            & L.filter (`Set.notMember` pendingInputs)
            & Set.fromList

        pendingInputs :: Set TxIn
        pendingInputs = pendingTxs
            & F.foldMap (fmap fst . view #resolvedInputs)
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
        = over #resolvedCollateral
            (filter ((`Set.member` utxoInputs) . fst))
        . over #resolvedInputs
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
    :: SlotNo
    -> Quantity "block" Word32
    -> Tx
    -> UTxO
    -> Property
prop_applyOurTxToUTxO_allOurs slotNo blockHeight tx utxo =
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
    maybeResult = get <$> applyOurTxToUTxO slotNo blockHeight AllOurs tx utxo
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
    -> SlotNo
    -> Quantity "block" Word32
    -> Tx
    -> UTxO
    -> Property
prop_applyOurTxToUTxO_someOurs ourState slotNo blockHeight tx utxo =
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
    maybeResult = get <$> applyOurTxToUTxO slotNo blockHeight ourState tx utxo
      where get (_,_,a) = a
    shouldHaveResult :: Bool
    shouldHaveResult = evalState (isOurTx tx utxo) ourState

{-------------------------------------------------------------------------------
                               Address discovery
-------------------------------------------------------------------------------}

{- HLINT ignore prop_discoverAddresses "Avoid lambda using `infix`" -}
prop_discoverAddresses :: ApplyBlock -> Property
prop_discoverAddresses (ApplyBlock s utxo block) =
    snd (discoverAddresses block s)
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
    | failedScriptValidation tx =
        txHasRelevantCollateral
    | otherwise =
        F.or <$> sequence
            [ txHasRelevantInput
            , txHasRelevantOutput
            , txHasRelevantWithdrawal
            ]
  where
    txHasRelevantCollateral =
        pure . not . UTxO.null $
        u `UTxO.restrictedBy` Set.fromList (fst <$> tx ^. #resolvedCollateral)
    txHasRelevantInput =
        pure . not . UTxO.null $
        u `UTxO.restrictedBy` Set.fromList (fst <$> tx ^. #resolvedInputs)
    txHasRelevantOutput =
        F.or <$> sequence (isOursState . (view #address) <$> tx ^. #outputs)
    txHasRelevantWithdrawal =
        F.or <$> sequence (isOursState . fst <$> Map.toList (tx ^. #withdrawals))

    isOursState :: IsOurs s addr => addr -> State s Bool
    isOursState = fmap isJust . state . isOurs

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
    return $ (utxo <> utxo') `excluding` txIns txs

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
    runState $ Set.fromList <$>
        forMaybe (foldMap (\tx -> zip (repeat tx) (outputs tx)) txs) pick
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

instance (CoArbitrary a, CoArbitrary b) => Arbitrary (IsOursIf2 a b) where
    arbitrary = IsOursIf2
        <$> arbitrary
        <*> arbitrary

instance Arbitrary Coin where
    shrink = shrinkCoin
    arbitrary = genCoin

instance Arbitrary (Quantity "block" Word32) where
    arbitrary = Quantity <$> arbitrarySizedBoundedIntegral @Word32
    shrink = shrinkMapBy Quantity getQuantity shrinkIntegral

instance Arbitrary SlotNo where
    arbitrary = genSlotNo
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
        pending <- genPendingTx (totalUTxO Set.empty wallet) rewards
        pure $ WithPending wallet pending rewards
      where
        genPendingTx :: UTxO -> Coin -> Gen (Set Tx)
        genPendingTx (UTxO u) rewards
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
                let tokens = TokenBundle.fromCoin $ simulateFee $ txOutCoin out
                let pending = withWithdrawal $ Tx
                        { txId = arbitraryHash
                        , fee = Nothing
                        , resolvedInputs = [(inp, txOutCoin out)]
                        -- TODO: (ADP-957)
                        , resolvedCollateral = []
                        , outputs = [out {tokens}]
                        , withdrawals = mempty
                        , metadata = Nothing
                        , scriptValidity = Nothing
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
                , resolvedInputs =
                    [ (TxIn
                        { inputId = Hash "\199D\198\229\227\196\204\231\178\166m\226\134\211\DC1}\243[\204\DC4\171\213\230\246\SOHy\229\t\167\184\235g"
                        , inputIx = 0
                        }, Coin 0)
                    , (TxIn
                        { inputId = Hash "\a\241.\180(\a\148\201u$\229\251\147\224\f\166\159\EOT\166m\US\178dN\242\227\b\254\227G\169\RS"
                        , inputIx = 0
                        }, Coin 0)
                    ]
                , resolvedCollateral = []
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
                , fee = Nothing
                , resolvedInputs =
                    [ (TxIn
                        { inputId = Hash "+\253\232\DC3\132\"M\NULf\EM\228\bh)\STX\171W\215@#\198\a\228\229Z2]\156_fjg"
                        , inputIx = 0
                        }, Coin 0)
                    ]
                , resolvedCollateral = []
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
                , withdrawals = mempty
                , metadata = Nothing
                , scriptValidity = Nothing
                }
            , Tx
                { txId = Hash "b17ca3d2b8a991ea4680d1ebd9940a03449b1b6261fbe625d5cae6599726ea41"
                , fee = Nothing
                , resolvedInputs =
                    [ (TxIn
                        { inputId = Hash "\137\150\&8\141\164l\v\ACK\132\198\SI\GS7\201\&3Dd\177fM,\GS)\EM\DC4\242#\211'3\233\163"
                        , inputIx = 0
                        }, Coin 0)
                    ]
                , resolvedCollateral = []
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
                , fee = Nothing
                , resolvedInputs =
                    [ (TxIn
                        { inputId = Hash "(\EM#\f\165\236\169=\227\163>MY\225ts\192\SYN\137=\145\155~\212.\252\130l\166v0\SOH"
                        , inputIx = 0
                        }, Coin 0)
                    ]
                , resolvedCollateral = []
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
                , withdrawals = mempty
                , metadata = Nothing
                , scriptValidity = Nothing
                }
            , Tx
                { txId = Hash "6ed51b05821f0dc130a9411f0d63a241a624fbc8a9c8a2a13da8194ce3c463f4"
                , fee = Nothing
                , resolvedInputs =
                    [ ( TxIn
                        { inputId = Hash "\128\168muc\212\EMP\238\\\173w\203\159N\205T:\230V\134\164w\143>\192\134\153\SUB$cD"
                        , inputIx = 0
                        }, Coin 0)
                    ]
                , resolvedCollateral = []
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
                , fee = Nothing
                , resolvedInputs =
                    [ (TxIn
                        { inputId = Hash "\164\254\137\218h\f\DLE\245\141u\SYN\248~\253n;\202\144\150\v\229\177\218\195\238\157\230\158\241O\153\215"
                        , inputIx = 0
                        }, Coin 0)
                    ]
                , resolvedCollateral = []
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
                , fee = Nothing
                , resolvedInputs =
                    [ (TxIn
                        { inputId = Hash "\187\199\161\240\222$\bZ\196\138R\238o\137\209\129QE\132Z\135\DC2TsP\167\228\146\&8Yt\171"
                        , inputIx = 0
                        }, Coin 0)
                    ]
                , resolvedCollateral = []
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
                , fee = Nothing
                , resolvedInputs =
                    [ (TxIn
                        { inputId = Hash "s\165\210\a@\213\DC1\224\DLE\144$\DEL\138\202\144\225\229PVBD\ETB25\161\164u\137\NUL{\158v"
                        , inputIx = 0
                        }, Coin 0) ]
                , resolvedCollateral = []
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
                , fee = Nothing
                , resolvedInputs =
                    [ (TxIn
                        { inputId = Hash "\177|\163\210\184\169\145\234F\128\209\235\217\148\n\ETXD\155\ESCba\251\230%\213\202\230Y\151&\234A"
                        , inputIx = 0
                        }, Coin 0)
                    ]
                , resolvedCollateral = []
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
                , fee = Nothing
                , resolvedInputs =
                    [ ( TxIn
                        { inputId = Hash "\195\242\DEL-\232v(c\SI+\172\163\245\142\189\214aiB#4\139\172\166\237\167\ETB9\246\150\185\219"
                        , inputIx = 1
                        }, Coin 0)
                    , ( TxIn
                        { inputId = Hash "8O\137\193\224w\243\252s\198\250\201\&04\169\129E\155{\n\DC3H<\199\208\154\214\237\141\128<+"
                        , inputIx = 1
                        }, Coin 0)
                    ]
                , resolvedCollateral = []
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
                , fee = Nothing
                , resolvedInputs =
                    [ ( TxIn
                        { inputId = Hash "\ETXX\189\235\195q81{D\DC3\DLE\228\237(\251\184`l\226\229\184\FSG\132\217\224\202\222\249\246J"
                        , inputIx = 1
                        }, Coin 0)
                    ]
                , resolvedCollateral = []
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
                , withdrawals = mempty
                , metadata = Nothing
                , scriptValidity = Nothing
                }
            , Tx
                { txId = Hash "611ce641f0f9282a35b1678fcd996016833c0de9e83a04bfa1178c8f045196ea"
                , fee = Nothing
                , resolvedInputs =
                    [ ( TxIn
                        { inputId = Hash "\151\146\133\SYN\187\ENQ\252\226\&4\210n\153\178+.h\200\CANAs\SI\181\189\GS\131[g7O\GS\232\215"
                        , inputIx = 1
                        }, Coin 0)
                    ]
                , resolvedCollateral = []
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
                , fee = Nothing
                , resolvedInputs =
                    [ ( TxIn
                        { inputId = Hash "_>\240.\159\145\US\NUL1\158r\231\&8\214\241\134\&2\DC4\ETB\160\134\237z\143D\229d\DC4\245\208\DC3?"
                        , inputIx = 0
                        }, Coin 0)
                    ]
                , resolvedCollateral = []
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
                , withdrawals = mempty
                , metadata = Nothing
                , scriptValidity = Nothing
                }
            , Tx
                { txId = Hash "b8e9699ffff40c993d6778f586110b78cd30826feaa5314adf3a2e9894b9313a"
                , fee = Nothing
                , resolvedInputs =
                    [ ( TxIn
                        { inputId = Hash "\187\177J\189\132K\n\175\130\148\&3[\150\193zL\153\191Qjcl\n\162B\241G)>\151\DC4\225"
                        , inputIx = 0
                        }, Coin 0)
                    ]
                , resolvedCollateral = []
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
                , fee = Nothing
                , resolvedInputs =
                    [ ( TxIn
                        { inputId = Hash "\150\225pI\SUB\251n\189W\159\213|v\198\132\242$6\248\204:\145#\151\221\177\201\197\ESC\134\251S"
                        , inputIx = 0
                        }, Coin 0)
                    ]
                , resolvedCollateral = []
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
                , withdrawals = mempty
                , metadata = Nothing
                , scriptValidity = Nothing
                }
              , Tx
                  { txId = Hash "7726526b5cc003f71d9629c611397285004b5438eac9a118c2b20e2810e0783e"
                  , fee = Nothing
                  , resolvedInputs =
                      [ ( TxIn
                          { inputId = Hash "\249\DC2\146\&0\GSK\177\182\224@\206\205\255@0\149\155I\201^}\174\bw\130\221U\139\235\182f\138"
                          , inputIx = 0
                          }, Coin 0)
                      ]
                  , resolvedCollateral = []
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
                , fee = Nothing
                , resolvedInputs =
                    [ (TxIn
                        { inputId = Hash "\194\157>\160\221\163\&4\218\149\215\178\161]p\185\246\208\198\ENQ \188\216\242\160\190\236\137\151\DC3\134\"\DC4"
                        , inputIx = 0
                        }, Coin 0)
                    ]
                , resolvedCollateral = []
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
                , withdrawals = mempty
                , metadata = Nothing
                , scriptValidity = Nothing
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
        (failedScriptValidation tx)
        "failedScriptValidation tx" $
    cover 10
        (not $ failedScriptValidation tx)
        "not $ failedScriptValidation tx" $
    balance (applyTxToUTxO tx u) === expectedBalance
  where
    expectedBalance =
        if failedScriptValidation tx
        then
            balance (u `excluding` Set.fromList (collateralInputs tx))
        else
            balance (u `excluding` Set.fromList (inputs tx))
                `TokenBundle.add` balance (utxoFromTx tx)

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
        (failedScriptValidation tx)
        "failedScriptValidation tx" $
    cover 10
        (not $ failedScriptValidation tx)
        "not $ failedScriptValidation tx" $
    applyTxToUTxO tx u === expectedResult
  where
    expectedResult =
        if failedScriptValidation tx
        then u `excluding` Set.fromList (collateralInputs tx)
        else u `excluding` Set.fromList (inputs tx) <> utxoFromTx tx

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
        (failedScriptValidation tx)
        "failedScriptValidation tx" $
    cover 10
        (not $ failedScriptValidation tx)
        "not $ failedScriptValidation tx" $
    balance (filterByAddress f (applyTxToUTxO tx mempty))
    ===
    expectedResult
  where
    expectedResult =
        if failedScriptValidation tx
        then mempty
        else foldMap m (outputs tx)
      where
        m output =
            if f (address output)
            then tokens output
            else mempty

prop_utxoFromTx_is_unspent :: Tx -> Property
prop_utxoFromTx_is_unspent tx =
    checkCoverage $
    cover 10
        (utxoFromTx tx /= mempty)
        "utxoFromTx tx /= mempty" $
    cover 10
        (Set.fromList (inputs tx) /= mempty)
        "Set.fromList (inputs tx) /= mempty" $
    utxoFromTx tx `excluding` Set.fromList (inputs tx)
    === utxoFromTx tx

unit_applyTxToUTxO_spends_input :: Tx -> TxIn -> TxOut -> Coin -> Property
unit_applyTxToUTxO_spends_input tx txin txout coin =
    let
        tx' = tx
            { resolvedInputs = [(txin, coin)]
            , scriptValidity = Nothing
            }
    in
        applyTxToUTxO tx' (UTxO $ Map.fromList [(txin, txout)])
        === utxoFromTx tx' `excluding` Set.singleton txin

unit_applyTxToUTxO_loses_collateral :: Tx -> TxIn -> TxOut -> Coin -> Property
unit_applyTxToUTxO_loses_collateral tx txin txout coin =
    let
        tx' = tx
            { resolvedCollateral = [(txin, coin)]
            , scriptValidity = Just TxScriptInvalid
            }
    in
        applyTxToUTxO tx' (UTxO $ Map.fromList [(txin, txout)])
        === mempty

prop_utxoFromTx_balance :: Tx -> Property
prop_utxoFromTx_balance tx =
    checkCoverage $
    cover 10
        (outputs tx /= mempty)
        "outputs tx /= mempty" $
    cover 10
        (failedScriptValidation tx)
        "failedScriptValidation tx)" $
    cover 10
        (not $ failedScriptValidation tx)
        "not $ failedScriptValidation tx)" $
    balance (utxoFromTx tx) === foldMap f (outputs tx)
  where
    f output =
        if failedScriptValidation tx
        then mempty
        else tokens output

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
    lhs === rhs
  where
    lhs = balance (spendTx tx u)
    rhs = TokenBundle.unsafeSubtract (balance u) toSubtract
      where
        toSubtract =
            if failedScriptValidation tx
            then balance
                (u `UTxO.restrictedBy` Set.fromList (collateralInputs tx))
            else balance
                (u `UTxO.restrictedBy` Set.fromList (inputs tx))

prop_spendTx :: Tx -> UTxO -> Property
prop_spendTx tx u =
    checkCoverage $
    cover 10
        (spendTx tx u /= mempty)
        "spendTx tx u /= mempty" $
    spendTx tx u === u `excluding` toExclude
  where
    toExclude =
        if failedScriptValidation tx
        then Set.fromList (collateralInputs tx)
        else Set.fromList (inputs tx)

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

instance CoArbitrary Address where
    coarbitrary = coarbitraryAddress

instance CoArbitrary RewardAccount where
    coarbitrary = coarbitraryRewardAccount

instance Show (Address -> Bool) where
    show = const "(Address -> Bool)"

instance Show (RewardAccount -> Bool) where
    show = const "(RewardAccount -> Bool)"
