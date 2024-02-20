-- |
-- Copyright: © 2023 IOHK
-- License: Apache-2.0
module Cardano.Wallet.DB.Store.UTxOHistory.ModelSpec
    ( spec
    , genSlotNo
    , genSlot
    , genUTxO
    , genDelta
    ) where

import Prelude

import Cardano.Slotting.Slot
    ( SlotNo (..)
    , WithOrigin (..)
    )
import Cardano.Wallet.DB.Store.UTxOHistory.Model
    ( DeltaUTxOHistory (..)
    , Pruned (..)
    , UTxOHistory
    , empty
    , getFinality
    , getSpent
    , getTip
    , getUTxO
    )
import Cardano.Wallet.Primitive.Types
    ( Slot
    )
import Cardano.Wallet.Primitive.Types.Address.Gen
    ( genAddress
    )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..)
    )
import Cardano.Wallet.Primitive.Types.Tx.TxIn
    ( TxIn (..)
    )
import Cardano.Wallet.Primitive.Types.Tx.TxOut
    ( TxOut (..)
    )
import Cardano.Wallet.Primitive.Types.UTxO
    ( DeltaUTxO (..)
    , UTxO (..)
    , dom
    , excludingD
    , receiveD
    , size
    )
import Data.Delta
    ( Delta (apply)
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    )
import Test.QuickCheck
    ( Gen
    , Property
    , Testable (property)
    , conjoin
    , counterexample
    , cover
    , elements
    , forAll
    , frequency
    , listOf1
    , sublistOf
    , vectorOf
    , (===)
    )

import qualified Cardano.Wallet.Primitive.Types.UTxO as UTxO
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map.Strict as Map

spec :: Spec
spec = do
    describe "on DeltaUTxO history" $ do
        describe "apply AppendBlock" $ do
            it "move forward the tip" $ property prop_new_tip
            it "update of utxo" $ property prop_new_utxo
        describe "apply Prune" $ do
            it "move forward the finality" $ property prop_prune_finality
            it "do not prune utxo" $ property prop_dont_prune_utxo
            it "remove spent utxo before finality" $
                property prop_prune_spent_utxo
        describe "apply Rollback" $ do
            it "move back the tip" $ property prop_rollback_tip
            it "revert an update of utxo" $
                property prop_rollback_revert_append_block
            it "reset the history when rollback before finality" $
                property prop_rollback_reset_history

-- | Observe nothing has changed.
noop :: UTxOHistory -> UTxOHistory -> Property
noop history history' =
    conjoin
        [ getTip history' === getTip history
        , getFinality history' === getFinality history
        , getUTxO history' === getUTxO history
        , getSpent history' === getSpent history
        ]

setupHistory
    :: Testable prop
    => (UTxOHistory -> DeltaUTxO -> prop)
    -> Property
setupHistory f = historyProp $ \_delta history ->
    deltaProp history $ \delta' -> nonTrivialDelta delta' $ f history delta'

{-----------------------------------------------------------------------------
    Properties
------------------------------------------------------------------------------}
-- prop> prop_new_tip
-- +++ OK, passed 100 tests:
-- 93% non-trivial received
-- 92% non-trivial excluded
-- 65% non-trivial tip
prop_new_tip :: Property
prop_new_tip = setupHistory $ \history delta -> slotNoProp history (1, 1, 4) $
    \slot ->
        cover 50 (At slot > getTip history) "non-trivial tip" $
            let history' = AppendBlock slot delta `apply` history
             in if At slot > getTip history
                    then getTip history' === At slot
                    else noop history' history

-- prop> prop_new_utxo
-- +++ OK, passed 100 tests:
-- 91% non-trivial excluded
-- 90% non-trivial received
-- 73% non-trivial tip
prop_new_utxo :: Property
prop_new_utxo = setupHistory $ \history delta -> slotNoProp history (1, 1, 4) $
    \slot ->
        cover 50 (At slot > getTip history) "non-trivial tip" $
            let history' = AppendBlock slot delta `apply` history
             in if At slot > getTip history
                    then
                        counterexample (show (history, history', delta)) $
                            getUTxO history'
                                === apply delta (getUTxO history)
                    else counterexample "noop" $ noop history' history

-- prop> prop_rollback_tip
prop_rollback_tip :: Property
prop_rollback_tip =
    setupPrune $ \_ history _ -> slotProp history (3, 4, 1) $
        \slot ->
            -- label (show $ (slot, getFinality history)) $
            cover 50 (slot < getTip history) "non-trivial tip" $
            cover 20 (not $ tipIsAfterFinality slot (getFinality history))
                    "catastrophic rollback, back to Origin" $
                let history' = Rollback slot `apply` history
                 in if slot < getTip history
                        then
                            if tipIsAfterFinality slot (getFinality history) then
                                getTip history'
                                    === case getFinality history of
                                        NotPruned -> slot
                                        PrunedUpTo finality ->
                                            max (At finality) slot
                            else
                                getTip history' === Origin
                        else noop history' history

tipIsAfterFinality :: Slot -> Pruned -> Bool
tipIsAfterFinality _ NotPruned = True
tipIsAfterFinality (At slot) (PrunedUpTo slot') = slot >= slot'
tipIsAfterFinality Origin _ = False

-- prop> prop_rollback_revert_append_block
prop_rollback_revert_append_block :: Property
prop_rollback_revert_append_block =
    setupPrune $ \_ history _ -> slotNoProp history (3, 4, 1) $
        \slot ->
            deltaProp history $ \delta ->
                cover 50 (At slot < getTip history) "non-trivial tip" $
                    let
                        history' =
                            Rollback (getTip history)
                                `apply` (AppendBlock slot delta `apply` history)
                    in
                        counterexample ("rb " <> show (history, history', delta)) $
                            noop history' history

prop_rollback_reset_history :: Property
prop_rollback_reset_history =
    setupPrune $ \_ history _ -> slotProp history (3, 1, 1) $
        \slot ->
            let
                history' =
                    Rollback slot `apply` history
            in
                cover
                    50
                    (not $ tipIsAfterFinality slot $ getFinality history)
                    "catastrophic rollback, back to Origin"
                    $ counterexample ("rb " <> show (history, history'))
                    $ if (not $ tipIsAfterFinality slot $ getFinality history')
                        then history' === empty mempty
                        else property True

slotIsAfterFinality :: SlotNo -> Pruned -> Bool
slotIsAfterFinality _slot NotPruned = True
slotIsAfterFinality slot (PrunedUpTo slot') = slot > slot'

-- prop> prop_prune_finality
prop_prune_finality :: Property
prop_prune_finality = setupHistory $ \history _delta ->
    slotNoProp history (1, 4, 1) $ \slot ->
        cover
            50
            (slotIsAfterFinality slot $ getFinality history)
            "non-trivial finality"
            $ let history' = Prune slot `apply` history
               in if slotIsAfterFinality slot (getFinality history)
                    && At slot <= getTip history
                    then getFinality history' === PrunedUpTo slot
                    else
                        if At slot > getTip history
                            then case getFinality history' of
                                NotPruned -> property False
                                PrunedUpTo slot' -> At slot' === getTip history
                            else noop history' history

-- Setup a property that requires a last two history states and the new finality.
setupPrune
    :: Testable prop
    => (UTxOHistory -> UTxOHistory -> SlotNo -> prop)
    -> Property
setupPrune f = setupHistory $ \history delta -> slotNoProp history (1, 1, 4) $
    \newTip ->
        cover 50 (At newTip > getTip history) "non-trivial finality" $
            let history' = AppendBlock newTip delta `apply` history
             in slotNoProp history' (1, 4, 1) $ \newFinality ->
                    cover
                        50
                        (slotIsAfterFinality newFinality $ getFinality history')
                        "non-trivial finality"
                        $ let history'' = Prune newFinality `apply` history'
                           in counterexample
                                (show (newFinality, history', history''))
                                $ f history' history'' newFinality

-- prop> prop_dont_prune_utxo
prop_dont_prune_utxo :: Property
prop_dont_prune_utxo = setupPrune $ \history' history _newFinality ->
    getUTxO history' === getUTxO history

-- prop> prop_prune_spent_utxo
prop_prune_spent_utxo :: Property
prop_prune_spent_utxo = setupPrune $ \history history' newFinality ->
    cover 50 (not $ null $ getSpent history) "non-trivial spent" $
        counterexample (show $ getSpent history') $
            not $
                any (<= newFinality) $
                    getSpent history'

{-----------------------------------------------------------------------------
    Generators
------------------------------------------------------------------------------}
-- Convert a Slot to an Int.
slotInt :: Slot -> Int
slotInt Origin = -1
slotInt (At s) = fromIntegral $ unSlotNo s

-- Convert an int to a Slot.
intSlot :: Int -> Slot
intSlot n
    | n < 0 = Origin
    | otherwise = At $ SlotNo $ fromIntegral n

-- Generate a random Slot, but biased towards ater the current tip.
-- >>> import Test.QuickCheck
-- >>> sample' $ genSlot empty (1,1,1)
genSlot :: UTxOHistory -> (Int, Int, Int) -> Gen Slot
genSlot h (l, m, r) = genTime h (l, m, r) (-1) intSlot

genSlotNo :: UTxOHistory -> (Int, Int, Int) -> Gen SlotNo
genSlotNo h (l, m, r) = genTime h (l, m, r) 0 (SlotNo . fromIntegral)

genTime
    :: UTxOHistory
    -> (Int, Int, Int)
    -> Int
    -> (Int -> a)
    -> Gen a
genTime h (l, m, r) z p =
    let left = case getFinality h of
            NotPruned -> []
            PrunedUpTo s -> f l z (slotInt $ At s)
        middle = case getFinality h of
            NotPruned -> f m z (slotInt (getTip h))
            PrunedUpTo s -> f m (slotInt $ At s) (slotInt (getTip h))
        right = f r (slotInt (getTip h)) (slotInt (getTip h) + 10)
     in frequency $ case left ++ middle ++ right of
            [] -> error "genTime: no cases"
            xs -> filter ((>=0) . fst) xs
  where
    f g x y = case p <$> dropWhile (< z) [x .. y] of
        [] -> []
        xs -> [(g, elements xs)]

-- Generate a property that holds for any Slot depending on the given
-- UTxOHistory.
slotNoProp
    :: Testable prop
    => UTxOHistory
    -> (Int, Int, Int)
    -> (SlotNo -> prop)
    -> Property
slotNoProp history how =
    forAll (genSlotNo history how)

slotProp
    :: Testable prop
    => UTxOHistory
    -> (Int, Int, Int)
    -> (Slot -> prop)
    -> Property
slotProp history how = forAll $ genSlot history how

-- Generate a property that holds for any UTxOHistory.
-- >>> import Cardano.Wallet.Primitive.Types.UTxO (size)
-- prop> historyProp $ \history -> size (getUTxO history) > 0
historyProp
    :: Testable prop
    => ((SlotNo, DeltaUTxO) -> UTxOHistory -> prop)
    -> Property
historyProp prop = forAll (genUTxO $ empty mempty) $ \utxo ->
    let base = empty utxo
     in deltaProp base $ \delta ->
            forAll (genSlotNo base (1, 1, 4)) $
                \slot -> prop (slot, delta) $
                    AppendBlock slot delta `apply` base

genDelta :: UTxOHistory -> Gen DeltaUTxO
genDelta h = do
    utxo <- genUTxO h
    pure $  fst (receiveD (getUTxO h) utxo)
            <> fst (excludingD (getUTxO h) $ dom utxo)

-- Generate a property that holds for any DeltaUTxO that is valid for
-- the given UTxOHistory.
-- >>> import Cardano.Wallet.Primitive.Types.UTxO (size)
-- prop> deltaProp empty $ \delta -> size (received delta)  > 0
deltaProp
    :: Testable prop
    => UTxOHistory
    -> (DeltaUTxO -> prop)
    -> Property
deltaProp h = forAll (genDelta h)

-- Generate a property that holds for any DeltaUTxO that is not trivial.
nonTrivialDelta :: Testable prop => DeltaUTxO -> prop -> Property
nonTrivialDelta delta prop =
    cover 50 (size (received delta) > 1) "non-trivial received" $
        cover 50 (length (excluded delta) > 1) "non-trivial excluded" prop

-- Generate a random UTxO, with readable hashes.
-- >>> import Test.QuickCheck
-- > sample' genUTxO
genUTxO :: UTxOHistory -> Gen UTxO
genUTxO h = do
    random <- do
        ins <- listOf1 $ TxIn <$> genHash <*> genIndex
        outs <- listOf1 $ TxOut <$> genAddress <*> pure mempty
        pure . UTxO $ Map.fromList $ zip ins outs
    unSpent <- sublistOf $ UTxO.toList $ getUTxO h
    -- traceShow (getUTxO h) $
    pure $ random <> UTxO (Map.fromList unSpent)
  where
    genHash = fmap (Hash . B8.pack) $ vectorOf 32 $ elements ['a' .. 'z']
    genIndex = elements [0 .. 255]
