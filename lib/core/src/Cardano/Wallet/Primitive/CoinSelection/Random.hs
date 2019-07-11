{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: MIT
--
-- This module contains the implementation of random
-- input selection algorithm


module Cardano.Wallet.Primitive.CoinSelection.Random
    ( random
    ) where

import Prelude

import Cardano.Wallet.Primitive.CoinSelection
    ( CoinSelection (..), CoinSelectionOptions (..), ErrCoinSelection (..) )
import Cardano.Wallet.Primitive.CoinSelection.LargestFirst
    ( largestFirst )
import Cardano.Wallet.Primitive.Types
    ( Coin (..)
    , TxIn
    , TxOut (..)
    , UTxO (..)
    , balance'
    , distance
    , invariant
    , pickRandom
    )
import Control.Arrow
    ( left )
import Control.Monad
    ( foldM )
import Control.Monad.Trans.Class
    ( lift )
import Control.Monad.Trans.Except
    ( ExceptT (..), except )
import Control.Monad.Trans.Maybe
    ( MaybeT (..), runMaybeT )
import Crypto.Random.Types
    ( MonadRandom )
import Data.Functor
    ( ($>) )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Ord
    ( comparing )
import Data.Word
    ( Word64 )

import qualified Data.List as L
import qualified Data.List.NonEmpty as NE


-- | Target range for picking inputs
data TargetRange = TargetRange
    { targetMin :: Word64
        -- ^ Minimum value to cover: only the requested amount, no change at all
    , targetAim :: Word64
        -- ^ Ideal case: change equal to requested amount
    , targetMax :: Word64
        -- ^ Maximum value: an arbitrary upper bound (e.g. @2 * targetMin@)
    }

-- | Random-Improve Algorithm
--
-- 1. Randomly select outputs from the UTxO until the payment value is covered.
--    (In the rare case that this fails because the maximum number of
--    transaction inputs has been exceeded, fall back on the largest-first
--    algorithm for this step.)
--
-- 2. The algorithm first makes a random  selection for each output from the UTxO,
--    processing the biggest output first and proceeding in a descending order.
--    If the selection is not successful largest-first fallback kicks in.
--    If the selection is successful for each output then the
--    improvement is tried for each selection, once again starting from the selection
--    made for the biggest output. The improvement is tried for the next biggest output's
--    selection. An output is considered an improvement when:
--
--    (a)  It doesn’t exceed a specified upper limit.
--    (b)  Adding the new output gets us closer to the ideal change value.
--    (c)  It doesn’t exceed a maximum number of transaction inputs.
--
-- This algorithm follows three principles:
--
-- @
-- **Self organisation principle 1**
-- Random selection has a high probability of picking dust outputs precisely
-- when there is a lot of dust in the UTxO.
-- @
--
-- @
-- **Self organisation principle 2**
-- If for each payment request for value `x` we create a change output roughly
-- of the same value `x`, then we will end up with a lot of change outputs in
-- our UTxO of size `x` precisely when we have a lot of payment requests of
-- size `x`
-- @
--
-- @
-- **Self organisation principle 3**
-- Searching the UTxO for additional entries to improve our change output is
-- only useful if the UTxO contains entries that are sufficiently small enough.
-- But precisely when the UTxO contains many small entries, it is less likely
-- that a randomly chosen UTxO entry will push the total above the upper bound
-- we set.
-- @
random
    :: forall m e. MonadRandom m
    => CoinSelectionOptions e
    -> NonEmpty TxOut
    -> UTxO
    -> ExceptT (ErrCoinSelection e) m (CoinSelection, UTxO)
random opt outs utxo = do
    let descending = NE.toList . NE.sortBy (flip $ comparing coin)
    randomMaybe <- lift $ runMaybeT $
        foldM makeSelection (opt, utxo, []) (descending outs)
    case randomMaybe of
        Just (opt', utxo', res) -> do
            (_, sel, remUtxo) <- lift $
                foldM improveTxOut (opt', mempty, utxo') (reverse res)
            guard sel $> (sel, remUtxo)
        Nothing ->
            largestFirst opt outs utxo
  where
    guard = except . left ErrInvalidSelection . validate opt

-- | Perform a random selection on a given output, without improvement.
makeSelection
    :: forall m e. MonadRandom m
    => (CoinSelectionOptions e, UTxO, [([(TxIn, TxOut)], TxOut)])
    -> TxOut
    -> MaybeT m (CoinSelectionOptions e, UTxO, [([(TxIn, TxOut)], TxOut)])
makeSelection (CoinSelectionOptions maxNumInputs fn, utxo0, selection) txout = do
    (inps, utxo1) <- coverRandomly ([], utxo0)
    return
        ( CoinSelectionOptions (maxNumInputs - fromIntegral (L.length inps)) fn
        , utxo1
        , (inps, txout) : selection
        )
  where
    coverRandomly
        :: forall m. MonadRandom m
        => ([(TxIn, TxOut)], UTxO)
        -> MaybeT m ([(TxIn, TxOut)], UTxO)
    coverRandomly (inps, utxo)
        | L.length inps > (fromIntegral maxNumInputs) =
            MaybeT $ return Nothing
        | balance' inps >= targetMin (mkTargetRange txout) =
            MaybeT $ return $ Just (inps, utxo)
        | otherwise = do
            pickRandomT utxo >>= \(io, utxo') -> coverRandomly (io:inps, utxo')

-- | Perform an improvement to random selection on a given output.
improveTxOut
    :: forall m e. MonadRandom m
    => (CoinSelectionOptions e, CoinSelection, UTxO)
    -> ([(TxIn, TxOut)], TxOut)
    -> m (CoinSelectionOptions e, CoinSelection, UTxO)
improveTxOut (opt0, selection, utxo0) (inps0, txout) = do
    (opt, inps, utxo) <- improve (opt0, inps0, utxo0)
    return
        ( opt
        , selection <> CoinSelection
            { inputs = inps
            , outputs = [txout]
            , change = mkChange txout inps
            }
        , utxo
        )
  where
    target = mkTargetRange txout

    improve
        :: forall m e. MonadRandom m
        => (CoinSelectionOptions e, [(TxIn, TxOut)], UTxO)
        -> m (CoinSelectionOptions e, [(TxIn, TxOut)], UTxO)
    improve (opt@(CoinSelectionOptions maxN fn), inps, utxo)
        | maxN >= 1 && balance' inps < targetAim target = do
            runMaybeT (pickRandomT utxo) >>= \case
                Nothing ->
                    return (opt, inps, utxo)
                Just (io, utxo') | isImprovement io inps -> do
                    let inps' = io : inps
                    let opt' = CoinSelectionOptions (maxN - 1) fn
                    improve (opt', inps', utxo')
                Just _ ->
                    return (opt, inps, utxo)
        | otherwise =
            return (opt, inps, utxo)

    isImprovement :: (TxIn, TxOut) -> [(TxIn, TxOut)] -> Bool
    isImprovement io selected =
        let
            condA = -- (a) It doesn’t exceed a specified upper limit.
                balance' (io : selected) < targetMax target

            condB = -- (b) Addition gets us closer to the ideal change
                distance (targetAim target) (balance' (io : selected))
                <
                distance (targetAim target) (balance' selected)

            -- (c) Doesn't exceed maximum number of inputs
            -- Guaranteed by the precondition on 'improve'.
        in
            condA && condB

{-------------------------------------------------------------------------------
                                 Internals
-------------------------------------------------------------------------------}

-- | Re-wrap 'pickRandom' in a 'MaybeT' monad
pickRandomT :: MonadRandom m => UTxO -> MaybeT m ((TxIn, TxOut), UTxO)
pickRandomT =
    MaybeT . fmap (\(m,u) -> (,u) <$> m) . pickRandom

-- | Compute the target range for a given output
mkTargetRange :: TxOut -> TargetRange
mkTargetRange (TxOut _ (Coin c)) = TargetRange
    { targetMin = c
    , targetAim = 2 * c
    , targetMax = 3 * c
    }

-- | Compute corresponding change outputs from a target output and a selection
-- of inputs.
--
-- > pre-condition: the output must be smaller (or eq) than the sum of inputs
mkChange :: TxOut -> [(TxIn, TxOut)] -> [Coin]
mkChange (TxOut _ (Coin out)) inps =
    let
        selected = invariant
            "mkChange: output is smaller than selected inputs!"
            (balance' inps)
            (>= out)
        Coin maxCoinValue = maxBound
    in
        case selected - out of
            c | c > maxCoinValue ->
                let h = (c `div` 2) in [Coin h, Coin (c - h)]
            c | c == 0 ->
                []
            c ->
                [ Coin c ]
