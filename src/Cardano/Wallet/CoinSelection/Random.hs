{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: MIT
--
-- This module contains the implementation of random
-- input selection algorithm


module Cardano.Wallet.CoinSelection.Random
    ( random
    ) where

import Prelude

import Cardano.Wallet.CoinSelection
    ( CoinSelection (..)
    , CoinSelectionError (..)
    , CoinSelectionOptions (..)
    , pickRandom
    )
import Cardano.Wallet.CoinSelection.LargestFirst
    ( largestFirst )
import Cardano.Wallet.Primitive.Types
    ( Coin (..), TxIn, TxOut (..), UTxO (..), balance, distance, invariant )
import Control.Monad
    ( foldM )
import Control.Monad.Trans.Class
    ( lift )
import Control.Monad.Trans.Except
    ( ExceptT (..) )
import Control.Monad.Trans.Maybe
    ( MaybeT (..), runMaybeT )
import Crypto.Random.Types
    ( MonadRandom )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Ord
    ( comparing )
import Data.Word
    ( Word64 )

import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map


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
--    (In the rare case that this fails because the maximum number of transaction
--    inputs has been exceeded, fall-back on the largest-first algorithm for this
--    step.)
--
-- 2. Randomly select outputs from the UTxO, considering for each output if that
--    output is animprovement. If it is, add it to the transaction, and keep
--    going. An output is considered an improvement when:
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
    :: forall m. MonadRandom m
    => CoinSelectionOptions
    -> UTxO
    -> NonEmpty TxOut
    -> ExceptT CoinSelectionError m CoinSelection
random opt utxo outs = do
    let descending = NE.toList . NE.sortBy (flip $ comparing coin)
    randomMaybe <- lift $ runMaybeT $ foldM
        (processTxOut opt)
        (utxo, mempty)
        (descending outs)
    case randomMaybe of
        Just (_,res) ->
            return res
        Nothing ->
            largestFirst opt utxo outs

-- | Perform a random selection on a given output, with improvement.
processTxOut
    :: forall m. MonadRandom m
    => CoinSelectionOptions
    -> (UTxO, CoinSelection)
    -> TxOut
    -> MaybeT m (UTxO, CoinSelection)
processTxOut (CoinSelectionOptions maxNumInputs) (utxo0, selection) txout = do
    attempt <- coverRandomly ([], utxo0)
    (inps, utxo') <- lift (improve attempt)
    return
        ( utxo'
        , selection <> CoinSelection
            { inputs = inps
            , outputs = [txout]
            , change = mkChange txout inps
            }
        )
  where
    target :: TargetRange
    target = mkTargetRange txout

    coverRandomly
        :: forall m. MonadRandom m
        => ([(TxIn, TxOut)], UTxO)
        -> MaybeT m ([(TxIn, TxOut)], UTxO)
    coverRandomly (inps, utxo)
        | L.length inps > (fromIntegral maxNumInputs) =
            MaybeT $ return Nothing
        | balance' inps >= targetMin target =
            MaybeT $ return $ Just (inps, utxo)
        | otherwise = do
            pickRandom utxo >>= \(io, utxo') -> coverRandomly (io:inps, utxo')

    improve
        :: forall m. MonadRandom m
        => ([(TxIn, TxOut)], UTxO)
        -> m ([(TxIn, TxOut)], UTxO)
    improve (inps, utxo) =
        runMaybeT (pickRandom utxo) >>= \case
            Nothing ->
                return (inps, utxo)
            Just (io, utxo') | isImprovement io inps -> do
                let inps' = io : inps
                if balance' inps' >= targetAim target
                    then return (inps', utxo')
                    else improve (inps', utxo')
            Just _ ->
                return (inps, utxo)

    isImprovement :: (TxIn, TxOut) -> [(TxIn, TxOut)] -> Bool
    isImprovement io selected =
        let
            condA = -- (a) It doesn’t exceed a specified upper limit.
                balance' (io : selected) < targetMax target

            condB = -- (b) Addition gets us closer to the ideal change
                distance (targetAim target) (balance' (io : selected))
                <
                distance (targetAim target) (balance' selected)

            condC = -- (c) Doesn't exceed maximum number of inputs
                length (io : selected) <= fromIntegral maxNumInputs
        in
            condA && condB && condC

{-------------------------------------------------------------------------------
                                 Internals
-------------------------------------------------------------------------------}

-- | Compute the target range for a given output
mkTargetRange :: TxOut -> TargetRange
mkTargetRange (TxOut _ (Coin c)) = TargetRange
    { targetMin = c
    , targetAim = 2 * c
    , targetMax = 3 * c
    }

-- | Compute the balance of a unwrapped UTxO
balance' :: [(TxIn, TxOut)] -> Word64
balance' =
    fromIntegral . balance . UTxO . Map.fromList

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
            (> out)
        Coin maxCoinValue = maxBound
    in
        case selected - out of
            c | c > maxCoinValue ->
                let h = (c `div` 2) in [Coin h, Coin (c - h)]
            c | c == 0 ->
                []
            c ->
                [ Coin c ]
