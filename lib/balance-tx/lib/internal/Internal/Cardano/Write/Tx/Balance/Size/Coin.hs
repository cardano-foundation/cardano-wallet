{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}

-- |
-- Copyright: © 2024 Cardano Foundation
-- License: Apache-2.0
--
-- The main purpose of this module is containing 'distributeSurplus', which is
-- needed by 'balanceTransaction'.
module Internal.Cardano.Write.Tx.Balance.Size.Coin
    ( sizeOfCoin
    , costOfIncreasingCoin
    , maximumCostOfIncreasingCoin

    -- * distributeSurplus
    , distributeSurplus
    , TxFeeAndChange (..)
    , ErrMoreSurplusNeeded (..)

    -- ** Internals
    , distributeSurplusDelta
    )
    where

import Prelude

import Data.Functor
    ( (<&>)
    )
import Data.Monoid.Cancellative
    ( (</>)
    )
import Data.Monoid.Monus
    ( (<\>)
    )
import GHC.Generics
    ( Generic
    )
import Internal.Cardano.Write.Tx
    ( FeePerByte (..)
    )

import qualified Cardano.Wallet.Primitive.Types.Coin as W
import qualified Cardano.Wallet.Primitive.Types.Tx.Constraints as W
import qualified Cardano.Wallet.Primitive.Types.Tx.TxOut as W
import qualified Cardano.Wallet.Primitive.Types.Tx.TxOut as W.TxOut

-- | Indicates that it's impossible for 'distributeSurplus' to distribute a
-- surplus. As long as the surplus is larger than 'costOfIncreasingCoin', this
-- should never happen.
newtype ErrMoreSurplusNeeded = ErrMoreSurplusNeeded W.Coin
    deriving (Generic, Eq, Show)

-- | Small helper record to disambiguate between a fee and change Coin values.
-- Used by 'distributeSurplus'.
data TxFeeAndChange change = TxFeeAndChange
    { fee :: W.Coin
    , change :: change
    }
    deriving (Eq, Show)

-- | Manipulates a 'TxFeeAndChange' value.
--
mapTxFeeAndChange
    :: (W.Coin -> W.Coin)
    -- ^ A function to transform the fee
    -> (change1 -> change2)
    -- ^ A function to transform the change
    -> TxFeeAndChange change1
    -- ^ The original fee and change
    -> TxFeeAndChange change2
    -- ^ The transformed fee and change
mapTxFeeAndChange mapFee mapChange TxFeeAndChange{fee, change} =
    TxFeeAndChange (mapFee fee) (mapChange change)

-- | Calculate the cost of increasing a CBOR-encoded Coin-value by another Coin
-- with the lovelace/byte cost given by the 'FeePolicy'.
--
-- Outputs values in the range of [0, 8 * perByteFee]
--
-- >>> let p = FeePolicy (Quantity 0) (Quantity 44)
--
-- >>> costOfIncreasingCoin p 4294967295 1
-- Coin 176 -- (9 bytes - 5 bytes) * 44 lovelace/byte
--
-- >>> costOfIncreasingCoin p 0 4294967296
-- Coin 352 -- 8 bytes * 44 lovelace/byte
costOfIncreasingCoin
    :: FeePerByte
    -> W.Coin -- ^ Original coin
    -> W.Coin -- ^ Increment
    -> W.Coin
costOfIncreasingCoin (FeePerByte perByte) from delta =
    costOfCoin (from <> delta) <\> costOfCoin from
  where
    costOfCoin = W.Coin . (perByte *) . W.unTxSize . sizeOfCoin

-- The maximum cost increase 'costOfIncreasingCoin' can return, which is the
-- cost of 8 bytes.
maximumCostOfIncreasingCoin :: FeePerByte -> W.Coin
maximumCostOfIncreasingCoin (FeePerByte perByte) = W.Coin $ 8 * perByte

-- | Calculate the size of a coin when encoded as CBOR.
sizeOfCoin :: W.Coin -> W.TxSize
sizeOfCoin (W.Coin c)
    | c >= 4_294_967_296 = W.TxSize 9 -- c >= 2^32
    | c >=        65_536 = W.TxSize 5 -- c >= 2^16
    | c >=           256 = W.TxSize 3 -- c >= 2^ 8
    | c >=            24 = W.TxSize 2
    | otherwise          = W.TxSize 1

-- | Distributes a surplus transaction balance between the given change
-- outputs and the given fee. This function is aware of the fact that
-- any increase in a 'Coin' value could increase the size and fee
-- requirement of a transaction.
--
-- When comparing the original fee and change outputs to the adjusted
-- fee and change outputs, this function guarantees that:
--
--    - The number of the change outputs remains constant;
--
--    - The fee quantity either remains the same or increases.
--
--    - For each change output:
--        - the ada quantity either remains constant or increases.
--        - non-ada quantities remain the same.
--
--    - The surplus is conserved:
--        The total increase in the fee and change ada quantities is
--        exactly equal to the surplus.
--
--    - Any increase in cost is covered:
--        If the total cost has increased by ??c, then the fee value
--        will have increased by at least ??c.
--
-- If the cost of distributing the provided surplus is greater than the
-- surplus itself, the function will return 'ErrMoreSurplusNeeded'. If
-- the provided surplus is greater or equal to
-- @maximumCostOfIncreasingCoin feePolicy@, the function will always
-- return 'Right'.
distributeSurplus
    :: FeePerByte
    -> W.Coin
    -- ^ Surplus transaction balance to distribute.
    -> TxFeeAndChange [W.TxOut]
    -- ^ Original fee and change outputs.
    -> Either ErrMoreSurplusNeeded (TxFeeAndChange [W.TxOut])
    -- ^ Adjusted fee and change outputs.
distributeSurplus feePolicy surplus fc@(TxFeeAndChange fee change) =
    distributeSurplusDelta feePolicy surplus
        (mapTxFeeAndChange id (fmap W.TxOut.coin) fc)
    <&> mapTxFeeAndChange
        (fee <>)
        (zipWith (flip W.TxOut.addCoin) change)

distributeSurplusDelta
    :: FeePerByte
    -> W.Coin
    -- ^ Surplus to distribute
    -> TxFeeAndChange [W.Coin]
    -> Either ErrMoreSurplusNeeded (TxFeeAndChange [W.Coin])
distributeSurplusDelta feePolicy surplus (TxFeeAndChange fee change) =
    case change of
        changeHead : changeTail ->
            distributeSurplusDeltaWithOneChangeCoin feePolicy surplus
                (TxFeeAndChange fee changeHead)
            <&> mapTxFeeAndChange id
                (: (W.Coin 0 <$ changeTail))
        [] ->
            burnSurplusAsFees feePolicy surplus
                (TxFeeAndChange fee ())
            <&> mapTxFeeAndChange id
                (\() -> [])

distributeSurplusDeltaWithOneChangeCoin
    :: FeePerByte
    -> W.Coin -- ^ Surplus to distribute
    -> TxFeeAndChange W.Coin
    -> Either ErrMoreSurplusNeeded (TxFeeAndChange W.Coin)
distributeSurplusDeltaWithOneChangeCoin
    feePolicy surplus fc@(TxFeeAndChange fee0 change0) =
    let
        -- We calculate the maximum possible fee increase, by assuming the
        -- **entire** surplus is added to the change.
        extraFee = findFixpointIncreasingFeeBy $
            costOfIncreasingCoin feePolicy change0 surplus
    in
        case surplus </> extraFee of
            Just extraChange ->
                Right $ TxFeeAndChange
                    { fee = extraFee
                    , change = extraChange
                    }
            Nothing ->
                -- The fee increase from adding the surplus to the change was
                -- greater than the surplus itself. This could happen if the
                -- surplus is small.
                burnSurplusAsFees feePolicy surplus
                    (mapTxFeeAndChange id (const ()) fc)
                        <&> mapTxFeeAndChange id (\() -> W.Coin 0)
  where
    -- Increasing the fee may itself increase the fee. If that is the case, this
    -- function will increase the fee further. The process repeats until the fee
    -- doesn't need to be increased.
    --
    -- The function will always converge because the result of
    -- 'costOfIncreasingCoin' is bounded to @8 * feePerByte@.
    --
    -- On mainnet it seems unlikely that the function would recurse more than
    -- one time, and certainly not more than twice. If the protocol parameters
    -- are updated to allow for slightly more expensive txs, it might be
    -- possible to hit the boundary at Å4 ada where the fee would need 9 bytes
    -- rather than 5. This is already the largest boundary.
    --
    -- Note that both the argument and the result of this function are increases
    -- relative to 'fee0'.
    --
    -- == Example ==
    --
    -- In this more extreme example the fee is increased from increasing the fee
    -- itself:
    --
    -- @@
    --     let fee0 = 23
    --     let feePolicy = -- 300 lovelace / byte
    --
    --     findFixpointIncreasingFeeBy 1 = go 0 1
    --     -- Recurse:
    --     = go (0 + 1) (costOfIncreasingCoin feePolicy (23 + 0) 1)
    --     = go (0 + 1) 300
    --     -- Recurse:
    --     = go (1 + 300) (costOfIncreasingCoin feePolicy (23 + 1) 300)
    --     = go 301 300
    --     = go (301 + 300) (costOfIncreasingCoin feePolicy (23 + 301) 300)
    --     = go (301 + 300) 0
    --     = go 601 0
    --     = 601
    -- @@
    findFixpointIncreasingFeeBy = go mempty
      where
        go :: W.Coin -> W.Coin -> W.Coin
        go c (W.Coin 0) = c
        go c increase = go
            (c <> increase)
            (costOfIncreasingCoin feePolicy (c <> fee0) increase)

burnSurplusAsFees
    :: FeePerByte
    -> W.Coin -- Surplus
    -> TxFeeAndChange ()
    -> Either ErrMoreSurplusNeeded (TxFeeAndChange ())
burnSurplusAsFees feePolicy surplus (TxFeeAndChange fee0 ())
    | shortfall > W.Coin 0 =
        Left $ ErrMoreSurplusNeeded shortfall
    | otherwise =
        Right $ TxFeeAndChange surplus ()
  where
    costOfBurningSurplus = costOfIncreasingCoin feePolicy fee0 surplus
    shortfall = costOfBurningSurplus <\> surplus
