module Cardano.Wallet.UI.Lib.Pagination.TimedSeq
    ( Paginate (..)
    , mkTimedSeqPaginate
    )
where

import Prelude

import Cardano.Wallet.Deposit.Map.Timed
    ( TimedSeq
    , minKey
    , takeAfter
    , takeUpTo
    )
import Cardano.Wallet.UI.Lib.Pagination.Type
    ( MkPaginatePure
    , Paginate (..)
    )

mkTimedSeqPaginate
    :: (Ord q, Monoid a, Ord k)
    => (k -> q)
    -> MkPaginatePure k (TimedSeq k a)
mkTimedSeqPaginate proj pageSize a =
    Paginate
        { nextIndex = next pageSize proj a
        , previousIndex = previous pageSize proj a
        , pageAtIndex = \k -> nextPage pageSize proj k a
        , minIndex = minKey a
        }

next
    :: (Monoid a, Ord q, Ord k)
    => Int
    -> (k -> q)
    -> TimedSeq k a
    -> k
    -> Maybe k
next n proj s start' = snd $ takeAfter proj (Just start') (Just n) s

previous
    :: (Ord q, Monoid a, Ord k)
    => Int
    -> (k -> q)
    -> TimedSeq k a
    -> k
    -> Maybe k
previous n proj s start' = snd $ takeUpTo proj (Just start') (Just n) s

nextPage
    :: (Ord q, Monoid a, Ord k)
    => Int
    -> (k -> q)
    -> k
    -> TimedSeq k a
    -> Maybe (Int, TimedSeq k a)
nextPage n proj start' s =
    Just
        $ (\(result, _) -> (length result, result))
        $ takeAfter proj (Just start') (Just n) s
