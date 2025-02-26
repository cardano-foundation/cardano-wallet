{-# LANGUAGE TypeFamilies #-}

module Cardano.Wallet.UI.Lib.Pagination.Type
    ( Paginate (..)
    , PaginatePure
    , MkPaginatePure
    , PaginateM
    , MkPaginateM
    )
where

import Prelude

import Control.Monad.Identity
    ( Identity
    )

-- | A type-level tag for the monadic paginators.
data M

-- | A type-level tag for the pure paginators.
data P

-- | A type-level function todistinguish between the pure and monadic paginators.
type family T f m a where
    T M m a = m a
    T P m a = a

-- | A closure of a value of type 'a' that can be paginated using a key of type 'k'.
data Paginate r m k a = Paginate
    { nextIndex :: k -> T r m (Maybe k)
    , previousIndex :: k -> T r m (Maybe k)
    , pageAtIndex :: k -> T r m (Maybe (Int, a))
    , minIndex :: T r m (Maybe k)
    }

type PaginatePure k a = Paginate P Identity k a

type MkPaginatePure k a = Int -> a -> PaginatePure k a

type PaginateM m k a = Paginate M m k a

type MkPaginateM m k a = Int -> Paginate M m k a
