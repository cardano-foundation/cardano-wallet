module Cardano.Wallet.UI.Deposit.Handlers.Pagination
    ( PaginationHandlers (..)
    )
where

import Prelude

-- | A handler for paginating
data PaginationHandlers m k a = PaginationHandlers
    { previousPageIndex :: k -> m (Maybe k)
    , nextPageIndex :: k -> m (Maybe k)
    , retrievePage :: k -> m a
    , startingIndex :: m (Maybe k)
    }
