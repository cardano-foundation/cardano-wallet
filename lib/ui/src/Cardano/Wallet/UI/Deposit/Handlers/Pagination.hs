module Cardano.Wallet.UI.Deposit.Handlers.Pagination
    ( PageHandler (..)
    )
where

import Prelude

-- | A handler for paginating
data PageHandler m k a = PageHandler
    { pagePrevious :: k -> m (Maybe k)
    , pageNext :: k -> m (Maybe k)
    , page :: k -> m a
    , start :: m (Maybe k)
    }
