{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.Wallet.Transaction.Jormungandr
    ( newTransactionLayer
    ) where

import Prelude

import Cardano.Wallet.Transaction
    ( TransactionLayer (..) )


-- | Construct a 'TransactionLayer' compatible with Shelley and 'Jörmungandr'
newTransactionLayer :: TransactionLayer
newTransactionLayer = TransactionLayer
    { mkStdTx = error "TODO: See http-bridge as starting point"
    , estimateSize = error "TODO: See http-bridge as starting point"
    }
