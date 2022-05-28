{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}


module Cardano.Wallet.DB.Store.TxHistory.Types where

import qualified Cardano.Wallet.DB.Model as Model
import Data.Delta
    ( Delta (..) )
import Fmt
    ( Buildable (build) )
import Prelude

newtype TxHistory = TxHistory Model.TxHistory deriving (Eq)

instance Buildable TxHistory where
    build (TxHistory txs) = "TxHistory " <> build (length txs)

instance Semigroup TxHistory where
    TxHistory xs <> TxHistory ys = TxHistory zs
        where zs = xs <> ys -- FIXME with invariants

instance Monoid TxHistory where
    mempty = TxHistory []

newtype DeltaTxHistory = DeltaTxHistory TxHistory

instance Buildable DeltaTxHistory where
    build (DeltaTxHistory (TxHistory txs))
        = "DeltaTxHistory " <> build (length txs)

instance Delta DeltaTxHistory where
    type Base DeltaTxHistory = TxHistory
    apply (DeltaTxHistory txs) h = h <> txs
