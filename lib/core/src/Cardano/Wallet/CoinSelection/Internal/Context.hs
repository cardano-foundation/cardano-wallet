{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Wallet.CoinSelection.Internal.Context
    ( SelectionContext (..)
    )
    where

import Prelude

import Fmt
    ( Buildable )

class
    ( Buildable (Address c)
    , Buildable (UTxO c)
    , Ord (Address c)
    , Ord (UTxO c)
    , Show (Address c)
    , Show (UTxO c)
    ) =>
    SelectionContext c
  where
    type Address c
    type UTxO c
