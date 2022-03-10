{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Wallet.CoinSelection.Internal.Context
    ( Dummy (..)
    , SelectionContext (..)
    )
    where

import Prelude

import Fmt
    ( Buildable )

class Dummy d where
    dummy :: d

class
    ( Buildable (Address c)
    , Buildable (UTxO c)
    , Dummy (Address c)
    , Ord (Address c)
    , Ord (UTxO c)
    , Show (Address c)
    , Show (UTxO c)
    ) =>
    SelectionContext c
  where
    type Address c
    type UTxO c
