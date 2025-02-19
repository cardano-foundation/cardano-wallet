{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.Deposit.Testing.DSL.Types where

import Prelude

import Cardano.Wallet.Deposit.Read
    ( Ix
    )

newtype TxI = TxI Int
    deriving (Eq, Ord, Show, Num)

newtype UnspentI = UnspentI (TxI, Ix)
    deriving (Eq, Ord, Show)

newtype BlockI = BlockI Int
    deriving (Eq, Ord, Show, Num)

newtype TimeI = TimeI Int
    deriving (Eq, Ord, Show)
