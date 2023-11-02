{-# LANGUAGE TypeFamilies #-}
module Cardano.Wallet.Deposit.Pure.UTxO.UTxOHistory
    ( UTxOHistory
    , empty

    , DeltaUTxOHistory (..)
    , getUTxO
    ) where

{- Note [MockUTxOHistory]

At the moment, this module implements a mock version of

import Cardano.Wallet.DB.Store.UTxOHistory.Model

in order to work with a 'UTxO' that is different from the
'Cardano.Wallet.Primitive.*' types.

Eventually, the implementation here should subsume the implementation
in 'UTxOHistory.Model'.

-}

import Prelude

import Cardano.Wallet.Deposit.Pure.UTxO.DeltaUTxO
    ( DeltaUTxO
    )
import Cardano.Wallet.Deposit.Pure.UTxO.UTxO
    ( UTxO
    )
import Data.Delta
    ( Delta (..)
    )

import qualified Cardano.Wallet.Deposit.Read as Read

{-----------------------------------------------------------------------------
    Type and Operations
------------------------------------------------------------------------------}

newtype UTxOHistory
    = UTxOHistory {latest :: UTxO}
    deriving (Eq, Show)

-- | An empty UTxO history.
empty :: UTxO -> UTxOHistory
empty = UTxOHistory

-- | Returns the most recent UTxO.
getUTxO :: UTxOHistory -> UTxO
getUTxO = latest

{-----------------------------------------------------------------------------
    Delta type
------------------------------------------------------------------------------}

-- | Changes to the UTxO history.
data DeltaUTxOHistory
    = -- | New slot tip, changes within that block.
      AppendBlock Read.Slot DeltaUTxO

instance Delta DeltaUTxOHistory where
    type Base DeltaUTxOHistory = UTxOHistory
    apply (AppendBlock newTip delta) = appendBlock newTip delta

appendBlock :: Read.Slot -> DeltaUTxO -> UTxOHistory -> UTxOHistory
appendBlock _newTip delta (UTxOHistory u)
    = UTxOHistory (apply delta u)
