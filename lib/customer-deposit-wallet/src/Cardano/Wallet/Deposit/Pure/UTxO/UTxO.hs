{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cardano.Wallet.Deposit.Pure.UTxO.UTxO
    ( UTxO (..)
    , null
    , dom
    , balance
    , excluding

    , filterByAddress
    ) where

import Prelude hiding
    ( null
    )

import Control.DeepSeq
    ( NFData (..)
    )
import Data.Map
    ( Map
    )
import Data.Set
    ( Set
    )
import GHC.Generics
    ( Generic
    )

import qualified Cardano.Wallet.Deposit.Read as Read
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

{-----------------------------------------------------------------------------
    Type
------------------------------------------------------------------------------}

newtype UTxO = UTxO { unUTxO :: Map Read.TxIn Read.TxOut }
    deriving stock (Show, Generic, Eq, Ord)
    deriving newtype (Semigroup, Monoid)

instance NFData UTxO

{-----------------------------------------------------------------------------
    Operations
------------------------------------------------------------------------------}
-- | Test whether a 'UTxO' is empty.
--
-- > null u = Set.null (dom u)
null :: UTxO -> Bool
null (UTxO u) = Map.null u

-- | Domain of a 'UTxO' = the set of /inputs/ of the /utxo/.
dom :: UTxO -> Set Read.TxIn
dom (UTxO utxo) = Map.keysSet utxo

-- | Compute the balance of a UTxO
balance :: UTxO -> Read.Value
balance =
    Map.foldl' fn mempty . unUTxO
  where
    fn :: Read.Value -> Read.TxOut -> Read.Value
    fn tot out = tot <> Read.value out

-- | Exclude a 'Set' of inputs from the inputs of a 'UTxO'.
-- Notation: @ins⋪ u@.
--
-- > dom (u `excluding` ins) = dom u `Set.difference` ins
excluding :: UTxO -> Set Read.TxIn -> UTxO
excluding (UTxO utxo) =
    UTxO . Map.withoutKeys utxo

-- | Filters a 'UTxO' set with an indicator function on 'Address' values.
--
-- Returns the subset of UTxO entries that have addresses for which the given
-- indicator function returns 'True'.
--
-- > filterByAddress (const True) u = u
-- > filterByAddress (const False) u = mempty
-- > filterByAddress f mempty = mempty
-- > filterByAddress f u `isSubsetOf` u
filterByAddress :: (Read.Address -> Bool) -> UTxO -> UTxO
filterByAddress p =
    UTxO . Map.filter (p . Read.address) . unUTxO
