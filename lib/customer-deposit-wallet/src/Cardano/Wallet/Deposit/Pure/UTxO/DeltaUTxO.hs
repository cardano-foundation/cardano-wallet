{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Wallet.Deposit.Pure.UTxO.DeltaUTxO
    ( DeltaUTxO
    , excludingD
    , receiveD
    ) where

import Prelude

import Cardano.Wallet.Deposit.Pure.UTxO.UTxO
    ( UTxO (..)
    , dom
    , excluding
    )
import Control.DeepSeq
    ( NFData (..)
    )
import Data.Delta
    ( Delta (..)
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

-- | Efficient delta encoding for 'UTxO'.
data DeltaUTxO = DeltaUTxO
    { excluded :: !(Set Read.TxIn)
        -- ^ First exclude these inputs
    , received :: !UTxO
        -- ^ Then receive these additional outputs.
    } deriving (Generic, Eq, Show)

instance NFData DeltaUTxO

{-----------------------------------------------------------------------------
    Operations
------------------------------------------------------------------------------}

instance Delta DeltaUTxO where
    type Base DeltaUTxO = UTxO
    du `apply` u = (u `excluding` excluded du) <> received du

-- | Left argument is applied /after/ right argument.
instance Semigroup DeltaUTxO where
    db <> da = DeltaUTxO
        { excluded = excluded da <> excluded'db
        , received = received'da <> received db
        }
      where
        received'da = received da `excluding` excluded db
        excluded'db = excluded db `excludingS` received da

instance Monoid DeltaUTxO where
    mempty = DeltaUTxO { excluded = mempty, received = mempty }

-- | Exclude a set of transaction inputs, typically because we spend them.
excludingD :: UTxO -> Set Read.TxIn -> (DeltaUTxO, UTxO)
excludingD u ins = (du, u `excluding` spent)
  where
    spent = ins `restrictedByS` u
    du = DeltaUTxO { excluded = spent, received = mempty }

-- | Receive additional 'UTxO' / union.
receiveD :: UTxO -> UTxO -> (DeltaUTxO, UTxO)
receiveD a b = (da, a <> new)
  where
    new =  b `excluding` dom a
    da = DeltaUTxO { excluded = mempty, received = new}

{-----------------------------------------------------------------------------
    Helpers
------------------------------------------------------------------------------}
-- | Exclude the inputs of a 'UTxO' from a 'Set' of inputs.
--
-- > ins `excludingS` u = ins `Set.difference` dom u
excludingS :: Set Read.TxIn -> UTxO -> Set Read.TxIn
excludingS a (UTxO b) = Set.filter (not . (`Map.member` b)) a

-- | Restrict a 'Set' of inputs by the inputs of a 'UTxO'.
--
-- > ins `restrictedByS` u = ins `Set.intersection` dom u
restrictedByS :: Set Read.TxIn -> UTxO -> Set Read.TxIn
restrictedByS a (UTxO b) = Set.filter (`Map.member` b) a
