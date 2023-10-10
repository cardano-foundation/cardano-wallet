-- | Indirection module that re-exports types
-- used for writing transactions to the blockchain,
-- in the most recent and the next future eras.
--
-- TODO: Match this up with the @Write@ hierarchy.
module Cardano.Wallet.Deposit.Write
    ( Address

    , Value

    , TxId
    , Tx (..)
    , TxBody (..)
    , TxWitness
    ) where

import Prelude

import Data.Map
    ( Map )
import Data.Set
    ( Set )

import Cardano.Wallet.Deposit.Read hiding
    ( Tx, TxBody )

{-----------------------------------------------------------------------------
    Type definitions
    with dummies
------------------------------------------------------------------------------}
data Tx = Tx
    { txbody :: TxBody
    , txwits :: TxWitness
    }
    deriving (Eq, Ord, Show)

data TxBody = TxBody
    { spendInputs :: Set TxIn
    , collInputs :: Set TxIn
    , txouts :: Map Ix TxOut
    , collRet :: Maybe TxOut
    }
    deriving (Eq, Ord, Show)
