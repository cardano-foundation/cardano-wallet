module Cardano.Wallet.Deposit.Pure
    (
    -- * Types
      WalletState
    , DeltaWalletState

    -- * Operations
    -- ** Mapping between customers and addresses
    , Customer
    , listCustomers
    , createAddress
    , deriveAddress
    , knownCustomer
    , knownCustomerAddress
    , isCustomerAddress

    -- ** Reading from the blockchain
    , fromGenesis
    , localTip
    , availableBalance
    , rollForwardMany
    , rollForwardOne
    , rollBackward

    , TxSummary (..)
    , ValueTransfer (..)
    , getCustomerHistory

    -- ** Writing to the blockchain
    , createPayment

    , addTxSubmission
    , listTxsInSubmission

    -- * Internal
    , fromGenesisUTxO
    ) where

import Prelude

import Cardano.Wallet.Deposit.Pure.UTxOHistory
    ( UTxOHistory )
import Cardano.Wallet.Deposit.Read
    ( Address )
import Data.Foldable
    ( foldl' )
import Data.List.NonEmpty
    ( NonEmpty )
import Data.Maybe
    ( isJust )
import Data.Set
    ( Set )
import Numeric.Natural
    ( Natural )

import qualified Cardano.Wallet.Deposit.Pure.Balance as Balance
import qualified Cardano.Wallet.Deposit.Pure.Submissions as Sbm
import qualified Cardano.Wallet.Deposit.Pure.UTxO as UTxO
import qualified Cardano.Wallet.Deposit.Pure.UTxOHistory as UTxOHistory
import qualified Cardano.Wallet.Deposit.Read as Read
import qualified Cardano.Wallet.Deposit.Write as Write
import qualified Data.Delta as Delta
import qualified Data.Map.Strict as Map

{-----------------------------------------------------------------------------
    Types
------------------------------------------------------------------------------}
type Customer = Natural

data WalletState = WalletState
    { customers :: !(Map.Map Customer Address)
    , changeAddress :: !Address
    , utxoHistory :: !UTxOHistory.UTxOHistory
    -- , txHistory :: [Read.Tx]
    , submissions :: Sbm.TxSubmissions
    -- , credentials :: Maybe (HashedCredentials (KeyOf s))
    -- , info :: !WalletInfo
    }
    deriving (Eq, Show)

type DeltaWalletState = Delta.Replace WalletState

{-----------------------------------------------------------------------------
    Operations
    Mapping between customers and addresses
------------------------------------------------------------------------------}

listCustomers :: WalletState -> [(Customer, Address)]
listCustomers = Map.toList . customers

createAddress :: Customer -> WalletState -> (Address, WalletState)
createAddress = undefined

-- depend on the private key only, not on the entire wallet state
deriveAddress :: WalletState -> (Customer -> Address)
deriveAddress = undefined

knownCustomer :: Customer -> WalletState -> Bool
knownCustomer c = (c `Map.member`) . customers

knownCustomerAddress :: Address -> WalletState -> Bool
knownCustomerAddress address = isJust . isCustomerAddress address

isCustomerAddress :: Address -> WalletState -> Maybe Customer
isCustomerAddress _ _ = Nothing

{-----------------------------------------------------------------------------
    Operations
    Reading from the blockchain
------------------------------------------------------------------------------}

fromGenesis :: Read.GenesisData -> WalletState
fromGenesis = undefined

fromGenesisUTxO :: Read.UTxO -> WalletState
fromGenesisUTxO utxo =
    WalletState
        { customers = Map.empty
        , changeAddress = Read.dummyAddress
        , utxoHistory = UTxOHistory.empty utxo
        , submissions = Sbm.empty
        }

localTip :: WalletState -> Read.ChainPoint
localTip = error "localTip"

rollForwardMany :: NonEmpty Read.Block -> WalletState -> WalletState
rollForwardMany blocks w = foldl' (flip rollForwardOne) w blocks

rollForwardOne :: Read.Block -> WalletState -> WalletState
rollForwardOne block w =
    w
        { utxoHistory = rollForwardUTxO isOurs block (utxoHistory w)
        , submissions = Delta.apply (Sbm.rollForward block) (submissions w)
        }
  where
    isOurs :: Address -> Bool
    isOurs addr =
        ( addr == changeAddress w ) || knownCustomerAddress addr w
        -- FIXME: Consider payment part only, ignore staking part.

rollForwardUTxO
    :: (Address -> Bool) -> Read.Block -> UTxOHistory -> UTxOHistory
rollForwardUTxO isOurs block u =
    Delta.apply (UTxOHistory.AppendBlock slot deltaUTxO) u
  where
    (deltaUTxO,_) = Balance.applyBlock isOurs block (UTxOHistory.getUTxO u)
    slot = Read.slot . Read.blockHeaderBody $ Read.blockHeader block

rollBackward
    :: Read.ChainPoint
    -> WalletState
    -> (WalletState, Read.ChainPoint)
rollBackward = undefined

availableBalance :: WalletState -> Read.Value
availableBalance w =
    UTxO.balance $ Balance.availableUTxO utxo pending
  where
    pending = listTxsInSubmission w
    utxo = UTxOHistory.getUTxO $ utxoHistory w

data TxSummary = TxSummary
    { txid :: Read.TxId
    , blockHeaderBody :: Read.BHBody
    , transfer :: ValueTransfer
    }

data ValueTransfer = ValueTransfer
    { spent :: Read.Value
    , received :: Read.Value
    }

getCustomerHistory :: Customer -> WalletState -> [TxSummary]
getCustomerHistory = undefined

{-----------------------------------------------------------------------------
    Operations
    Writing to blockchain
------------------------------------------------------------------------------}

createPayment :: [(Address, Write.Value)] -> WalletState -> Maybe Write.Tx
createPayment = undefined
    -- needs balanceTx
    -- needs to sign the transaction

addTxSubmission :: Write.Tx -> WalletState -> WalletState
addTxSubmission _tx _w = undefined

listTxsInSubmission :: WalletState -> Set Write.Tx
-- listTxsInSubmission = Sbm.listInSubmission . submissions
listTxsInSubmission _ = mempty
