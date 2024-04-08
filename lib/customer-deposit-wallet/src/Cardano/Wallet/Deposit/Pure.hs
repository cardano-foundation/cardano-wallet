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
    , fromXPubAndGenesis
    , getWalletTip
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

import Cardano.Crypto.Wallet
    ( XPub
    )
import Cardano.Wallet.Deposit.Pure.UTxOHistory
    ( UTxOHistory
    )
import Cardano.Wallet.Deposit.Read
    ( Address
    )
import Data.Foldable
    ( foldl'
    )
import Data.List.NonEmpty
    ( NonEmpty
    )
import Data.Map.Strict
    ( Map
    )
import Data.Maybe
    ( isJust
    )
import Data.Set
    ( Set
    )
import Numeric.Natural
    ( Natural
    )

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
    { customers :: !(Map Customer Address)
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
createAddress customer w1 = (address, w2)
  where
    address = deriveAddress w1 customer
    w2 = w1{customers = Map.insert customer address (customers w1)}

-- depend on the private key only, not on the entire wallet state
deriveAddress :: WalletState -> (Customer -> Address)
deriveAddress _ = Read.mockAddress

knownCustomer :: Customer -> WalletState -> Bool
knownCustomer c = (c `Map.member`) . customers

knownCustomerAddress :: Address -> WalletState -> Bool
knownCustomerAddress address = isJust . isCustomerAddress address

isCustomerAddress :: Address -> WalletState -> Maybe Customer
isCustomerAddress address w =
    case filter ((== address) . snd) (Map.toList $ customers w) of
        [(customer,_address)] -> Just customer
        _ -> Nothing

{-----------------------------------------------------------------------------
    Operations
    Reading from the blockchain
------------------------------------------------------------------------------}

fromXPubAndGenesis :: XPub -> Integer -> Read.GenesisData -> WalletState
fromXPubAndGenesis _xpub _knownCustomerCount _ = fromGenesisUTxO mempty
    -- FIXME: This is a mock implementation

fromGenesisUTxO :: Read.UTxO -> WalletState
fromGenesisUTxO utxo =
    WalletState
        { customers = Map.empty
        , changeAddress = Read.dummyAddress
        , utxoHistory = UTxOHistory.empty utxo
        , submissions = Sbm.empty
        }

getWalletTip :: WalletState -> Read.ChainPoint
getWalletTip = error "getWalletTip"

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
rollBackward point w = (w, point) -- FIXME: This is a mock implementation

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

createPayment :: [(Address, Write.Value)] -> WalletState -> Maybe Write.TxBody
createPayment = undefined
    -- needs balanceTx
    -- needs to sign the transaction

addTxSubmission :: Write.Tx -> WalletState -> WalletState
addTxSubmission _tx _w = undefined

listTxsInSubmission :: WalletState -> Set Write.Tx
-- listTxsInSubmission = Sbm.listInSubmission . submissions
listTxsInSubmission _ = mempty
