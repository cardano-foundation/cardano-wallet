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
    , Word31
    , getWalletTip
    , availableBalance
    , rollForwardMany
    , rollForwardOne
    , rollBackward

    , TxSummary (..)
    , ValueTransfer (..)
    , getCustomerHistory
    , getCustomerHistories

    -- ** Writing to the blockchain
    , createPayment
    , getBIP32PathsForOwnedInputs

    , addTxSubmission
    , listTxsInSubmission
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
import Data.Bifunctor
    ( second
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
import Data.Set
    ( Set
    )
import Data.Word.Odd
    ( Word31
    )

import qualified Cardano.Wallet.Deposit.Pure.Address as Address
import qualified Cardano.Wallet.Deposit.Pure.Balance as Balance
import qualified Cardano.Wallet.Deposit.Pure.Submissions as Sbm
import qualified Cardano.Wallet.Deposit.Pure.UTxO as UTxO
import qualified Cardano.Wallet.Deposit.Pure.UTxOHistory as UTxOHistory
import qualified Cardano.Wallet.Deposit.Read as Read
import qualified Cardano.Wallet.Deposit.Write as Write
import qualified Data.Delta as Delta

{-----------------------------------------------------------------------------
    Types
------------------------------------------------------------------------------}
type Customer = Address.Customer

data WalletState = WalletState
    { addresses :: !Address.AddressState
    , utxoHistory :: !UTxOHistory.UTxOHistory
    -- , txHistory :: [Read.Tx]
    , submissions :: Sbm.TxSubmissions
    -- , credentials :: Maybe (HashedCredentials (KeyOf s))
    -- , info :: !WalletInfo
    }

type DeltaWalletState = Delta.Replace WalletState

{-----------------------------------------------------------------------------
    Operations
    Mapping between customers and addresses
------------------------------------------------------------------------------}

listCustomers :: WalletState -> [(Customer, Address)]
listCustomers =
    map (second Read.fromRawAddress)
    . Address.listCustomers . addresses

createAddress :: Customer -> WalletState -> (Address, WalletState)
createAddress customer w0 =
    (Read.fromRawAddress address, w0{addresses = s1})
  where
    (address, s1) = Address.createAddress customer (addresses w0)

-- depend on the private key only, not on the entire wallet state
deriveAddress :: WalletState -> (Customer -> Address)
deriveAddress w =
    Read.fromRawAddress
    . Address.deriveAddress (Address.getXPub (addresses w))
    . Address.DerivationCustomer

-- FIXME: More performant with a double index.
knownCustomer :: Customer -> WalletState -> Bool
knownCustomer c = (c `elem`) . map fst . listCustomers

knownCustomerAddress :: Address -> WalletState -> Bool
knownCustomerAddress address =
    Address.knownCustomerAddress (Read.toRawAddress address) . addresses

isCustomerAddress :: Address -> WalletState -> Bool
isCustomerAddress address =
    flip Address.isCustomerAddress (Read.toRawAddress address) . addresses

{-----------------------------------------------------------------------------
    Operations
    Reading from the blockchain
------------------------------------------------------------------------------}

fromXPubAndGenesis :: XPub -> Word31 -> Read.GenesisData -> WalletState
fromXPubAndGenesis xpub knownCustomerCount _ =
    WalletState
        { addresses =
            Address.fromXPubAndCount xpub knownCustomerCount
        , utxoHistory = UTxOHistory.empty initialUTxO
        , submissions = Sbm.empty
        }
  where
    initialUTxO = mempty

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
    isOurs = Address.isOurs (addresses w) . Read.toRawAddress

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
    deriving (Eq, Ord, Show)

data ValueTransfer = ValueTransfer
    { spent :: Read.Value
    , received :: Read.Value
    }
    deriving (Eq, Ord, Show)

getCustomerHistory :: Customer -> WalletState -> [TxSummary]
getCustomerHistory = undefined

-- TODO: Return an error if any of the `ChainPoint` are no longer
-- part of the consensus chain?
getCustomerHistories
    :: (Read.ChainPoint, Read.ChainPoint)
    -> WalletState
    -> Map Customer ValueTransfer
getCustomerHistories = undefined

{-----------------------------------------------------------------------------
    Operations
    Writing to blockchain
------------------------------------------------------------------------------}

createPayment :: [(Address, Write.Value)] -> WalletState -> Maybe Write.TxBody
createPayment = undefined
    -- needs balanceTx
    -- needs to sign the transaction

getBIP32PathsForOwnedInputs :: Write.TxBody -> WalletState -> [()]
getBIP32PathsForOwnedInputs = undefined

addTxSubmission :: Write.Tx -> WalletState -> WalletState
addTxSubmission _tx _w = undefined

listTxsInSubmission :: WalletState -> Set Write.Tx
-- listTxsInSubmission = Sbm.listInSubmission . submissions
listTxsInSubmission _ = mempty
