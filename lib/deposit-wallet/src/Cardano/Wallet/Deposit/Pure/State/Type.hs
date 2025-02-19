{-# LANGUAGE StrictData #-}
module Cardano.Wallet.Deposit.Pure.State.Type
    ( -- * Types
      WalletState (..)
    , DeltaWalletState
    , Customer

      -- * Operations
    , listCustomers
    , customerAddress
    , addressToCustomer
    , deriveAddress
    , knownCustomer
    , knownCustomerAddress
    , isCustomerAddress
    , fromRawCustomer
    , trackedCustomers
    , walletXPub
    , getUTxO
    , getWalletTip
    , networkTag
    ) where

import Prelude hiding
    ( lookup
    )

import Cardano.Crypto.Wallet
    ( XPrv
    , XPub
    )
import Cardano.Wallet.Deposit.Pure.API.TxHistory
    ( TxHistory (..)
    )
import Cardano.Wallet.Deposit.Read
    ( NetworkTag
    )
import Cardano.Wallet.Deposit.Write
    ( Address
    )
import Data.Word.Odd
    ( Word31
    )

import qualified Cardano.Wallet.Deposit.Pure.Address as Address
import qualified Cardano.Wallet.Deposit.Pure.Submissions as Sbm
import qualified Cardano.Wallet.Deposit.Pure.UTxO.UTxO as UTxO
import qualified Cardano.Wallet.Deposit.Pure.UTxO.UTxOHistory as UTxOHistory
import qualified Cardano.Wallet.Deposit.Read as Read
import qualified Data.Delta as Delta
import qualified Data.List as L
import qualified Data.Map.Strict as Map

type Customer = Address.Customer

data WalletState = WalletState
    { walletTip :: Read.ChainPoint
    -- ^ The wallet includes information from all blocks until
    -- and including this one.
    , addresses :: Address.AddressState
    -- ^ Addresses and public keys known to this wallet.
    , utxoHistory :: UTxOHistory.UTxOHistory
    -- ^ UTxO of this wallet, with support for rollbacks.
    , txHistory :: TxHistory
    -- ^ (Summarized) transaction history of this wallet.
    , submissions :: Sbm.TxSubmissions
    -- ^ Queue of pending transactions.
    , rootXSignKey :: Maybe XPrv
    -- ^ Maybe a private key for signing transactions.
    -- , info :: WalletInfo
    }

type DeltaWalletState = Delta.Replace WalletState

listCustomers :: WalletState -> [(Customer, Address)]
listCustomers =
    Address.listCustomers . addresses

customerAddress :: Customer -> WalletState -> Maybe Address
customerAddress c = L.lookup c . listCustomers

addressToCustomer :: Address -> WalletState -> Maybe Customer
addressToCustomer address =
    Map.lookup address
        . Map.fromList
        . fmap (\(a, c) -> (c, a))
        . listCustomers

-- depend on the public key only, not on the entire wallet state
deriveAddress :: WalletState -> (Customer -> Address)
deriveAddress w =
    Address.deriveCustomerAddress
        (Address.getNetworkTag as)
        (Address.getXPub as)
  where
    as = addresses w

-- FIXME: More performant with a double index.
knownCustomer :: Customer -> WalletState -> Bool
knownCustomer c = (c `elem`) . map fst . listCustomers

knownCustomerAddress :: Address -> WalletState -> Bool
knownCustomerAddress address =
    Address.knownCustomerAddress address . addresses

isCustomerAddress :: Address -> WalletState -> Bool
isCustomerAddress address =
    flip Address.isCustomerAddress address . addresses

fromRawCustomer :: Word31 -> Customer
fromRawCustomer = id

-- | Maximum 'Customer' that is being tracked.
trackedCustomers :: WalletState -> Customer
trackedCustomers = (+1) . Address.getMaxCustomer . addresses

walletXPub :: WalletState -> XPub
walletXPub = Address.getXPub . addresses

getUTxO :: WalletState -> UTxO.UTxO
getUTxO = UTxOHistory.getUTxO . utxoHistory

getWalletTip :: WalletState -> Read.ChainPoint
getWalletTip = walletTip

networkTag :: WalletState -> NetworkTag
networkTag = Address.getNetworkTag . addresses
