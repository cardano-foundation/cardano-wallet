module Cardano.Wallet.Deposit.Pure.State.Creation
    ( WalletPublicIdentity (..)
    , fromXPubAndGenesis
    ) where

import Prelude hiding
    ( lookup
    )

import Cardano.Crypto.Wallet
    ( XPub
    )
import Cardano.Wallet.Deposit.Pure.State.Type
    ( WalletState (..)
    )
import Data.Word.Odd
    ( Word31
    )

import qualified Cardano.Wallet.Deposit.Pure.Address as Address
import qualified Cardano.Wallet.Deposit.Pure.Submissions as Sbm
import qualified Cardano.Wallet.Deposit.Pure.UTxO.UTxOHistory as UTxOHistory
import qualified Cardano.Wallet.Deposit.Read as Read

data WalletPublicIdentity = WalletPublicIdentity
    { pubXpub :: XPub
    , pubNextUser :: Word31
    }
    deriving (Show)

fromXPubAndGenesis
    :: XPub -> Word31 -> Read.GenesisData -> WalletState
fromXPubAndGenesis xpub knownCustomerCount genesisData =
    WalletState
        { walletTip = Read.GenesisPoint
        , addresses =
            Address.fromXPubAndCount network xpub knownCustomerCount
        , utxoHistory = UTxOHistory.fromOrigin initialUTxO
        , txHistory = mempty
        , submissions = Sbm.empty
        , rootXSignKey = Nothing
        }
  where
    network = Read.getNetworkId genesisData
    initialUTxO = mempty
