module Cardano.Wallet.Deposit.Pure.State.Signing
    ( getBIP32PathsForOwnedInputs
    , signTx
    ) where

import Prelude

import Cardano.Wallet.Address.BIP32
    ( BIP32Path (..)
    )
import Cardano.Wallet.Deposit.Pure.State.Submissions
    ( availableUTxO
    )
import Cardano.Wallet.Deposit.Pure.State.Type
    ( WalletState (..)
    )
import Data.Maybe
    ( mapMaybe
    )
import Data.Set
    ( Set
    )

import qualified Cardano.Wallet.Deposit.Pure.Address as Address
import qualified Cardano.Wallet.Deposit.Pure.UTxO as UTxO
import qualified Cardano.Wallet.Deposit.Read as Read
import qualified Cardano.Wallet.Deposit.Write as Write

getBIP32PathsForOwnedInputs :: Write.Tx -> WalletState -> [BIP32Path]
getBIP32PathsForOwnedInputs tx w =
    getBIP32Paths w $ resolveInputAddresses inputs
  where
    inputs = Read.getInputs tx <> Read.getCollateralInputs tx

    resolveInputAddresses :: Set Read.TxIn -> [Read.Address]
    resolveInputAddresses ins =
        map (Read.address . snd)
            . UTxO.toList
            $ UTxO.restrictedBy (availableUTxO w) ins

getBIP32Paths :: WalletState -> [Read.Address] -> [BIP32Path]
getBIP32Paths w =
    mapMaybe $ Address.getBIP32Path (addresses w)

signTx :: Write.Tx -> WalletState -> Maybe Write.Tx
signTx _tx _w = undefined
