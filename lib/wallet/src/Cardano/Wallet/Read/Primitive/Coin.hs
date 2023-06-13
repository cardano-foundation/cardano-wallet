module Cardano.Wallet.Read.Primitive.Coin
    ( unsafeFromLedger
    ) where

import GHC.Stack
    ( HasCallStack )

import qualified Cardano.Ledger.Coin as Ledger
import qualified Cardano.Wallet.Primitive.Types.Coin as Coin
import qualified Cardano.Wallet.Primitive.Types.Coin as Wallet

unsafeFromLedger :: HasCallStack => Ledger.Coin -> Wallet.Coin
unsafeFromLedger (Ledger.Coin c) = Coin.unsafeFromIntegral c
