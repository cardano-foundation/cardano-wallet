module Cardano.Wallet.Primitive.Types.Tx.TxExtended
    ( TxExtended (..)
    )
where

import Prelude

import Cardano.Wallet.Primitive.Types.TokenMapWithScripts
    ( TokenMapWithScripts
    )
import Cardano.Wallet.Primitive.Types.ValidityIntervalExplicit
    ( ValidityIntervalExplicit
    )
import Cardano.Wallet.Primitive.Types.WitnessCount
    ( WitnessCount
    , WitnessCountCtx
    )

import qualified Cardano.Wallet.Primitive.Types.Certificates as W
import qualified Cardano.Wallet.Primitive.Types.Tx as W

data TxExtended = TxExtended
    { walletTx :: W.Tx
    , certificates :: [W.Certificate]
    , toMint :: TokenMapWithScripts
    , toBurn :: TokenMapWithScripts
    , validity :: Maybe ValidityIntervalExplicit
    , witnessCount :: WitnessCountCtx -> WitnessCount
    }
