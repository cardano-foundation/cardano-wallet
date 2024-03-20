{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2020-2022 IOHK 2023- CardanoFoundation
-- License: Apache-2.0
module Cardano.Wallet.Primitive.Ledger.Read.Tx.Shelley
    ( fromShelleyTx
    )
where

import Prelude

import Cardano.Address.Script
    ( KeyRole (..)
    )
import Cardano.Ledger.Api
    ( Shelley
    , addrTxWitsL
    , bootAddrTxWitsL
    , scriptTxWitsL
    , witsTxL
    )
import Cardano.Ledger.Shelley
    ( ShelleyTx
    )
import Cardano.Wallet.Primitive.Ledger.Convert
    ( toWalletScriptFromShelley
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx
    ( primitiveTx
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Certificates
    ( anyEraCerts
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Validity
    ( getValidity
    )
import Cardano.Wallet.Primitive.Types.AnyExplicitScripts
    ( AnyExplicitScript (NativeExplicitScript)
    )
import Cardano.Wallet.Primitive.Types.TokenMapWithScripts
    ( ScriptReference (..)
    , TokenMapWithScripts (..)
    , emptyTokenMapWithScripts
    )
import Cardano.Wallet.Primitive.Types.ValidityIntervalExplicit
    ( ValidityIntervalExplicit
    )
import Cardano.Wallet.Primitive.Types.WitnessCount
    ( WitnessCount (..)
    )
import Cardano.Wallet.Read.Tx.Validity
    ( getEraValidity
    )
import Control.Lens
    ( folded
    , (^.)
    , (^..)
    )

import qualified Cardano.Wallet.Primitive.Types.Certificates as W
import qualified Cardano.Wallet.Primitive.Types.Tx as W
import qualified Cardano.Wallet.Read as Read
import qualified Data.Set as Set

-- NOTE: For resolved inputs we have to pass in a dummy value of 0.
fromShelleyTx
    :: ShelleyTx Shelley
    -> ( W.Tx
       , [W.Certificate]
       , TokenMapWithScripts
       , TokenMapWithScripts
       , Maybe ValidityIntervalExplicit
       , WitnessCount
       )
fromShelleyTx tx =
    ( primitiveTx @Shelley $ Read.Tx tx
    , anyEraCerts @Shelley $ Read.Tx tx
    , emptyTokenMapWithScripts
    , emptyTokenMapWithScripts
    , getValidity . getEraValidity @Shelley $ Read.Tx tx
    , WitnessCount
        (fromIntegral $ Set.size $ tx ^. witsTxL . addrTxWitsL)
        ( (`NativeExplicitScript` ViaSpending)
            . toWalletScriptFromShelley Payment
            <$> tx ^.. witsTxL . scriptTxWitsL . folded
        )
        (fromIntegral $ Set.size $ tx ^. witsTxL . bootAddrTxWitsL)
    )
