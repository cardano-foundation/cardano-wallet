{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2020-2022 IOHK 2023- CardanoFoundation
-- License: Apache-2.0
module Cardano.Wallet.Primitive.Ledger.Read.Tx.Allegra
    ( fromAllegraTx
    )
where

import Prelude

import Cardano.Address.Script
    ( KeyRole (..)
    )
import Cardano.Ledger.Api
    ( Allegra
    , addrTxWitsL
    , bodyTxL
    , bootAddrTxWitsL
    , scriptTxWitsL
    , vldtTxBodyL
    , witsTxL
    )
import Cardano.Ledger.Core
    ()
import Cardano.Ledger.Shelley.Tx
    ( ShelleyTx
    )
import Cardano.Wallet.Primitive.Ledger.Convert
    ( toWalletScript
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx
    ( primitiveTx
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Certificates
    ( anyEraCerts
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Validity
    ( afterShelleyValidityInterval
    )
import Cardano.Wallet.Primitive.Types.AnyExplicitScripts
    ( AnyExplicitScript (NativeExplicitScript)
    )
import Cardano.Wallet.Primitive.Types.TokenMapWithScripts
    ( ScriptReference (ViaSpending)
    , emptyTokenMapWithScripts
    )
import Cardano.Wallet.Primitive.Types.Tx.TxExtended
    ( TxExtended (..)
    )
import Cardano.Wallet.Primitive.Types.WitnessCount
    ( WitnessCount (WitnessCount)
    )
import Control.Lens
    ( folded
    , (^.)
    , (^..)
    )

import qualified Cardano.Wallet.Read as Read
import qualified Data.Set as Set

-- NOTE: For resolved inputs we have to pass in a dummy value of 0.

fromAllegraTx
    :: ShelleyTx Allegra
    -> TxExtended
fromAllegraTx tx =
    TxExtended{..}
  where
    walletTx = primitiveTx @Allegra $ Read.Tx tx
    certificates = anyEraCerts @Allegra $ Read.Tx tx
    toMint = emptyTokenMapWithScripts
    toBurn = emptyTokenMapWithScripts
    validity = Just $ afterShelleyValidityInterval $ tx ^. bodyTxL . vldtTxBodyL
    witnessCount _ =
        WitnessCount
            (fromIntegral $ Set.size $ tx ^. witsTxL . addrTxWitsL)
            ( (`NativeExplicitScript` ViaSpending)
                . toWalletScript (\_vkey -> Payment)
                <$> tx ^.. witsTxL . scriptTxWitsL . folded
            )
            (fromIntegral $ Set.size $ tx ^. witsTxL . bootAddrTxWitsL)
