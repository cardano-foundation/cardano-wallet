{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2020-2022 IOHK
-- License: Apache-2.0
module Cardano.Wallet.Primitive.Ledger.Read.Tx.Alonzo
    ( fromAlonzoTx
    )
where

import Prelude

import Cardano.Ledger.Api
    ( Alonzo
    , addrTxWitsL
    , bodyTxL
    , bootAddrTxWitsL
    , mintTxBodyL
    , scriptTxWitsL
    , vldtTxBodyL
    , witsTxL
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx
    ( primitiveTx
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Certificates
    ( getCertificates
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Mint
    ( alonzoMint
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Scripts
    ( alonzoAnyExplicitScript
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Validity
    ( afterShelleyValidityInterval
    )
import Cardano.Wallet.Primitive.Types.Tx.TxExtended
    ( TxExtended (..)
    )
import Cardano.Wallet.Primitive.Types.WitnessCount
    ( WitnessCount (WitnessCount)
    )
import Cardano.Wallet.Read.Tx.Certificates
    ( getEraCertificates
    )
import Control.Lens
    ( folded
    , (^.)
    , (^..)
    )

import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import qualified Cardano.Wallet.Read as Read
import qualified Data.Set as Set

fromAlonzoTx
    :: Alonzo.AlonzoTx Alonzo
    -> TxExtended
fromAlonzoTx tx = TxExtended{..}
  where
    walletTx = primitiveTx @Alonzo $ Read.Tx tx
    certificates = getCertificates @Alonzo $ getEraCertificates $ Read.Tx tx
    toMint = assetsToMint
    toBurn = assetsToBurn
    validity = Just $ afterShelleyValidityInterval $ tx ^. bodyTxL . vldtTxBodyL
    witnessCount witCtx =
        WitnessCount
            (fromIntegral $ Set.size $ tx ^. witsTxL . addrTxWitsL)
            (alonzoAnyExplicitScript witCtx <$> tx ^.. witsTxL . scriptTxWitsL . folded)
            (fromIntegral $ Set.size $ tx ^. witsTxL . bootAddrTxWitsL)

    (assetsToMint, assetsToBurn) =
        alonzoMint (tx ^. bodyTxL . mintTxBodyL) (tx ^. witsTxL)
