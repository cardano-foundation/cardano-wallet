{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2020-2022 IOHK
-- License: Apache-2.0
module Cardano.Wallet.Primitive.Ledger.Read.Tx.Mary
    ( fromMaryTx
    )
where

import Prelude

import Cardano.Ledger.Api
    ( Mary
    , addrTxWitsL
    , bodyTxL
    , bootAddrTxWitsL
    , mintTxBodyL
    , scriptTxWitsL
    , vldtTxBodyL
    , witsTxL
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
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Mint
    ( maryMint
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Validity
    ( afterShelleyValidityInterval
    )
import Cardano.Wallet.Primitive.Types.AnyExplicitScripts
    ( AnyExplicitScript (NativeExplicitScript)
    )
import Cardano.Wallet.Primitive.Types.TokenMapWithScripts
    ( ScriptReference (ViaSpending)
    )
import Cardano.Wallet.Primitive.Types.Tx.TxExtended
    ( TxExtended (..)
    )
import Cardano.Wallet.Primitive.Types.WitnessCount
    ( WitnessCount (WitnessCount)
    , toKeyRole
    )
import Control.Lens
    ( folded
    , (^.)
    , (^..)
    )

import qualified Cardano.Ledger.Shelley.API as SL
import qualified Cardano.Wallet.Read as Read
import qualified Data.Set as Set

fromMaryTx
    :: SL.ShelleyTx Mary
    -> TxExtended
fromMaryTx tx = TxExtended{..}
  where
    walletTx = primitiveTx @Mary $ Read.Tx tx
    certificates = anyEraCerts @Mary $ Read.Tx tx
    toMint = assetsToMint
    toBurn = assetsToBurn
    validity = Just $ afterShelleyValidityInterval $ tx ^. bodyTxL . vldtTxBodyL
    witnessCount witCtx =
        WitnessCount
            (fromIntegral $ Set.size $ tx ^. witsTxL . addrTxWitsL)
            ( (`NativeExplicitScript` ViaSpending)
                . toWalletScript (toKeyRole witCtx)
                <$> tx ^.. witsTxL . scriptTxWitsL . folded
            )
            (fromIntegral $ Set.size $ tx ^. witsTxL . bootAddrTxWitsL)

    (assetsToMint, assetsToBurn) =
        maryMint (tx ^. bodyTxL . mintTxBodyL) (tx ^. witsTxL)
