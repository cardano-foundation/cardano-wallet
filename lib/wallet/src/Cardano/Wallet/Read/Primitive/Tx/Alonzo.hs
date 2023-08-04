{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2020-2022 IOHK
-- License: Apache-2.0
module Cardano.Wallet.Read.Primitive.Tx.Alonzo
  ( fromAlonzoTx
  )
where

import Cardano.Api
  ( AlonzoEra
  )
import Cardano.Api.Shelley qualified as Cardano
import Cardano.Ledger.Alonzo.Scripts qualified as Alonzo
import Cardano.Ledger.Alonzo.Tx qualified as Alonzo
import Cardano.Ledger.Api
  ( addrTxWitsL
  , auxDataTxL
  , bodyTxL
  , bootAddrTxWitsL
  , certsTxBodyL
  , collateralInputsTxBodyL
  , feeTxBodyL
  , inputsTxBodyL
  , isValidTxL
  , mintTxBodyL
  , outputsTxBodyL
  , scriptTxWitsL
  , vldtTxBodyL
  , witsTxL
  )
import Cardano.Ledger.BaseTypes qualified as SL
import Cardano.Ledger.Core qualified as Core
import Cardano.Ledger.Language qualified as Language
import Cardano.Wallet.Primitive.Types qualified as W
import Cardano.Wallet.Primitive.Types.Hash qualified as W
import Cardano.Wallet.Primitive.Types.Tx qualified as W
import Cardano.Wallet.Read.Eras
  ( alonzo
  , inject
  )
import Cardano.Wallet.Read.Primitive.Tx.Features.Certificates
  ( anyEraCerts
  )
import Cardano.Wallet.Read.Primitive.Tx.Features.Inputs
  ( fromShelleyTxIn
  )
import Cardano.Wallet.Read.Primitive.Tx.Features.Metadata
  ( fromAlonzoMetadata
  )
import Cardano.Wallet.Read.Primitive.Tx.Features.Mint
  ( alonzoMint
  , fromLedgerScriptHash
  )
import Cardano.Wallet.Read.Primitive.Tx.Features.Outputs
  ( fromAlonzoTxOut
  )
import Cardano.Wallet.Read.Primitive.Tx.Features.Validity
  ( afterShelleyValidityInterval
  )
import Cardano.Wallet.Read.Primitive.Tx.Features.Withdrawals
  ( fromLedgerWithdrawals
  )
import Cardano.Wallet.Read.Tx
  ( Tx (..)
  )
import Cardano.Wallet.Read.Tx.CBOR
  ( renderTxToCBOR
  )
import Cardano.Wallet.Read.Tx.Hash
  ( shelleyTxHash
  )
import Cardano.Wallet.Read.Tx.Withdrawals
  ( shelleyWithdrawals
  )
import Cardano.Wallet.Shelley.Compatibility.Ledger
  ( toWalletScript
  )
import Cardano.Wallet.Shelley.Compatibility.Ledger qualified as Ledger
import Cardano.Wallet.Transaction
  ( AnyExplicitScript (..)
  , PlutusScriptInfo (..)
  , PlutusVersion (..)
  , ScriptReference (..)
  , TokenMapWithScripts (..)
  , ValidityIntervalExplicit (..)
  , WitnessCount (..)
  , WitnessCountCtx
  , toKeyRole
  )
import Control.Lens
  ( folded
  , (^.)
  , (^..)
  )
import Data.Set qualified as Set
import Prelude

fromAlonzoTx
  :: Alonzo.AlonzoTx (Cardano.ShelleyLedgerEra AlonzoEra)
  -> WitnessCountCtx
  -> ( W.Tx
     , [W.Certificate]
     , TokenMapWithScripts
     , TokenMapWithScripts
     , Maybe ValidityIntervalExplicit
     , WitnessCount
     )
fromAlonzoTx tx witCtx =
  ( W.Tx
      { txId =
          W.Hash $ shelleyTxHash tx
      , txCBOR =
          Just $ renderTxToCBOR $ inject alonzo $ Tx tx
      , fee =
          Just $ Ledger.toWalletCoin $ tx ^. bodyTxL . feeTxBodyL
      , resolvedInputs =
          (,Nothing) . fromShelleyTxIn <$> tx ^.. bodyTxL . inputsTxBodyL . folded
      , resolvedCollateralInputs =
          (,Nothing) . fromShelleyTxIn
            <$> tx ^.. bodyTxL . collateralInputsTxBodyL . folded
      , outputs =
          fromAlonzoTxOut <$> tx ^.. bodyTxL . outputsTxBodyL . folded
      , collateralOutput =
          Nothing -- Collateral outputs are not supported in Alonzo.
      , withdrawals =
          fromLedgerWithdrawals . shelleyWithdrawals $ tx
      , metadata =
          fromAlonzoMetadata <$> SL.strictMaybeToMaybe (tx ^. auxDataTxL)
      , scriptValidity =
          Just $ case tx ^. isValidTxL of
            Alonzo.IsValid True -> W.TxScriptValid
            Alonzo.IsValid False -> W.TxScriptInvalid
      }
  , anyEraCerts $ tx ^. bodyTxL . certsTxBodyL
  , assetsToMint
  , assetsToBurn
  , Just $ afterShelleyValidityInterval $ tx ^. bodyTxL . vldtTxBodyL
  , WitnessCount
      (fromIntegral $ Set.size $ tx ^. witsTxL . addrTxWitsL)
      (fromAlonzoScriptMap <$> tx ^.. witsTxL . scriptTxWitsL . folded)
      (fromIntegral $ Set.size $ tx ^. witsTxL . bootAddrTxWitsL)
  )
  where
    (assetsToMint, assetsToBurn) =
      alonzoMint (tx ^. bodyTxL . mintTxBodyL) (tx ^. witsTxL)

    fromAlonzoScriptMap = \case
      Alonzo.TimelockScript script ->
        NativeExplicitScript
          (toWalletScript (toKeyRole witCtx) script)
          ViaSpending
      script@(Alonzo.PlutusScript ver _) ->
        PlutusExplicitScript
          (PlutusScriptInfo (toPlutusVer ver) (hashAlonzoScript script))
          ViaSpending
        where
          toPlutusVer Language.PlutusV1 = PlutusVersionV1
          toPlutusVer Language.PlutusV2 = PlutusVersionV2
          toPlutusVer Language.PlutusV3 = PlutusVersionV3

          hashAlonzoScript =
            fromLedgerScriptHash
              . Core.hashScript @(Cardano.ShelleyLedgerEra AlonzoEra)
