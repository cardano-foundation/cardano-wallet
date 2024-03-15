{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2020-2022 IOHK
-- License: Apache-2.0
module Cardano.Wallet.Primitive.Ledger.Read.Tx.Babbage
    ( fromBabbageTx
    )
where

import Prelude

import Cardano.Ledger.Api
    ( Babbage
    , ScriptHash
    , StandardCrypto
    , addrTxWitsL
    , bodyTxL
    , bootAddrTxWitsL
    , hashScript
    , mintTxBodyL
    , outputsTxBodyL
    , referenceInputsTxBodyL
    , scriptTxWitsL
    , vldtTxBodyL
    , witsTxL
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Certificates
    ( anyEraCerts
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Mint
    ( babbageMint
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Outputs
    ( fromBabbageTxOut
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Scripts
    ( babbageAnyExplicitScript
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Validity
    ( afterShelleyValidityInterval
    )
import Cardano.Wallet.Primitive.Types.TokenPolicyId
    ( TokenPolicyId
    )
import Cardano.Wallet.Primitive.Types.Tx
    ( Tx (txId)
    )
import Cardano.Wallet.Primitive.Types.Tx.TxIn
    ( TxIn (..)
    )

import Cardano.Ledger.Babbage
    ( AlonzoScript
    , BabbageTxOut
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx
    ( primitiveTx
    )
import Cardano.Wallet.Primitive.Types.AnyExplicitScripts
    ( AnyExplicitScript (..)
    )
import Cardano.Wallet.Primitive.Types.TokenMapWithScripts
    ( ReferenceInput (ReferenceInput)
    , ScriptReference (ViaReferenceInput, ViaSpending)
    )
import Cardano.Wallet.Primitive.Types.Tx.TxExtended
    ( TxExtended (..)
    )
import Cardano.Wallet.Primitive.Types.WitnessCount
    ( WitnessCount (WitnessCount)
    )
import Control.Lens
    ( folded
    , (<&>)
    , (^.)
    , (^..)
    )
import Data.Map
    ( Map
    )
import Data.Word
    ( Word32
    )

import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import qualified Cardano.Wallet.Read as Read
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

fromBabbageTx
    :: Alonzo.AlonzoTx Babbage
    -> TxExtended
fromBabbageTx tx = TxExtended{..}
  where
    walletTx = primitiveTx @Babbage $ Read.Tx tx
    certificates = anyEraCerts @Babbage $ Read.Tx tx
    toMint = assetsToMint
    toBurn = assetsToBurn
    validity = Just $ afterShelleyValidityInterval $ tx ^. bodyTxL . vldtTxBodyL
    witnessCount witCtx =
        WitnessCount
            (fromIntegral $ Set.size $ tx ^. witsTxL . addrTxWitsL)
            (Map.elems $ Map.union anyScriptsFromWits anyScriptsFromTxOuts)
            (fromIntegral $ Set.size $ tx ^. witsTxL . bootAddrTxWitsL)
      where
        txId' = txId walletTx

        anyScriptsFromTxOuts :: Map TokenPolicyId AnyExplicitScript
        anyScriptsFromTxOuts =
            Map.fromList
                [ babbageAnyExplicitScript witCtx ledgerScript
                | Just ledgerScript <-
                    L.zipWith
                        scriptWithHashIx
                        [0 ..]
                        (tx ^.. bodyTxL . outputsTxBodyL . folded)
                ]
        scriptWithHashIx
            :: Word32
            -> BabbageTxOut Babbage
            -> Maybe
                ( ScriptReference
                , ScriptHash StandardCrypto
                , AlonzoScript Babbage
                )
        scriptWithHashIx ix txout =
            snd (fromBabbageTxOut txout) <&> \script ->
                ( ViaReferenceInput (ReferenceInput (TxIn txId' ix))
                , hashScript @Babbage script
                , script
                )

        anyScriptsFromWits :: Map TokenPolicyId AnyExplicitScript
        anyScriptsFromWits =
            Map.fromList
                [ babbageAnyExplicitScript witCtx (ViaSpending, scriptH, script)
                | (scriptH, script) <- Map.toList (tx ^. witsTxL . scriptTxWitsL)
                ]

    (assetsToMint, assetsToBurn) =
        babbageMint
            (tx ^. bodyTxL . referenceInputsTxBodyL)
            (tx ^. bodyTxL . mintTxBodyL)
            (tx ^. witsTxL)
