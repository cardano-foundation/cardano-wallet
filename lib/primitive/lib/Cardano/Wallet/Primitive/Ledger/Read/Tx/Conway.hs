{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2020-2022 IOHK
-- License: Apache-2.0
module Cardano.Wallet.Primitive.Ledger.Read.Tx.Conway
    ( fromConwayTx
    )
where

import Prelude

import Cardano.Ledger.Alonzo.Scripts
    ( AlonzoScript
    )
import Cardano.Ledger.Api
    ( Conway
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
import Cardano.Ledger.Babbage
    ( BabbageTxOut
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx
    ( primitiveTx
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Certificates
    ( getCertificates
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Mint
    ( conwayMint
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Outputs
    ( fromConwayTxOut
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Scripts
    ( conwayAnyExplicitScript
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Validity
    ( afterShelleyValidityInterval
    )
import Cardano.Wallet.Primitive.Types.AnyExplicitScripts
    ( AnyExplicitScript (..)
    )
import Cardano.Wallet.Primitive.Types.TokenMapWithScripts
    ( ReferenceInput (ReferenceInput)
    , ScriptReference (..)
    )
import Cardano.Wallet.Primitive.Types.TokenPolicyId
    ( TokenPolicyId (..)
    )
import Cardano.Wallet.Primitive.Types.Tx
    ( Tx (txId)
    )
import Cardano.Wallet.Primitive.Types.Tx.TxExtended
    ( TxExtended (..)
    )
import Cardano.Wallet.Primitive.Types.Tx.TxIn
    ( TxIn (..)
    )
import Cardano.Wallet.Primitive.Types.WitnessCount
    ( WitnessCount (WitnessCount)
    )
import Cardano.Wallet.Read.Tx.Certificates
    ( getEraCertificates
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
import qualified Cardano.Ledger.Api as Ledger
import qualified Cardano.Wallet.Read as Read
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

fromConwayTx
    :: Alonzo.AlonzoTx Conway
    -> TxExtended
fromConwayTx tx = TxExtended{..}
  where
    walletTx = primitiveTx @Conway $ Read.Tx tx
    certificates = getCertificates @Conway $ getEraCertificates $ Read.Tx tx
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
                [ conwayAnyExplicitScript witCtx ledgerScript
                | Just ledgerScript <-
                    L.zipWith
                        scriptWithHashIx
                        [0 ..]
                        (tx ^.. bodyTxL . outputsTxBodyL . folded)
                ]
          where
            scriptWithHashIx
                :: Word32
                -> BabbageTxOut Conway
                -> Maybe
                    ( ScriptReference
                    , Ledger.ScriptHash StandardCrypto
                    , AlonzoScript Conway
                    )
            scriptWithHashIx ix txout =
                snd (fromConwayTxOut txout) <&> \script ->
                    ( ViaReferenceInput (ReferenceInput (TxIn txId' ix))
                    , hashScript @Conway script
                    , script
                    )

        anyScriptsFromWits :: Map TokenPolicyId AnyExplicitScript
        anyScriptsFromWits =
            Map.fromList
                [ conwayAnyExplicitScript witCtx (ViaSpending, scriptH, script)
                | (scriptH, script) <- Map.toList (tx ^. witsTxL . scriptTxWitsL)
                ]

    (assetsToMint, assetsToBurn) =
        conwayMint
            (tx ^. bodyTxL . referenceInputsTxBodyL)
            (tx ^. bodyTxL . mintTxBodyL)
            (tx ^. witsTxL)
