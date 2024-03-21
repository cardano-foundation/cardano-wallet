{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.WitnessCount where

import Prelude

import Cardano.Ledger.Api
    ( ScriptHash
    , StandardCrypto
    , addrTxWitsL
    , bootAddrTxWitsL
    , hashScript
    , scriptTxWitsL
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Outputs
    ( fromBabbageTxOut
    , fromConwayTxOut
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Scripts
    ( alonzoAnyExplicitScript
    , babbageAnyExplicitScript
    , conwayAnyExplicitScript
    )
import Cardano.Wallet.Primitive.Types.TokenPolicyId
    ( TokenPolicyId
    )
import Cardano.Wallet.Primitive.Types.Tx.TxIn
    ( TxIn (..)
    )
import Cardano.Wallet.Read
    ( Allegra
    , Alonzo
    , Babbage
    , Conway
    , Era (..)
    , IsEra (..)
    , Mary
    , Shelley
    , Tx
    )
import Cardano.Wallet.Read.Tx.Outputs
    ( Outputs (..)
    , getEraOutputs
    )
import Cardano.Wallet.Read.Tx.Witnesses
    ( Witnesses (..)
    , getEraWitnesses
    )

import Cardano.Address.Script
    ( KeyRole (..)
    )
import Cardano.Ledger.Babbage
    ( AlonzoScript
    , BabbageTxOut
    )
import Cardano.Wallet.Primitive.Ledger.Convert
    ( toWalletScript
    , toWalletScriptFromShelley
    )
import Cardano.Wallet.Primitive.Types.AnyExplicitScripts
    ( AnyExplicitScript (..)
    )
import Cardano.Wallet.Primitive.Types.TokenMapWithScripts
    ( ReferenceInput (ReferenceInput)
    , ScriptReference (ViaReferenceInput, ViaSpending)
    )
import Cardano.Wallet.Primitive.Types.WitnessCount
    ( WitnessCount (WitnessCount)
    , WitnessCountCtx
    , emptyWitnessCount
    , toKeyRole
    )
import Cardano.Wallet.Read.Tx.Hash
    ( getEraTxHash
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
    , Word8
    )

import qualified Cardano.Ledger.Api as L
import qualified Cardano.Wallet.Primitive.Types.Hash as W
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

getWitnessCount
    :: forall era
     . IsEra era
    => Tx era
    -> WitnessCountCtx
    -> WitnessCount
getWitnessCount = case theEra @era of
    Byron -> \_ _ -> emptyWitnessCount
    Shelley -> shelleyWitnessCount . getEraWitnesses
    Allegra -> allegraWitnessCount . getEraWitnesses
    Mary -> maryWitnessCount . getEraWitnesses
    Alonzo -> alonzoWitnessCount . getEraWitnesses
    Babbage -> babbageWitnessCount
    Conway -> conwayWitnessCount

addrWits :: L.EraTxWits era => L.TxWits era -> Word8
addrWits w = fromIntegral $ Set.size $ w ^. addrTxWitsL

bootAddrWits :: L.EraTxWits era => L.TxWits era -> Word8
bootAddrWits w = fromIntegral $ Set.size $ w ^. bootAddrTxWitsL

mkAnyEraWitnessCount
    :: (L.EraTxWits era)
    => L.TxWits era
    -> [AnyExplicitScript]
    -> WitnessCount
mkAnyEraWitnessCount wits anyScripts =
    WitnessCount (addrWits wits) anyScripts (bootAddrWits wits)

shelleyWitnessCount :: Witnesses Shelley -> WitnessCountCtx -> WitnessCount
shelleyWitnessCount (Witnesses w) _ =
    mkAnyEraWitnessCount w $ do
        s <- w ^.. scriptTxWitsL . folded
        let script = toWalletScriptFromShelley Payment s
        pure $ NativeExplicitScript script ViaSpending

allegraWitnessCount :: Witnesses Allegra -> WitnessCountCtx -> WitnessCount
allegraWitnessCount (Witnesses w) _ =
    mkAnyEraWitnessCount w $ do
        s <- w ^.. scriptTxWitsL . folded
        let script = toWalletScript (\_vkey -> Payment) s
        pure $ NativeExplicitScript script ViaSpending

maryWitnessCount :: Witnesses Mary -> WitnessCountCtx -> WitnessCount
maryWitnessCount (Witnesses w) witCtx =
    mkAnyEraWitnessCount w $ do
        s <- w ^.. scriptTxWitsL . folded
        let script = toWalletScript (toKeyRole witCtx) s
        pure $ NativeExplicitScript script ViaSpending

alonzoWitnessCount :: Witnesses Alonzo -> WitnessCountCtx -> WitnessCount
alonzoWitnessCount (Witnesses w) witCtx =
    mkAnyEraWitnessCount w $ do
        s <- w ^.. scriptTxWitsL . folded
        pure $ alonzoAnyExplicitScript witCtx s

babbageWitnessCount :: Tx Babbage -> WitnessCountCtx -> WitnessCount
babbageWitnessCount tx witCtx =
    mkAnyEraWitnessCount w
        $ Map.elems
        $ Map.union anyScriptsFromWits anyScriptsFromTxOuts
  where
    Outputs o = getEraOutputs tx
    txId' = W.Hash $ getEraTxHash tx
    Witnesses w = getEraWitnesses tx

    anyScriptsFromWits :: Map TokenPolicyId AnyExplicitScript
    anyScriptsFromWits =
        Map.fromList
            [ babbageAnyExplicitScript witCtx (ViaSpending, scriptH, script)
            | (scriptH, script) <- Map.toList (w ^. scriptTxWitsL)
            ]

    anyScriptsFromTxOuts :: Map TokenPolicyId AnyExplicitScript
    anyScriptsFromTxOuts =
        Map.fromList
            [ babbageAnyExplicitScript witCtx ledgerScript
            | Just ledgerScript <-
                zipWith scriptWithHashIx [0 ..] (o ^.. folded)
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

conwayWitnessCount :: Tx Conway -> WitnessCountCtx -> WitnessCount
conwayWitnessCount tx witCtx =
    mkAnyEraWitnessCount w
        $ Map.elems
        $ Map.union anyScriptsFromWits anyScriptsFromTxOuts
  where
    Outputs o = getEraOutputs tx
    txId' = W.Hash $ getEraTxHash tx
    Witnesses w = getEraWitnesses tx

    anyScriptsFromWits :: Map TokenPolicyId AnyExplicitScript
    anyScriptsFromWits =
        Map.fromList
            [ conwayAnyExplicitScript witCtx (ViaSpending, scriptH, script)
            | (scriptH, script) <- Map.toList (w ^. scriptTxWitsL)
            ]

    anyScriptsFromTxOuts :: Map TokenPolicyId AnyExplicitScript
    anyScriptsFromTxOuts =
        Map.fromList
            [ conwayAnyExplicitScript witCtx ledgerScript
            | Just ledgerScript <-
                zipWith
                    scriptWithHashIx
                    [0 ..]
                    (o ^.. folded)
            ]
      where
        scriptWithHashIx
            :: Word32
            -> BabbageTxOut Conway
            -> Maybe
                ( ScriptReference
                , ScriptHash StandardCrypto
                , AlonzoScript Conway
                )
        scriptWithHashIx ix txout =
            snd (fromConwayTxOut txout) <&> \script ->
                ( ViaReferenceInput (ReferenceInput (TxIn txId' ix))
                , hashScript @Conway script
                , script
                )
