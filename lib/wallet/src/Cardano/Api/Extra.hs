{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}

-- |
-- Copyright: © 2022 IOHK
-- License: Apache-2.0
--
-- Module containing extra 'Cardano.Api' functionality needed by the wallet.
module Cardano.Api.Extra
    ( withShelleyBasedTx
    , inAnyCardanoEra
    , asAnyShelleyBasedEra
    , fromShelleyBasedScript

    , paymentPartialTx
    , emptyTx
    , addReferenceInputs
    , addInputs
    ) where

import Prelude

import Cardano.Api
    ( BabbageEra
    , CardanoEra (..)
    , CtxUTxO
    , InAnyCardanoEra (..)
    , InAnyShelleyBasedEra (..)
    , IsCardanoEra (cardanoEra)
    , IsShelleyBasedEra
    , PlutusScriptVersion (..)
    , Script (..)
    , ScriptInEra (..)
    , ScriptLanguageInEra (..)
    , ShelleyBasedEra (..)
    , SimpleScriptVersion (..)
    , TimeLocksSupported (TimeLocksInSimpleScriptV2)
    , Tx (..)
    , TxBodyScriptData (TxBodyNoScriptData)
    , TxIn
    , TxOut
    , TxScriptValidity (TxScriptValidityNone)
    )
import Cardano.Api.Shelley
    ( PlutusScript (PlutusScriptSerialised)
    , ShelleyLedgerEra
    , TxBody (..)
    , fromAllegraTimelock
    , fromShelleyMultiSig
    , toShelleyTxIn
    , toShelleyTxOut
    )
import Cardano.Ledger.Shelley.API
    ( StrictMaybe (..), Wdrl (Wdrl) )
import Cardano.Ledger.ShelleyMA.TxBody
    ( ValidityInterval (ValidityInterval) )
import Cardano.Wallet.Orphans
    ()
import Ouroboros.Consensus.Shelley.Eras
    ( StandardBabbage )

import qualified Cardano.Ledger.Alonzo.Language as Alonzo
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import qualified Cardano.Ledger.Babbage.Tx as Babbage
import qualified Cardano.Ledger.Core as Ledger
import qualified Cardano.Ledger.Serialization as Ledger
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set

-- | Apply an era-parameterized function to an existentially-wrapped
-- tx.
withShelleyBasedTx
    :: InAnyShelleyBasedEra Tx
    -> (forall era. IsShelleyBasedEra era => Tx era -> a)
    -> a
withShelleyBasedTx (InAnyShelleyBasedEra _era tx) f
    = f tx

-- | Helper function for more easily creating an existential
-- @InAnyCardanoEra Tx@.
inAnyCardanoEra :: IsCardanoEra era => Tx era -> InAnyCardanoEra Tx
inAnyCardanoEra = InAnyCardanoEra cardanoEra

-- | "Downcast" an existentially wrapped tx.
asAnyShelleyBasedEra
    :: InAnyCardanoEra a
    -> Maybe (InAnyShelleyBasedEra a)
asAnyShelleyBasedEra = \case
    InAnyCardanoEra ByronEra _ ->
        Nothing
    InAnyCardanoEra ShelleyEra a ->
        Just $ InAnyShelleyBasedEra ShelleyBasedEraShelley a
    InAnyCardanoEra AllegraEra a ->
        Just $ InAnyShelleyBasedEra ShelleyBasedEraAllegra a
    InAnyCardanoEra MaryEra a ->
        Just $ InAnyShelleyBasedEra ShelleyBasedEraMary a
    InAnyCardanoEra AlonzoEra a ->
        Just $ InAnyShelleyBasedEra ShelleyBasedEraAlonzo a
    InAnyCardanoEra BabbageEra a ->
        Just $ InAnyShelleyBasedEra ShelleyBasedEraBabbage a

-- Copied from cardano-api because it is not exported.
fromShelleyBasedScript  :: ShelleyBasedEra era
                        -> Ledger.Script (ShelleyLedgerEra era)
                        -> ScriptInEra era
fromShelleyBasedScript era script =
  case era of
    ShelleyBasedEraShelley ->
      ScriptInEra SimpleScriptV1InShelley $
      SimpleScript SimpleScriptV1 $
      fromShelleyMultiSig script
    ShelleyBasedEraAllegra ->
      ScriptInEra SimpleScriptV2InAllegra $
      SimpleScript SimpleScriptV2 $
      fromAllegraTimelock TimeLocksInSimpleScriptV2 script
    ShelleyBasedEraMary ->
      ScriptInEra SimpleScriptV2InMary $
      SimpleScript SimpleScriptV2 $
      fromAllegraTimelock TimeLocksInSimpleScriptV2 script
    ShelleyBasedEraAlonzo ->
      case script of
        Alonzo.TimelockScript s ->
          ScriptInEra SimpleScriptV2InAlonzo $
          SimpleScript SimpleScriptV2 $
          fromAllegraTimelock TimeLocksInSimpleScriptV2 s
        Alonzo.PlutusScript Alonzo.PlutusV1 s ->
          ScriptInEra PlutusScriptV1InAlonzo $
          PlutusScript PlutusScriptV1 $
          PlutusScriptSerialised s
        Alonzo.PlutusScript Alonzo.PlutusV2 _ ->
          error "fromShelleyBasedScript: PlutusV2 not supported in Alonzo era"
    ShelleyBasedEraBabbage ->
      case script of
        Alonzo.TimelockScript s ->
          ScriptInEra SimpleScriptV2InBabbage $
          SimpleScript SimpleScriptV2 $
          fromAllegraTimelock TimeLocksInSimpleScriptV2 s
        Alonzo.PlutusScript Alonzo.PlutusV1 s ->
          ScriptInEra PlutusScriptV1InBabbage $
          PlutusScript PlutusScriptV1 $
          PlutusScriptSerialised s
        Alonzo.PlutusScript Alonzo.PlutusV2 s ->
          ScriptInEra PlutusScriptV2InBabbage $
          PlutusScript PlutusScriptV2 $
          PlutusScriptSerialised s

-- NOTE: Could be moved to some build/balanceTx module
emptyTx :: Tx BabbageEra
emptyTx = Tx body []
  where
    body = ShelleyTxBody
        ShelleyBasedEraBabbage
        alonzoBody
        []
        TxBodyNoScriptData
        Nothing
        TxScriptValidityNone

    alonzoBody = Babbage.TxBody
        { Babbage.inputs = mempty
        , Babbage.collateral = mempty
        , Babbage.outputs = mempty
        , Babbage.txcerts = mempty
        , Babbage.txwdrls = Wdrl mempty
        , Babbage.txfee = mempty
        , Babbage.txvldt = ValidityInterval SNothing SNothing
        , Babbage.txUpdates = SNothing
        , Babbage.reqSignerHashes = mempty
        , Babbage.mint = mempty
        , Babbage.scriptIntegrityHash = SNothing
        , Babbage.adHash = SNothing
        , Babbage.txnetworkid = SNothing
        , Babbage.referenceInputs = mempty
        , Babbage.collateralReturn = SNothing
        , Babbage.totalCollateral = SNothing
        }

addReferenceInputs :: [TxIn] -> Tx BabbageEra -> Tx BabbageEra
addReferenceInputs ins = modifyBabbageTxBody $ \body ->
    body
        { Babbage.referenceInputs =
                Set.map toShelleyTxIn (Set.fromList ins)
                <> (Babbage.referenceInputs body)
        }

addInputs
    :: [TxIn]
    -> Tx BabbageEra
    -> Tx BabbageEra
addInputs ins = modifyBabbageTxBody $ \body ->
    body
        { Babbage.inputs = Babbage.inputs body
            <> (Set.map toShelleyTxIn (Set.fromList ins))
        }

-- Ideally merge with 'updateSealedTx'
modifyBabbageTxBody
    :: (Babbage.TxBody StandardBabbage -> Babbage.TxBody StandardBabbage)
    -> Tx BabbageEra -> Tx BabbageEra
modifyBabbageTxBody
    f
    (Tx
        (ShelleyTxBody
            era
            body
            scripts
            scriptData
            auxData
            scriptValidity)
        keyWits)
    = Tx
        (ShelleyTxBody
            era
            (f body)
            scripts
            scriptData
            auxData
            scriptValidity)
        keyWits

-- NOTE: Could be moved to some build/balanceTx module
paymentPartialTx :: [TxOut CtxUTxO BabbageEra] -> Tx BabbageEra
paymentPartialTx txouts = Tx body []
  where
    body = ShelleyTxBody
        ShelleyBasedEraBabbage
        alonzoBody
        []
        TxBodyNoScriptData
        Nothing
        TxScriptValidityNone

    -- NOTE: It would be nicer not to have to deal with the ledger internals
    -- here. Perhaps we could go through cardano-api instead, if we create a
    -- @TxBodyContent -> Tx@, where - unlike @makeTransactionBody@ - there is
    -- no validation.
    alonzoBody = Babbage.TxBody
        { Babbage.inputs = mempty
        , Babbage.collateral = mempty
        , Babbage.outputs = StrictSeq.fromList $
            map (Ledger.mkSized . toShelleyTxOut ShelleyBasedEraBabbage) txouts
        , Babbage.txcerts = mempty
        , Babbage.txwdrls = Wdrl mempty
        , Babbage.txfee = mempty
        , Babbage.txvldt = ValidityInterval SNothing SNothing
        , Babbage.txUpdates = SNothing
        , Babbage.reqSignerHashes = mempty
        , Babbage.mint = mempty
        , Babbage.scriptIntegrityHash = SNothing
        , Babbage.adHash = SNothing
        , Babbage.txnetworkid = SNothing
        , Babbage.referenceInputs = mempty
        , Babbage.collateralReturn = SNothing
        , Babbage.totalCollateral = SNothing
        }
