{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{- HLINT ignore "Use <$>" -}

-- |
-- Copyright: © 2023 IOHK
-- License: Apache-2.0
--
-- Module containing 'assignScriptRedeemers'
module Cardano.Wallet.Write.Tx.Redeemers
    ( assignScriptRedeemers
    , ErrAssignRedeemers (..)
    ) where

import Prelude

import Cardano.Ledger.Alonzo.TxInfo
    ( TranslationError )
import Cardano.Ledger.Api
    ( Tx, bodyTxL, rdmrsTxWitsL, scriptIntegrityHashTxBodyL, witsTxL )
import Cardano.Ledger.Shelley.API
    ( StrictMaybe (..) )
import Cardano.Slotting.EpochInfo
    ( EpochInfo, hoistEpochInfo )
import Cardano.Wallet.Primitive.Types.Redeemer
    ( Redeemer, redeemerData )
import Cardano.Wallet.Shelley.Compatibility
    ( toScriptPurpose )
import Cardano.Wallet.Write.Tx
    ( IsRecentEra (recentEra)
    , PParams
    , RecentEraLedgerConstraints
    , ShelleyLedgerEra
    , StandardCrypto
    , fromCardanoTx
    , fromCardanoUTxO
    , shelleyBasedEra
    , txBody
    , withConstraints
    )
import Cardano.Wallet.Write.Tx.TimeTranslation
    ( TimeTranslation, epochInfo, systemStartTime )
import Codec.Serialise
    ( deserialiseOrFail )
import Control.Arrow
    ( left )
import Control.Lens
    ( (.~) )
import Control.Monad
    ( forM )
import Control.Monad.Trans.Class
    ( lift )
import Control.Monad.Trans.State.Strict
    ( StateT (..), execStateT, get, modify', put )
import Data.Bifunctor
    ( bimap )
import Data.Function
    ( (&) )
import Data.Generics.Internal.VL.Lens
    ( view )
import Data.Generics.Labels
    ()
import Data.Map.Strict
    ( Map, (!) )
import GHC.Generics
    ( Generic )

import qualified Cardano.Api as Cardano
import qualified Cardano.Api.Shelley as Cardano
import qualified Cardano.Ledger.Alonzo.PlutusScriptApi as Alonzo
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import qualified Cardano.Ledger.Alonzo.Scripts.Data as Alonzo
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import qualified Cardano.Ledger.Alonzo.TxWits as Alonzo
import qualified Cardano.Ledger.Api as Ledger
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as Map
import qualified Data.Map.Merge.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T

data ErrAssignRedeemers
    = ErrAssignRedeemersScriptFailure Redeemer String
    | ErrAssignRedeemersTargetNotFound Redeemer
    -- ^ The given redeemer target couldn't be located in the transaction.
    | ErrAssignRedeemersInvalidData Redeemer String
    -- ^ Redeemer's data isn't a valid Plutus' data.
    | ErrAssignRedeemersTranslationError (TranslationError StandardCrypto)
    deriving (Generic, Eq, Show)

assignScriptRedeemers
    :: forall era. IsRecentEra era
    => PParams (ShelleyLedgerEra era)
    -> TimeTranslation
    -> Cardano.UTxO era
    -> [Redeemer]
    -> Cardano.Tx era
    -> Either ErrAssignRedeemers (Cardano.Tx era)
assignScriptRedeemers pparams timeTranslation utxo redeemers tx =
    withConstraints (recentEra @era) $ do
        let ledgerTx = fromCardanoTx tx
        ledgerTx' <- flip execStateT ledgerTx $ do
            indexedRedeemers <- StateT assignNullRedeemers
            executionUnits <- get
                >>= lift . evaluateExecutionUnits indexedRedeemers
            modifyM (assignExecutionUnits executionUnits)
            modify' addScriptIntegrityHash
        pure $ Cardano.ShelleyTx shelleyBasedEra ledgerTx'
  where
    epochInformation :: EpochInfo (Either T.Text)
    epochInformation =
        hoistEpochInfo (left (T.pack . show)) $ epochInfo timeTranslation

    systemStart = systemStartTime timeTranslation

    -- | Assign redeemers with null execution units to the input transaction.
    --
    -- Redeemers are determined from the context given to the caller via the
    -- 'Redeemer' type which is mapped to an 'Alonzo.ScriptPurpose'.
    assignNullRedeemers
        :: RecentEraLedgerConstraints (ShelleyLedgerEra era)
        => Tx (ShelleyLedgerEra era)
        -> Either ErrAssignRedeemers
            ( Map Alonzo.RdmrPtr Redeemer
            , Tx (ShelleyLedgerEra era)
            )
    assignNullRedeemers ledgerTx = do
        (indexedRedeemers, nullRedeemers) <-
            fmap unzip $ forM redeemers parseRedeemer
        pure
            ( Map.fromList indexedRedeemers
            , ledgerTx
                & witsTxL . rdmrsTxWitsL
                    .~ (Alonzo.Redeemers (Map.fromList nullRedeemers))
            )
      where
        parseRedeemer rd = do
            let mPtr = Alonzo.rdptr
                    (txBody (recentEra @era) ledgerTx)
                    (toScriptPurpose rd)
            ptr <- case mPtr of
                SNothing -> Left $ ErrAssignRedeemersTargetNotFound rd
                SJust ptr -> pure ptr
            let mDeserialisedData =
                    deserialiseOrFail $ BL.fromStrict $ redeemerData rd
            rData <- case mDeserialisedData of
                Left e -> Left $ ErrAssignRedeemersInvalidData rd (show e)
                Right d -> pure (Alonzo.Data d)
            pure ((ptr, rd), (ptr, (rData, mempty)))

    -- | Evaluate execution units of each script/redeemer in the transaction.
    -- This may fail for each script.
    evaluateExecutionUnits
        :: RecentEraLedgerConstraints (ShelleyLedgerEra era)
        => Map Alonzo.RdmrPtr Redeemer
        -> Tx (Cardano.ShelleyLedgerEra era)
        -> Either ErrAssignRedeemers
            (Map Alonzo.RdmrPtr (Either ErrAssignRedeemers Alonzo.ExUnits))
    evaluateExecutionUnits indexedRedeemers ledgerTx =
        Ledger.evalTxExUnits
            pparams ledgerTx (fromCardanoUTxO utxo) epochInformation systemStart
        & bimap
            ErrAssignRedeemersTranslationError
            (hoistScriptFailure indexedRedeemers)

    hoistScriptFailure
        :: Show scriptFailure
        => Map Alonzo.RdmrPtr Redeemer
        -> Map Alonzo.RdmrPtr (Either scriptFailure a)
        -> Map Alonzo.RdmrPtr (Either ErrAssignRedeemers a)
    hoistScriptFailure indexedRedeemers = Map.mapWithKey $ \ptr -> left $ \e ->
        ErrAssignRedeemersScriptFailure (indexedRedeemers ! ptr) (show e)

    -- | Change execution units for each redeemers in the transaction to what
    -- they ought to be.
    assignExecutionUnits
        :: RecentEraLedgerConstraints (ShelleyLedgerEra era)
        => Map Alonzo.RdmrPtr (Either ErrAssignRedeemers Alonzo.ExUnits)
        -> Tx (ShelleyLedgerEra era)
        -> Either ErrAssignRedeemers (Tx (ShelleyLedgerEra era))
    assignExecutionUnits exUnits ledgerTx = do
        let Alonzo.Redeemers rdmrs = view (witsTxL . rdmrsTxWitsL) ledgerTx

        rdmrs' <- Map.mergeA
            Map.preserveMissing
            Map.dropMissing
            (Map.zipWithAMatched (const assignUnits))
            rdmrs
            exUnits

        pure $ ledgerTx
            & (witsTxL . rdmrsTxWitsL) .~ (Alonzo.Redeemers rdmrs')

    assignUnits
        :: (dat, Alonzo.ExUnits)
        -> Either err Alonzo.ExUnits
        -> Either err (dat, Alonzo.ExUnits)
    assignUnits (dats, _zero) = fmap (dats,)

    -- | Finally, calculate and add the script integrity hash with the new
    -- final redeemers, if any.
    addScriptIntegrityHash
        :: RecentEraLedgerConstraints (ShelleyLedgerEra era)
        => Tx (ShelleyLedgerEra era)
        -> Tx (ShelleyLedgerEra era)
    addScriptIntegrityHash ledgerTx =
        ledgerTx & (bodyTxL . scriptIntegrityHashTxBodyL) .~
            Alonzo.hashScriptIntegrity
                (Set.fromList $ Alonzo.getLanguageView pparams <$> langs)
                (Alonzo.txrdmrs wits)
                (Alonzo.txdats wits)
      where
        wits = Alonzo.wits ledgerTx
        langs =
            [ l
            | (_hash, script) <- Map.toList (Alonzo.txscripts wits)
            , (not . Ledger.isNativeScript @(ShelleyLedgerEra era)) script
            , Just l <- [Alonzo.language script]
            ]

--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------

-- | Effectfully modify the state of a state-monad transformer stack.
modifyM  :: forall m s. (Monad m) => (s -> m s) -> StateT s m ()
modifyM fn = get >>= lift . fn >>= put
