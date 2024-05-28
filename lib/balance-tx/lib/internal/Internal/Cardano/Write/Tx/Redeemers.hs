{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{- HLINT ignore "Use <$>" -}

-- |
-- Copyright: © 2023 IOHK
-- License: Apache-2.0
--
-- Module containing 'assignScriptRedeemers'
module Internal.Cardano.Write.Tx.Redeemers
    ( assignScriptRedeemers
    , ErrAssignRedeemers (..)
    , Redeemer (..)
    ) where

import Prelude

import Cardano.Ledger.Alonzo.Plutus.Context
    ( EraPlutusContext (..)
    )
import Cardano.Ledger.Api
    ( AsItem (..)
    , Tx
    , bodyTxL
    , rdmrsTxWitsL
    , scriptIntegrityHashTxBodyL
    , witsTxL
    )
import Cardano.Ledger.Shelley.API
    ( StrictMaybe (..)
    )
import Cardano.Slotting.EpochInfo
    ( EpochInfo
    , hoistEpochInfo
    )
import Codec.Serialise
    ( deserialiseOrFail
    )
import Control.Arrow
    ( left
    )
import Control.Lens
    ( (.~)
    )
import Control.Monad
    ( forM
    )
import Control.Monad.Trans.Class
    ( lift
    )
import Control.Monad.Trans.State.Strict
    ( StateT (..)
    , execStateT
    , get
    , modify'
    , put
    )
import Data.Bifunctor
    ( bimap
    )
import Data.ByteString
    ( ByteString
    )
import Data.Function
    ( (&)
    )
import Data.Generics.Internal.VL.Lens
    ( view
    )
import Data.Generics.Labels
    ()
import Data.Map.Strict
    ( Map
    , (!)
    )
import Data.Word
    ( Word32
    )
import Fmt
    ( Buildable (..)
    )
import GHC.Generics
    ( Generic
    )
import Internal.Cardano.Write.Tx
    ( IsRecentEra (..)
    , PParams
    , PolicyId
    , RecentEra (..)
    , RewardAccount
    , TxIn
    , UTxO
    )
import Internal.Cardano.Write.Tx.TimeTranslation
    ( TimeTranslation
    , epochInfo
    , systemStartTime
    )

import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import qualified Cardano.Ledger.Alonzo.TxBody as Alonzo
import qualified Cardano.Ledger.Alonzo.TxWits as Alonzo
import qualified Cardano.Ledger.Api as Alonzo
import qualified Cardano.Ledger.Api as Conway
import qualified Cardano.Ledger.Api as Ledger
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as Map
import qualified Data.Map.Merge.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T

data ErrAssignRedeemers era
    = ErrAssignRedeemersScriptFailure Redeemer String
    | ErrAssignRedeemersTargetNotFound Redeemer
    -- ^ The given redeemer target couldn't be located in the transaction.
    | ErrAssignRedeemersInvalidData Redeemer String
    -- ^ Redeemer's data isn't a valid Plutus' data.
    | ErrAssignRedeemersTranslationError (ContextError era)
    deriving (Generic)

deriving instance Eq (ContextError era) => Eq (ErrAssignRedeemers era)
deriving instance Show (ContextError era) => Show (ErrAssignRedeemers era)

assignScriptRedeemers
    :: forall era. IsRecentEra era
    => PParams era
    -> TimeTranslation
    -> UTxO era
    -> [Redeemer]
    -> Tx era
    -> Either (ErrAssignRedeemers era) (Tx era)
assignScriptRedeemers pparams timeTranslation utxo redeemers tx = do
    flip execStateT tx $ do
        indexedRedeemers <- StateT assignNullRedeemers
        executionUnits <- get
            >>= lift . evaluateExecutionUnits indexedRedeemers
        modifyM (assignExecutionUnits executionUnits)
        modify' addScriptIntegrityHash
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
        :: Tx era
        -> Either (ErrAssignRedeemers era)
            ( Map (Alonzo.PlutusPurpose Alonzo.AsIx era)  Redeemer
            , Tx era
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
            let mPtr = Alonzo.redeemerPointer
                     (view bodyTxL ledgerTx)
                     (toScriptPurpose @era rd)
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
        :: Map (Alonzo.PlutusPurpose Alonzo.AsIx era) Redeemer
        -> Tx era
        -> Either (ErrAssignRedeemers era)
            (Map (Alonzo.PlutusPurpose Alonzo.AsIx era)
                (Either (ErrAssignRedeemers era) Alonzo.ExUnits))
    evaluateExecutionUnits indexedRedeemers ledgerTx =
        Ledger.evalTxExUnits
            pparams ledgerTx utxo epochInformation systemStart
        & bimap
            ErrAssignRedeemersTranslationError (hoistScriptFailure indexedRedeemers)

    hoistScriptFailure
        :: Show scriptFailure
        => Map (Alonzo.PlutusPurpose Alonzo.AsIx era) Redeemer
        -> Map (Alonzo.PlutusPurpose Alonzo.AsIx era) (Either scriptFailure a)
        -> Map (Alonzo.PlutusPurpose Alonzo.AsIx era) (Either (ErrAssignRedeemers era) a)
    hoistScriptFailure indexedRedeemers = Map.mapWithKey $ \ptr -> left $ \e ->
        ErrAssignRedeemersScriptFailure (indexedRedeemers ! ptr) (show e)

    -- | Change execution units for each redeemers in the transaction to what
    -- they ought to be.
    assignExecutionUnits
        :: Map (Alonzo.PlutusPurpose Alonzo.AsIx era)
                (Either (ErrAssignRedeemers era) Alonzo.ExUnits)
        -> Tx era
        -> Either (ErrAssignRedeemers era) (Tx era)
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
        :: Tx era
        -> Tx era
    addScriptIntegrityHash ledgerTx =
        ledgerTx & (bodyTxL . scriptIntegrityHashTxBodyL) .~
            Alonzo.hashScriptIntegrity
                (Set.fromList $ Alonzo.getLanguageView pparams <$> langs)
                (Alonzo.txrdmrs wits)
                (Alonzo.txdats' wits)
      where
        wits = Alonzo.wits ledgerTx
        langs =
            [ Alonzo.plutusScriptLanguage plutus
            | (_hash, script) <- Map.toList (Alonzo.txscripts wits)
            , (not . Ledger.isNativeScript @era) script
            , Just plutus <- [Alonzo.toPlutusScript script]
            ]

--
-- The 'Redeemer' type
-- TODO: Move back to the wallet and/or retire
--

data Redeemer
    = RedeemerSpending ByteString TxIn
    | RedeemerMinting ByteString PolicyId
    | RedeemerRewarding ByteString RewardAccount
    deriving (Eq, Generic, Show)

instance Buildable Redeemer where
    build = \case
        RedeemerSpending _ input ->
            "spending(" <> build (show input) <> ")"
        RedeemerMinting _ pid ->
            "minting(" <> build (show pid) <> ")"
        RedeemerRewarding _ acc ->
            "rewarding(" <> build (show acc) <> ")"

redeemerData :: Redeemer -> ByteString
redeemerData = \case
    RedeemerSpending  bytes _ -> bytes
    RedeemerMinting   bytes _ -> bytes
    RedeemerRewarding bytes _ -> bytes

toScriptPurpose
    :: IsRecentEra era
    => Redeemer
    -> Alonzo.PlutusPurpose AsItem  era
toScriptPurpose = \case
    RedeemerSpending _ txin ->
        mkSpendingPurpose $ AsItem txin
    RedeemerMinting _ pid ->
        mkMintingPurpose $ AsItem pid
    RedeemerRewarding _ acc ->
        mkRewardingPurpose $ AsItem acc

mkSpendingPurpose
    :: forall era
     . IsRecentEra era
    => AsItem Word32 TxIn
    -> Alonzo.PlutusPurpose AsItem era
mkSpendingPurpose = case recentEra @era of
    RecentEraBabbage -> Alonzo.AlonzoSpending
    RecentEraConway -> Conway.ConwaySpending

mkMintingPurpose
    :: forall era
     . IsRecentEra era
    => AsItem Word32 PolicyId
    -> Alonzo.PlutusPurpose AsItem era
mkMintingPurpose = case recentEra @era of
    RecentEraBabbage -> Alonzo.AlonzoMinting
    RecentEraConway -> Conway.ConwayMinting

mkRewardingPurpose
    :: forall era
     . IsRecentEra era
    => AsItem Word32 RewardAccount
    -> Alonzo.PlutusPurpose AsItem era
mkRewardingPurpose = case recentEra @era of
    RecentEraBabbage -> Alonzo.AlonzoRewarding
    RecentEraConway -> Conway.ConwayRewarding

--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------

-- | Effectfully modify the state of a state-monad transformer stack.
modifyM  :: forall m s. (Monad m) => (s -> m s) -> StateT s m ()
modifyM fn = get >>= lift . fn >>= put
