{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

{- HLINT ignore "Use ||" -}

module Cardano.Wallet.Write.Tx.Balance where

import Prelude

import Cardano.Address.Script
    ( KeyHash, ScriptTemplate )
import Cardano.BM.Data.Tracer
    ( HasPrivacyAnnotation, HasSeverityAnnotation, Tracer )
import Cardano.BM.Tracing
    ( HasSeverityAnnotation (..), Severity (..), traceWith )
import Cardano.Tx.Balance.Internal.CoinSelection
    ( Selection
    , SelectionBalanceError (..)
    , SelectionCollateralRequirement (..)
    , SelectionConstraints (..)
    , SelectionError (..)
    , SelectionLimitOf (NoLimit)
    , SelectionOf (..)
    , SelectionOutputError (..)
    , SelectionParams (..)
    , SelectionReportDetailed
    , SelectionReportSummarized
    , SelectionSkeleton (..)
    , SelectionStrategy (..)
    , WalletSelectionContext
    , WalletUTxO (..)
    , makeSelectionReportDetailed
    , makeSelectionReportSummarized
    , performSelection
    , toExternalUTxOMap
    )
import Cardano.Wallet.Primitive.AddressDerivation
    ( BoundedAddressLength (..) )
import Cardano.Wallet.Primitive.Slotting
    ( PastHorizonException, TimeInterpreter )
import Cardano.Wallet.Primitive.Types
    ( FeePolicy (LinearFee), LinearFunction (LinearFunction) )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Cardano.Wallet.Primitive.Types.Redeemer
    ( Redeemer )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle (..) )
import Cardano.Wallet.Primitive.Types.TokenPolicy
    ( TokenName (..), TokenPolicyId (..) )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity (..) )
import Cardano.Wallet.Primitive.Types.Tx
    ( SealedTx, sealedTxFromCardano )
import Cardano.Wallet.Primitive.Types.Tx.Constraints
    ( TxSize (..) )
import Cardano.Wallet.Primitive.Types.UTxOIndex
    ( UTxOIndex )
import Cardano.Wallet.Primitive.Types.UTxOSelection
    ( UTxOSelection )
import Cardano.Wallet.Shelley.Compatibility
    ( fromCardanoTxIn, fromCardanoTxOut, toCardanoUTxO )
import Cardano.Wallet.Shelley.Transaction
    ( TxUpdate (..) )
import Cardano.Wallet.Transaction
    ( ErrAssignRedeemers
    , ErrMoreSurplusNeeded (..)
    , ErrUpdateSealedTx
    , TransactionCtx (..)
    , TransactionLayer (..)
    , TxFeeAndChange (..)
    , TxFeeUpdate (UseNewTxFee)
    , WitnessCountCtx (..)
    , defaultTransactionCtx
    )
import Cardano.Wallet.Write.Tx
    ( IsRecentEra (..)
    , PParams
    , RecentEra (..)
    , computeMinimumCoinForTxOut
    , modifyLedgerBody
    , modifyTxOutCoin
    , modifyTxOutputs
    )
import Control.Arrow
    ( left )
import Control.Monad
    ( forM, unless, when )
import Control.Monad.Random
    ( MonadRandom, evalRand )
import Control.Monad.Trans.Class
    ( lift )
import Control.Monad.Trans.Except
    ( ExceptT (ExceptT), catchE, except, runExceptT, throwE, withExceptT )
import Control.Monad.Trans.State
    ( runState, state )
import Data.Either
    ( lefts, partitionEithers )
import qualified Data.Foldable as F
import Data.Generics.Internal.VL.Lens
    ( over, view, (^.) )
import Data.Generics.Labels
    ()
import Data.IntCast
    ( intCast, intCastMaybe )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Quantity (..) )
import Data.Text.Class
    ( ToText (..) )
import Data.Type.Equality
    ( (:~:) (..), testEquality )
import Data.Word
    ( Word16 )
import Fmt
    ( Buildable
    , Builder
    , blockListF'
    , build
    , nameF
    , pretty
    , (+|)
    , (+||)
    , (|+)
    , (||+)
    )
import GHC.Generics
    ( Generic )
import Numeric.Natural
    ( Natural )
import System.Random.StdGenSeed
    ( StdGenSeed (..), stdGenFromSeed, stdGenSeed )
import Text.Pretty.Simple
    ( pShow )

import qualified Cardano.Address.Script as CA
import qualified Cardano.Api as Cardano
import qualified Cardano.Api.Shelley as Cardano
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Cardano.Wallet.Primitive.Types.Address as W
import qualified Cardano.Wallet.Primitive.Types.Coin as Coin
import qualified Cardano.Wallet.Primitive.Types.Coin as W
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.Tx as W
import qualified Cardano.Wallet.Primitive.Types.Tx.TxIn as W
import qualified Cardano.Wallet.Primitive.Types.Tx.TxOut as W
import qualified Cardano.Wallet.Primitive.Types.UTxO as W
import qualified Cardano.Wallet.Primitive.Types.UTxO as UTxO
import qualified Cardano.Wallet.Primitive.Types.UTxOIndex as UTxOIndex
import qualified Cardano.Wallet.Primitive.Types.UTxOSelection as UTxOSelection
import qualified Cardano.Wallet.Write.Tx as Write.Tx
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- | Helper wrapper type for the sake of logging.
data BuildableInAnyEra tx = forall era.
    ( Eq (tx era)
    , Show (tx era)
    , Buildable (tx era)
    ) => BuildableInAnyEra (Cardano.CardanoEra era) (tx era)

instance Show (BuildableInAnyEra a) where
    show (BuildableInAnyEra _ a) = show a

instance Eq (BuildableInAnyEra a) where
    BuildableInAnyEra era1 thing1 == BuildableInAnyEra era2 thing2 =
        case testEquality era1 era2 of
            Just Refl -> thing1 == thing2
            Nothing -> False

instance Buildable (BuildableInAnyEra a) where
    build (BuildableInAnyEra _ x) = build x

data BalanceTxLog
    = MsgSelectionForBalancingStart Int (BuildableInAnyEra PartialTx)
    | MsgSelectionStart Int [W.TxOut]
    | MsgSelectionError (SelectionError WalletSelectionContext)
    | MsgSelectionReportSummarized SelectionReportSummarized
    | MsgSelectionReportDetailed SelectionReportDetailed
    deriving (Eq, Show)

instance ToText BalanceTxLog where
    toText = \case
        MsgSelectionStart utxoSize recipients ->
            "Starting coin selection " <>
            "|utxo| = "+|utxoSize|+" " <>
            "#recipients = "+|length recipients|+""
        MsgSelectionForBalancingStart utxoSize partialTx ->
            "Starting coin selection for balancing " <>
            "|utxo| = "+|utxoSize|+" " <>
            "partialTx = "+|partialTx|+""
        MsgSelectionError e ->
            "Failed to select assets:\n"+|| e ||+""
        MsgSelectionReportSummarized s ->
            "Selection report (summarized):\n"+| s |+""
        MsgSelectionReportDetailed s ->
            "Selection report (detailed):\n"+| s |+""

instance HasPrivacyAnnotation BalanceTxLog
instance HasSeverityAnnotation BalanceTxLog where
    getSeverityAnnotation = \case
        MsgSelectionStart{} -> Debug
        MsgSelectionForBalancingStart{} -> Debug
        MsgSelectionError{} -> Debug
        MsgSelectionReportSummarized{} -> Info
        MsgSelectionReportDetailed{} -> Debug

data ErrSelectAssets
    = ErrSelectAssetsPrepareOutputsError
        (SelectionOutputError WalletSelectionContext)
    | ErrSelectAssetsAlreadyWithdrawing W.Tx
    | ErrSelectAssetsSelectionError (SelectionError WalletSelectionContext)
    deriving (Generic, Eq, Show)

data ErrBalanceTxInternalError
    = ErrUnderestimatedFee W.Coin SealedTx
    | ErrFailedBalancing Cardano.Value
    deriving (Show, Eq)

-- | Errors that can occur when balancing transaction.
data ErrBalanceTx
    = ErrBalanceTxUpdateError ErrUpdateSealedTx
    | ErrBalanceTxSelectAssets ErrSelectAssets
    | ErrBalanceTxMaxSizeLimitExceeded
    | ErrBalanceTxExistingCollateral
    | ErrBalanceTxExistingTotalCollateral
    | ErrBalanceTxExistingReturnCollateral
    | ErrBalanceTxConflictingNetworks
    | ErrBalanceTxAssignRedeemers ErrAssignRedeemers
    | ErrBalanceTxInternalError ErrBalanceTxInternalError
    | ErrBalanceTxInputResolutionConflicts (NonEmpty (W.TxOut, W.TxOut))
    | ErrBalanceTxUnresolvedInputs (NonEmpty W.TxIn)
    | ErrOldEraNotSupported Cardano.AnyCardanoEra
    deriving (Show, Eq)

-- | A 'PartialTx' is an an unbalanced 'SealedTx' along with the necessary
-- information to balance it.
--
-- The 'TxIn's of the 'inputs' must exactly match the inputs contained in the
-- 'tx'. If not, the behaviour is undefined. This will be fixed by ADP-1662.
--
-- The provided 'redeemers' will overwrite any redeemers inside the 'tx'. This
-- is done as the internal redeemers in the 'tx' use an index referring to a
-- 'TxIn', rather than an actual 'TxIn'. When we are adding extra inputs as part
-- of balancing, these indexes become incorrect.
--
-- TODO: With some extra care, we could probably remove the 'redeemers' field
-- and instead adjust the existing redeemer indexes ourselves when balancing,
-- even though they are in an "unordered" set.
data PartialTx era = PartialTx
    { tx :: Cardano.Tx era
    , inputs :: Cardano.UTxO era
      -- ^ NOTE: Can we rename this to something better? Perhaps 'extraUTxO'?
    , redeemers :: [Redeemer]
    } deriving (Show, Generic, Eq)

instance Buildable (PartialTx era) where
    build (PartialTx tx (Cardano.UTxO ins) redeemers)
        = nameF "PartialTx" $ mconcat
            [ nameF "inputs" (blockListF' "-" inF (Map.toList ins))
            , nameF "redeemers" (pretty redeemers)
            , nameF "tx" (cardanoTxF tx)
            ]
      where
        inF = build . show

        cardanoTxF :: Cardano.Tx era -> Builder
        cardanoTxF tx' = pretty $ pShow tx'

balanceTransaction
    :: forall era m s k ktype.
        ( MonadRandom m
        , IsRecentEra era
        , BoundedAddressLength k
        )
    => Tracer m BalanceTxLog
    -> TransactionLayer k ktype SealedTx
    -> Maybe ([(W.TxIn, W.TxOut)] -> [CA.Script KeyHash])
    -> Maybe ScriptTemplate
    -> (W.ProtocolParameters, Cardano.ProtocolParameters)
    -- ^ 'Cardano.ProtocolParameters' can be retrieved via a Local State Query
    -- to a local node.
    --
    -- If passed an incorrect value, a phase 1 script integrity hash mismatch
    -- will protect against collateral being forfeited.
    --
    -- TODO: Remove the 'W.ProtocolParameters' argument.
    -> TimeInterpreter (Either PastHorizonException)
    -- ^ Needed to convert convert validity intervals from 'UTCTime' to 'SlotNo'
    -- when executing Plutus scripts.
    --
    -- Can be hard-coded for a given network configuration. Just be
    -- cautious of the fact that the value will occasionally
    -- change as new eras are introduced to Cardano.
    --
    -- It is unclear whether an incorrect value could cause collateral to be
    -- forfeited. We should ideally investigate and clarify as part of ADP-1544
    -- or similar ticket. Relevant ledger code: https://github.com/input-output-hk/cardano-ledger/blob/fdec04e8c071060a003263cdcb37e7319fb4dbf3/eras/alonzo/impl/src/Cardano/Ledger/Alonzo/TxInfo.hs#L428-L440
    -> UTxOIndex WalletUTxO
    -- ^ TODO [ADP-1789] Replace with @Cardano.UTxO@
    -> ChangeAddressGen s
    -> s
    -> PartialTx era
    -> ExceptT ErrBalanceTx m (Cardano.Tx era, s)
balanceTransaction
    tr txLayer toInpScriptsM pScriptTemplateM pp ti idx genChange s unadjustedPtx = do
    -- TODO [ADP-1490] Take 'Ledger.PParams era' directly as argument, and avoid
    -- converting to/from Cardano.ProtocolParameters. This may affect
    -- performance. The addition of this one specific conversion seems to have
    -- made the --match "balanceTransaction" unit tests 11% slower in CPU time.
    let ledgerPP = Cardano.toLedgerPParams shelleyEra $ snd pp
    let adjustedPtx = over (#tx)
            (increaseZeroAdaOutputs (recentEra @era) ledgerPP)
            unadjustedPtx

    let balanceWith strategy =
            balanceTransactionWithSelectionStrategyAndNoZeroAdaAdjustment
                @era @m @s @k @ktype
                tr txLayer toInpScriptsM pScriptTemplateM
                pp ti idx genChange s strategy adjustedPtx
    balanceWith SelectionStrategyOptimal
        `catchE` \e ->
            if minimalStrategyIsWorthTrying e
            then balanceWith SelectionStrategyMinimal
            else throwE e
  where
    shelleyEra = Cardano.shelleyBasedEra @era

    -- Determines whether or not the minimal selection strategy is worth trying.
    -- This depends upon the way in which the optimal selection strategy failed.
    minimalStrategyIsWorthTrying :: ErrBalanceTx -> Bool
    minimalStrategyIsWorthTrying e = or
        [ maxSizeLimitExceeded
        , unableToConstructChange
        , selectionCollateralError
        ]
      where
        -- The size of a transaction can be reduced by selecting fewer inputs,
        -- or by generating less change. Since the minimal selection strategy
        -- selects as few inputs and generates as little change as possible,
        -- using this strategy might allow us to generate a transaction within
        -- the size limit.
        maxSizeLimitExceeded = case e of
            ErrBalanceTxMaxSizeLimitExceeded ->
                True
            _someOtherError ->
                False

        -- In situations where the available supply of ada is constrained, or
        -- where all available ada is bundled up with other tokens, this can
        -- prevent us from generating change. In this case, trying again with
        -- the minimal selection strategy might allow us to select fewer
        -- inputs, generate less change, lower the amount of ada required to
        -- pay for the change, and therefore increase the chance that we can
        -- generate change successfully.
        unableToConstructChange = case e of
            ErrBalanceTxSelectAssets
                (ErrSelectAssetsSelectionError
                (SelectionBalanceErrorOf
                (UnableToConstructChange {}))) ->
                True
            _someOtherError ->
                False

        -- The minimum required amount of collateral depends on the transaction
        -- fee, which in turn depends on the space occupied by ordinary inputs
        -- and generated change. If we select fewer inputs and generate less
        -- change, we can lower the transaction fee, lower the minimum required
        -- amount of collateral, and increase the chance of being able to
        -- satisfy the minimum.
        selectionCollateralError = case e of
            ErrBalanceTxSelectAssets
                (ErrSelectAssetsSelectionError
                (SelectionCollateralErrorOf {})) ->
                True
            _someOtherError ->
                False

-- | Increases the ada value of any 0-ada outputs in the transaction to the
-- minimum according to 'computeMinimumCoinForTxOut'.
increaseZeroAdaOutputs
    :: forall era. RecentEra era
    -> PParams (Cardano.ShelleyLedgerEra era)
    -> Cardano.Tx era
    -> Cardano.Tx era
increaseZeroAdaOutputs era pp = modifyLedgerBody $
    modifyTxOutputs era $ \out ->
        flip (modifyTxOutCoin era) out $ \c ->
            if c == mempty
            then computeMinimumCoinForTxOut era pp out
            else c

-- | Internal helper to 'balanceTransaction'
balanceTransactionWithSelectionStrategyAndNoZeroAdaAdjustment
    :: forall era m s k ktype.
        ( BoundedAddressLength k
        , MonadRandom m
        , IsRecentEra era
        )
    => Tracer m BalanceTxLog
    -> TransactionLayer k ktype SealedTx
    -> Maybe ([(W.TxIn, W.TxOut)] -> [CA.Script KeyHash])
    -> Maybe ScriptTemplate
    -> (W.ProtocolParameters, Cardano.ProtocolParameters)
    -> TimeInterpreter (Either PastHorizonException)
    -> UTxOIndex WalletUTxO
    -> ChangeAddressGen s
    -> s
    -> SelectionStrategy
    -> PartialTx era
    -> ExceptT ErrBalanceTx m (Cardano.Tx era, s)
balanceTransactionWithSelectionStrategyAndNoZeroAdaAdjustment
    tr
    txLayer
    toInpScriptsM
    pScriptTemplateM
    (pp, nodePParams)
    ti
    internalUtxoAvailable
    genChange
    s
    selectionStrategy
    ptx@(PartialTx partialTx inputUTxO redeemers)
    = do
    guardExistingCollateral partialTx
    guardExistingTotalCollateral partialTx
    guardExistingReturnCollateral partialTx
    guardConflictingWithdrawalNetworks partialTx
    guardWalletUTxOConsistencyWith inputUTxO

    let era = Cardano.anyCardanoEra $ Cardano.cardanoEra @era

    (balance0, minfee0) <- balanceAfterSettingMinFee partialTx

    (extraInputs, extraCollateral', extraOutputs, s') <- do

        -- NOTE: It is not possible to know the script execution cost in
        -- advance because it actually depends on the final transaction. Inputs
        -- selected as part of the fee balancing might have an influence on the
        -- execution cost.
        -- However, they are bounded so it is possible to balance the
        -- transaction considering only the maximum cost, and only after, try to
        -- adjust the change and ExUnits of each redeemer to something more
        -- sensible than the max execution cost.

        randomSeed <- stdGenSeed
        let
            transform
                :: Selection
                -> ([(W.TxIn, W.TxOut)], [(W.TxIn, W.TxOut)], [W.TxOut], s)
            transform sel =
                let (sel', s') = assignChangeAddresses genChange sel s
                    inputs = F.toList (sel' ^. #inputs)
                in  ( inputs
                    , sel' ^. #collateral
                    , sel' ^. #change
                    , s'
                    )

        lift $ traceWith tr $ MsgSelectionForBalancingStart
            (UTxOIndex.size internalUtxoAvailable)
            (BuildableInAnyEra Cardano.cardanoEra ptx)

        externalSelectedUtxo <- extractExternallySelectedUTxO ptx
        let mSel = selectAssets'
                era
                (extractOutputsFromTx $ toSealed partialTx)
                (UTxOSelection.fromIndexPair
                    (internalUtxoAvailable, externalSelectedUtxo))
                balance0
                minfee0
                randomSeed

        case mSel of
            Left e -> lift $
                traceWith tr $ MsgSelectionError e
            Right sel -> lift $ do
                traceWith tr $ MsgSelectionReportSummarized
                    $ makeSelectionReportSummarized sel
                traceWith tr $ MsgSelectionReportDetailed
                    $ makeSelectionReportDetailed sel

        withExceptT (ErrBalanceTxSelectAssets . ErrSelectAssetsSelectionError)
            . except
            $ transform <$> mSel

    -- NOTE:
    -- Once the coin selection is done, we need to
    --
    -- (a) Add selected inputs, collateral and change outputs to the transaction
    -- (b) Assign correct execution units to every redeemer
    -- (c) Correctly reference redeemed entities with redeemer pointers
    -- (d) Adjust fees and change output(s) to the new fees.
    -- (e) If added inputs are native script originated coresponding scripts
    --     need to be added as witnesses
    --
    -- There's a strong assumption that modifying the fee value AND increasing
    -- the coin values of change outputs does not modify transaction fees; or
    -- more exactly, does not modify the execution units of scripts. This is in
    -- principle a fair assumption because script validators ought to be
    -- unaware of change outputs. If their execution costs increase when change
    -- output values increase, then it becomes impossible to guarantee that fee
    -- balancing will ever converge towards a fixed point. A script validator
    -- doing such a thing is considered bonkers and this is not a behavior we
    -- ought to support.

    let extraInputScripts = case toInpScriptsM of
            Just toInpScripts ->
                toInpScripts (extraInputs <> extraCollateral')
            Nothing ->
                []
    let extraCollateral = fst <$> extraCollateral'
    let unsafeFromLovelace (Cardano.Lovelace l) = Coin.unsafeFromIntegral l
    candidateTx <- assembleTransaction $ TxUpdate
        { extraInputs
        , extraCollateral
        , extraOutputs
        , extraInputScripts
        , feeUpdate = UseNewTxFee $ unsafeFromLovelace minfee0
        }

    (balance, candidateMinFee) <- balanceAfterSettingMinFee candidateTx
    surplus <- case Cardano.selectLovelace balance of
        (Cardano.Lovelace c)
            | c >= 0 ->
                pure $ Coin.unsafeFromIntegral c
            | otherwise ->
                throwE . ErrBalanceTxInternalError $
                ErrUnderestimatedFee
                    (Coin.unsafeFromIntegral (-c))
                    (toSealed candidateTx)

    let feeAndChange = TxFeeAndChange
            (unsafeFromLovelace candidateMinFee)
            (extraOutputs)
    let feePolicy = view (#txParameters . #getFeePolicy) pp

    -- @distributeSurplus@ should never fail becase we have provided enough
    -- padding in @selectAssets'@.
    TxFeeAndChange updatedFee updatedChange <- withExceptT
        (\(ErrMoreSurplusNeeded c) ->
            ErrBalanceTxInternalError $
                ErrUnderestimatedFee c (toSealed candidateTx))
        (ExceptT . pure $
            distributeSurplus txLayer feePolicy surplus feeAndChange)

    fmap (, s') . guardTxSize =<< guardTxBalanced =<< assembleTransaction
        TxUpdate
            { extraInputs
            , extraCollateral
            , extraOutputs = updatedChange
            , extraInputScripts
            , feeUpdate = UseNewTxFee updatedFee
            }
  where
    toSealed :: Cardano.Tx era -> SealedTx
    toSealed = sealedTxFromCardano . Cardano.InAnyCardanoEra Cardano.cardanoEra

    -- | Extract the inputs from the raw 'tx' of the 'Partialtx', with the
    -- corresponding 'TxOut' according to @combinedUTxO@.
    --
    -- === Examples using pseudo-code
    --
    -- >>> let extraUTxO = {inA -> outA, inB -> outB }
    -- >>> let tx = addInputs [inA] emptyTx
    -- >>> let ptx = PartialTx tx extraUTxO []
    -- >>> extractExternallySelectedUTxO ptx
    -- Right (UTxOIndex.fromMap {inA -> outA})
    --
    -- >>> let extraUTxO = {inB -> outB }
    -- >>> let tx = addInputs [inA, inC] emptyTx
    -- >>> let ptx = PartialTx tx extraUTxO []
    -- >>> extractExternallySelectedUTxO ptx
    -- Left (ErrBalanceTxUnresolvedInputs [inA, inC])
    extractExternallySelectedUTxO
        :: PartialTx era
        -> ExceptT ErrBalanceTx m (UTxOIndex WalletUTxO)
    extractExternallySelectedUTxO (PartialTx tx _ _rdms) = do
        let res = flip map txIns $ \(i, _) -> do
                case Map.lookup i utxo of
                    Nothing ->
                       Left i
                    Just o -> do
                        let i' = fromCardanoTxIn i
                        let W.TxOut addr bundle = fromCardanoTxOut o
                        pure (WalletUTxO i' addr, bundle)

        case partitionEithers res of
            ([], resolved) ->
                pure $ UTxOIndex.fromSequence resolved
            (unresolvedInsHead:unresolvedInsTail, _) ->
                throwE
                . ErrBalanceTxUnresolvedInputs
                . fmap fromCardanoTxIn
                $ (unresolvedInsHead :| unresolvedInsTail)
      where
        Cardano.UTxO utxo = combinedUTxO
        Cardano.Tx (Cardano.TxBody (Cardano.TxBodyContent { Cardano.txIns })) _
            = tx

    guardTxSize :: Cardano.Tx era -> ExceptT ErrBalanceTx m (Cardano.Tx era)
    guardTxSize tx = do
        let size = estimateSignedTxSize txLayer nodePParams combinedUTxO tx
        let maxSize = TxSize
                . intCast
                . getQuantity
                $ view (#txParameters . #getTxMaxSize) pp
        when (size > maxSize) $
            throwE ErrBalanceTxMaxSizeLimitExceeded
        return tx

    guardTxBalanced :: Cardano.Tx era -> ExceptT ErrBalanceTx m (Cardano.Tx era)
    guardTxBalanced tx = do
        let bal = txBalance tx
        if bal == mempty
            then pure tx
            else throwE $ ErrBalanceTxInternalError $ ErrFailedBalancing bal

    txBalance :: Cardano.Tx era -> Cardano.Value
    txBalance
        = Write.Tx.toCardanoValue @era
        . Write.Tx.evaluateTransactionBalance (recentEra @era) ledgerPP
            (Write.Tx.fromCardanoUTxO combinedUTxO)
        . Write.Tx.txBody (recentEra @era)
        . Write.Tx.fromCardanoTx

    ledgerPP =
        Cardano.toLedgerPParams (Cardano.shelleyBasedEra @era) nodePParams

    balanceAfterSettingMinFee
        :: Cardano.Tx era
        -> ExceptT ErrBalanceTx m (Cardano.Value, Cardano.Lovelace)
    balanceAfterSettingMinFee tx = ExceptT . pure $ do
        -- NOTE: evaluateMinimumFee relies on correctly estimating the required
        -- number of witnesses.
        let minfee = evaluateMinimumFee txLayer nodePParams combinedUTxO tx
        let update = TxUpdate [] [] [] [] (UseNewTxFee minfee)
        tx' <- left ErrBalanceTxUpdateError $ updateTx txLayer tx update
        let balance = txBalance tx'
        let minfee' = Cardano.Lovelace $ fromIntegral $ W.unCoin minfee
        return (balance, minfee')

    -- | Ensure the wallet UTxO is consistent with a provided @Cardano.UTxO@.
    --
    -- They are not consistent iff an input can be looked up in both UTxO sets
    -- with different @Address@, or @TokenBundle@ values.
    --
    -- The @Cardano.UTxO era@ is allowed to contain additional information, like
    -- datum hashes, which the wallet UTxO cannot represent.
    --
    -- NOTE: Representing the wallet utxo as a @Cardano.UTxO@ will not make this
    -- check easier, even if it may be useful in other regards.
    guardWalletUTxOConsistencyWith
        :: Cardano.UTxO era
        -> ExceptT ErrBalanceTx m ()
    guardWalletUTxOConsistencyWith u' = do
        let u = Map.mapKeys fromCardanoTxIn
                . Map.map fromCardanoTxOut
                $ (unUTxO u')
        let conflicts = lefts $ flip map (Map.toList u) $ \(i, o) ->
                case i `UTxO.lookup` walletUTxO of
                    Just o' -> unless (o == o') $ Left (o, o')
                    Nothing -> pure ()

        case conflicts of
            [] -> return ()
            (c:cs) -> throwE $ ErrBalanceTxInputResolutionConflicts (c :| cs)
      where
         unUTxO (Cardano.UTxO u) = u

    walletUTxO :: W.UTxO
    walletUTxO = toExternalUTxOMap $ UTxOIndex.toMap internalUtxoAvailable

    combinedUTxO :: Cardano.UTxO era
    combinedUTxO = Cardano.UTxO $ mconcat
         -- The @Cardano.UTxO@ can contain strictly more information than
         -- @W.UTxO@. Therefore we make the user-specified @inputUTxO@ to take
         -- precedence. This matters if a user is trying to balance a tx making
         -- use of a datum hash in a UTxO which is also present in the wallet
         -- UTxO set. (Whether or not this is a sane thing for the user to do,
         -- is another question.)
         [ unUTxO inputUTxO
         , unUTxO $ toCardanoUTxO Cardano.shelleyBasedEra walletUTxO
         ]

      where
         unUTxO (Cardano.UTxO u) = u

    assembleTransaction
        :: TxUpdate
        -> ExceptT ErrBalanceTx m (Cardano.Tx era)
    assembleTransaction update = ExceptT . pure $ do
        tx' <- left ErrBalanceTxUpdateError $ updateTx txLayer partialTx update
        left ErrBalanceTxAssignRedeemers $ assignScriptRedeemers
            txLayer nodePParams ti combinedUTxO redeemers tx'

    extractOutputsFromTx tx =
        let
            era = Cardano.AnyCardanoEra $ Cardano.cardanoEra @era
            (W.Tx {outputs}, _, _, _, _, _) = decodeTx txLayer era AnyWitnessCountCtx tx
        in outputs

    guardConflictingWithdrawalNetworks
        (Cardano.Tx (Cardano.TxBody body) _) = do
        -- Use of withdrawals with different networks breaks balancing.
        --
        -- For instance the partial tx might contain two withdrawals with the
        -- same key but different networks:
        -- [ (Mainnet, pkh1, coin1)
        -- , (Testnet, pkh1, coin2)
        -- ]
        --
        -- Even though this is absurd, the node/ledger
        -- @evaluateTransactionBalance@ will count @coin1+coin2@ towards the
        -- total balance. Because the wallet does not consider the network tag,
        -- it will drop one of the two, leading to a discrepancy.
        let networkOfWdrl ((Cardano.StakeAddress nw _), _, _) = nw
        let conflictingWdrlNetworks = case Cardano.txWithdrawals body of
                Cardano.TxWithdrawalsNone -> False
                Cardano.TxWithdrawals _ wdrls -> Set.size
                    (Set.fromList $ map networkOfWdrl wdrls) > 1
        when conflictingWdrlNetworks $
            throwE ErrBalanceTxConflictingNetworks

    guardExistingCollateral (Cardano.Tx (Cardano.TxBody body) _) = do
        -- Coin selection does not support pre-defining collateral. In Sep 2021
        -- consensus was that we /could/ allow for it with just a day's work or
        -- so, but that the need for it was unclear enough that it was not in
        -- any way a priority.
        case Cardano.txInsCollateral body of
            Cardano.TxInsCollateralNone -> return ()
            Cardano.TxInsCollateral _ [] -> return ()
            Cardano.TxInsCollateral _ _ ->
                throwE ErrBalanceTxExistingCollateral

    guardExistingTotalCollateral (Cardano.Tx (Cardano.TxBody body) _) =
        case Cardano.txTotalCollateral body of
            Cardano.TxTotalCollateralNone -> return ()
            Cardano.TxTotalCollateral _ _ ->
               throwE ErrBalanceTxExistingTotalCollateral

    guardExistingReturnCollateral (Cardano.Tx (Cardano.TxBody body) _) =
        case Cardano.txReturnCollateral body of
            Cardano.TxReturnCollateralNone -> return ()
            Cardano.TxReturnCollateral _ _ ->
               throwE ErrBalanceTxExistingReturnCollateral

    -- | Select assets to cover the specified balance and fee.
    --
    -- If the transaction contains redeemers, the function will also ensure the
    -- selection covers the fees for the maximum allowed execution units of a
    -- transaction. For this, and other reasons, the selection may include too
    -- much ada.
    selectAssets'
        :: Cardano.AnyCardanoEra
        -> [W.TxOut]
        -> UTxOSelection WalletUTxO
        -- ^ Describes which utxos are pre-selected, and which can be used as
        -- inputs or collateral.
        -> Cardano.Value -- Balance to cover
        -> Cardano.Lovelace -- Current minfee (before selecting assets)
        -> StdGenSeed
        -> Either (SelectionError WalletSelectionContext) Selection
    selectAssets' era outs utxoSelection balance fee0 seed =
        let
            txPlutusScriptExecutionCost = maxScriptExecutionCost txLayer pp redeemers
            colReq =
                if txPlutusScriptExecutionCost > W.Coin 0 then
                    SelectionCollateralRequired
                else
                    SelectionCollateralNotRequired

            (positiveBundle, negativeBundle) = posAndNegFromCardanoValue balance
            (TokenBundle positiveAda positiveTokens) = positiveBundle
            (TokenBundle negativeAda negativeTokens) = negativeBundle

            adaInOutputs = F.foldMap (TokenBundle.getCoin . view #tokens) outs
            tokensInOutputs = F.foldMap (TokenBundle.tokens . view #tokens) outs
            tokensInInputs = TokenBundle.tokens
                $ UTxOSelection.selectedBalance utxoSelection
            adaInInputs = TokenBundle.getCoin
                $ UTxOSelection.selectedBalance utxoSelection

            boringFee =
                let
                    boringSkeleton = SelectionSkeleton
                        { skeletonInputCount =
                            UTxOSelection.selectedSize utxoSelection
                        , skeletonOutputs = outs
                        , skeletonChange = []
                        }
                in calcMinimumCost
                        txLayer
                        era
                        pp
                        defaultTransactionCtx
                        boringSkeleton

            feePadding =
                let LinearFee LinearFunction {slope = perByte} =
                        view (#txParameters . #getFeePolicy) pp

                    -- Could be made smarter by only padding for the script
                    -- integrity hash when we intend to add one. [ADP-2621]
                    scriptIntegrityHashBytes = 32 + 2

                    -- Add padding to allow the fee value to increase.
                    -- Out of caution, assume it can increase by the theoretical
                    -- maximum of 8 bytes ('maximumCostOfIncreasingCoin').
                    --
                    -- NOTE: It's not convenient to import the constant at the
                    -- moment because of the package split.
                    --
                    -- Any overestimation will be reduced by 'distributeSurplus'
                    -- in the final stage of 'balanceTransaction'.
                    extraBytes = 8
                in
                W.Coin $ (round perByte) * (extraBytes + scriptIntegrityHashBytes)

            fromCardanoLovelace (Cardano.Lovelace l) = Coin.unsafeFromIntegral l

            selectionConstraints = SelectionConstraints
                { assessTokenBundleSize =
                    view #assessTokenBundleSize $
                    tokenBundleSizeAssessor txLayer $
                    pp ^. (#txParameters . #getTokenBundleMaxSize)
                , certificateDepositAmount =
                    view #stakeKeyDeposit pp
                , computeMinimumAdaQuantity =
                    view #txOutputMinimumAdaQuantity
                        (constraints txLayer era pp)
                , isBelowMinimumAdaQuantity =
                    view #txOutputBelowMinimumAdaQuantity
                        (constraints txLayer era pp)
                , computeMinimumCost = \skeleton -> mconcat
                    [ feePadding
                    , fromCardanoLovelace fee0
                    , calcMinimumCost txLayer era pp
                        (defaultTransactionCtx
                            { txPaymentCredentialScriptTemplate = pScriptTemplateM
                            , txPlutusScriptExecutionCost =
                                txPlutusScriptExecutionCost })
                        skeleton
                    ] `Coin.difference` boringFee
                , computeSelectionLimit = \_ -> NoLimit
                , maximumCollateralInputCount =
                    intCast @Word16 @Int $ view #maximumCollateralInputCount pp
                , minimumCollateralPercentage =
                    view #minimumCollateralPercentage pp
                , maximumLengthChangeAddress =
                    maxLengthAddressFor $ Proxy @k
                }

            selectionParams = SelectionParams
                -- The following fields are essensially adjusting the coin
                -- selections notion of balance by @balance0 - sum inputs + sum
                -- outputs + fee0@ where @balance0@ is the balance of the
                -- partial tx.
                { assetsToMint = positiveTokens <> tokensInOutputs
                , assetsToBurn = negativeTokens <> tokensInInputs
                , extraCoinIn =
                    positiveAda
                    <> adaInOutputs
                    <> fromCardanoLovelace fee0
                , extraCoinOut = negativeAda <> adaInInputs

                -- We don't use the following 3 fields because certs and
                -- withdrawals are already included in the balance (passed in
                -- above).
                , rewardWithdrawal = W.Coin 0
                , certificateDepositsReturned = 0
                , certificateDepositsTaken = 0

                -- NOTE: It is important that coin selection has the correct
                -- notion of fees, because it will be used to tell how much
                -- collateral is needed.
                , collateralRequirement = colReq
                , outputsToCover = outs
                , utxoAvailableForCollateral =
                      UTxOSelection.availableMap utxoSelection
                , utxoAvailableForInputs = utxoSelection
                , selectionStrategy = selectionStrategy
                }
            in
                flip evalRand (stdGenFromSeed seed)
                    $ runExceptT
                    $ performSelection selectionConstraints selectionParams

newtype ChangeAddressGen s =
    ChangeAddressGen { getChangeAddressGen ::  (s -> (W.Address, s)) }

-- | Augments the given outputs with new outputs. These new outputs correspond
-- to change outputs to which new addresses have been assigned. This updates
-- the wallet state as it needs to keep track of new pending change addresses.
assignChangeAddresses
    :: ChangeAddressGen s
    -> SelectionOf TokenBundle
    -> s
    -> (SelectionOf W.TxOut, s)
assignChangeAddresses (ChangeAddressGen genChange) sel = runState $ do
    changeOuts <- forM (view #change sel) $ \bundle -> do
        addr <- state genChange
        pure $ W.TxOut addr bundle
    pure $ (sel :: SelectionOf TokenBundle) { change = changeOuts }

-- | Convert a 'Cardano.Value' into a positive and negative component. Useful
-- to convert the potentially negative balance of a partial tx into
-- TokenBundles.
posAndNegFromCardanoValue
    :: Cardano.Value
    -> (TokenBundle.TokenBundle, TokenBundle.TokenBundle)
posAndNegFromCardanoValue = foldMap go . Cardano.valueToList
  where
    go :: (Cardano.AssetId, Cardano.Quantity)
       -> (TokenBundle.TokenBundle, TokenBundle.TokenBundle)
    go (Cardano.AdaAssetId, q) = partition q $
        TokenBundle.fromCoin . Coin.fromNatural
    go (Cardano.AssetId policy name, q) = partition q $ \n ->
        TokenBundle.fromFlatList (W.Coin 0)
            [ ( TokenBundle.AssetId (mkPolicyId policy) (mkTokenName name)
              , TokenQuantity n
              )
            ]

    -- | Convert a 'Cardano.Quantity' to a 'TokenBundle' using the supplied
    -- function. The result is stored in 'fst' for positive quantities, and
    -- 'snd' for negative quantities.
    partition
        :: Cardano.Quantity
        -> (Natural -> TokenBundle.TokenBundle)
        -> (TokenBundle.TokenBundle, TokenBundle.TokenBundle)
    partition (Cardano.Quantity i) f
        | Just n <- maybeIntegerToNatural      i  = (f n, mempty)
        | Just n <- maybeIntegerToNatural (abs i) = (mempty, f n)
        | otherwise = (mempty, mempty)

    maybeIntegerToNatural = intCastMaybe @Integer @Natural

    mkPolicyId = UnsafeTokenPolicyId . Hash . Cardano.serialiseToRawBytes
    mkTokenName = UnsafeTokenName . Cardano.serialiseToRawBytes
