{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

{- HLINT ignore "Use ||" -}

-- TODO: https://cardanofoundation.atlassian.net/browse/ADP-2841
{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 902
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}
#endif

module Cardano.Wallet.Write.Tx.Balance
    (
    -- * Balancing transactions
      balanceTransaction
    , ErrBalanceTx (..)
    , ErrBalanceTxInternalError (..)
    , ErrBalanceTxOutputError (..)
    , ErrBalanceTxOutputErrorInfo (..)
    , ErrBalanceTxOutputAdaQuantityInsufficientError (..)
    , ErrBalanceTxOutputSizeExceedsLimitError (..)
    , ErrBalanceTxOutputTokenQuantityExceedsLimitError (..)
    , ErrSelectAssets (..)
    , ErrUpdateSealedTx (..)
    , ErrAssignRedeemers (..)

    -- * Change addresses
    , ChangeAddressGen (..)
    , assignChangeAddresses

    -- * Partial transactions
    , PartialTx (..)
    , Redeemer (..)

    -- * UTxO assumptions
    , UTxOAssumptions (..)

    -- * UTxO indices
    , UTxOIndex
    , constructUTxOIndex

    -- * Utilities
    , posAndNegFromCardanoValue
    , fromWalletUTxO

    -- ** updateTx
    , TxUpdate (..)
    , noTxUpdate
    , updateTx
    , TxFeeUpdate (..)

    -- ** distributeSurplus
    , distributeSurplus
    , distributeSurplusDelta
    , sizeOfCoin
    , maximumCostOfIncreasingCoin
    , costOfIncreasingCoin
    , TxFeeAndChange (..)
    , ErrMoreSurplusNeeded (..)
    )
    where

import Prelude

import Cardano.Ledger.Alonzo.Core
    ( ppCollateralPercentageL, ppMaxCollateralInputsL )
import Cardano.Ledger.Alonzo.Scripts
    ( AlonzoScript (..) )
import Cardano.Ledger.Api
    ( bodyTxL
    , collateralInputsTxBodyL
    , feeTxBodyL
    , inputsTxBodyL
    , outputsTxBodyL
    , outputsTxBodyL
    , ppMaxTxSizeL
    , ppMaxValSizeL
    )
import Cardano.Ledger.UTxO
    ( txinLookup )
import Cardano.Tx.Balance.Internal.CoinSelection
    ( Selection
    , SelectionBalanceError (..)
    , SelectionCollateralRequirement (..)
    , SelectionConstraints (..)
    , SelectionError (..)
    , SelectionOf (change)
    , SelectionParams (..)
    , SelectionStrategy (..)
    , WalletSelectionContext
    , WalletUTxO (..)
    , performSelection
    , toInternalUTxOMap
    )
import Cardano.Wallet.Primitive.Types
    ( TokenBundleMaxSize (TokenBundleMaxSize) )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle (..) )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( AssetId )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity (..) )
import Cardano.Wallet.Primitive.Types.Tx
    ( SealedTx, sealedTxFromCardano )
import Cardano.Wallet.Primitive.Types.Tx.Constraints
    ( TokenBundleSizeAssessment (..)
    , TxSize (..)
    , txOutMaxCoin
    , txOutMaxTokenQuantity
    )
import Cardano.Wallet.Primitive.Types.UTxOSelection
    ( UTxOSelection )
import Cardano.Wallet.Read.Primitive.Tx.Features.Outputs
    ( fromCardanoValue )
import Cardano.Wallet.Write.ProtocolParameters
    ( ProtocolParameters (..) )
import Cardano.Wallet.Write.Tx
    ( FeePerByte (..)
    , IsRecentEra (..)
    , KeyWitnessCount (..)
    , PParams
    , RecentEra (..)
    , ShelleyLedgerEra
    , TxBody
    , TxIn
    , TxOut
    , UTxO (..)
    , computeMinimumCoinForTxOut
    , evaluateMinimumFee
    , evaluateTransactionBalance
    , feeOfBytes
    , fromCardanoTx
    , fromCardanoUTxO
    , getFeePerByte
    , isBelowMinimumCoinForTxOut
    , maxScriptExecutionCost
    , modifyLedgerBody
    , modifyTxOutCoin
    , outputs
    , toCardanoValue
    , txBody
    , withConstraints
    )
import Cardano.Wallet.Write.Tx.Redeemers
    ( ErrAssignRedeemers (..), Redeemer (..), assignScriptRedeemers )
import Cardano.Wallet.Write.Tx.Sign
    ( estimateKeyWitnessCount, estimateSignedTxSize )
import Cardano.Wallet.Write.Tx.SizeEstimation
    ( TxSkeleton (..), estimateTxCost )
import Cardano.Wallet.Write.Tx.TimeTranslation
    ( TimeTranslation )
import Cardano.Wallet.Write.UTxOAssumptions
    ( UTxOAssumptions (..), assumedInputScriptTemplate, assumedTxWitnessTag )
import Control.Arrow
    ( left )
import Control.Monad
    ( forM, unless, when )
import Control.Monad.Random
    ( MonadRandom, evalRand )
import Control.Monad.Trans.Except
    ( ExceptT (ExceptT), catchE, except, runExceptT, throwE, withExceptT )
import Control.Monad.Trans.State
    ( runState, state )
import Data.Bifunctor
    ( bimap, second )
import Data.Bits
    ( Bits )
import Data.Either
    ( lefts, partitionEithers )
import Data.Functor
    ( (<&>) )
import Data.Generics.Internal.VL.Lens
    ( over, view, (^.) )
import Data.Generics.Labels
    ()
import Data.IntCast
    ( intCastMaybe )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Maybe
    ( fromMaybe, mapMaybe )
import Data.Type.Equality
    ( (:~:) (..), testEquality )
import Fmt
    ( Buildable, Builder, blockListF', build, nameF, pretty )
import GHC.Generics
    ( Generic )
import GHC.Stack
    ( HasCallStack )
import Numeric.Natural
    ( Natural )
import System.Random.StdGenSeed
    ( StdGenSeed (..), stdGenFromSeed, stdGenSeed )
import Text.Pretty.Simple
    ( pShow )

import qualified Cardano.Address.Script as CA
import qualified Cardano.Api as Cardano
import qualified Cardano.Api.Byron as Cardano
import qualified Cardano.Api.Byron as Byron
import qualified Cardano.Api.Shelley as Cardano
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Wallet.Primitive.Types.Address as W
import qualified Cardano.Wallet.Primitive.Types.Coin as Coin
import qualified Cardano.Wallet.Primitive.Types.Coin as W
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap
import qualified Cardano.Wallet.Primitive.Types.Tx as W
import qualified Cardano.Wallet.Primitive.Types.Tx.TxIn as W
import qualified Cardano.Wallet.Primitive.Types.Tx.TxOut as W
import qualified Cardano.Wallet.Primitive.Types.Tx.TxOut as W.TxOut
import qualified Cardano.Wallet.Primitive.Types.UTxO as UTxO
import qualified Cardano.Wallet.Primitive.Types.UTxO as W
import qualified Cardano.Wallet.Primitive.Types.UTxOIndex as UTxOIndex
import qualified Cardano.Wallet.Primitive.Types.UTxOSelection as UTxOSelection
import qualified Cardano.Wallet.Shelley.Compatibility as Compatibility
import qualified Cardano.Wallet.Shelley.Compatibility.Ledger as W
import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.Map as Map
import qualified Data.Sequence.Strict as StrictSeq
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

data ErrSelectAssets
    = ErrSelectAssetsAlreadyWithdrawing W.Tx
    | ErrSelectAssetsSelectionError (SelectionError WalletSelectionContext)
    deriving (Generic, Eq, Show)

data ErrBalanceTxInternalError
    = ErrUnderestimatedFee W.Coin SealedTx KeyWitnessCount
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
    | ErrBalanceTxOutputError ErrBalanceTxOutputError
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

data UTxOIndex era = UTxOIndex
    { walletUTxO :: !W.UTxO
    , walletUTxOIndex :: !(UTxOIndex.UTxOIndex WalletUTxO)
    , ledgerUTxO :: !(UTxO (ShelleyLedgerEra era))
    }

constructUTxOIndex :: forall era. IsRecentEra era => W.UTxO -> UTxOIndex era
constructUTxOIndex walletUTxO =
    UTxOIndex {walletUTxO, walletUTxOIndex, ledgerUTxO}
  where
    era = recentEra @era
    walletUTxOIndex = UTxOIndex.fromMap $ toInternalUTxOMap walletUTxO
    ledgerUTxO = fromWalletUTxO era walletUTxO

fromWalletUTxO
    :: RecentEra era
    -> W.UTxO
    -> UTxO (ShelleyLedgerEra era)
fromWalletUTxO era (W.UTxO m) = withConstraints era $ UTxO
    $ Map.mapKeys W.toLedger
    $ Map.map (toLedgerTxOut era) m

toWalletUTxO
    :: RecentEra era
    -> UTxO (ShelleyLedgerEra era)
    -> W.UTxO
toWalletUTxO era (UTxO m) = withConstraints era $ W.UTxO
    $ Map.mapKeys W.toWallet
    $ Map.map (toWalletTxOut era) m

balanceTransaction
    :: forall era m changeState.
        ( MonadRandom m
        , IsRecentEra era
        )
    => UTxOAssumptions
    -> ProtocolParameters era
    -- ^ 'Cardano.ProtocolParameters' can be retrieved via a Local State Query
    -- to a local node.
    --
    -- If passed an incorrect value, a phase 1 script integrity hash mismatch
    -- will protect against collateral being forfeited.
    --
    -- TODO: Remove the 'W.ProtocolParameters' argument.
    -> TimeTranslation
    -- ^ Needed to convert convert validity intervals from 'UTCTime' to 'SlotNo'
    -- when executing Plutus scripts.
    --
    -- Can be hard-coded for a given network configuration. Just be
    -- cautious of the fact that the value will occasionally
    -- change as new eras are introduced to Cardano.
    --
    -- It is unclear whether an incorrect value could cause collateral to be
    -- forfeited. We should ideally investigate and clarify as part of ADP-1544
    -- or similar ticket. Relevant ledger code:
    -- https://github.com/input-output-hk/cardano-ledger/blob/fdec04e8c071060a003263cdcb37e7319fb4dbf3/eras/alonzo/impl/src/Cardano/Ledger/Alonzo/TxInfo.hs#L428-L440
    -> UTxOIndex era
    -- ^ TODO [ADP-1789] Replace with @Cardano.UTxO@
    -> ChangeAddressGen changeState
    -> changeState
    -> PartialTx era
    -> ExceptT ErrBalanceTx m (Cardano.Tx era, changeState)
balanceTransaction
    utxoAssumptions
    pp
    timeTranslation
    utxo
    genChange
    s
    partialTx
    = do
    let adjustedPartialTx = flip (over #tx) partialTx $
            assignMinimalAdaQuantitiesToOutputsWithoutAda
                (recentEra @era)
                (pparamsLedger pp)
        balanceWith strategy =
            balanceTransactionWithSelectionStrategyAndNoZeroAdaAdjustment
                @era @m @changeState
                utxoAssumptions
                pp
                timeTranslation
                utxo
                genChange
                s
                strategy
                adjustedPartialTx
    balanceWith SelectionStrategyOptimal
        `catchE` \e ->
            if minimalStrategyIsWorthTrying e
            then balanceWith SelectionStrategyMinimal
            else throwE e
  where
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

-- | Assigns minimal ada quantities to outputs without ada.
--
-- This function only modifies outputs with ada quantities of zero. Outputs
-- that have non-zero ada quantities will not be modified.
--
-- Minimal ada quantities are computed with the 'computeMinimumCoinForTxOut'
-- function.
--
assignMinimalAdaQuantitiesToOutputsWithoutAda
    :: forall era
     . RecentEra era
    -> PParams (Cardano.ShelleyLedgerEra era)
    -> Cardano.Tx era
    -> Cardano.Tx era
assignMinimalAdaQuantitiesToOutputsWithoutAda era pp = withConstraints era $
    modifyLedgerBody $ over outputsTxBodyL $ fmap modifyTxOut
  where
    modifyTxOut out = flip (modifyTxOutCoin era) out $ \c ->
        if c == mempty then computeMinimumCoinForTxOut era pp out else c

-- | Internal helper to 'balanceTransaction'
balanceTransactionWithSelectionStrategyAndNoZeroAdaAdjustment
    :: forall era m changeState.
        ( MonadRandom m
        , IsRecentEra era
        )
    => UTxOAssumptions
    -> ProtocolParameters era
    -> TimeTranslation
    -> UTxOIndex era
    -> ChangeAddressGen changeState
    -> changeState
    -> SelectionStrategy
    -> PartialTx era
    -> ExceptT ErrBalanceTx m (Cardano.Tx era, changeState)
balanceTransactionWithSelectionStrategyAndNoZeroAdaAdjustment
    utxoAssumptions
    protocolParameters@(ProtocolParameters pp)
    timeTranslation
    (UTxOIndex walletUTxO internalUtxoAvailable walletLedgerUTxO)
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

    (balance0, minfee0, _) <- balanceAfterSettingMinFee partialTx

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
                -> ( [(W.TxIn, W.TxOut)]
                   , [(W.TxIn, W.TxOut)]
                   , [W.TxOut]
                   , changeState
                   )
            transform sel =
                let (sel', s') = assignChangeAddresses genChange sel s
                    inputs = F.toList (sel' ^. #inputs)
                in  ( inputs
                    , sel' ^. #collateral
                    , sel' ^. #change
                    , s'
                    )

        externalSelectedUtxo <- extractExternallySelectedUTxO ptx

        let mSel = selectAssets
                (recentEra @era)
                protocolParameters
                utxoAssumptions
                (extractOutputsFromTx partialTx)
                redeemers
                (UTxOSelection.fromIndexPair
                    (internalUtxoAvailable, externalSelectedUtxo))
                balance0
                (fromCardanoLovelace minfee0)
                randomSeed
                genChange
                selectionStrategy

        except $ transform <$> mSel

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

    let extraInputScripts =
            case utxoAssumptions of
                AllKeyPaymentCredentials -> []
                AllByronKeyPaymentCredentials -> []
                AllScriptPaymentCredentialsFrom _template toInpScripts ->
                    toInpScripts . view #address . snd <$>
                        extraInputs <> extraCollateral'
        extraCollateral = fst <$> extraCollateral'
        unsafeFromLovelace (Cardano.Lovelace l) = Coin.unsafeFromIntegral l
    candidateTx <- assembleTransaction $ TxUpdate
        { extraInputs
        , extraCollateral
        , extraOutputs
        , extraInputScripts
        , feeUpdate = UseNewTxFee $ unsafeFromLovelace minfee0
        }

    (balance, candidateMinFee, witCount) <-
        balanceAfterSettingMinFee candidateTx
    surplus <- case Cardano.selectLovelace balance of
        (Cardano.Lovelace c)
            | c >= 0 ->
                pure $ Coin.unsafeFromIntegral c
            | otherwise ->
                throwE . ErrBalanceTxInternalError $
                ErrUnderestimatedFee
                    (Coin.unsafeFromIntegral (-c))
                    (toSealed candidateTx)
                    witCount

    let feeAndChange = TxFeeAndChange
            (unsafeFromLovelace candidateMinFee)
            (extraOutputs)
        feePerByte = getFeePerByte (recentEra @era) pp

    -- @distributeSurplus@ should never fail becase we have provided enough
    -- padding in @selectAssets@.
    TxFeeAndChange updatedFee updatedChange <- withExceptT
        (\(ErrMoreSurplusNeeded c) ->
            ErrBalanceTxInternalError $
                ErrUnderestimatedFee c (toSealed candidateTx) witCount)
        (ExceptT . pure $
            distributeSurplus feePerByte surplus feeAndChange)

    fmap (, s') . guardTxSize witCount
        =<< guardTxBalanced
        =<< assembleTransaction
        TxUpdate
            { extraInputs
            , extraCollateral
            , extraOutputs = updatedChange
            , extraInputScripts
            , feeUpdate = UseNewTxFee updatedFee
            }
  where
    era = recentEra @era

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
        -> ExceptT ErrBalanceTx m (UTxOIndex.UTxOIndex WalletUTxO)
    extractExternallySelectedUTxO (PartialTx tx _ _rdms) =
        withConstraints era $ do
            let res = flip map txIns $ \i-> do
                    case txinLookup i combinedUTxO of
                        Nothing ->
                           Left i
                        Just o -> do
                            let i' = W.toWallet i
                            let W.TxOut addr bundle = toWalletTxOut era o
                            pure (WalletUTxO i' addr, bundle)

            case partitionEithers res of
                ([], resolved) ->
                    pure $ UTxOIndex.fromSequence resolved
                (unresolvedInsHead:unresolvedInsTail, _) ->
                    throwE
                    . ErrBalanceTxUnresolvedInputs
                    . fmap W.toWallet
                    $ (unresolvedInsHead :| unresolvedInsTail)
      where
        txIns :: [TxIn]
        txIns = withConstraints (recentEra @era) $
            Set.toList $ (fromCardanoTx tx) ^. (bodyTxL . inputsTxBodyL)

    guardTxSize
        :: KeyWitnessCount
        -> Cardano.Tx era
        -> ExceptT ErrBalanceTx m (Cardano.Tx era)
    guardTxSize witCount tx@(Cardano.Tx body _noKeyWits) =
        withConstraints (recentEra @era) $ do
            let maxSize = TxSize (pp ^. ppMaxTxSizeL)
            when (estimateSignedTxSize pp witCount body > maxSize) $
                throwE ErrBalanceTxMaxSizeLimitExceeded
            pure tx

    guardTxBalanced :: Cardano.Tx era -> ExceptT ErrBalanceTx m (Cardano.Tx era)
    guardTxBalanced tx = do
        let bal = txBalance tx
        if bal == mempty
            then pure tx
            else throwE $ ErrBalanceTxInternalError $ ErrFailedBalancing bal

    txBalance :: Cardano.Tx era -> Cardano.Value
    txBalance
        = toCardanoValue @era
        . evaluateTransactionBalance (recentEra @era) pp combinedUTxO
        . txBody (recentEra @era)
        . fromCardanoTx

    balanceAfterSettingMinFee
        :: Cardano.Tx era
        -> ExceptT ErrBalanceTx m
            (Cardano.Value, Cardano.Lovelace, KeyWitnessCount)
    balanceAfterSettingMinFee tx = ExceptT . pure $ do
        let witCount = estimateKeyWitnessCount combinedUTxO (getBody tx)
            minfee = W.toWalletCoin $ evaluateMinimumFee
                (recentEra @era) pp (fromCardanoTx tx) witCount
            update = TxUpdate [] [] [] [] (UseNewTxFee minfee)
        tx' <- left ErrBalanceTxUpdateError $ updateTx tx update
        let balance = txBalance tx'
            minfee' = Cardano.Lovelace $ fromIntegral $ W.unCoin minfee
        return (balance, minfee', witCount)
      where
        getBody (Cardano.Tx body _) = body

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
        let W.UTxO u = toWalletUTxO (recentEra @era) $ fromCardanoUTxO u'
        let conflicts = lefts $ flip map (Map.toList u) $ \(i, o) ->
                case i `UTxO.lookup` walletUTxO of
                    Just o' -> unless (o == o') $ Left (o, o')
                    Nothing -> pure ()

        case conflicts of
            [] -> return ()
            (c:cs) -> throwE $ ErrBalanceTxInputResolutionConflicts (c :| cs)

    combinedUTxO :: UTxO (ShelleyLedgerEra era)
    combinedUTxO = withConstraints era $ mconcat
         -- The @Cardano.UTxO@ can contain strictly more information than
         -- @W.UTxO@. Therefore we make the user-specified @inputUTxO@ to take
         -- precedence. This matters if a user is trying to balance a tx making
         -- use of a datum hash in a UTxO which is also present in the wallet
         -- UTxO set. (Whether or not this is a sane thing for the user to do,
         -- is another question.)
         [ fromCardanoUTxO inputUTxO
         , walletLedgerUTxO
         ]

    extractOutputsFromTx :: Cardano.Tx era -> [W.TxOut]
    extractOutputsFromTx (Cardano.ByronTx _) = case recentEra @era of {}
    extractOutputsFromTx (Cardano.ShelleyTx _ tx) =
        map fromLedgerTxOut
        $ outputs (recentEra @era)
        $ txBody (recentEra @era) tx
      where
        fromLedgerTxOut :: TxOut (ShelleyLedgerEra era) -> W.TxOut
        fromLedgerTxOut o = case recentEra @era of
           RecentEraBabbage -> W.fromBabbageTxOut o
           RecentEraConway -> W.fromConwayTxOut o

    assembleTransaction :: TxUpdate -> ExceptT ErrBalanceTx m (Cardano.Tx era)
    assembleTransaction update = ExceptT . pure $ do
        tx' <- left ErrBalanceTxUpdateError $ updateTx partialTx update
        left ErrBalanceTxAssignRedeemers $
            assignScriptRedeemers
                pp timeTranslation combinedUTxO redeemers tx'

    guardConflictingWithdrawalNetworks
        :: Cardano.Tx era -> ExceptT ErrBalanceTx m ()
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
            conflictingWdrlNetworks = case Cardano.txWithdrawals body of
                Cardano.TxWithdrawalsNone -> False
                Cardano.TxWithdrawals _ wdrls -> Set.size
                    (Set.fromList $ map networkOfWdrl wdrls) > 1
        when conflictingWdrlNetworks $
            throwE ErrBalanceTxConflictingNetworks

    guardExistingCollateral :: Cardano.Tx era -> ExceptT ErrBalanceTx m ()
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

    guardExistingTotalCollateral :: Cardano.Tx era -> ExceptT ErrBalanceTx m ()
    guardExistingTotalCollateral (Cardano.Tx (Cardano.TxBody body) _) =
        case Cardano.txTotalCollateral body of
            Cardano.TxTotalCollateralNone -> return ()
            Cardano.TxTotalCollateral _ _ ->
               throwE ErrBalanceTxExistingTotalCollateral

    guardExistingReturnCollateral :: Cardano.Tx era -> ExceptT ErrBalanceTx m ()
    guardExistingReturnCollateral (Cardano.Tx (Cardano.TxBody body) _) =
        case Cardano.txReturnCollateral body of
            Cardano.TxReturnCollateralNone -> return ()
            Cardano.TxReturnCollateral _ _ ->
               throwE ErrBalanceTxExistingReturnCollateral

    fromCardanoLovelace (Cardano.Lovelace l) = Coin.unsafeFromIntegral l

-- | Select assets to cover the specified balance and fee.
--
-- If the transaction contains redeemers, the function will also ensure the
-- selection covers the fees for the maximum allowed execution units of a
-- transaction. For this, and other reasons, the selection may include too
-- much ada.
selectAssets
    :: forall era changeState
     . RecentEra era
    -> ProtocolParameters era
    -> UTxOAssumptions
    -> [W.TxOut]
    -> [Redeemer]
    -> UTxOSelection WalletUTxO
    -- ^ Specifies which UTxOs are pre-selected, and which UTxOs can be used as
    -- inputs or collateral.
    -> Cardano.Value -- Balance to cover
    -> W.Coin -- Current minfee (before selecting assets)
    -> StdGenSeed
    -> ChangeAddressGen changeState
    -> SelectionStrategy
    -- ^ A function to assess the size of a token bundle.
    -> Either ErrBalanceTx Selection
selectAssets era (ProtocolParameters pp) utxoAssumptions outs redeemers
    utxoSelection balance fee0 seed changeGen selectionStrategy = do
        validateTxOutputs'
        performSelection'
  where
    validateTxOutputs'
        :: Either ErrBalanceTx ()
    validateTxOutputs'
        = left ErrBalanceTxOutputError
        $ validateTxOutputs selectionConstraints
            (outs <&> \out -> (view #address out, view #tokens out))

    performSelection'
        :: Either ErrBalanceTx Selection
    performSelection'
        = left
            ( ErrBalanceTxSelectAssets
            . ErrSelectAssetsSelectionError
            )
        $ (`evalRand` stdGenFromSeed seed) . runExceptT
        $ performSelection selectionConstraints selectionParams

    selectionConstraints = SelectionConstraints
        { assessTokenBundleSize =
            withConstraints era $
                -- TODO (ADP-2967): avoid importing Compatibility.
                Compatibility.tokenBundleSizeAssessor
                    (TokenBundleMaxSize (TxSize (pp ^. ppMaxValSizeL)))
                        ^. #assessTokenBundleSize
        , computeMinimumAdaQuantity = \addr tokens -> W.toWallet $
            computeMinimumCoinForTxOut
                era
                pp
                (mkLedgerTxOut era addr (TokenBundle txOutMaxCoin tokens))
        , isBelowMinimumAdaQuantity = \addr bundle ->
            isBelowMinimumCoinForTxOut
                era
                pp
                (mkLedgerTxOut era addr bundle)
        , computeMinimumCost = \skeleton -> mconcat
            [ feePadding
            , fee0
            , txPlutusScriptExecutionCost
            , estimateTxCost feePerByte $ TxSkeleton
                { txWitnessTag = assumedTxWitnessTag utxoAssumptions
                , txInputCount = view #skeletonInputCount skeleton
                , txOutputs = view #skeletonOutputs skeleton
                , txChange = view #skeletonChange skeleton
                , txPaymentTemplate = view #template <$>
                    assumedInputScriptTemplate utxoAssumptions
                }
            ] `Coin.difference` boringFee
        , maximumCollateralInputCount = withConstraints era $
            unsafeIntCast @Natural @Int $ pp ^. ppMaxCollateralInputsL
    , minimumCollateralPercentage =
        withConstraints era $ pp ^. ppCollateralPercentageL
    , maximumLengthChangeAddress =
        maxLengthChangeAddress changeGen
    }

    selectionParams = SelectionParams
        -- The following fields are essentially adjusting the coin selection
        -- algorithm's notion of balance by @balance0 - sum inputs + sum
        -- outputs + fee0@ where @balance0@ is the balance of the
        -- partial tx.
        { extraValueIn =
            balancePositive <> valueOfOutputs <> TokenBundle.fromCoin fee0
        , extraValueOut =
            balanceNegative <> valueOfInputs
        -- NOTE: It is important that coin selection has the correct
        -- notion of fees, because it will be used to tell how much
        -- collateral is needed.
        , collateralRequirement
        , outputsToCover = outs
        , utxoAvailableForCollateral = UTxOSelection.availableMap utxoSelection
        , utxoAvailableForInputs = utxoSelection
        , selectionStrategy = selectionStrategy
        }
      where
        (balancePositive, balanceNegative) = posAndNegFromCardanoValue balance
        valueOfOutputs = F.foldMap' (view #tokens) outs
        valueOfInputs = UTxOSelection.selectedBalance utxoSelection

    mkLedgerTxOut
        :: HasCallStack
        => RecentEra era
        -> W.Address
        -> TokenBundle
        -> TxOut (ShelleyLedgerEra era)
    mkLedgerTxOut txOutEra address bundle =
        case txOutEra of
            RecentEraBabbage -> W.toBabbageTxOut txOut
            RecentEraConway -> W.toConwayTxOut txOut
          where
            txOut = W.TxOut address bundle

    txPlutusScriptExecutionCost = W.toWallet @W.Coin $
        if null redeemers
            then mempty
            else maxScriptExecutionCost era pp

    collateralRequirement =
        if txPlutusScriptExecutionCost > W.Coin 0
            then SelectionCollateralRequired
            else SelectionCollateralNotRequired

    feePerByte = getFeePerByte era pp

    boringFee =
        estimateTxCost
            feePerByte
            TxSkeleton
                { txWitnessTag = assumedTxWitnessTag utxoAssumptions
                , txInputCount = UTxOSelection.selectedSize utxoSelection
                , txOutputs = outs
                , txChange = []
                , txPaymentTemplate = Nothing
                }

    feePadding
        = W.toWallet . feeOfBytes feePerByte
        $ extraBytes + scriptIntegrityHashBytes
      where
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

data ChangeAddressGen s = ChangeAddressGen
    { getChangeAddressGen :: s -> (W.Address, s)

    -- | Returns the longest address that the wallet can generate for a given
    --   key.
    --
    -- This is useful in situations where we want to compute some function of
    -- an output under construction (such as a minimum UTxO value), but don't
    -- yet have convenient access to a real address.
    --
    -- Please note that this address should:
    --
    --  - never be used for anything besides its length and validity properties.
    --  - never be used as a payment target within a real transaction.
    --
    , maxLengthChangeAddress :: W.Address
    }

-- | Augments the given outputs with new outputs. These new outputs correspond
-- to change outputs to which new addresses have been assigned. This updates
-- the wallet state as it needs to keep track of new pending change addresses.
assignChangeAddresses
    :: ChangeAddressGen s
    -> SelectionOf TokenBundle
    -> s
    -> (SelectionOf W.TxOut, s)
assignChangeAddresses (ChangeAddressGen genChange _) sel = runState $ do
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
posAndNegFromCardanoValue
    = bimap
        (fromCardanoValue . Cardano.valueFromList)
        (fromCardanoValue . Cardano.valueFromList . L.map (second negate))
    . L.partition ((>= 0) . snd)
    . Cardano.valueToList

unsafeIntCast
    :: (HasCallStack, Integral a, Integral b, Bits a, Bits b, Show a)
    => a
    -> b
unsafeIntCast x = fromMaybe err $ intCastMaybe x
  where
    err = error $ "unsafeIntCast failed for " <> show x

--------------------------------------------------------------------------------
-- updateTx
--------------------------------------------------------------------------------

-- | Describes modifications that can be made to a `Tx` using `updateTx`.
data TxUpdate = TxUpdate
    { extraInputs :: [(W.TxIn, W.TxOut)]
    , extraCollateral :: [W.TxIn]
       -- ^ Only used in the Alonzo era and later. Will be silently ignored in
       -- previous eras.
    , extraOutputs :: [W.TxOut]
    , extraInputScripts :: [CA.Script CA.KeyHash]
    , feeUpdate :: TxFeeUpdate
        -- ^ Set a new fee or use the old one.
    }

-- | For testing that
-- @
--   forall tx. updateTx noTxUpdate tx
--      == Right tx or Left
-- @
noTxUpdate :: TxUpdate
noTxUpdate = TxUpdate [] [] [] [] UseOldTxFee

-- | Method to use when updating the fee of a transaction.
data TxFeeUpdate
    = UseOldTxFee
        -- ^ Instead of updating the fee, just use the old fee of the
        -- Tx (no-op for fee update).
    | UseNewTxFee W.Coin
        -- ^ Specify a new fee to use instead.
    deriving (Eq, Show)

newtype ErrUpdateSealedTx
    = ErrExistingKeyWitnesses Int
    -- ^ The `SealedTx` could not be updated because the *n* existing
    -- key-witnesses would be rendered invalid.
    deriving (Generic, Eq, Show)

-- | Used to add inputs and outputs when balancing a transaction.
--
-- If the transaction contains existing key witnesses, it will return `Left`,
-- *even if `noTxUpdate` is used*. This last detail could be changed.
--
-- == Notes on implementation choices
--
-- We cannot rely on cardano-api here because `Cardano.TxBodyContent BuildTx`
-- cannot be extracted from an existing `TxBody`.
--
-- To avoid the need for `ledger -> wallet` conversions, this function can only
-- be used to *add* tx body content.
updateTx
    :: forall era. IsRecentEra era
    => Cardano.Tx era
    -> TxUpdate
    -> Either ErrUpdateSealedTx (Cardano.Tx era)
updateTx (Cardano.Tx body existingKeyWits) extraContent = do
    -- NOTE: The script witnesses are carried along with the cardano-api
    -- `anyEraBody`.
    body' <- modifyTxBody extraContent body

    if null existingKeyWits
       then Right $ Cardano.Tx body' mempty
       else Left $ ErrExistingKeyWitnesses $ length existingKeyWits
  where
    era = recentEra @era

    modifyTxBody
        :: TxUpdate
        -> Cardano.TxBody era
        -> Either ErrUpdateSealedTx (Cardano.TxBody era)
    modifyTxBody ebc = \case
        Cardano.ShelleyTxBody shelleyEra bod scripts scriptData aux val ->
            Right $ Cardano.ShelleyTxBody shelleyEra
                (modifyShelleyTxBody ebc era bod)
                (scripts ++ (flip toLedgerScript era
                    <$> extraInputScripts))
                scriptData
                aux
                val
        Byron.ByronTxBody _ -> case Cardano.shelleyBasedEra @era of {}

    TxUpdate _ _ _ extraInputScripts _ = extraContent

    toLedgerScript
        :: CA.Script CA.KeyHash
        -> RecentEra era
        -> Core.Script (ShelleyLedgerEra era)
    toLedgerScript s = \case
        RecentEraBabbage -> TimelockScript $ W.toLedgerTimelockScript s
        RecentEraConway -> TimelockScript $ W.toLedgerTimelockScript s


modifyShelleyTxBody
    :: forall era. TxUpdate
    -> RecentEra era
    -> TxBody (ShelleyLedgerEra era)
    -> TxBody (ShelleyLedgerEra era)
modifyShelleyTxBody txUpdate era = withConstraints era $
    over feeTxBodyL modifyFee
    . over outputsTxBodyL
        (<> extraOutputs')
    . over inputsTxBodyL
        (<> extraInputs')
    . over collateralInputsTxBodyL
        (<> extraCollateral')
  where
    TxUpdate extraInputs extraCollateral extraOutputs _ feeUpdate = txUpdate
    extraOutputs' = StrictSeq.fromList $ map (toLedgerTxOut era) extraOutputs
    extraInputs' = Set.fromList (W.toLedger . fst <$> extraInputs)
    extraCollateral' = Set.fromList $ W.toLedger <$> extraCollateral

    modifyFee old = case feeUpdate of
        UseNewTxFee c -> W.toLedger c
        UseOldTxFee -> old

--------------------------------------------------------------------------------
-- distributeSurplus
--------------------------------------------------------------------------------

-- | Indicates that it's impossible for 'distributeSurplus' to distribute a
-- surplus. As long as the surplus is larger than 'costOfIncreasingCoin', this
-- should never happen.
newtype ErrMoreSurplusNeeded = ErrMoreSurplusNeeded W.Coin
    deriving (Generic, Eq, Show)

-- | Small helper record to disambiguate between a fee and change Coin values.
-- Used by 'distributeSurplus'.
data TxFeeAndChange change = TxFeeAndChange
    { fee :: W.Coin
    , change :: change
    }
    deriving (Eq, Show)

-- | Manipulates a 'TxFeeAndChange' value.
--
mapTxFeeAndChange
    :: (W.Coin -> W.Coin)
    -- ^ A function to transform the fee
    -> (change1 -> change2)
    -- ^ A function to transform the change
    -> TxFeeAndChange change1
    -- ^ The original fee and change
    -> TxFeeAndChange change2
    -- ^ The transformed fee and change
mapTxFeeAndChange mapFee mapChange TxFeeAndChange {fee, change} =
    TxFeeAndChange (mapFee fee) (mapChange change)
-- | Calculate the cost of increasing a CBOR-encoded Coin-value by another Coin
-- with the lovelace/byte cost given by the 'FeePolicy'.
--
-- Outputs values in the range of [0, 8 * perByteFee]
--
-- >>> let p = FeePolicy (Quantity 0) (Quantity 44)
--
-- >>> costOfIncreasingCoin p 4294967295 1
-- Coin 176 -- (9 bytes - 5 bytes) * 44 lovelace/byte
--
-- >>> costOfIncreasingCoin p 0 4294967296
-- Coin 352 -- 8 bytes * 44 lovelace/byte
costOfIncreasingCoin
    :: FeePerByte
    -> W.Coin -- ^ Original coin
    -> W.Coin -- ^ Increment
    -> W.Coin
costOfIncreasingCoin (FeePerByte perByte) from delta =
    costOfCoin (from <> delta) `Coin.difference` costOfCoin from
  where
    costOfCoin = W.Coin . (perByte *) . unTxSize . sizeOfCoin

-- The maximum cost increase 'costOfIncreasingCoin' can return, which is the
-- cost of 8 bytes.
maximumCostOfIncreasingCoin :: FeePerByte -> W.Coin
maximumCostOfIncreasingCoin (FeePerByte perByte) = W.Coin $ 8 * perByte

-- | Calculate the size of a coin when encoded as CBOR.
sizeOfCoin :: W.Coin -> TxSize
sizeOfCoin (W.Coin c)
    | c >= 4_294_967_296 = TxSize 9 -- c >= 2^32
    | c >=        65_536 = TxSize 5 -- c >= 2^16
    | c >=           256 = TxSize 3 -- c >= 2^ 8
    | c >=            24 = TxSize 2
    | otherwise          = TxSize 1

-- | Distributes a surplus transaction balance between the given change
-- outputs and the given fee. This function is aware of the fact that
-- any increase in a 'Coin' value could increase the size and fee
-- requirement of a transaction.
--
-- When comparing the original fee and change outputs to the adjusted
-- fee and change outputs, this function guarantees that:
--
--    - The number of the change outputs remains constant;
--
--    - The fee quantity either remains the same or increases.
--
--    - For each change output:
--        - the ada quantity either remains constant or increases.
--        - non-ada quantities remain the same.
--
--    - The surplus is conserved:
--        The total increase in the fee and change ada quantities is
--        exactly equal to the surplus.
--
--    - Any increase in cost is covered:
--        If the total cost has increased by 𝛿c, then the fee value
--        will have increased by at least 𝛿c.
--
-- If the cost of distributing the provided surplus is greater than the
-- surplus itself, the function will return 'ErrMoreSurplusNeeded'. If
-- the provided surplus is greater or equal to
-- @maximumCostOfIncreasingCoin feePolicy@, the function will always
-- return 'Right'.
distributeSurplus
    :: FeePerByte
    -> W.Coin
    -- ^ Surplus transaction balance to distribute.
    -> TxFeeAndChange [W.TxOut]
    -- ^ Original fee and change outputs.
    -> Either ErrMoreSurplusNeeded (TxFeeAndChange [W.TxOut])
    -- ^ Adjusted fee and change outputs.
distributeSurplus feePolicy surplus fc@(TxFeeAndChange fee change) =
    distributeSurplusDelta feePolicy surplus
        (mapTxFeeAndChange id (fmap W.TxOut.coin) fc)
    <&> mapTxFeeAndChange
        (fee <>)
        (zipWith (flip W.TxOut.addCoin) change)

distributeSurplusDelta
    :: FeePerByte
    -> W.Coin
    -- ^ Surplus to distribute
    -> TxFeeAndChange [W.Coin]
    -> Either ErrMoreSurplusNeeded (TxFeeAndChange [W.Coin])
distributeSurplusDelta feePolicy surplus (TxFeeAndChange fee change) =
    case change of
        changeHead : changeTail ->
            distributeSurplusDeltaWithOneChangeCoin feePolicy surplus
                (TxFeeAndChange fee changeHead)
            <&> mapTxFeeAndChange id
                (: (W.Coin 0 <$ changeTail))
        [] ->
            burnSurplusAsFees feePolicy surplus
                (TxFeeAndChange fee ())
            <&> mapTxFeeAndChange id
                (\() -> [])

distributeSurplusDeltaWithOneChangeCoin
    :: FeePerByte
    -> W.Coin -- ^ Surplus to distribute
    -> TxFeeAndChange W.Coin
    -> Either ErrMoreSurplusNeeded (TxFeeAndChange W.Coin)
distributeSurplusDeltaWithOneChangeCoin
    feePolicy surplus fc@(TxFeeAndChange fee0 change0) =
    let
        -- We calculate the maximum possible fee increase, by assuming the
        -- **entire** surplus is added to the change.
        extraFee = findFixpointIncreasingFeeBy $
            costOfIncreasingCoin feePolicy change0 surplus
    in
        case surplus `Coin.subtract` extraFee of
            Just extraChange ->
                Right $ TxFeeAndChange
                    { fee = extraFee
                    , change = extraChange
                    }
            Nothing ->
                -- The fee increase from adding the surplus to the change was
                -- greater than the surplus itself. This could happen if the
                -- surplus is small.
                burnSurplusAsFees feePolicy surplus
                    (mapTxFeeAndChange id (const ()) fc)
                        <&> mapTxFeeAndChange id (\() -> W.Coin 0)
  where
    -- Increasing the fee may itself increase the fee. If that is the case, this
    -- function will increase the fee further. The process repeats until the fee
    -- doesn't need to be increased.
    --
    -- The function will always converge because the result of
    -- 'costOfIncreasingCoin' is bounded to @8 * feePerByte@.
    --
    -- On mainnet it seems unlikely that the function would recurse more than
    -- one time, and certainly not more than twice. If the protocol parameters
    -- are updated to allow for slightly more expensive txs, it might be
    -- possible to hit the boundary at ≈4 ada where the fee would need 9 bytes
    -- rather than 5. This is already the largest boundary.
    --
    -- Note that both the argument and the result of this function are increases
    -- relative to 'fee0'.
    --
    -- == Example ==
    --
    -- In this more extreme example the fee is increased from increasing the fee
    -- itself:
    --
    -- @@
    --     let fee0 = 23
    --     let feePolicy = -- 300 lovelace / byte
    --
    --     findFixpointIncreasingFeeBy 1 = go 0 1
    --     -- Recurse:
    --     = go (0 + 1) (costOfIncreasingCoin feePolicy (23 + 0) 1)
    --     = go (0 + 1) 300
    --     -- Recurse:
    --     = go (1 + 300) (costOfIncreasingCoin feePolicy (23 + 1) 300)
    --     = go 301 300
    --     = go (301 + 300) (costOfIncreasingCoin feePolicy (23 + 301) 300)
    --     = go (301 + 300) 0
    --     = go 601 0
    --     = 601
    -- @@
    findFixpointIncreasingFeeBy = go mempty
      where
        go :: W.Coin -> W.Coin -> W.Coin
        go c (W.Coin 0) = c
        go c increase = go
            (c <> increase)
            (costOfIncreasingCoin feePolicy (c <> fee0) increase)

burnSurplusAsFees
    :: FeePerByte
    -> W.Coin -- Surplus
    -> TxFeeAndChange ()
    -> Either ErrMoreSurplusNeeded (TxFeeAndChange ())
burnSurplusAsFees feePolicy surplus (TxFeeAndChange fee0 ())
    | shortfall > W.Coin 0 =
        Left $ ErrMoreSurplusNeeded shortfall
    | otherwise =
        Right $ TxFeeAndChange surplus ()
  where
    costOfBurningSurplus = costOfIncreasingCoin feePolicy fee0 surplus
    shortfall = costOfBurningSurplus `Coin.difference` surplus

toLedgerTxOut
    :: HasCallStack
    => RecentEra era
    -> W.TxOut
    -> TxOut (ShelleyLedgerEra era)
toLedgerTxOut txOutEra txOut =
    case txOutEra of
        RecentEraBabbage -> W.toBabbageTxOut txOut
        RecentEraConway -> W.toConwayTxOut txOut

toWalletTxOut
    :: RecentEra era
    -> TxOut (ShelleyLedgerEra era)
    -> W.TxOut
toWalletTxOut RecentEraBabbage = W.fromBabbageTxOut
toWalletTxOut RecentEraConway = W.fromConwayTxOut

--------------------------------------------------------------------------------
-- Validation of transaction outputs
--------------------------------------------------------------------------------

-- | A validation error for a user-specified transaction output.
--
data ErrBalanceTxOutputError = ErrBalanceTxOutputErrorOf
    { outputIndex :: Int
    , outputErrorInfo :: ErrBalanceTxOutputErrorInfo
    }
    deriving (Eq, Show)

data ErrBalanceTxOutputErrorInfo
    = ErrBalanceTxOutputAdaQuantityInsufficient
        ErrBalanceTxOutputAdaQuantityInsufficientError
    | ErrBalanceTxOutputSizeExceedsLimit
        ErrBalanceTxOutputSizeExceedsLimitError
    | ErrBalanceTxOutputTokenQuantityExceedsLimit
        ErrBalanceTxOutputTokenQuantityExceedsLimitError
    deriving (Eq, Show)

data ErrBalanceTxOutputAdaQuantityInsufficientError =
    ErrBalanceTxOutputAdaQuantityInsufficientError
    { minimumExpectedCoin :: W.Coin
    , output :: (W.Address, TokenBundle)
    }
    deriving (Eq, Generic, Show)

newtype ErrBalanceTxOutputSizeExceedsLimitError =
    ErrBalanceTxOutputSizeExceedsLimitError
    { outputThatExceedsLimit :: (W.Address, TokenBundle)
    }
    deriving (Eq, Generic, Show)

data ErrBalanceTxOutputTokenQuantityExceedsLimitError =
    ErrBalanceTxOutputTokenQuantityExceedsLimitError
    { address :: W.Address
      -- ^ The address to which this token quantity was to be sent.
    , asset :: AssetId
      -- ^ The asset identifier to which this token quantity corresponds.
    , quantity :: TokenQuantity
      -- ^ The token quantity that exceeded the bound.
    , quantityMaxBound :: TokenQuantity
      -- ^ The maximum allowable token quantity.
    }
    deriving (Eq, Generic, Show)

-- | Validates the given transaction outputs.
--
validateTxOutputs
    :: SelectionConstraints
    -> [(W.Address, TokenBundle)]
    -> Either ErrBalanceTxOutputError ()
validateTxOutputs constraints outs =
    -- If we encounter an error, just report the first error we encounter:
    case errors of
        e : _ -> Left e
        []    -> pure ()
  where
    errors :: [ErrBalanceTxOutputError]
    errors = uncurry ErrBalanceTxOutputErrorOf <$> foldMap withOutputsIndexed
        [ (fmap . fmap) ErrBalanceTxOutputSizeExceedsLimit
            . mapMaybe (traverse (validateTxOutputSize constraints))
        , (fmap . fmap) ErrBalanceTxOutputTokenQuantityExceedsLimit
            . foldMap (traverse validateTxOutputTokenQuantities)
        , (fmap . fmap) ErrBalanceTxOutputAdaQuantityInsufficient
            . mapMaybe (traverse (validateTxOutputAdaQuantity constraints))
        ]
      where
        withOutputsIndexed f = f $ zip [0 ..] outs

-- | Validates the size of a transaction output.
--
-- Returns an error if (and only if) the size exceeds the limit defined by the
-- protocol.
--
validateTxOutputSize
    :: SelectionConstraints
    -> (W.Address, TokenBundle)
    -> Maybe ErrBalanceTxOutputSizeExceedsLimitError
validateTxOutputSize cs out = case sizeAssessment of
    TokenBundleSizeWithinLimit ->
        Nothing
    TokenBundleSizeExceedsLimit ->
        Just $ ErrBalanceTxOutputSizeExceedsLimitError out
  where
    sizeAssessment :: TokenBundleSizeAssessment
    sizeAssessment = (cs ^. #assessTokenBundleSize) (snd out)

-- | Validates the token quantities of a transaction output.
--
-- Returns a list of token quantities that exceed the limit defined by the
-- protocol.
--
validateTxOutputTokenQuantities
    :: (W.Address, TokenBundle)
    -> [ErrBalanceTxOutputTokenQuantityExceedsLimitError]
validateTxOutputTokenQuantities out =
    [ ErrBalanceTxOutputTokenQuantityExceedsLimitError
        {address, asset, quantity, quantityMaxBound = txOutMaxTokenQuantity}
    | let address = fst out
    , (asset, quantity) <- TokenMap.toFlatList $ (snd out) ^. #tokens
    , quantity > txOutMaxTokenQuantity
    ]

-- | Validates the ada quantity associated with a transaction output.
--
-- An output's ada quantity must be greater than or equal to the minimum
-- required quantity for that output.
--
validateTxOutputAdaQuantity
    :: SelectionConstraints
    -> (W.Address, TokenBundle)
    -> Maybe ErrBalanceTxOutputAdaQuantityInsufficientError
validateTxOutputAdaQuantity constraints output
    | isBelowMinimum =
        Just ErrBalanceTxOutputAdaQuantityInsufficientError
            {minimumExpectedCoin, output}
    | otherwise =
        Nothing
  where
    isBelowMinimum :: Bool
    isBelowMinimum = uncurry (constraints ^. #isBelowMinimumAdaQuantity) output

    minimumExpectedCoin :: W.Coin
    minimumExpectedCoin =
        (constraints ^. #computeMinimumAdaQuantity)
        (fst output)
        (snd output ^. #tokens)
