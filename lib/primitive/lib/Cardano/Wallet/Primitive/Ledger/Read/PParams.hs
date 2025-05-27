{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2024 Cardano Foundation
-- License: Apache-2.0
--
-- Convert from "Cardano.Wallet.Read" to primitive types.
module Cardano.Wallet.Primitive.Ledger.Read.PParams
    ( primitiveProtocolParameters
    , optimumNumberOfPools
    )
where

import Prelude

import Cardano.Ledger.Alonzo.Scripts
    ( Prices (..)
    )
import Cardano.Ledger.Api
    ( AlonzoEraPParams
    , EraPParams
    , PParams
    , ProtVerAtMost
    , ppCollateralPercentageL
    , ppDL
    , ppKeyDepositL
    , ppMaxCollateralInputsL
    , ppMaxTxExUnitsL
    , ppMaxTxSizeL
    , ppMaxValSizeL
    , ppMinFeeAL
    , ppMinFeeBL
    , ppNOptL
    , ppPricesL
    )
import Cardano.Ledger.Coin
    ( coinToRational
    )
import Cardano.Read.Ledger.Eras.KnownEras
    ( IsEra
    , theEra
    )
import Cardano.Wallet.Primitive.Ledger.Byron
    ( maryTokenBundleMaxSize
    , protocolParametersFromPP
    )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..)
    , unsafeFromIntegral
    )
import Cardano.Wallet.Primitive.Types.DecentralizationLevel
    ( DecentralizationLevel
    , fromFederationPercentage
    )
import Cardano.Wallet.Primitive.Types.EpochNo
    ( EpochNo (..)
    )
import Cardano.Wallet.Primitive.Types.EraInfo
    ( EraInfo (..)
    )
import Cardano.Wallet.Primitive.Types.ExecutionUnitPrices
    ( ExecutionUnitPrices (..)
    )
import Cardano.Wallet.Primitive.Types.FeePolicy
    ( FeePolicy (LinearFee)
    , LinearFunction (LinearFunction, intercept, slope)
    )
import Cardano.Wallet.Primitive.Types.ProtocolParameters
    ( ProtocolParameters (..)
    )
import Cardano.Wallet.Primitive.Types.TokenBundleMaxSize
    ( TokenBundleMaxSize (TokenBundleMaxSize)
    )
import Cardano.Wallet.Primitive.Types.Tx.Constraints
    ( TxSize (TxSize)
    )
import Cardano.Wallet.Primitive.Types.TxParameters
    ( ExecutionUnits (..)
    , TxParameters (..)
    )
import Cardano.Wallet.Unsafe
    ( unsafeIntToWord
    )
import Control.Lens
    ( view
    , (&)
    , (^.)
    )
import Data.IntCast
    ( intCast
    , intCastMaybe
    )
import Data.Maybe
    ( fromMaybe
    )
import Data.Percentage
    ( Percentage
    )
import Data.Quantity
    ( Quantity (..)
    )
import Data.Word
    ( Word16
    , Word32
    )
import GHC.Stack
    ( HasCallStack
    )
import Numeric.Natural
    ( Natural
    )
import Ouroboros.Consensus.HardFork.History.Summary
    ( Bound (..)
    )

import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import qualified Cardano.Ledger.BaseTypes as SL
import qualified Cardano.Wallet.Read as Read
import qualified Data.Percentage as Percentage

{-----------------------------------------------------------------------------
    Protocol Parameters
------------------------------------------------------------------------------}

-- | Compute wallet primitive 'ProtocolParameters' from ledger 'PParams'.
primitiveProtocolParameters
    :: forall era
     . Read.IsEra era
    => EraInfo Bound
    -> Read.PParams era
    -> ProtocolParameters
primitiveProtocolParameters eraBounds (Read.PParams pparams) =
    case Read.theEra :: Read.Era era of
        Read.Byron -> protocolParametersFromPP eraBounds pparams
        Read.Shelley -> fromLedger eraBounds pparams
        Read.Allegra -> fromLedger eraBounds pparams
        Read.Mary -> fromLedger eraBounds pparams
        Read.Alonzo -> fromLedger eraBounds pparams
        Read.Babbage -> fromLedger eraBounds pparams
        Read.Conway -> fromLedger eraBounds pparams

fromUnitInterval :: HasCallStack => SL.UnitInterval -> Percentage
fromUnitInterval x =
    either bomb id
        . Percentage.fromRational
        . toRational
        . SL.unboundRational
        $ x
  where
    bomb =
        error
            $ "fromUnitInterval: encountered invalid parameter value: " <> show x

-- | Extract the current network decentralization level from the given set of
-- protocol parameters.
fromLedgerExUnits
    :: Alonzo.ExUnits
    -> ExecutionUnits
fromLedgerExUnits (Alonzo.ExUnits mem steps) =
    ExecutionUnits
        { executionSteps = steps
        , executionMemory = mem
        }

txParametersFromPParams
    :: EraPParams era
    => TokenBundleMaxSize
    -> ExecutionUnits
    -> PParams era
    -> TxParameters
txParametersFromPParams maxBundleSize getMaxExecutionUnits pp =
    TxParameters
        { getFeePolicy =
            LinearFee
                $ LinearFunction
                    { intercept = coinToDouble (pp ^. ppMinFeeBL)
                    , slope = coinToDouble (pp ^. ppMinFeeAL)
                    }
        , getTxMaxSize = fromMaxSize $ pp ^. ppMaxTxSizeL
        , getTokenBundleMaxSize = maxBundleSize
        , getMaxExecutionUnits
        }
  where
    coinToDouble :: Read.Coin -> Double
    coinToDouble = fromRational . coinToRational
    -- NOTE: Unsafe conversion from Natural -> Word16
    fromMaxSize :: Word32 -> Quantity "byte" Word16
    fromMaxSize = Quantity . fromIntegral

desiredNumberOfStakePoolsFromPParams
    :: (HasCallStack, EraPParams era) => PParams era -> Word16
desiredNumberOfStakePoolsFromPParams pp =
    intCastMaybe (pp ^. ppNOptL)
        & fromMaybe (error "Desired number of stake pools exceeds 2^16")

stakeKeyDepositFromPParams
    :: EraPParams era => PParams era -> Coin
stakeKeyDepositFromPParams = toWalletCoin . view ppKeyDepositL
  where
    toWalletCoin :: HasCallStack => Read.Coin -> Coin
    toWalletCoin (Read.CoinC c) = unsafeFromIntegral c

decentralizationLevelF
    :: forall era
     . (HasCallStack, EraPParams era, IsEra era)
    => PParams era
    -> DecentralizationLevel
decentralizationLevelF pp = case theEra @era of
    Read.Shelley -> decentralizationLevelFromPParams pp
    Read.Allegra -> decentralizationLevelFromPParams pp
    Read.Mary -> decentralizationLevelFromPParams pp
    Read.Alonzo -> decentralizationLevelFromPParams pp
    Read.Babbage -> fromFederationPercentage $ Percentage.fromRationalClipped 0
    Read.Conway -> fromFederationPercentage $ Percentage.fromRationalClipped 0
    _ -> error "ByronEra does not have decentralization level"
  where
    decentralizationLevelFromPParams
        :: forall shelleyEra
         . (EraPParams shelleyEra, ProtVerAtMost shelleyEra 6)
        => PParams shelleyEra
        -> DecentralizationLevel
    decentralizationLevelFromPParams shelleyPP =
        fromFederationPercentage $ fromUnitInterval $ shelleyPP ^. ppDL

txParametersF
    :: forall era
     . (IsEra era, EraPParams era)
    => PParams era
    -> TxParameters
txParametersF pp = case theEra @era of
    Read.Shelley -> shelleyTxParametersFromPParams
    Read.Allegra -> shelleyTxParametersFromPParams
    Read.Mary -> shelleyTxParametersFromPParams
    Read.Alonzo -> alonzoTxParametersFromPParams pp
    Read.Babbage -> alonzoTxParametersFromPParams pp
    Read.Conway -> alonzoTxParametersFromPParams pp
    _ -> error "ByronEra does not have tx parameters"
  where
    alonzoTxParametersFromPParams
        :: forall alonzoEra
         . (AlonzoEraPParams alonzoEra)
        => PParams alonzoEra
        -> TxParameters
    alonzoTxParametersFromPParams alonzoPP =
        txParametersFromPParams
            (TokenBundleMaxSize $ TxSize $ alonzoPP ^. ppMaxValSizeL)
            (fromLedgerExUnits (alonzoPP ^. ppMaxTxExUnitsL))
            alonzoPP
    shelleyTxParametersFromPParams
        :: TxParameters
    shelleyTxParametersFromPParams =
        txParametersFromPParams
            maryTokenBundleMaxSize
            (ExecutionUnits{executionSteps = 0, executionMemory = 0})
            pp

desiredNumberOfStakePoolsF
    :: forall era. (EraPParams era, IsEra era) => PParams era -> Word16
desiredNumberOfStakePoolsF pp = case theEra @era of
    Read.Shelley -> desiredNumberOfStakePoolsFromPParams pp
    Read.Allegra -> desiredNumberOfStakePoolsFromPParams pp
    Read.Mary -> desiredNumberOfStakePoolsFromPParams pp
    Read.Alonzo -> desiredNumberOfStakePoolsFromPParams pp
    Read.Babbage -> desiredNumberOfStakePoolsFromPParams pp
    Read.Conway -> desiredNumberOfStakePoolsFromPParams pp
    _ -> error "ByronEra does not have desired number of stake pools"

stakeKeyDepositF
    :: forall era
     . (EraPParams era, IsEra era)
    => PParams era
    -> Coin
stakeKeyDepositF pp = case theEra @era of
    Read.Shelley -> stakeKeyDepositFromPParams pp
    Read.Allegra -> stakeKeyDepositFromPParams pp
    Read.Mary -> stakeKeyDepositFromPParams pp
    Read.Alonzo -> stakeKeyDepositFromPParams pp
    Read.Babbage -> stakeKeyDepositFromPParams pp
    Read.Conway -> stakeKeyDepositFromPParams pp
    _ -> error "ByronEra does not have stake key deposit"

maximumCollateralInputCountF
    :: forall era
     . IsEra era
    => PParams era
    -> Word16
maximumCollateralInputCountF pp = case theEra @era of
    Read.Shelley -> 0
    Read.Allegra -> 0
    Read.Mary -> 0
    Read.Alonzo -> unsafeIntToWord $ pp ^. ppMaxCollateralInputsL
    Read.Babbage -> unsafeIntToWord $ pp ^. ppMaxCollateralInputsL
    Read.Conway ->
        intCastMaybe (pp ^. ppMaxCollateralInputsL)
            & fromMaybe (error "Maximum count of collateral inputs exceeds 2^16")
    _ -> error "ByronEra does not have maximum collateral input count"

minimumCollateralPercentageF
    :: forall era
     . IsEra era
    => PParams era
    -> Natural
minimumCollateralPercentageF pp = case theEra @era of
    Read.Shelley -> 0
    Read.Allegra -> 0
    Read.Mary -> 0
    Read.Alonzo -> pp ^. ppCollateralPercentageL
    Read.Babbage -> pp ^. ppCollateralPercentageL
    Read.Conway -> pp ^. ppCollateralPercentageL
    _ -> error "ByronEra does not have minimum collateral percentage"

executionUnitPricesF
    :: forall era
     . IsEra era
    => PParams era
    -> Maybe ExecutionUnitPrices
executionUnitPricesF pp = case theEra @era of
    Read.Shelley -> Nothing
    Read.Allegra -> Nothing
    Read.Mary -> Nothing
    Read.Alonzo -> Just $ executionUnitPricesFromPParams pp
    Read.Babbage -> Just $ executionUnitPricesFromPParams pp
    Read.Conway -> Just $ executionUnitPricesFromPParams pp
    _ -> error "ByronEra does not have execution unit prices"
  where
    executionUnitPricesFromPParams
        :: AlonzoEraPParams era
        => PParams era
        -> ExecutionUnitPrices
    executionUnitPricesFromPParams alonzoPP = fromAlonzoPrices (alonzoPP ^. ppPricesL)
    fromAlonzoPrices Alonzo.Prices{prMem, prSteps} =
        ExecutionUnitPrices
            { pricePerStep = SL.unboundRational prSteps
            , pricePerMemoryUnit = SL.unboundRational prMem
            }
fromLedger
    :: forall era
     . (IsEra era, EraPParams era)
    => EraInfo Bound
    -> PParams era
    -> ProtocolParameters
fromLedger eraInfo pp =
    ProtocolParameters
        { decentralizationLevel = decentralizationLevelF pp
        , txParameters = txParametersF pp
        , desiredNumberOfStakePools = desiredNumberOfStakePoolsF pp
        , stakeKeyDeposit = stakeKeyDepositF pp
        , eras = fromBoundToEpochNo <$> eraInfo
        , -- Collateral inputs were not supported or required in Shelley:
          maximumCollateralInputCount = maximumCollateralInputCountF pp
        , minimumCollateralPercentage = minimumCollateralPercentageF pp
        , executionUnitPrices = executionUnitPricesF pp
        }

fromBoundToEpochNo :: Bound -> EpochNo
fromBoundToEpochNo (Bound _relTime _slotNo (SL.EpochNo e)) =
    EpochNo $ fromIntegral e

optimumNumberOfPools
    :: (EraPParams era, IsEra era) => PParams era -> Int
optimumNumberOfPools = intCast . desiredNumberOfStakePoolsF
