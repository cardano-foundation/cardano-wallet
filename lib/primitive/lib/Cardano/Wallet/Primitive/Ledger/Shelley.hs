{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- Orphan instances for {Encode,Decode}Address until we get rid of the
-- Jörmungandr dual support.
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Copyright: © 2020 IOHK
-- License: Apache-2.0
--
-- Conversion functions and static chain settings for Shelley.
module Cardano.Wallet.Primitive.Ledger.Shelley
    ( CardanoBlock
    , StandardCrypto
    , StandardShelley

      -- * Protocol Parameters
    , NetworkId (..)
    , NodeToClientVersionData
    , nodeToClientVersions

      -- * Genesis
    , emptyGenesis

      -- * Eras
    , AnyCardanoEra (..)
    , CardanoEra (..)
    , ShelleyBasedEra (..)

      -- * Conversions
    , toCardanoHash
    , unsealShelleyTx
    , toPoint
    , fromPoint
    , toCardanoTxId
    , toCardanoTxIn
    , fromCardanoTxIn
    , fromCardanoTxOut
    , fromCardanoWdrls
    , cardanoCertKeysForWitnesses
    , toCardanoTxOut
    , toCardanoLovelace
    , toStakeKeyRegCert
    , toStakeKeyDeregCert
    , toStakePoolDlgCert
    , toLedgerStakeCredential
    , fromStakeCredential
    , toShelleyCoin
    , toCardanoStakeCredential
    , toCardanoValue
    , fromCardanoValue
    , fromCardanoLovelace
    , rewardAccountFromAddress
    , fromShelleyPParams
    , fromAllegraPParams
    , fromMaryPParams
    , fromAlonzoPParams
    , fromBabbagePParams
    , fromConwayPParams
    , fromLedgerExUnits
    , fromCardanoAddress
    , fromShelleyTxIn
    , toCardanoPolicyId
    , toCardanoSimpleScript

      -- ** Stake pools
    , fromPoolId
    , fromPoolDistr
    , fromNonMyopicMemberRewards
    , optimumNumberOfPools
    , getProducer
    , fromBlockNo
    , toCardanoEra
    , fromShelleyTxOut
    , fromGenesisData
    , fromTip
    , fromTip'
    , toTip
    , slottingParametersFromGenesis
    , getBabbageProducer
    , getConwayProducer

      -- * Internal Conversions
    , decentralizationLevelFromPParams

      -- * Utilities
    , invertUnitInterval
    , interval0
    , interval1
    , numberOfTransactionsInBlock
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPub
    , xpubPublicKey
    )
import Cardano.Address.Script
    ( KeyHash (..)
    , Script (..)
    )
import Cardano.Api
    ( AllegraEra
    , AlonzoEra
    , AnyCardanoEra (..)
    , AsType (..)
    , BabbageEra
    , CardanoEra (..)
    , CardanoMode
    , ConwayEra
    , EraInMode (..)
    , InAnyCardanoEra (..)
    , IsCardanoEra (..)
    , MaryEra
    , NetworkId
    , ShelleyEra
    , TxInMode (..)
    )
import Cardano.Api.Shelley
    ( ShelleyBasedEra (..)
    , ShelleyGenesis (..)
    )
import Cardano.Chain.Block
    ( ABlockOrBoundary (ABOBBlock, ABOBBoundary)
    , blockTxPayload
    )
import Cardano.Chain.UTxO
    ( unTxPayload
    )
import Cardano.Crypto.Hash.Class
    ( Hash (UnsafeHash)
    , hashToBytes
    )
import Cardano.Ledger.Api
    ( ppCollateralPercentageL
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
import Cardano.Ledger.BaseTypes
    ( strictMaybeToMaybe
    , urlToText
    )
import Cardano.Ledger.Binary
    ( EncCBORGroup
    )
import Cardano.Ledger.Era
    ( Era (..)
    , TxSeq
    )
import Cardano.Ledger.PoolParams
    ( PoolMetadata (..)
    , PoolParams (..)
    )
import Cardano.Ledger.Shelley.Genesis
    ( fromNominalDiffTimeMicro
    )
import Cardano.Slotting.Slot
    ( EpochNo (..)
    , EpochSize (..)
    )
import Cardano.Wallet.Primitive.Ledger.Byron
    ( maryTokenBundleMaxSize
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Byron
    ( fromTxAux
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Certificates
    ( fromStakeCredential
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Inputs
    ( fromShelleyTxIn
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Outputs
    ( fromCardanoValue
    , fromShelleyAddress
    , fromShelleyTxOut
    )
import Cardano.Wallet.Primitive.Types.Block
    ( ChainPoint (..)
    )
import Cardano.Wallet.Primitive.Types.Certificates
    ( PoolCertificate
    , PoolRegistrationCertificate (..)
    )
import Cardano.Wallet.Primitive.Types.Pool
    ( PoolId (..)
    , PoolOwner (..)
    )
import Cardano.Wallet.Primitive.Types.StakePoolMetadata
    ( StakePoolMetadataHash (..)
    , StakePoolMetadataUrl (..)
    )
import Cardano.Wallet.Read.Tx.Hash
    ( fromShelleyTxId
    )
import Cardano.Wallet.Unsafe
    ( unsafeIntToWord
    , unsafeMkPercentage
    )
import Cardano.Wallet.Util
    ( internalError
    , tina
    )
import Control.Lens
    ( view
    , (&)
    , (^.)
    )
import Cryptography.Hash.Extra
    ( blake2b224
    )
import Data.Bifunctor
    ( bimap
    )
import Data.ByteString
    ( ByteString
    )
import Data.ByteString.Short
    ( fromShort
    , toShort
    )
import Data.Coerce
    ( coerce
    )
import Data.Either.Extra
    ( eitherToMaybe
    )
import Data.IntCast
    ( intCast
    , intCastMaybe
    )
import Data.Map.Strict
    ( Map
    )
import Data.Maybe
    ( fromMaybe
    , mapMaybe
    )
import Data.Percentage
    ( Percentage
    )
import Data.Quantity
    ( Quantity (..)
    )
import Data.Type.Equality
    ( (:~:) (..)
    , testEquality
    )
import Data.Word
    ( Word16
    , Word32
    )
import Fmt
    ( Buildable (..)
    , Builder
    , (+|)
    , (+||)
    , (||+)
    )
import GHC.Stack
    ( HasCallStack
    )
import Numeric.Natural
    ( Natural
    )
import Ouroboros.Consensus.Byron.Ledger
    ( byronBlockRaw
    )
import Ouroboros.Consensus.Cardano.Block
    ( CardanoBlock
    , CardanoEras
    , HardForkBlock (..)
    , StandardAllegra
    , StandardAlonzo
    , StandardBabbage
    , StandardConway
    , StandardMary
    , StandardShelley
    )
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras
    ( OneEraHash (..)
    )
import Ouroboros.Consensus.HardFork.History.Summary
    ( Bound (..)
    )
import Ouroboros.Consensus.Shelley.Eras
    ( StandardCrypto
    )
import Ouroboros.Consensus.Shelley.Ledger.Block
    ( ShelleyBlock (..)
    )
import Ouroboros.Network.Block
    ( BlockNo (..)
    , Point (..)
    , Tip (..)
    , getTipPoint
    )
import Ouroboros.Network.NodeToClient
    ( ConnectionId (..)
    , LocalAddress (..)
    , NodeToClientVersion (..)
    , NodeToClientVersionData
    )
import Ouroboros.Network.Point
    ( WithOrigin (..)
    )

import qualified Cardano.Api as Cardano
import qualified Cardano.Api.Shelley as Cardano
import qualified Cardano.Crypto.Hash as Crypto
import qualified Cardano.Ledger.Address as SL
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import qualified Cardano.Ledger.Alonzo.TxSeq as Alonzo
import qualified Cardano.Ledger.Api as Ledger
import qualified Cardano.Ledger.Babbage as Babbage
import qualified Cardano.Ledger.BaseTypes as SL
import qualified Cardano.Ledger.Coin as Ledger
import qualified Cardano.Ledger.Conway as Conway
import qualified Cardano.Ledger.Credential as SL
import qualified Cardano.Ledger.Crypto as SL
import qualified Cardano.Ledger.Shelley.API as SL
import qualified Cardano.Ledger.Shelley.API as SLAPI
import qualified Cardano.Ledger.Shelley.BlockChain as SL
import qualified Cardano.Protocol.TPraos.BHeader as SL
import qualified Cardano.Slotting.Slot as Slotting
import qualified Cardano.Wallet.Primitive.Ledger.Convert as Ledger
import qualified Cardano.Wallet.Primitive.Slotting as W
import qualified Cardano.Wallet.Primitive.Types.Address as W
import qualified Cardano.Wallet.Primitive.Types.AssetId as W
import qualified Cardano.Wallet.Primitive.Types.AssetName as W
import qualified Cardano.Wallet.Primitive.Types.Block as W
import qualified Cardano.Wallet.Primitive.Types.Certificates as W
import qualified Cardano.Wallet.Primitive.Types.Coin as Coin
import qualified Cardano.Wallet.Primitive.Types.Coin as W
import qualified Cardano.Wallet.Primitive.Types.DecentralizationLevel as W
import qualified Cardano.Wallet.Primitive.Types.EpochNo as W
import qualified Cardano.Wallet.Primitive.Types.EraInfo as W
import qualified Cardano.Wallet.Primitive.Types.ExecutionUnitPrices as W
import qualified Cardano.Wallet.Primitive.Types.FeePolicy as W
import qualified Cardano.Wallet.Primitive.Types.GenesisParameters as W
import qualified Cardano.Wallet.Primitive.Types.Hash as W
import qualified Cardano.Wallet.Primitive.Types.NetworkParameters as W
import qualified Cardano.Wallet.Primitive.Types.ProtocolParameters as W
import qualified Cardano.Wallet.Primitive.Types.RewardAccount as W
import qualified Cardano.Wallet.Primitive.Types.SlottingParameters as W
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.TokenBundleMaxSize as W
import qualified Cardano.Wallet.Primitive.Types.TokenPolicyId as W
import qualified Cardano.Wallet.Primitive.Types.TokenQuantity as W
import qualified Cardano.Wallet.Primitive.Types.Tx.Constraints as W
import qualified Cardano.Wallet.Primitive.Types.Tx.SealedTx as W
    ( SealedTx
    , cardanoTxIdeallyNoLaterThan
    )
import qualified Cardano.Wallet.Primitive.Types.Tx.Tx as W
    ( Tx (..)
    )
import qualified Cardano.Wallet.Primitive.Types.Tx.TxIn as W
    ( TxIn (TxIn)
    )
import qualified Cardano.Wallet.Primitive.Types.Tx.TxOut as W
    ( TxOut (TxOut)
    )
import qualified Cardano.Wallet.Primitive.Types.TxParameters as W
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as SBS
import qualified Data.ListMap as ListMap
import qualified Data.Map.Strict as Map
import qualified Data.Percentage as Percentage
import qualified Data.Set as Set
import qualified Ouroboros.Consensus.Protocol.Praos as Consensus
import qualified Ouroboros.Consensus.Protocol.Praos.Header as Consensus
import qualified Ouroboros.Consensus.Protocol.TPraos as Consensus
import qualified Ouroboros.Consensus.Shelley.Ledger as O
import qualified Ouroboros.Network.Block as O
import qualified Ouroboros.Network.Point as Point

--------------------------------------------------------------------------------
--
-- Chain Parameters

-- NOTE
-- For MainNet and TestNet, we can get away with empty genesis blocks with
-- the following assumption:
--
-- - Users won't ever restore a wallet that has genesis UTxO.
--
-- This assumption is _true_ for any user using HD wallets (sequential or
-- random) which means, any user of cardano-wallet.
emptyGenesis :: W.GenesisParameters -> W.Block
emptyGenesis gp = W.Block
    { transactions = []
    , delegations  = []
    , header = W.BlockHeader
        { slotNo =
            Slotting.SlotNo 0
        , blockHeight =
            Quantity 0
        , headerHash =
            coerce $ W.getGenesisBlockHash gp
        , parentHeaderHash =
            Nothing
        }
    }

--------------------------------------------------------------------------------
--
-- Network Parameters

-- | The protocol client version. Distinct from the codecs version.
nodeToClientVersions :: [NodeToClientVersion]
nodeToClientVersions = [NodeToClientV_13]

--------------------------------------------------------------------------------
--
-- Type Conversions

toCardanoHash :: W.Hash "BlockHeader" -> OneEraHash (CardanoEras sc)
toCardanoHash (W.Hash bytes) =
    OneEraHash $ toShort bytes

toPoint :: W.ChainPoint -> O.Point (CardanoBlock sc)
toPoint ChainPointAtGenesis = O.GenesisPoint
toPoint (ChainPoint slot h) = O.BlockPoint slot (toCardanoHash h)

fromPoint :: O.Point (CardanoBlock sc) -> W.ChainPoint
fromPoint O.GenesisPoint = ChainPointAtGenesis
fromPoint (O.BlockPoint slot h) = ChainPoint slot (fromCardanoHash h)

getProducer
    :: (Era era, EncCBORGroup (TxSeq era))
    => ShelleyBlock (Consensus.TPraos StandardCrypto) era -> PoolId
getProducer (ShelleyBlock (SL.Block (SL.BHeader header _) _) _) =
    fromPoolKeyHash $ SL.hashKey (SL.bheaderVk header)

getBabbageProducer
    :: (Era era, EncCBORGroup (TxSeq era))
    => ShelleyBlock (Consensus.Praos StandardCrypto) era -> PoolId
getBabbageProducer (ShelleyBlock (SL.Block (Consensus.Header header _) _) _) =
    fromPoolKeyHash $ SL.hashKey (Consensus.hbVk header)

getConwayProducer
    :: (Era era, EncCBORGroup (TxSeq era))
    => ShelleyBlock (Consensus.Praos StandardCrypto) era -> PoolId
getConwayProducer (ShelleyBlock (SL.Block (Consensus.Header header _) _) _) =
    fromPoolKeyHash $ SL.hashKey (Consensus.hbVk header)

numberOfTransactionsInBlock
    :: CardanoBlock StandardCrypto -> (Int, (Quantity "block" Word32, O.SlotNo))
numberOfTransactionsInBlock = \case
    BlockByron byb -> transactionsByron byb
    BlockShelley shb -> transactions shb
    BlockAllegra shb -> transactions shb
    BlockMary shb -> transactions shb
    BlockAlonzo shb -> transactionsAlonzo shb
    BlockBabbage shb -> transactionsBabbage shb
    BlockConway shb -> transactionsConway shb
  where
    transactions
        (ShelleyBlock
            (SL.Block (SL.BHeader header _) (SL.ShelleyTxSeq txs'))
            _
        ) =
            ( length txs'
            , (fromBlockNo $ SL.bheaderBlockNo header, SL.bheaderSlotNo header)
            )
    transactionsAlonzo
        (ShelleyBlock
            (SL.Block (SL.BHeader header _) (Alonzo.AlonzoTxSeq txs'))
            _
        ) =
            ( length txs'
            , (fromBlockNo $ SL.bheaderBlockNo header, SL.bheaderSlotNo header)
            )
    transactionsBabbage
        :: ShelleyBlock
            (Consensus.Praos StandardCrypto)
            (Babbage.BabbageEra StandardCrypto)
        -> (Int, (Quantity "block" Word32, O.SlotNo))
    transactionsBabbage
        (ShelleyBlock
            (SL.Block (Consensus.Header header _)
            (Alonzo.AlonzoTxSeq txs')) _) =
                ( length txs'
                , ( fromBlockNo $ Consensus.hbBlockNo header
                  , Consensus.hbSlotNo header
                  )
                )
    transactionsByron blk =
        (, (fromBlockNo $ O.blockNo blk, O.blockSlot blk)) $
            case byronBlockRaw blk of
            ABOBBlock blk' ->
                length $ fromTxAux <$> unTxPayload (blockTxPayload blk')
            ABOBBoundary _ ->
                0

    transactionsConway
        :: ShelleyBlock
            (Consensus.Praos StandardCrypto)
            (Conway.ConwayEra StandardCrypto)
        -> (Int, (Quantity "block" Word32, O.SlotNo))
    transactionsConway
        (ShelleyBlock
            (SL.Block (Consensus.Header header _)
            (Alonzo.AlonzoTxSeq txs')) _) =
                ( length txs'
                , ( fromBlockNo $ Consensus.hbBlockNo header
                  , Consensus.hbSlotNo header
                  )
                )

toCardanoEra :: CardanoBlock c -> AnyCardanoEra
toCardanoEra = \case
    BlockByron{}   -> AnyCardanoEra ByronEra
    BlockShelley{} -> AnyCardanoEra ShelleyEra
    BlockAllegra{} -> AnyCardanoEra AllegraEra
    BlockMary{}    -> AnyCardanoEra MaryEra
    BlockAlonzo{}  -> AnyCardanoEra AlonzoEra
    BlockBabbage{} -> AnyCardanoEra BabbageEra
    BlockConway{}  -> AnyCardanoEra ConwayEra

fromCardanoHash :: O.HeaderHash (CardanoBlock sc) -> W.Hash "BlockHeader"
fromCardanoHash = W.Hash . fromShort . getOneEraHash

-- FIXME unsafe conversion (Word64 -> Word32)
fromBlockNo :: BlockNo -> Quantity "block" Word32
fromBlockNo (BlockNo h) = Quantity (fromIntegral h)

fromTip' :: W.GenesisParameters -> Tip (CardanoBlock sc) -> W.BlockHeader
fromTip' gp = fromTip (W.getGenesisBlockHash gp)

fromTip
    :: W.Hash "Genesis"
    -> Tip (CardanoBlock sc)
    -> W.BlockHeader
fromTip genesisHash tip = case getPoint (getTipPoint tip) of
    Origin -> W.BlockHeader
        { slotNo = Slotting.SlotNo 0
        , blockHeight = Quantity 0
        , headerHash = coerce genesisHash
        , parentHeaderHash = Nothing
        }
    At blk -> W.BlockHeader
        { slotNo = Point.blockPointSlot blk
        , blockHeight = fromBlockNo $ getLegacyTipBlockNo tip
        , headerHash = fromCardanoHash $ Point.blockPointHash blk
        -- TODO: parentHeaderHash could be removed.
        , parentHeaderHash = Just $ W.Hash "parentHeaderHash - unused in Shelley"
        }
  where
    -- TODO: This function was marked deprecated in ouroboros-network.
    -- It is wrong, because `Origin` doesn't have a block number.
    -- We should remove it.
    getLegacyTipBlockNo t = case O.getTipBlockNo t of
        Origin -> BlockNo 0
        At x -> x

toTip :: W.Hash "Genesis" -> W.BlockHeader -> Tip (CardanoBlock sc)
toTip genesisHash (W.BlockHeader sl bl h _)
    | h == (coerce genesisHash) = O.TipGenesis
    | otherwise = O.Tip sl
        (toCardanoHash h)
        (BlockNo $ fromIntegral $ getQuantity bl)

-- NOTE: Unsafe conversion from Natural -> Word16
fromMaxSize :: Natural -> Quantity "byte" Word16
fromMaxSize = Quantity . fromIntegral

fromShelleyPParams
    :: W.EraInfo Bound
    -> Ledger.PParams StandardShelley
    -> W.ProtocolParameters
fromShelleyPParams eraInfo pp =
    W.ProtocolParameters
        { decentralizationLevel = decentralizationLevelFromPParams pp
        , txParameters =
            txParametersFromPParams
                maryTokenBundleMaxSize (W.ExecutionUnits 0 0) pp
        , desiredNumberOfStakePools =
            desiredNumberOfStakePoolsFromPParams pp
        , stakeKeyDeposit = stakeKeyDepositFromPParams pp
        , eras = fromBoundToEpochNo <$> eraInfo
        -- Collateral inputs were not supported or required in Shelley:
        , maximumCollateralInputCount = 0
        , minimumCollateralPercentage = 0
        , executionUnitPrices = Nothing
        }

fromAllegraPParams
    :: W.EraInfo Bound
    -> Ledger.PParams StandardAllegra
    -> W.ProtocolParameters
fromAllegraPParams eraInfo pp =
    W.ProtocolParameters
        { decentralizationLevel = decentralizationLevelFromPParams pp
        , txParameters =
            txParametersFromPParams
                maryTokenBundleMaxSize (W.ExecutionUnits 0 0) pp
        , desiredNumberOfStakePools =
            desiredNumberOfStakePoolsFromPParams pp
        , stakeKeyDeposit = stakeKeyDepositFromPParams pp
        , eras = fromBoundToEpochNo <$> eraInfo
        -- Collateral inputs were not supported or required in Allegra:
        , maximumCollateralInputCount = 0
        , minimumCollateralPercentage = 0
        , executionUnitPrices = Nothing
        }

fromMaryPParams
    :: W.EraInfo Bound
    -> Ledger.PParams StandardMary
    -> W.ProtocolParameters
fromMaryPParams eraInfo pp =
    W.ProtocolParameters
        { decentralizationLevel = decentralizationLevelFromPParams pp
        , txParameters =
            txParametersFromPParams
                maryTokenBundleMaxSize (W.ExecutionUnits 0 0) pp
        , desiredNumberOfStakePools =
            desiredNumberOfStakePoolsFromPParams pp
        , stakeKeyDeposit = stakeKeyDepositFromPParams pp
        , eras = fromBoundToEpochNo <$> eraInfo
        -- Collateral inputs were not supported or required in Mary:
        , maximumCollateralInputCount = 0
        , minimumCollateralPercentage = 0
        , executionUnitPrices = Nothing
        }

fromBoundToEpochNo :: Bound -> W.EpochNo
fromBoundToEpochNo (Bound _relTime _slotNo (EpochNo e)) =
    W.EpochNo $ fromIntegral e

fromAlonzoPParams
    :: HasCallStack
    => W.EraInfo Bound
    -> Ledger.PParams StandardAlonzo
    -> W.ProtocolParameters
fromAlonzoPParams eraInfo pp =
    W.ProtocolParameters
        { decentralizationLevel = decentralizationLevelFromPParams pp
        , txParameters = txParametersFromPParams
            (W.TokenBundleMaxSize $ W.TxSize $ pp ^. ppMaxValSizeL)
            (fromLedgerExUnits (pp ^. ppMaxTxExUnitsL))
            pp
        , desiredNumberOfStakePools =
            desiredNumberOfStakePoolsFromPParams pp
        , stakeKeyDeposit = stakeKeyDepositFromPParams pp
        , eras = fromBoundToEpochNo <$> eraInfo
        , maximumCollateralInputCount =
            unsafeIntToWord $ pp ^. ppMaxCollateralInputsL
        , minimumCollateralPercentage =
            pp ^. ppCollateralPercentageL
        , executionUnitPrices =
            Just $ executionUnitPricesFromPParams pp
        }

fromBabbagePParams
    :: HasCallStack
    => W.EraInfo Bound
    -> Ledger.PParams StandardBabbage
    -> W.ProtocolParameters
fromBabbagePParams eraInfo pp =
    W.ProtocolParameters
        { decentralizationLevel =
            W.fromFederationPercentage $ Percentage.fromRationalClipped 0
        , txParameters = txParametersFromPParams
            (W.TokenBundleMaxSize $ W.TxSize $ pp ^. ppMaxValSizeL)
            (fromLedgerExUnits (pp ^. ppMaxTxExUnitsL))
            pp
        , desiredNumberOfStakePools =
            desiredNumberOfStakePoolsFromPParams pp
        , stakeKeyDeposit = stakeKeyDepositFromPParams pp
        , eras = fromBoundToEpochNo <$> eraInfo
        , maximumCollateralInputCount =
            unsafeIntToWord $ pp ^. ppMaxCollateralInputsL
        , minimumCollateralPercentage =
            pp ^. ppCollateralPercentageL
        , executionUnitPrices =
            Just $ executionUnitPricesFromPParams pp
        }

fromConwayPParams
    :: HasCallStack
    => W.EraInfo Bound
    -> Ledger.PParams StandardConway
    -> W.ProtocolParameters
fromConwayPParams eraInfo pp =
    W.ProtocolParameters
        { decentralizationLevel =
            W.fromFederationPercentage $ Percentage.fromRationalClipped 0
        , txParameters = txParametersFromPParams
            (W.TokenBundleMaxSize $ W.TxSize $ pp ^. ppMaxValSizeL)
            (fromLedgerExUnits (pp ^. ppMaxTxExUnitsL))
            pp
        , desiredNumberOfStakePools = desiredNumberOfStakePoolsFromPParams pp
        , stakeKeyDeposit = stakeKeyDepositFromPParams pp
        , eras = fromBoundToEpochNo <$> eraInfo
        , maximumCollateralInputCount =
            intCastMaybe (pp ^. ppMaxCollateralInputsL)
                & fromMaybe
                    (error "Maximum count of collateral inputs exceeds 2^16")
        , minimumCollateralPercentage = pp ^. ppCollateralPercentageL
        , executionUnitPrices = Just $ executionUnitPricesFromPParams pp
        }

-- | Extract the current network decentralization level from the given set of
-- protocol parameters.
decentralizationLevelFromPParams
    :: (Ledger.EraPParams era, Ledger.ProtVerAtMost era 6)
    => Ledger.PParams era -> W.DecentralizationLevel
decentralizationLevelFromPParams pp =
    W.fromFederationPercentage $ fromUnitInterval $ pp ^. ppDL

executionUnitPricesFromPParams
    :: Ledger.AlonzoEraPParams era
    => Ledger.PParams era
    -> W.ExecutionUnitPrices
executionUnitPricesFromPParams pp = fromAlonzoPrices (pp ^. ppPricesL)
  where
    fromAlonzoPrices Alonzo.Prices{prMem, prSteps} =
        W.ExecutionUnitPrices
        { W.pricePerStep = SL.unboundRational prSteps
        , W.pricePerMemoryUnit = SL.unboundRational prMem
        }

fromLedgerExUnits
    :: Alonzo.ExUnits
    -> W.ExecutionUnits
fromLedgerExUnits (Alonzo.ExUnits mem steps) =
    W.ExecutionUnits
    { executionSteps = steps
    , executionMemory = mem
    }

txParametersFromPParams
    :: Ledger.EraPParams era
    => W.TokenBundleMaxSize
    -> W.ExecutionUnits
    -> Ledger.PParams era
    -> W.TxParameters
txParametersFromPParams maxBundleSize getMaxExecutionUnits pp = W.TxParameters
    { getFeePolicy = W.LinearFee $ W.LinearFunction
        { intercept = coinToDouble (pp ^. ppMinFeeBL)
        , slope = coinToDouble (pp ^. ppMinFeeAL)
        }
    , getTxMaxSize = fromMaxSize $ pp ^. ppMaxTxSizeL
    , getTokenBundleMaxSize = maxBundleSize
    , getMaxExecutionUnits
    }
  where
    coinToDouble :: Ledger.Coin -> Double
    coinToDouble = fromRational . Ledger.coinToRational

desiredNumberOfStakePoolsFromPParams
    :: (HasCallStack, Ledger.EraPParams era) => Ledger.PParams era -> Word16
desiredNumberOfStakePoolsFromPParams pp =
    intCastMaybe (pp ^. ppNOptL)
        & fromMaybe (error "Desired number of stake pools exceeds 2^16")

stakeKeyDepositFromPParams
    :: Ledger.EraPParams era => Ledger.PParams era -> W.Coin
stakeKeyDepositFromPParams = toWalletCoin . view ppKeyDepositL

slottingParametersFromGenesis :: ShelleyGenesis e -> W.SlottingParameters
slottingParametersFromGenesis g =
    W.SlottingParameters
        { getSlotLength =
            W.SlotLength . fromNominalDiffTimeMicro $ sgSlotLength g
        , getEpochLength =
            W.EpochLength . fromIntegral . unEpochSize $ sgEpochLength g
        , getActiveSlotCoefficient =
            W.ActiveSlotCoefficient . fromRational . SL.unboundRational
                $ sgActiveSlotsCoeff g
        , getSecurityParameter =
            Quantity . fromIntegral $ sgSecurityParam g
        }

-- | Convert genesis data into blockchain params and an initial set of UTxO
fromGenesisData
    :: ShelleyGenesis StandardCrypto
    -> (W.NetworkParameters, W.Block, [PoolCertificate])
fromGenesisData g =
    ( W.NetworkParameters
        { genesisParameters = W.GenesisParameters
            { getGenesisBlockHash = dummyGenesisHash
            , getGenesisBlockDate = W.StartTime $ sgSystemStart g
            }
        , slottingParameters = slottingParametersFromGenesis g
        , protocolParameters =
            fromShelleyPParams W.emptyEraInfo (sgProtocolParams g)
        }
    , genesisBlockFromTxOuts (ListMap.toList $ sgInitialFunds g)
    , poolCerts $ sgStaking g
    )
  where
    -- TODO: There is not yet any agreed upon definition of a
    -- genesis hash for a shelley-only testnet.
    --
    -- For now we use a dummy value.
    dummyGenesisHash = W.Hash . BS.pack $ replicate 32 1

    poolCerts :: SLAPI.ShelleyGenesisStaking StandardCrypto -> [PoolCertificate]
    poolCerts (SLAPI.ShelleyGenesisStaking pools _stake) = do
        (_, pp) <- ListMap.toList pools
        pure $ W.Registration $ PoolRegistrationCertificate
            { W.poolId = fromPoolKeyHash $ ppId pp
            , W.poolOwners = fromOwnerKeyHash <$> Set.toList (ppOwners pp)
            , W.poolMargin = fromUnitInterval (ppMargin pp)
            , W.poolCost = toWalletCoin (ppCost pp)
            , W.poolPledge = toWalletCoin (ppPledge pp)
            , W.poolMetadata =
                fromPoolMetadata <$> strictMaybeToMaybe (ppMetadata pp)
            }

    -- | Construct a ("fake") genesis block from genesis transaction outputs.
    --
    -- The genesis data on haskell nodes is not a block at all, unlike the
    -- block0 on jormungandr. This function is a method to deal with the
    -- discrepancy.
    genesisBlockFromTxOuts :: [(SL.Addr StandardCrypto, SL.Coin)] -> W.Block
    genesisBlockFromTxOuts outs = W.Block
        { delegations  = []
        , header = W.BlockHeader
            { slotNo = Slotting.SlotNo 0
            , blockHeight = Quantity 0
            , headerHash = dummyGenesisHash
            , parentHeaderHash = Nothing
            }
        , transactions = mkTx <$> outs
        }
      where
        mkTx (addr, c) = W.Tx
            { txId = pseudoHash
            , txCBOR = Nothing
            , fee = Nothing
            , resolvedInputs = []
            , resolvedCollateralInputs = []
            , outputs =
                [W.TxOut
                    (fromShelleyAddress addr)
                    (TokenBundle.fromCoin $ Ledger.toWalletCoin c)
                ]
            -- Collateral outputs were not supported at the time of genesis:
            , collateralOutput = Nothing
            , withdrawals = mempty
            , metadata = Nothing
            , scriptValidity = Nothing
            }
          where
            W.TxIn pseudoHash _ = fromShelleyTxIn $
                SL.initialFundsPseudoTxIn @StandardCrypto addr

--
-- Stake pools
--

fromPoolId :: forall crypto. SL.KeyHash 'SL.StakePool crypto -> PoolId
fromPoolId (SL.KeyHash x) = PoolId $ hashToBytes x

fromPoolDistr
    :: forall crypto. ()
    => SL.PoolDistr crypto
    -> Map PoolId Percentage
fromPoolDistr =
    Map.map (unsafeMkPercentage . SL.individualPoolStake)
    . Map.mapKeys fromPoolId
    . SL.unPoolDistr

-- NOTE: This function disregards results that are using staking keys
fromNonMyopicMemberRewards
    :: forall era. ()
    => O.NonMyopicMemberRewards era
    -> Map (Either W.Coin W.RewardAccount) (Map PoolId W.Coin)
fromNonMyopicMemberRewards =
    Map.map (Map.map toWalletCoin . Map.mapKeys fromPoolId)
    . Map.mapKeys (bimap Ledger.toWalletCoin fromStakeCredential)
    . O.unNonMyopicMemberRewards

optimumNumberOfPools
    :: (HasCallStack, Ledger.EraPParams era) => Ledger.PParams era -> Int
optimumNumberOfPools = intCast . desiredNumberOfStakePoolsFromPParams

--
-- Txs
--

fromCardanoTxIn :: Cardano.TxIn -> W.TxIn
fromCardanoTxIn (Cardano.TxIn txid (Cardano.TxIx ix)) =
    W.TxIn
        (W.Hash $ fromShelleyTxId $ Cardano.toShelleyTxId txid)
        (fromIntegral ix)

-- | WARNING: Datum hashes are lost in the conversion!
fromCardanoTxOut :: IsCardanoEra era => Cardano.TxOut ctx era -> W.TxOut
fromCardanoTxOut (Cardano.TxOut addr out _datumHash _) =
    W.TxOut
        (W.Address $ Cardano.serialiseToRawBytes addr)
        (fromCardanoTxOutValue out)
  where
    fromCardanoTxOutValue (Cardano.TxOutValue _ val) = fromCardanoValue val
    fromCardanoTxOutValue (Cardano.TxOutAdaOnly _ lovelace) =
        TokenBundle.fromCoin $ fromCardanoLovelace lovelace

fromCardanoWdrls
    :: Cardano.TxWithdrawals build era
    -> [(W.RewardAccount, W.Coin)]
fromCardanoWdrls = \case
    Cardano.TxWithdrawalsNone -> []
    Cardano.TxWithdrawals _era xs ->
        flip fmap xs $ \((Cardano.StakeAddress _ creds), coin, _) ->
            ( fromStakeCredential creds
            , fromCardanoLovelace coin
            )

cardanoCertKeysForWitnesses
    :: Cardano.TxCertificates build era
    -> [W.RewardAccount]
cardanoCertKeysForWitnesses = \case
    Cardano.TxCertificatesNone -> []
    Cardano.TxCertificates _era certs _witsMap ->
        mapMaybe f certs
 where
    toRewardAccount = Just . fromStakeCredential . Cardano.toShelleyStakeCredential
    f = \case
        Cardano.StakeAddressDeregistrationCertificate cred ->
            toRewardAccount cred
        Cardano.StakeAddressPoolDelegationCertificate cred _ ->
            toRewardAccount cred
        _ ->
            Nothing

toShelleyCoin :: W.Coin -> SL.Coin
toShelleyCoin (W.Coin c) = SL.Coin $ intCast c

-- Lovelace to coin. Quantities from ledger should always fit in Word64.
fromCardanoLovelace :: HasCallStack => Cardano.Lovelace -> W.Coin
fromCardanoLovelace =
    Coin.unsafeFromIntegral . unQuantity . Cardano.lovelaceToQuantity
  where
    unQuantity (Cardano.Quantity q) = q

toWalletCoin :: HasCallStack => SL.Coin -> W.Coin
toWalletCoin (SL.Coin c) = Coin.unsafeFromIntegral c

fromPoolMetadata :: SL.PoolMetadata -> (StakePoolMetadataUrl, StakePoolMetadataHash)
fromPoolMetadata meta =
    ( StakePoolMetadataUrl (urlToText (pmUrl meta))
    , StakePoolMetadataHash (pmHash meta)
    )

fromPoolKeyHash :: SL.KeyHash rol sc -> PoolId
fromPoolKeyHash (SL.KeyHash h) = PoolId (hashToBytes h)

fromOwnerKeyHash :: SL.KeyHash 'SL.Staking crypto -> PoolOwner
fromOwnerKeyHash (SL.KeyHash h) = PoolOwner (hashToBytes h)

fromCardanoAddress :: Cardano.Address Cardano.ShelleyAddr -> W.Address
fromCardanoAddress = W.Address . Cardano.serialiseToRawBytes

fromUnitInterval :: HasCallStack => SL.UnitInterval -> Percentage
fromUnitInterval x
    = either bomb id
    . Percentage.fromRational
    . toRational
    . SL.unboundRational
    $ x
  where
    bomb = internalError $
        "fromUnitInterval: encountered invalid parameter value: " +|| x ||+ ""

toCardanoTxId :: W.Hash "Tx" -> Cardano.TxId
toCardanoTxId (W.Hash h) = Cardano.TxId $ UnsafeHash $ toShort h

toCardanoTxIn :: W.TxIn -> Cardano.TxIn
toCardanoTxIn (W.TxIn tid ix) =
    Cardano.TxIn (toCardanoTxId tid) (Cardano.TxIx (fromIntegral ix))

toCardanoStakeCredential :: W.RewardAccount -> Cardano.StakeCredential
toCardanoStakeCredential = \case
    W.FromKeyHash bs ->
        Cardano.StakeCredentialByKey
        . Cardano.StakeKeyHash
        . SL.KeyHash
        . UnsafeHash
        . SBS.toShort
        $ bs
    W.FromScriptHash bs ->
        Cardano.StakeCredentialByScript
        . Cardano.fromShelleyScriptHash
        . SL.ScriptHash
        . unsafeHashFromBytes
        $ bs

toCardanoLovelace :: W.Coin -> Cardano.Lovelace
toCardanoLovelace (W.Coin c) = Cardano.Lovelace $ intCast c

toCardanoTxOut
    :: HasCallStack
    => ShelleyBasedEra era
    -> Maybe (Script KeyHash)
    -> W.TxOut
    -> Cardano.TxOut ctx era
toCardanoTxOut era refScriptM = case era of
    ShelleyBasedEraShelley -> toShelleyTxOut
    ShelleyBasedEraAllegra -> toAllegraTxOut
    ShelleyBasedEraMary    -> toMaryTxOut
    ShelleyBasedEraAlonzo  -> toAlonzoTxOut
    ShelleyBasedEraBabbage -> toBabbageTxOut
    ShelleyBasedEraConway  -> toConwayTxOut
  where
    toShelleyTxOut :: HasCallStack => W.TxOut -> Cardano.TxOut ctx ShelleyEra
    toShelleyTxOut (W.TxOut (W.Address addr) tokens) =
        Cardano.TxOut
            addrInEra
            (adaOnly $ toCardanoLovelace $ TokenBundle.getCoin tokens)
            Cardano.TxOutDatumNone
            Cardano.ReferenceScriptNone
      where
        adaOnly = Cardano.TxOutAdaOnly Cardano.AdaOnlyInShelleyEra
        addrInEra = tina "toCardanoTxOut: malformed address"
            [ Cardano.AddressInEra
                (Cardano.ShelleyAddressInEra Cardano.ShelleyBasedEraShelley)
                <$> eitherToMaybe
                    (Cardano.deserialiseFromRawBytes AsShelleyAddress addr)

            , Cardano.AddressInEra Cardano.ByronAddressInAnyEra
                <$> eitherToMaybe
                    (Cardano.deserialiseFromRawBytes AsByronAddress addr)
            ]

    toAllegraTxOut :: HasCallStack => W.TxOut -> Cardano.TxOut ctx AllegraEra
    toAllegraTxOut (W.TxOut (W.Address addr) tokens) =
        Cardano.TxOut
            addrInEra
            (adaOnly $ toCardanoLovelace $ TokenBundle.getCoin tokens)
            Cardano.TxOutDatumNone
            Cardano.ReferenceScriptNone
      where
        adaOnly = Cardano.TxOutAdaOnly Cardano.AdaOnlyInAllegraEra
        addrInEra = tina "toCardanoTxOut: malformed address"
            [ Cardano.AddressInEra
                (Cardano.ShelleyAddressInEra Cardano.ShelleyBasedEraAllegra)
                <$> eitherToMaybe
                    (Cardano.deserialiseFromRawBytes AsShelleyAddress addr)

            , Cardano.AddressInEra Cardano.ByronAddressInAnyEra
                <$> eitherToMaybe
                    (Cardano.deserialiseFromRawBytes AsByronAddress addr)
            ]

    toMaryTxOut :: HasCallStack => W.TxOut -> Cardano.TxOut ctx MaryEra
    toMaryTxOut (W.TxOut (W.Address addr) tokens) =
        Cardano.TxOut
            addrInEra
            (Cardano.TxOutValue Cardano.MultiAssetInMaryEra
                $ toCardanoValue tokens)
            Cardano.TxOutDatumNone
            Cardano.ReferenceScriptNone
      where
        addrInEra = tina "toCardanoTxOut: malformed address"
            [ Cardano.AddressInEra
                (Cardano.ShelleyAddressInEra Cardano.ShelleyBasedEraMary)
                    <$> eitherToMaybe
                        (Cardano.deserialiseFromRawBytes AsShelleyAddress addr)

            , Cardano.AddressInEra Cardano.ByronAddressInAnyEra
                <$> eitherToMaybe
                    (Cardano.deserialiseFromRawBytes AsByronAddress addr)
            ]

    toAlonzoTxOut :: HasCallStack => W.TxOut -> Cardano.TxOut ctx AlonzoEra
    toAlonzoTxOut (W.TxOut (W.Address addr) tokens) =
        Cardano.TxOut
            addrInEra
            (Cardano.TxOutValue Cardano.MultiAssetInAlonzoEra
                $ toCardanoValue tokens)
            datumHash
            refScript
      where
        refScript = Cardano.ReferenceScriptNone
        datumHash = Cardano.TxOutDatumNone
        addrInEra = tina "toCardanoTxOut: malformed address"
            [ Cardano.AddressInEra
                (Cardano.ShelleyAddressInEra Cardano.ShelleyBasedEraAlonzo)
                    <$> eitherToMaybe
                        (Cardano.deserialiseFromRawBytes AsShelleyAddress addr)

            , Cardano.AddressInEra Cardano.ByronAddressInAnyEra
                <$> eitherToMaybe
                    (Cardano.deserialiseFromRawBytes AsByronAddress addr)
            ]

    toBabbageTxOut :: HasCallStack => W.TxOut -> Cardano.TxOut ctx BabbageEra
    toBabbageTxOut (W.TxOut (W.Address addr) tokens) =
        Cardano.TxOut
            addrInEra
            (Cardano.TxOutValue Cardano.MultiAssetInBabbageEra
                $ toCardanoValue tokens)
            datumHash
            refScript
      where
        refScript = case refScriptM of
            Nothing ->
                Cardano.ReferenceScriptNone
            Just script ->
                let aux = Cardano.ReferenceTxInsScriptsInlineDatumsInBabbageEra
                    scriptApi = Cardano.toScriptInAnyLang $ Cardano.SimpleScript $
                        toCardanoSimpleScript script
                in Cardano.ReferenceScript aux scriptApi
        datumHash = Cardano.TxOutDatumNone
        addrInEra = tina "toCardanoTxOut: malformed address"
            [ Cardano.AddressInEra
                (Cardano.ShelleyAddressInEra Cardano.ShelleyBasedEraBabbage)
                    <$> eitherToMaybe
                        (Cardano.deserialiseFromRawBytes AsShelleyAddress addr)
            , Cardano.AddressInEra Cardano.ByronAddressInAnyEra
                <$> eitherToMaybe
                    (Cardano.deserialiseFromRawBytes AsByronAddress addr)
            ]

    toConwayTxOut :: HasCallStack => W.TxOut -> Cardano.TxOut ctx ConwayEra
    toConwayTxOut (W.TxOut (W.Address addr) tokens) =
        Cardano.TxOut
            addrInEra
            (Cardano.TxOutValue Cardano.MultiAssetInConwayEra
                $ toCardanoValue tokens)
            datumHash
            refScript
      where
        refScript = case refScriptM of
            Nothing ->
                Cardano.ReferenceScriptNone
            Just script ->
                let aux = Cardano.ReferenceTxInsScriptsInlineDatumsInConwayEra
                    scriptApi = Cardano.toScriptInAnyLang $ Cardano.SimpleScript $
                        toCardanoSimpleScript script
                in Cardano.ReferenceScript aux scriptApi
        datumHash = Cardano.TxOutDatumNone
        addrInEra = tina "toCardanoTxOut: malformed address"
            [ Cardano.AddressInEra
                (Cardano.ShelleyAddressInEra Cardano.ShelleyBasedEraConway)
                    <$> eitherToMaybe
                        (Cardano.deserialiseFromRawBytes AsShelleyAddress addr)

            , Cardano.AddressInEra Cardano.ByronAddressInAnyEra
                <$> eitherToMaybe
                    (Cardano.deserialiseFromRawBytes AsByronAddress addr)
            ]

toCardanoValue :: TokenBundle.TokenBundle -> Cardano.Value
toCardanoValue tb = Cardano.valueFromList $
    (Cardano.AdaAssetId, coinToQuantity coin) :
    map (bimap toCardanoAssetId toQuantity) bundle
  where
    (coin, bundle) = TokenBundle.toFlatList tb
    toCardanoAssetId (W.AssetId pid name) =
        Cardano.AssetId (toCardanoPolicyId pid) (toCardanoAssetName name)

    toCardanoAssetName (W.UnsafeAssetName name) =
        just "toCardanoValue" "AssetName"
        [ eitherToMaybe
            $ Cardano.deserialiseFromRawBytes Cardano.AsAssetName name
        ]

    coinToQuantity = fromIntegral . W.unCoin
    toQuantity = fromIntegral . W.unTokenQuantity

toCardanoPolicyId :: W.TokenPolicyId -> Cardano.PolicyId
toCardanoPolicyId (W.UnsafeTokenPolicyId (W.Hash pid)) =
    just "toCardanoPolicyId" "PolicyId"
    [eitherToMaybe $ Cardano.deserialiseFromRawBytes Cardano.AsPolicyId pid]

toCardanoSimpleScript
    :: Script KeyHash
    -> Cardano.SimpleScript
toCardanoSimpleScript = \case
    RequireSignatureOf (KeyHash _ keyhash) ->
        case eitherToMaybe $ Cardano.deserialiseFromRawBytes
            (Cardano.AsHash Cardano.AsPaymentKey) keyhash of
                Just payKeyHash -> Cardano.RequireSignature payKeyHash
                Nothing -> error "Hash key not valid"
    RequireAllOf contents ->
        Cardano.RequireAllOf $ map toCardanoSimpleScript contents
    RequireAnyOf contents ->
        Cardano.RequireAnyOf $ map toCardanoSimpleScript contents
    RequireSomeOf num contents ->
        Cardano.RequireMOf (fromIntegral num) $
            map toCardanoSimpleScript contents
    ActiveFromSlot slot ->
        Cardano.RequireTimeAfter
        (O.SlotNo $ fromIntegral slot)
    ActiveUntilSlot slot ->
        Cardano.RequireTimeBefore
        (O.SlotNo $ fromIntegral slot)

just :: Builder -> Builder -> [Maybe a] -> a
just t1 t2 = tina (t1+|": unable to deserialise "+|t2)

toLedgerStakeCredential
    :: (Crypto.HashAlgorithm (SL.ADDRHASH crypto))
    => W.RewardAccount
    -> SL.StakeCredential crypto
toLedgerStakeCredential = \case
    W.FromKeyHash bs ->
          SL.KeyHashObj
        . SL.KeyHash
        . unsafeHashFromBytes
        $ bs
    W.FromScriptHash bs ->
          SL.ScriptHashObj
        . SL.ScriptHash
        . unsafeHashFromBytes
        $ bs

unsafeHashFromBytes :: Crypto.HashAlgorithm h => ByteString -> Hash h a
unsafeHashFromBytes =
    fromMaybe (error "unsafeHashFromBytes: wrong length")
    . Crypto.hashFromBytes

toStakeKeyDeregCert :: Either XPub (Script KeyHash) -> Cardano.Certificate
toStakeKeyDeregCert = \case
    Left xpub ->
        Cardano.makeStakeAddressDeregistrationCertificate
        . Cardano.StakeCredentialByKey
        . Cardano.StakeKeyHash
        . SL.KeyHash
        . UnsafeHash
        . toShort
        . blake2b224
        $ xpubPublicKey xpub
    Right script ->
        Cardano.makeStakeAddressDeregistrationCertificate
        . Cardano.StakeCredentialByScript
        . Cardano.hashScript
        . Cardano.SimpleScript
        $ toCardanoSimpleScript script

toStakeKeyRegCert :: Either XPub (Script KeyHash) -> Cardano.Certificate
toStakeKeyRegCert cred = case cred of
    Left xpub ->
        Cardano.makeStakeAddressRegistrationCertificate
        . Cardano.StakeCredentialByKey
        . Cardano.StakeKeyHash
        . SL.KeyHash
        . UnsafeHash
        . toShort
        . blake2b224
        $ xpubPublicKey xpub
    Right script ->
        Cardano.makeStakeAddressRegistrationCertificate
        . Cardano.StakeCredentialByScript
        . Cardano.hashScript
        . Cardano.SimpleScript
        $ toCardanoSimpleScript script

toStakePoolDlgCert :: Either XPub (Script KeyHash) -> PoolId -> Cardano.Certificate
toStakePoolDlgCert cred (PoolId pid) = case cred of
    Left xpub ->
        Cardano.makeStakeAddressPoolDelegationCertificate
        (Cardano.StakeCredentialByKey $ Cardano.StakeKeyHash (toKeyHash xpub))
        (Cardano.StakePoolKeyHash pool)
    Right script ->
        Cardano.makeStakeAddressPoolDelegationCertificate
        (Cardano.StakeCredentialByScript
        . Cardano.hashScript
        . Cardano.SimpleScript
        $ toCardanoSimpleScript script)
        (Cardano.StakePoolKeyHash pool)
  where
    toKeyHash = SL.KeyHash . UnsafeHash . toShort . blake2b224 . xpubPublicKey
    pool = SL.KeyHash $ UnsafeHash $ toShort pid

-- | Extract a stake reference / `RewardAccount` from an address, if it exists.
--
-- Note that this returns `Nothing` for pointer addresses, not just enterprise
-- addresses.
rewardAccountFromAddress :: W.Address -> Maybe W.RewardAccount
rewardAccountFromAddress (W.Address bytes) = refToAccount . ref =<< parseAddr bytes
  where
    parseAddr :: ByteString -> Maybe (Cardano.Address Cardano.ShelleyAddr)
    parseAddr = eitherToMaybe . Cardano.deserialiseFromRawBytes AsShelleyAddress

    ref :: Cardano.Address Cardano.ShelleyAddr -> SL.StakeReference StandardCrypto
    ref (Cardano.ShelleyAddress _n _paymentKey stakeRef) = stakeRef

    refToAccount :: SL.StakeReference StandardCrypto -> Maybe W.RewardAccount
    refToAccount (SL.StakeRefBase cred) = Just $ fromStakeCredential cred
    refToAccount (SL.StakeRefPtr _) = Nothing
    refToAccount SL.StakeRefNull = Nothing

-- | Converts 'SealedTx' to something that can be submitted with the
-- 'Cardano.Api' local tx submission client.
unsealShelleyTx
    :: AnyCardanoEra
    -- ^ Preferred latest era (see 'ideallyNoLaterThan')
    -> W.SealedTx
    -> TxInMode CardanoMode
unsealShelleyTx era wtx = case W.cardanoTxIdeallyNoLaterThan era wtx of
    Cardano.InAnyCardanoEra ByronEra tx ->
        TxInMode tx ByronEraInCardanoMode
    Cardano.InAnyCardanoEra ShelleyEra tx ->
        TxInMode tx ShelleyEraInCardanoMode
    Cardano.InAnyCardanoEra AllegraEra tx ->
        TxInMode tx AllegraEraInCardanoMode
    Cardano.InAnyCardanoEra MaryEra tx ->
        TxInMode tx MaryEraInCardanoMode
    Cardano.InAnyCardanoEra AlonzoEra tx ->
        TxInMode tx AlonzoEraInCardanoMode
    Cardano.InAnyCardanoEra BabbageEra tx ->
        TxInMode tx BabbageEraInCardanoMode
    Cardano.InAnyCardanoEra ConwayEra tx ->
        TxInMode tx ConwayEraInCardanoMode

instance (forall era. IsCardanoEra era => Show (thing era)) =>
    Show (InAnyCardanoEra thing) where
    show (InAnyCardanoEra era thing) =
        "InAnyCardanoEra " ++ show era ++ " (" ++ show thing ++ ")"

instance (forall era. IsCardanoEra era => Eq (thing era)) =>
    Eq (InAnyCardanoEra thing) where
    InAnyCardanoEra e1 a == InAnyCardanoEra e2 b = case testEquality e1 e2 of
        Just Refl -> a == b
        Nothing -> False

{-------------------------------------------------------------------------------
                                    Logging
-------------------------------------------------------------------------------}

-- Compact representation of connection id for log messages.
instance Buildable addr => Buildable (ConnectionId addr) where
   build (ConnectionId a b) = "conn:" <> build a <> ":" <> build b

instance Buildable LocalAddress where
    build (LocalAddress p) = build p

{-------------------------------------------------------------------------------
                                 Utilities
-------------------------------------------------------------------------------}

-- Inverts a value in the unit interval [0, 1].
--
-- Examples:
--
-- >>> invertUnitInterval interval0 == interval1
-- >>> invertUnitInterval interval1 == interval0
--
-- Satisfies the following properties:
--
-- >>> invertUnitInterval . invertUnitInterval == id
-- >>> intervalValue (invertUnitInterval i) + intervalValue i == 1
--
invertUnitInterval :: HasCallStack => SL.UnitInterval -> SL.UnitInterval
invertUnitInterval = unsafeBoundRational . (1 - ) . SL.unboundRational
  where
    unsafeBoundRational :: Rational -> SL.UnitInterval
    unsafeBoundRational = tina "invertUnitInterval: the impossible happened"
        . pure . SL.boundRational

interval1 :: SL.UnitInterval
interval1 = maxBound

interval0 :: SL.UnitInterval
interval0 = minBound
