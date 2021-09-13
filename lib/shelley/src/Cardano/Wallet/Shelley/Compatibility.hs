{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- Orphan instances for {Encode,Decode}Address until we get rid of the
-- Jörmungandr dual support.
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Copyright: © 2020 IOHK
-- License: Apache-2.0
--
-- Conversion functions and static chain settings for Shelley.

module Cardano.Wallet.Shelley.Compatibility
    ( AnyCardanoEra(..)
    , CardanoEra(..)
    , ShelleyEra
    , AllegraEra
    , CardanoBlock
    , NetworkId

    , NodeToClientVersionData
    , StandardCrypto
    , StandardShelley

      -- * Protocol Parameters
    , nodeToClientVersions

      -- * Genesis
    , emptyGenesis

      -- * Conversions
    , toCardanoHash
    , toPoint
    , toCardanoTxId
    , toCardanoTxIn
    , toShelleyTxOut
    , toAllegraTxOut
    , toMaryTxOut
    , toAlonzoTxOut
    , toCardanoLovelace
    , sealShelleyTx
    , toStakeKeyRegCert
    , toStakeKeyDeregCert
    , toStakePoolDlgCert
    , toStakeCredential
    , fromStakeCredential
    , toShelleyCoin
    , fromShelleyCoin
    , toHDPayloadAddress
    , toCardanoStakeCredential
    , toCardanoValue
    , fromCardanoValue
    , rewardAccountFromAddress
    , fromShelleyPParams
    , fromAlonzoPParams
    , unsafeIntToWord
    , internalError
    , isInternalError
    , ToCardanoGenTx (..)
    , fromLedgerExUnits

      -- ** Assessing sizes of token bundles
    , tokenBundleSizeAssessor
    , computeTokenBundleSerializedLengthBytes

      -- ** Stake pools
    , fromPoolId
    , fromPoolDistr
    , fromNonMyopicMemberRewards
    , optimumNumberOfPools
    , getProducer

    , HasNetworkId (..)
    , fromBlockNo
    , fromCardanoBlock
    , toCardanoEra
    , toCardanoBlockHeader
    , toShelleyBlockHeader
    , fromShelleyHash
    , fromCardanoHash
    , fromChainHash
    , fromPrevHash
    , fromGenesisData
    , fromTip
    , fromTip'
    , fromShelleyTx
    , fromAllegraTx
    , fromShelleyBlock
    , fromAllegraBlock
    , slottingParametersFromGenesis
    , fromMaryBlock
    , fromMaryTx
    , fromAlonzoTx
    , fromAlonzoBlock

      -- * Internal Conversions
    , decentralizationLevelFromPParams

      -- * Utilities
    , inspectAddress
    , invertUnitInterval
    , interval0
    , interval1
    ) where

import Prelude

import Cardano.Address
    ( unsafeMkAddress )
import Cardano.Address.Derivation
    ( XPub, xpubPublicKey )
import Cardano.Api
    ( AllegraEra
    , AlonzoEra
    , AnyCardanoEra (..)
    , AsType (..)
    , CardanoEra (..)
    , MaryEra
    , NetworkId
    , ShelleyEra
    , deserialiseFromRawBytes
    )
import Cardano.Api.Shelley
    ( ShelleyGenesis (..), fromShelleyMetadata )
import Cardano.Binary
    ( fromCBOR, serialize' )
import Cardano.Crypto.Hash.Class
    ( Hash (UnsafeHash), hashToBytes )
import Cardano.Ledger.BaseTypes
    ( strictMaybeToMaybe, urlToText )
import Cardano.Ledger.Era
    ( Era (..) )
import Cardano.Ledger.Serialization
    ( ToCBORGroup )
import Cardano.Slotting.Slot
    ( EpochNo (..), EpochSize (..) )
import Cardano.Wallet.Api.Types
    ( DecodeAddress (..)
    , DecodeStakeAddress (..)
    , EncodeAddress (..)
    , EncodeStakeAddress (..)
    )
import Cardano.Wallet.Byron.Compatibility
    ( fromByronBlock, fromTxAux, maryTokenBundleMaxSize, toByronBlockHeader )
import Cardano.Wallet.Primitive.AddressDerivation
    ( NetworkDiscriminant (..) )
import Cardano.Wallet.Primitive.Types
    ( MinimumUTxOValue (..)
    , PoolCertificate (..)
    , PoolRegistrationCertificate (..)
    , PoolRetirementCertificate (..)
    , ProtocolParameters (txParameters)
    , TxParameters (getTokenBundleMaxSize)
    )
import Cardano.Wallet.Unsafe
    ( safeDeserialiseCbor, unsafeMkPercentage )
import Codec.Binary.Bech32
    ( dataPartFromBytes, dataPartToBytes )
import Control.Applicative
    ( (<|>) )
import Control.Arrow
    ( left )
import Control.Exception
    ( ErrorCall, displayException )
import Control.Monad
    ( when, (>=>) )
import Crypto.Hash.Utils
    ( blake2b224 )
import Data.Bifunctor
    ( bimap )
import Data.Binary.Get
    ( runGetOrFail )
import Data.Binary.Put
    ( putByteString, putWord8, runPut )
import Data.Bits
    ( (.&.), (.|.) )
import Data.ByteArray.Encoding
    ( Base (Base16), convertFromBase )
import Data.ByteString
    ( ByteString )
import Data.ByteString.Base58
    ( bitcoinAlphabet, decodeBase58, encodeBase58 )
import Data.ByteString.Short
    ( fromShort, toShort )
import Data.Coerce
    ( coerce )
import Data.Either.Extra
    ( eitherToMaybe )
import Data.Foldable
    ( asum, toList )
import Data.Function
    ( (&) )
import Data.List
    ( isPrefixOf )
import Data.Map.Strict
    ( Map )
import Data.Maybe
    ( fromMaybe, isJust, mapMaybe )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Percentage, Quantity (..), mkPercentage )
import Data.Text
    ( Text )
import Data.Text.Class
    ( TextDecodingError (..) )
import Data.Typeable
    ( Typeable, typeRep )
import Data.Word
    ( Word16, Word32, Word64, Word8 )
import Fmt
    ( Buildable (..), Builder, fmt, (+|), (+||), (|+), (||+) )
import GHC.Records
    ( HasField (..) )
import GHC.Stack
    ( HasCallStack )
import GHC.TypeLits
    ( KnownNat, natVal )
import Numeric.Natural
    ( Natural )
import Ouroboros.Consensus.Cardano.Block
    ( CardanoBlock
    , CardanoEras
    , CardanoGenTx
    , GenTx (..)
    , HardForkBlock (..)
    , StandardAlonzo
    , StandardShelley
    )
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras
    ( OneEraHash (..) )
import Ouroboros.Consensus.HardFork.History.Summary
    ( Bound (..) )
import Ouroboros.Consensus.Shelley.Eras
    ( StandardCrypto )
import Ouroboros.Consensus.Shelley.Ledger
    ( ShelleyHash (..) )
import Ouroboros.Consensus.Shelley.Ledger.Block
    ( ShelleyBlock (..) )
import Ouroboros.Network.Block
    ( BlockNo (..), ChainHash, Point (..), Tip (..), getTipPoint )
import Ouroboros.Network.NodeToClient
    ( ConnectionId (..)
    , LocalAddress (..)
    , NodeToClientVersion (..)
    , NodeToClientVersionData (..)
    )
import Ouroboros.Network.Point
    ( WithOrigin (..) )

import qualified Cardano.Address.Style.Shelley as CA
import qualified Cardano.Api as Cardano
import qualified Cardano.Api.Byron as Cardano
    ( Tx (ByronTx) )
import qualified Cardano.Api.Shelley as Cardano
import qualified Cardano.Binary as Binary
import qualified Cardano.Byron.Codec.Cbor as CBOR
import qualified Cardano.Chain.Common as Byron
import qualified Cardano.Crypto.Hash as Crypto
import qualified Cardano.Ledger.Address as SL
import qualified Cardano.Ledger.Alonzo as Alonzo
import qualified Cardano.Ledger.Alonzo.Data as Alonzo
import qualified Cardano.Ledger.Alonzo.PParams as Alonzo
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import qualified Cardano.Ledger.Alonzo.TxBody as Alonzo
import qualified Cardano.Ledger.Alonzo.TxSeq as Alonzo
import qualified Cardano.Ledger.BaseTypes as Ledger
import qualified Cardano.Ledger.BaseTypes as SL
import qualified Cardano.Ledger.Core as SL.Core
import qualified Cardano.Ledger.Credential as SL
import qualified Cardano.Ledger.Crypto as SL
import qualified Cardano.Ledger.Era as Ledger.Era
import qualified Cardano.Ledger.SafeHash as SafeHash
import qualified Cardano.Ledger.Shelley as SL
import qualified Cardano.Ledger.Shelley.Constraints as SL
import qualified Cardano.Ledger.ShelleyMA as MA
import qualified Cardano.Ledger.ShelleyMA.AuxiliaryData as MA
import qualified Cardano.Ledger.ShelleyMA.TxBody as MA
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Cardano.Wallet.Primitive.Types.Address as W
import qualified Cardano.Wallet.Primitive.Types.Coin as W
import qualified Cardano.Wallet.Primitive.Types.Hash as W
import qualified Cardano.Wallet.Primitive.Types.RewardAccount as W
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.TokenPolicy as W
import qualified Cardano.Wallet.Primitive.Types.TokenQuantity as W
import qualified Cardano.Wallet.Primitive.Types.Tx as W
import qualified Codec.Binary.Bech32 as Bech32
import qualified Codec.Binary.Bech32.TH as Bech32
import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Write as CBOR
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Short as SBS
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text.Encoding as T
import qualified Ouroboros.Consensus.Shelley.Eras as O
import qualified Ouroboros.Consensus.Shelley.Ledger as O
import qualified Ouroboros.Network.Block as O
import qualified Ouroboros.Network.Point as Point
import qualified Shelley.Spec.Ledger.API as SL
import qualified Shelley.Spec.Ledger.API as SLAPI
import qualified Shelley.Spec.Ledger.BlockChain as SL
import qualified Shelley.Spec.Ledger.UTxO as SL

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
            W.SlotNo 0
        , blockHeight =
            Quantity 0
        , headerHash =
            coerce $ W.getGenesisBlockHash gp
        , parentHeaderHash =
            hashOfNoParent
        }
    }

--------------------------------------------------------------------------------
--
-- Network Parameters

-- | The protocol client version. Distinct from the codecs version.
nodeToClientVersions :: [NodeToClientVersion]
nodeToClientVersions = [NodeToClientV_8, NodeToClientV_9]

--------------------------------------------------------------------------------
--
-- Type Conversions

-- | Magic value for the absence of a block.
hashOfNoParent :: W.Hash "BlockHeader"
hashOfNoParent =
    W.Hash . BS.pack $ replicate 32 0

toCardanoHash :: W.Hash "BlockHeader" -> OneEraHash (CardanoEras sc)
toCardanoHash (W.Hash bytes) =
    OneEraHash $ toShort bytes

toPoint
    :: W.Hash "Genesis"
    -> W.BlockHeader
    -> Point (CardanoBlock sc)
toPoint genesisH (W.BlockHeader sl _ (W.Hash h) _)
  | h == (coerce genesisH) = O.GenesisPoint
  | otherwise = O.BlockPoint sl (OneEraHash $ toShort h)

toCardanoBlockHeader
    :: forall c. Era (SL.ShelleyEra c)
    => W.GenesisParameters
    -> CardanoBlock c
    -> W.BlockHeader
toCardanoBlockHeader gp = \case
    BlockByron blk ->
        toByronBlockHeader gp blk
    BlockShelley blk ->
        toShelleyBlockHeader (W.getGenesisBlockHash gp) blk
    BlockAllegra blk ->
        toShelleyBlockHeader (W.getGenesisBlockHash gp) blk
    BlockMary blk ->
        toShelleyBlockHeader (W.getGenesisBlockHash gp) blk
    BlockAlonzo blk ->
        toShelleyBlockHeader (W.getGenesisBlockHash gp) blk

toShelleyBlockHeader
    :: (Era e, ToCBORGroup (Ledger.Era.TxSeq e))
    => W.Hash "Genesis"
    -> ShelleyBlock e
    -> W.BlockHeader
toShelleyBlockHeader genesisHash blk =
    let
        ShelleyBlock (SL.Block (SL.BHeader header _) _) headerHash = blk
    in
        W.BlockHeader
            { slotNo =
                SL.bheaderSlotNo header
            , blockHeight =
                fromBlockNo $ SL.bheaderBlockNo header
            , headerHash =
                fromShelleyHash headerHash
            , parentHeaderHash =
                fromPrevHash (coerce genesisHash) $
                    SL.bheaderPrev header
            }

getProducer
    :: (Era e, ToCBORGroup (Ledger.Era.TxSeq e))
    => ShelleyBlock e -> W.PoolId
getProducer (ShelleyBlock (SL.Block (SL.BHeader header _) _) _) =
    fromPoolKeyHash $ SL.hashKey (SL.bheaderVk header)

fromCardanoBlock
    :: W.GenesisParameters
    -> CardanoBlock StandardCrypto
    -> W.Block
fromCardanoBlock gp = \case
    BlockByron blk ->
        fromByronBlock gp blk
    BlockShelley blk ->
        fst $ fromShelleyBlock gp blk
    BlockAllegra blk ->
        fst $ fromAllegraBlock gp blk
    BlockMary blk ->
        fst $ fromMaryBlock gp blk
    BlockAlonzo blk ->
        fst $ fromAlonzoBlock gp blk

toCardanoEra :: CardanoBlock c -> AnyCardanoEra
toCardanoEra = \case
    BlockByron{}   -> AnyCardanoEra ByronEra
    BlockShelley{} -> AnyCardanoEra ShelleyEra
    BlockAllegra{} -> AnyCardanoEra AllegraEra
    BlockMary{}    -> AnyCardanoEra MaryEra
    BlockAlonzo{}  -> AnyCardanoEra AlonzoEra

fromShelleyBlock
    :: W.GenesisParameters
    -> ShelleyBlock (SL.ShelleyEra StandardCrypto)
    -> (W.Block, [W.PoolCertificate])
fromShelleyBlock gp blk@(ShelleyBlock (SL.Block _ (SL.TxSeq txs')) _) =
    let
       (txs, dlgCerts, poolCerts) = unzip3 $ map fromShelleyTx $ toList txs'
    in
        ( W.Block
            { header = toShelleyBlockHeader (W.getGenesisBlockHash gp) blk
            , transactions = txs
            , delegations  = mconcat dlgCerts
            }
        , mconcat poolCerts
        )

fromAllegraBlock
    :: W.GenesisParameters
    -> ShelleyBlock (MA.ShelleyMAEra 'MA.Allegra StandardCrypto)
    -> (W.Block, [W.PoolCertificate])
fromAllegraBlock gp blk@(ShelleyBlock (SL.Block _ (SL.TxSeq txs')) _) =
    let
       (txs, dlgCerts, poolCerts) = unzip3 $ map fromAllegraTx $ toList txs'
    in
        ( W.Block
            { header = toShelleyBlockHeader (W.getGenesisBlockHash gp) blk
            , transactions = txs
            , delegations  = mconcat dlgCerts
            }
        , mconcat poolCerts
        )

fromMaryBlock
    :: W.GenesisParameters
    -> ShelleyBlock (MA.ShelleyMAEra 'MA.Mary StandardCrypto)
    -> (W.Block, [W.PoolCertificate])
fromMaryBlock gp blk@(ShelleyBlock (SL.Block _ (SL.TxSeq txs')) _) =
    let
       (txs, dlgCerts, poolCerts) = unzip3 $ map fromMaryTx $ toList txs'
    in
        ( W.Block
            { header = toShelleyBlockHeader (W.getGenesisBlockHash gp) blk
            , transactions = txs
            , delegations  = mconcat dlgCerts
            }
        , mconcat poolCerts
        )

-- TODO: We could use the cardano-api `Block` pattern to very elegently get the
-- header and txs of any era block.
--
-- We would need to remove the previous block hash from our `W.BlockHeader`,
-- which shouldn't be needed modulo some hacks w.r.t. the genesis point which
-- would need to be cleaned up too. We probably will need to use `Point block`,
-- in all chain followers (including the DBLayer).
fromAlonzoBlock
    :: W.GenesisParameters
    -> ShelleyBlock (Alonzo.AlonzoEra StandardCrypto)
    -> (W.Block, [W.PoolCertificate])
fromAlonzoBlock gp blk@(ShelleyBlock (SL.Block _ txSeq) _) =
    let
        Alonzo.TxSeq txs' = txSeq
        (txs, dlgCerts, poolCerts) = unzip3 $ map fromAlonzoValidatedTx $ toList txs'
    in
        ( W.Block
            { header = toShelleyBlockHeader (W.getGenesisBlockHash gp) blk
            , transactions = txs
            , delegations  = mconcat dlgCerts
            }
        , mconcat poolCerts
        )

fromShelleyHash :: ShelleyHash c -> W.Hash "BlockHeader"
fromShelleyHash (ShelleyHash (SL.HashHeader h)) = W.Hash (hashToBytes h)

fromCardanoHash :: O.HeaderHash (CardanoBlock sc) -> W.Hash "BlockHeader"
fromCardanoHash = W.Hash . fromShort . getOneEraHash

fromPrevHash
    :: W.Hash "BlockHeader"
    -> SL.PrevHash sc
    -> W.Hash "BlockHeader"
fromPrevHash genesisHash = \case
    SL.GenesisHash -> genesisHash
    SL.BlockHash h -> fromShelleyHash (ShelleyHash h)

fromChainHash
    :: W.Hash "Genesis"
    -> ChainHash (CardanoBlock sc)
    -> W.Hash "BlockHeader"
fromChainHash genesisHash = \case
    O.GenesisHash -> coerce genesisHash
    O.BlockHash (OneEraHash h) -> W.Hash $ fromShort h

-- FIXME unsafe conversion (Word64 -> Word32)
fromBlockNo :: BlockNo -> Quantity "block" Word32
fromBlockNo (BlockNo h) =
    Quantity (fromIntegral h)

fromTip' :: W.GenesisParameters -> Tip (CardanoBlock sc) -> W.BlockHeader
fromTip' gp = fromTip (W.getGenesisBlockHash gp)

fromTip
    :: W.Hash "Genesis"
    -> Tip (CardanoBlock sc)
    -> W.BlockHeader
fromTip genesisHash tip = case getPoint (getTipPoint tip) of
    Origin -> W.BlockHeader
        { slotNo = W.SlotNo 0
        , blockHeight = Quantity 0
        , headerHash = coerce genesisHash
        , parentHeaderHash = hashOfNoParent
        }
    At blk -> W.BlockHeader
        { slotNo = Point.blockPointSlot blk
        , blockHeight = fromBlockNo $ getLegacyTipBlockNo tip
        , headerHash = fromCardanoHash $ Point.blockPointHash blk
        -- TODO: parentHeaderHash could be removed.
        , parentHeaderHash = W.Hash "parentHeaderHash - unused in Shelley"
        }
  where
    -- TODO: This function was marked deprecated in ouroboros-network.
    -- It is wrong, because `Origin` doesn't have a block number.
    -- We should remove it.
    getLegacyTipBlockNo t = case O.getTipBlockNo t of
        Origin -> BlockNo 0
        At x -> x

-- NOTE: Unsafe conversion from Natural -> Word16
fromMaxSize :: Natural -> Quantity "byte" Word16
fromMaxSize =
    Quantity . fromIntegral

fromShelleyPParams
    :: HasCallStack
    => W.EraInfo Bound
    -> SLAPI.PParams era
    -> W.ProtocolParameters
fromShelleyPParams eraInfo pp = W.ProtocolParameters
    { decentralizationLevel =
        decentralizationLevelFromPParams pp
    , txParameters =
        txParametersFromPParams maryTokenBundleMaxSize pp
    , desiredNumberOfStakePools =
        desiredNumberOfStakePoolsFromPParams pp
    , minimumUTxOvalue =
        MinimumUTxOValue . toWalletCoin $ SLAPI._minUTxOValue pp
    , stakeKeyDeposit = stakeKeyDepositFromPParams pp
    , eras = fromBound <$> eraInfo
    , maximumCollateralInputCount = minBound
    , executionUnitPrices = Nothing
    }
  where
    fromBound (Bound _relTime _slotNo (EpochNo e)) =
        W.EpochNo $ fromIntegral e

fromAlonzoPParams
    :: HasCallStack
    => W.EraInfo Bound
    -> Alonzo.PParams StandardAlonzo
    -> W.ProtocolParameters
fromAlonzoPParams eraInfo pp = W.ProtocolParameters
    { decentralizationLevel =
        decentralizationLevelFromPParams pp
    , txParameters = txParametersFromPParams
        (W.TokenBundleMaxSize $ W.TxSize $ Alonzo._maxValSize pp)
        pp
    , desiredNumberOfStakePools =
        desiredNumberOfStakePoolsFromPParams pp
    , minimumUTxOvalue = MinimumUTxOValueCostPerWord
        . toWalletCoin $ Alonzo._coinsPerUTxOWord pp
    , stakeKeyDeposit = stakeKeyDepositFromPParams pp
    , eras = fromBound <$> eraInfo
    , maximumCollateralInputCount = unsafeIntToWord $
        Alonzo._maxCollateralInputs pp
    , executionUnitPrices =
        Just $ executionUnitPricesFromPParams pp
    }
  where
    fromBound (Bound _relTime _slotNo (EpochNo e)) =
        W.EpochNo $ fromIntegral e

-- | Extract the current network decentralization level from the given set of
-- protocol parameters.
--
-- According to the Design Specification for Delegation and Incentives in
-- Cardano, the decentralization parameter __/d/__ is a value in the range
-- '[0, 1]', where:
--
--   * __/d/__ = '1' indicates that the network is /completely federalized/.
--   * __/d/__ = '0' indicates that the network is /completely decentralized/.
--
-- However, in Cardano Wallet, we represent the decentralization level as a
-- percentage, where:
--
--   * '  0 %' indicates that the network is /completely federalized/.
--   * '100 %' indicates that the network is /completely decentralized/.
--
-- Therefore, we must invert the value provided by cardano-node before we
-- convert it into a percentage.
--
decentralizationLevelFromPParams
    :: HasField "_d" pparams SL.UnitInterval
    => pparams
    -> W.DecentralizationLevel
decentralizationLevelFromPParams pp =
    W.DecentralizationLevel $ fromUnitInterval
        -- We must invert the value provided: (see function comment)
        $ invertUnitInterval d
  where
    d = getField @"_d" pp

executionUnitPricesFromPParams
    :: HasField "_prices" pparams Alonzo.Prices
    => pparams
    -> W.ExecutionUnitPrices
executionUnitPricesFromPParams pp =
    fromAlonzoPrices prices
  where
    prices = getField @"_prices" pp
    fromAlonzoPrices (Alonzo.Prices prSteps prMem) =
        W.ExecutionUnitPrices
        { W.priceExecutionSteps  = Ledger.unboundRational prSteps
        , W.priceExecutionMemory = Ledger.unboundRational prMem
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
    :: HasField "_minfeeA" pparams Natural
    => HasField "_minfeeB" pparams Natural
    => HasField "_maxTxSize" pparams Natural
    => W.TokenBundleMaxSize
    -> pparams
    -> W.TxParameters
txParametersFromPParams maxBundleSize pp = W.TxParameters
    { getFeePolicy = W.LinearFee
        (Quantity (naturalToDouble (getField @"_minfeeB" pp)))
        (Quantity (naturalToDouble (getField @"_minfeeA" pp)))
    , getTxMaxSize = fromMaxSize $ getField @"_maxTxSize" pp
    , getTokenBundleMaxSize = maxBundleSize
    }
  where
    naturalToDouble :: Natural -> Double
    naturalToDouble = fromIntegral

desiredNumberOfStakePoolsFromPParams
    :: HasField "_nOpt" pparams Natural
    => pparams
    -> Word16
desiredNumberOfStakePoolsFromPParams pp = fromIntegral $ getField @"_nOpt" pp

stakeKeyDepositFromPParams
    :: HasField "_keyDeposit" pparams SLAPI.Coin
    => pparams
    -> W.Coin
stakeKeyDepositFromPParams = toWalletCoin . getField @"_keyDeposit"

slottingParametersFromGenesis
    :: ShelleyGenesis e
    -> W.SlottingParameters
slottingParametersFromGenesis g =
    W.SlottingParameters
        { getSlotLength =
            W.SlotLength $ sgSlotLength g
        , getEpochLength =
            W.EpochLength . fromIntegral . unEpochSize . sgEpochLength $ g
        , getActiveSlotCoefficient =
            W.ActiveSlotCoefficient . fromRational . SL.unboundRational . sgActiveSlotsCoeff $ g
        , getSecurityParameter =
            Quantity . fromIntegral . sgSecurityParam $ g
        }

-- | Convert genesis data into blockchain params and an initial set of UTxO
fromGenesisData
    :: forall e crypto. (Era e, e ~ SL.ShelleyEra crypto)
    => ShelleyGenesis e
    -> [(SL.Addr crypto, SL.Coin)]
    -> (W.NetworkParameters, W.Block)
fromGenesisData g initialFunds =
    ( W.NetworkParameters
        { genesisParameters = W.GenesisParameters
            { getGenesisBlockHash = dummyGenesisHash
            , getGenesisBlockDate =
                W.StartTime . sgSystemStart $ g
            }
        , slottingParameters =
            slottingParametersFromGenesis g
        , protocolParameters =
            (fromShelleyPParams W.emptyEraInfo) . sgProtocolParams $ g
        }
    , genesisBlockFromTxOuts initialFunds
    )
  where
    -- TODO: There is not yet any agreed upon definition of a
    -- genesis hash for a shelley-only testnet.
    --
    -- For now we use a dummy value.
    dummyGenesisHash = W.Hash . BS.pack $ replicate 32 1


    -- | Construct a ("fake") genesis block from genesis transaction outputs.
    --
    -- The genesis data on haskell nodes is not a block at all, unlike the
    -- block0 on jormungandr. This function is a method to deal with the
    -- discrepancy.
    genesisBlockFromTxOuts :: [(SL.Addr crypto, SL.Coin)] -> W.Block
    genesisBlockFromTxOuts outs = W.Block
        { delegations  = []
        , header = W.BlockHeader
            { slotNo =
                W.SlotNo 0
            , blockHeight =
                Quantity 0
            , headerHash =
                dummyGenesisHash
            , parentHeaderHash =
                W.Hash (BS.replicate 32 0)
            }
        , transactions = mkTx <$> outs
        }
      where
        mkTx (addr, c) = W.Tx
            { txId = pseudoHash
            , fee = Nothing
            , resolvedCollateral = []
            , resolvedInputs = []
            , outputs =
                [W.TxOut
                    (fromShelleyAddress addr)
                    (TokenBundle.fromCoin $ fromShelleyCoin c)
                ]
            , withdrawals = mempty
            , metadata = Nothing
            , scriptValidity = Nothing
            }
          where
            W.TxIn pseudoHash _ = fromShelleyTxIn $
                SL.initialFundsPseudoTxIn @crypto addr

--
-- Stake pools
--

fromPoolId :: forall crypto. SL.KeyHash 'SL.StakePool crypto -> W.PoolId
fromPoolId (SL.KeyHash x) = W.PoolId $ hashToBytes x

fromPoolDistr
    :: forall crypto. ()
    => SL.PoolDistr crypto
    -> Map W.PoolId Percentage
fromPoolDistr =
    Map.map (unsafeMkPercentage . SL.individualPoolStake)
    . Map.mapKeys fromPoolId
    . SL.unPoolDistr

-- NOTE: This function disregards results that are using staking keys
fromNonMyopicMemberRewards
    :: forall era. ()
    => O.NonMyopicMemberRewards era
    -> Map (Either W.Coin W.RewardAccount) (Map W.PoolId W.Coin)
fromNonMyopicMemberRewards =
    Map.map (Map.map toWalletCoin . Map.mapKeys fromPoolId)
    . Map.mapKeys (bimap fromShelleyCoin fromStakeCredential)
    . O.unNonMyopicMemberRewards

optimumNumberOfPools
    :: HasField "_nOpt" pparams Natural
    => pparams
    -> Int
optimumNumberOfPools = unsafeConvert . getField @"_nOpt"
  where
    -- A value of ~100 can be expected, so should be fine.
    unsafeConvert :: Natural -> Int
    unsafeConvert = fromIntegral

--
-- Txs
--

fromShelleyTxId :: SL.TxId crypto -> W.Hash "Tx"
fromShelleyTxId (SL.TxId h) =
    W.Hash $ Crypto.hashToBytes $ SafeHash.extractHash h

fromShelleyTxIn
    :: SL.Crypto crypto
    => SL.TxIn crypto
    -> W.TxIn
fromShelleyTxIn (SL.TxIn txid ix) =
    W.TxIn (fromShelleyTxId txid) (unsafeCast ix)
  where
    unsafeCast :: Natural -> Word32
    unsafeCast = fromIntegral

fromShelleyTxOut
    :: ( SL.ShelleyBased era
       , SL.Core.Value era ~ SL.Coin
       )
    => SLAPI.TxOut era
    -> W.TxOut
fromShelleyTxOut (SLAPI.TxOut addr amount) = W.TxOut
    (fromShelleyAddress addr)
    (TokenBundle.fromCoin $ fromShelleyCoin amount)

fromShelleyAddress :: SL.Addr crypto -> W.Address
fromShelleyAddress = W.Address
    . SL.serialiseAddr

fromShelleyCoin :: SL.Coin -> W.Coin
fromShelleyCoin (SL.Coin c) = W.Coin $ unsafeCast c
  where
    -- (but probably safe)
    unsafeCast :: Integer -> Word64
    unsafeCast = fromIntegral

toShelleyCoin :: W.Coin -> SL.Coin
toShelleyCoin (W.Coin c) = SL.Coin $ safeCast c
  where
    safeCast :: Word64 -> Integer
    safeCast = fromIntegral

-- NOTE: For resolved inputs we have to pass in a dummy value of 0.
fromShelleyTx
    :: SLAPI.Tx (Cardano.ShelleyLedgerEra ShelleyEra)
    -> ( W.Tx
       , [W.DelegationCertificate]
       , [W.PoolCertificate]
       )
fromShelleyTx tx =
    ( W.Tx
        { txId =
            fromShelleyTxId $ SL.txid @(Cardano.ShelleyLedgerEra ShelleyEra) bod
        , fee =
            Just $ fromShelleyCoin fee
        , resolvedCollateral =
            []
        , resolvedInputs =
            map ((,W.Coin 0) . fromShelleyTxIn) (toList ins)
        , outputs =
            map fromShelleyTxOut (toList outs)
        , withdrawals =
            fromShelleyWdrl wdrls
        , metadata =
            fromShelleyMD <$> SL.strictMaybeToMaybe mmd
        , scriptValidity =
            Nothing
        }
    , mapMaybe fromShelleyDelegationCert (toList certs)
    , mapMaybe fromShelleyRegistrationCert (toList certs)
    )
  where
    SL.Tx bod@(SL.TxBody ins outs certs wdrls fee _ _ _) _ mmd = tx

fromAllegraTx
    :: SLAPI.Tx (Cardano.ShelleyLedgerEra AllegraEra)
    -> ( W.Tx
       , [W.DelegationCertificate]
       , [W.PoolCertificate]
       )
fromAllegraTx tx =
    ( W.Tx
        { txId =
            fromShelleyTxId $ SL.txid @(Cardano.ShelleyLedgerEra AllegraEra) bod
        , fee =
            Just $ fromShelleyCoin fee
        , resolvedCollateral =
            -- TODO: (ADP-957)
            []
        , resolvedInputs =
            map ((,W.Coin 0) . fromShelleyTxIn) (toList ins)
        , outputs =
            map fromShelleyTxOut (toList outs)
        , withdrawals =
            fromShelleyWdrl wdrls
        , metadata =
            fromShelleyMD . toSLMetadata <$> SL.strictMaybeToMaybe mmd
        , scriptValidity =
            Nothing
        }
    , mapMaybe fromShelleyDelegationCert (toList certs)
    , mapMaybe fromShelleyRegistrationCert (toList certs)
    )
  where
    SL.Tx bod@(MA.TxBody ins outs certs wdrls fee _ _ _ _) _ mmd = tx

    -- fixme: [ADP-525] It is fine for now since we do not look at script
    -- pre-images. But this is precisely what we want as part of the
    -- multisig/script balance reporting.
    toSLMetadata (MA.AuxiliaryData blob _scripts) = SL.Metadata blob

fromMaryTx
    :: SLAPI.Tx (Cardano.ShelleyLedgerEra MaryEra)
    -> ( W.Tx
       , [W.DelegationCertificate]
       , [W.PoolCertificate]
       )
fromMaryTx tx =
    ( W.Tx
        { txId
            = fromShelleyTxId $ SL.txid @(Cardano.ShelleyLedgerEra MaryEra) bod
        , fee =
            Just $ fromShelleyCoin fee
        , resolvedCollateral =
            []
        , resolvedInputs =
            map ((,W.Coin 0) . fromShelleyTxIn) (toList ins)
        , outputs =
            map fromMaryTxOut (toList outs)
        , withdrawals =
            fromShelleyWdrl wdrls
        , metadata =
            fromShelleyMD . toSLMetadata <$> SL.strictMaybeToMaybe mad
        , scriptValidity =
            Nothing
        }
    , mapMaybe fromShelleyDelegationCert (toList certs)
    , mapMaybe fromShelleyRegistrationCert (toList certs)
    )
  where
    SL.Tx bod _wits mad = tx
    MA.TxBody ins outs certs wdrls fee _valid _upd _adh _value = bod

    -- fixme: [ADP-525] It is fine for now since we do not look at script
    -- pre-images. But this is precisely what we want as part of the
    -- multisig/script balance reporting.
    toSLMetadata (MA.AuxiliaryData blob _scripts) = SL.Metadata blob

    fromMaryTxOut
         :: SLAPI.TxOut (Cardano.ShelleyLedgerEra MaryEra)
         -> W.TxOut
    fromMaryTxOut (SL.TxOut addr value) =
        W.TxOut (fromShelleyAddress addr) $
        fromCardanoValue $ Cardano.fromMaryValue value

fromAlonzoTxBodyAndAux
    :: Alonzo.TxBody (Cardano.ShelleyLedgerEra AlonzoEra)
    -> SLAPI.StrictMaybe (Alonzo.AuxiliaryData (Cardano.ShelleyLedgerEra AlonzoEra))
    -> ( W.Tx
       , [W.DelegationCertificate]
       , [W.PoolCertificate]
       )
fromAlonzoTxBodyAndAux bod mad =
    ( W.Tx
        { txId =
            fromShelleyTxId $ SL.txid @(Cardano.ShelleyLedgerEra AlonzoEra) bod
        , fee =
            Just $ fromShelleyCoin fee
        , resolvedCollateral =
            -- TODO: (ADP-957)
            []
        , resolvedInputs =
            map ((,W.Coin 0) . fromShelleyTxIn) (toList ins)
        , outputs =
            map fromAlonzoTxOut (toList outs)
        , withdrawals =
            fromShelleyWdrl wdrls
        , metadata =
            fromShelleyMD . toSLMetadata <$> SL.strictMaybeToMaybe mad
        , scriptValidity =
            Nothing
        }
    , mapMaybe fromShelleyDelegationCert (toList certs)
    , mapMaybe fromShelleyRegistrationCert (toList certs)
    )
  where
    Alonzo.TxBody
        ins
        _collateral
        outs
        certs
        wdrls
        fee
        _valid
        _upd
        _reqSignerHashes
        _mint
        _wwpHash
        _adHash
        _network
        = bod

    fromAlonzoTxOut
         :: Alonzo.TxOut (Cardano.ShelleyLedgerEra AlonzoEra)
         -> W.TxOut
    fromAlonzoTxOut (Alonzo.TxOut addr value _) =
        W.TxOut (fromShelleyAddress addr) $
        fromCardanoValue $ Cardano.fromMaryValue value

    toSLMetadata (Alonzo.AuxiliaryData blob _scripts) = SL.Metadata blob

fromAlonzoValidatedTx
    :: Alonzo.ValidatedTx (Cardano.ShelleyLedgerEra AlonzoEra)
    -> ( W.Tx
       , [W.DelegationCertificate]
       , [W.PoolCertificate]
       )
fromAlonzoValidatedTx (Alonzo.ValidatedTx bod _wits _isValidating aux) =
    fromAlonzoTxBodyAndAux bod aux

fromAlonzoTx
    :: Alonzo.ValidatedTx (Cardano.ShelleyLedgerEra AlonzoEra)
    -> ( W.Tx
       , [W.DelegationCertificate]
       , [W.PoolCertificate]
       )
fromAlonzoTx (Alonzo.ValidatedTx bod _wits (Alonzo.IsValid isValid) aux) =
    (\(tx, d, p) -> (tx { W.scriptValidity = validity }, d, p))
    $ fromAlonzoTxBodyAndAux bod aux
    where
        validity =
            if isValid
            then Just W.TxScriptValid
            else Just W.TxScriptInvalid

fromCardanoValue :: HasCallStack => Cardano.Value -> TokenBundle.TokenBundle
fromCardanoValue = uncurry TokenBundle.fromFlatList . extract
  where
    extract value =
        ( mkCoin $ Cardano.selectLovelace value
        , mkBundle $ Cardano.valueToList value
        )

    -- Lovelace to coin. Quantities from ledger should always fit in Word64.
    mkCoin :: Cardano.Lovelace -> W.Coin
    mkCoin = W.Coin . unsafeIntToWord . unQuantity . Cardano.lovelaceToQuantity

    -- Do Integer to Natural conversion. Quantities from ledger TxOuts can
    -- never be negative (but unminted values could be negative).
    mkQuantity :: Integer -> W.TokenQuantity
    mkQuantity = W.TokenQuantity . checkBounds
      where
        checkBounds n
          | n >= 0 = fromIntegral n
          | otherwise = internalError "negative token quantity"

    mkBundle assets =
        [ (TokenBundle.AssetId (mkPolicyId p) (mkTokenName n) , mkQuantity q)
        | (Cardano.AssetId p n, Cardano.Quantity q) <- assets
        ]

    mkPolicyId = W.UnsafeTokenPolicyId . W.Hash . Cardano.serialiseToRawBytes
    mkTokenName = W.UnsafeTokenName . Cardano.serialiseToRawBytes

    unQuantity (Cardano.Quantity q) = q

fromShelleyWdrl :: SL.Wdrl crypto -> Map W.RewardAccount W.Coin
fromShelleyWdrl (SL.Wdrl wdrl) = Map.fromList $
    bimap (fromStakeCredential . SL.getRwdCred) fromShelleyCoin
        <$> Map.toList wdrl

fromShelleyMD :: SL.Metadata c -> Cardano.TxMetadata
fromShelleyMD (SL.Metadata m) =
    Cardano.makeTransactionMetadata . fromShelleyMetadata $ m

-- Convert & filter Shelley certificate into delegation certificate. Returns
-- 'Nothing' if certificates aren't delegation certificate.
fromShelleyDelegationCert
    :: SL.DCert crypto
    -> Maybe W.DelegationCertificate
fromShelleyDelegationCert = \case
    SL.DCertDeleg (SL.Delegate delegation)  ->
        Just $ W.CertDelegateFull
            (fromStakeCredential (SL._delegator delegation))
            (fromPoolKeyHash (SL._delegatee delegation))

    SL.DCertDeleg (SL.DeRegKey credentials) ->
        Just $ W.CertDelegateNone (fromStakeCredential credentials)

    SL.DCertDeleg (SL.RegKey cred) ->
        Just $ W.CertRegisterKey $ fromStakeCredential cred
    SL.DCertPool{}            -> Nothing
    SL.DCertGenesis{}         -> Nothing
    SL.DCertMir{}             -> Nothing

-- Convert & filter Shelley certificate into delegation certificate. Returns
-- 'Nothing' if certificates aren't delegation certificate.
fromShelleyRegistrationCert
    :: SL.DCert crypto
    -> Maybe (W.PoolCertificate)
fromShelleyRegistrationCert = \case
    SL.DCertPool (SL.RegPool pp) -> Just $ Registration
        ( W.PoolRegistrationCertificate
            { W.poolId = fromPoolKeyHash $ SL._poolId pp
            , W.poolOwners = fromOwnerKeyHash <$> Set.toList (SL._poolOwners pp)
            , W.poolMargin = fromUnitInterval (SL._poolMargin pp)
            , W.poolCost = toWalletCoin (SL._poolCost pp)
            , W.poolPledge = toWalletCoin (SL._poolPledge pp)
            , W.poolMetadata = fromPoolMetadata <$> strictMaybeToMaybe (SL._poolMD pp)
            }
        )

    SL.DCertPool (SL.RetirePool pid (EpochNo e)) ->
        Just $ Retirement $ PoolRetirementCertificate (fromPoolKeyHash pid)
        (W.EpochNo $ fromIntegral e)

    SL.DCertDeleg{}   -> Nothing
    SL.DCertGenesis{} -> Nothing
    SL.DCertMir{}     -> Nothing

toWalletCoin :: HasCallStack => SL.Coin -> W.Coin
toWalletCoin = W.Coin . unsafeCoinToWord64

-- | The reverse of 'word64ToCoin', where overflow is fatal.
unsafeCoinToWord64 :: HasCallStack => SL.Coin -> Word64
unsafeCoinToWord64 (SL.Coin c) = unsafeIntToWord c

-- | Convert an integer type of any range to a machine word.
--
-- Only use it for values which have come from the ledger, and should fit in the
-- given type, according to the spec.
--
-- If this conversion would under/overflow, there is not much we can do except
-- to hastily exit.
unsafeIntToWord
    :: forall from to
     . ( HasCallStack
       , Integral from
       , Bounded to
       , Integral to
       , Typeable from
       , Typeable to
       , Show from)
    => from -> to
unsafeIntToWord n
    | n < fromIntegral (minBound :: to) = crash "underflow"
    | n > fromIntegral (maxBound :: to) = crash "overflow"
    | otherwise = fromIntegral n
  where
    crash :: Builder -> to
    crash err = internalError $ err |+" converting value "+|| n ||+
        " from " +|| typeRep (Proxy @from) ||+
        " to "+|| typeRep (Proxy @to) ||+"!"

-- | Calls the 'error' function, which will usually crash the program.
internalError :: HasCallStack => Builder -> a
internalError msg = error $ fmt $ "INTERNAL ERROR: "+|msg

-- | Tests whether an 'Exception' was caused by 'internalError'.
isInternalError :: ErrorCall -> Bool
isInternalError e = "INTERNAL ERROR" `isPrefixOf` displayException e

-- | Take the first 'Just' from a list of 'Maybe', or die trying.
-- There is no alternative.
tina :: HasCallStack => Builder -> [Maybe a] -> a
tina msg = fromMaybe (internalError msg) . asum

fromPoolMetadata :: SL.PoolMetadata -> (W.StakePoolMetadataUrl, W.StakePoolMetadataHash)
fromPoolMetadata meta =
    ( W.StakePoolMetadataUrl (urlToText (SL._poolMDUrl meta))
    , W.StakePoolMetadataHash (SL._poolMDHash meta)
    )


-- | Convert a stake credentials to a 'RewardAccount' type.
--
-- Unlike with Jörmungandr, the reward account payload doesn't represent a
-- public key but a HASH of a public key.
--
fromStakeCredential :: SL.Credential 'SL.Staking crypto -> W.RewardAccount
fromStakeCredential = \case
    SL.ScriptHashObj (SL.ScriptHash h) ->
        W.RewardAccount (hashToBytes h)
    SL.KeyHashObj (SL.KeyHash h) ->
        W.RewardAccount (hashToBytes h)

fromPoolKeyHash :: SL.KeyHash rol sc -> W.PoolId
fromPoolKeyHash (SL.KeyHash h) =
    W.PoolId (hashToBytes h)

fromOwnerKeyHash :: SL.KeyHash 'SL.Staking crypto -> W.PoolOwner
fromOwnerKeyHash (SL.KeyHash h) =
    W.PoolOwner (hashToBytes h)

fromUnitInterval :: HasCallStack => SL.UnitInterval -> Percentage
fromUnitInterval x =
    either bomb id . mkPercentage . toRational . SL.unboundRational $ x
  where
    bomb = internalError $
        "fromUnitInterval: encountered invalid parameter value: "+||x||+""

class ToCardanoGenTx era where
    toCardanoGenTx
        :: forall c. SLAPI.PraosCrypto c
        => W.SealedTx
        -> CardanoGenTx c

instance ToCardanoGenTx Cardano.ShelleyEra where
    toCardanoGenTx tx = parse & fromMaybe
        (error "Wrong deserialisation for ShelleyEra")
      where
        parse = toGenTx tx GenTxShelley

instance ToCardanoGenTx Cardano.AllegraEra where
    toCardanoGenTx tx = parse & fromMaybe
        (error "Wrong deserialisation for AllegraEra")
      where
        parse = toGenTx tx GenTxAllegra
            <|> toGenTx tx GenTxShelley

instance ToCardanoGenTx Cardano.MaryEra where
    toCardanoGenTx tx = parse & fromMaybe
        (error "Wrong deserialisation for MaryEra")
      where
        parse = toGenTx tx GenTxMary
            <|> toGenTx tx GenTxAllegra
            <|> toGenTx tx GenTxShelley

instance ToCardanoGenTx Cardano.AlonzoEra where
    toCardanoGenTx tx = parse & fromMaybe
        (error "Wrong deserialisation for AlonzoEra")
      where
        parse = toGenTx tx GenTxAlonzo
            <|> toGenTx tx GenTxMary
            <|> toGenTx tx GenTxAllegra
            <|> toGenTx tx GenTxShelley

toGenTx
    :: Binary.FromCBOR t
    => W.SealedTx
    -> (t -> CardanoGenTx c)
    -> Maybe (CardanoGenTx c)
toGenTx tx constructor = constructor <$> eitherToMaybe
    (safeDeserialiseCbor fromCBOR $ BL.fromStrict $ W.getSealedTx tx)

sealShelleyTx
    :: forall era b c. (O.ShelleyBasedEra (Cardano.ShelleyLedgerEra era))
    => (SL.Core.Tx (Cardano.ShelleyLedgerEra era) -> (W.Tx, b, c))
    -> Cardano.Tx era
    -> (W.Tx, W.SealedTx)
sealShelleyTx fromTx (Cardano.ShelleyTx _era tx) =
    let
        -- The Cardano.Tx GADT won't allow the Shelley crypto type param escape,
        -- so we convert directly to the concrete wallet Tx type:
        (walletTx, _, _) = fromTx tx
        sealed = serialize' $ O.mkShelleyTx tx
    in
        (walletTx, W.SealedTx sealed)

-- Needed to compile, but in principle should never be called.
sealShelleyTx _ (Cardano.ByronTx txaux) =
    let
        tx = fromTxAux txaux
        inps = fst <$> W.resolvedInputs tx
        outs = W.outputs tx
    in
        (tx, W.SealedTx $ CBOR.toStrictByteString $ CBOR.encodeTx (inps, outs))

toCardanoTxId :: W.Hash "Tx" -> Cardano.TxId
toCardanoTxId (W.Hash h) = Cardano.TxId $ UnsafeHash $ toShort h

toCardanoTxIn :: W.TxIn -> Cardano.TxIn
toCardanoTxIn (W.TxIn tid ix) =
    Cardano.TxIn (toCardanoTxId tid) (Cardano.TxIx (fromIntegral ix))

toCardanoStakeCredential :: W.RewardAccount -> Cardano.StakeCredential
toCardanoStakeCredential = Cardano.StakeCredentialByKey
    . Cardano.StakeKeyHash
    . SL.KeyHash
    . UnsafeHash
    . SBS.toShort
    . W.unRewardAccount

toCardanoLovelace :: W.Coin -> Cardano.Lovelace
toCardanoLovelace (W.Coin c) = Cardano.Lovelace $ safeCast c
  where
    safeCast :: Word64 -> Integer
    safeCast = fromIntegral

toShelleyTxOut :: HasCallStack => W.TxOut -> Cardano.TxOut ShelleyEra
toShelleyTxOut (W.TxOut (W.Address addr) tokens) =
    Cardano.TxOut
        addrInEra
        (adaOnly $ toCardanoLovelace $ TokenBundle.getCoin tokens)
        Cardano.TxOutDatumHashNone
  where
    adaOnly = Cardano.TxOutAdaOnly Cardano.AdaOnlyInShelleyEra
    addrInEra = tina "toCardanoTxOut: malformed address"
        [ Cardano.AddressInEra
            (Cardano.ShelleyAddressInEra Cardano.ShelleyBasedEraShelley)
            <$> deserialiseFromRawBytes AsShelleyAddress addr

        , Cardano.AddressInEra Cardano.ByronAddressInAnyEra
            <$> deserialiseFromRawBytes AsByronAddress addr
        ]

toAllegraTxOut :: HasCallStack => W.TxOut -> Cardano.TxOut AllegraEra
toAllegraTxOut (W.TxOut (W.Address addr) tokens) =
    Cardano.TxOut
        addrInEra
        (adaOnly $ toCardanoLovelace $ TokenBundle.getCoin tokens)
        Cardano.TxOutDatumHashNone
  where
    adaOnly = Cardano.TxOutAdaOnly Cardano.AdaOnlyInAllegraEra
    addrInEra = tina "toCardanoTxOut: malformed address"
        [ Cardano.AddressInEra
            (Cardano.ShelleyAddressInEra Cardano.ShelleyBasedEraAllegra)
            <$> deserialiseFromRawBytes AsShelleyAddress addr

        , Cardano.AddressInEra Cardano.ByronAddressInAnyEra
            <$> deserialiseFromRawBytes AsByronAddress addr
        ]

toMaryTxOut :: HasCallStack => W.TxOut -> Cardano.TxOut MaryEra
toMaryTxOut (W.TxOut (W.Address addr) tokens) =
    Cardano.TxOut
        addrInEra
        (Cardano.TxOutValue Cardano.MultiAssetInMaryEra $ toCardanoValue tokens)
        Cardano.TxOutDatumHashNone
  where
    addrInEra = tina "toCardanoTxOut: malformed address"
        [ Cardano.AddressInEra (Cardano.ShelleyAddressInEra Cardano.ShelleyBasedEraMary)
            <$> deserialiseFromRawBytes AsShelleyAddress addr

        , Cardano.AddressInEra Cardano.ByronAddressInAnyEra
            <$> deserialiseFromRawBytes AsByronAddress addr
        ]

toAlonzoTxOut :: HasCallStack => W.TxOut -> Cardano.TxOut AlonzoEra
toAlonzoTxOut (W.TxOut (W.Address addr) tokens) =
    Cardano.TxOut
        addrInEra
        (Cardano.TxOutValue Cardano.MultiAssetInAlonzoEra $ toCardanoValue tokens)
        datumHash
  where
    datumHash = Cardano.TxOutDatumHashNone
    addrInEra = tina "toCardanoTxOut: malformed address"
        [ Cardano.AddressInEra (Cardano.ShelleyAddressInEra Cardano.ShelleyBasedEraAlonzo)
            <$> deserialiseFromRawBytes AsShelleyAddress addr

        , Cardano.AddressInEra Cardano.ByronAddressInAnyEra
            <$> deserialiseFromRawBytes AsByronAddress addr
        ]

toCardanoValue :: HasCallStack => TokenBundle.TokenBundle -> Cardano.Value
toCardanoValue tb = Cardano.valueFromList $
    (Cardano.AdaAssetId, coinToQuantity coin) :
    map (bimap toCardanoAssetId toQuantity) bundle
  where
    (coin, bundle) = TokenBundle.toFlatList tb
    toCardanoAssetId (TokenBundle.AssetId pid name) =
        Cardano.AssetId (toCardanoPolicyId pid) (toCardanoAssetName name)

    toCardanoPolicyId (W.UnsafeTokenPolicyId (W.Hash pid)) = just "PolicyId"
        [Cardano.deserialiseFromRawBytes Cardano.AsPolicyId pid]
    toCardanoAssetName (W.UnsafeTokenName name) = just "TokenName"
        [Cardano.deserialiseFromRawBytes Cardano.AsAssetName name]

    just :: Builder -> [Maybe a] -> a
    just t = tina ("toCardanoValue: unable to deserialise "+|t)

    coinToQuantity = fromIntegral . W.unCoin
    toQuantity = fromIntegral . W.unTokenQuantity

-- | Convert from reward account address (which is a hash of a public key)
-- to a shelley ledger stake credential.
toStakeCredential :: W.RewardAccount -> SL.StakeCredential crypto
toStakeCredential = SL.KeyHashObj
    . SL.KeyHash . UnsafeHash . toShort . W.unRewardAccount

toStakeKeyDeregCert :: XPub -> Cardano.Certificate
toStakeKeyDeregCert = Cardano.makeStakeAddressDeregistrationCertificate
    . Cardano.StakeCredentialByKey
    . Cardano.StakeKeyHash
    . SL.KeyHash
    . UnsafeHash
    . toShort
    . blake2b224
    . xpubPublicKey

toStakeKeyRegCert :: XPub -> Cardano.Certificate
toStakeKeyRegCert = Cardano.makeStakeAddressRegistrationCertificate
    . Cardano.StakeCredentialByKey
    . Cardano.StakeKeyHash
    . SL.KeyHash
    . UnsafeHash
    . toShort
    . blake2b224
    . xpubPublicKey

toStakePoolDlgCert :: XPub -> W.PoolId -> Cardano.Certificate
toStakePoolDlgCert xpub (W.PoolId pid) =
    Cardano.makeStakeAddressDelegationCertificate
        (Cardano.StakeCredentialByKey $ Cardano.StakeKeyHash cred)
        (Cardano.StakePoolKeyHash pool)
  where
    cred = SL.KeyHash $ UnsafeHash $ toShort $ blake2b224 $ xpubPublicKey xpub
    pool = SL.KeyHash $ UnsafeHash $ toShort pid


-- | Extract a stake reference / `RewardAccount` from an address, if it exists.
--
-- Note that this returns `Nothing` for pointer addresses, not just enterprise
-- addresses.
rewardAccountFromAddress :: W.Address -> Maybe W.RewardAccount
rewardAccountFromAddress (W.Address bytes) = refToAccount . ref =<< parseAddr bytes
  where
    parseAddr :: ByteString -> Maybe (Cardano.Address Cardano.ShelleyAddr)
    parseAddr = Cardano.deserialiseFromRawBytes AsShelleyAddress

    ref :: Cardano.Address Cardano.ShelleyAddr -> SL.StakeReference StandardCrypto
    ref (Cardano.ShelleyAddress _n _paymentKey stakeRef) = stakeRef

    refToAccount :: SL.StakeReference StandardCrypto -> Maybe W.RewardAccount
    refToAccount (SL.StakeRefBase cred) = Just $ fromStakeCredential cred
    refToAccount (SL.StakeRefPtr _) = Nothing
    refToAccount SL.StakeRefNull = Nothing

{-------------------------------------------------------------------------------
                   Assessing sizes of token bundles
-------------------------------------------------------------------------------}

-- | Assesses a token bundle size in relation to the maximum size that can be
--   included in a transaction output.
--
-- See 'W.TokenBundleSizeAssessor' for the expected properties of this function.
--
tokenBundleSizeAssessor :: W.TokenBundleMaxSize -> W.TokenBundleSizeAssessor
tokenBundleSizeAssessor maxSize = W.TokenBundleSizeAssessor {..}
  where
    assessTokenBundleSize tb
        | serializedLengthBytes <= maxSize' =
            W.TokenBundleSizeWithinLimit
        | otherwise =
            W.OutputTokenBundleSizeExceedsLimit
      where
        serializedLengthBytes :: W.TxSize
        serializedLengthBytes = computeTokenBundleSerializedLengthBytes tb

        maxSize' :: W.TxSize
        maxSize' = W.unTokenBundleMaxSize maxSize

computeTokenBundleSerializedLengthBytes :: TokenBundle.TokenBundle -> W.TxSize
computeTokenBundleSerializedLengthBytes = W.TxSize . safeCast
    . BS.length . Binary.serialize' . Cardano.toMaryValue . toCardanoValue
  where
    safeCast :: Int -> Natural
    safeCast = fromIntegral

{-------------------------------------------------------------------------------
                      Address Encoding / Decoding
-------------------------------------------------------------------------------}

instance EncodeStakeAddress 'Mainnet where
    encodeStakeAddress = _encodeStakeAddress SL.Mainnet
instance EncodeStakeAddress ('Testnet pm) where
    encodeStakeAddress = _encodeStakeAddress SL.Testnet

instance DecodeStakeAddress 'Mainnet where
    decodeStakeAddress = _decodeStakeAddress SL.Mainnet
instance DecodeStakeAddress ('Testnet pm) where
    decodeStakeAddress = _decodeStakeAddress SL.Testnet

stakeAddressPrefix :: Word8
stakeAddressPrefix = 0xE0

networkIdMask :: Word8
networkIdMask = 0x0F

toNetworkId :: SL.Network -> Word8
toNetworkId = \case
    SL.Testnet -> 0
    SL.Mainnet -> 1

_encodeStakeAddress
    :: SL.Network
    -> W.RewardAccount
    -> Text
_encodeStakeAddress network (W.RewardAccount acct) =
    Bech32.encodeLenient hrp (dataPartFromBytes bytes)
  where
    hrp = case network of
        SL.Testnet -> [Bech32.humanReadablePart|stake_test|]
        SL.Mainnet -> [Bech32.humanReadablePart|stake|]
    bytes = BL.toStrict $ runPut $ do
        putWord8 $ (networkIdMask .&. toNetworkId network) .|. stakeAddressPrefix
        putByteString acct

_decodeStakeAddress
    :: SL.Network
    -> Text
    -> Either TextDecodingError W.RewardAccount
_decodeStakeAddress serverNetwork txt = do
    (_, dp) <- left (const errBech32) $ Bech32.decodeLenient txt
    bytes <- maybe (Left errBech32) Right $ dataPartToBytes dp
    rewardAcnt <- runGetOrFail' (SL.getRewardAcnt @StandardCrypto) bytes

    guardNetwork (SL.getRwdNetwork rewardAcnt) serverNetwork

    pure $ fromStakeCredential $ SL.getRwdCred rewardAcnt
  where
    runGetOrFail' decoder bytes =
        case runGetOrFail decoder (BL.fromStrict bytes) of
            Left e ->
                Left (TextDecodingError (show e))

            Right (remaining,_,_) | not (BL.null remaining) ->
                Left errDecode

            Right (_,_,a) ->
                Right a

    errDecode = TextDecodingError
        "Unable to decode stake-address: not a well-formed address."

    errBech32 = TextDecodingError
        "Unable to decode stake-address: must be a valid bech32 string."

instance EncodeAddress 'Mainnet where
    encodeAddress = _encodeAddress [Bech32.humanReadablePart|addr|]

instance EncodeAddress ('Testnet pm) where
    -- https://github.com/cardano-foundation/CIPs/tree/master/CIP5
    encodeAddress = _encodeAddress [Bech32.humanReadablePart|addr_test|]

_encodeAddress :: Bech32.HumanReadablePart -> W.Address -> Text
_encodeAddress hrp (W.Address bytes) =
    if isJust (CBOR.deserialiseCbor CBOR.decodeAddressPayload bytes)
        then base58
        else bech32
  where
    base58 = T.decodeUtf8 $ encodeBase58 bitcoinAlphabet bytes
    bech32 = Bech32.encodeLenient hrp (dataPartFromBytes bytes)

instance DecodeAddress 'Mainnet where
    decodeAddress = _decodeAddress SL.Mainnet

instance DecodeAddress ('Testnet pm) where
    decodeAddress = _decodeAddress SL.Testnet

decodeBytes :: Text -> Either TextDecodingError ByteString
decodeBytes t =
    case tryBase16 t <|> tryBech32 t <|> tryBase58 t of
        Just bytes ->
            Right bytes
        _ ->
            Left $ TextDecodingError
                "Unrecognized address encoding: must be either bech32, base58 or base16"

-- | Attempt decoding an 'Address' using a Bech32 encoding.
tryBech32 :: Text -> Maybe ByteString
tryBech32 text = do
    (_, dp) <- either (const Nothing) Just (Bech32.decodeLenient text)
    dataPartToBytes dp

-- | Attempt decoding a legacy 'Address' using a Base58 encoding.
tryBase58 :: Text -> Maybe ByteString
tryBase58 text =
    decodeBase58 bitcoinAlphabet (T.encodeUtf8 text)

-- | Attempt decoding an 'Address' using Base16 encoding
tryBase16 :: Text -> Maybe ByteString
tryBase16 text =
    either (const Nothing) Just $ convertFromBase Base16 (T.encodeUtf8 text)

errMalformedAddress :: TextDecodingError
errMalformedAddress = TextDecodingError
    "Unable to decode address: not a well-formed Shelley nor Byron address."

-- Note that for 'Byron', we always assume no discrimination. In
-- practice, there is one discrimination for 'Shelley' addresses, and one for
-- 'Byron' addresses. Yet, on Mainnet, 'Byron' addresses have no explicit
-- discrimination.
_decodeAddress
    :: SL.Network
    -> Text
    -> Either TextDecodingError W.Address
_decodeAddress serverNetwork =
    decodeBytes >=> decodeShelleyAddress @StandardCrypto
  where
    decodeShelleyAddress :: forall c. (SL.Crypto c) => ByteString -> Either TextDecodingError W.Address
    decodeShelleyAddress bytes = do
        case SL.deserialiseAddr @c bytes of
            Just (SL.Addr addrNetwork _ _) -> do
                guardNetwork addrNetwork serverNetwork
                pure (W.Address bytes)

            Just (SL.AddrBootstrap (SL.BootstrapAddress addr)) -> do
                guardNetwork (toNetwork (Byron.addrNetworkMagic addr)) serverNetwork
                pure (W.Address bytes)

            Nothing -> Left errMalformedAddress

      where
        toNetwork :: Byron.NetworkMagic -> SL.Network
        toNetwork = \case
            Byron.NetworkMainOrStage -> SL.Mainnet
            Byron.NetworkTestnet{}   -> SL.Testnet

-- FIXME: 'cardano-addresses' currently gives us an opaque 'Value'. It'd be
-- nicer to model this as a proper Haskell type and to serialize in due times.
inspectAddress
    :: Text
    -> Either TextDecodingError Aeson.Value
inspectAddress =
    decodeBytes >=> inspect
  where
    inspect :: ByteString -> Either TextDecodingError Aeson.Value
    inspect = maybe (Left errMalformedAddress) Right
        . CA.inspectAddress mRootPub
        . unsafeMkAddress
    -- TODO: It's possible to inspect a byron address, given a root XPub.
    -- However, this is not yet exposed by the API.
    mRootPub = Nothing

toHDPayloadAddress :: W.Address -> Maybe Byron.HDAddressPayload
toHDPayloadAddress (W.Address addr) = do
    payload <- CBOR.deserialiseCbor CBOR.decodeAddressPayload addr
    attributes <- CBOR.deserialiseCbor decodeAllAttributes' payload
    case filter (\(tag,_) -> tag == 1) attributes of
        [(1, bytes)] ->
            Byron.HDAddressPayload <$> CBOR.decodeNestedBytes CBOR.decodeBytes bytes
        _ ->
            Nothing
  where
    decodeAllAttributes' = do
        _ <- CBOR.decodeListLenCanonicalOf 3
        _ <- CBOR.decodeBytes
        CBOR.decodeAllAttributes

guardNetwork :: SL.Network -> SL.Network -> Either TextDecodingError ()
guardNetwork addrNetwork serverNetwork =
    when (addrNetwork /= serverNetwork) $
        Left $ TextDecodingError $
            "Invalid network discrimination on address. Expecting "
            <> show serverNetwork
            <> " but got "
            <> show addrNetwork
            <> "."

-- | Class to extract a @NetworkId@ from @NetworkDiscriminant@.
class HasNetworkId (n :: NetworkDiscriminant) where
    networkIdVal :: Proxy n -> NetworkId

instance HasNetworkId 'Mainnet where
    networkIdVal _ = Cardano.Mainnet

instance KnownNat protocolMagic => HasNetworkId ('Testnet protocolMagic) where
    networkIdVal _ = Cardano.Testnet networkMagic
      where
        networkMagic = Cardano.NetworkMagic
            . fromIntegral
            $ natVal (Proxy @protocolMagic)

instance HasNetworkId ('Staging protocolMagic) where
    networkIdVal _ = Cardano.Mainnet

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
