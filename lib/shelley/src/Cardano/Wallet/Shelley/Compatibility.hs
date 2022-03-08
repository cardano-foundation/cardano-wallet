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
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- Orphan instances for {Encode,Decode}Address until we get rid of the
-- Jörmungandr dual support.
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Copyright: © 2020 IOHK
-- License: Apache-2.0
--
-- Conversion functions and static chain settings for Shelley.

module Cardano.Wallet.Shelley.Compatibility
    ( CardanoBlock
    , StandardCrypto
    , StandardShelley

      -- * Protocol Parameters
    , NetworkId (..)
    , NodeToClientVersionData
    , nodeToClientVersions

      -- * Node Connection
    , localNodeConnectInfo

      -- * Genesis
    , emptyGenesis

      -- * Eras
    , AnyCardanoEra (..)
    , AnyShelleyBasedEra (..)
    , CardanoEra (..)
    , ShelleyBasedEra (..)
    , shelleyBasedToCardanoEra
    , shelleyToCardanoEra
    , getShelleyBasedEra

      -- * Conversions
    , toCardanoHash
    , unsealShelleyTx
    , toPoint
    , fromPoint
    , toCardanoTxId
    , toCardanoTxIn
    , toCardanoUTxO
    , fromCardanoTxIn
    , fromCardanoTxOut
    , fromCardanoWdrls
    , cardanoCertKeysForWitnesses
    , toCardanoTxOut
    , toCardanoLovelace
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
    , fromCardanoLovelace
    , rewardAccountFromAddress
    , fromShelleyPParams
    , fromAlonzoPParams
    , fromLedgerExUnits
    , toLedgerExUnits
    , fromLedgerPParams
    , fromLedgerAlonzoPParams
    , toAlonzoPParams
    , fromCardanoAddress
    , toSystemStart
    , toScriptPurpose
    , fromShelleyTxIn
    , toCostModelsAsArray
    , toCardanoPolicyId
    , toCardanoSimpleScript

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
    , toTip
    , fromCardanoTx
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
    , getScriptIntegrityHash
    ) where

import Prelude

import Cardano.Address
    ( unsafeMkAddress )
import Cardano.Address.Derivation
    ( XPub, xpubPublicKey )
import Cardano.Address.Script
    ( KeyHash (..), KeyRole (..), Script (..) )
import Cardano.Api
    ( AllegraEra
    , AlonzoEra
    , AnyCardanoEra (..)
    , AsType (..)
    , CardanoEra (..)
    , CardanoEraStyle (..)
    , CardanoMode
    , ConsensusModeParams (CardanoModeParams)
    , EraInMode (..)
    , InAnyCardanoEra (..)
    , IsCardanoEra (..)
    , LocalNodeConnectInfo (LocalNodeConnectInfo)
    , MaryEra
    , NetworkId
    , ShelleyEra
    , TxInMode (..)
    , cardanoEraStyle
    , deserialiseFromRawBytes
    )
import Cardano.Api.Shelley
    ( InAnyShelleyBasedEra (..)
    , IsShelleyBasedEra (..)
    , ShelleyBasedEra (..)
    , ShelleyGenesis (..)
    , fromShelleyMetadata
    )
import Cardano.Crypto.Hash.Class
    ( Hash (UnsafeHash), hashToBytes )
import Cardano.Launcher.Node
    ( CardanoNodeConn, nodeSocketFile )
import Cardano.Ledger.BaseTypes
    ( strictMaybeToMaybe, urlToText )
import Cardano.Ledger.Era
    ( Era (..) )
import Cardano.Ledger.Serialization
    ( ToCBORGroup )
import Cardano.Slotting.Slot
    ( EpochNo (..), EpochSize (..) )
import Cardano.Slotting.Time
    ( SystemStart (..) )
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
    ( Certificate (..)
    , ChainPoint (..)
    , MinimumUTxOValue (..)
    , PoolCertificate (..)
    , PoolRegistrationCertificate (..)
    , PoolRetirementCertificate (..)
    , ProtocolParameters (txParameters)
    , TxParameters (getTokenBundleMaxSize)
    )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( TokenMap )
import Cardano.Wallet.Primitive.Types.TokenPolicy
    ( TokenPolicyId )
import Cardano.Wallet.Shelley.Compatibility.Ledger
    ( toWalletTokenName, toWalletTokenPolicyId, toWalletTokenQuantity )
import Cardano.Wallet.Transaction
    ( TokenMapWithScripts (..), emptyTokenMapWithScripts )
import Cardano.Wallet.Unsafe
    ( unsafeIntToWord, unsafeMkPercentage )
import Cardano.Wallet.Util
    ( internalError, tina )
import Codec.Binary.Bech32
    ( dataPartFromBytes, dataPartToBytes )
import Control.Applicative
    ( Const (..), (<|>) )
import Control.Arrow
    ( left )
import Control.Monad
    ( when, (>=>) )
import Crypto.Hash.Utils
    ( blake2b224 )
import Data.Array
    ( Array )
import Data.Bifunctor
    ( bimap )
import Data.Binary.Get
    ( runGetOrFail )
import Data.Binary.Put
    ( putByteString, putWord8, runPut )
import Data.Bits
    ( (.&.), (.|.) )
import Data.ByteString
    ( ByteString )
import Data.ByteString.Base58
    ( bitcoinAlphabet, encodeBase58 )
import Data.ByteString.Short
    ( fromShort, toShort )
import Data.Coerce
    ( coerce )
import Data.Foldable
    ( toList )
import Data.Function
    ( (&) )
import Data.IntCast
    ( intCast )
import Data.List
    ( unzip4 )
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
import Data.Type.Equality
    ( (:~:) (..), testEquality )
import Data.Word
    ( Word16, Word32, Word8 )
import Fmt
    ( Buildable (..), Builder, (+|), (+||), (||+) )
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
    , NodeToClientVersionData
    )
import Ouroboros.Network.Point
    ( WithOrigin (..) )

import qualified Cardano.Address as CA
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
import qualified Cardano.Ledger.Alonzo.Language as Alonzo
import qualified Cardano.Ledger.Alonzo.PParams as Alonzo
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import qualified Cardano.Ledger.Alonzo.TxBody as Alonzo
import qualified Cardano.Ledger.Alonzo.TxSeq as Alonzo
import qualified Cardano.Ledger.Alonzo.TxWitness as Alonzo
import qualified Cardano.Ledger.BaseTypes as BT
import qualified Cardano.Ledger.BaseTypes as Ledger
import qualified Cardano.Ledger.BaseTypes as SL
import qualified Cardano.Ledger.Coin as Ledger
import qualified Cardano.Ledger.Core as Ledger
import qualified Cardano.Ledger.Core as SL.Core
import qualified Cardano.Ledger.Credential as SL
import qualified Cardano.Ledger.Crypto as SL
import qualified Cardano.Ledger.Era as Ledger.Era
import qualified Cardano.Ledger.Mary.Value as SL
import qualified Cardano.Ledger.SafeHash as SafeHash
import qualified Cardano.Ledger.Shelley as SL hiding
    ( Value )
import qualified Cardano.Ledger.Shelley.API as SL
import qualified Cardano.Ledger.Shelley.API as SLAPI
import qualified Cardano.Ledger.Shelley.BlockChain as SL
import qualified Cardano.Ledger.Shelley.PParams as Shelley
import qualified Cardano.Ledger.Shelley.Tx as Shelley
import qualified Cardano.Ledger.ShelleyMA as MA
import qualified Cardano.Ledger.ShelleyMA.AuxiliaryData as MA
import qualified Cardano.Ledger.ShelleyMA.Timelocks as MA
import qualified Cardano.Ledger.ShelleyMA.TxBody as MA
import qualified Cardano.Ledger.TxIn as TxIn
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Cardano.Wallet.Primitive.Types.Address as W
import qualified Cardano.Wallet.Primitive.Types.Coin as Coin
import qualified Cardano.Wallet.Primitive.Types.Coin as W
import qualified Cardano.Wallet.Primitive.Types.Hash as W
import qualified Cardano.Wallet.Primitive.Types.Redeemer as W
import qualified Cardano.Wallet.Primitive.Types.RewardAccount as W
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap
import qualified Cardano.Wallet.Primitive.Types.TokenPolicy as W
import qualified Cardano.Wallet.Primitive.Types.TokenQuantity as W
import qualified Cardano.Wallet.Primitive.Types.Tx as W
import qualified Cardano.Wallet.Primitive.Types.UTxO as W
import qualified Codec.Binary.Bech32 as Bech32
import qualified Codec.Binary.Bech32.TH as Bech32
import qualified Codec.CBOR.Decoding as CBOR
import qualified Data.Aeson as Aeson
import qualified Data.Array as Array
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Short as SBS
import qualified Data.Map.Strict as Map
import qualified Data.Map.Strict.NonEmptyMap as NonEmptyMap
import qualified Data.Set as Set
import qualified Data.Text.Encoding as T
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
            W.SlotNo 0
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
nodeToClientVersions = [NodeToClientV_8, NodeToClientV_9]

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
            , parentHeaderHash = Just $
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
       (txs, certs, _, _) = unzip4 $ map fromShelleyTx $ toList txs'
       certs' = mconcat certs
    in
        ( W.Block
            { header = toShelleyBlockHeader (W.getGenesisBlockHash gp) blk
            , transactions = txs
            , delegations  = toDelegationCertificates certs'
            }
        , toPoolCertificates certs'
        )

fromAllegraBlock
    :: W.GenesisParameters
    -> ShelleyBlock (MA.ShelleyMAEra 'MA.Allegra StandardCrypto)
    -> (W.Block, [W.PoolCertificate])
fromAllegraBlock gp blk@(ShelleyBlock (SL.Block _ (SL.TxSeq txs')) _) =
    let
       (txs, certs, _, _) = unzip4 $ map fromAllegraTx $ toList txs'
       certs' = mconcat certs
    in
        ( W.Block
            { header = toShelleyBlockHeader (W.getGenesisBlockHash gp) blk
            , transactions = txs
            , delegations  = toDelegationCertificates certs'
            }
        , toPoolCertificates certs'
        )

fromMaryBlock
    :: W.GenesisParameters
    -> ShelleyBlock (MA.ShelleyMAEra 'MA.Mary StandardCrypto)
    -> (W.Block, [W.PoolCertificate])
fromMaryBlock gp blk@(ShelleyBlock (SL.Block _ (SL.TxSeq txs')) _) =
    let
       (txs, certs, _, _) = unzip4 $ map fromMaryTx $ toList txs'
       certs' = mconcat certs
    in
        ( W.Block
            { header = toShelleyBlockHeader (W.getGenesisBlockHash gp) blk
            , transactions = txs
            , delegations  = toDelegationCertificates certs'
            }
        , toPoolCertificates certs'
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
        (txs, certs, _, _) = unzip4 $ map fromAlonzoValidatedTx $ toList txs'
        certs' = mconcat certs
    in
        ( W.Block
            { header = toShelleyBlockHeader (W.getGenesisBlockHash gp) blk
            , transactions = txs
            , delegations  = toDelegationCertificates certs'
            }
        , toPoolCertificates certs'
        )

fromShelleyHash :: ShelleyHash c -> W.Hash "BlockHeader"
fromShelleyHash (ShelleyHash (SL.HashHeader h)) = W.Hash (hashToBytes h)

fromCardanoHash :: O.HeaderHash (CardanoBlock sc) -> W.Hash "BlockHeader"
fromCardanoHash = W.Hash . fromShort . getOneEraHash

fromPrevHash
    :: W.Hash "BlockHeader"
    -> SLAPI.PrevHash sc
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
        txParametersFromPParams maryTokenBundleMaxSize (W.ExecutionUnits 0 0) pp
    , desiredNumberOfStakePools =
        desiredNumberOfStakePoolsFromPParams pp
    , minimumUTxOvalue =
        MinimumUTxOValue . toWalletCoin $ SLAPI._minUTxOValue pp
    , stakeKeyDeposit = stakeKeyDepositFromPParams pp
    , eras = fromBound <$> eraInfo
    -- Collateral inputs were not supported or required in Shelley:
    , maximumCollateralInputCount = 0
    , minimumCollateralPercentage = 0
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
        (fromLedgerExUnits (getField @"_maxTxExUnits" pp))
        pp
    , desiredNumberOfStakePools =
        desiredNumberOfStakePoolsFromPParams pp
    , minimumUTxOvalue = MinimumUTxOValueCostPerWord
        . toWalletCoin $ Alonzo._coinsPerUTxOWord pp
    , stakeKeyDeposit = stakeKeyDepositFromPParams pp
    , eras = fromBound <$> eraInfo
    , maximumCollateralInputCount = unsafeIntToWord $
        Alonzo._maxCollateralInputs pp
    , minimumCollateralPercentage =
        Alonzo._collateralPercentage pp
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
    fromAlonzoPrices Alonzo.Prices{prMem, prSteps} =
        W.ExecutionUnitPrices
        { W.pricePerStep = Ledger.unboundRational prSteps
        , W.pricePerMemoryUnit = Ledger.unboundRational prMem
        }

fromLedgerExUnits
    :: Alonzo.ExUnits
    -> W.ExecutionUnits
fromLedgerExUnits (Alonzo.ExUnits mem steps) =
    W.ExecutionUnits
    { executionSteps = steps
    , executionMemory = mem
    }

toLedgerExUnits
    :: W.ExecutionUnits
    -> Alonzo.ExUnits
toLedgerExUnits W.ExecutionUnits{executionSteps,executionMemory} =
    Alonzo.ExUnits
    { Alonzo.exUnitsMem = executionMemory
    , Alonzo.exUnitsSteps = executionSteps
    }

txParametersFromPParams
    :: HasField "_minfeeA" pparams Natural
    => HasField "_minfeeB" pparams Natural
    => HasField "_maxTxSize" pparams Natural
    => W.TokenBundleMaxSize
    -> W.ExecutionUnits
    -> pparams
    -> W.TxParameters
txParametersFromPParams maxBundleSize getMaxExecutionUnits pp = W.TxParameters
    { getFeePolicy = W.LinearFee
        (Quantity (naturalToDouble (getField @"_minfeeB" pp)))
        (Quantity (naturalToDouble (getField @"_minfeeA" pp)))
    , getTxMaxSize = fromMaxSize $ getField @"_maxTxSize" pp
    , getTokenBundleMaxSize = maxBundleSize
    , getMaxExecutionUnits
    }
  where
    naturalToDouble :: Natural -> Double
    naturalToDouble = fromIntegral

--------------------------------------------------------------------------------
-- Copied from cardano-api
-- To be removed when once again exposed.
--------------------------------------------------------------------------------

fromLedgerPParams
  :: Cardano.ShelleyBasedEra era
  -> Ledger.PParams (Cardano.ShelleyLedgerEra era)
  -> Cardano.ProtocolParameters
fromLedgerPParams Cardano.ShelleyBasedEraShelley = fromLedgerShelleyPParams
fromLedgerPParams Cardano.ShelleyBasedEraAllegra = fromLedgerShelleyPParams
fromLedgerPParams Cardano.ShelleyBasedEraMary    = fromLedgerShelleyPParams
fromLedgerPParams Cardano.ShelleyBasedEraAlonzo  = fromLedgerAlonzoPParams

fromShelleyLovelace :: Ledger.Coin -> Cardano.Lovelace
fromShelleyLovelace (Ledger.Coin c) = Cardano.Lovelace c

fromLedgerNonce :: Ledger.Nonce -> Maybe Cardano.PraosNonce
fromLedgerNonce Ledger.NeutralNonce = Nothing
fromLedgerNonce (Ledger.Nonce h) =
    Just (Cardano.makePraosNonce $ Crypto.hashToBytes h)

fromLedgerShelleyPParams
    :: Shelley.PParams ledgerera
    -> Cardano.ProtocolParameters
fromLedgerShelleyPParams
    Shelley.PParams {
      Shelley._minfeeA
    , Shelley._minfeeB
    , Shelley._maxBBSize
    , Shelley._maxTxSize
    , Shelley._maxBHSize
    , Shelley._keyDeposit
    , Shelley._poolDeposit
    , Shelley._eMax
    , Shelley._nOpt
    , Shelley._a0
    , Shelley._rho
    , Shelley._tau
    , Shelley._d
    , Shelley._extraEntropy
    , Shelley._protocolVersion
    , Shelley._minUTxOValue
    , Shelley._minPoolCost
    } = Cardano.ProtocolParameters {
      protocolParamProtocolVersion     = (\(BT.ProtVer a b) -> (a,b))
                                           _protocolVersion
    , protocolParamDecentralization    = SL.unboundRational _d
    , protocolParamExtraPraosEntropy   = fromLedgerNonce _extraEntropy
    , protocolParamMaxBlockHeaderSize  = _maxBHSize
    , protocolParamMaxBlockBodySize    = _maxBBSize
    , protocolParamMaxTxSize           = _maxTxSize
    , protocolParamTxFeeFixed          = _minfeeB
    , protocolParamTxFeePerByte        = _minfeeA
    , protocolParamMinUTxOValue        = Just (fromShelleyLovelace _minUTxOValue)
    , protocolParamStakeAddressDeposit = fromShelleyLovelace _keyDeposit
    , protocolParamStakePoolDeposit    = fromShelleyLovelace _poolDeposit
    , protocolParamMinPoolCost         = fromShelleyLovelace _minPoolCost
    , protocolParamPoolRetireMaxEpoch  = _eMax
    , protocolParamStakePoolTargetNum  = _nOpt
    , protocolParamPoolPledgeInfluence = SL.unboundRational _a0
    , protocolParamMonetaryExpansion   = SL.unboundRational _rho
    , protocolParamTreasuryCut         = SL.unboundRational _tau
    , protocolParamUTxOCostPerWord     = Nothing
    , protocolParamCostModels          = Map.empty
    , protocolParamPrices              = Nothing
    , protocolParamMaxTxExUnits        = Nothing
    , protocolParamMaxBlockExUnits     = Nothing
    , protocolParamMaxValueSize        = Nothing
    , protocolParamCollateralPercent   = Nothing
    , protocolParamMaxCollateralInputs = Nothing
    }

fromLedgerAlonzoPParams
    :: Alonzo.PParams ledgerera
    -> Cardano.ProtocolParameters
fromLedgerAlonzoPParams
    Alonzo.PParams {
      Alonzo._minfeeA
    , Alonzo._minfeeB
    , Alonzo._maxBBSize
    , Alonzo._maxTxSize
    , Alonzo._maxBHSize
    , Alonzo._keyDeposit
    , Alonzo._poolDeposit
    , Alonzo._eMax
    , Alonzo._nOpt
    , Alonzo._a0
    , Alonzo._rho
    , Alonzo._tau
    , Alonzo._d
    , Alonzo._extraEntropy
    , Alonzo._protocolVersion
    , Alonzo._minPoolCost
    , Alonzo._coinsPerUTxOWord
    , Alonzo._costmdls
    , Alonzo._prices
    , Alonzo._maxTxExUnits
    , Alonzo._maxBlockExUnits
    , Alonzo._maxValSize
    , Alonzo._collateralPercentage
    , Alonzo._maxCollateralInputs
    } = Cardano.ProtocolParameters {
      protocolParamProtocolVersion     = (\(BT.ProtVer a b) -> (a,b))
                                           _protocolVersion
    , protocolParamDecentralization    = SL.unboundRational _d
    , protocolParamExtraPraosEntropy   = fromLedgerNonce _extraEntropy
    , protocolParamMaxBlockHeaderSize  = _maxBHSize
    , protocolParamMaxBlockBodySize    = _maxBBSize
    , protocolParamMaxTxSize           = _maxTxSize
    , protocolParamTxFeeFixed          = _minfeeB
    , protocolParamTxFeePerByte        = _minfeeA
    , protocolParamMinUTxOValue        = Nothing
    , protocolParamStakeAddressDeposit = fromShelleyLovelace _keyDeposit
    , protocolParamStakePoolDeposit    = fromShelleyLovelace _poolDeposit
    , protocolParamMinPoolCost         = fromShelleyLovelace _minPoolCost
    , protocolParamPoolRetireMaxEpoch  = _eMax
    , protocolParamStakePoolTargetNum  = _nOpt
    , protocolParamPoolPledgeInfluence = SL.unboundRational _a0
    , protocolParamMonetaryExpansion   = SL.unboundRational _rho
    , protocolParamTreasuryCut         = SL.unboundRational _tau
    , protocolParamUTxOCostPerWord     = Just (fromShelleyLovelace _coinsPerUTxOWord)
    , protocolParamCostModels          = fromAlonzoCostModels _costmdls
    , protocolParamPrices              = Just (fromAlonzoPrices _prices)
    , protocolParamMaxTxExUnits        = Just (fromAlonzoExUnits _maxTxExUnits)
    , protocolParamMaxBlockExUnits     = Just (fromAlonzoExUnits _maxBlockExUnits)
    , protocolParamMaxValueSize        = Just _maxValSize
    , protocolParamCollateralPercent   = Just _collateralPercentage
    , protocolParamMaxCollateralInputs = Just _maxCollateralInputs
    }
  where
      fromAlonzoPrices :: Alonzo.Prices -> Cardano.ExecutionUnitPrices
      fromAlonzoPrices Alonzo.Prices{Alonzo.prSteps, Alonzo.prMem} = Cardano.ExecutionUnitPrices
          { priceExecutionSteps  = Ledger.unboundRational prSteps
          , priceExecutionMemory = Ledger.unboundRational prMem
          }

      fromAlonzoExUnits :: Alonzo.ExUnits -> Cardano.ExecutionUnits
      fromAlonzoExUnits Alonzo.ExUnits{Alonzo.exUnitsSteps, Alonzo.exUnitsMem} = Cardano.ExecutionUnits
          { executionSteps  = exUnitsSteps
          , executionMemory = exUnitsMem
          }

      fromAlonzoScriptLanguage :: Alonzo.Language -> Cardano.AnyPlutusScriptVersion
      fromAlonzoScriptLanguage Alonzo.PlutusV1 =
          Cardano.AnyPlutusScriptVersion Cardano.PlutusScriptV1
      fromAlonzoScriptLanguage Alonzo.PlutusV2 =
          Cardano.AnyPlutusScriptVersion Cardano.PlutusScriptV2

      fromAlonzoCostModel :: Alonzo.CostModel -> Cardano.CostModel
      fromAlonzoCostModel (Alonzo.CostModel m) = Cardano.CostModel m

      fromAlonzoCostModels
          :: Map Alonzo.Language Alonzo.CostModel
          -> Map Cardano.AnyPlutusScriptVersion Cardano.CostModel
      fromAlonzoCostModels =
          Map.fromList
          . map (bimap fromAlonzoScriptLanguage fromAlonzoCostModel)
          . Map.toList

toAlonzoPParams
    :: Cardano.ProtocolParameters
    -> Alonzo.PParams era
toAlonzoPParams
    Cardano.ProtocolParameters
        { protocolParamProtocolVersion
        , protocolParamDecentralization
        , protocolParamExtraPraosEntropy
        , protocolParamMaxBlockHeaderSize
        , protocolParamMaxBlockBodySize
        , protocolParamMaxTxSize
        , protocolParamTxFeeFixed
        , protocolParamTxFeePerByte
        , protocolParamStakeAddressDeposit
        , protocolParamStakePoolDeposit
        , protocolParamMinPoolCost
        , protocolParamPoolRetireMaxEpoch
        , protocolParamStakePoolTargetNum
        , protocolParamPoolPledgeInfluence
        , protocolParamMonetaryExpansion
        , protocolParamTreasuryCut
        , protocolParamUTxOCostPerWord = Just utxoCostPerWord
        , protocolParamCostModels
        , protocolParamPrices = Just prices
        , protocolParamMaxTxExUnits = Just maxTxExUnits
        , protocolParamMaxBlockExUnits = Just maxBlockExUnits
        , protocolParamMaxValueSize = Just maxValueSize
        , protocolParamCollateralPercent = Just collateralPercentage
        , protocolParamMaxCollateralInputs = Just maxCollateralInputs
        } =
    Alonzo.PParams
        { Alonzo._protocolVersion =
            let (maj, minor) = protocolParamProtocolVersion
             in BT.ProtVer maj minor
        , Alonzo._d =
            fromMaybe
                (error "toAlonzoPParams: invalid Decentralization value")
                (Ledger.boundRational protocolParamDecentralization)
        , Alonzo._extraEntropy =
            toLedgerNonce protocolParamExtraPraosEntropy
        , Alonzo._maxBHSize =
            protocolParamMaxBlockHeaderSize
        , Alonzo._maxBBSize =
            protocolParamMaxBlockBodySize
        , Alonzo._maxTxSize =
            protocolParamMaxTxSize
        , Alonzo._minfeeB =
            protocolParamTxFeeFixed
        , Alonzo._minfeeA =
            protocolParamTxFeePerByte
        , Alonzo._keyDeposit =
            toShelleyLovelace protocolParamStakeAddressDeposit
        , Alonzo._poolDeposit =
            toShelleyLovelace protocolParamStakePoolDeposit
        , Alonzo._minPoolCost =
            toShelleyLovelace protocolParamMinPoolCost
        , Alonzo._eMax =
            protocolParamPoolRetireMaxEpoch
        , Alonzo._nOpt =
            protocolParamStakePoolTargetNum
        , Alonzo._a0 =
            fromMaybe
                (error "toAlonzoPParams: invalid PoolPledgeInfluence value")
                (Ledger.boundRational protocolParamPoolPledgeInfluence)
        , Alonzo._rho =
            fromMaybe
                (error "toAlonzoPParams: invalid MonetaryExpansion value")
                (Ledger.boundRational protocolParamMonetaryExpansion)
        , Alonzo._tau =
            fromMaybe
                (error "toAlonzoPParams: invalid TreasuryCut value")
                (Ledger.boundRational protocolParamTreasuryCut)
        , Alonzo._coinsPerUTxOWord =
            toShelleyLovelace utxoCostPerWord
        , Alonzo._costmdls =
            toAlonzoCostModels protocolParamCostModels
        , Alonzo._prices =
            fromMaybe
                (error "toAlonzoPParams: invalid Price values")
                (toAlonzoPrices prices)
        , Alonzo._maxTxExUnits =
            toAlonzoExUnits maxTxExUnits
        , Alonzo._maxBlockExUnits =
            toAlonzoExUnits maxBlockExUnits
        , Alonzo._maxValSize =
            maxValueSize
        , Alonzo._collateralPercentage =
            collateralPercentage
        , Alonzo._maxCollateralInputs =
            maxCollateralInputs
        }
  where
    toShelleyLovelace :: Cardano.Lovelace -> SLAPI.Coin
    toShelleyLovelace (Cardano.Lovelace l) = SLAPI.Coin l

    toAlonzoCostModels
      :: Map Cardano.AnyPlutusScriptVersion Cardano.CostModel
      -> Map Alonzo.Language Alonzo.CostModel
    toAlonzoCostModels
        = Map.fromList
        . map (bimap toAlonzoScriptLanguage toAlonzoCostModel)
        . Map.toList
      where
        toAlonzoCostModel :: Cardano.CostModel -> Alonzo.CostModel
        toAlonzoCostModel (Cardano.CostModel m) =
            Alonzo.CostModel m

        toAlonzoScriptLanguage :: Cardano.AnyPlutusScriptVersion -> Alonzo.Language
        toAlonzoScriptLanguage (Cardano.AnyPlutusScriptVersion Cardano.PlutusScriptV1) =
            Alonzo.PlutusV1
        toAlonzoScriptLanguage (Cardano.AnyPlutusScriptVersion Cardano.PlutusScriptV2) =
            Alonzo.PlutusV2

    toAlonzoPrices :: Cardano.ExecutionUnitPrices -> Maybe Alonzo.Prices
    toAlonzoPrices Cardano.ExecutionUnitPrices
        { priceExecutionSteps
        , priceExecutionMemory
        } = do
      prSteps <- Ledger.boundRational priceExecutionSteps
      prMem   <- Ledger.boundRational priceExecutionMemory
      return Alonzo.Prices { Alonzo.prSteps, Alonzo.prMem }

    toAlonzoExUnits :: Cardano.ExecutionUnits -> Alonzo.ExUnits
    toAlonzoExUnits Cardano.ExecutionUnits{executionSteps, executionMemory} =
      Alonzo.ExUnits
        { Alonzo.exUnitsSteps = executionSteps
        , Alonzo.exUnitsMem   = executionMemory
        }

    toLedgerNonce :: Maybe Cardano.PraosNonce -> Ledger.Nonce
    toLedgerNonce = \case
        Nothing -> Ledger.NeutralNonce
        Just nonce -> Ledger.Nonce (unsafeHashFromBytes (Cardano.serialiseToRawBytes nonce))
toAlonzoPParams Cardano.ProtocolParameters { protocolParamUTxOCostPerWord = Nothing } =
  error "toAlonzoPParams: must specify protocolParamUTxOCostPerWord"
toAlonzoPParams Cardano.ProtocolParameters { protocolParamPrices = Nothing } =
  error "toAlonzoPParams: must specify protocolParamPrices"
toAlonzoPParams Cardano.ProtocolParameters { protocolParamMaxTxExUnits = Nothing } =
  error "toAlonzoPParams: must specify protocolParamMaxTxExUnits"
toAlonzoPParams Cardano.ProtocolParameters { protocolParamMaxBlockExUnits = Nothing } =
  error "toAlonzoPParams: must specify protocolParamMaxBlockExUnits"
toAlonzoPParams Cardano.ProtocolParameters { protocolParamMaxValueSize = Nothing } =
    error "toAlonzoPParams: must specify protocolParamMaxValueSize"
toAlonzoPParams Cardano.ProtocolParameters { protocolParamCollateralPercent = Nothing } =
    error "toAlonzoPParams: must specify protocolParamCollateralPercent"
toAlonzoPParams Cardano.ProtocolParameters { protocolParamMaxCollateralInputs = Nothing } =
    error "toAlonzoPParams: must specify protocolParamMaxCollateralInputs"

toCostModelsAsArray
    :: Map Alonzo.Language Alonzo.CostModel
    -> Array Alonzo.Language Alonzo.CostModel
toCostModelsAsArray costModels =
    Array.array (minBound, maxBound) [ (k, v) | (k, v) <- Map.toList costModels ]

--------------------------------------------------------------------------------

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

-- note: upcasts Word32 -> Word64
getCardanoEpochSlots :: W.SlottingParameters -> Cardano.EpochSlots
getCardanoEpochSlots =
    Cardano.EpochSlots . fromIntegral . W.unEpochLength . W.getEpochLength

localNodeConnectInfo
    :: W.SlottingParameters
    -> NetworkId
    -> CardanoNodeConn
    -> LocalNodeConnectInfo CardanoMode
localNodeConnectInfo sp net = LocalNodeConnectInfo params net . nodeSocketFile
    where params = CardanoModeParams (getCardanoEpochSlots sp)

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
                Nothing
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
    :: SL.TxIn crypto
    -> W.TxIn
fromShelleyTxIn (SL.TxIn txid ix) =
    W.TxIn (fromShelleyTxId txid) (unsafeCast ix)
  where
    unsafeCast :: Natural -> Word32
    unsafeCast = fromIntegral

fromCardanoTxIn
    :: Cardano.TxIn
    -> W.TxIn
fromCardanoTxIn (Cardano.TxIn txid (Cardano.TxIx ix)) =
    W.TxIn (fromShelleyTxId $ Cardano.toShelleyTxId txid) (fromIntegral ix)

-- | WARNING: Datum hashes are lost in the conversion!
fromCardanoTxOut :: IsCardanoEra era => Cardano.TxOut ctx era -> W.TxOut
fromCardanoTxOut (Cardano.TxOut addr out _datumHash) =
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
        Cardano.StakeAddressDelegationCertificate cred _ ->
            toRewardAccount cred
        _ ->
            Nothing

fromShelleyTxOut
    :: ( Era era
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
fromShelleyCoin (SL.Coin c) = Coin.unsafeFromIntegral c

toShelleyCoin :: W.Coin -> SL.Coin
toShelleyCoin (W.Coin c) = SL.Coin $ intCast c

fromCardanoTx
    :: Cardano.Tx era
    -> (W.Tx, TokenMapWithScripts, TokenMapWithScripts, [Certificate])
fromCardanoTx = \case
    Cardano.ShelleyTx era tx -> case era of
        Cardano.ShelleyBasedEraShelley -> extract $ fromShelleyTx tx
        Cardano.ShelleyBasedEraAllegra -> extract $ fromAllegraTx tx
        Cardano.ShelleyBasedEraMary    -> extract $ fromMaryTx tx
        Cardano.ShelleyBasedEraAlonzo  -> extract $ fromAlonzoTx tx
    Cardano.ByronTx tx                 -> (fromTxAux tx, emptyTokenMapWithScripts, emptyTokenMapWithScripts, [])
  where
    extract (tx, certs, mint, burn) = (tx, mint, burn, certs)

-- NOTE: For resolved inputs we have to pass in a dummy value of 0.
fromShelleyTx
    :: SLAPI.Tx (Cardano.ShelleyLedgerEra ShelleyEra)
    -> ( W.Tx
       , [W.Certificate]
       , TokenMapWithScripts
       , TokenMapWithScripts
       )
fromShelleyTx tx =
    ( W.Tx
        { txId =
            fromShelleyTxId $ TxIn.txid @(Cardano.ShelleyLedgerEra ShelleyEra) bod
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
    , map fromShelleyCert (toList certs)
    , emptyTokenMapWithScripts
    , emptyTokenMapWithScripts
    )
  where
    SL.Tx bod@(SL.TxBody ins outs certs wdrls fee _ _ _) _ mmd = tx

fromAllegraTx
    :: SLAPI.Tx (Cardano.ShelleyLedgerEra AllegraEra)
    -> ( W.Tx
       , [W.Certificate]
       , TokenMapWithScripts
       , TokenMapWithScripts
       )
fromAllegraTx tx =
    ( W.Tx
        { txId =
            fromShelleyTxId $ TxIn.txid @(Cardano.ShelleyLedgerEra AllegraEra) bod
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
    , map fromShelleyCert (toList certs)
    , emptyTokenMapWithScripts
    , emptyTokenMapWithScripts
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
       , [W.Certificate]
       , TokenMapWithScripts
       , TokenMapWithScripts
       )
fromMaryTx tx =
    ( W.Tx
        { txId
            = fromShelleyTxId $ TxIn.txid @(Cardano.ShelleyLedgerEra MaryEra) bod
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
    , map fromShelleyCert (toList certs)
    , TokenMapWithScripts assetsToMint mintScriptMap
    , TokenMapWithScripts assetsToBurn burnScriptMap
    )
  where
    SL.Tx bod wits mad = tx
    MA.TxBody ins outs certs wdrls fee _valid _upd _adh mint = bod
    (assetsToMint, assetsToBurn) = fromLedgerMintValue mint
    scriptMap = fromMaryScriptMap $ Shelley.scriptWits wits
    (mintScriptMap, burnScriptMap) =
        if (assetsToMint == TokenMap.empty && assetsToBurn /= TokenMap.empty) then
            (Map.empty, scriptMap)
        else if (assetsToMint /= TokenMap.empty && assetsToBurn == TokenMap.empty) then
            (scriptMap, Map.empty)
        else
            (Map.empty, Map.empty)

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

    fromMaryScriptMap
        :: Map (SL.ScriptHash (Crypto (MA.ShelleyMAEra 'MA.Mary StandardCrypto))) (SL.Core.Script (MA.ShelleyMAEra 'MA.Mary StandardCrypto))
        -> Map TokenPolicyId (Script KeyHash)
    fromMaryScriptMap =
        Map.map fromLedgerScript .
        Map.mapKeys (toWalletTokenPolicyId . SL.PolicyID)
      where
        fromLedgerScript (MA.RequireSignature (SL.KeyHash h)) =
            -- here Policy Role after 3.9.0 cardano-addresses
            RequireSignatureOf (KeyHash Payment (hashToBytes h))
        fromLedgerScript (MA.RequireAllOf contents) =
            RequireAllOf $ map fromLedgerScript $ toList contents
        fromLedgerScript (MA.RequireAnyOf contents) =
            RequireAnyOf $ map fromLedgerScript $ toList contents
        fromLedgerScript (MA.RequireMOf num contents) =
            RequireSomeOf (fromIntegral num) $ map fromLedgerScript $ toList contents
        fromLedgerScript (MA.RequireTimeExpire (O.SlotNo slot)) =
            ActiveUntilSlot $ fromIntegral slot
        fromLedgerScript (MA.RequireTimeStart (O.SlotNo slot)) =
            ActiveFromSlot $ fromIntegral slot


getScriptIntegrityHash
    :: Cardano.Tx era
    -> Maybe ByteString
getScriptIntegrityHash = \case
    Cardano.ShelleyTx era tx -> case era of
        Cardano.ShelleyBasedEraShelley -> Nothing
        Cardano.ShelleyBasedEraAllegra -> Nothing
        Cardano.ShelleyBasedEraMary    -> Nothing
        Cardano.ShelleyBasedEraAlonzo  -> SafeHash.originalBytes <$> scriptIntegrityHashOfAlonzoTx tx
    Cardano.ByronTx _                  -> Nothing

    where
        scriptIntegrityHashOfAlonzoTx
            :: Alonzo.ValidatedTx (Alonzo.AlonzoEra StandardCrypto)
            -> Maybe (Alonzo.ScriptIntegrityHash StandardCrypto)
        scriptIntegrityHashOfAlonzoTx (Alonzo.ValidatedTx body _wits _isValid _auxData)
            = strictMaybeToMaybe . Alonzo.scriptIntegrityHash $ body

fromAlonzoTxBodyAndAux
    :: Alonzo.TxBody (Cardano.ShelleyLedgerEra AlonzoEra)
    -> SLAPI.StrictMaybe (Alonzo.AuxiliaryData (Cardano.ShelleyLedgerEra AlonzoEra))
    -> ( W.Tx
       , [W.Certificate]
       , TokenMapWithScripts
       , TokenMapWithScripts
       )
fromAlonzoTxBodyAndAux bod mad =
    ( W.Tx
        { txId =
            fromShelleyTxId $ TxIn.txid @(Cardano.ShelleyLedgerEra AlonzoEra) bod
        , fee =
            Just $ fromShelleyCoin fee
        , resolvedCollateral =
            map ((,W.Coin 0) . fromShelleyTxIn) (toList collateral)
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
    , map fromShelleyCert (toList certs)
    , TokenMapWithScripts assetsToMint Map.empty
    , TokenMapWithScripts assetsToBurn Map.empty
    )
  where
    Alonzo.TxBody
        ins
        collateral
        outs
        certs
        wdrls
        fee
        _valid
        _upd
        _reqSignerHashes
        mint
        _wwpHash
        _adHash
        _network
        = bod
    (assetsToMint, assetsToBurn) = fromLedgerMintValue mint

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
       , [W.Certificate]
       , TokenMapWithScripts
       , TokenMapWithScripts
       )
fromAlonzoValidatedTx (Alonzo.ValidatedTx bod _wits _isValidating aux) =
    fromAlonzoTxBodyAndAux bod aux

fromAlonzoTx
    :: Alonzo.ValidatedTx (Cardano.ShelleyLedgerEra AlonzoEra)
    -> ( W.Tx
       , [W.Certificate]
       , TokenMapWithScripts
       , TokenMapWithScripts
       )
fromAlonzoTx (Alonzo.ValidatedTx bod wits (Alonzo.IsValid isValid) aux) =
    (\(tx, c, m, b) -> (tx { W.scriptValidity = validity }, c, m, b))
    $ fromAlonzoTxBodyAndAux bod aux
    where
        validity =
            if isValid
            then Just W.TxScriptValid
            else Just W.TxScriptInvalid
        scriptMap = Alonzo.txscripts wits

-- Lovelace to coin. Quantities from ledger should always fit in Word64.
fromCardanoLovelace :: HasCallStack => Cardano.Lovelace -> W.Coin
fromCardanoLovelace =
    Coin.unsafeFromIntegral . unQuantity . Cardano.lovelaceToQuantity
  where
    unQuantity (Cardano.Quantity q) = q

fromCardanoValue :: HasCallStack => Cardano.Value -> TokenBundle.TokenBundle
fromCardanoValue = uncurry TokenBundle.fromFlatList . extract
  where
    extract value =
        ( fromCardanoLovelace $ Cardano.selectLovelace value
        , mkBundle $ Cardano.valueToList value
        )

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

fromShelleyWdrl :: SL.Wdrl crypto -> Map W.RewardAccount W.Coin
fromShelleyWdrl (SL.Wdrl wdrl) = Map.fromList $
    bimap (fromStakeCredential . SL.getRwdCred) fromShelleyCoin
        <$> Map.toList wdrl

fromShelleyMD :: SL.Metadata c -> Cardano.TxMetadata
fromShelleyMD (SL.Metadata m) =
    Cardano.makeTransactionMetadata . fromShelleyMetadata $ m

toDelegationCertificates
    :: [W.Certificate]
    -> [W.DelegationCertificate]
toDelegationCertificates = mapMaybe isDelCert
  where
      isDelCert = \case
          W.CertificateOfDelegation cert -> Just cert
          _ -> Nothing

toPoolCertificates
    :: [W.Certificate]
    -> [W.PoolCertificate]
toPoolCertificates = mapMaybe isPoolCert
  where
      isPoolCert = \case
          W.CertificateOfPool cert -> Just cert
          _ -> Nothing

fromShelleyCert
    :: SL.DCert crypto
    -> W.Certificate
fromShelleyCert = \case
    SL.DCertDeleg (SL.Delegate delegation)  ->
        W.CertificateOfDelegation $ W.CertDelegateFull
            (fromStakeCredential (SL._delegator delegation))
            (fromPoolKeyHash (SL._delegatee delegation))

    SL.DCertDeleg (SL.DeRegKey credentials) ->
        W.CertificateOfDelegation $ W.CertDelegateNone (fromStakeCredential credentials)

    SL.DCertDeleg (SL.RegKey cred) ->
        W.CertificateOfDelegation $ W.CertRegisterKey $ fromStakeCredential cred

    SL.DCertPool (SL.RegPool pp) -> W.CertificateOfPool $ Registration
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
        W.CertificateOfPool $ Retirement $ PoolRetirementCertificate (fromPoolKeyHash pid)
        (W.EpochNo $ fromIntegral e)

    SL.DCertGenesis{} -> W.CertificateOther W.GenesisCertificate

    SL.DCertMir{}     -> W.CertificateOther W.MIRCertificate

toWalletCoin :: HasCallStack => SL.Coin -> W.Coin
toWalletCoin (SL.Coin c) = Coin.unsafeFromIntegral c

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

fromCardanoAddress :: Cardano.Address Cardano.ShelleyAddr -> W.Address
fromCardanoAddress = W.Address . Cardano.serialiseToRawBytes

fromUnitInterval :: HasCallStack => SL.UnitInterval -> Percentage
fromUnitInterval x =
    either bomb id . mkPercentage . toRational . SL.unboundRational $ x
  where
    bomb = internalError $
        "fromUnitInterval: encountered invalid parameter value: "+||x||+""

toSystemStart :: W.StartTime -> SystemStart
toSystemStart (W.StartTime t) = SystemStart t

toScriptPurpose :: W.Redeemer -> Alonzo.ScriptPurpose StandardCrypto
toScriptPurpose = \case
    W.RedeemerSpending _ txin ->
        Alonzo.Spending (toTxIn txin)
    W.RedeemerMinting _ pid ->
        Alonzo.Minting (toPolicyID pid)
    W.RedeemerRewarding _ (Cardano.StakeAddress ntwrk acct) ->
        Alonzo.Rewarding (SL.RewardAcnt ntwrk acct)

toCardanoTxId :: W.Hash "Tx" -> Cardano.TxId
toCardanoTxId (W.Hash h) = Cardano.TxId $ UnsafeHash $ toShort h

toCardanoTxIn :: W.TxIn -> Cardano.TxIn
toCardanoTxIn (W.TxIn tid ix) =
    Cardano.TxIn (toCardanoTxId tid) (Cardano.TxIx (fromIntegral ix))

toTxIn :: SL.Crypto crypto => W.TxIn -> SL.TxIn crypto
toTxIn (W.TxIn tid ix) =
    SL.TxIn (toTxId tid) (fromIntegral ix)

toTxId :: Crypto.HashAlgorithm (SL.HASH crypto) => W.Hash "Tx" -> SL.TxId crypto
toTxId (W.Hash h) =
    (SL.TxId (SafeHash.unsafeMakeSafeHash $ UnsafeHash $ toShort h))

toPolicyID :: SL.Crypto crypto => W.TokenPolicyId -> SL.PolicyID crypto
toPolicyID (W.UnsafeTokenPolicyId (W.Hash bytes)) =
    SL.PolicyID (SL.ScriptHash (unsafeHashFromBytes bytes))

toCardanoStakeCredential :: W.RewardAccount -> Cardano.StakeCredential
toCardanoStakeCredential = Cardano.StakeCredentialByKey
    . Cardano.StakeKeyHash
    . SL.KeyHash
    . UnsafeHash
    . SBS.toShort
    . W.unRewardAccount

toCardanoLovelace :: W.Coin -> Cardano.Lovelace
toCardanoLovelace (W.Coin c) = Cardano.Lovelace $ intCast c

toCardanoUTxO :: ShelleyBasedEra era -> W.UTxO -> Cardano.UTxO era
toCardanoUTxO era = Cardano.UTxO
    . Map.fromList
    . map (bimap toCardanoTxIn (toCardanoTxOut era))
    . Map.toList
    . W.unUTxO

toCardanoTxOut :: ShelleyBasedEra era -> W.TxOut -> Cardano.TxOut ctx era
toCardanoTxOut era = case era of
    ShelleyBasedEraShelley -> toShelleyTxOut
    ShelleyBasedEraAllegra -> toAllegraTxOut
    ShelleyBasedEraMary    -> toMaryTxOut
    ShelleyBasedEraAlonzo  -> toAlonzoTxOut
  where
    toShelleyTxOut :: HasCallStack => W.TxOut -> Cardano.TxOut ctx ShelleyEra
    toShelleyTxOut (W.TxOut (W.Address addr) tokens) =
        Cardano.TxOut
            addrInEra
            (adaOnly $ toCardanoLovelace $ TokenBundle.getCoin tokens)
            Cardano.TxOutDatumNone
      where
        adaOnly = Cardano.TxOutAdaOnly Cardano.AdaOnlyInShelleyEra
        addrInEra = tina "toCardanoTxOut: malformed address"
            [ Cardano.AddressInEra
                (Cardano.ShelleyAddressInEra Cardano.ShelleyBasedEraShelley)
                <$> deserialiseFromRawBytes AsShelleyAddress addr

            , Cardano.AddressInEra Cardano.ByronAddressInAnyEra
                <$> deserialiseFromRawBytes AsByronAddress addr
            ]

    toAllegraTxOut :: HasCallStack => W.TxOut -> Cardano.TxOut ctx AllegraEra
    toAllegraTxOut (W.TxOut (W.Address addr) tokens) =
        Cardano.TxOut
            addrInEra
            (adaOnly $ toCardanoLovelace $ TokenBundle.getCoin tokens)
            Cardano.TxOutDatumNone
      where
        adaOnly = Cardano.TxOutAdaOnly Cardano.AdaOnlyInAllegraEra
        addrInEra = tina "toCardanoTxOut: malformed address"
            [ Cardano.AddressInEra
                (Cardano.ShelleyAddressInEra Cardano.ShelleyBasedEraAllegra)
                <$> deserialiseFromRawBytes AsShelleyAddress addr

            , Cardano.AddressInEra Cardano.ByronAddressInAnyEra
                <$> deserialiseFromRawBytes AsByronAddress addr
            ]

    toMaryTxOut :: HasCallStack => W.TxOut -> Cardano.TxOut ctx MaryEra
    toMaryTxOut (W.TxOut (W.Address addr) tokens) =
        Cardano.TxOut
            addrInEra
            (Cardano.TxOutValue Cardano.MultiAssetInMaryEra $ toCardanoValue tokens)
            Cardano.TxOutDatumNone
      where
        addrInEra = tina "toCardanoTxOut: malformed address"
            [ Cardano.AddressInEra (Cardano.ShelleyAddressInEra Cardano.ShelleyBasedEraMary)
                <$> deserialiseFromRawBytes AsShelleyAddress addr

            , Cardano.AddressInEra Cardano.ByronAddressInAnyEra
                <$> deserialiseFromRawBytes AsByronAddress addr
            ]

    toAlonzoTxOut :: HasCallStack => W.TxOut -> Cardano.TxOut ctx AlonzoEra
    toAlonzoTxOut (W.TxOut (W.Address addr) tokens) =
        Cardano.TxOut
            addrInEra
            (Cardano.TxOutValue Cardano.MultiAssetInAlonzoEra $ toCardanoValue tokens)
            datumHash
      where
        datumHash = Cardano.TxOutDatumNone
        addrInEra = tina "toCardanoTxOut: malformed address"
            [ Cardano.AddressInEra (Cardano.ShelleyAddressInEra Cardano.ShelleyBasedEraAlonzo)
                <$> deserialiseFromRawBytes AsShelleyAddress addr

            , Cardano.AddressInEra Cardano.ByronAddressInAnyEra
                <$> deserialiseFromRawBytes AsByronAddress addr
            ]

toCardanoValue :: TokenBundle.TokenBundle -> Cardano.Value
toCardanoValue tb = Cardano.valueFromList $
    (Cardano.AdaAssetId, coinToQuantity coin) :
    map (bimap toCardanoAssetId toQuantity) bundle
  where
    (coin, bundle) = TokenBundle.toFlatList tb
    toCardanoAssetId (TokenBundle.AssetId pid name) =
        Cardano.AssetId (toCardanoPolicyId pid) (toCardanoAssetName name)

    toCardanoAssetName (W.UnsafeTokenName name) = just "toCardanoValue" "TokenName"
        [Cardano.deserialiseFromRawBytes Cardano.AsAssetName name]

    coinToQuantity = fromIntegral . W.unCoin
    toQuantity = fromIntegral . W.unTokenQuantity

toCardanoPolicyId :: W.TokenPolicyId -> Cardano.PolicyId
toCardanoPolicyId (W.UnsafeTokenPolicyId (W.Hash pid)) =
    just "toCardanoPolicyId" "PolicyId"
    [Cardano.deserialiseFromRawBytes Cardano.AsPolicyId pid]

toCardanoSimpleScript
    :: Script KeyHash
    -> Cardano.SimpleScript Cardano.SimpleScriptV2
toCardanoSimpleScript = \case
    RequireSignatureOf (KeyHash _ keyhash) ->
        case Cardano.deserialiseFromRawBytes (Cardano.AsHash Cardano.AsPaymentKey) keyhash of
            Just payKeyHash -> Cardano.RequireSignature payKeyHash
            Nothing -> error "Hash key not valid"
    RequireAllOf contents ->
        Cardano.RequireAllOf $ map toCardanoSimpleScript contents
    RequireAnyOf contents ->
        Cardano.RequireAnyOf $ map toCardanoSimpleScript contents
    RequireSomeOf num contents ->
        Cardano.RequireMOf (fromIntegral num) $ map toCardanoSimpleScript contents
    ActiveFromSlot slot ->
        Cardano.RequireTimeAfter Cardano.TimeLocksInSimpleScriptV2
        (O.SlotNo $ fromIntegral slot)
    ActiveUntilSlot slot ->
        Cardano.RequireTimeBefore Cardano.TimeLocksInSimpleScriptV2
        (O.SlotNo $ fromIntegral slot)

just :: Builder -> Builder -> [Maybe a] -> a
just t1 t2 = tina (t1+|": unable to deserialise "+|t2)

fromLedgerMintValue
    :: SL.Value StandardCrypto
    -> (TokenMap, TokenMap)
fromLedgerMintValue (SL.Value _ ledgerTokens) =
    (assetsToMint, assetsToBurn)
  where
    assetsToMint = ledgerTokens
        & Map.map (Map.filter (> 0))
        & Map.mapKeys toWalletTokenPolicyId
        & Map.map mapInner
        & Map.mapMaybe NonEmptyMap.fromMap
        & TokenMap.fromNestedMap

    assetsToBurn = ledgerTokens
        & Map.map (Map.mapMaybe (\n -> if n > 0 then Nothing else Just (-n)))
        & Map.mapKeys toWalletTokenPolicyId
        & Map.map mapInner
        & Map.mapMaybe NonEmptyMap.fromMap
        & TokenMap.fromNestedMap

    mapInner inner = inner
        & Map.mapKeys toWalletTokenName
        & Map.map toWalletTokenQuantity

-- | Convert from reward account address (which is a hash of a public key)
-- to a shelley ledger stake credential.
toStakeCredential
    :: (Crypto.HashAlgorithm (SL.ADDRHASH crypto))
    => W.RewardAccount
    -> SL.StakeCredential crypto
toStakeCredential = SL.KeyHashObj
    . SL.KeyHash . unsafeHashFromBytes . W.unRewardAccount

unsafeHashFromBytes :: Crypto.HashAlgorithm h => ByteString -> Hash h a
unsafeHashFromBytes =
    fromMaybe (error "unsafeHashFromBytes: wrong length")
    . Crypto.hashFromBytes

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

-- | Converts 'SealedTx' to something that can be submitted with the
-- 'Cardano.Api' local tx submission client.
unsealShelleyTx :: W.SealedTx -> TxInMode CardanoMode
unsealShelleyTx wtx = case W.cardanoTx wtx of
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

-- | Converts a 'ShelleyBasedEra' to the broader 'CardanoEra'.
shelleyBasedToCardanoEra :: ShelleyBasedEra era -> CardanoEra era
shelleyBasedToCardanoEra Cardano.ShelleyBasedEraShelley = ShelleyEra
shelleyBasedToCardanoEra Cardano.ShelleyBasedEraAllegra = AllegraEra
shelleyBasedToCardanoEra Cardano.ShelleyBasedEraMary    = MaryEra
shelleyBasedToCardanoEra Cardano.ShelleyBasedEraAlonzo  = AlonzoEra

-- | An existential type like 'AnyCardanoEra', but for 'ShelleyBasedEra'.
data AnyShelleyBasedEra where
     AnyShelleyBasedEra :: IsShelleyBasedEra era -- Provide class constraint
                        => ShelleyBasedEra era   -- and explicit value.
                        -> AnyShelleyBasedEra    -- and that's it.

instance Show AnyShelleyBasedEra where
    show (AnyShelleyBasedEra era) = "AnyShelleyBasedEra " ++ show era

anyShelleyBasedEra :: InAnyShelleyBasedEra (Const ()) -> AnyShelleyBasedEra
anyShelleyBasedEra (InAnyShelleyBasedEra era _) = AnyShelleyBasedEra era

shelleyToCardanoEra :: AnyShelleyBasedEra -> AnyCardanoEra
shelleyToCardanoEra (AnyShelleyBasedEra era) =
    AnyCardanoEra (shelleyBasedToCardanoEra era)

getShelleyBasedEra :: AnyCardanoEra -> Maybe AnyShelleyBasedEra
getShelleyBasedEra (AnyCardanoEra e) = case cardanoEraStyle e of
    LegacyByronEra -> Nothing
    ShelleyBasedEra era -> Just
        (anyShelleyBasedEra (InAnyShelleyBasedEra era (Const ())))

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
            W.TokenBundleSizeExceedsLimit
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
    case tryBech32 t <|> tryBase58 t of
        Just bytes ->
            Right bytes
        _ ->
            Left $ TextDecodingError $ unwords
                [ "Unrecognized address encoding: must be either bech32 or base58."
                , "Perhaps your address is not entirely correct?"
                , "Please double-check each character within the address and try again."
                ]

-- | Attempt to decode a Shelley 'Address' using a Bech32 encoding.
tryBech32 :: Text -> Maybe ByteString
tryBech32 = fmap CA.unAddress . CA.fromBech32

-- | Attempt to decode a legacy Byron 'Address' using a Base58 encoding.
--
-- NOTE: As of Oct 2021, the Shelley ledger does *not* check whether
-- a Byron address is in valid Byron binary format. This implies that
-- an invalid Base58 Byron address can be interpreted as a valid Shelly
-- address, which results in unexpected loss of user funds.
--
-- Here, the 'tryBase58' function uses 'Cardano.Address',
-- which performs the additional check of deserializing the
-- address from Byron CBOR format.
--
-- Even so, we strongly recommend the Bech32 format,
-- as it includes error detection
-- and is more robust against typos and misspellings.
tryBase58 :: Text -> Maybe ByteString
tryBase58 = fmap CA.unAddress . CA.fromBase58

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
                guardNetwork (fromByronNetworkMagic (Byron.addrNetworkMagic addr)) serverNetwork
                pure (W.Address bytes)

            Nothing -> Left errMalformedAddress

      where
        fromByronNetworkMagic :: Byron.NetworkMagic -> SL.Network
        fromByronNetworkMagic = \case
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
