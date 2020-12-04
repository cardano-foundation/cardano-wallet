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

    , NodeVersionData
    , StandardCrypto
    , StandardShelley

      -- * Chain Parameters
    , mainnetVersionData
    , testnetVersionData

      -- * Genesis
    , emptyGenesis
    , genesisTip

      -- * Conversions
    , toCardanoHash
    , toEpochSize
    , unsealShelleyTx
    , toPoint
    , toCardanoTxId
    , toCardanoTxIn
    , toCardanoTxOut
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
    , fromShelleyChainHash
    , fromGenesisData
    , fromNetworkMagic
    , toByronNetworkMagic
    , fromTip
    , fromTip'
    , fromShelleyPParams
    , fromNetworkDiscriminant
    , fromShelleyTx
    , fromAllegraTx
    , fromShelleyBlock
    , fromAllegraBlock

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
import Cardano.Api.Shelley
    ( fromShelleyMetaData )
import Cardano.Api.Shelley.Genesis
    ( ShelleyGenesis (..) )
import Cardano.Api.Typed
    ( AllegraEra
    , AnyCardanoEra (..)
    , AsType (..)
    , CardanoEra (..)
    , NetworkId
    , ShelleyEra
    , StandardShelley
    , deserialiseFromRawBytes
    )
import Cardano.Binary
    ( fromCBOR, serialize' )
import Cardano.Crypto.Hash.Class
    ( Hash (UnsafeHash), hashToBytes )
import Cardano.Ledger.Era
    ( Era (..) )
import Cardano.Slotting.Slot
    ( EpochNo (..), EpochSize (..) )
import Cardano.Wallet.Api.Types
    ( DecodeAddress (..)
    , DecodeStakeAddress (..)
    , EncodeAddress (..)
    , EncodeStakeAddress (..)
    )
import Cardano.Wallet.Byron.Compatibility
    ( fromByronBlock, fromTxAux, toByronBlockHeader )
import Cardano.Wallet.Primitive.AddressDerivation
    ( NetworkDiscriminant (..) )
import Cardano.Wallet.Primitive.Types
    ( PoolCertificate (..)
    , PoolRegistrationCertificate (..)
    , PoolRetirementCertificate (..)
    )
import Cardano.Wallet.Unsafe
    ( unsafeDeserialiseCbor, unsafeMkPercentage )
import Codec.Binary.Bech32
    ( dataPartFromBytes, dataPartToBytes )
import Control.Applicative
    ( (<|>) )
import Control.Arrow
    ( left )
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
import Data.Foldable
    ( asum, toList )
import Data.Map.Strict
    ( Map )
import Data.Maybe
    ( fromMaybe, isJust, mapMaybe )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Percentage, Quantity (..), mkPercentage )
import Data.Ratio
    ( (%) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( TextDecodingError (..) )
import Data.Type.Equality
    ( testEquality )
import Data.Word
    ( Word16, Word32, Word64, Word8 )
import Fmt
    ( Buildable (..) )
import GHC.Stack
    ( HasCallStack )
import GHC.TypeLits
    ( KnownNat, natVal )
import Numeric.Natural
    ( Natural )
import Ouroboros.Consensus.Cardano.Block
    ( CardanoBlock, CardanoEras, CardanoGenTx, GenTx (..), HardForkBlock (..) )
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras
    ( OneEraHash (..) )
import Ouroboros.Consensus.HardFork.History.Summary
    ( Bound (..) )
import Ouroboros.Consensus.Shelley.Ledger
    ( ShelleyHash (..) )
import Ouroboros.Consensus.Shelley.Ledger.Block
    ( ShelleyBlock (..) )
import Ouroboros.Consensus.Shelley.Protocol.Crypto
    ( StandardCrypto )
import Ouroboros.Network.Block
    ( BlockNo (..)
    , ChainHash
    , Point (..)
    , Tip (..)
    , genesisPoint
    , getLegacyTipBlockNo
    , getTipPoint
    , legacyTip
    )
import Ouroboros.Network.CodecCBORTerm
    ( CodecCBORTerm )
import Ouroboros.Network.Magic
    ( NetworkMagic (..) )
import Ouroboros.Network.NodeToClient
    ( ConnectionId (..)
    , LocalAddress (..)
    , NodeToClientVersion (..)
    , NodeToClientVersionData (..)
    , nodeToClientCodecCBORTerm
    )
import Ouroboros.Network.Point
    ( WithOrigin (..) )
import Shelley.Spec.Ledger.BaseTypes
    ( interval0, strictMaybeToMaybe, urlToText )
import Type.Reflection
    ( Typeable, typeRep )

import qualified Cardano.Address.Style.Shelley as CA
import qualified Cardano.Api.Typed as Cardano
import qualified Cardano.Byron.Codec.Cbor as CBOR
import qualified Cardano.Chain.Common as Byron
import qualified Cardano.Ledger.Core as SL.Core
import qualified Cardano.Ledger.Crypto as SL
import qualified Cardano.Ledger.Shelley as SL
import qualified Cardano.Ledger.Shelley.Constraints as SL
import qualified Cardano.Ledger.ShelleyMA as MA
import qualified Cardano.Ledger.ShelleyMA.Metadata as MA
import qualified Cardano.Ledger.ShelleyMA.TxBody as MA
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Cardano.Wallet.Primitive.Types.Address as W
import qualified Cardano.Wallet.Primitive.Types.Coin as W
import qualified Cardano.Wallet.Primitive.Types.Hash as W
import qualified Cardano.Wallet.Primitive.Types.RewardAccount as W
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
import qualified Ouroboros.Consensus.Shelley.Ledger as O
import qualified Ouroboros.Network.Block as O
import qualified Ouroboros.Network.Point as Point
import qualified Shelley.Spec.Ledger.Address as SL
import qualified Shelley.Spec.Ledger.API as SL
import qualified Shelley.Spec.Ledger.BaseTypes as SL
import qualified Shelley.Spec.Ledger.BlockChain as SL
import qualified Shelley.Spec.Ledger.Credential as SL
import qualified Shelley.Spec.Ledger.UTxO as SL

type NodeVersionData =
    (NodeToClientVersionData, CodecCBORTerm Text NodeToClientVersionData)

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
-- Genesis


genesisTip :: Tip (CardanoBlock sc)
genesisTip = legacyTip genesisPoint genesisBlockNo
  where
    -- NOTE: ourobouros-network states that:
    --
    -- There /is/ no block number if we are at genesis
    -- ('genesisBlockNo' is the block number of the first block on the chain).
    -- Usage of this function should be phased out.
    genesisBlockNo = BlockNo 0


--------------------------------------------------------------------------------
--
-- Network Parameters

-- | Settings for configuring a MainNet network client
mainnetVersionData
    :: NodeVersionData
mainnetVersionData =
    ( NodeToClientVersionData
        { networkMagic =
            NetworkMagic $ fromIntegral $ W.getProtocolMagic W.mainnetMagic
        }
    , nodeToClientCodecCBORTerm NodeToClientV_5
    )

-- | Settings for configuring a TestNet network client
testnetVersionData
    :: W.ProtocolMagic
    -> NodeVersionData
testnetVersionData pm =
    ( NodeToClientVersionData
        { networkMagic =
            NetworkMagic $ fromIntegral $ W.getProtocolMagic pm
        }
    , nodeToClientCodecCBORTerm NodeToClientV_5
    )

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

toEpochSize :: W.EpochLength -> EpochSize
toEpochSize =
    EpochSize . fromIntegral . W.unEpochLength

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
    BlockMary _blk ->
        error "FIXME: toCardanoBlockHeader, BlockMary"

toShelleyBlockHeader
    :: Era e
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

getProducer :: Era e => ShelleyBlock e -> W.PoolId
getProducer (ShelleyBlock (SL.Block (SL.BHeader header _) _) _) =
    fromPoolKeyHash $ SL.hashKey (SL.bheaderVk header)

fromCardanoBlock
    :: forall c. (SL.Crypto c)
    => W.GenesisParameters
    -> CardanoBlock c
    -> W.Block
fromCardanoBlock gp = \case
    BlockByron blk ->
        fromByronBlock gp blk
    BlockShelley blk ->
        fst $ fromShelleyBlock gp blk
    BlockAllegra blk ->
        fst $ fromAllegraBlock gp blk
    BlockMary _blk ->
        error "TODO: fromCardanoBlock, BlockMary"

toCardanoEra :: CardanoBlock c -> AnyCardanoEra
toCardanoEra = \case
    BlockByron{}   -> AnyCardanoEra ByronEra
    BlockShelley{} -> AnyCardanoEra ShelleyEra
    BlockAllegra{} -> AnyCardanoEra AllegraEra
    BlockMary{}    -> AnyCardanoEra MaryEra

fromShelleyBlock
    :: forall c. (SL.Crypto c)
    => W.GenesisParameters
    -> ShelleyBlock (SL.ShelleyEra c)
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
    :: forall c. (SL.Crypto c)
    => W.GenesisParameters
    -> ShelleyBlock (MA.ShelleyMAEra 'MA.Allegra c)
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

fromShelleyChainHash
    :: W.Hash "Genesis"
    -> ChainHash (ShelleyBlock e)
    -> W.Hash "BlockHeader"
fromShelleyChainHash genesisHash = \case
    O.GenesisHash -> coerce genesisHash
    O.BlockHash h -> fromShelleyHash h

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
        -- TODO
        -- We only use the parentHeaderHash in the
        -- 'Cardano.Wallet.Network.BlockHeaders' chain follower only required for
        -- Jörmungandr, this is therefore useless to have in 'normal' BlockHeader
        --
        -- Yet, since we also serialize these to the database, this requires
        -- some non-trivial changes. Not fixing this right now is also a
        -- possibility.
        , parentHeaderHash = W.Hash "parentHeaderHash - unused in Shelley"
        }

-- NOTE: Unsafe conversion from Natural -> Word16
fromMaxTxSize :: Natural -> Quantity "byte" Word16
fromMaxTxSize =
    Quantity . fromIntegral

fromShelleyPParams :: Maybe Bound -> SL.PParams era -> W.ProtocolParameters
fromShelleyPParams bound pp = W.ProtocolParameters
    { decentralizationLevel =
        decentralizationLevelFromPParams pp
    , txParameters =
        txParametersFromPParams pp
    , desiredNumberOfStakePools =
        desiredNumberOfStakePoolsFromPParams pp
    , minimumUTxOvalue =
        minimumUTxOvalueFromPParams pp
    , hardforkEpochNo = fromBound <$> bound
    }
  where
    fromBound (Bound _relTime _slotNo (EpochNo e)) =
        W.EpochNo $ fromIntegral e

-- | Extract the current network decentralization level from the given set of
--   protocol parameters.
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
    :: SL.PParams era
    -> W.DecentralizationLevel
decentralizationLevelFromPParams pp =
    W.DecentralizationLevel $ fromUnitInterval
        -- We must invert the value provided: (see function comment)
        $ invertUnitInterval d
  where
    d = SL._d pp

txParametersFromPParams
    :: SL.PParams era
    -> W.TxParameters
txParametersFromPParams pp = W.TxParameters
    { getFeePolicy = W.LinearFee
        (Quantity (naturalToDouble (SL._minfeeB pp)))
        (Quantity (naturalToDouble (SL._minfeeA pp)))
        (Quantity (coinToDouble (SL._keyDeposit pp)))
    , getTxMaxSize = fromMaxTxSize $ SL._maxTxSize pp
    }
  where
    naturalToDouble :: Natural -> Double
    naturalToDouble = fromIntegral

    coinToDouble :: SL.Coin -> Double
    coinToDouble (SL.Coin c) = fromIntegral c

desiredNumberOfStakePoolsFromPParams
    :: SL.PParams era
    -> Word16
desiredNumberOfStakePoolsFromPParams pp = fromIntegral (SL._nOpt pp)

minimumUTxOvalueFromPParams
    :: SL.PParams era
    -> W.Coin
minimumUTxOvalueFromPParams pp = toWalletCoin $ SL._minUTxOValue pp

-- | Convert genesis data into blockchain params and an initial set of UTxO
fromGenesisData
    :: forall e crypto. (Era e, e ~ SL.ShelleyEra crypto)
    => ShelleyGenesis e
    -> [(SL.Addr e, SL.Coin)]
    -> (W.NetworkParameters, W.Block)
fromGenesisData g initialFunds =
    ( W.NetworkParameters
        { genesisParameters = W.GenesisParameters
            { getGenesisBlockHash = dummyGenesisHash
            , getGenesisBlockDate =
                W.StartTime . sgSystemStart $ g
            , getEpochStability =
                Quantity . fromIntegral . sgSecurityParam $ g
            }
        , slottingParameters = W.SlottingParameters
            { getSlotLength =
                W.SlotLength $ sgSlotLength g
            , getEpochLength =
                W.EpochLength . fromIntegral . unEpochSize . sgEpochLength $ g
            , getActiveSlotCoefficient =
                W.ActiveSlotCoefficient . fromRational . sgActiveSlotsCoeff $ g
            }
        , protocolParameters =
            (fromShelleyPParams Nothing) . sgProtocolParams $ g
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
    genesisBlockFromTxOuts :: [(SL.Addr e, SL.Coin)] -> W.Block
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
            pseudoHash
            []
            [W.TxOut (fromShelleyAddress addr) (fromShelleyCoin c)]
            mempty
            Nothing
          where
            W.TxIn pseudoHash _ = fromShelleyTxIn $
                SL.initialFundsPseudoTxIn @e addr

fromNetworkMagic :: NetworkMagic -> W.ProtocolMagic
fromNetworkMagic (NetworkMagic magic) =
    W.ProtocolMagic (fromIntegral magic)

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
    -> Map
        (Either W.Coin W.RewardAccount)
        (Map W.PoolId (Quantity "lovelace" Word64))
fromNonMyopicMemberRewards =
    Map.map (Map.map lovelaceFromCoin . Map.mapKeys fromPoolId)
    . Map.mapKeys (bimap fromShelleyCoin fromStakeCredential)
    . O.unNonMyopicMemberRewards

optimumNumberOfPools :: forall era. SL.PParams era -> Int
optimumNumberOfPools = unsafeConvert . SL._nOpt
  where
    -- A value of ~100 can be expected, so should be fine.
    unsafeConvert :: Natural -> Int
    unsafeConvert = fromIntegral

--
-- Txs
--

fromShelleyTxId :: SL.TxId crypto -> W.Hash "Tx"
fromShelleyTxId (SL.TxId (UnsafeHash h)) = W.Hash $ fromShort h

fromShelleyTxIn
    :: Era era
    => SL.TxIn era
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
    => SL.TxOut era
    -> W.TxOut
fromShelleyTxOut (SL.TxOut addr amount) =
  W.TxOut (fromShelleyAddress addr) (fromShelleyCoin amount)

fromShelleyAddress :: SL.Addr e -> W.Address
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
    :: ( SL.ShelleyBased era
       , SL.Core.TxBody era ~ SL.TxBody era
       , SL.Core.Value era ~ SL.Coin
       , SL.Core.Metadata era ~ SL.MetaData
       )
    => SL.Tx era
    -> ( W.Tx
       , [W.DelegationCertificate]
       , [W.PoolCertificate]
       )
fromShelleyTx tx =
    ( W.Tx
        (fromShelleyTxId $ SL.txid bod)
        (map ((,W.Coin 0) . fromShelleyTxIn) (toList ins))
        (map fromShelleyTxOut (toList outs))
        (fromShelleyWdrl wdrls)
        (fromShelleyMD <$> SL.strictMaybeToMaybe mmd)
    , mapMaybe fromShelleyDelegationCert (toList certs)
    , mapMaybe fromShelleyRegistrationCert (toList certs)
    )
  where
    SL.Tx bod@(SL.TxBody ins outs certs wdrls _ _ _ _) _ mmd = tx

fromAllegraTx
    :: ( SL.ShelleyBased era
       , SL.Core.TxBody era ~ MA.TxBody era
       , SL.Core.Value era ~ SL.Coin
       , SL.Core.Metadata era ~ MA.Metadata era
       , Ord (SL.Core.Script era)
       )
    => SL.Tx era
    -> ( W.Tx
       , [W.DelegationCertificate]
       , [W.PoolCertificate]
       )
fromAllegraTx tx =
    ( W.Tx
        (fromShelleyTxId $ SL.txid bod)
        (map ((,W.Coin 0) . fromShelleyTxIn) (toList ins))
        (map fromShelleyTxOut (toList outs))
        (fromShelleyWdrl wdrls)
        (fromShelleyMD . toSLMetadata <$> SL.strictMaybeToMaybe mmd)
    , mapMaybe fromShelleyDelegationCert (toList certs)
    , mapMaybe fromShelleyRegistrationCert (toList certs)
    )
  where
    SL.Tx bod@(MA.TxBody ins outs certs wdrls _ _ _ _ _) _ mmd = tx

    -- FIXME (ADP-525): It is fine for now since we do not look at script
    -- pre-images. But this is precisely what we want as part of the
    -- multisig/script balance reporting.
    toSLMetadata (MA.Metadata blob _scripts) = SL.MetaData blob

fromShelleyWdrl :: SL.Wdrl crypto -> Map W.RewardAccount W.Coin
fromShelleyWdrl (SL.Wdrl wdrl) = Map.fromList $
    bimap (fromStakeCredential . SL.getRwdCred) fromShelleyCoin
        <$> Map.toList wdrl

fromShelleyMD :: SL.MetaData -> Cardano.TxMetadata
fromShelleyMD (SL.MetaData m) =
    Cardano.makeTransactionMetadata . fromShelleyMetaData $ m

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
            , W.poolCost = lovelaceFromCoin (SL._poolCost pp)
            , W.poolPledge = lovelaceFromCoin (SL._poolPledge pp)
            , W.poolMetadata = fromPoolMetaData <$> strictMaybeToMaybe (SL._poolMD pp)
            }
        )

    SL.DCertPool (SL.RetirePool pid (EpochNo e)) ->
        Just $ Retirement $ PoolRetirementCertificate (fromPoolKeyHash pid)
        (W.EpochNo $ fromIntegral e)

    SL.DCertDeleg{}   -> Nothing
    SL.DCertGenesis{} -> Nothing
    SL.DCertMir{}     -> Nothing

lovelaceFromCoin :: SL.Coin -> Quantity "lovelace" Word64
lovelaceFromCoin = Quantity . unsafeCoinToWord64

toWalletCoin :: SL.Coin -> W.Coin
toWalletCoin = W.Coin . unsafeCoinToWord64

-- | The reverse of 'word64ToCoin', without overflow checks.
unsafeCoinToWord64 :: SL.Coin -> Word64
unsafeCoinToWord64 (SL.Coin c) = fromIntegral c

fromPoolMetaData :: SL.PoolMetaData -> (W.StakePoolMetadataUrl, W.StakePoolMetadataHash)
fromPoolMetaData meta =
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
    either bomb id . mkPercentage . toRational . SL.intervalValue $ x
  where
    bomb = error $ mconcat
        [ "fromUnitInterval: "
        , "encountered invalid parameter value: "
        , show x
        ]

fromNetworkDiscriminant
    :: forall (n :: NetworkDiscriminant). (Typeable n)
    => Proxy n
    -> SL.Network
fromNetworkDiscriminant _ =
    case testEquality (typeRep @n) (typeRep @'Mainnet) of
        Just{}  -> SL.Mainnet
        Nothing -> SL.Testnet

toByronNetworkMagic :: W.ProtocolMagic -> Byron.NetworkMagic
toByronNetworkMagic pm@(W.ProtocolMagic magic) =
    if pm == W.mainnetMagic then
        Byron.NetworkMainOrStage
    else
        Byron.NetworkTestnet (fromIntegral magic)

-- | SealedTx are the result of rightfully constructed shelley transactions so, it
-- is relatively safe to unserialize them from CBOR.
unsealShelleyTx
    :: (HasCallStack, O.ShelleyBasedEra (era c))
    => (GenTx (ShelleyBlock (era c)) -> CardanoGenTx c)
    -> W.SealedTx
    -> CardanoGenTx c
unsealShelleyTx wrap = wrap
    . unsafeDeserialiseCbor fromCBOR
    . BL.fromStrict
    . W.getSealedTx

sealShelleyTx
    :: forall era b c. (O.ShelleyBasedEra (Cardano.ShelleyLedgerEra era))
    => (SL.Tx (Cardano.ShelleyLedgerEra era) -> (W.Tx, b, c))
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

toCardanoTxOut :: W.TxOut -> Cardano.TxOut AllegraEra
toCardanoTxOut (W.TxOut (W.Address addr) coin) =
    Cardano.TxOut addrInEra (adaOnly $ toCardanoLovelace coin)
  where
    adaOnly = Cardano.TxOutAdaOnly Cardano.AdaOnlyInAllegraEra
    addrInEra = fromMaybe (error "toCardanoTxOut: malformed address") $
        asum
        [ Cardano.AddressInEra (Cardano.ShelleyAddressInEra Cardano.ShelleyBasedEraAllegra)
            <$> deserialiseFromRawBytes AsShelleyAddress addr

        , Cardano.AddressInEra Cardano.ByronAddressInAnyEra
            <$> deserialiseFromRawBytes AsByronAddress addr
        ]

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
    rewardAcnt <- runGetOrFail' (SL.getRewardAcnt @(SL.ShelleyEra StandardCrypto)) bytes

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
        case SL.deserialiseAddr @(SL.ShelleyEra c) bytes of
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
invertUnitInterval :: SL.UnitInterval -> SL.UnitInterval
invertUnitInterval = SL.truncateUnitInterval . (1 - ) . SL.intervalValue

interval1 :: SL.UnitInterval
interval1 = SL.truncateUnitInterval (1 % 1)
