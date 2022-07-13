{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2020 IOHK
-- License: Apache-2.0
--
-- Conversion functions and static chain settings for Byron.

module Cardano.Wallet.Byron.Compatibility
    ( -- * Chain Parameters
      mainnetNetworkParameters
    , maryTokenBundleMaxSize

      -- * Genesis
    , emptyGenesis
    , genesisBlockFromTxOuts

      -- * Conversions
    , fromBlockNo
    , fromByronBlock
    , toByronBlockHeader
    , fromByronHash
    , fromChainHash
    , fromGenesisData
    , byronCodecConfig
    , fromProtocolMagicId
    , fromTxAux
    , fromTxIn
    , fromTxOut

    , protocolParametersFromUpdateState
    ) where

import Prelude

import Cardano.Binary
    ( serialize' )
import Cardano.Chain.Block
    ( ABlockOrBoundary (..), blockTxPayload )
import Cardano.Chain.Common
    ( BlockCount (..)
    , Lovelace
    , TxFeePolicy (..)
    , TxSizeLinear (..)
    , unsafeGetLovelace
    )
import Cardano.Chain.Genesis
    ( GenesisData (..), GenesisHash (..), GenesisNonAvvmBalances (..) )
import Cardano.Chain.Slotting
    ( EpochSlots (..) )
import Cardano.Chain.Update
    ( ProtocolParameters (..) )
import Cardano.Chain.UTxO
    ( ATxAux (..), Tx (..), TxIn (..), TxOut (..), taTx, unTxPayload )
import Cardano.Crypto
    ( serializeCborHash )
import Cardano.Crypto.ProtocolMagic
    ( ProtocolMagicId, unProtocolMagicId )
import Cardano.Wallet.Primitive.Types.MinimumUTxO
    ( minimumUTxONone )
import Cardano.Wallet.Unsafe
    ( unsafeFromHex )
import Crypto.Hash.Utils
    ( blake2b256 )
import Data.Coerce
    ( coerce )
import Data.Quantity
    ( Quantity (..) )
import Data.Time.Clock.POSIX
    ( posixSecondsToUTCTime )
import Data.Word
    ( Word16, Word32 )
import Numeric.Natural
    ( Natural )
import Ouroboros.Consensus.Block.Abstract
    ( headerPrevHash )
import Ouroboros.Consensus.Byron.Ledger
    ( ByronBlock (..), ByronHash (..) )
import Ouroboros.Consensus.Byron.Ledger.Config
    ( CodecConfig (..) )
import Ouroboros.Consensus.HardFork.History.Summary
    ( Bound (..) )
import Ouroboros.Network.Block
    ( BlockNo (..), ChainHash, SlotNo (..) )

import qualified Cardano.Api.Shelley as Node
import qualified Cardano.Chain.Update as Update
import qualified Cardano.Chain.Update.Validation.Interface as Update
import qualified Cardano.Crypto.Hashing as CC
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Cardano.Wallet.Primitive.Types.Address as W
import qualified Cardano.Wallet.Primitive.Types.Coin as Coin
import qualified Cardano.Wallet.Primitive.Types.Coin as W
import qualified Cardano.Wallet.Primitive.Types.Hash as W
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.Tx as W
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Ouroboros.Consensus.Block as O

--------------------------------------------------------------------------------
--
-- Chain Parameters

mainnetNetworkParameters :: W.NetworkParameters
mainnetNetworkParameters = W.NetworkParameters
    { genesisParameters = W.GenesisParameters
        { getGenesisBlockHash = W.Hash $ unsafeFromHex
            "5f20df933584822601f9e3f8c024eb5eb252fe8cefb24d1317dc3d432e940ebb"
        , getGenesisBlockDate =
            W.StartTime $ posixSecondsToUTCTime 1506203091
        }
    , slottingParameters = W.SlottingParameters
        { getSlotLength =
            W.SlotLength 20
        , getEpochLength =
            W.EpochLength 21600
        , getActiveSlotCoefficient =
            W.ActiveSlotCoefficient 1.0
        , getSecurityParameter =
            Quantity 2160
        }
    , protocolParameters = W.ProtocolParameters
        { decentralizationLevel =
            minBound
        , txParameters = W.TxParameters
            { getFeePolicy =
                W.LinearFee $
                    W.LinearFunction { intercept = 155381, slope = 43.946 }
            , getTxMaxSize =
                Quantity 4096
            , getTokenBundleMaxSize = maryTokenBundleMaxSize
            , getMaxExecutionUnits = W.ExecutionUnits 0 0
            }
        , desiredNumberOfStakePools = 0
        , minimumUTxO = minimumUTxONone
        , stakeKeyDeposit = W.Coin 0
        , eras = W.emptyEraInfo
        -- Collateral inputs were not supported or required in Byron:
        , maximumCollateralInputCount = 0
        , minimumCollateralPercentage = 0
        , executionUnitPrices = Nothing
        , currentNodeProtocolParameters = Nothing
        }
    }

-- | The max size of token bundles hard-coded in Mary.
--
-- The concept was introduced in Mary, and hard-coded to this value. In Alonzo
-- it became an updateable protocol parameter.
--
-- NOTE: A bit weird to define in "Cardano.Wallet.Byron.Compatibility", but we
-- need it both here and in "Cardano.Wallet.Shelley.Compatibility".
maryTokenBundleMaxSize :: W.TokenBundleMaxSize
maryTokenBundleMaxSize = W.TokenBundleMaxSize $ W.TxSize 4000

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
-- Genesis


-- | Construct a ("fake") genesis block from genesis transaction outputs.
--
-- The genesis data on haskell nodes is not a block at all, unlike the block0 on
-- jormungandr. This function is a method to deal with the discrepancy.
genesisBlockFromTxOuts :: W.GenesisParameters -> [W.TxOut] -> W.Block
genesisBlockFromTxOuts gp outs = W.Block
    { delegations  = []
    , header = W.BlockHeader
        { slotNo =
            SlotNo 0
        , blockHeight =
            Quantity 0
        , headerHash =
            coerce $ W.getGenesisBlockHash gp
        , parentHeaderHash =
            Nothing
        }
    , transactions = mkTx <$> outs
    }
  where
    mkTx out@(W.TxOut (W.Address bytes) _) = W.Tx
        { txId = W.Hash $ blake2b256 bytes
        , fee = Nothing
        , resolvedInputs = []
        , resolvedCollateralInputs = []
        , outputs = [out]
        , collateralOutput = Nothing
        , withdrawals = mempty
        , metadata = Nothing
        , scriptValidity = Nothing
        }

--------------------------------------------------------------------------------
--
-- Type Conversions

toEpochSlots :: W.EpochLength -> EpochSlots
toEpochSlots =
    EpochSlots . fromIntegral . W.unEpochLength

byronCodecConfig :: W.SlottingParameters -> CodecConfig ByronBlock
byronCodecConfig W.SlottingParameters{getEpochLength} =
    ByronCodecConfig (toEpochSlots getEpochLength)

fromByronBlock :: W.GenesisParameters -> ByronBlock -> W.Block
fromByronBlock gp byronBlk = case byronBlockRaw byronBlk of
  ABOBBlock blk  ->
    mkBlock $ fromTxAux <$> unTxPayload (blockTxPayload blk)
  ABOBBoundary _ ->
    mkBlock []
  where
    mkBlock :: [W.Tx] -> W.Block
    mkBlock txs = W.Block
        { header = toByronBlockHeader gp byronBlk
        , transactions = txs
        , delegations  = []
        }

toByronBlockHeader
    :: W.GenesisParameters
    -> ByronBlock
    -> W.BlockHeader
toByronBlockHeader gp blk = W.BlockHeader
    { slotNo =
        O.blockSlot blk
    , blockHeight =
        fromBlockNo $ O.blockNo blk
    , headerHash =
        fromByronHash $ O.blockHash blk
    , parentHeaderHash = Just $
        fromChainHash (W.getGenesisBlockHash gp) $
        headerPrevHash (O.getHeader blk)
    }

fromTxAux :: ATxAux a -> W.Tx
fromTxAux txAux = case taTx txAux of
    tx@(UnsafeTx inputs outputs _attributes) -> W.Tx
        { txId = W.Hash $ CC.hashToBytes $ serializeCborHash tx

        , fee = Nothing

        -- TODO: Review 'W.Tx' to not require resolved inputs but only inputs
        , resolvedInputs =
            (, W.Coin 0) . fromTxIn <$> NE.toList inputs

        , resolvedCollateralInputs = []

        , outputs =
            fromTxOut <$> NE.toList outputs

        , collateralOutput =
            Nothing

        , withdrawals =
            mempty

        , metadata =
            Nothing

        , scriptValidity =
            Nothing
        }

fromTxIn :: TxIn -> W.TxIn
fromTxIn (TxInUtxo id_ ix) = W.TxIn
    { inputId = W.Hash $ CC.hashToBytes id_
    , inputIx = fromIntegral ix
    }

fromTxOut :: TxOut -> W.TxOut
fromTxOut (TxOut addr coin) = W.TxOut
    { address = W.Address (serialize' addr)
    , tokens = TokenBundle.fromCoin $ Coin.fromWord64 $ unsafeGetLovelace coin
    }

fromByronHash :: ByronHash -> W.Hash "BlockHeader"
fromByronHash =
    W.Hash . CC.hashToBytes . unByronHash

fromChainHash :: W.Hash "Genesis" -> ChainHash ByronBlock -> W.Hash "BlockHeader"
fromChainHash genesisHash = \case
    O.GenesisHash -> coerce genesisHash
    O.BlockHash h -> fromByronHash h

-- FIXME unsafe conversion (Word64 -> Word32)
fromBlockNo :: BlockNo -> Quantity "block" Word32
fromBlockNo (BlockNo h) =
    Quantity (fromIntegral h)

fromTxFeePolicy :: TxFeePolicy -> W.FeePolicy
fromTxFeePolicy (TxFeePolicyTxSizeLinear (TxSizeLinear a b)) =
    W.LinearFee $ W.LinearFunction
        { intercept = lovelaceToDouble a
        , slope = rationalToDouble b
        }
  where
    lovelaceToDouble :: Lovelace -> Double
    lovelaceToDouble = fromIntegral . unsafeGetLovelace

    rationalToDouble :: Rational -> Double
    rationalToDouble = fromRational

fromSlotDuration :: Natural -> W.SlotLength
fromSlotDuration =
    W.SlotLength . toEnum . (*1_000_000_000) . fromIntegral

-- NOTE: Unsafe conversion from Word64 -> Word32 here.
--
-- Although... Word64 for `k`? For real?
fromBlockCount :: BlockCount -> W.EpochLength
fromBlockCount (BlockCount k) =
    W.EpochLength (10 * fromIntegral k)

-- NOTE: Unsafe conversion from Natural -> Word16
fromMaxSize :: Natural -> Quantity "byte" Word16
fromMaxSize =
    Quantity . fromIntegral

protocolParametersFromPP
    :: W.EraInfo Bound
    -> Maybe Node.ProtocolParameters
    -> Update.ProtocolParameters
    -> W.ProtocolParameters
protocolParametersFromPP eraInfo currentNodeProtocolParameters pp =
    W.ProtocolParameters
        { decentralizationLevel = minBound
        , txParameters = W.TxParameters
            { getFeePolicy = fromTxFeePolicy $ Update.ppTxFeePolicy pp
            , getTxMaxSize = fromMaxSize $ Update.ppMaxTxSize pp
            , getTokenBundleMaxSize = maryTokenBundleMaxSize
            , getMaxExecutionUnits = W.ExecutionUnits 0 0
            }
        , desiredNumberOfStakePools = 0
        , minimumUTxO = minimumUTxONone
        , stakeKeyDeposit = W.Coin 0
        , eras = fromBound <$> eraInfo
        -- Collateral inputs were not supported or required in Byron:
        , maximumCollateralInputCount = 0
        , minimumCollateralPercentage = 0
        , executionUnitPrices = Nothing
        , currentNodeProtocolParameters
        }
  where
    fromBound (Bound _relTime _slotNo (O.EpochNo e)) =
        W.EpochNo $ fromIntegral e

-- | Extract the protocol parameters relevant to the wallet out of the
--   cardano-chain update state record.
protocolParametersFromUpdateState
    :: W.EraInfo Bound
    -> Maybe Node.ProtocolParameters
    -> Update.State
    -> W.ProtocolParameters
protocolParametersFromUpdateState b ppNode =
    (protocolParametersFromPP b ppNode) . Update.adoptedProtocolParameters

-- | Convert non AVVM balances to genesis UTxO.
fromNonAvvmBalances :: GenesisNonAvvmBalances -> [W.TxOut]
fromNonAvvmBalances (GenesisNonAvvmBalances m) =
    fromTxOut . uncurry TxOut <$> Map.toList m

-- | Convert genesis data into blockchain params and an initial set of UTxO
fromGenesisData :: (GenesisData, GenesisHash) -> (W.NetworkParameters, [W.TxOut])
fromGenesisData (genesisData, genesisHash) =
    ( W.NetworkParameters
        { genesisParameters = W.GenesisParameters
            { getGenesisBlockHash =
                W.Hash . CC.hashToBytes . unGenesisHash $ genesisHash
            , getGenesisBlockDate =
                W.StartTime . gdStartTime $ genesisData
            }
        , slottingParameters = W.SlottingParameters
            { getSlotLength = fromSlotDuration . ppSlotDuration $
                gdProtocolParameters genesisData
            , getEpochLength = fromBlockCount . gdK $ genesisData
            , getActiveSlotCoefficient = W.ActiveSlotCoefficient 1.0
            , getSecurityParameter = Quantity . fromIntegral . unBlockCount $
                gdK genesisData
            }
        , protocolParameters =
            -- emptyEraInfo contains no info about byron. Should we add it?
            protocolParametersFromPP W.emptyEraInfo Nothing $
                gdProtocolParameters genesisData
        }
    , fromNonAvvmBalances . gdNonAvvmBalances $ genesisData
    )

fromProtocolMagicId :: ProtocolMagicId -> W.ProtocolMagic
fromProtocolMagicId = W.ProtocolMagic . fromIntegral . unProtocolMagicId
