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

-- Orphan instances for {Encode,Decode}Address until we get rid of the
-- Jörmungandr dual support.
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Copyright: © 2020 IOHK
-- License: Apache-2.0
--
-- Conversion functions and static chain settings for Byron.

module Cardano.Wallet.Byron.Compatibility
    ( -- * Chain Parameters
      mainnetNetworkParameters

      -- * Genesis
    , emptyGenesis
    , genesisBlockFromTxOuts

      -- * Conversions
    , toByronHash
    , toGenTx
    , toPoint

    , fromBlockNo
    , fromByronBlock
    , toByronBlockHeader
    , fromByronHash
    , fromChainHash
    , fromGenesisData
    , byronCodecConfig
    , fromNetworkMagic
    , fromProtocolMagicId
    , fromTip
    , fromTxAux
    , fromTxIn
    , fromTxOut

    , protocolParametersFromUpdateState
    ) where

import Prelude

import Cardano.Binary
    ( fromCBOR, serialize' )
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
import Cardano.Chain.MempoolPayload
    ( AMempoolPayload (..) )
import Cardano.Chain.Slotting
    ( EpochSlots (..) )
import Cardano.Chain.Update
    ( ProtocolParameters (..) )
import Cardano.Chain.UTxO
    ( ATxAux (..)
    , Tx (..)
    , TxIn (..)
    , TxOut (..)
    , annotateTxAux
    , taTx
    , unTxPayload
    )
import Cardano.Crypto
    ( serializeCborHash )
import Cardano.Crypto.ProtocolMagic
    ( ProtocolMagicId, unProtocolMagicId )
import Cardano.Wallet.Unsafe
    ( unsafeDeserialiseCbor, unsafeFromHex )
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
import GHC.Stack
    ( HasCallStack )
import Numeric.Natural
    ( Natural )
import Ouroboros.Consensus.Block.Abstract
    ( headerPrevHash )
import Ouroboros.Consensus.Byron.Ledger
    ( ByronBlock (..), ByronHash (..), GenTx, fromMempoolPayload )
import Ouroboros.Consensus.Byron.Ledger.Config
    ( CodecConfig (..) )
import Ouroboros.Consensus.HardFork.History.Summary
    ( Bound (..) )
import Ouroboros.Network.Block
    ( BlockNo (..), ChainHash, Point (..), SlotNo (..), Tip (..), getTipPoint )
import Ouroboros.Network.Magic
    ( NetworkMagic (..) )
import Ouroboros.Network.Point
    ( WithOrigin (..) )

import qualified Cardano.Chain.Update as Update
import qualified Cardano.Chain.Update.Validation.Interface as Update
import qualified Cardano.Crypto.Hashing as CC
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Cardano.Wallet.Primitive.Types.Address as W
import qualified Cardano.Wallet.Primitive.Types.Coin as W
import qualified Cardano.Wallet.Primitive.Types.Hash as W
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.Tx as W
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Ouroboros.Consensus.Block as O
import qualified Ouroboros.Network.Block as O
import qualified Ouroboros.Network.Point as Point

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
                W.LinearFee (Quantity 155381) (Quantity 43.946)
            , getTxMaxSize =
                Quantity 4096
            }
        , desiredNumberOfStakePools = 0
        , minimumUTxOvalue = W.Coin 0
        , stakeKeyDeposit = W.Coin 0
        , eras = W.emptyEraInfo
        }
    }

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
            W.Hash (BS.replicate 32 0)
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
            W.Hash (BS.replicate 32 0)
        }
    , transactions = mkTx <$> outs
    }
  where
    mkTx out@(W.TxOut (W.Address bytes) _) =
        W.Tx (W.Hash $ blake2b256 bytes) Nothing [] [out] mempty Nothing

--------------------------------------------------------------------------------
--
-- Type Conversions

toByronHash :: W.Hash "BlockHeader" -> ByronHash
toByronHash (W.Hash bytes) =
    case CC.hashFromBytes bytes of
        Just h ->
            ByronHash h
        Nothing ->
            error "unsafeHash: failed to convert bytes to hash?"

toEpochSlots :: W.EpochLength -> EpochSlots
toEpochSlots =
    EpochSlots . fromIntegral . W.unEpochLength

-- | Magic value for the absence of a block.
hashOfNoParent :: W.Hash "BlockHeader"
hashOfNoParent = W.Hash . BS.pack $ replicate 0 32

toPoint
    :: W.Hash "Genesis"
    -> W.BlockHeader
    -> Point ByronBlock
toPoint genesisH (W.BlockHeader sl _ h _)
  | h == (coerce genesisH) = O.GenesisPoint
  | otherwise = O.Point $ Point.block sl (toByronHash h)

-- | SealedTx are the result of rightfully constructed byron transactions so, it
-- is relatively safe to unserialize them from CBOR.
toGenTx :: HasCallStack => W.SealedTx -> GenTx ByronBlock
toGenTx =
    fromMempoolPayload
    . MempoolTx
    . annotateTxAux
    . unsafeDeserialiseCbor fromCBOR
    . BL.fromStrict
    . W.getSealedTx

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
    , parentHeaderHash =
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

        , outputs =
            fromTxOut <$> NE.toList outputs

        , withdrawals =
            mempty

        , metadata =
            Nothing
        }

fromTxIn :: TxIn -> W.TxIn
fromTxIn (TxInUtxo id_ ix) = W.TxIn
    { inputId = W.Hash $ CC.hashToBytes id_
    , inputIx = ix
    }

fromTxOut :: TxOut -> W.TxOut
fromTxOut (TxOut addr coin) = W.TxOut
    { address = W.Address (serialize' addr)
    , tokens = TokenBundle.fromCoin $ W.Coin $ unsafeGetLovelace coin
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


fromTip :: W.Hash "Genesis" -> Tip ByronBlock -> W.BlockHeader
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
        , headerHash = fromByronHash $ Point.blockPointHash blk
        -- TODO
        -- We only use the parentHeaderHash in the
        -- 'Cardano.Wallet.Network.BlockHeaders' chain follower only required for
        -- Jörmungandr, this is therefore useless to have in 'normal' BlockHeader
        --
        -- Yet, since we also serialize these to the database, this requires
        -- some non-trivial changes. Not fixing this right now is also a
        -- possibility.
        , parentHeaderHash = W.Hash "parentHeaderHash - unused in Byron"
        }
  where
    -- TODO: This function was marked deprecated in ouroboros-network.
    -- It is wrong, because `Origin` doesn't have a block number.
    -- We should remove it.
    getLegacyTipBlockNo t = case O.getTipBlockNo t of
        Origin -> BlockNo 0
        At x -> x

fromTxFeePolicy :: TxFeePolicy -> W.FeePolicy
fromTxFeePolicy (TxFeePolicyTxSizeLinear (TxSizeLinear a b)) =
    W.LinearFee
        (Quantity (lovelaceToDouble a))
        (Quantity (rationalToDouble b))
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
fromMaxTxSize :: Natural -> Quantity "byte" Word16
fromMaxTxSize =
    Quantity . fromIntegral

protocolParametersFromPP
    :: W.EraInfo Bound
    -> Update.ProtocolParameters
    -> W.ProtocolParameters
protocolParametersFromPP eraInfo pp = W.ProtocolParameters
    { decentralizationLevel = minBound
    , txParameters = W.TxParameters
        { getFeePolicy = fromTxFeePolicy $ Update.ppTxFeePolicy pp
        , getTxMaxSize = fromMaxTxSize $ Update.ppMaxTxSize pp
        }
    , desiredNumberOfStakePools = 0
    , minimumUTxOvalue = W.Coin 0
    , stakeKeyDeposit = W.Coin 0
    , eras = fromBound <$> eraInfo
    }
  where
    fromBound (Bound _relTime _slotNo (O.EpochNo e)) =
        W.EpochNo $ fromIntegral e

-- | Extract the protocol parameters relevant to the wallet out of the
--   cardano-chain update state record.
protocolParametersFromUpdateState
    :: W.EraInfo Bound
    -> Update.State
    -> W.ProtocolParameters
protocolParametersFromUpdateState b =
    (protocolParametersFromPP b) . Update.adoptedProtocolParameters

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
            { getSlotLength =
                fromSlotDuration . ppSlotDuration . gdProtocolParameters $ genesisData
            , getEpochLength =
                fromBlockCount . gdK $ genesisData
            , getActiveSlotCoefficient =
                W.ActiveSlotCoefficient 1.0
            , getSecurityParameter =
                Quantity . fromIntegral . unBlockCount . gdK $ genesisData
            }
        , protocolParameters =
            -- emptyEraInfo contains no info about byron. Should we add it?
            (protocolParametersFromPP W.emptyEraInfo) . gdProtocolParameters $ genesisData
        }
    , fromNonAvvmBalances . gdNonAvvmBalances $ genesisData
    )

fromNetworkMagic :: NetworkMagic -> W.ProtocolMagic
fromNetworkMagic (NetworkMagic magic) =
    W.ProtocolMagic (fromIntegral magic)

fromProtocolMagicId :: ProtocolMagicId -> W.ProtocolMagic
fromProtocolMagicId = W.ProtocolMagic . fromIntegral . unProtocolMagicId
