{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2020 IOHK
-- License: Apache-2.0
--
-- Conversion functions and static chain settings for Byron.

module Cardano.Wallet.Byron.Compatibility
    ( Byron
    , ByronBlock

      -- * Chain Parameters
    , KnownNetwork (..)
    , mainnetParameters
    , mainnetGenesisHash
    , mainnetStartTime
    , byronFeePolicy
    , byronSlotLength
    , byronEpochLength
    , byronTxMaxSize
    , byronEpochStability
    , byronActiveSlotCoefficient

      -- * Genesis
    , genesisTip
    , genesisBlock

      -- * Network Parameters
    , mainnetVersionData
    , testnetVersionData

      -- * Conversions
    , toByronHash
    , toEpochSlots
    , toPoint
    , toSlotNo
    , toGenTx

    , fromByronBlock
    , fromTxAux
    , fromTxIn
    , fromTxOut
    , fromByronHash
    , fromChainHash
    , fromSlotNo
    , fromBlockNo
    , fromTip
    ) where

import Prelude

import Cardano.Binary
    ( serialize' )
import Cardano.Chain.Block
    ( ABlockOrBoundary (..)
    , ABoundaryBlock (..)
    , ABoundaryBody (..)
    , ABoundaryHeader (..)
    , blockTxPayload
    )
import Cardano.Chain.Common
    ( ChainDifficulty (..), unsafeGetLovelace )
import Cardano.Chain.Slotting
    ( EpochSlots (..) )
import Cardano.Chain.UTxO
    ( Tx (..), TxAux, TxIn (..), TxOut (..), taTx, unTxPayload )
import Cardano.Crypto
    ( AbstractHash (..), hash )
import Cardano.Wallet.Primitive.AddressDerivation
    ( NetworkDiscriminant (..) )
import Cardano.Wallet.Unsafe
    ( unsafeDeserialiseCbor, unsafeFromHex )
import Codec.SerialiseTerm
    ( CodecCBORTerm )
import Data.Coerce
    ( coerce )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text )
import Data.Time.Clock.POSIX
    ( posixSecondsToUTCTime )
import Data.Word
    ( Word16, Word32 )
import GHC.Stack
    ( HasCallStack )
import Ouroboros.Consensus.Ledger.Byron
    ( ByronBlock (..), ByronHash (..), GenTx (..), decodeByronGenTx )
import Ouroboros.Network.Block
    ( BlockNo (..)
    , ChainHash (..)
    , Point (..)
    , SlotNo (..)
    , Tip (..)
    , genesisBlockNo
    , genesisPoint
    , genesisSlotNo
    )
import Ouroboros.Network.ChainFragment
    ( HasHeader (..) )
import Ouroboros.Network.Magic
    ( NetworkMagic (..) )
import Ouroboros.Network.NodeToClient
    ( NodeToClientVersionData (..), nodeToClientCodecCBORTerm )
import Ouroboros.Network.Point
    ( WithOrigin (..) )

import qualified Cardano.Chain.Genesis as Genesis
import qualified Crypto.Hash as Crypto
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.List.NonEmpty as NE
import qualified Ouroboros.Network.Block as O
import qualified Ouroboros.Network.Point as Point

import qualified Cardano.Wallet.Primitive.Types as W

data Byron

--------------------------------------------------------------------------------
--
-- Chain Parameters

-- | Embed some constants into a network type.
class KnownNetwork (n :: NetworkDiscriminant) where
    blockchainParameters
        :: W.BlockchainParameters
    versionData
        :: ( NodeToClientVersionData
           , CodecCBORTerm Text NodeToClientVersionData
           )

instance KnownNetwork 'Mainnet where
    blockchainParameters = mainnetParameters
    versionData = mainnetVersionData

mainnetParameters
    :: W.BlockchainParameters
mainnetParameters = W.BlockchainParameters
    { getGenesisBlockHash = mainnetGenesisHash
    , getGenesisBlockDate = mainnetStartTime
    , getFeePolicy = byronFeePolicy
    , getSlotLength = byronSlotLength
    , getEpochLength = byronEpochLength
    , getTxMaxSize = byronTxMaxSize
    , getEpochStability = byronEpochStability
    , getActiveSlotCoefficient = byronActiveSlotCoefficient
    }

-- | Hard-coded mainnet genesis hash
mainnetGenesisHash :: W.Hash "Genesis"
mainnetGenesisHash = W.Hash $ unsafeFromHex
    "f0f7892b5c333cffc4b3c4344de48af4\
    \cc63f55e44936196f365a9ef2244134f"

-- | Hard-coded mainnet start time
mainnetStartTime :: W.StartTime
mainnetStartTime =
    W.StartTime $ posixSecondsToUTCTime 1506203091

-- | Hard-coded fee policy for Cardano on Byron
byronFeePolicy :: W.FeePolicy
byronFeePolicy =
    W.LinearFee (Quantity 155381) (Quantity 43.946) (Quantity 0)

-- | Hard-coded slot duration
byronSlotLength :: W.SlotLength
byronSlotLength =
    W.SlotLength 20
-- | Hard-coded byron epoch length
byronEpochLength :: W.EpochLength
byronEpochLength =
    W.EpochLength 21600

-- | Hard-coded max transaction size
byronTxMaxSize :: Quantity "byte" Word16
byronTxMaxSize =
    Quantity 8192

-- | Hard-coded epoch stability (a.k.a 'k')
byronEpochStability :: Quantity "block" Word32
byronEpochStability =
    Quantity 2160

-- | Hard-coded active slot coefficient (a.k.a 'f' in Ouroboros/Praos)
byronActiveSlotCoefficient :: W.ActiveSlotCoefficient
byronActiveSlotCoefficient =
    W.ActiveSlotCoefficient 1.0

--------------------------------------------------------------------------------
--
-- Genesis

genesisTip :: Tip ByronBlock
genesisTip = Tip genesisPoint genesisBlockNo

-- FIXME
-- Actually figure out a way to get this from the network. For Haskell nodes,
-- there's actually no such thing as a genesis block. But there's a genesis
-- UTxO and a genesis hash. So, we might be able to ajust our abstractions to
-- this.
genesisBlock :: ByronHash -> ByronBlock
genesisBlock genesisHash = ByronBlock
    { byronBlockRaw = ABOBBoundary $ ABoundaryBlock
        { boundaryBlockLength = 0
        , boundaryHeader = UnsafeABoundaryHeader
          { boundaryPrevHash = Left (Genesis.GenesisHash (coerce genesisHash))
          , boundaryEpoch = 0
          , boundaryDifficulty = ChainDifficulty 0
          , boundaryHeaderAnnotation = mempty
          }
        , boundaryBody = ABoundaryBody mempty
        , boundaryAnnotation = mempty
        }
    , byronBlockSlotNo = genesisSlotNo
    , byronBlockHash = genesisHash
    }

--------------------------------------------------------------------------------
--
-- Network Parameters

-- | Settings for configuring a MainNet network client
mainnetVersionData
    :: (NodeToClientVersionData, CodecCBORTerm Text NodeToClientVersionData)
mainnetVersionData =
    ( NodeToClientVersionData { networkMagic = NetworkMagic 764824073 }
    , nodeToClientCodecCBORTerm
    )

-- | Settings
testnetVersionData
    :: (NodeToClientVersionData, CodecCBORTerm Text NodeToClientVersionData)
testnetVersionData =
    ( NodeToClientVersionData { networkMagic = NetworkMagic 1097911063 }
    , nodeToClientCodecCBORTerm
    )

--------------------------------------------------------------------------------
--
-- Type Conversions

toByronHash :: W.Hash "BlockHeader" -> ByronHash
toByronHash (W.Hash bytes) =
    case Crypto.digestFromByteString bytes of
        Just digest ->
            ByronHash $ AbstractHash digest
        Nothing ->
            error "unsafeHash: failed to convert bytes to hash?"

toEpochSlots :: W.EpochLength -> EpochSlots
toEpochSlots =
    EpochSlots . fromIntegral . W.unEpochLength

toPoint :: W.BlockHeader -> Point ByronBlock
toPoint (W.BlockHeader sid _ h _)
    | sid == W.SlotId 0 0 = genesisPoint
    | otherwise = O.Point $ Point.block (toSlotNo sid) (toByronHash h)

toSlotNo :: W.SlotId -> SlotNo
toSlotNo =
    SlotNo . W.flatSlot byronEpochLength

-- | SealedTx are the result of rightfully constructed byron transactions so, it
-- is relatively safe to unserialize them from CBOR.
toGenTx :: HasCallStack => W.SealedTx -> GenTx ByronBlock
toGenTx =
    unsafeDeserialiseCbor decodeByronGenTx . BL.fromStrict . W.getSealedTx

fromByronBlock :: W.Hash "Genesis" -> ByronBlock -> W.Block
fromByronBlock genesisHash byronBlk = case byronBlockRaw byronBlk of
  ABOBBlock blk  ->
    mkBlock $ fromTxAux <$> unTxPayload (blockTxPayload blk)
  ABOBBoundary _ ->
    mkBlock []
  where
    mkBlock :: [W.Tx] -> W.Block
    mkBlock txs = W.Block
        { header = W.BlockHeader
            { slotId =
                fromSlotNo $ blockSlot byronBlk
            , blockHeight =
                fromBlockNo $ blockNo byronBlk
            , headerHash =
                fromByronHash $ blockHash byronBlk
            , parentHeaderHash =
                fromChainHash genesisHash $ blockPrevHash byronBlk
            }
        , transactions = txs
        , delegations  = []
        }

fromTxAux :: TxAux -> W.Tx
fromTxAux txAux = case taTx txAux of
    tx@(UnsafeTx inputs outputs _attributes) -> W.Tx
        { txId = W.Hash $ BA.convert $ hash tx

        -- TODO: Review 'W.Tx' to not require resolved inputs but only inputs
        , resolvedInputs =
            (, W.Coin 0) . fromTxIn <$> NE.toList inputs

        , outputs =
            fromTxOut <$> NE.toList outputs
        }

fromTxIn :: TxIn -> W.TxIn
fromTxIn (TxInUtxo id_ ix) = W.TxIn
    { inputId = W.Hash $ BA.convert id_
    , inputIx = ix
    }

fromTxOut :: TxOut -> W.TxOut
fromTxOut (TxOut addr coin) = W.TxOut
    { address = W.Address (serialize' addr)
    , coin = W.Coin (unsafeGetLovelace coin)
    }

fromByronHash :: ByronHash -> W.Hash "BlockHeader"
fromByronHash =
    W.Hash . BA.convert . unByronHash

fromChainHash :: W.Hash "Genesis" -> ChainHash ByronBlock -> W.Hash "BlockHeader"
fromChainHash genesisHash = \case
    GenesisHash -> coerce genesisHash
    BlockHash h -> fromByronHash h

fromSlotNo :: SlotNo -> W.SlotId
fromSlotNo (SlotNo sl) =
    W.fromFlatSlot byronEpochLength sl

-- FIXME unsafe conversion (Word64 -> Word32)
fromBlockNo :: BlockNo -> Quantity "block" Word32
fromBlockNo (BlockNo h) =
    Quantity (fromIntegral h)

fromTip :: W.Hash "Genesis" -> Tip ByronBlock -> W.BlockHeader
fromTip genesisHash tip = case getPoint (tipPoint tip) of
    Origin -> W.BlockHeader
        { slotId = W.SlotId 0 0
        , blockHeight = Quantity 0
        , headerHash = coerce genesisHash
        , parentHeaderHash = W.Hash (BS.replicate 32 0)
        }
    At blk -> W.BlockHeader
        { slotId = fromSlotNo $ Point.blockPointSlot blk
        , blockHeight = fromBlockNo $ tipBlockNo tip
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
