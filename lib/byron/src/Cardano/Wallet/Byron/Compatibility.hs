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

      -- * Genesis
    , genesisTip
    , genesisBlock

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
    ( Word32 )
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
    versionData = mainnetVersionData
    blockchainParameters = W.BlockchainParameters
        { getGenesisBlockHash = W.Hash $ unsafeFromHex
            "f0f7892b5c333cffc4b3c4344de48af4\
            \cc63f55e44936196f365a9ef2244134f"
        , getGenesisBlockDate =
            W.StartTime $ posixSecondsToUTCTime 1506203091
        , getFeePolicy =
            W.LinearFee (Quantity 155381) (Quantity 43.946) (Quantity 0)
        , getSlotLength =
            W.SlotLength 20
        , getEpochLength =
            W.EpochLength 21600
        , getTxMaxSize =
            Quantity 8192
        , getEpochStability =
            Quantity 2160
        , getActiveSlotCoefficient =
            W.ActiveSlotCoefficient 1.0
        }

--
-- FIXME: This currently matches the integration configuration, not TestNet
instance KnownNetwork 'Testnet where
    versionData = integrationVersionData
    blockchainParameters = W.BlockchainParameters
        { getGenesisBlockHash = W.Hash $ unsafeFromHex
            "613448b413afb8d6edc031fcc71b5f15\
            \2600e98db95d632cdc08188005deb220"
        , getGenesisBlockDate =
            W.StartTime $ posixSecondsToUTCTime 1582738775
        , getFeePolicy =
            W.LinearFee (Quantity 155381) (Quantity 43) (Quantity 0)
        , getSlotLength =
            W.SlotLength 20
        , getEpochLength =
            W.EpochLength 21600
        , getTxMaxSize =
            Quantity 4096
        , getEpochStability =
            Quantity 2160
        , getActiveSlotCoefficient =
            W.ActiveSlotCoefficient 1.0
        }


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

-- | Settings for configuring an Integration network client
--
-- FIXME: This currently matches the integration configuration, not TestNet
integrationVersionData
    :: (NodeToClientVersionData, CodecCBORTerm Text NodeToClientVersionData)
integrationVersionData =
    ( NodeToClientVersionData { networkMagic = NetworkMagic 459045235 }
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

toPoint :: W.EpochLength -> W.BlockHeader -> Point ByronBlock
toPoint epLength (W.BlockHeader sid _ h _)
    | sid == W.SlotId 0 0 = genesisPoint
    | otherwise = O.Point $ Point.block (toSlotNo epLength sid) (toByronHash h)

toSlotNo :: W.EpochLength -> W.SlotId -> SlotNo
toSlotNo epLength =
    SlotNo . W.flatSlot epLength

-- | SealedTx are the result of rightfully constructed byron transactions so, it
-- is relatively safe to unserialize them from CBOR.
toGenTx :: HasCallStack => W.SealedTx -> GenTx ByronBlock
toGenTx =
    unsafeDeserialiseCbor decodeByronGenTx . BL.fromStrict . W.getSealedTx

fromByronBlock :: W.Hash "Genesis" -> W.EpochLength -> ByronBlock -> W.Block
fromByronBlock genesisHash epLength byronBlk = case byronBlockRaw byronBlk of
  ABOBBlock blk  ->
    mkBlock $ fromTxAux <$> unTxPayload (blockTxPayload blk)
  ABOBBoundary _ ->
    mkBlock []
  where
    mkBlock :: [W.Tx] -> W.Block
    mkBlock txs = W.Block
        { header = W.BlockHeader
            { slotId =
                fromSlotNo epLength $ blockSlot byronBlk
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

fromSlotNo :: W.EpochLength -> SlotNo -> W.SlotId
fromSlotNo epLength (SlotNo sl) =
    W.fromFlatSlot epLength sl

-- FIXME unsafe conversion (Word64 -> Word32)
fromBlockNo :: BlockNo -> Quantity "block" Word32
fromBlockNo (BlockNo h) =
    Quantity (fromIntegral h)

fromTip :: W.Hash "Genesis" -> W.EpochLength -> Tip ByronBlock -> W.BlockHeader
fromTip genesisHash epLength tip = case getPoint (tipPoint tip) of
    Origin -> W.BlockHeader
        { slotId = W.SlotId 0 0
        , blockHeight = Quantity 0
        , headerHash = coerce genesisHash
        , parentHeaderHash = W.Hash (BS.replicate 32 0)
        }
    At blk -> W.BlockHeader
        { slotId = fromSlotNo epLength $ Point.blockPointSlot blk
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
