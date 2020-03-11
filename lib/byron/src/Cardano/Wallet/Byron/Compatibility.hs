{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2020 IOHK
-- License: Apache-2.0
--
-- Conversion functions and static chain settings for Byron.

module Cardano.Wallet.Byron.Compatibility
    ( Byron
    , ByronBlock
    , NodeVersionData

      -- * Chain Parameters
    , KnownNetwork (..)

      -- * Genesis
    , emptyGenesis
    , genesisTip

      -- * Conversions
    , toByronHash
    , toEpochSlots
    , toGenTx
    , toPoint
    , toSlotNo

    , fromBlockNo
    , fromByronBlock
    , fromByronHash
    , fromChainHash
    , fromGenesisData
    , fromNetworkMagic
    , fromSlotNo
    , fromTip
    , fromTxAux
    , fromTxIn
    , fromTxOut
    ) where

import Prelude

import Cardano.Binary
    ( serialize' )
import Cardano.Chain.Block
    ( ABlockOrBoundary (..), blockTxPayload )
import Cardano.Chain.Common
    ( BlockCount (..), TxFeePolicy (..), TxSizeLinear (..), unsafeGetLovelace )
import Cardano.Chain.Genesis
    ( GenesisData (..), GenesisHash (..), GenesisNonAvvmBalances (..) )
import Cardano.Chain.Slotting
    ( EpochSlots (..) )
import Cardano.Chain.Update
    ( ProtocolParameters (..) )
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
import Numeric.Natural
    ( Natural )
import Ouroboros.Consensus.Ledger.Byron
    ( ByronBlock (..), ByronHash (..), GenTx (..), decodeByronGenTx )
import Ouroboros.Network.Block
    ( BlockNo (..)
    , ChainHash
    , Point (..)
    , SlotNo (..)
    , Tip (..)
    , genesisBlockNo
    , genesisPoint
    )
import Ouroboros.Network.ChainFragment
    ( HasHeader (..) )
import Ouroboros.Network.Magic
    ( NetworkMagic (..) )
import Ouroboros.Network.NodeToClient
    ( NodeToClientVersionData (..), nodeToClientCodecCBORTerm )
import Ouroboros.Network.Point
    ( WithOrigin (..) )

import qualified Cardano.Wallet.Primitive.Types as W
import qualified Crypto.Hash as Crypto
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Ouroboros.Network.Block as O
import qualified Ouroboros.Network.Point as Point

data Byron

type NodeVersionData =
    (NodeToClientVersionData, CodecCBORTerm Text NodeToClientVersionData)

--------------------------------------------------------------------------------
--
-- Chain Parameters

-- | Embed some constants into a network type.
class KnownNetwork (n :: NetworkDiscriminant) where
    blockchainParameters
        :: W.BlockchainParameters
    versionData
        :: NodeVersionData

instance KnownNetwork 'Mainnet where
    versionData = mainnetVersionData
    blockchainParameters = W.BlockchainParameters
        { getGenesisBlockHash = W.Hash $ unsafeFromHex
            "f0f7892b5c333cffc4b3c4344de48af4cc63f55e44936196f365a9ef2244134f"
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

instance KnownNetwork 'Testnet where
    versionData = testnetVersionData
    blockchainParameters = W.BlockchainParameters
        { getGenesisBlockHash = W.Hash $ unsafeFromHex
            "96fceff972c2c06bd3bb5243c39215333be6d56aaf4823073dca31afe5038471"
        , getGenesisBlockDate =
            W.StartTime $ posixSecondsToUTCTime 1563999616
        , getFeePolicy =
            W.LinearFee (Quantity 155381) (Quantity 43.946) (Quantity 0)
        , getSlotLength =
            W.SlotLength 20
        , getEpochLength =
            W.EpochLength 21600
        , getTxMaxSize =
            Quantity 65535
        , getEpochStability =
            Quantity 2160
        , getActiveSlotCoefficient =
            W.ActiveSlotCoefficient 1.0
        }

-- NOTE
-- For MainNet and TestNet, we can get away with empty genesis blocks with
-- the following assumption:
--
-- - Users won't ever restore a wallet that has genesis UTxO.
--
-- This assumption is _true_ for any user using HD wallets (sequential or
-- random) which means, any user of cardano-wallet.
emptyGenesis :: W.BlockchainParameters -> W.Block
emptyGenesis bp = W.Block
    { transactions = []
    , delegations  = []
    , header = W.BlockHeader
        { slotId =
            W.SlotId 0 0
        , blockHeight =
            Quantity 0
        , headerHash =
            coerce $ W.getGenesisBlockHash bp
        , parentHeaderHash =
            W.Hash (BS.replicate 32 0)
        }
    }

--------------------------------------------------------------------------------
--
-- Genesis

genesisTip :: Tip ByronBlock
genesisTip = Tip genesisPoint genesisBlockNo


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
    , nodeToClientCodecCBORTerm
    )

-- | Settings for configuring a TestNet network client
testnetVersionData
    :: NodeVersionData
testnetVersionData =
    ( NodeToClientVersionData
        { networkMagic =
            NetworkMagic $ fromIntegral $ W.getProtocolMagic W.testnetMagic
        }
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
    O.GenesisHash -> coerce genesisHash
    O.BlockHash h -> fromByronHash h

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

fromTxFeePolicy :: TxFeePolicy -> W.FeePolicy
fromTxFeePolicy (TxFeePolicyTxSizeLinear (TxSizeLinear a b)) =
    W.LinearFee
        (Quantity (double a))
        (Quantity (double b))
        (Quantity 0)
  where
    double = fromIntegral . unsafeGetLovelace

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

-- | Convert non AVVM balances to genesis UTxO.
fromNonAvvmBalances :: GenesisNonAvvmBalances -> [W.TxOut]
fromNonAvvmBalances (GenesisNonAvvmBalances m) =
    fromTxOut . uncurry TxOut <$> Map.toList m

-- | Convert genesis data into blockchain params and an initial set of UTxO
fromGenesisData :: (GenesisData, GenesisHash) -> (W.BlockchainParameters, [W.TxOut])
fromGenesisData (genesisData, genesisHash) =
    ( W.BlockchainParameters
        { getGenesisBlockHash =
            W.Hash . BA.convert . unGenesisHash $ genesisHash
        , getGenesisBlockDate =
            W.StartTime . gdStartTime $ genesisData
        , getFeePolicy =
            fromTxFeePolicy . ppTxFeePolicy . gdProtocolParameters $ genesisData
        , getSlotLength =
            fromSlotDuration . ppSlotDuration . gdProtocolParameters $ genesisData
        , getEpochLength =
            fromBlockCount . gdK $ genesisData
        , getTxMaxSize =
            fromMaxTxSize . ppMaxTxSize . gdProtocolParameters $ genesisData
        , getEpochStability =
            Quantity . fromIntegral . unBlockCount . gdK $ genesisData
        , getActiveSlotCoefficient =
            W.ActiveSlotCoefficient 1.0
        }
    , fromNonAvvmBalances . gdNonAvvmBalances $ genesisData
    )

fromNetworkMagic :: NetworkMagic -> W.ProtocolMagic
fromNetworkMagic (NetworkMagic magic) =
    W.ProtocolMagic (fromIntegral magic)
