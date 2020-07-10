{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
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
    ( Byron
    , ByronBlock
    , NodeVersionData

      -- * Chain Parameters
    , mainnetVersionData
    , testnetVersionData

    , mainnetNetworkParameters

      -- * Genesis
    , emptyGenesis
    , genesisTip

      -- * Conversions
    , toByronHash
    , toGenTx
    , toPoint
    , toSlotNo

    , fromBlockNo
    , fromByronBlock
    , fromByronHash
    , fromChainHash
    , fromGenesisData
    , byronCodecConfig
    , fromNetworkMagic
    , fromProtocolMagicId
    , fromSlotNo
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
    ( Tx (..), TxAux, TxIn (..), TxOut (..), annotateTxAux, taTx, unTxPayload )
import Cardano.Crypto
    ( serializeCborHash )
import Cardano.Crypto.ProtocolMagic
    ( ProtocolMagicId, unProtocolMagicId )
import Cardano.Wallet.Api.Types
    ( DecodeAddress (..), EncodeAddress (..) )
import Cardano.Wallet.Primitive.AddressDerivation
    ( NetworkDiscriminant (..) )
import Cardano.Wallet.Primitive.AddressDerivation.Byron
    ( decodeLegacyAddress )
import Cardano.Wallet.Unsafe
    ( unsafeDeserialiseCbor, unsafeFromHex )
import Data.ByteString
    ( ByteString )
import Data.ByteString.Base58
    ( bitcoinAlphabet, decodeBase58, encodeBase58 )
import Data.Coerce
    ( coerce )
import Data.Either.Extra
    ( maybeToEither )
import Data.Function
    ( (&) )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( TextDecodingError (..) )
import Data.Time.Clock.POSIX
    ( posixSecondsToUTCTime )
import Data.Word
    ( Word16, Word32 )
import GHC.Stack
    ( HasCallStack )
import GHC.TypeLits
    ( KnownNat )
import Numeric.Natural
    ( Natural )
import Ouroboros.Consensus.Block.Abstract
    ( getHeader, headerPrevHash )
import Ouroboros.Consensus.Byron.Ledger
    ( ByronBlock (..), ByronHash (..), GenTx, fromMempoolPayload )
import Ouroboros.Consensus.Byron.Ledger.Config
    ( CodecConfig (..) )
import Ouroboros.Consensus.Config.SecurityParam
    ( SecurityParam (..) )
import Ouroboros.Network.Block
    ( BlockNo (..)
    , ChainHash
    , Point (..)
    , SlotNo (..)
    , Tip (..)
    , blockHash
    , blockNo
    , blockSlot
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
    ( NodeToClientVersionData (..), nodeToClientCodecCBORTerm )
import Ouroboros.Network.Point
    ( WithOrigin (..) )

import qualified Cardano.Chain.Update as Update
import qualified Cardano.Chain.Update.Validation.Interface as Update
import qualified Cardano.Crypto.Hashing as CC
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Text.Encoding as T
import qualified Ouroboros.Network.Block as O
import qualified Ouroboros.Network.Point as Point

data Byron

type NodeVersionData =
    (NodeToClientVersionData, CodecCBORTerm Text NodeToClientVersionData)

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
        , getSlotLength =
            W.SlotLength 20
        , getEpochLength =
            W.EpochLength 21600
        , getEpochStability =
            Quantity 2160
        , getActiveSlotCoefficient =
            W.ActiveSlotCoefficient 1.0
        }
    , protocolParameters = W.ProtocolParameters
        { decentralizationLevel =
            minBound
        , txParameters = W.TxParameters
            { getFeePolicy =
                W.LinearFee (Quantity 155381) (Quantity 43.946) (Quantity 0)
            , getTxMaxSize =
                Quantity 4096
            }
        , desiredNumberOfStakePools = 0
        , minimumUTxOvalue = W.Coin 0
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
        { slotId =
            W.SlotId 0 0
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


genesisTip :: Tip ByronBlock
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
    , nodeToClientCodecCBORTerm
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
    , nodeToClientCodecCBORTerm
    )

--------------------------------------------------------------------------------
--
-- Type Conversions

-- fixme: maybe just toByronHash = ByronHash . CC.unsafeHashFromBytes
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
    -> W.EpochLength
    -> W.BlockHeader
    -> Point ByronBlock
toPoint genesisH epLength (W.BlockHeader sid _ h _)
  | h == (coerce genesisH) = O.GenesisPoint
  | otherwise = O.Point $ Point.block (toSlotNo epLength sid) (toByronHash h)

toSlotNo :: W.EpochLength -> W.SlotId -> SlotNo
toSlotNo epLength =
    SlotNo . W.flatSlot epLength

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

byronCodecConfig :: W.GenesisParameters -> CodecConfig ByronBlock
byronCodecConfig W.GenesisParameters{getEpochLength,getEpochStability} =
    ByronCodecConfig (toEpochSlots getEpochLength) (SecurityParam k)
  where
    k = fromIntegral . getQuantity $ getEpochStability

fromByronBlock :: W.GenesisParameters -> ByronBlock -> W.Block
fromByronBlock gp byronBlk = case byronBlockRaw byronBlk of
  ABOBBlock blk  ->
    mkBlock $ fromTxAux <$> unTxPayload (blockTxPayload blk)
  ABOBBoundary _ ->
    mkBlock []
  where
    W.GenesisParameters genesisHash _ _ epLength _ _ = gp
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
                fromChainHash genesisHash $ headerPrevHash (byronCodecConfig gp) (getHeader byronBlk)
            }
        , transactions = txs
        , delegations  = []
        }

fromTxAux :: TxAux -> W.Tx
fromTxAux txAux = case taTx txAux of
    tx@(UnsafeTx inputs outputs _attributes) -> W.Tx
        { txId = W.Hash $ CC.hashToBytes $ serializeCborHash tx

        -- TODO: Review 'W.Tx' to not require resolved inputs but only inputs
        , resolvedInputs =
            (, W.Coin 0) . fromTxIn <$> NE.toList inputs

        , outputs =
            fromTxOut <$> NE.toList outputs
        }

fromTxIn :: TxIn -> W.TxIn
fromTxIn (TxInUtxo id_ ix) = W.TxIn
    { inputId = W.Hash $ CC.hashToBytes id_
    , inputIx = ix
    }

fromTxOut :: TxOut -> W.TxOut
fromTxOut (TxOut addr coin) = W.TxOut
    { address = W.Address (serialize' addr)
    , coin = W.Coin (unsafeGetLovelace coin)
    }

fromByronHash :: ByronHash -> W.Hash "BlockHeader"
fromByronHash =
    W.Hash . CC.hashToBytes . unByronHash

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
fromTip genesisHash epLength tip = case getPoint (getTipPoint tip) of
    Origin -> W.BlockHeader
        { slotId = W.SlotId 0 0
        , blockHeight = Quantity 0
        , headerHash = coerce genesisHash
        , parentHeaderHash = hashOfNoParent
        }
    At blk -> W.BlockHeader
        { slotId = fromSlotNo epLength $ Point.blockPointSlot blk
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

fromTxFeePolicy :: TxFeePolicy -> W.FeePolicy
fromTxFeePolicy (TxFeePolicyTxSizeLinear (TxSizeLinear a b)) =
    W.LinearFee
        (Quantity (lovelaceToDouble a))
        (Quantity (rationalToDouble b))
        (Quantity 0) -- certificates do not exist for Byron
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

protocolParametersFromPP :: Update.ProtocolParameters -> W.ProtocolParameters
protocolParametersFromPP pp = W.ProtocolParameters
    { decentralizationLevel = minBound
    , txParameters = W.TxParameters
        { getFeePolicy = fromTxFeePolicy $ Update.ppTxFeePolicy pp
        , getTxMaxSize = fromMaxTxSize $ Update.ppMaxTxSize pp
        }
    , desiredNumberOfStakePools = 0
    , minimumUTxOvalue = W.Coin 0
    }

-- | Extract the protocol parameters relevant to the wallet out of the
--   cardano-chain update state record.
protocolParametersFromUpdateState :: Update.State -> W.ProtocolParameters
protocolParametersFromUpdateState =
    protocolParametersFromPP . Update.adoptedProtocolParameters

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
            , getSlotLength =
                fromSlotDuration . ppSlotDuration . gdProtocolParameters $ genesisData
            , getEpochLength =
                fromBlockCount . gdK $ genesisData
            , getEpochStability =
                Quantity . fromIntegral . unBlockCount . gdK $ genesisData
            , getActiveSlotCoefficient =
                W.ActiveSlotCoefficient 1.0
            }
        , protocolParameters =
            protocolParametersFromPP . gdProtocolParameters $ genesisData
        }
    , fromNonAvvmBalances . gdNonAvvmBalances $ genesisData
    )

fromNetworkMagic :: NetworkMagic -> W.ProtocolMagic
fromNetworkMagic (NetworkMagic magic) =
    W.ProtocolMagic (fromIntegral magic)

fromProtocolMagicId :: ProtocolMagicId -> W.ProtocolMagic
fromProtocolMagicId = W.ProtocolMagic . fromIntegral . unProtocolMagicId

{-------------------------------------------------------------------------------
                      Address Encoding / Decoding
-------------------------------------------------------------------------------}

instance EncodeAddress 'Mainnet where
    encodeAddress =
        gEncodeAddress

instance EncodeAddress ('Testnet pm) where
    encodeAddress =
        gEncodeAddress

gEncodeAddress :: W.Address -> Text
gEncodeAddress (W.Address bytes) =
    T.decodeUtf8 $ encodeBase58 bitcoinAlphabet bytes

instance DecodeAddress 'Mainnet where
    decodeAddress =
        gDecodeAddress (decodeLegacyAddress Nothing)

instance KnownNat pm => DecodeAddress ('Testnet pm) where
    decodeAddress =
        gDecodeAddress (decodeLegacyAddress $ Just $ W.testnetMagic @pm)

gDecodeAddress
    :: (ByteString -> Maybe W.Address)
    -> Text
    -> Either TextDecodingError W.Address
gDecodeAddress decodeByron text =
    case tryBase58 of
        Just bytes ->
            decodeByron bytes
                & maybeToEither (TextDecodingError
                    "Unable to decode Address: not a well-formed Byron Address.")

        Nothing ->
            Left $ TextDecodingError
                "Unable to decode Address: not a valid Base58 encoded string."
  where
    -- | Attempt decoding a legacy 'Address' using a Base58 encoding.
    tryBase58 :: Maybe ByteString
    tryBase58 =
        decodeBase58 bitcoinAlphabet (T.encodeUtf8 text)
