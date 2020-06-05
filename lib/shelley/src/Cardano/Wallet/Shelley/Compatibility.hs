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

-- Orphan instances for {Encode,Decode}Address until we get rid of the
-- Jörmungandr dual support.
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Copyright: © 2020 IOHK
-- License: Apache-2.0
--
-- Conversion functions and static chain settings for Shelley.

module Cardano.Wallet.Shelley.Compatibility
    ( Shelley
    , ShelleyBlock

    , NodeVersionData
    , TPraosStandardCrypto

      -- * Chain Parameters
    , mainnetVersionData
    , testnetVersionData

      -- * Genesis
    , emptyGenesis
    , genesisTip

      -- * Conversions
    , toShelleyHash
    , toEpochSize
    , toGenTx
    , toPoint
    , toSlotNo

    , fromBlockNo
    , fromShelleyBlock
    , fromShelleyHash
    , fromPrevHash
    , fromChainHash
    , fromGenesisData
    , fromNetworkMagic
    , fromSlotNo
    , fromTip
    , fromPParams

    , toCardanoTxId
    , toCardanoTxIn
    , toCardanoTxOut
    , toCardanoLovelace
    , toSealed
    ) where

import Prelude

import Cardano.Binary
    ( fromCBOR, serialize' )
import Cardano.Config.Shelley.Genesis
    ( ShelleyGenesis (..) )
import Cardano.Crypto.Hash.Class
    ( Hash (UnsafeHash), getHash )
import Cardano.Slotting.Slot
    ( EpochSize (..) )
import Cardano.Wallet.Api.Types
    ( DecodeAddress (..), EncodeAddress (..) )
import Cardano.Wallet.Primitive.AddressDerivation
    ( NetworkDiscriminant (..), hex )
import Cardano.Wallet.Unsafe
    ( unsafeDeserialiseCbor )
import Control.Arrow
    ( left )
import Data.ByteArray.Encoding
    ( Base (Base16), convertFromBase )
import Data.ByteString
    ( ByteString )
import Data.Coerce
    ( coerce )
import Data.Foldable
    ( toList )
import Data.Maybe
    ( fromMaybe )
import Data.Quantity
    ( Quantity (..), mkPercentage )
import Data.Text
    ( Text )
import Data.Text.Class
    ( TextDecodingError (..) )
import Data.Word
    ( Word16, Word32, Word64 )
import GHC.Stack
    ( HasCallStack )
import Numeric.Natural
    ( Natural )
import Ouroboros.Consensus.BlockchainTime.WallClock.Types
    ( SlotLength (..), getSystemStart )
import Ouroboros.Consensus.Protocol.Abstract
    ( SecurityParam (..) )
import Ouroboros.Consensus.Shelley.Ledger
    ( GenTx, ShelleyHash (..) )
import Ouroboros.Consensus.Shelley.Node
    ( initialFundsPseudoTxIn )
import Ouroboros.Consensus.Shelley.Protocol.Crypto
    ( TPraosStandardCrypto )
import Ouroboros.Network.Block
    ( BlockNo (..)
    , ChainHash
    , Point (..)
    , SlotNo (..)
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
    ( NodeToClientVersionData (..), nodeToClientCodecCBORTerm )
import Ouroboros.Network.Point
    ( WithOrigin (..) )

import qualified Cardano.Api as Cardano
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import qualified Data.Text.Encoding as T
import qualified Ouroboros.Consensus.Shelley.Ledger as O
import qualified Ouroboros.Network.Block as O
import qualified Ouroboros.Network.Point as Point
import qualified Shelley.Spec.Ledger.Address as SL
import qualified Shelley.Spec.Ledger.BaseTypes as SL
import qualified Shelley.Spec.Ledger.BlockChain as SL
import qualified Shelley.Spec.Ledger.Coin as SL
import qualified Shelley.Spec.Ledger.PParams as SL
import qualified Shelley.Spec.Ledger.Tx as SL
import qualified Shelley.Spec.Ledger.UTxO as SL

data Shelley

type NodeVersionData =
    (NodeToClientVersionData, CodecCBORTerm Text NodeToClientVersionData)

type ShelleyBlock = O.ShelleyBlock TPraosStandardCrypto

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
        { slotId =
            W.SlotId 0 0
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


genesisTip :: Tip (O.ShelleyBlock TPraosStandardCrypto)
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

-- | Magic value for the absence of a block.
hashOfNoParent :: W.Hash "BlockHeader"
hashOfNoParent =
    W.Hash . BS.pack $ replicate 32 0

-- fixme: maybe just toShelleyHash = ShelleyHash . CC.unsafeHashFromBytes
toShelleyHash :: W.Hash "BlockHeader" -> ShelleyHash c
toShelleyHash (W.Hash bytes) =
    ShelleyHash $ SL.HashHeader $ UnsafeHash bytes

toEpochSize :: W.EpochLength -> EpochSize
toEpochSize =
    EpochSize . fromIntegral . W.unEpochLength

toPoint
    :: W.Hash "Genesis"
    -> W.EpochLength
    -> W.BlockHeader
    -> Point ShelleyBlock
toPoint genesisH epLength (W.BlockHeader sid _ h _)
  | h == (coerce genesisH) = O.GenesisPoint
  | otherwise = O.Point $ Point.block (toSlotNo epLength sid) (toShelleyHash h)

toSlotNo :: W.EpochLength -> W.SlotId -> SlotNo
toSlotNo epLength =
    SlotNo . W.flatSlot epLength

fromShelleyBlock
    :: W.Hash "Genesis"
    -> W.EpochLength
    -> ShelleyBlock
    -> W.Block
fromShelleyBlock genesisHash epLength blk =
    let
       O.ShelleyBlock (SL.Block (SL.BHeader header _) txSeq) headerHash = blk
       SL.TxSeq txs' = txSeq
       txs = map fromShelleyTx $ toList txs'

    in W.Block
        { header = W.BlockHeader
            { slotId =
                fromSlotNo epLength $ SL.bheaderSlotNo header
            , blockHeight =
                fromBlockNo $ SL.bheaderBlockNo header
            , headerHash =
                fromShelleyHash headerHash
            , parentHeaderHash =
                fromPrevHash (coerce genesisHash) $
                    SL.bheaderPrev header
            }
        , transactions = txs
        , delegations  = []
        }

fromShelleyHash :: ShelleyHash c -> W.Hash "BlockHeader"
fromShelleyHash (ShelleyHash (SL.HashHeader h)) = W.Hash (getHash h)

fromPrevHash
    :: W.Hash "BlockHeader"
    -> SL.PrevHash TPraosStandardCrypto
    -> W.Hash "BlockHeader"
fromPrevHash genesisHash = \case
    SL.GenesisHash -> genesisHash
    SL.BlockHash h -> fromShelleyHash (ShelleyHash h)

fromChainHash
    :: W.Hash "Genesis"
    -> ChainHash ShelleyBlock
    -> W.Hash "BlockHeader"
fromChainHash genesisHash = \case
    O.GenesisHash -> coerce genesisHash
    O.BlockHash h -> fromShelleyHash h

fromSlotNo :: W.EpochLength -> SlotNo -> W.SlotId
fromSlotNo epLength (SlotNo sl) =
    W.fromFlatSlot epLength sl

-- FIXME unsafe conversion (Word64 -> Word32)
fromBlockNo :: BlockNo -> Quantity "block" Word32
fromBlockNo (BlockNo h) =
    Quantity (fromIntegral h)

fromTip
    :: W.Hash "Genesis"
    -> W.EpochLength
    -> Tip ShelleyBlock
    -> W.BlockHeader
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
        , headerHash = fromShelleyHash $ Point.blockPointHash blk
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

fromSlotLength :: SlotLength -> W.SlotLength
fromSlotLength = W.SlotLength
    . getSlotLength

-- NOTE: Unsafe conversion from Natural -> Word16
fromMaxTxSize :: Natural -> Quantity "byte" Word16
fromMaxTxSize =
    Quantity . fromIntegral

fromPParams :: HasCallStack => SL.PParams -> W.ProtocolParameters
fromPParams pp = W.ProtocolParameters
    { decentralizationLevel =
        decentralizationLevelFromPParams pp
    , txParameters =
        txParametersFromPParams pp
    }

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
decentralizationLevelFromPParams
    :: HasCallStack
    => SL.PParams
    -> W.DecentralizationLevel
decentralizationLevelFromPParams pp =
    either reportInvalidValue W.DecentralizationLevel
        $ mkPercentage
        $ (1 -)
        $ SL.intervalValue d
  where
    d = SL._d pp
    reportInvalidValue = error $ mconcat
        [ "decentralizationLevelFromPParams: "
        , "encountered invalid decentralization parameter value: "
        , show d
        ]

txParametersFromPParams
    :: SL.PParams
    -> W.TxParameters
txParametersFromPParams pp = W.TxParameters
    { getFeePolicy = W.LinearFee
        (Quantity (naturalToDouble (SL._minfeeB pp)))
        (Quantity (fromIntegral (SL._minfeeA pp)))
        (Quantity 0) -- TODO: it's not as simple as this?
    , getTxMaxSize = fromMaxTxSize $ SL._maxTxSize pp
    }
  where
    naturalToDouble :: Natural -> Double
    naturalToDouble = fromIntegral

-- | Convert genesis data into blockchain params and an initial set of UTxO
fromGenesisData
    :: HasCallStack
    => ShelleyGenesis TPraosStandardCrypto
    -> (W.NetworkParameters, W.Block)
fromGenesisData g =
    ( W.NetworkParameters
        { genesisParameters = W.GenesisParameters
            { getGenesisBlockHash = dummyGenesisHash
            , getGenesisBlockDate =
                W.StartTime . getSystemStart . sgSystemStart $ g
            , getSlotLength =
                fromSlotLength . sgSlotLength $ g
            , getEpochLength =
                W.EpochLength . fromIntegral . unEpochSize . sgEpochLength $ g
            , getEpochStability =
                Quantity . fromIntegral . maxRollbacks . sgSecurityParam $ g
            , getActiveSlotCoefficient =
                W.ActiveSlotCoefficient 1.0
            }
        , protocolParameters = fromPParams . sgProtocolParams $ g
        }
    , genesisBlockFromTxOuts $ Map.toList $ sgInitialFunds g
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
    genesisBlockFromTxOuts
        :: [(SL.Addr TPraosStandardCrypto, SL.Coin)] -> W.Block
    genesisBlockFromTxOuts outs = W.Block
        { delegations  = []
        , header = W.BlockHeader
            { slotId =
                W.SlotId 0 0
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
          where
            W.TxIn pseudoHash _ = fromShelleyTxIn $
                initialFundsPseudoTxIn @TPraosStandardCrypto addr

fromNetworkMagic :: NetworkMagic -> W.ProtocolMagic
fromNetworkMagic (NetworkMagic magic) =
    W.ProtocolMagic (fromIntegral magic)

--
-- Txs
--

-- | SealedTx are the result of rightfully constructed shelley transactions so, it
-- is relatively safe to unserialize them from CBOR.
toGenTx :: HasCallStack => W.SealedTx -> GenTx ShelleyBlock
toGenTx = unsafeDeserialiseCbor fromCBOR
    . BL.fromStrict
    . W.getSealedTx

fromShelleyTxId :: SL.TxId crypto -> W.Hash "Tx"
fromShelleyTxId (SL.TxId (UnsafeHash h)) = W.Hash h

fromShelleyTxIn :: SL.TxIn crypto -> W.TxIn
fromShelleyTxIn (SL.TxIn txid ix) =
    W.TxIn (fromShelleyTxId txid) (unsafeCast ix)
  where
    unsafeCast :: Natural -> Word32
    unsafeCast = fromIntegral

fromShelleyTxOut :: SL.TxOut crypto -> W.TxOut
fromShelleyTxOut (SL.TxOut addr amount) =
  W.TxOut (fromShelleyAddress addr) (fromShelleyCoin amount)

fromShelleyAddress :: SL.Addr crypto -> W.Address
fromShelleyAddress = W.Address
    . SL.serialiseAddr

fromShelleyCoin :: SL.Coin -> W.Coin
fromShelleyCoin (SL.Coin c) = W.Coin $ unsafeCast c
  where
    -- (but probably safe)
    unsafeCast :: Integer -> Word64
    unsafeCast = fromIntegral

-- NOTE: For resolved inputs we have to pass in a dummy value of 0.
fromShelleyTx :: SL.Tx TPraosStandardCrypto -> W.Tx
fromShelleyTx (SL.Tx bod@(SL.TxBody ins outs _ _ _ _ _ _) _ _ _) = W.Tx
    (fromShelleyTxId $ SL.txid bod)
    (map ((,W.Coin 0) . fromShelleyTxIn) (toList ins))
    (map fromShelleyTxOut (toList outs))

-- NOTE: Arguably breaks naming conventions. Perhaps fromCardanoSignedTx instead
toSealed :: SL.Tx TPraosStandardCrypto -> (W.Tx, W.SealedTx)
toSealed tx =
    let
        wtx = fromShelleyTx tx
        sealed = W.SealedTx $ serialize' $ O.mkShelleyTx tx
    in (wtx, sealed)

toCardanoTxId :: W.Hash "Tx" -> Cardano.TxId
toCardanoTxId (W.Hash h) = Cardano.TxId $ UnsafeHash h

toCardanoTxIn :: W.TxIn -> Cardano.TxIn
toCardanoTxIn (W.TxIn tid ix) =
    Cardano.TxIn (toCardanoTxId tid) (fromIntegral ix)

-- NOTE: Only creates Shelley addresses.
toCardanoAddress :: W.Address -> Cardano.Address
toCardanoAddress (W.Address bytes) =
    Cardano.AddressShelley
        . fromMaybe (error "toCardanoAddress: invalid address")
        . SL.deserialiseAddr @TPraosStandardCrypto
        $ bytes

toCardanoLovelace :: W.Coin -> Cardano.Lovelace
toCardanoLovelace (W.Coin c) = Cardano.Lovelace $ safeCast c
  where
    safeCast :: Word64 -> Integer
    safeCast = fromIntegral

toCardanoTxOut :: W.TxOut -> Cardano.TxOut
toCardanoTxOut (W.TxOut addr coin) =
    Cardano.TxOut (toCardanoAddress addr) (toCardanoLovelace coin)

{-------------------------------------------------------------------------------
                      Address Encoding / Decoding
-------------------------------------------------------------------------------}

instance EncodeAddress 'Mainnet where
    encodeAddress = T.decodeUtf8 . hex . W.unAddress

instance EncodeAddress ('Testnet pm) where
    encodeAddress = T.decodeUtf8 . hex . W.unAddress

_decodeAddress :: Text -> Either TextDecodingError W.Address
_decodeAddress x = validateWithLedger =<< W.Address <$> fromHex x
  where
    fromHex :: Text -> Either TextDecodingError ByteString
    fromHex =
        left (const $ TextDecodingError "Unable to decode Address: not valid hex encoding.")
        .  convertFromBase @ByteString @ByteString Base16
        . T.encodeUtf8

    validateWithLedger addr@(W.Address bytes) =
        case SL.deserialiseAddr @TPraosStandardCrypto bytes of
            Just _ -> Right addr
            Nothing -> Left $ TextDecodingError
                "Unable to decode Address: not a well-formed Shelley Address."

instance DecodeAddress 'Mainnet where
    decodeAddress = _decodeAddress

instance DecodeAddress ('Testnet pm) where
    decodeAddress = _decodeAddress
