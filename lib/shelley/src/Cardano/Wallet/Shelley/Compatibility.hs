{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
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
-- Conversion functions and static chain settings for Shelley.

module Cardano.Wallet.Shelley.Compatibility
    ( Shelley
    , ShelleyBlock
    , Delegations
    , RewardAccounts

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
    , toCardanoTxId
    , toCardanoTxIn
    , toCardanoTxOut
    , toCardanoLovelace
    , toSealed
    , toStakeKeyRegCert
    , toStakeKeyDeregCert
    , toStakePoolDlgCert
    , toStakeCredential
    , toShelleyCoin
    , fromShelleyCoin

      -- ** Stake pools
    , fromPoolId
    , fromPoolDistr
    , fromNonMyopicMemberRewards
    , optimumNumberOfPools
    , getProducer

    , fromBlockNo
    , fromShelleyBlock
    , fromShelleyBlock'
    , toBlockHeader
    , fromShelleyHash
    , fromPrevHash
    , fromChainHash
    , fromGenesisData
    , fromNetworkMagic
    , fromSlotNo
    , fromTip
    , fromTip'
    , fromPParams
    , fromNetworkDiscriminant

      -- * Internal Conversions
    , decentralizationLevelFromPParams

      -- * Utilities
    , invertUnitInterval
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPub, xpubPublicKey )
import Cardano.Api.Shelley.Genesis
    ( ShelleyGenesis (..) )
import Cardano.Binary
    ( fromCBOR, serialize' )
import Cardano.Crypto.Hash.Class
    ( Hash (UnsafeHash), getHash )
import Cardano.Slotting.Slot
    ( EpochNo (..), EpochSize (..) )
import Cardano.Wallet.Api.Types
    ( DecodeAddress (..), EncodeAddress (..) )
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
import Control.Monad
    ( when )
import Crypto.Hash.Utils
    ( blake2b224 )
import Data.Bifunctor
    ( bimap )
import Data.ByteArray.Encoding
    ( Base (Base16), convertFromBase )
import Data.ByteString
    ( ByteString )
import Data.ByteString.Base58
    ( bitcoinAlphabet, decodeBase58, encodeBase58 )
import Data.Coerce
    ( coerce )
import Data.Foldable
    ( toList )
import Data.Map.Strict
    ( Map )
import Data.Maybe
    ( fromMaybe, isJust, mapMaybe )
import Data.Proxy
    ( Proxy )
import Data.Quantity
    ( Percentage, Quantity (..), mkPercentage )
import Data.Text
    ( Text )
import Data.Text.Class
    ( TextDecodingError (..) )
import Data.Type.Equality
    ( testEquality )
import Data.Word
    ( Word16, Word32, Word64 )
import Fmt
    ( Buildable (..), hexF )
import GHC.Stack
    ( HasCallStack )
import Numeric.Natural
    ( Natural )
import Ouroboros.Consensus.Shelley.Ledger
    ( GenTx, ShelleyHash (..) )
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
    ( ConnectionId (..)
    , LocalAddress (..)
    , NodeToClientVersionData (..)
    , nodeToClientCodecCBORTerm
    )
import Ouroboros.Network.Point
    ( WithOrigin (..) )
import Shelley.Spec.Ledger.BaseTypes
    ( strictMaybeToMaybe, urlToText )
import Type.Reflection
    ( Typeable, typeRep )

import qualified Cardano.Api as Cardano
import qualified Cardano.Byron.Codec.Cbor as CBOR
import qualified Cardano.Chain.Common as Byron
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Codec.Binary.Bech32 as Bech32
import qualified Codec.Binary.Bech32.TH as Bech32
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text.Encoding as T
import qualified Ouroboros.Consensus.Shelley.Ledger as O
import qualified Ouroboros.Network.Block as O
import qualified Ouroboros.Network.Point as Point
import qualified Shelley.Spec.Ledger.Address as SL
import qualified Shelley.Spec.Ledger.BaseTypes as SL
import qualified Shelley.Spec.Ledger.BlockChain as SL
import qualified Shelley.Spec.Ledger.Coin as SL
import qualified Shelley.Spec.Ledger.Credential as SL
import qualified Shelley.Spec.Ledger.Delegation.Certificates as SL
import qualified Shelley.Spec.Ledger.Genesis as SL
import qualified Shelley.Spec.Ledger.Keys as SL
import qualified Shelley.Spec.Ledger.LedgerState as SL
import qualified Shelley.Spec.Ledger.PParams as SL
import qualified Shelley.Spec.Ledger.Scripts as SL
import qualified Shelley.Spec.Ledger.Tx as SL
import qualified Shelley.Spec.Ledger.TxData as SL
import qualified Shelley.Spec.Ledger.UTxO as SL

data Shelley

type NodeVersionData =
    (NodeToClientVersionData, CodecCBORTerm Text NodeToClientVersionData)

-- | Concrete block type, using shelley crypto.
type ShelleyBlock = O.ShelleyBlock TPraosStandardCrypto

-- | Shorthand for shelley delegations. Maps staking credentials to stake pool
-- key hash.
type Delegations =
    Map Cardano.ShelleyCredentialStaking Cardano.ShelleyVerificationKeyHashStakePool

-- | Concrete type for a shelley reward account.
type RewardAccounts = SL.RewardAccounts TPraosStandardCrypto

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

toBlockHeader
    :: W.Hash "Genesis"
    -> W.EpochLength
    -> ShelleyBlock
    -> W.BlockHeader
toBlockHeader genesisHash epLength blk =
    let
        O.ShelleyBlock (SL.Block (SL.BHeader header _) _) headerHash = blk
    in
    W.BlockHeader
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

getProducer :: ShelleyBlock -> W.PoolId
getProducer blk =
    let
        O.ShelleyBlock (SL.Block (SL.BHeader header _) _) _ = blk
    in
        fromPoolKeyHash $ SL.hashKey (SL.bheaderVk header)

fromShelleyBlock
    :: W.Hash "Genesis"
    -> W.EpochLength
    -> ShelleyBlock
    -> W.Block
fromShelleyBlock genesisHash epLength blk =
    let
       O.ShelleyBlock (SL.Block _ txSeq) _ = blk
       SL.TxSeq txs' = txSeq
       (txs, certs, _) = unzip3 $ map fromShelleyTx $ toList txs'

    in W.Block
        { header = toBlockHeader genesisHash epLength blk
        , transactions = txs
        , delegations  = mconcat certs
        }

fromShelleyBlock'
    :: W.EpochLength
    -> ShelleyBlock
    -> (W.SlotId, [W.PoolCertificate])
fromShelleyBlock' epLength blk =
    let
        O.ShelleyBlock (SL.Block (SL.BHeader header _) txSeq) _ = blk
        SL.TxSeq txs' = txSeq
        (_, _, certs) = unzip3 $ map fromShelleyTx $ toList txs'
    in
        (fromSlotNo epLength $ SL.bheaderSlotNo header, mconcat certs)

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

fromTip' :: W.GenesisParameters -> Tip ShelleyBlock -> W.BlockHeader
fromTip' gp = fromTip getGenesisBlockHash getEpochLength
  where
    W.GenesisParameters
        { getEpochLength
        , getGenesisBlockHash
        } = gp

-- NOTE: Unsafe conversion from Natural -> Word16
fromMaxTxSize :: Natural -> Quantity "byte" Word16
fromMaxTxSize =
    Quantity . fromIntegral

fromPParams :: SL.PParams -> W.ProtocolParameters
fromPParams pp = W.ProtocolParameters
    { decentralizationLevel =
        decentralizationLevelFromPParams pp
    , txParameters =
        txParametersFromPParams pp
    , desiredNumberOfStakePools =
        desiredNumberOfStakePoolsFromPParams pp
    , minimumUTxOvalue =
        minimumUTxOvalueFromPParams pp
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
-- Therefore, we must invert the value provided by cardano-node before we
-- convert it into a percentage.
--
decentralizationLevelFromPParams
    :: SL.PParams
    -> W.DecentralizationLevel
decentralizationLevelFromPParams pp =
    W.DecentralizationLevel $ fromUnitInterval
        -- We must invert the value provided: (see function comment)
        $ invertUnitInterval d
  where
    d = SL._d pp

txParametersFromPParams
    :: SL.PParams
    -> W.TxParameters
txParametersFromPParams pp = W.TxParameters
    { getFeePolicy = W.LinearFee
        (Quantity (naturalToDouble (SL._minfeeB pp)))
        (Quantity (fromIntegral (SL._minfeeA pp)))
        (Quantity (fromIntegral (SL._keyDeposit pp)))
    , getTxMaxSize = fromMaxTxSize $ SL._maxTxSize pp
    }
  where
    naturalToDouble :: Natural -> Double
    naturalToDouble = fromIntegral

desiredNumberOfStakePoolsFromPParams
    :: SL.PParams
    -> Word16
desiredNumberOfStakePoolsFromPParams pp = fromIntegral (SL._nOpt pp)

minimumUTxOvalueFromPParams
    :: SL.PParams
    -> W.Coin
minimumUTxOvalueFromPParams pp = W.Coin . fromIntegral $ SL._minUTxOValue pp

-- | Convert genesis data into blockchain params and an initial set of UTxO
fromGenesisData
    :: ShelleyGenesis TPraosStandardCrypto
    -> [(SL.Addr TPraosStandardCrypto, SL.Coin)]
    -> (W.NetworkParameters, W.Block)
fromGenesisData g initialFunds =
    ( W.NetworkParameters
        { genesisParameters = W.GenesisParameters
            { getGenesisBlockHash = dummyGenesisHash
            , getGenesisBlockDate =
                W.StartTime . sgSystemStart $ g
            , getSlotLength =
                W.SlotLength $ sgSlotLength g
            , getEpochLength =
                W.EpochLength . fromIntegral . unEpochSize . sgEpochLength $ g
            , getEpochStability =
                Quantity . fromIntegral . sgSecurityParam $ g
            , getActiveSlotCoefficient =
                W.ActiveSlotCoefficient . fromRational . sgActiveSlotsCoeff $ g
            }
        , protocolParameters = fromPParams . sgProtocolParams $ g
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
                SL.initialFundsPseudoTxIn @TPraosStandardCrypto addr

fromNetworkMagic :: NetworkMagic -> W.ProtocolMagic
fromNetworkMagic (NetworkMagic magic) =
    W.ProtocolMagic (fromIntegral magic)

--
-- Stake pools
--

fromPoolId :: SL.KeyHash 'SL.StakePool crypto -> W.PoolId
fromPoolId (SL.KeyHash x) = W.PoolId $ getHash x

fromPoolDistr
    :: SL.PoolDistr TPraosStandardCrypto
    -> Map W.PoolId Percentage
fromPoolDistr =
    Map.map (unsafeMkPercentage . fst)
    . Map.mapKeys fromPoolId
    . SL.unPoolDistr

-- NOTE: This function disregards results that are using staking keys
fromNonMyopicMemberRewards
    :: O.NonMyopicMemberRewards TPraosStandardCrypto
    -> Map (Either W.Coin W.ChimericAccount) (Map W.PoolId (Quantity "lovelace" Word64))
fromNonMyopicMemberRewards =
    Map.map (Map.map (Quantity . fromIntegral) . Map.mapKeys fromPoolId)
    . Map.mapKeys (bimap fromShelleyCoin fromStakeCredential)
    . O.unNonMyopicMemberRewards

optimumNumberOfPools :: SL.PParams -> Int
optimumNumberOfPools = unsafeConvert . SL._nOpt
  where
    -- A value of ~100 can be expected, so should be fine.
    unsafeConvert :: Natural -> Int
    unsafeConvert = fromIntegral

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

toShelleyCoin :: W.Coin -> SL.Coin
toShelleyCoin (W.Coin c) = SL.Coin $ safeCast c
  where
    safeCast :: Word64 -> Integer
    safeCast = fromIntegral

-- NOTE: For resolved inputs we have to pass in a dummy value of 0.
fromShelleyTx
    :: SL.Tx TPraosStandardCrypto
    -> ( W.Tx
       , [W.DelegationCertificate]
       , [W.PoolCertificate]
       )
fromShelleyTx (SL.Tx bod@(SL.TxBody ins outs certs _ _ _ _ _) _ _) =
    ( W.Tx
        (fromShelleyTxId $ SL.txid bod)
        (map ((,W.Coin 0) . fromShelleyTxIn) (toList ins))
        (map fromShelleyTxOut (toList outs))
    , mapMaybe fromShelleyDelegationCert (toList certs)
    , mapMaybe fromShelleyRegistrationCert (toList certs)
    )

-- Convert & filter Shelley certificate into delegation certificate. Returns
-- 'Nothing' if certificates aren't delegation certificate.
fromShelleyDelegationCert
    :: SL.DCert TPraosStandardCrypto
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
    :: SL.DCert TPraosStandardCrypto
    -> Maybe (W.PoolCertificate)
fromShelleyRegistrationCert = \case
    SL.DCertPool (SL.RegPool pp) -> Just $ Registration
        ( W.PoolRegistrationCertificate
            { W.poolId = fromPoolKeyHash $ SL._poolPubKey pp
            , W.poolOwners = fromOwnerKeyHash <$> Set.toList (SL._poolOwners pp)
            , W.poolMargin = fromUnitInterval (SL._poolMargin pp)
            , W.poolCost = Quantity $ fromIntegral (SL._poolCost pp)
            , W.poolPledge = Quantity $ fromIntegral (SL._poolPledge pp)
            , W.poolMetadata = fromPoolMetaData <$> strictMaybeToMaybe (SL._poolMD pp)
            }
        )

    SL.DCertPool (SL.RetirePool pid (EpochNo e)) ->
        Just $ Retirement $ PoolRetirementCertificate (fromPoolKeyHash pid)
        (W.EpochNo $ fromIntegral e)

    SL.DCertDeleg{}   -> Nothing
    SL.DCertGenesis{} -> Nothing
    SL.DCertMir{}     -> Nothing

fromPoolMetaData :: SL.PoolMetaData -> (W.StakePoolMetadataUrl, W.StakePoolMetadataHash)
fromPoolMetaData meta =
    ( W.StakePoolMetadataUrl (urlToText (SL._poolMDUrl meta))
    , W.StakePoolMetadataHash (SL._poolMDHash meta)
    )

-- | Convert a stake credentials to a 'ChimericAccount' type. Unlike with
-- Jörmungandr, the Chimeric payload doesn't represent a public key but a HASH
-- of a public key.
fromStakeCredential :: Cardano.ShelleyCredentialStaking -> W.ChimericAccount
fromStakeCredential = \case
    SL.ScriptHashObj (SL.ScriptHash h) ->
        W.ChimericAccount (getHash h)
    SL.KeyHashObj (SL.KeyHash h) ->
        W.ChimericAccount (getHash h)

fromPoolKeyHash :: SL.KeyHash rol TPraosStandardCrypto -> W.PoolId
fromPoolKeyHash (SL.KeyHash h) =
    W.PoolId (getHash h)

fromOwnerKeyHash :: SL.KeyHash 'SL.Staking TPraosStandardCrypto -> W.PoolOwner
fromOwnerKeyHash (SL.KeyHash h) =
    W.PoolOwner (getHash h)

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

-- NOTE: Arguably breaks naming conventions. Perhaps fromCardanoSignedTx instead
toSealed :: SL.Tx TPraosStandardCrypto -> (W.Tx, W.SealedTx)
toSealed tx =
    let
        (wtx, _, _) = fromShelleyTx tx
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

-- | Convert from a chimeric account address (which is a hash of a public key)
-- to a shelley ledger stake credential.
toStakeCredential :: W.ChimericAccount -> Cardano.ShelleyCredentialStaking
toStakeCredential = Cardano.mkShelleyStakingCredential
    . SL.KeyHash . UnsafeHash . W.unChimericAccount

toStakeKeyDeregCert :: XPub -> Cardano.Certificate
toStakeKeyDeregCert xpub =
    Cardano.shelleyDeregisterStakingAddress
        (SL.KeyHash $ UnsafeHash $ blake2b224 $ xpubPublicKey xpub)

toStakeKeyRegCert :: XPub -> Cardano.Certificate
toStakeKeyRegCert xpub =
    Cardano.shelleyRegisterStakingAddress
        (SL.KeyHash $ UnsafeHash $ blake2b224 $ xpubPublicKey xpub)

toStakePoolDlgCert :: XPub -> W.PoolId -> Cardano.Certificate
toStakePoolDlgCert xpub (W.PoolId pid) =
    Cardano.shelleyDelegateStake
        (SL.KeyHash $ UnsafeHash $ blake2b224 $ xpubPublicKey xpub)
        (SL.KeyHash $ UnsafeHash pid)

{-------------------------------------------------------------------------------
                      Address Encoding / Decoding
-------------------------------------------------------------------------------}

instance EncodeAddress 'Mainnet where
    encodeAddress = _encodeAddress

instance EncodeAddress ('Testnet pm) where
    encodeAddress = _encodeAddress

_encodeAddress :: W.Address -> Text
_encodeAddress (W.Address bytes) =
    if isJust (CBOR.deserialiseCbor CBOR.decodeAddressPayload bytes)
        then base58
        else bech32
  where
    base58 = T.decodeUtf8 $ encodeBase58 bitcoinAlphabet bytes
    bech32 = Bech32.encodeLenient hrp (dataPartFromBytes bytes)
    hrp = [Bech32.humanReadablePart|addr|]

instance DecodeAddress 'Mainnet where
    decodeAddress = _decodeAddress SL.Mainnet

instance DecodeAddress ('Testnet pm) where
    decodeAddress = _decodeAddress SL.Testnet

-- Note that for 'Byron', we always assume no discrimination. In
-- practice, there is one discrimination for 'Shelley' addresses, and one for
-- 'Byron' addresses. Yet, on Mainnet, 'Byron' addresses have no explicit
-- discrimination.
_decodeAddress
    :: SL.Network
    -> Text
    -> Either TextDecodingError W.Address
_decodeAddress serverNetwork text =
    case tryBase16 <|> tryBech32 <|> tryBase58 of
        Just bytes ->
            decodeShelleyAddress bytes
        _ ->
            Left $ TextDecodingError
                "Unrecognized address encoding: must be either bech32, base58 or base16"
  where
    -- | Attempt decoding an 'Address' using a Bech32 encoding.
    tryBech32 :: Maybe ByteString
    tryBech32 = do
        (_, dp) <- either (const Nothing) Just (Bech32.decodeLenient text)
        dataPartToBytes dp

    -- | Attempt decoding a legacy 'Address' using a Base58 encoding.
    tryBase58 :: Maybe ByteString
    tryBase58 =
        decodeBase58 bitcoinAlphabet (T.encodeUtf8 text)

    -- | Attempt decoding an 'Address' using Base16 encoding
    tryBase16 :: Maybe ByteString
    tryBase16 =
        either (const Nothing) Just $ convertFromBase Base16 (T.encodeUtf8 text)

    decodeShelleyAddress :: ByteString -> Either TextDecodingError W.Address
    decodeShelleyAddress bytes = do
        case SL.deserialiseAddr @TPraosStandardCrypto bytes of
            Just (SL.Addr addrNetwork _ _) -> do
                guardNetwork addrNetwork
                pure (W.Address bytes)

            Just (SL.AddrBootstrap (SL.BootstrapAddress addr)) -> do
                guardNetwork (toNetwork (Byron.addrNetworkMagic addr))
                pure (W.Address bytes)

            Nothing -> Left $ TextDecodingError
                "Unable to decode address: not a well-formed Shelley nor Byron address."

      where
        guardNetwork :: SL.Network -> Either TextDecodingError ()
        guardNetwork addrNetwork =
            when (addrNetwork /= serverNetwork) $
                Left $ TextDecodingError $
                    "Invalid network discrimination on address. Expecting "
                    <> show serverNetwork
                    <> " but got "
                    <> show addrNetwork
                    <> "."

        toNetwork :: Byron.NetworkMagic -> SL.Network
        toNetwork = \case
            Byron.NetworkMainOrStage -> SL.Mainnet
            Byron.NetworkTestnet{}   -> SL.Testnet

{-------------------------------------------------------------------------------
                                    Logging
-------------------------------------------------------------------------------}

-- Compact representation of connection id for log messages.
instance Buildable addr => Buildable (ConnectionId addr) where
   build (ConnectionId a b) = "conn:" <> build a <> ":" <> build b

instance Buildable LocalAddress where
    build (LocalAddress p) = build p

instance Buildable W.ChimericAccount where
    build (W.ChimericAccount addr) = hexF addr

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
