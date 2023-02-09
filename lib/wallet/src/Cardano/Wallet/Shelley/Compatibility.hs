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
{-# LANGUAGE RankNTypes #-}
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
    , fromAllegraPParams
    , fromMaryPParams
    , fromAlonzoPParams
    , fromBabbagePParams
    , fromLedgerExUnits
    , toLedgerExUnits
    , fromCardanoAddress
    , toSystemStart
    , toScriptPurpose
    , fromShelleyTxIn
    , toCostModelsAsArray
    , toCardanoPolicyId
    , toCardanoSimpleScript
    , toCardanoSimpleScriptV1

      -- * Address encoding
    , shelleyEncodeAddress
    , shelleyEncodeStakeAddress
    , shelleyDecodeAddress
    , shelleyDecodeStakeAddress

      -- * Unsafe conversions
    , unsafeLovelaceToWalletCoin
    , unsafeValueToLovelace

      -- ** Assessing sizes of token bundles
    , tokenBundleSizeAssessor
    , computeTokenBundleSerializedLengthBytes

      -- ** Stake pools
    , fromPoolId
    , fromPoolDistr
    , fromNonMyopicMemberRewards
    , optimumNumberOfPools
    , getProducer
    , fromBlockNo
    , fromCardanoBlock
    , toCardanoEra
    , toCardanoBlockHeader
    , toShelleyBlockHeader
    , toBabbageBlockHeader
    , fromShelleyHash
    , fromShelleyTxOut
    , fromCardanoHash
    , fromChainHash
    , fromPrevHash
    , fromGenesisData
    , fromTip
    , fromTip'
    , toTip
    , fromShelleyBlock
    , fromAllegraBlock
    , slottingParametersFromGenesis
    , fromMaryBlock
    , fromAlonzoBlock
    , fromBabbageBlock
    , getBabbageProducer

      -- * Internal Conversions
    , decentralizationLevelFromPParams

      -- * Utilities
    , inspectAddress
    , invertUnitInterval
    , interval0
    , interval1
    , getScriptIntegrityHash
    , numberOfTransactionsInBlock
    ) where

import Prelude

import Cardano.Address
    ( unsafeMkAddress )
import Cardano.Address.Derivation
    ( XPub, xpubPublicKey )
import Cardano.Address.Script
    ( KeyHash (..), Script (..) )
import Cardano.Api
    ( AllegraEra
    , AlonzoEra
    , AnyCardanoEra (..)
    , AsType (..)
    , BabbageEra
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
    )
import Cardano.Binary
    ( serialize' )
import Cardano.Chain.Block
    ( ABlockOrBoundary (ABOBBlock, ABOBBoundary), blockTxPayload )
import Cardano.Chain.UTxO
    ( unTxPayload )
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
import Cardano.Pool.Metadata.Types
    ( StakePoolMetadataHash (..), StakePoolMetadataUrl (..) )
import Cardano.Pool.Types
    ( PoolId (..), PoolOwner (..) )
import Cardano.Slotting.Slot
    ( EpochNo (..), EpochSize (..) )
import Cardano.Slotting.Time
    ( SystemStart (..) )
import Cardano.Wallet.Byron.Compatibility
    ( fromByronBlock, fromTxAux, maryTokenBundleMaxSize, toByronBlockHeader )
import Cardano.Wallet.Primitive.AddressDerivation
    ( NetworkDiscriminant (..) )
import Cardano.Wallet.Primitive.Types
    ( ChainPoint (..)
    , PoolCertificate
    , PoolRegistrationCertificate (..)
    , ProtocolParameters (txParameters)
    , TokenBundleMaxSize (..)
    , TxParameters (getTokenBundleMaxSize)
    )
import Cardano.Wallet.Primitive.Types.MinimumUTxO
    ( minimumUTxOForShelleyBasedEra )
import Cardano.Wallet.Primitive.Types.Tx.Constraints
    ( TokenBundleSizeAssessment (..), TokenBundleSizeAssessor (..) )
import Cardano.Wallet.Read.Primitive.Tx.Allegra
    ( fromAllegraTx )
import Cardano.Wallet.Read.Primitive.Tx.Alonzo
    ( fromAlonzoTx )
import Cardano.Wallet.Read.Primitive.Tx.Babbage
    ( fromBabbageTx )
import Cardano.Wallet.Read.Primitive.Tx.Features.Fee
    ( fromShelleyCoin )
import Cardano.Wallet.Read.Primitive.Tx.Features.Inputs
    ( fromShelleyTxIn )
import Cardano.Wallet.Read.Primitive.Tx.Features.Outputs
    ( fromCardanoValue, fromShelleyAddress, fromShelleyTxOut )
import Cardano.Wallet.Read.Primitive.Tx.Mary
    ( fromMaryTx )
import Cardano.Wallet.Read.Primitive.Tx.Shelley
import Cardano.Wallet.Read.Tx.Hash
    ( fromShelleyTxId )
import Cardano.Wallet.Shelley.Network.Discriminant
    ( DecodeAddress (..)
    , DecodeStakeAddress (..)
    , EncodeAddress (..)
    , EncodeStakeAddress (..)
    )
import Cardano.Wallet.Transaction
    ( WitnessCountCtx (..) )
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
import Data.IntCast
    ( intCast, intCastMaybe )
import Data.List
    ( unzip6 )
import Data.Map.Strict
    ( Map )
import Data.Maybe
    ( fromMaybe, isJust, mapMaybe )
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
import Numeric.Natural
    ( Natural )
import Ouroboros.Consensus.Byron.Ledger
    ( byronBlockRaw )
import Ouroboros.Consensus.Cardano.Block
    ( CardanoBlock
    , CardanoEras
    , HardForkBlock (..)
    , StandardAllegra
    , StandardAlonzo
    , StandardBabbage
    , StandardMary
    , StandardShelley
    )
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras
    ( OneEraHash (..) )
import Ouroboros.Consensus.HardFork.History.Summary
    ( Bound (..) )
import Ouroboros.Consensus.Shelley.Eras
    ( StandardCrypto )
import Ouroboros.Consensus.Shelley.Ledger
    ( ShelleyCompatible, ShelleyHash (..) )
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
import qualified Cardano.Byron.Codec.Cbor as CBOR
import qualified Cardano.Chain.Common as Byron
import qualified Cardano.Crypto.Hash as Crypto
import qualified Cardano.Ledger.Address as SL
import qualified Cardano.Ledger.Allegra as Allegra
import qualified Cardano.Ledger.Alonzo as Alonzo
import qualified Cardano.Ledger.Alonzo.Language as Alonzo
import qualified Cardano.Ledger.Alonzo.PParams as Alonzo
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import qualified Cardano.Ledger.Alonzo.TxSeq as Alonzo
import qualified Cardano.Ledger.Babbage as Babbage
import qualified Cardano.Ledger.Babbage.PParams as Babbage
import qualified Cardano.Ledger.Babbage.Tx as Babbage hiding
    ( ScriptIntegrityHash, TxBody )
import qualified Cardano.Ledger.Babbage.TxBody as Babbage
import qualified Cardano.Ledger.BaseTypes as SL
import qualified Cardano.Ledger.Credential as SL
import qualified Cardano.Ledger.Crypto as SL
import qualified Cardano.Ledger.Era as Ledger.Era
import qualified Cardano.Ledger.Mary as Mary
import qualified Cardano.Ledger.Mary.Value as SL
import qualified Cardano.Ledger.SafeHash as SafeHash
import qualified Cardano.Ledger.Shelley as SL hiding
    ( Value )
import qualified Cardano.Ledger.Shelley as Shelley
import qualified Cardano.Ledger.Shelley.API as SL
import qualified Cardano.Ledger.Shelley.API as SLAPI
import qualified Cardano.Ledger.Shelley.BlockChain as SL
import qualified Cardano.Ledger.ShelleyMA as MA
import qualified Cardano.Protocol.TPraos.BHeader as SL
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Cardano.Wallet.Primitive.Types.Address as W
import qualified Cardano.Wallet.Primitive.Types.Coin as Coin
import qualified Cardano.Wallet.Primitive.Types.Coin as W
import qualified Cardano.Wallet.Primitive.Types.Hash as W
import qualified Cardano.Wallet.Primitive.Types.Redeemer as W
import qualified Cardano.Wallet.Primitive.Types.RewardAccount as W
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.TokenPolicy as W
import qualified Cardano.Wallet.Primitive.Types.TokenQuantity as W
import qualified Cardano.Wallet.Primitive.Types.Tx.Constraints as W
import qualified Cardano.Wallet.Primitive.Types.Tx.SealedTx as W
    ( SealedTx, cardanoTxIdeallyNoLaterThan )
import qualified Cardano.Wallet.Primitive.Types.Tx.Tx as W
    ( Tx (..) )
import qualified Cardano.Wallet.Primitive.Types.Tx.TxIn as W
    ( TxIn (TxIn) )
import qualified Cardano.Wallet.Primitive.Types.Tx.TxOut as W
    ( TxOut (TxOut) )
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
import qualified Data.Set as Set
import qualified Data.Text.Encoding as T
import qualified Ouroboros.Consensus.Protocol.Praos as Consensus
import qualified Ouroboros.Consensus.Protocol.Praos.Header as Consensus
import qualified Ouroboros.Consensus.Protocol.TPraos as Consensus
import qualified Ouroboros.Consensus.Shelley.Ledger as O
import qualified Ouroboros.Consensus.Shelley.Protocol.Abstract as Consensus
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
nodeToClientVersions = [NodeToClientV_13]

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
    :: W.GenesisParameters
    -> CardanoBlock StandardCrypto
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
    BlockBabbage blk ->
        toBabbageBlockHeader (W.getGenesisBlockHash gp) blk

toShelleyBlockHeader
    :: (ShelleyCompatible (Consensus.TPraos StandardCrypto) era)
    => W.Hash "Genesis"
    -> ShelleyBlock (Consensus.TPraos StandardCrypto) era
    -> W.BlockHeader
toShelleyBlockHeader genesisHash blk =
    let
        ShelleyBlock (SL.Block header _txSeq) _headerHash = blk
    in
        W.BlockHeader
            { slotNo =
                Consensus.pHeaderSlot header
            , blockHeight =
                fromBlockNo $ Consensus.pHeaderBlock header
            , headerHash =
                fromShelleyHash $ Consensus.pHeaderHash header
            , parentHeaderHash = Just $
                fromPrevHash (coerce genesisHash) $
                    Consensus.pHeaderPrevHash header
            }

toBabbageBlockHeader
    :: (ShelleyCompatible (Consensus.Praos StandardCrypto) era)
    => W.Hash "Genesis"
    -> ShelleyBlock (Consensus.Praos StandardCrypto) era
    -> W.BlockHeader
toBabbageBlockHeader genesisHash blk =
    let
        ShelleyBlock (SL.Block header _txSeq) _headerHash = blk
    in
        W.BlockHeader
            { slotNo =
                Consensus.pHeaderSlot header
            , blockHeight =
                fromBlockNo $ Consensus.pHeaderBlock header
            , headerHash =
                fromShelleyHash $ Consensus.pHeaderHash header
            , parentHeaderHash = Just $
                fromPrevHash (coerce genesisHash) $
                    Consensus.pHeaderPrevHash header
            }

getProducer
    :: (Era era, ToCBORGroup (Ledger.Era.TxSeq era))
    => ShelleyBlock (Consensus.TPraos StandardCrypto) era -> PoolId
getProducer (ShelleyBlock (SL.Block (SL.BHeader header _) _) _) =
    fromPoolKeyHash $ SL.hashKey (SL.bheaderVk header)

getBabbageProducer
    :: (Era era, ToCBORGroup (Ledger.Era.TxSeq era))
    => ShelleyBlock (Consensus.Praos StandardCrypto) era -> PoolId
getBabbageProducer (ShelleyBlock (SL.Block (Consensus.Header header _) _) _) =
    fromPoolKeyHash $ SL.hashKey (Consensus.hbVk header)

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
    BlockBabbage blk ->
        fst $ fromBabbageBlock gp blk

numberOfTransactionsInBlock
    :: CardanoBlock StandardCrypto -> (Int, (Quantity "block" Word32, O.SlotNo))
numberOfTransactionsInBlock = \case
    BlockByron byb -> transactionsByron byb
    BlockShelley shb -> transactions shb
    BlockAllegra shb -> transactions shb
    BlockMary shb -> transactions shb
    BlockAlonzo shb -> transactionsAlonzo shb
    BlockBabbage shb -> transactionsBabbage shb
  where
    transactions
        (ShelleyBlock (SL.Block (SL.BHeader header _) (SL.TxSeq txs')) _) =
            ( length txs'
            , (fromBlockNo $ SL.bheaderBlockNo header, SL.bheaderSlotNo header)
            )
    transactionsAlonzo
        (ShelleyBlock (SL.Block (SL.BHeader header _) (Alonzo.TxSeq txs')) _) =
            ( length txs'
            , (fromBlockNo $ SL.bheaderBlockNo header, SL.bheaderSlotNo header)
            )
    transactionsBabbage
        :: ShelleyBlock
            (Consensus.Praos StandardCrypto)
            (Babbage.BabbageEra StandardCrypto)
        -> (Int, (Quantity "block" Word32, O.SlotNo))
    transactionsBabbage
        (ShelleyBlock
            (SL.Block (Consensus.Header header _)
            (Alonzo.TxSeq txs')) _) =
                ( length txs'
                , ( fromBlockNo $ Consensus.hbBlockNo header
                  , Consensus.hbSlotNo header
                  )
                )
    transactionsByron blk =
        (, (fromBlockNo $ O.blockNo blk, O.blockSlot blk)) $
            case byronBlockRaw blk of
            ABOBBlock blk' ->
                length $ fromTxAux <$> unTxPayload (blockTxPayload blk')
            ABOBBoundary _ ->
                0

toCardanoEra :: CardanoBlock c -> AnyCardanoEra
toCardanoEra = \case
    BlockByron{}   -> AnyCardanoEra ByronEra
    BlockShelley{} -> AnyCardanoEra ShelleyEra
    BlockAllegra{} -> AnyCardanoEra AllegraEra
    BlockMary{}    -> AnyCardanoEra MaryEra
    BlockAlonzo{}  -> AnyCardanoEra AlonzoEra
    BlockBabbage{} -> AnyCardanoEra BabbageEra

fromShelleyBlock
    :: W.GenesisParameters
    -> ShelleyBlock
        (Consensus.TPraos StandardCrypto)
        (SL.ShelleyEra StandardCrypto)
    -> (W.Block, [PoolCertificate])
fromShelleyBlock gp blk@(ShelleyBlock (SL.Block _ (SL.TxSeq txs')) _) =
    let
       (txs, certs, _, _, _, _) = unzip6 $ map fromShelleyTx $ toList txs'
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
    -> ShelleyBlock
        (Consensus.TPraos StandardCrypto)
        (MA.ShelleyMAEra 'MA.Allegra StandardCrypto)
    -> (W.Block, [PoolCertificate])
fromAllegraBlock gp blk@(ShelleyBlock (SL.Block _ (SL.TxSeq txs')) _) =
    let
       (txs, certs, _, _, _, _) = unzip6 $ map fromAllegraTx $ toList txs'
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
    -> ShelleyBlock
        (Consensus.TPraos StandardCrypto)
        (MA.ShelleyMAEra 'MA.Mary StandardCrypto)
    -> (W.Block, [PoolCertificate])
fromMaryBlock gp blk@(ShelleyBlock (SL.Block _ (SL.TxSeq txs')) _) =
    let
       (txs, certs, _, _, _, _) = unzip6 $
           map (`fromMaryTx` AnyWitnessCountCtx) $ toList txs'
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
    :: ShelleyCompatible
        (Consensus.TPraos StandardCrypto)
        (Alonzo.AlonzoEra StandardCrypto)
    => W.GenesisParameters
    -> ShelleyBlock
        (Consensus.TPraos StandardCrypto)
        (Alonzo.AlonzoEra StandardCrypto)
    -> (W.Block, [PoolCertificate])
fromAlonzoBlock gp blk@(ShelleyBlock (SL.Block _ txSeq) _) =
    let
        Alonzo.TxSeq txs' = txSeq
        (txs, certs, _, _, _, _) = unzip6 $
            map (`fromAlonzoTx` AnyWitnessCountCtx) $ toList txs'
        certs' = mconcat certs
    in
        ( W.Block
            { header = toShelleyBlockHeader (W.getGenesisBlockHash gp) blk
            , transactions = txs
            , delegations  = toDelegationCertificates certs'
            }
        , toPoolCertificates certs'
        )

fromBabbageBlock
    :: W.GenesisParameters
    -> ShelleyBlock
        (Consensus.Praos StandardCrypto)
        (Babbage.BabbageEra StandardCrypto)
    -> (W.Block, [PoolCertificate])
fromBabbageBlock gp blk@(ShelleyBlock (SL.Block _ txSeq) _) =
    let
        Alonzo.TxSeq txs' = txSeq
        (txs, certs, _, _, _, _) = unzip6 $
            map (`fromBabbageTx` AnyWitnessCountCtx) $ toList txs'
        certs' = mconcat certs
    in
        ( W.Block
            { header = toBabbageBlockHeader (W.getGenesisBlockHash gp) blk
            , transactions = txs
            , delegations  = toDelegationCertificates certs'
            }
        , toPoolCertificates certs'
        )

fromShelleyHash :: ShelleyHash crypto -> W.Hash "BlockHeader"
fromShelleyHash (ShelleyHash h) = W.Hash (hashToBytes h)

fromCardanoHash :: O.HeaderHash (CardanoBlock sc) -> W.Hash "BlockHeader"
fromCardanoHash = W.Hash . fromShort . getOneEraHash

fromPrevHash
    :: W.Hash "BlockHeader"
    -> SL.PrevHash crypto
    -> W.Hash "BlockHeader"
fromPrevHash genesisHash = \case
    SL.GenesisHash -> genesisHash
    SL.BlockHash (SL.HashHeader h) -> W.Hash (hashToBytes h)

fromChainHash
    :: W.Hash "Genesis"
    -> ChainHash (CardanoBlock sc)
    -> W.Hash "BlockHeader"
fromChainHash genesisHash = \case
    O.GenesisHash -> coerce genesisHash
    O.BlockHash (OneEraHash h) -> W.Hash $ fromShort h

-- FIXME unsafe conversion (Word64 -> Word32)
fromBlockNo :: BlockNo -> Quantity "block" Word32
fromBlockNo (BlockNo h) = Quantity (fromIntegral h)

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
fromMaxSize = Quantity . fromIntegral

fromShelleyPParams
    :: W.EraInfo Bound
    -> Maybe Cardano.ProtocolParameters
    -> Shelley.PParams StandardShelley
    -> W.ProtocolParameters
fromShelleyPParams eraInfo currentNodeProtocolParameters pp =
    W.ProtocolParameters
        { decentralizationLevel =
            decentralizationLevelFromPParams pp
        , txParameters =
            txParametersFromPParams
                maryTokenBundleMaxSize (W.ExecutionUnits 0 0) pp
        , desiredNumberOfStakePools =
            desiredNumberOfStakePoolsFromPParams pp
        , minimumUTxO =
            minimumUTxOForShelleyBasedEra ShelleyBasedEraShelley pp
        , stakeKeyDeposit = stakeKeyDepositFromPParams pp
        , eras = fromBoundToEpochNo <$> eraInfo
        -- Collateral inputs were not supported or required in Shelley:
        , maximumCollateralInputCount = 0
        , minimumCollateralPercentage = 0
        , executionUnitPrices = Nothing
        , currentNodeProtocolParameters
        }

fromAllegraPParams
    :: W.EraInfo Bound
    -> Maybe Cardano.ProtocolParameters
    -> Allegra.PParams StandardAllegra
    -> W.ProtocolParameters
fromAllegraPParams eraInfo currentNodeProtocolParameters pp =
    W.ProtocolParameters
        { decentralizationLevel =
            decentralizationLevelFromPParams pp
        , txParameters =
            txParametersFromPParams
                maryTokenBundleMaxSize (W.ExecutionUnits 0 0) pp
        , desiredNumberOfStakePools =
            desiredNumberOfStakePoolsFromPParams pp
        , minimumUTxO =
            minimumUTxOForShelleyBasedEra ShelleyBasedEraAllegra pp
        , stakeKeyDeposit = stakeKeyDepositFromPParams pp
        , eras = fromBoundToEpochNo <$> eraInfo
        -- Collateral inputs were not supported or required in Allegra:
        , maximumCollateralInputCount = 0
        , minimumCollateralPercentage = 0
        , executionUnitPrices = Nothing
        , currentNodeProtocolParameters
        }

fromMaryPParams
    :: W.EraInfo Bound
    -> Maybe Cardano.ProtocolParameters
    -> Mary.PParams StandardMary
    -> W.ProtocolParameters
fromMaryPParams eraInfo currentNodeProtocolParameters pp =
    W.ProtocolParameters
        { decentralizationLevel =
            decentralizationLevelFromPParams pp
        , txParameters =
            txParametersFromPParams
                maryTokenBundleMaxSize (W.ExecutionUnits 0 0) pp
        , desiredNumberOfStakePools =
            desiredNumberOfStakePoolsFromPParams pp
        , minimumUTxO =
            minimumUTxOForShelleyBasedEra ShelleyBasedEraMary pp
        , stakeKeyDeposit = stakeKeyDepositFromPParams pp
        , eras = fromBoundToEpochNo <$> eraInfo
        -- Collateral inputs were not supported or required in Mary:
        , maximumCollateralInputCount = 0
        , minimumCollateralPercentage = 0
        , executionUnitPrices = Nothing
        , currentNodeProtocolParameters
        }

fromBoundToEpochNo :: Bound -> W.EpochNo
fromBoundToEpochNo (Bound _relTime _slotNo (EpochNo e)) =
    W.EpochNo $ fromIntegral e

fromAlonzoPParams
    :: HasCallStack
    => W.EraInfo Bound
    -> Maybe Cardano.ProtocolParameters
    -> Alonzo.PParams StandardAlonzo
    -> W.ProtocolParameters
fromAlonzoPParams eraInfo currentNodeProtocolParameters pp =
    W.ProtocolParameters
        { decentralizationLevel =
            decentralizationLevelFromPParams pp
        , txParameters = txParametersFromPParams
            (W.TokenBundleMaxSize $ W.TxSize $ Alonzo._maxValSize pp)
            (fromLedgerExUnits (getField @"_maxTxExUnits" pp))
            pp
        , desiredNumberOfStakePools =
            desiredNumberOfStakePoolsFromPParams pp
        , minimumUTxO =
            minimumUTxOForShelleyBasedEra ShelleyBasedEraAlonzo pp
        , stakeKeyDeposit = stakeKeyDepositFromPParams pp
        , eras = fromBoundToEpochNo <$> eraInfo
        , maximumCollateralInputCount = unsafeIntToWord $
            Alonzo._maxCollateralInputs pp
        , minimumCollateralPercentage =
            Alonzo._collateralPercentage pp
        , executionUnitPrices =
            Just $ executionUnitPricesFromPParams pp
        , currentNodeProtocolParameters
        }

fromBabbagePParams
    :: HasCallStack
    => W.EraInfo Bound
    -> Maybe Cardano.ProtocolParameters
    -> Babbage.PParams StandardBabbage
    -> W.ProtocolParameters
fromBabbagePParams eraInfo currentNodeProtocolParameters pp =
    W.ProtocolParameters
        { decentralizationLevel =
            decentralizationLevelFromPParams pp
        , txParameters = txParametersFromPParams
            (W.TokenBundleMaxSize $ W.TxSize $ Babbage._maxValSize pp)
            (fromLedgerExUnits (getField @"_maxTxExUnits" pp))
            pp
        , desiredNumberOfStakePools =
            desiredNumberOfStakePoolsFromPParams pp
        , minimumUTxO =
            minimumUTxOForShelleyBasedEra ShelleyBasedEraBabbage pp
        , stakeKeyDeposit = stakeKeyDepositFromPParams pp
        , eras = fromBoundToEpochNo <$> eraInfo
        , maximumCollateralInputCount = unsafeIntToWord $
            Babbage._maxCollateralInputs pp
        , minimumCollateralPercentage =
            Babbage._collateralPercentage pp
        , executionUnitPrices =
            Just $ executionUnitPricesFromPParams pp
        , currentNodeProtocolParameters
        }

-- | Extract the current network decentralization level from the given set of
-- protocol parameters.
decentralizationLevelFromPParams
    :: HasField "_d" pparams SL.UnitInterval
    => pparams
    -> W.DecentralizationLevel
decentralizationLevelFromPParams pp =
    W.fromFederationPercentage $ fromUnitInterval $ getField @"_d" pp

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
        { W.pricePerStep = SL.unboundRational prSteps
        , W.pricePerMemoryUnit = SL.unboundRational prMem
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
    { getFeePolicy = W.LinearFee $ W.LinearFunction
        { intercept = naturalToDouble (getField @"_minfeeB" pp)
        , slope = naturalToDouble (getField @"_minfeeA" pp)
        }
    , getTxMaxSize = fromMaxSize $ getField @"_maxTxSize" pp
    , getTokenBundleMaxSize = maxBundleSize
    , getMaxExecutionUnits
    }
  where
    naturalToDouble :: Natural -> Double
    naturalToDouble = fromIntegral

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
    :: forall e crypto. (e ~ SL.ShelleyEra crypto, crypto ~ StandardCrypto)
    => ShelleyGenesis e
    -> (W.NetworkParameters, W.Block, [PoolCertificate])
fromGenesisData g =
    ( W.NetworkParameters
        { genesisParameters = W.GenesisParameters
            { getGenesisBlockHash = dummyGenesisHash
            , getGenesisBlockDate = W.StartTime $ sgSystemStart g
            }
        , slottingParameters = slottingParametersFromGenesis g
        , protocolParameters =
            fromShelleyPParams W.emptyEraInfo Nothing $ sgProtocolParams g
        }
    , genesisBlockFromTxOuts (Map.toList $ sgInitialFunds g)
    , poolCerts $ sgStaking g
    )
  where
    -- TODO: There is not yet any agreed upon definition of a
    -- genesis hash for a shelley-only testnet.
    --
    -- For now we use a dummy value.
    dummyGenesisHash = W.Hash . BS.pack $ replicate 32 1

    poolCerts :: SLAPI.ShelleyGenesisStaking (Crypto e) -> [PoolCertificate]
    poolCerts (SLAPI.ShelleyGenesisStaking pools _stake) = do
        (_, pp) <- Map.toList pools
        pure $ W.Registration $ PoolRegistrationCertificate
            { W.poolId = fromPoolKeyHash $ SL._poolId pp
            , W.poolOwners = fromOwnerKeyHash <$> Set.toList (SL._poolOwners pp)
            , W.poolMargin = fromUnitInterval (SL._poolMargin pp)
            , W.poolCost = toWalletCoin (SL._poolCost pp)
            , W.poolPledge = toWalletCoin (SL._poolPledge pp)
            , W.poolMetadata = fromPoolMetadata <$> strictMaybeToMaybe (SL._poolMD pp)
            }

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
            , txCBOR = Nothing
            , fee = Nothing
            , resolvedInputs = []
            , resolvedCollateralInputs = []
            , outputs =
                [W.TxOut
                    (fromShelleyAddress addr)
                    (TokenBundle.fromCoin $ fromShelleyCoin c)
                ]
            -- Collateral outputs were not supported at the time of genesis:
            , collateralOutput = Nothing
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

fromPoolId :: forall crypto. SL.KeyHash 'SL.StakePool crypto -> PoolId
fromPoolId (SL.KeyHash x) = PoolId $ hashToBytes x

fromPoolDistr
    :: forall crypto. ()
    => SL.PoolDistr crypto
    -> Map PoolId Percentage
fromPoolDistr =
    Map.map (unsafeMkPercentage . SL.individualPoolStake)
    . Map.mapKeys fromPoolId
    . SL.unPoolDistr

-- NOTE: This function disregards results that are using staking keys
fromNonMyopicMemberRewards
    :: forall era. ()
    => O.NonMyopicMemberRewards era
    -> Map (Either W.Coin W.RewardAccount) (Map PoolId W.Coin)
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

fromCardanoTxIn
    :: Cardano.TxIn
    -> W.TxIn
fromCardanoTxIn (Cardano.TxIn txid (Cardano.TxIx ix)) =
    W.TxIn
        (W.Hash $ fromShelleyTxId $ Cardano.toShelleyTxId txid)
        (fromIntegral ix)

-- | WARNING: Datum hashes are lost in the conversion!
fromCardanoTxOut :: IsCardanoEra era => Cardano.TxOut ctx era -> W.TxOut
fromCardanoTxOut (Cardano.TxOut addr out _datumHash _) =
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

toShelleyCoin :: W.Coin -> SL.Coin
toShelleyCoin (W.Coin c) = SL.Coin $ intCast c

getScriptIntegrityHash
    :: Cardano.Tx era
    -> Maybe ByteString
getScriptIntegrityHash = \case
    Cardano.ShelleyTx era tx -> case era of
        Cardano.ShelleyBasedEraShelley -> Nothing
        Cardano.ShelleyBasedEraAllegra -> Nothing
        Cardano.ShelleyBasedEraMary    -> Nothing
        Cardano.ShelleyBasedEraAlonzo  ->
            SafeHash.originalBytes <$> scriptIntegrityHashOfAlonzoTx tx
        Cardano.ShelleyBasedEraBabbage ->
            SafeHash.originalBytes <$> scriptIntegrityHashOfBabbageTx tx
    Cardano.ByronTx _                  -> Nothing

    where
      scriptIntegrityHashOfAlonzoTx
          :: Alonzo.ValidatedTx (Alonzo.AlonzoEra StandardCrypto)
          -> Maybe (Alonzo.ScriptIntegrityHash StandardCrypto)
      scriptIntegrityHashOfAlonzoTx
          (Alonzo.ValidatedTx body _wits _isValid _auxData)
              = strictMaybeToMaybe . Alonzo.scriptIntegrityHash $ body

      scriptIntegrityHashOfBabbageTx
          :: Babbage.ValidatedTx (Babbage.BabbageEra StandardCrypto)
          -> Maybe (Babbage.ScriptIntegrityHash StandardCrypto)
      scriptIntegrityHashOfBabbageTx
          (Babbage.ValidatedTx body _wits _isValid _auxData)
              = strictMaybeToMaybe . Babbage.scriptIntegrityHash $ body
-- Lovelace to coin. Quantities from ledger should always fit in Word64.
fromCardanoLovelace :: HasCallStack => Cardano.Lovelace -> W.Coin
fromCardanoLovelace =
    Coin.unsafeFromIntegral . unQuantity . Cardano.lovelaceToQuantity
  where
    unQuantity (Cardano.Quantity q) = q

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
    -> [PoolCertificate]
toPoolCertificates = mapMaybe isPoolCert
  where
      isPoolCert = \case
          W.CertificateOfPool cert -> Just cert
          _ -> Nothing

toWalletCoin :: HasCallStack => SL.Coin -> W.Coin
toWalletCoin (SL.Coin c) = Coin.unsafeFromIntegral c

fromPoolMetadata :: SL.PoolMetadata -> (StakePoolMetadataUrl, StakePoolMetadataHash)
fromPoolMetadata meta =
    ( StakePoolMetadataUrl (urlToText (SL._poolMDUrl meta))
    , StakePoolMetadataHash (SL._poolMDHash meta)
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

fromPoolKeyHash :: SL.KeyHash rol sc -> PoolId
fromPoolKeyHash (SL.KeyHash h) = PoolId (hashToBytes h)

fromOwnerKeyHash :: SL.KeyHash 'SL.Staking crypto -> PoolOwner
fromOwnerKeyHash (SL.KeyHash h) = PoolOwner (hashToBytes h)

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
    SL.TxIn (toTxId tid) (SL.mkTxIxPartial $ fromIntegral ix)

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
    ShelleyBasedEraBabbage -> toBabbageTxOut
  where
    toShelleyTxOut :: HasCallStack => W.TxOut -> Cardano.TxOut ctx ShelleyEra
    toShelleyTxOut (W.TxOut (W.Address addr) tokens) =
        Cardano.TxOut
            addrInEra
            (adaOnly $ toCardanoLovelace $ TokenBundle.getCoin tokens)
            Cardano.TxOutDatumNone
            Cardano.ReferenceScriptNone
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
            Cardano.ReferenceScriptNone
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
            (Cardano.TxOutValue Cardano.MultiAssetInMaryEra
                $ toCardanoValue tokens)
            Cardano.TxOutDatumNone
            Cardano.ReferenceScriptNone
      where
        addrInEra = tina "toCardanoTxOut: malformed address"
            [ Cardano.AddressInEra
                (Cardano.ShelleyAddressInEra Cardano.ShelleyBasedEraMary)
                    <$> deserialiseFromRawBytes AsShelleyAddress addr

            , Cardano.AddressInEra Cardano.ByronAddressInAnyEra
                <$> deserialiseFromRawBytes AsByronAddress addr
            ]

    toAlonzoTxOut :: HasCallStack => W.TxOut -> Cardano.TxOut ctx AlonzoEra
    toAlonzoTxOut (W.TxOut (W.Address addr) tokens) =
        Cardano.TxOut
            addrInEra
            (Cardano.TxOutValue Cardano.MultiAssetInAlonzoEra
                $ toCardanoValue tokens)
            datumHash
            refScript
      where
        refScript = Cardano.ReferenceScriptNone
        datumHash = Cardano.TxOutDatumNone
        addrInEra = tina "toCardanoTxOut: malformed address"
            [ Cardano.AddressInEra
                (Cardano.ShelleyAddressInEra Cardano.ShelleyBasedEraAlonzo)
                    <$> deserialiseFromRawBytes AsShelleyAddress addr

            , Cardano.AddressInEra Cardano.ByronAddressInAnyEra
                <$> deserialiseFromRawBytes AsByronAddress addr
            ]

    toBabbageTxOut :: HasCallStack => W.TxOut -> Cardano.TxOut ctx BabbageEra
    toBabbageTxOut (W.TxOut (W.Address addr) tokens) =
        Cardano.TxOut
            addrInEra
            (Cardano.TxOutValue Cardano.MultiAssetInBabbageEra
                $ toCardanoValue tokens)
            datumHash
            refScript
      where
        refScript = Cardano.ReferenceScriptNone
        datumHash = Cardano.TxOutDatumNone
        addrInEra = tina "toCardanoTxOut: malformed address"
            [ Cardano.AddressInEra
                (Cardano.ShelleyAddressInEra Cardano.ShelleyBasedEraBabbage)
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

    toCardanoAssetName (W.UnsafeTokenName name) =
        just "toCardanoValue" "TokenName"
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
        case Cardano.deserialiseFromRawBytes
            (Cardano.AsHash Cardano.AsPaymentKey) keyhash of
                Just payKeyHash -> Cardano.RequireSignature payKeyHash
                Nothing -> error "Hash key not valid"
    RequireAllOf contents ->
        Cardano.RequireAllOf $ map toCardanoSimpleScript contents
    RequireAnyOf contents ->
        Cardano.RequireAnyOf $ map toCardanoSimpleScript contents
    RequireSomeOf num contents ->
        Cardano.RequireMOf (fromIntegral num) $
            map toCardanoSimpleScript contents
    ActiveFromSlot slot ->
        Cardano.RequireTimeAfter Cardano.TimeLocksInSimpleScriptV2
        (O.SlotNo $ fromIntegral slot)
    ActiveUntilSlot slot ->
        Cardano.RequireTimeBefore Cardano.TimeLocksInSimpleScriptV2
        (O.SlotNo $ fromIntegral slot)

toCardanoSimpleScriptV1
    :: Script KeyHash
    -> Cardano.SimpleScript Cardano.SimpleScriptV1
toCardanoSimpleScriptV1 = \case
    RequireSignatureOf (KeyHash _ keyhash) ->
        case Cardano.deserialiseFromRawBytes
            (Cardano.AsHash Cardano.AsPaymentKey) keyhash of
                Just payKeyHash -> Cardano.RequireSignature payKeyHash
                Nothing -> error "Hash key not valid"
    RequireAllOf contents ->
        Cardano.RequireAllOf $ map toCardanoSimpleScriptV1 contents
    RequireAnyOf contents ->
        Cardano.RequireAnyOf $ map toCardanoSimpleScriptV1 contents
    RequireSomeOf num contents ->
        Cardano.RequireMOf (fromIntegral num) $
            map toCardanoSimpleScriptV1 contents
    _ -> error "timelocks not available in SimpleScriptV1"

just :: Builder -> Builder -> [Maybe a] -> a
just t1 t2 = tina (t1+|": unable to deserialise "+|t2)

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

toStakeKeyDeregCert :: Either XPub (Script KeyHash) -> Cardano.Certificate
toStakeKeyDeregCert = \case
    Left xpub ->
        Cardano.makeStakeAddressDeregistrationCertificate
        . Cardano.StakeCredentialByKey
        . Cardano.StakeKeyHash
        . SL.KeyHash
        . UnsafeHash
        . toShort
        . blake2b224
        $ xpubPublicKey xpub
    Right script ->
        Cardano.makeStakeAddressDeregistrationCertificate
        . Cardano.StakeCredentialByScript
        . Cardano.hashScript
        . Cardano.SimpleScript Cardano.SimpleScriptV2
        $ toCardanoSimpleScript script

toStakeKeyRegCert :: Either XPub (Script KeyHash) -> Cardano.Certificate
toStakeKeyRegCert cred = case cred of
    Left xpub ->
        Cardano.makeStakeAddressRegistrationCertificate
        . Cardano.StakeCredentialByKey
        . Cardano.StakeKeyHash
        . SL.KeyHash
        . UnsafeHash
        . toShort
        . blake2b224
        $ xpubPublicKey xpub
    Right script ->
        Cardano.makeStakeAddressRegistrationCertificate
        . Cardano.StakeCredentialByScript
        . Cardano.hashScript
        . Cardano.SimpleScript Cardano.SimpleScriptV2
        $ toCardanoSimpleScript script

toStakePoolDlgCert :: Either XPub (Script KeyHash) -> PoolId -> Cardano.Certificate
toStakePoolDlgCert cred (PoolId pid) = case cred of
    Left xpub ->
        Cardano.makeStakeAddressDelegationCertificate
        (Cardano.StakeCredentialByKey $ Cardano.StakeKeyHash (toKeyHash xpub))
        (Cardano.StakePoolKeyHash pool)
    Right script ->
        Cardano.makeStakeAddressDelegationCertificate
        (Cardano.StakeCredentialByScript
        . Cardano.hashScript
        . Cardano.SimpleScript Cardano.SimpleScriptV2
        $ toCardanoSimpleScript script)
        (Cardano.StakePoolKeyHash pool)
  where
    toKeyHash = SL.KeyHash . UnsafeHash . toShort . blake2b224 . xpubPublicKey
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
unsealShelleyTx
    :: AnyCardanoEra
    -- ^ Preferred latest era (see 'ideallyNoLaterThan')
    -> W.SealedTx
    -> TxInMode CardanoMode
unsealShelleyTx era wtx = case W.cardanoTxIdeallyNoLaterThan era wtx of
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
    Cardano.InAnyCardanoEra BabbageEra tx ->
        TxInMode tx BabbageEraInCardanoMode

-- | Converts a 'ShelleyBasedEra' to the broader 'CardanoEra'.
shelleyBasedToCardanoEra :: ShelleyBasedEra era -> CardanoEra era
shelleyBasedToCardanoEra = \case
    Cardano.ShelleyBasedEraShelley -> ShelleyEra
    Cardano.ShelleyBasedEraAllegra -> AllegraEra
    Cardano.ShelleyBasedEraMary    -> MaryEra
    Cardano.ShelleyBasedEraAlonzo  -> AlonzoEra
    Cardano.ShelleyBasedEraBabbage -> BabbageEra

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


--------------------------------------------------------------------------------
-- Unsafe conversions
--------------------------------------------------------------------------------

-- | Extracts a 'Coin' value from a 'Cardano.Lovelace' value.
--
-- Fails with a run-time error if the value is negative.
--
unsafeLovelaceToWalletCoin :: HasCallStack => Cardano.Lovelace -> W.Coin
unsafeLovelaceToWalletCoin (Cardano.Lovelace v) =
  case intCastMaybe @Integer @Natural v of
      Nothing -> error $ unwords
          [ "unsafeLovelaceToWalletCoin:"
          , "encountered negative value:"
          , show v
          ]
      Just lovelaceNonNegative ->
          W.Coin lovelaceNonNegative

-- | Extracts a 'Cardano.Lovelace' value from a 'Cardano.Value'.
--
-- Fails with a run-time error if the 'Cardano.Value' contains any non-ada
-- assets.
--
unsafeValueToLovelace :: HasCallStack => Cardano.Value -> Cardano.Lovelace
unsafeValueToLovelace v =
    case Cardano.valueToLovelace v of
        Nothing -> error $ unwords
            [ "unsafeValueToLovelace:"
            , "encountered value with non-ada assets:"
            , show v
            ]
        Just lovelace -> lovelace

{-------------------------------------------------------------------------------
                   Assessing sizes of token bundles
-------------------------------------------------------------------------------}

-- | Assesses a token bundle size in relation to the maximum size that can be
--   included in a transaction output.
--
-- See 'W.TokenBundleSizeAssessor' for the expected properties of this function.
--
tokenBundleSizeAssessor :: TokenBundleMaxSize -> TokenBundleSizeAssessor
tokenBundleSizeAssessor maxSize = TokenBundleSizeAssessor {..}
  where
    assessTokenBundleSize tb
        | serializedLengthBytes <= maxSize' =
            TokenBundleSizeWithinLimit
        | otherwise =
            TokenBundleSizeExceedsLimit
      where
        serializedLengthBytes :: W.TxSize
        serializedLengthBytes = computeTokenBundleSerializedLengthBytes tb

        maxSize' :: W.TxSize
        maxSize' = W.unTokenBundleMaxSize maxSize

computeTokenBundleSerializedLengthBytes :: TokenBundle.TokenBundle -> W.TxSize
computeTokenBundleSerializedLengthBytes = W.TxSize . safeCast
    . BS.length . serialize' . Cardano.toMaryValue . toCardanoValue
  where
    safeCast :: Int -> Natural
    safeCast = fromIntegral

{-------------------------------------------------------------------------------
                      Address Encoding / Decoding
-------------------------------------------------------------------------------}

instance EncodeStakeAddress 'Mainnet where
    encodeStakeAddress = shelleyEncodeStakeAddress SL.Mainnet
instance EncodeStakeAddress ('Testnet pm) where
    encodeStakeAddress = shelleyEncodeStakeAddress SL.Testnet

instance DecodeStakeAddress 'Mainnet where
    decodeStakeAddress = shelleyDecodeStakeAddress SL.Mainnet
instance DecodeStakeAddress ('Testnet pm) where
    decodeStakeAddress = shelleyDecodeStakeAddress SL.Testnet

stakeAddressPrefix :: Word8
stakeAddressPrefix = 0xE0

networkIdMask :: Word8
networkIdMask = 0x0F

toNetworkId :: SL.Network -> Word8
toNetworkId = \case
    SL.Testnet -> 0
    SL.Mainnet -> 1

shelleyEncodeStakeAddress :: SL.Network -> W.RewardAccount -> Text
shelleyEncodeStakeAddress network (W.RewardAccount acct) =
    Bech32.encodeLenient hrp (dataPartFromBytes bytes)
  where
    hrp = case network of
        SL.Testnet -> [Bech32.humanReadablePart|stake_test|]
        SL.Mainnet -> [Bech32.humanReadablePart|stake|]
    bytes = BL.toStrict $ runPut $ do
        putWord8 $ (networkIdMask .&. toNetworkId network) .|. stakeAddressPrefix
        putByteString acct

shelleyDecodeStakeAddress ::
    SL.Network -> Text -> Either TextDecodingError W.RewardAccount
shelleyDecodeStakeAddress serverNetwork txt = do
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
    encodeAddress = shelleyEncodeAddress SL.Mainnet

instance EncodeAddress ('Testnet pm) where
    -- https://github.com/cardano-foundation/CIPs/tree/master/CIP5
    encodeAddress = shelleyEncodeAddress SL.Testnet

shelleyEncodeAddress :: SL.Network -> W.Address -> Text
shelleyEncodeAddress network (W.Address bytes) =
    if isJust (CBOR.deserialiseCbor CBOR.decodeAddressPayload bytes)
        then base58
        else bech32
  where
    base58 = T.decodeUtf8 $ encodeBase58 bitcoinAlphabet bytes
    bech32 = Bech32.encodeLenient hrp (dataPartFromBytes bytes)
    hrp = case network of
        SL.Testnet -> [Bech32.humanReadablePart|addr_test|]
        SL.Mainnet -> [Bech32.humanReadablePart|addr|]

instance DecodeAddress 'Mainnet where
    decodeAddress = shelleyDecodeAddress SL.Mainnet

instance DecodeAddress ('Testnet pm) where
    decodeAddress = shelleyDecodeAddress SL.Testnet

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
shelleyDecodeAddress :: SL.Network -> Text -> Either TextDecodingError W.Address
shelleyDecodeAddress serverNetwork =
    decodeBytes >=> decodeShelleyAddress @StandardCrypto
  where
    decodeShelleyAddress :: forall c.
        (SL.Crypto c) => ByteString -> Either TextDecodingError W.Address
    decodeShelleyAddress bytes = do
        case SL.deserialiseAddr @c bytes of
            Just (SL.Addr addrNetwork _ _) -> do
                guardNetwork addrNetwork serverNetwork
                pure (W.Address bytes)

            Just (SL.AddrBootstrap (SL.BootstrapAddress addr)) -> do
                guardNetwork
                    (fromByronNetworkMagic (Byron.addrNetworkMagic addr))
                    serverNetwork
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
