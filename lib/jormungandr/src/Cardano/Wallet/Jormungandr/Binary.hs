{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: Apache-2.0
--
-- The format is for the Shelley era as implemented by the Jörmungandr node.
-- It is described [here](https://github.com/input-output-hk/chain-libs/blob/master/chain-impl-mockchain/doc/format.md)
--
-- The module to some extent defines its own Jörmungandr-specific types,
-- different from "Cardano.Wallet.Primitive.Types". Here, transactions are just
-- one of many possible 'Message's that can be included in a block.
--
-- In some cases it also leads us to /throw exceptions/ when integers would
-- otherwise overflow (look for uses of 'toEnum').

module Cardano.Wallet.Jormungandr.Binary
    ( Block (..)
    , BlockHeader (..)
    , ConfigParam (..)
    , ConsensusVersion (..)
    , LeaderId (..)
    , Message (..)
    , MessageType (..)
    , Milli (..)
    , getBlock
    , getBlockHeader
    , getBlockId
    , getMessage
    , getTransaction
    , putSignedTx
    , putTx

    -- * Transaction witnesses
    , signData
    , utxoWitness
    , legacyUtxoWitness
    , TxWitnessTag (..)
    , putTxWitnessTag
    , getTxWitnessTag
    , txWitnessSize

    -- * Purification of chain block types
    , convertBlock
    , convertBlockHeader

    -- * Addresses
    , putAddress
    , getAddress

      -- * Helpers
    , blake2b256
    , estimateMaxNumberOfInputsParams
    , fragmentId
    , maxNumberOfInputs
    , maxNumberOfOutputs
    , withHeader

      -- * Re-export
    , Get
    , runGet
    , runGetOrFail
    , Put
    , runPut
    ) where

import Prelude

import Cardano.Crypto.Wallet
    ( XPub, unXPub )
import Cardano.Wallet.Jormungandr.Primitive.Types
    ( Tx (..) )
import Cardano.Wallet.Primitive.AddressDerivation
    ( NetworkDiscriminant (..) )
import Cardano.Wallet.Primitive.Fee
    ( FeePolicy (..) )
import Cardano.Wallet.Primitive.Types
    ( Address (..)
    , Coin (..)
    , Hash (..)
    , PoolId (..)
    , SlotId (..)
    , TxIn (..)
    , TxOut (..)
    , TxWitness (..)
    )
import Cardano.Wallet.Transaction
    ( EstimateMaxNumberOfInputsParams (..) )
import Control.Applicative
    ( many )
import Control.Monad
    ( replicateM, unless )
import Crypto.Hash
    ( hash )
import Crypto.Hash.Algorithms
    ( Blake2b_256 )
import Data.Binary.Get
    ( Get
    , bytesRead
    , getByteString
    , getLazyByteString
    , getWord16be
    , getWord32be
    , getWord64be
    , getWord8
    , isolate
    , label
    , lookAhead
    , runGet
    , runGetOrFail
    , skip
    )
import Data.Binary.Put
    ( Put, putByteString, putWord16be, putWord64be, putWord8, runPut )
import Data.Bits
    ( shift, (.&.) )
import Data.ByteString
    ( ByteString )
import Data.Quantity
    ( Quantity (..) )
import Data.Time.Clock
    ( NominalDiffTime, secondsToDiffTime )
import Data.Time.Clock.POSIX
    ( posixSecondsToUTCTime )
import Data.Word
    ( Word16, Word32, Word64, Word8 )

import qualified Cardano.Wallet.Primitive.Types as W
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

-- maximum number of inputs in a valid transaction
maxNumberOfInputs :: Int
maxNumberOfInputs = 255

-- maximum number of outputs in a valid transaction
maxNumberOfOutputs :: Int
maxNumberOfOutputs = 255

{-------------------------------------------------------------------------------
                                 Blocks
-------------------------------------------------------------------------------}

-- Do-notation is favoured over applicative syntax for readability:
{-# ANN module ("HLint: ignore Use <$>" :: String) #-}

data BlockHeader = BlockHeader
    { version :: Word16
    , contentSize :: Word32
    , slot :: SlotId
    , chainLength :: Word32
    , contentHash :: Hash "content"
    , headerHash :: Hash "BlockHeader"
    , parentHeaderHash :: Hash "BlockHeader"
    , producedBy :: Maybe PoolId
        -- ^ Will contain the VRFPubKey of the stake pool for non-genesis
        -- Genesis/Praos blocks.
    } deriving (Show, Eq)

data Block = Block
    { header :: BlockHeader
    , messages :: [Message]
    } deriving (Eq, Show)

getBlockHeader :: Get BlockHeader
getBlockHeader = label "getBlockHeader" $ do
    size <- fromIntegral <$> getWord16be
    bytes <- lookAhead (getByteString size)
    isolate size $ do
        -- Common structure.
        version <- getWord16be
        contentSize <- getWord32be
        slotEpoch <- fromIntegral <$> getWord32be
        slotId <- toEnum . fromEnum <$> getWord32be
        chainLength <- getWord32be
        contentHash <- Hash <$> getByteString 32 -- or 256 bits
        parentHeaderHash <- Hash <$> getByteString 32
        let headerHash = Hash $ blake2b256 bytes
        -- Proof.
        -- There are three different types of proofs:
        -- 1. no proof (used for the genesis blockheader)
        -- 2. BFT
        -- 3. Praos / Genesis
        --
        -- We could make sure we get the right kind of proof, but we don't need to.
        -- Just checking that the length is not totally wrong, is much simpler
        -- and gives us sanity about the binary format being correct.
        read' <- fromIntegral <$> bytesRead
        let remaining = size - read'
        producedBy <- case remaining of
            0 ->
                -- no proof
                skip remaining >> return Nothing
            96 ->
                -- BFT
                skip remaining >> return Nothing
            612 ->
                -- Praos/Genesis
                Just . PoolId <$> getByteString 32 <* skip (remaining - 32)
            _ -> fail $ "BlockHeader proof has unexpected size " <> (show remaining)
        return $ BlockHeader
            { version
            , contentSize
            , slot = SlotId { epochNumber = slotEpoch, slotNumber = slotId }
            , chainLength
            , contentHash
            , parentHeaderHash
            , headerHash
            , producedBy
            }

getBlock :: Get Block
getBlock = label "getBlock" $ do
    header <- getBlockHeader
    messages <- isolate (fromIntegral $ contentSize header) (many getMessage)
    return $ Block{header,messages}

-- | Extract a 'Block' id from a serialized 'Block'.
getBlockId :: Get (Hash "BlockHeader")
getBlockId = lookAhead getBlock *> label "getBlockId" (do
    size <- getWord16be
    bytes <- getByteString (fromEnum size)
    return $ Hash $ blake2b256 bytes)

{-------------------------------------------------------------------------------
                           Messages
-------------------------------------------------------------------------------}

-- | The block-body consists of messages. There are several types of messages.
--
--  Following, as closely as possible:
-- https://github.com/input-output-hk/rust-cardano/blob/e0616f13bebd6b908320bddb1c1502dea0d3305a/chain-impl-mockchain/src/message/mod.rs#L22-L29
data Message
    = Initial [ConfigParam]
    -- ^ Found in the genesis block.
    | Transaction (Tx, [TxWitness])
    -- ^ A standard signed transaction
    | UnimplementedMessage Int
    -- Messages not yet supported go there.
    deriving (Eq, Show)

data MessageType
    = MsgTypeInitial
    | MsgTypeLegacyUTxO
    | MsgTypeTransaction

data TxWitnessTag
    = TxWitnessLegacyUTxO
    | TxWitnessUTxO
    | TxWitnessAccount
    | TxWitnessMultisig
    deriving (Show, Eq)

putTxWitnessTag :: TxWitnessTag -> Put
putTxWitnessTag = \case
    TxWitnessLegacyUTxO -> putWord8 0
    TxWitnessUTxO -> putWord8 1
    TxWitnessAccount -> putWord8 2
    TxWitnessMultisig -> putWord8 3

getTxWitnessTag :: Get TxWitnessTag
getTxWitnessTag = getWord8 >>= \case
    0 -> pure TxWitnessLegacyUTxO
    1 -> pure TxWitnessUTxO
    2 -> pure TxWitnessAccount
    3 -> pure TxWitnessMultisig
    other -> fail $ "Invalid witness type: " ++ show other

-- | Decode a message (header + contents).
getMessage :: Get Message
getMessage = label "getMessage" $ do
    size <- fromIntegral <$> getWord16be

    -- We lazily compute the fragment-id, using lookAHead, before calling the
    -- specialized decoders.
    --
    -- The fragment-id is needed, for instance, to construct a @Tx@, where it
    -- corresponds to the txId (a.k.a "tx hash").
    fragId <- Hash . blake2b256 . BL.toStrict
        <$> lookAhead (getLazyByteString $ fromIntegral size)

    msgType <- fromIntegral <$> getWord8
    let remaining = size - 1
    let unimpl = skip remaining >> return (UnimplementedMessage msgType)
    isolate remaining $ case msgType of
        0 -> Initial <$> getInitial
        1 -> Transaction . (,[]) <$> getLegacyTransaction remaining
        2 -> Transaction <$> getTransaction fragId
        3 -> unimpl -- Certificate
        4 -> unimpl -- UpdateProposal
        5 -> unimpl -- UpdateVote
        other -> fail $ "Unexpected content type tag " ++ show other

messageTypeTag :: MessageType -> Word8
messageTypeTag = \case
    MsgTypeInitial -> 0
    MsgTypeLegacyUTxO -> 1
    MsgTypeTransaction -> 2

-- | Decode the contents of a @Initial@-message.
getInitial :: Get [ConfigParam]
getInitial = label "getInitial" $ do
    len <- fromIntegral <$> getWord16be
    replicateM len getConfigParam

{-------------------------------------------------------------------------------
                                Transactions
-------------------------------------------------------------------------------}

txWitnessSize :: TxWitnessTag -> Int
txWitnessSize = \case
    TxWitnessLegacyUTxO -> 128
    TxWitnessUTxO -> 64
    TxWitnessAccount -> 64
    TxWitnessMultisig -> 68

txWitnessTagSize :: Int
txWitnessTagSize = 1

-- | Construct a UTxO witness from a signature
utxoWitness :: ByteString -> TxWitness
utxoWitness bytes = TxWitness $ BL.toStrict $ runPut $ do
    putTxWitnessTag TxWitnessUTxO
    putByteString bytes

-- | Construct a legacy UTxO witness from a public key and a signature
legacyUtxoWitness :: XPub -> ByteString -> TxWitness
legacyUtxoWitness xpub bytes = TxWitness $ BL.toStrict $ runPut $ do
    putTxWitnessTag TxWitnessLegacyUTxO
    putByteString (unXPub xpub)
    putByteString bytes

-- | Decode the contents of a @Transaction@-message.
getTransaction :: Hash "Tx" -> Get (Tx, [TxWitness])
getTransaction tid = label "getTransaction" $ do
    (ins, outs) <- getTokenTransfer
    let witnessCount = length ins
    wits <- replicateM witnessCount getWitness
    return (Tx tid ins outs, wits)
  where
    getWitness :: Get TxWitness
    getWitness = do
        tag <- lookAhead getTxWitnessTag
        let len = txWitnessSize tag + txWitnessTagSize
        -- NOTE: Regardless of the type of witness, we decode it as a
        -- @TxWitness@.
        TxWitness <$> getByteString len

    getTokenTransfer :: Get ([(TxIn, Coin)], [TxOut])
    getTokenTransfer = label "getTokenTransfer" $ do
        inCount <- fromIntegral <$> getWord8
        outCount <- fromIntegral <$> getWord8
        ins <- replicateM inCount getInput
        outs <- replicateM outCount getOutput
        return (ins, outs)
      where
        getInput = isolate 41 $ do
            -- NOTE: special value 0xff indicates account spending
            index <- fromIntegral <$> getWord8
            coin <- Coin <$> getWord64be
            tx <- Hash <$> getByteString 32
            return (TxIn tx index, coin)

        getOutput = do
            addr <- getAddress
            value <- Coin <$> getWord64be
            return $ TxOut addr value

--
-- @
-- FRAGMENT-ID = H(TYPE | CONTENT)
-- CONTENT = #OUTPUTS (1 byte) | OUTPUT-1 | .. | OUTPUT-N
-- OUTPUT = VALUE (8 bytes) | ADDR-SIZE (2 bytes) | ADDR (ADDR-SIZE bytes)
-- TYPE = 1
--
-- H = blake2b_256
-- @
getLegacyTransaction :: Int -> Get Tx
getLegacyTransaction size = do
    bytes <- lookAhead (getLazyByteString $ fromIntegral size)
    let tag = runPut (putWord8 (messageTypeTag MsgTypeLegacyUTxO))
    let tid = Hash . blake2b256 . BL.toStrict $ (tag <> bytes)
    n <- fromIntegral <$> getWord8
    outs <- replicateM n $ do
        coin <- Coin <$> getWord64be
        addr <- getLegacyAddress
        pure (TxOut addr coin)
    let inps = pure (error "TODO: getLegacyTransaction inputs?")
    pure (Tx tid inps outs)

putSignedTx :: [(TxIn, Coin)] -> [TxOut] -> [TxWitness] -> Put
putSignedTx inputs outputs witnesses = do
    putTx inputs outputs
    unless (length inputs == length witnesses) $
        fail "number of witnesses must equal number of inputs"
    mapM_ putWitness witnesses
  where
    -- Assumes the `TxWitness` has been faithfully constructed
    putWitness :: TxWitness -> Put
    putWitness (TxWitness bytes) = putByteString bytes

putTx :: [(TxIn, Coin)] -> [TxOut] -> Put
putTx inputs outputs = do
    unless (length inputs <= fromIntegral (maxBound :: Word8)) $
        fail ("number of inputs cannot be greater than " ++ show maxNumberOfInputs)
    unless (length outputs <= fromIntegral (maxBound :: Word8)) $
        fail ("number of outputs cannot be greater than " ++ show maxNumberOfOutputs)
    putWord8 $ toEnum $ length inputs
    putWord8 $ toEnum $ length outputs
    mapM_ putInput inputs
    mapM_ putOutput outputs
  where
    putInput (TxIn inputId inputIx, coin) = do
        -- NOTE: special value 0xff indicates account spending
        -- only old utxo/address scheme supported for now
        putWord8 . toEnum . fromEnum $ inputIx
        putWord64be $ getCoin coin
        putByteString $ getHash inputId

    putOutput (TxOut address coin) = do
        putAddress address
        putWord64be $ getCoin coin

{-------------------------------------------------------------------------------
                            Config Parameters
-------------------------------------------------------------------------------}

data ConfigParam
    = Block0Date W.StartTime
    -- ^ The official start time of the blockchain, in seconds since the Unix
    -- epoch.
    | Discrimination NetworkDiscriminant
    -- ^ Address discrimination. Testnet / Mainnet.
    | Consensus ConsensusVersion
    -- ^ Consensus version. BFT / Genesis Praos.
    | SlotsPerEpoch W.EpochLength
    -- ^ Number of slots in an epoch.
    | SlotDuration NominalDiffTime
    -- ^ Slot duration in seconds.
    | EpochStabilityDepth (Quantity "block" Word32)
    -- ^ The length of the suffix of the chain (in blocks) considered unstable.
    | ConsensusGenesisPraosParamF Milli
    -- ^ Determines maximum probability of a stakeholder being elected as leader
    -- in a slot.
    | MaxNumberOfTransactionsPerBlock Word32
    -- ^ Maximum number of transactions in a block.
    | BftSlotsRatio Milli
    -- ^ Fraction of blocks to be created by BFT leaders.
    | AddBftLeader LeaderId
    -- ^ Add a BFT Leader
    | RemoveBftLeader LeaderId
    -- ^ Remove a BFT Leader
    | AllowAccountCreation Bool
    -- ^ Enable/disable account creation.
    | ConfigLinearFee FeePolicy
    -- ^ Coefficients for fee calculations.
    | ProposalExpiration (Quantity "epoch" Word32)
    -- ^ Number of epochs until an update proposal expires.
    | KesUpdateSpeed (Quantity "second/update" Word32)
    -- ^ Maximum number of seconds per update for KES keys known by the system
    -- after start time.
    deriving (Eq, Show)

getConfigParam :: Get ConfigParam
getConfigParam = label "getConfigParam" $ do
    -- The tag and the size/length of the config param is stored in a single
    -- @Word16@.
    --
    -- 6 least-significant bits: length
    -- 12 most-significant bits: tag
    taglen <- getWord16be
    let tag = taglen `shift` (-6)
    let len = fromIntegral $ taglen .&. (63) -- 0b111111
    isolate len $ case tag of
        1 -> Discrimination <$> getNetworkDiscriminant
        2 -> Block0Date . W.StartTime . posixSecondsToUTCTime . fromIntegral <$> getWord64be
        3 -> Consensus <$> getConsensusVersion
        4 -> SlotsPerEpoch . W.EpochLength . fromIntegral  <$> getWord32be
        5 -> SlotDuration . secondsToNominalDiffTime <$> getWord8
        6 -> EpochStabilityDepth . Quantity <$> getWord32be
        8 -> ConsensusGenesisPraosParamF <$> getMilli
        9 -> MaxNumberOfTransactionsPerBlock <$> getWord32be
        10 -> BftSlotsRatio <$> getMilli
        11 -> AddBftLeader <$> getLeaderId
        12 -> RemoveBftLeader <$> getLeaderId
        13 -> AllowAccountCreation <$> getBool
        14 -> ConfigLinearFee <$> getLinearFee
        15 -> ProposalExpiration . Quantity <$> getWord32be
        16 -> KesUpdateSpeed . Quantity <$> getWord32be
        other -> fail $ "Invalid config param with tag " ++ show other
  where
    -- NOTE
    -- Conversion between 'DiffTime' <-> 'NominalDiffTime' is safe here because
    -- the duration can't never be more than 255 seconds, so the leap second
    -- is never involved.
    secondsToNominalDiffTime :: Word8 -> NominalDiffTime
    secondsToNominalDiffTime =
        toEnum . fromEnum . secondsToDiffTime .  fromIntegral

-- | Used to represent (>= 0) rational numbers as (>= 0) integers, by just
-- multiplying by 1000. For instance: '3.141592' is represented as 'Milli 3142'.
newtype Milli = Milli Word64
    deriving (Eq, Show)

newtype LeaderId = LeaderId ByteString
    deriving (Eq, Show)

data ConsensusVersion = BFT | GenesisPraos
    deriving (Eq, Show)

getConsensusVersion :: Get ConsensusVersion
getConsensusVersion = label "getConsensusVersion" $ getWord16be >>= \case
    1 -> return BFT
    2 -> return GenesisPraos
    other -> fail $ "Unknown consensus version: " ++ show other

getNetworkDiscriminant :: Get NetworkDiscriminant
getNetworkDiscriminant = label "getNetworkDiscriminant" $ getWord8 >>= \case
    1 -> return Mainnet
    2 -> return Testnet
    other -> fail $ "Invalid network/discrimination value: " ++ show other

getMilli :: Get Milli
getMilli = label "getMilli" $ Milli <$> getWord64be

getLeaderId :: Get LeaderId
getLeaderId = label "getLeaderId" $ LeaderId <$> getByteString 32

getLinearFee :: Get FeePolicy
getLinearFee = label "getFeePolicy" $ do
    a <- getWord64be
    b <- getWord64be
    _perCert <- getWord64be
    return $ LinearFee (Quantity $ double a) (Quantity $ double b)
  where
    double = fromRational . toRational

getBool :: Get Bool
getBool = getWord8 >>= \case
    1 ->
        return True
    0 ->
        return False
    other ->
        fail $ "Unexpected integer: " ++ show other ++ ". Expected a boolean."

{-------------------------------------------------------------------------------
                            Addresses
-------------------------------------------------------------------------------}

getAddress :: Get Address
getAddress = do
    -- We use 'lookAhead' to not consume the header, and let it
    -- be included in the underlying Address ByteString.
    headerByte <- label "address header" . lookAhead $ getWord8
    let kind = kindValue headerByte
    case kind of
        0x3 -> Address <$> getByteString 33 -- single address
        0x4 -> Address <$> getByteString 65 -- grouped address
        0x5 -> Address <$> getByteString 33 -- account address
        0x6 -> Address <$> getByteString 33 -- multisig address
        other -> fail $ "Invalid address type: " ++ show other
  where
    kindValue :: Word8 -> Word8
    kindValue = (.&. 0b01111111)

getLegacyAddress :: Get Address
getLegacyAddress = do
    size <- fromIntegral <$> getWord16be
    Address <$> getByteString size

putAddress :: Address -> Put
putAddress (Address bs) = putByteString bs

{-------------------------------------------------------------------------------
                              Helpers
-------------------------------------------------------------------------------}

-- | Add a corresponding header to a message. Every message is encoded as:
--
--     HEADER(MESSAGE) | MESSAGE
--
-- where `HEADER` is:
--
--     SIZE (2 bytes) | TYPE (1 byte)
--
withHeader :: MessageType -> Put -> Put
withHeader typ content = do
    let bs = BL.toStrict $ runPut (putWord8 (messageTypeTag typ) *> content)
    putWord16be (toEnum $ BS.length bs)
    putByteString bs

-- | Compute a Blake2b_256 hash of a given 'ByteString'
blake2b256 :: ByteString -> ByteString
blake2b256 =
    BA.convert . hash @_ @Blake2b_256

-- | This provides network encoding specific variables to be used by the
-- 'estimateMaxNumberOfInputs' function.
estimateMaxNumberOfInputsParams
    :: EstimateMaxNumberOfInputsParams t
estimateMaxNumberOfInputsParams = EstimateMaxNumberOfInputsParams
    { estMeasureTx = \ins outs wits -> fromIntegral $ BL.length $
        runPut $ withHeader MsgTypeTransaction $
            putSignedTx (map (, Coin 0) ins) outs wits

    -- Block IDs are always this long.
    , estBlockHashSize = 32

    -- The length of the smallest type of witness.
    , estTxWitnessSize = txWitnessSize TxWitnessUTxO + txWitnessTagSize
    }

-- | Jörmungandr distinguish 'fragment id' (what we commonly call 'txId')
-- from 'transaction sign data'. A transaction fragment corresponds to
-- a signed transaction (inputs, outputs and witnesses). So, the
-- witnesses are required to compute a `txid`.
fragmentId
    :: [(TxIn, Coin)]
    -> [TxOut]
    -> [TxWitness]
    -> Hash "Tx"
fragmentId inps outs wits =
    Hash $ blake2b256 $ BL.toStrict $ runPut $ do
        putWord8 (messageTypeTag MsgTypeTransaction)
        putSignedTx inps outs wits

-- | See 'fragmentId'. This computes the signing data required for producing
-- transaction witnesses.
signData
    :: [(TxIn, Coin)]
    -> [TxOut]
    -> Hash "SignData"
signData inps outs =
    Hash $ blake2b256 $ BL.toStrict $ runPut $ do
        putTx inps outs

{-------------------------------------------------------------------------------
                                Conversions
-------------------------------------------------------------------------------}

-- | Convert the Jörmungandr binary format block into a simpler Wallet block.
convertBlock :: Block -> W.Block Tx
convertBlock (Block h msgs) =
    W.Block (convertBlockHeader h) coerceMessages
  where
    coerceMessages = msgs >>= \case
        Initial _ -> []
        Transaction (tx, _wits) -> return tx
        UnimplementedMessage _ -> []

-- | Convert the Jörmungandr binary format header into a simpler Wallet header.
convertBlockHeader :: BlockHeader -> W.BlockHeader
convertBlockHeader h = (W.BlockHeader (slot h) (bh h) (Hash "") (Hash ""))
    { W.headerHash = headerHash h
    , W.parentHeaderHash = parentHeaderHash h
    }
  where
    bh = Quantity . fromIntegral . chainLength
