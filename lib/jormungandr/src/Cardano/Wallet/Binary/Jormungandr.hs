{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: MIT
--
-- The format is for the Shelley era as implemented by the Jörmungandr node.
--
-- It is described [here](https://github.com/input-output-hk/rust-cardano/blob/master/chain-impl-mockchain/doc/format.md)

module Cardano.Wallet.Binary.Jormungandr
    ( Block (..)
    , BlockHeader (..)
    , Message (..)
    , getBlockHeader
    , getBlock

    , ConfigParam (..)
    , ConsensusVersion (..)
    , LeaderId (..)
    , LinearFee (..)
    , Milli (..)

     -- * Re-export
    , runGet

    ) where

import Prelude

import Cardano.Environment.Jormungandr
    ( Network (..) )
import Cardano.Wallet.Primitive.Types
    ( Address (..)
    , Coin (..)
    , Hash (..)
    , SlotId (..)
    , Tx (..)
    , TxIn (..)
    , TxOut (..)
    , TxWitness (..)
    )
import Control.Monad
    ( replicateM )
import Data.Binary.Get
    ( Get
    , getByteString
    , getWord16be
    , getWord32be
    , getWord64be
    , getWord8
    , isEmpty
    , isolate
    , runGet
    , skip
    )
import Data.Bits
    ( shift, (.&.) )
import Data.ByteString
    ( ByteString )
import Data.Quantity
    ( Quantity (..) )
import Data.Word
    ( Word16, Word32, Word64, Word8 )

data BlockHeader = BlockHeader
    { version :: Word16
    , contentSize :: Word32
    , slot :: SlotId
    , chainLength :: Word32
    , contentHash :: Hash "content"
    , parentHeaderHash :: Hash "parentHeader"
    } deriving (Show, Eq)

data Block = Block BlockHeader [Message]
    deriving (Eq, Show)

data SignedUpdateProposal = SignedUpdateProposal
    deriving (Eq, Show)
data TODO = TODO
    deriving (Eq, Show)
data SignedVote = SignedVote
    deriving (Eq, Show)

-- Do-notation is favoured over applicative syntax for readability:
{-# ANN module ("HLint: ignore Use <$>" :: String) #-}

getBlockHeader :: Get BlockHeader
getBlockHeader =  (fromIntegral <$> getWord16be) >>= \s -> isolate s $ do
    version <- getWord16be
    contentSize <- getWord32be
    slotEpoch <- fromIntegral <$> getWord32be
    slotId <- fromIntegral <$> getWord32be
    chainLength <- getWord32be
    contentHash <- Hash <$> getByteString 32 -- or 256 bits
    parentHeaderHash <- Hash <$> getByteString 32

    -- TODO: Handle special case for BFT
    -- TODO: Handle special case for Praos/Genesis

    return $ BlockHeader
        { version = version
        , contentSize = contentSize
        , slot = (SlotId slotId slotEpoch)
        , chainLength = chainLength
        , contentHash = contentHash
        , parentHeaderHash = parentHeaderHash
        }

getBlock :: Get Block
getBlock = do
    header <- getBlockHeader
    msgs <- isolate (fromIntegral $ contentSize header)
        $ whileM (not <$> isEmpty) getMessage
    return $ Block header msgs

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
--  | OldUtxoDeclaration UtxoDeclaration
    | Transaction Tx
--  | Certificate (Tx with Extra=Certificate)
--  | UpdateProposal SignedUpdateProposal
--  | UpdateVote SignedVote
    | UnimplementedMessage Int -- For development. Remove later.
    deriving (Eq, Show)

-- | Decode a message (header + contents).
getMessage :: Get Message
getMessage = do
    size <- fromIntegral <$> getWord16be
    contentType <- fromIntegral <$> getWord8
    let remaining = size - 1
    let unimpl = skip remaining >> return (UnimplementedMessage contentType)
    isolate remaining $ case contentType of
        0 -> Initial <$> getInitial
        1 -> unimpl
        2 -> Transaction <$> getTransaction
        3 -> unimpl
        4 -> unimpl
        5 -> unimpl
        other -> fail $ "Unexpected content type tag " ++ show other

-- | Decode the contents of a @Initial@-message.
getInitial :: Get [ConfigParam]
getInitial = do
    len <- fromIntegral <$> getWord16be
    replicateM len getConfigParam

-- | Decode the contents of a @Transaction@-message.
getTransaction :: Get Tx
getTransaction = isolate 43 $ do
    (ins, outs) <- getTokenTransfer

    let witnessCount = length ins
    _wits <- replicateM witnessCount getWitness

    return $ Tx ins outs
  where
    getWitness = do
        tag <- getWord8
        case tag of
            1 -> isolate 128 $ do
                -- Old address witness scheme
                xpub <- getByteString 64
                sig <- Hash <$> getByteString 64
                return $ PublicKeyWitness xpub sig

            2 -> isolate 64 $ do
                _sig <- Hash <$> getByteString 64
                error "unimplemented: New address witness scheme"

            3 -> isolate 68 $ do
                error "unimplemented: Account witness"
            other -> fail $ "Invalid witness type: " ++ show other


{-------------------------------------------------------------------------------
                            Common Structure
-------------------------------------------------------------------------------}

getTokenTransfer :: Get ([TxIn], [TxOut])
getTokenTransfer = do
    inCount <- fromIntegral <$> getWord8
    outCount <- fromIntegral <$> getWord8
    ins <- replicateM inCount getInput
    outs <- replicateM outCount getOutput
    return (ins, outs)
  where
    getInput = isolate 41 $ do
        -- NOTE: special value 0xff indicates account spending
        index <- fromIntegral <$> getWord8
        tx <- Hash <$> getByteString 32

        return $ TxIn tx index

    getOutput = do
        addr <- getAddress
        value <- Coin <$> getWord64be
        return $ TxOut addr value

    getAddress = do
        headerByte <- getWord8
        let kind = kindValue headerByte
        let _discrimination = discriminationValue headerByte
        case kind of
            -- Single Address
            0x3 -> Address <$> getByteString 32
            0x4 -> error "unimplemented group address decoder"
            0x5 -> error "unimplemented account address decoder"
            0x6 -> error "unimplemented multisig address decoder"
            other -> fail $ "Invalid address type: " ++ show other

    kindValue :: Word8 -> Word8
    kindValue = (.&. 0b01111111)

    discriminationValue :: Word8 -> Network
    discriminationValue b = case b .&. 0b10000000 of
        0 -> Mainnet
        _ -> Testnet

{-------------------------------------------------------------------------------
                            Config Parameters
-------------------------------------------------------------------------------}

data ConfigParam
    = Block0Date Word64
    -- ^ The official start time of the blockchain, in seconds since the Unix
    -- epoch.
    | Discrimination Network
    -- ^ Address discrimination. Testnet / Mainnet.
    | Consensus ConsensusVersion
    -- ^ Consensus version. BFT / Genesis Praos.
    | SlotsPerEpoch (Quantity "slot/epoch" Word32)
    -- ^ Number of slots in an epoch.
    | SlotDuration (Quantity "second/slot" Word8)
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
    | ConfigLinearFee LinearFee
    -- ^ Coefficients for fee calculations.
    | ProposalExpiration (Quantity "epoch" Word32)
    -- ^ Number of epochs until an update proposal expires.
    | KesUpdateSpeed (Quantity "second/update" Word32)
    -- ^ Maximum number of seconds per update for KES keys known by the system
    -- after start time.
    deriving (Eq, Show)

getConfigParam :: Get ConfigParam
getConfigParam = do
    -- The tag and the size/length of the config param is stored in a single
    -- @Word16@.
    --
    -- 6 least-significant bits: length
    -- 12 most-significant bits: tag
    taglen <- getWord16be
    let tag = taglen `shift` (-6)
    let len = fromIntegral $ taglen .&. (63) -- 0b111111

    isolate len $ case tag of
        1 -> Discrimination <$> getNetwork
        2 -> Block0Date <$> getWord64be
        3 -> Consensus <$> getConsensusVersion
        4 -> SlotsPerEpoch . Quantity <$> getWord32be
        5 -> SlotDuration . Quantity <$> getWord8
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

-- | Used to represent (>= 0) rational numbers as (>= 0) integers, by just
-- multiplying by 1000. For instance: '3.141592' is represented as 'Milli 3142'.
newtype Milli = Milli Word64
    deriving (Eq, Show)

newtype LeaderId = LeaderId ByteString
    deriving (Eq, Show)

data LinearFee = LinearFee
    { const :: Quantity "lovelace" Word64
    , perByte :: Quantity "lovelace/byte" Word64
    , perCert :: Quantity "lovelace/cert" Word64
    } deriving (Eq, Show)

data ConsensusVersion = BFT | GenesisPraos
    deriving (Eq, Show)

getConsensusVersion :: Get ConsensusVersion
getConsensusVersion = getWord16be >>= \case
    1 -> return BFT
    2 -> return GenesisPraos
    other -> fail $ "Unknown consensus version: " ++ show other

getNetwork :: Get Network
getNetwork = getWord8 >>= \case
    1 -> return Mainnet
    2 -> return Testnet
    other -> fail $ "Invalid network/discrimination value: " ++ show other

getMilli :: Get Milli
getMilli = Milli <$> getWord64be

getLeaderId :: Get LeaderId
getLeaderId = LeaderId <$> getByteString 32

getLinearFee :: Get LinearFee
getLinearFee = do
    const' <- Quantity <$> getWord64be
    perByte <- Quantity <$> getWord64be
    perCert <- Quantity <$> getWord64be
    return $ LinearFee const' perByte perCert


getBool :: Get Bool
getBool = getWord8 >>= \case
    1 -> return True
    0 -> return False
    other -> fail $ "Unexpected integer: " ++ show other
                ++ ". Expected a boolean 0 or 1."

{-------------------------------------------------------------------------------
                              Helpers
-------------------------------------------------------------------------------}

whileM :: Monad m => m Bool -> m a -> m [a]
whileM cond next = go
  where
    go = do
        c <- cond
        if c then do
            a <- next
            as <- go
            return (a : as)
        else return []
