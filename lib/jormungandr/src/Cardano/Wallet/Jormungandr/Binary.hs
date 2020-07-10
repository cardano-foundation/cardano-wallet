{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- The format is for the Shelley era as implemented by the Jörmungandr node.
-- It is described [here](https://github.com/input-output-hk/chain-libs/blob/master/chain-impl-mockchain/doc/format.abnf)
--
-- The module to some extent defines its own Jörmungandr-specific types,
-- different from "Cardano.Wallet.Primitive.Types". Here, transactions are just
-- one of many possible 'Fragment' that can be included in a block.
--
-- In some cases it also leads us to /throw exceptions/ when integers would
-- otherwise overflow (look for uses of 'toEnum').

module Cardano.Wallet.Jormungandr.Binary
    ( Block (..)
    , BlockHeader (..)
    , ConfigParam (..)
    , ConsensusVersion (..)
    , LeaderId (..)
    , Fragment (..)
    , FragmentSpec (..)
    , Milli (..)
    , PerCertificateFee (..)
    , TaxParameters (..)
    , getBlock
    , getBlockHeader
    , getBlockId
    , getFragment
    , getTransaction
    , overrideFeePolicy

    -- * Constructing Fragment
    , MkFragment (..)
    , StakeDelegationType (..)
    , putFragment
    , finalizeFragment
    , fragmentId

    -- * Transaction witnesses
    , TxWitnessTag (..)
    , putTxWitnessTag
    , getTxWitnessTag
    , txWitnessSize

    -- * Purification of chain block types
    , convertBlock
    , convertBlockHeader
    , poolRegistrationsFromBlock
    , rankingEpochConstants

    -- * Addresses
    , putAddress
    , getAddress

      -- * Helpers
    , whileM
    , maxNumberOfInputs
    , maxNumberOfOutputs

      -- * Re-export
    , Get
    , runGet
    , eitherRunGet
    , runGetOrFail
    , Put
    , runPut
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPrv, toXPub, xpubToBytes )
import Cardano.Pool.Jormungandr.Ranking
    ( EpochConstants (..), unsafeMkNonNegative, unsafeMkPositive )
import Cardano.Wallet.Jormungandr.Rewards
    ( PoolCapping (..)
    , RewardFormula (..)
    , RewardLimit (..)
    , RewardParams (..)
    , TaxParameters (..)
    , rewardsAt
    )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Passphrase (..) )
import Cardano.Wallet.Primitive.Fee
    ( FeePolicy (..) )
import Cardano.Wallet.Primitive.Types
    ( Address (..)
    , ChimericAccount (..)
    , Coin (..)
    , EpochNo (..)
    , Hash (..)
    , PoolId (PoolId)
    , PoolOwner (..)
    , SealedTx (..)
    , SlotId (..)
    , SlotInEpoch (..)
    , Tx (..)
    , TxIn (..)
    , TxOut (..)
    , unsafeEpochNo
    )
import Control.DeepSeq
    ( NFData )
import Control.Monad
    ( replicateM, unless, void )
import Control.Monad.Loops
    ( whileM )
import Control.Monad.Trans.Class
    ( lift )
import Control.Monad.Trans.Maybe
    ( MaybeT (..) )
import Crypto.Hash.Utils
    ( blake2b256 )
import Data.Binary.Get
    ( Get
    , bytesRead
    , getByteString
    , getLazyByteString
    , getWord16be
    , getWord32be
    , getWord64be
    , getWord8
    , isEmpty
    , isolate
    , label
    , lookAhead
    , lookAheadM
    , runGet
    , runGetOrFail
    , skip
    )
import Data.Binary.Get.Safe
    ( eitherRunGet )
import Data.Binary.Put
    ( Put, PutM, putByteString, putWord16be, putWord64be, putWord8, runPut )
import Data.Bits
    ( shift, (.&.) )
import Data.ByteString
    ( ByteString )
import Data.Either
    ( fromRight )
import Data.Functor
    ( ($>), (<&>) )
import Data.Maybe
    ( catMaybes, mapMaybe )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Quantity (..), mkPercentage )
import Data.Ratio
    ( Ratio, (%) )
import Data.Time.Clock
    ( NominalDiffTime, secondsToDiffTime )
import Data.Time.Clock.POSIX
    ( posixSecondsToUTCTime )
import Data.Word
    ( Word16, Word32, Word64, Word8 )
import GHC.Generics
    ( Generic )
import GHC.Stack
    ( HasCallStack )
import Safe
    ( headMay )

import qualified Cardano.Crypto.Wallet as CC
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

-- maximum number of inputs in a valid transaction
maxNumberOfInputs :: Int
maxNumberOfInputs = 255

-- maximum number of outputs in a valid transaction
--
-- See: https://github.com/input-output-hk/chain-libs/blob/8ab073b04580e1034f45c63e650fa1e18a90d90f/chain-impl-mockchain/src/transaction/builder.rs#L94
maxNumberOfOutputs :: Int
maxNumberOfOutputs = 254

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
    } deriving (Show, Eq, Generic)

instance NFData BlockHeader

data Block = Block
    { header :: BlockHeader
    , fragments :: [Fragment]
    } deriving (Show, Eq, Generic)

instance NFData Block

getBlockHeader :: Get BlockHeader
getBlockHeader = label "getBlockHeader" $ do
    size <- fromIntegral <$> getWord16be
    bytes <- lookAhead (getByteString size)
    isolate size $ do
        -- Common structure.
        version <- getWord16be
        contentSize <- getWord32be
        slotEpoch <- getEpochNo
        slotNo <- SlotInEpoch <$> getWord32be
        chainLength <- getWord32be
        contentHash <- Hash <$> getByteString 32
        parentHeaderHash <- Hash <$> getByteString 32
        let headerHash = Hash $ blake2b256 bytes
        -- Proof.
        -- There are three different types of proofs:
        -- 1. no proof (used for the genesis blockheader)
        -- 2. BFT
        -- 3. Praos / Genesis
        --
        -- We could make sure we get the right kind of proof, but we don't need
        -- to.  Just checking that the length is not totally wrong is much
        -- simpler and gives us sanity about the binary format being correct.
        remaining <- (size -) . fromIntegral <$> bytesRead
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
            _ -> fail $
                "BlockHeader proof has unexpected size " <> (show remaining)
        return $ BlockHeader
            { version
            , contentSize
            , slot = SlotId { epochNumber = slotEpoch, slotNumber = slotNo }
            , chainLength
            , contentHash
            , parentHeaderHash
            , headerHash
            , producedBy
            }

getBlock :: Get Block
getBlock = label "getBlock" $ do
    header <- getBlockHeader
    fragments <- isolate (fromIntegral $ contentSize header)
        (whileM (not <$> isEmpty) getFragment)
    return $ Block{header,fragments}

-- | Extract a 'Block' id from a serialized 'Block'.
getBlockId :: Get (Hash "BlockHeader")
getBlockId = lookAhead getBlock *> label "getBlockId" (do
    size <- getWord16be
    bytes <- getByteString (fromEnum size)
    return $ Hash $ blake2b256 bytes)

getEpochNo :: HasCallStack => Get EpochNo
getEpochNo = unsafeEpochNo <$> getWord32be

{-------------------------------------------------------------------------------
                           Fragments
-------------------------------------------------------------------------------}

-- | The block-body consists of fragments. There are several types of fragments.
data Fragment
    = Initial [ConfigParam]
    -- ^ Found in the genesis block.
    | Transaction Tx
    -- ^ A standard signed transaction
    | StakeDelegation (StakeDelegationType, ChimericAccount, Tx)
    -- ^ A signed transaction with stake pool delegation
    | PoolRegistration (PoolId, [PoolOwner], TaxParameters, Tx)
    -- ^ A signed transaction with a stake pool registration.
    | UnimplementedFragment Word8
    -- Fragments not yet supported go there.
    deriving (Generic, Eq, Show)

instance NFData Fragment

data FragmentSpec
    = FragmentInitial
    | FragmentLegacyUTxO
    | FragmentTransaction
    | FragmentDelegation

putFragmentSpec :: FragmentSpec -> Put
putFragmentSpec spec = do
    putWord8 0x00
    putWord8 (fragmentSpec spec)
  where
    fragmentSpec :: FragmentSpec -> Word8
    fragmentSpec = \case
        FragmentInitial -> 0
        FragmentLegacyUTxO -> 1
        FragmentTransaction -> 2
        FragmentDelegation -> 4

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

-- | Decode a fragment (header + contents).
--
-- Corresponds to FRAGMENT in the specification.
--
getFragment :: Get Fragment
getFragment = label "getFragment" $ do
    size <- fromIntegral <$> getWord16be

    -- We lazily compute the fragment-id, using look-ahead, before calling the
    -- specialized decoders.
    --
    -- The fragment-id is needed, for instance, to construct a @Tx@, where it
    -- corresponds to the txId (a.k.a "tx hash").
    fragId <- Hash . blake2b256 . BL.toStrict
        <$> lookAhead (getLazyByteString $ fromIntegral size)

    -- A null byte for later extension
    _nullByte <- getWord8
    fragSpec <- getWord8
    let remaining = size - 2
    let unimpl = skip remaining >> return (UnimplementedFragment fragSpec)
    let typeLabelStr = "fragmentType " ++ show fragSpec

    label typeLabelStr $ isolate remaining $ case fragSpec of
        0  -> Initial <$> getInitial
        1  -> Transaction <$> getLegacyTransaction fragId
        2  -> Transaction <$> getTransaction fragId
        3  -> unimpl -- OwnerStakeDelegation
        4  -> lookAheadM (getStakeDelegation fragId) >>= maybe unimpl (pure . StakeDelegation)
        5  -> PoolRegistration <$> getPoolRegistration fragId
        6  -> unimpl -- PoolRetirement
        7  -> unimpl -- PoolUpdate
        8  -> unimpl -- UpdateProposal
        9  -> unimpl -- UpdateVote
        10 -> unimpl -- VotePlan
        11 -> unimpl -- VoteCast
        12 -> unimpl -- VoteTally
        other -> fail $ "Unexpected content type tag " ++ show other

-- | Decode the contents of a @Initial@-fragment.
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

data StakeDelegationType
    = DlgNone
    | DlgFull PoolId
    | DlgRatio
    deriving (Generic, Show, Eq)

instance NFData StakeDelegationType

stakeDelegationTypeTag :: StakeDelegationType -> Word8
stakeDelegationTypeTag = \case
    DlgNone{}  -> 0
    DlgFull{}  -> 1
    DlgRatio{} -> 2

-- | Decode the contents of a @Transaction@-fragment carrying a delegation cert.
--
-- Corresponds to STAKE-DELEGATION in the specification.
--
-- Returns 'Nothing' for unsupported stake delegation types: DLG-RATIO
getStakeDelegation
    :: Hash "Tx"
    -> Get (Maybe (StakeDelegationType, ChimericAccount, Tx))
getStakeDelegation tid = do
    label "getStakeDelegation" $ runMaybeT $ do
        accId <- lift $ getByteString 32
        dlgType <- lift getWord8 >>= \case
            0 -> getStakeDelegationNone
            1 -> getStakeDelegationFull
            _ -> getStakeDelegationRatio
        tx <- lift $ getGenericTransaction tid
        _accSignature <- lift $ getByteString 65
        pure
            ( dlgType
            , ChimericAccount accId
            , tx
            )
  where
    getStakeDelegationNone = MaybeT $ do
        pure $ Just DlgNone
    getStakeDelegationFull = MaybeT $ do
        poolId <- getPoolId
        pure $ Just $ DlgFull poolId
    getStakeDelegationRatio =
        MaybeT (pure Nothing)

-- | Corresponds to @POOL-ID@ in the specification.
getPoolId :: Get PoolId
getPoolId = PoolId <$> getByteString 32

-- | Decode the contents of a @Transaction@-fragment.
getTransaction :: Hash "Tx" -> Get Tx
getTransaction = label "getTransaction" . getGenericTransaction

getGenericTransaction
    :: Hash "Tx"
    -> Get Tx
getGenericTransaction tid = label "getGenericTransaction" $ do
    (ins, outs) <- getTokenTransfer
    let witnessCount = length ins
    _wits <- replicateM witnessCount getWitness
    return $ Tx tid ins outs
  where
    getWitness :: Get ByteString
    getWitness = do
        tag <- lookAhead getTxWitnessTag
        let len = txWitnessSize tag + txWitnessTagSize
        getByteString len

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
getLegacyTransaction :: Hash "Tx" -> Get Tx
getLegacyTransaction tid = do
    n <- fromIntegral <$> getWord8
    outs <- replicateM n $ do
        coin <- Coin <$> getWord64be
        addr <- getLegacyAddress
        pure (TxOut addr coin)
    -- NOTE
    -- Legacy transactions only show up in the genesis block and are treated as
    -- coinbase transactions with no inputs.
    let inps = mempty
    pure $ Tx tid inps outs

data MkFragment
    = MkFragmentSimpleTransaction
        TxWitnessTag
    | MkFragmentStakeDelegation
        TxWitnessTag
        StakeDelegationType
        ChimericAccount
        (XPrv, Passphrase "encryption")

getPoolRegistration :: Hash "Tx" -> Get (PoolId, [PoolOwner], TaxParameters, Tx)
getPoolRegistration tid = label "POOL-REGISTRATION" $ do
    (poolId, ownerAccIds, taxes) <- getPoolRegistrationCert
    tx <- getGenericTransaction tid
    _sig <- getPoolSig
    pure (poolId, ownerAccIds, taxes, tx)

getPoolRegistrationCert :: Get (PoolId, [PoolOwner], TaxParameters)
getPoolRegistrationCert = do
    (certBytes, (ownerAccIds, taxes)) <- withRaw getCert
    -- A PoolId is the blake2b-256 hash of the encoded registration certificate.
    let poolId = PoolId $ blake2b256 certBytes
    pure (poolId, ownerAccIds, taxes)

  where
    getCert :: Get ([PoolOwner], TaxParameters)
    getCert = label "REGISTRATION-CERT" $ do
        -- A random value, for user purpose similar to a UUID.  it may not be
        -- unique over a blockchain, so shouldn't be used a unique identifier
        _serial <- label "POOL-SERIAL" $ skip 16
        -- Beginning of validity for this pool. A time in seconds since genesis.
        -- This is used to keep track of the period of the expected key and the
        -- expiry
        _startTime <- label "TIME-SINCE-EPOCH0" getWord64be
        -- Permission system for this pool.
        label "POOL-PERMISSIONS" $ skip 8
        -- POOL-KEYS = VRF-PUBLICKEY (32 bytes) KES-PUBLICKEY (32 bytes)
        label "POOL-KEYS" $ do
            label "VRF-PUBLICKEY" $ skip 32
            label "KES-PUBLICKEY" $ skip 32
        -- Owners of this pool, between 1 and 31 of them.
        ownerAccIds <- label "POOL-OWNERS" $ do
            numOwners <- fromIntegral <$> getWord8
            label (show numOwners ++ " owner(s)") $
                replicateM numOwners getSingleAccountId
        -- Operators of this pool, between 0 and 3 of them.
        _operatorAccIds <- label "POOL-OPERATORS" $ do
            numOperators <- fromIntegral <$> getWord8
            label (show numOperators ++ " operator(s)") $
                replicateM numOperators getSingleAccountId
        -- Reward tax type -- fixed, ratio numerator, denominator, and optional
        -- limit value
        taxes <- label "POOL-REWARD-SCHM" getTaxParameters
        _rewardAcc <- label "POOL-REWARD-ACNT" getAccountIdMaybe
        pure ( PoolOwner <$> ownerAccIds
             , taxes
             )

    -- | Corresponds to @POOL-REWARD-ACNT = %x00 / ACCOUNT-ID@
    getAccountIdMaybe :: Get (Maybe ByteString)
    getAccountIdMaybe = do
        tag <- lookAhead getWord8
        if tag == 0 then (skip 1 $> Nothing) else Just <$> getAccountId

    -- | Corresponds to @ACCOUNT-ID@
    getAccountId :: Get ByteString
    getAccountId = label "ACCOUNT-ID" $ getWord8 >>= \case
        1 -> getSingleAccountId
        2 -> getMultiAccountId
        tag -> fail $ "Invalid tag " ++ show tag

    -- @SINGLE-ACNT-ID@ is a ed25519 public key (32 bytes)
    getSingleAccountId = getByteString 32
    -- @MULTI-ACNT-ID@ is 32 octets of "todo"
    getMultiAccountId = getByteString 32

-- | Get a pool signature, which is either a single signature from a pool
-- operator, or signatures from the pool owners (referenced by their index in
-- the registration certificate). Corresponds to
-- @POOL-SIG = OWNERS-SIG / OP-SIGNATURE@ in the specification.
getPoolSig :: Get (Either [(Word8, ByteString)] ByteString)
getPoolSig = label "getPoolSig" $ getWord8 >>= \case
    0 -> Right <$> getSingleAcntSig -- pool operator signature
    numSigs -> Left <$> replicateM (fromIntegral numSigs) getOwnersSig
  where
    getOwnersSig = (,) <$> getWord8 <*> getSingleAcntSig
    -- @SINGLE-ACNT-SIG@ is an ed25519 signature
    getSingleAcntSig = getByteString 64

putFragment
    :: Hash "Genesis"
    -> [((TxIn, Coin), (XPrv, Passphrase "encryption"))]
    -> [TxOut]
    -> MkFragment
    -> PutM (Proxy "Fragment")
putFragment (Hash block0Hash) inputs outputs = \case
    -- SIMPLE-TRANSACTION = TRANSACTION
    MkFragmentSimpleTransaction witTag ->
        withHeader FragmentTransaction $ do
            putTransaction (mempty :: Put) witTag

    -- STAKE-DELEGATION = DLG-CERT TRANSACTION STAKE-AUTH
    MkFragmentStakeDelegation witTag dlgTag account credentials ->
        withHeader FragmentDelegation $ do
            putPayload  <- returnPut $ putDelegationCertificate account dlgTag
            putAuthData <- returnPut $ putTransaction putPayload witTag
            putStakeAuthentication (putPayload >> putAuthData) credentials
  where
    -- TRANSACTION =
    --     SIZE-ELEMENT-8BIT ; number of inputs
    --     SIZE-ELEMENT-8BIT ; number of outputs
    --     *INPUT            ; as many as indicated in the number of inputs
    --     *OUTPUT           ; sa many as indicated in the number of outputs
    --     *WITNESS          ; as many as indicated in the number of inputs
    putTransaction putPayload witTag = do
        putInputsOutputs
        mapM_ (putWitness putPayload witTag) (snd <$> inputs)

    putInputsOutputs = do
        guardLength (<= maxNumberOfInputs) inputs
        guardLength (<= maxNumberOfOutputs) outputs
        putWord8 $ toEnum $ length inputs
        putWord8 $ toEnum $ length outputs
        mapM_ putInput (fst <$> inputs)
        mapM_ putOutput outputs

    -- INPUT             = INPUT-UTXO / INPUT_ACCOUNT
    -- INPUT-UTXO        = IDX VALUE FRAGMENT-ID
    -- INPUT-ACCOUNT     = %xff VALUE UNTAG-ACCOUNT-ID
    -- IDX               = %x00-fe
    putInput (TxIn inputId inputIx, inputValue) = do
        -- TODO invariant about inputIx /= 0xff
        putWord8 . toEnum . fromEnum $ inputIx
        putWord64be $ getCoin inputValue
        putByteString $ getHash inputId

    -- OUTPUT            = ADDRESS VALUE
    putOutput (TxOut outputAddr outputValue) = do
        putAddress outputAddr
        putWord64be $ getCoin outputValue

    -- WITNESS           = WITNESS-OLDUTXO / WITNESS-UTXO / WITNESS-ACCOUNT / WITNESS-MULTISIG
    -- WITNESS-OLDUTXO   = %x00 LEGACY-XPUB LEGACY-SIGNATURE
    -- WITNESS-UTXO      = %x01 ED25519-SIGNATURE
    -- WITNESS-ACCOUNT   = %x02 SINGLE-ACNT-SIG
    -- WITNESS-MULTISIG  = %x03 MULTI-ACNT-SIG
    putWitness (putPayload :: Put) tag (xprv, Passphrase pwd) = case tag of
        TxWitnessUTxO -> do
            putTxWitnessTag tag
            putByteString $ CC.unXSignature $ CC.sign pwd xprv msg
        TxWitnessLegacyUTxO -> do
            putTxWitnessTag tag
            putByteString $ xpubToBytes $ toXPub xprv
            putByteString $ CC.unXSignature $ CC.sign pwd xprv msg
        TxWitnessAccount ->
            error "putWitness: TxWitnessAccount: not implemented"
        TxWitnessMultisig ->
            error "putWitness: TxWitnessMultisig: not implemented"
      where
        msg = mconcat
            [ BL.toStrict $ runPut (putTxWitnessTag tag)
            , block0Hash
            , blake2b256 $ BL.toStrict $ runPut $ do
                putPayload
                putInputsOutputs
            ]

    -- DLG-CERT         = UNTAG-ACCOUNT-ID DLG-TYPE
    -- DLG-TYPE         = DLG-NONE / DLG-FULL / DLG-RATIO
    -- DLG-NONE         = %x00
    -- DLG-FULL         = %x01 POOL-ID
    -- DLG-RATIO        = %x02-FF %x02-08 2*8DLG-RATIO-POOL
    -- DLG-RATIO-POOL   = %x01-FF POOL-ID
    -- POOL-ID          = 32OCTET
    putDelegationCertificate (ChimericAccount accountId) = \case
        tag@DlgNone -> do
            putByteString accountId
            putWord8 (stakeDelegationTypeTag tag)
        tag@(DlgFull (PoolId poolId)) -> do
            putByteString accountId
            putWord8 (stakeDelegationTypeTag tag)
            putByteString poolId
        DlgRatio ->
            error "putDelegationCertificate: DlgCertRatio: not implemented"

    -- STAKE-AUTH       = ACCOUNT-SIG
    -- ACCOUNT-SIG      = %x01 SINGLE-ACNT-SIG
    -- SINGLE-ACNT-SIG  = ED25519-SIGNATURE
    putStakeAuthentication putAuthData (xprv, Passphrase pwd) = do
        putWord8 1
        putByteString $ CC.unXSignature $ CC.sign pwd xprv msg
      where
        msg = BL.toStrict $ runPut putAuthData

finalizeFragment :: PutM (Proxy "Fragment") -> SealedTx
finalizeFragment = SealedTx . BL.toStrict . runPut . void

fragmentId :: PutM (Proxy "Fragment") -> Hash "Tx"
fragmentId put =
    -- NOTE:
    -- The fragment id is a hash of the fragment, minus the size (first 2 bytes)
    Hash $ blake2b256 $ BS.drop 2 bytes
  where bytes = BL.toStrict $ runPut $ void put

{-------------------------------------------------------------------------------
                            Config Parameters
-------------------------------------------------------------------------------}

data ConfigParam
    = Block0Date W.StartTime
    -- ^ The official start time of the blockchain, in seconds since the Unix
    -- epoch.
    | ConfigDiscrimination Discrimination
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
    | ConfigPerCertificate PerCertificateFee
    -- ^ Per certificate fees, override the 'certificate' fee of the linear fee
    | ConfigRewardFormula RewardFormula
    -- ^ Reward Parameters.
    | TreasuryTax TaxParameters
    -- ^ Treasury tax parameters.
    | ConfigRewardLimit RewardLimit
    -- ^ limit the epoch total reward drawing limit to a portion of the total
    -- active stake of the system.
    | ConfigPoolCapping PoolCapping
    -- ^ settings to incentivize the numbers of stake pool to be registered on
    -- the blockchain.
    | UnimplementedConfigParam  Word16
    deriving (Generic, Eq, Show)

instance NFData ConfigParam

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
    let unimpl = skip len >> return (UnimplementedConfigParam tag)
    let lbl = "tag: " <> show tag <> "(" <> show len <> " bytes)"
    label lbl $ isolate len $ case tag of
        1 -> ConfigDiscrimination <$> getDiscrimination
        2 -> Block0Date . W.StartTime . posixSecondsToUTCTime . fromIntegral
            <$> getWord64be
        3 -> Consensus <$> getConsensusVersion
        4 -> SlotsPerEpoch . W.EpochLength <$> getWord32be
        5 -> SlotDuration . secondsToNominalDiffTime <$> getWord8
        6 -> EpochStabilityDepth . Quantity <$> getWord32be
        8 -> ConsensusGenesisPraosParamF <$> getMilli
        9 -> MaxNumberOfTransactionsPerBlock <$> getWord32be
        10 -> BftSlotsRatio <$> getMilli
        11 -> AddBftLeader <$> getLeaderId
        12 -> RemoveBftLeader <$> getLeaderId
        13 -> fail $ "Obsolete config param with tag " ++ show tag
        14 -> ConfigLinearFee <$> getLinearFee
        15 -> ProposalExpiration . Quantity <$> getWord32be
        16 -> KesUpdateSpeed . Quantity <$> getWord32be
        17 -> unimpl -- treasury initial value
        18 -> TreasuryTax <$> getTaxParameters
        19 -> unimpl -- reward pot
        20 -> ConfigRewardFormula <$> getRewardFormula
        21 -> ConfigPerCertificate <$> getPerCertificateFee
        22 -> unimpl -- fees in treasury
        23 -> pure (ConfigRewardLimit RewardLimitNone)
        24 -> ConfigRewardLimit . RewardLimitByAbsoluteStake <$> getRatio
        25 -> ConfigPoolCapping <$> getPoolCapping
        26 -> unimpl -- add committee id
        27 -> unimpl -- remove committee id
        28 -> unimpl -- per vote certificate fee
        other -> fail $ "Invalid config param with tag " ++ show other
  where
    -- NOTE
    -- Conversion between 'DiffTime' <-> 'NominalDiffTime' is safe here because
    -- the duration can't never be more than 255 seconds, so the leap second
    -- is never involved.
    secondsToNominalDiffTime :: Word8 -> NominalDiffTime
    secondsToNominalDiffTime =
        toEnum . fromEnum . secondsToDiffTime .  fromIntegral

getRatio :: Get (Ratio Word64)
getRatio = do
    ratioNum <- getWord64be
    ratioDen <- getWord64be
    return $ ratioNum % ratioDen

getTaxParameters :: Get TaxParameters
getTaxParameters = do
    taxFixed <- getWord64be
    taxRatio <- getRatio
    taxLimit <- getWord64be <&> \case
        0 -> Nothing
        n -> Just n
    pure $ TaxParameters { taxFixed, taxRatio, taxLimit }

getRewardFormula :: Get RewardFormula
getRewardFormula = do
    tag <- getWord8
    case tag of
        1 -> LinearFormula <$> getRewardParams
        2 -> HalvingFormula <$> getRewardParams
        _ -> fail $ "Unknown reward parameters with tag " ++ show tag
  where
    -- NOTE: getHalvingParams and getLinearParams are currently identical.
    --
    -- https://github.com/input-output-hk/chain-libs/blob/149e01cf61a9eef2660a6944fe0acc73d490e70f/chain-impl-mockchain/src/config.rs#L404-L442
    getRewardParams :: Get RewardParams
    getRewardParams = do
        rFixed <- getWord64be
        rRatio <- getRatio
        rEpochStart <- getWord32be
        rEpochRate <- getWord32be
        return $ RewardParams { rFixed, rRatio, rEpochStart, rEpochRate }

getPoolCapping :: Get PoolCapping
getPoolCapping = do
    minParticipation <- getWord32be
    maxParticipation <- getWord32be
    pure PoolCapping{minParticipation,maxParticipation}

data Discrimination
    = Production
    | Testing
    deriving (Generic, Eq, Show)

instance NFData Discrimination

-- | Used to represent (>= 0) rational numbers as (>= 0) integers, by just
-- multiplying by 1000. For instance: '3.141592' is represented as 'Milli 3142'.
newtype Milli = Milli Word64
    deriving (Generic, Eq, Show)

instance NFData Milli

newtype LeaderId = LeaderId ByteString
    deriving (Generic, Eq, Show)

instance NFData LeaderId

data ConsensusVersion = BFT | GenesisPraos
    deriving (Generic, Eq, Show)

instance NFData ConsensusVersion

data PerCertificateFee = PerCertificateFee
    { feePoolRegistration :: Word64
    , feeStakeDelegation :: Word64
    , feeOwnerStakeDelegation :: Word64
    } deriving (Generic, Eq, Show)

instance NFData PerCertificateFee

getConsensusVersion :: Get ConsensusVersion
getConsensusVersion = label "getConsensusVersion" $ getWord16be >>= \case
    1 -> return BFT
    2 -> return GenesisPraos
    other -> fail $ "Unknown consensus version: " ++ show other

getDiscrimination :: Get Discrimination
getDiscrimination = label "getDiscrimination" $ getWord8 >>= \case
    1 -> return Production
    2 -> return Testing
    other -> fail $ "Invalid network/discrimination value: " ++ show other

getMilli :: Get Milli
getMilli = label "getMilli" $ Milli <$> getWord64be

getLeaderId :: Get LeaderId
getLeaderId = label "getLeaderId" $ LeaderId <$> getByteString 32

getLinearFee :: Get FeePolicy
getLinearFee = label "getFeePolicy" $ do
    a <- getWord64be
    b <- getWord64be
    c <- getWord64be
    return $ LinearFee
        (Quantity $ double a)
        (Quantity $ double b)
        (Quantity $ double c)
  where
    double = fromRational . toRational

getPerCertificateFee :: Get PerCertificateFee
getPerCertificateFee = label "getPerCertificateFee" $ do
    feePoolRegistration <- getWord64be
    feeStakeDelegation <- getWord64be
    feeOwnerStakeDelegation <- getWord64be
    pure $ PerCertificateFee
        { feePoolRegistration
        , feeStakeDelegation
        , feeOwnerStakeDelegation
        }

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

guardLength :: (Int -> Bool) -> [a] -> Put
guardLength predicate xs = do
    unless (predicate len) $ fail $ error $ unwords
        [ "Invariant violation. Invalid list length:"
        , show len
        ]
  where
    len = length xs

-- | Add a corresponding header to a fragment. Every fragment is encoded as:
--
-- FRAGMENT         = FRAGMENT-SIZE %x00 FRAGMENT-SPEC
-- FRAGMENT-SIZE    = SIZE-BYTES-16BIT
-- FRAGMENT-SPEC    = %x00 INITIAL
--                  / %x01 OLD-UTXO-DECL
--                  / %x02 SIMPLE-TRANSACTION
--                  / %x03 OWNER-STAKE-DELEGATION
--                  / %x04 STAKE-DELEGATION
--                  / %x05 POOL-REGISTRATION
--                  / %x06 POOL-RETIREMENT
--                  / %x07 POOL-UPDATE
--                  / %x08 UPDATE-PROPOSAL
--                  / %x09 UPDATE-VOTE
withHeader :: FragmentSpec -> Put -> PutM (Proxy "Fragment")
withHeader spec content = do
    let bs = BL.toStrict $ runPut (putFragmentSpec spec *> content)
    putWord16be (toEnum $ BS.length bs)
    putByteString bs
    pure Proxy

-- | Write given serializer and returns it.
returnPut :: Put -> PutM Put
returnPut putData = putData $> putData

-- | Run a decoder and return its result, along with the raw bytes that encoded
-- this result.
withRaw :: Get a -> Get (ByteString, a)
withRaw get = do
    start <- fromIntegral <$> bytesRead
    (end, a) <- lookAhead $ do
        a <- get
        end <- fromIntegral <$> bytesRead
        pure (end, a)
    -- After decoding once, go back and get the raw bytes.
    raw <- getByteString (end - start)
    pure (raw, a)

{-------------------------------------------------------------------------------
                                Conversions
-------------------------------------------------------------------------------}

-- | Convert the Jörmungandr binary format block into a simpler Wallet block.
convertBlock :: Block -> W.Block
convertBlock (Block h msgs) =
    W.Block (convertBlockHeader h) transactions delegations
  where
    delegations :: [W.DelegationCertificate]
    delegations = catMaybes $ msgs >>= \case
        Initial _ -> []
        Transaction _ -> []
        StakeDelegation (dlgType, accountId, _tx) ->
            [convertDlgType accountId dlgType]
        PoolRegistration _ -> []
        UnimplementedFragment _ -> []
    transactions :: [Tx]
    transactions = msgs >>= \case
        Initial _ -> []
        Transaction tx -> return tx
        StakeDelegation (_poolId, _accountId, tx) -> return tx
        PoolRegistration (_poolId, _owners, _taxes, tx) -> return tx
        UnimplementedFragment _ -> []

-- | Convert the Jörmungandr binary format to a simpler form
convertDlgType
    :: ChimericAccount
    -> StakeDelegationType
    -> Maybe W.DelegationCertificate
convertDlgType accountId = \case
    DlgNone -> Just $ W.CertDelegateNone accountId
    DlgFull poolId -> Just $ W.CertDelegateFull accountId poolId
    _ -> Nothing

-- | Convert the Jörmungandr binary format header into a simpler Wallet header.
convertBlockHeader :: BlockHeader -> W.BlockHeader
convertBlockHeader h = (W.BlockHeader (slot h) (bh h) (Hash "") (Hash ""))
    { W.headerHash = headerHash h
    , W.parentHeaderHash = parentHeaderHash h
    }
  where
    bh = Quantity . fromIntegral . chainLength


-- | Take an existing fee policy and override the 'certificate' using the a
-- given per certificate policy.
--
-- This relies on the assumption that the certificate in the 'FeePolicy' is
-- only interpret as a 'stake delegation' certificate.
overrideFeePolicy :: FeePolicy -> PerCertificateFee -> FeePolicy
overrideFeePolicy linearFee@(LinearFee a b _) override =
    -- FIXME
    -- If the configuration option is not set, Jörmungandr still yields an extra
    -- configuration fragment where all fees are set to 0.
    -- This is true in 0.8.0-rc1, might no longer be true for subsequent
    -- releases.
    if all (== 0) [feeDlg, feeReg, feeOwn]
    then linearFee
    else LinearFee a b (Quantity $ fromIntegral feeDlg)
  where
    feeDlg = feeStakeDelegation override
    feeReg = feePoolRegistration override
    feeOwn = feeOwnerStakeDelegation override

-- | Extracts ownership information from all stake pool registration
-- certificates in the Jörmungandr block.
poolRegistrationsFromBlock :: Block -> [W.PoolRegistrationCertificate]
poolRegistrationsFromBlock (Block _hdr fragments) = do
    PoolRegistration (poolId, owners, taxes, _tx) <- fragments
    let margin = fromRight maxBound $ mkPercentage $ toRational $ taxRatio taxes
    let cost = Quantity (taxFixed taxes)
    let dummyPledge = Quantity 0
    pure $ W.PoolRegistrationCertificate poolId owners margin cost dummyPledge Nothing

-- | If all incentives parameters are present in the blocks, returns a function
-- that computes reward based on a given epoch.
-- Returns 'Nothing' otherwise.
rankingEpochConstants
    :: Block
    -> Maybe (EpochNo -> Quantity "lovelace" Word64 -> EpochConstants)
rankingEpochConstants block0 = do
    poolCapping   <- mPoolCapping
    rewardLimit   <- mRewardLimit
    treasuryTax   <- mTreasuryTax
    rewardFormula <- mRewardFormula
    pure $ \ep totalStake -> EpochConstants
        { leaderStakeInfluence =
            unsafeMkNonNegative 0
        , desiredNumberOfPools =
            unsafeMkPositive $ fromIntegral $ maxParticipation poolCapping
        , totalRewards =
            rewardsAt (rewardLimit, totalStake) treasuryTax ep rewardFormula
        }
  where
    params = mconcat $ mapMaybe matchConfigParameters (fragments block0)
      where
        matchConfigParameters = \case
            Initial xs -> Just xs
            _ -> Nothing

    mPoolCapping = headMay $ mapMaybe matchPoolCapping params
      where
        matchPoolCapping = \case
            ConfigPoolCapping x -> Just x
            _ -> Nothing

    mRewardLimit = headMay $ mapMaybe matchRewardLimit params
      where
        matchRewardLimit = \case
            ConfigRewardLimit x -> Just x
            _ -> Nothing

    mTreasuryTax = headMay $ mapMaybe matchTreasuryTax params
      where
        matchTreasuryTax = \case
            TreasuryTax x -> Just x
            _ -> Nothing

    mRewardFormula = headMay $ mapMaybe matchRewardFormula params
      where
        matchRewardFormula = \case
            ConfigRewardFormula x -> Just x
            _ -> Nothing
