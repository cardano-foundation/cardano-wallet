{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright: © 2020 IOHK
-- License: Apache-2.0
--
-- Working with Shelley transactions.

module Cardano.Wallet.Shelley.Transaction
    ( newTransactionLayer

    -- * Internals
    , _minimumFee
    , _decodeSignedTx
    , _estimateMaxNumberOfInputs
    , mkUnsignedTx
    , mkShelleyWitness
    , mkByronWitness
    , mkTx
    , TxPayload (..)
    , emptyTxPayload
    , TxWitnessTagFor (..)
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPrv, toXPub )
import Cardano.Api.Typed
    ( AnyCardanoEra (..)
    , ByronEra
    , CardanoEra (..)
    , IsShelleyBasedEra
    , NetworkId
    , SerialiseAsCBOR (..)
    , ShelleyBasedEra (..)
    )
import Cardano.Binary
    ( ToCBOR, serialize' )
import Cardano.Crypto.Wallet
    ( XPub )
import Cardano.Ledger.Crypto
    ( DSIGN )
import Cardano.Ledger.Era
    ( Crypto, Era )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..), Passphrase (..), RewardAccount (..), WalletKey (..) )
import Cardano.Wallet.Primitive.AddressDerivation.Byron
    ( ByronKey )
import Cardano.Wallet.Primitive.AddressDerivation.Icarus
    ( IcarusKey )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( ShelleyKey, toRewardAccountRaw )
import Cardano.Wallet.Primitive.CoinSelection
    ( CoinSelection (..), feeBalance )
import Cardano.Wallet.Primitive.Fee
    ( Fee (..), FeePolicy (..) )
import Cardano.Wallet.Primitive.Types
    ( PoolId (..) )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..) )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Cardano.Wallet.Primitive.Types.Tx
    ( SealedTx (..), Tx (..), TxIn (..), TxMetadata, TxOut (..) )
import Cardano.Wallet.Shelley.Compatibility
    ( AllegraEra
    , ShelleyEra
    , fromAllegraTx
    , fromShelleyTx
    , sealShelleyTx
    , toAllegraTxOut
    , toCardanoLovelace
    , toCardanoStakeCredential
    , toCardanoTxIn
    , toHDPayloadAddress
    , toShelleyTxOut
    , toStakeKeyDeregCert
    , toStakeKeyRegCert
    , toStakePoolDlgCert
    )
import Cardano.Wallet.Transaction
    ( DelegationAction (..)
    , ErrDecodeSignedTx (..)
    , ErrMkTx (..)
    , TransactionLayer (..)
    )
import Control.Arrow
    ( left )
import Control.Monad
    ( forM )
import Data.ByteString
    ( ByteString )
import Data.Quantity
    ( Quantity (..) )
import Data.Type.Equality
    ( type (==) )
import Data.Word
    ( Word16, Word64, Word8 )
import Ouroboros.Network.Block
    ( SlotNo )

import qualified Cardano.Api.Typed as Cardano
import qualified Cardano.Chain.Common as Byron
import qualified Cardano.Crypto as CC
import qualified Cardano.Crypto.DSIGN as DSIGN
import qualified Cardano.Crypto.Hash.Class as Crypto
import qualified Cardano.Crypto.Wallet as Crypto.HD
import qualified Cardano.Ledger.Core as SL
import qualified Cardano.Wallet.Primitive.CoinSelection as CS
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Write as CBOR
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Shelley.Spec.Ledger.Address.Bootstrap as SL

-- | Type encapsulating what we need to know to add things -- payloads,
-- certificates -- to a transaction.
--
-- Designed to allow us to have /one/ @mkTx@ which doesn't care whether we
-- include certificates or not.
data TxPayload era = TxPayload
    { _metadata :: Maybe Cardano.TxMetadata
      -- ^ User or application-defined metadata to be included in the
      -- transaction.

    , _certificates :: [Cardano.Certificate]
      -- ^ Certificates to be included in the transactions.

    , _extraWitnesses :: Cardano.TxBody era -> [Cardano.Witness era]
      -- ^ Create payload-specific witesses given the unsigned transaction body.
      --
      -- Caller has the freedom and responsibility to provide the correct
      -- witnesses for what they're trying to do.
    }

emptyTxPayload :: TxPayload c
emptyTxPayload = TxPayload Nothing mempty mempty

stdTxPayload :: Maybe TxMetadata -> TxPayload c
stdTxPayload md = TxPayload md mempty mempty

data TxWitnessTag
    = TxWitnessByronUTxO WalletStyle
    | TxWitnessShelleyUTxO
    deriving (Show, Eq)

data WalletStyle
    = Icarus
    | Byron
    deriving (Show, Eq)

type EraConstraints era =
    ( IsShelleyBasedEra era
    , ToCBOR (SL.TxBody (Cardano.ShelleyLedgerEra era))
    , Era (Cardano.ShelleyLedgerEra era)
    , DSIGN (Crypto (Cardano.ShelleyLedgerEra era)) ~ DSIGN.Ed25519DSIGN
    , (era == ByronEra) ~ 'False
    )

-- | Provide a transaction witness for a given private key. The type of witness
-- is different between types of keys and, with backward-compatible support, we
-- need to support many types for one backend target.
class TxWitnessTagFor (k :: Depth -> * -> *) where
    txWitnessTagFor :: TxWitnessTag

instance TxWitnessTagFor ShelleyKey where
    txWitnessTagFor = TxWitnessShelleyUTxO

instance TxWitnessTagFor IcarusKey where
    txWitnessTagFor = TxWitnessByronUTxO Icarus

instance TxWitnessTagFor ByronKey where
    txWitnessTagFor = TxWitnessByronUTxO Byron


mkTx
    :: forall k era.
        ( TxWitnessTagFor k
        , WalletKey k
        , EraConstraints era
        )
    => Cardano.NetworkId
    -> TxPayload era
    -> SlotNo
    -- ^ Slot at which the transaction will expire.
    -> (XPrv, Passphrase "encryption")
    -- ^ Reward account
    -> (Address -> Maybe (k 'AddressK XPrv, Passphrase "encryption"))
    -> CoinSelection
    -> ShelleyBasedEra era
    -> Either ErrMkTx (Tx, SealedTx)
mkTx networkId payload expirySlot (rewardAcnt, pwdAcnt) keyFrom cs era = do
    let TxPayload md certs mkExtraWits = payload
    let wdrls = mkWithdrawals
            networkId
            (toRewardAccountRaw . toXPub $ rewardAcnt)
            (withdrawal cs)

    unsigned <- mkUnsignedTx era expirySlot cs md wdrls certs

    wits <- case (txWitnessTagFor @k) of
        TxWitnessShelleyUTxO -> do
            addrWits <- forM (CS.inputs cs) $ \(_, TxOut addr _) -> do
                (k, pwd) <- lookupPrivateKey keyFrom addr
                pure $ mkShelleyWitness unsigned (getRawKey k, pwd)

            let wdrlsWits
                    | null wdrls = []
                    | otherwise =
                      [mkShelleyWitness unsigned (rewardAcnt, pwdAcnt)]

            pure $ mkExtraWits unsigned <> addrWits <> wdrlsWits

        TxWitnessByronUTxO{} -> do
            bootstrapWits <- forM (CS.inputs cs) $ \(_, TxOut addr _) -> do
                (k, pwd) <- lookupPrivateKey keyFrom addr
                pure $ mkByronWitness unsigned networkId addr (getRawKey k, pwd)
            pure $ bootstrapWits <> mkExtraWits unsigned

    let signed = Cardano.makeSignedTransaction wits unsigned
    case era of
        ShelleyBasedEraShelley -> Right $ sealShelleyTx fromShelleyTx signed
        ShelleyBasedEraAllegra -> Right $ sealShelleyTx fromAllegraTx signed
        ShelleyBasedEraMary    -> Left  $ ErrInvalidEra (AnyCardanoEra MaryEra)

newTransactionLayer
    :: forall k.
        ( TxWitnessTagFor k
        , WalletKey k
        )
    => NetworkId
    -> TransactionLayer k
newTransactionLayer networkId = TransactionLayer
    { mkStdTx = \era acc ks tip md cs ->
        withShelleyBasedEra era $ mkTx networkId (stdTxPayload md) tip acc ks cs
    , initDelegationSelection =
        _initDelegationSelection
    , mkDelegationJoinTx = \era poolId acc ks ttl cs ->
        withShelleyBasedEra era $ _mkDelegationJoinTx poolId acc ks ttl cs
    , mkDelegationQuitTx = \era acc ks ttl cs ->
        withShelleyBasedEra era $ _mkDelegationQuitTx acc ks ttl cs
    , decodeSignedTx =
        _decodeSignedTx
    , minimumFee =
        _minimumFee @k
    , estimateMaxNumberOfInputs =
        _estimateMaxNumberOfInputs @k
    }
  where
    _initDelegationSelection
        :: Coin
            -- stake key deposit
        -> DelegationAction
            -- What sort of action is going on
        -> CoinSelection
        -- ^ An initial selection where 'deposit' and/or 'reclaim' have been set
        -- accordingly.
    _initDelegationSelection (Coin c) = \case
        Quit{} -> mempty { reclaim = c }
        Join{} -> mempty
        RegisterKeyAndJoin{} -> mempty { deposit = c }

    _mkDelegationJoinTx
        :: forall era. (EraConstraints era)
        => PoolId
            -- ^ Pool Id to which we're planning to delegate
        -> (XPrv, Passphrase "encryption")
            -- ^ Reward account
        -> (Address -> Maybe (k 'AddressK XPrv, Passphrase "encryption"))
            -- ^ Key store
        -> SlotNo
            -- ^ TTL slot
        -> CoinSelection
            -- ^ A balanced coin selection where all change addresses have been
            -- assigned.
        -> ShelleyBasedEra era
            -- ^ Era for which the transaction should be created.
        -> Either ErrMkTx (Tx, SealedTx)
    _mkDelegationJoinTx poolId acc@(accXPrv, pwd') keyFrom ttl cs era = do
        let accXPub = toXPub accXPrv
        let certs =
                if deposit cs > 0
                then mkDelegationCertificates (RegisterKeyAndJoin poolId) accXPub
                else mkDelegationCertificates (Join poolId) accXPub

        let mkWits unsigned =
                [ mkShelleyWitness unsigned (accXPrv, pwd')
                ]

        let payload = TxPayload Nothing certs mkWits
        mkTx networkId payload ttl acc keyFrom cs era

    _mkDelegationQuitTx
        :: forall era. (EraConstraints era)
        => (XPrv, Passphrase "encryption")
            -- reward account
        -> (Address -> Maybe (k 'AddressK XPrv, Passphrase "encryption"))
            -- Key store
        -> SlotNo
            -- TTL slot
        -> CoinSelection
            -- A balanced coin selection where all change addresses have been
            -- assigned.
        -> ShelleyBasedEra era
            -- ^ Era for which the transaction should be created.
        -> Either ErrMkTx (Tx, SealedTx)
    _mkDelegationQuitTx acc@(accXPrv, pwd') keyFrom ttl cs era = do
        let accXPub = toXPub accXPrv
        let certs = [toStakeKeyDeregCert accXPub]
        let mkWits unsigned =
                [ mkShelleyWitness unsigned (accXPrv, pwd')
                ]

        let payload = TxPayload Nothing certs mkWits
        mkTx networkId payload ttl acc keyFrom cs era

mkDelegationCertificates
    :: DelegationAction
        -- Pool Id to which we're planning to delegate
    -> XPub
        -- Reward account public key
    -> [Cardano.Certificate]
mkDelegationCertificates da accXPub =
    case da of
       Join poolId ->
               [ toStakePoolDlgCert accXPub poolId ]
       RegisterKeyAndJoin poolId ->
               [ toStakeKeyRegCert  accXPub
               , toStakePoolDlgCert accXPub poolId
               ]
       Quit -> [toStakeKeyDeregCert accXPub]

_estimateMaxNumberOfInputs
    :: forall k. TxWitnessTagFor k
    => Quantity "byte" Word16
     -- ^ Transaction max size in bytes
    -> Maybe TxMetadata
     -- ^ Metadata associated with the transaction.
    -> Word8
    -- ^ Number of outputs in transaction
    -> Word8
_estimateMaxNumberOfInputs txMaxSize md nOuts =
    findLargestUntil ((> maxSize) . txSizeGivenInputs) 0
  where
    -- | Find the largest amount of inputs that doesn't make the tx too big.
    -- Tries in sequence from 0 and upward (up to 255, but smaller than 50 in
    -- practice because of the max transaction size).
    findLargestUntil :: (Word8 -> Bool) -> Word8 -> Word8
    findLargestUntil isTxTooLarge inf
        | inf == maxBound        = maxBound
        | isTxTooLarge (inf + 1) = inf
        | otherwise              = findLargestUntil isTxTooLarge (inf + 1)

    maxSize = fromIntegral (getQuantity txMaxSize)

    txSizeGivenInputs nInps = size
      where
        size = estimateTxSize (txWitnessTagFor @k) md Nothing sel
        sel  = dummyCoinSel (fromIntegral nInps) (fromIntegral nOuts)

dummyCoinSel :: Int -> Int -> CoinSelection
dummyCoinSel nInps nOuts = mempty
    { CS.inputs = map (\ix -> (dummyTxIn ix, dummyTxOut)) [0..nInps-1]
    , CS.outputs = replicate nOuts dummyTxOut
    , CS.change = replicate nOuts (Coin 1)
    }
  where
    dummyTxIn   = TxIn (Hash $ BS.pack (1:replicate 64 0)) . fromIntegral
    dummyTxOut  = TxOut dummyAddr (TokenBundle.fromCoin $ Coin 1)
    dummyAddr   = Address $ BS.pack (1:replicate 64 0)

_decodeSignedTx
    :: AnyCardanoEra
    -> ByteString
    -> Either ErrDecodeSignedTx (Tx, SealedTx)
_decodeSignedTx era bytes = do
    case era of
        AnyCardanoEra ShelleyEra ->
            case Cardano.deserialiseFromCBOR (Cardano.AsTx Cardano.AsShelleyEra) bytes of
                Right txValid ->
                    pure $ sealShelleyTx fromShelleyTx txValid
                Left decodeErr ->
                    Left $ ErrDecodeSignedTxWrongPayload (T.pack $ show decodeErr)

        AnyCardanoEra AllegraEra ->
            case Cardano.deserialiseFromCBOR (Cardano.AsTx Cardano.AsAllegraEra) bytes of
                Right txValid ->
                    pure $ sealShelleyTx fromAllegraTx txValid
                Left decodeErr ->
                    Left $ ErrDecodeSignedTxWrongPayload (T.pack $ show decodeErr)

        _ ->
            Left ErrDecodeSignedTxNotSupported

_minimumFee
    :: forall k. TxWitnessTagFor k
    => FeePolicy
    -> Maybe DelegationAction
    -> Maybe TxMetadata
    -> CoinSelection
    -> Fee
_minimumFee policy action md cs =
    computeFee $ estimateTxSize (txWitnessTagFor @k) md action cs
  where
    computeFee :: Integer -> Fee
    computeFee size =
        Fee $ ceiling (a + b*fromIntegral size)
      where
        LinearFee (Quantity a) (Quantity b) = policy

-- Estimate the size of a final transaction by using upper boundaries for cbor
-- serialized objects according to:
--
-- https://github.com/input-output-hk/cardano-ledger-specs/blob/master/shelley/chain-and-ledger/shelley-spec-ledger-test/cddl-files/shelley.cddl
--
-- All sizes below are in bytes.
estimateTxSize
    :: TxWitnessTag
    -> Maybe Cardano.TxMetadata
    -> Maybe DelegationAction
    -> CoinSelection
    -> Integer
estimateTxSize witTag md action cs =
    sizeOf_Transaction
  where
    numberOf_Inputs
        = toInteger $ length $ CS.inputs cs

    numberOf_CertificateSignatures
        = maybe 0 (const 1) action

    numberOf_Withdrawals
        = if CS.withdrawal cs > 0 then 1 else 0

    numberOf_VkeyWitnesses
        = case witTag of
            TxWitnessByronUTxO{} -> 0
            TxWitnessShelleyUTxO ->
                numberOf_Inputs
                + numberOf_Withdrawals
                + numberOf_CertificateSignatures

    numberOf_BootstrapWitnesses
        = case witTag of
            TxWitnessByronUTxO{} -> numberOf_Inputs
            TxWitnessShelleyUTxO -> 0

    -- transaction =
    --   [ transaction_body
    --   , transaction_witness_set
    --   , transaction_metadata / null
    --   ]
    sizeOf_Transaction
        = sizeOf_SmallArray
        + sizeOf_TransactionBody
        + sizeOf_WitnessSet
        + sizeOf_Metadata

    -- transaction_body =
    --   { 0 : set<transaction_input>
    --   , 1 : [* transaction_output]
    --   , 2 : coin ; fee
    --   , 3 : uint ; ttl
    --   , ? 4 : [* certificate]
    --   , ? 5 : withdrawals
    --   , ? 6 : update
    --   , ? 7 : metadata_hash
    --   }
    sizeOf_TransactionBody
        = sizeOf_SmallMap
        + sizeOf_Inputs
        + sizeOf_Outputs
        + sizeOf_Fee
        + sizeOf_Ttl
        + sizeOf_Certificates
        + sizeOf_Withdrawals
        + sizeOf_Update
        + sizeOf_MetadataHash
      where
        -- 0 => set<transaction_input>
        sizeOf_Inputs
            = sizeOf_SmallUInt
            + sizeOf_Array
            + sizeOf_Input * numberOf_Inputs

        -- 1 => [* transaction_output]
        sizeOf_Outputs
            = sizeOf_SmallUInt
            + sizeOf_Array
            + sum (sizeOf_Output <$> CS.outputs cs)
            + sum (sizeOf_ChangeOutput <$> CS.change cs)

        -- 2 => fee
        sizeOf_Fee
            = sizeOf_SmallUInt
            + sizeOf_UInt

        -- 3 => ttl
        sizeOf_Ttl
            = sizeOf_SmallUInt
            + sizeOf_UInt

        -- ?4 => [* certificates ]
        sizeOf_Certificates
            = case action of
                Nothing ->
                    0
                Just RegisterKeyAndJoin{} ->
                    sizeOf_SmallUInt
                    + sizeOf_SmallArray
                    + sizeOf_StakeRegistration
                    + sizeOf_StakeDelegation
                Just Join{} ->
                    sizeOf_SmallUInt
                    + sizeOf_SmallArray
                    + sizeOf_StakeDelegation
                Just Quit{} ->
                    sizeOf_SmallUInt
                    + sizeOf_SmallArray
                    + sizeOf_StakeDeregistration

        -- ?5 => withdrawals
        sizeOf_Withdrawals
            = (if numberOf_Withdrawals > 0 then sizeOf_SmallUInt + sizeOf_SmallMap else 0)
            + sizeOf_Withdrawal * numberOf_Withdrawals

        -- ?6 => update
        sizeOf_Update
            = 0 -- Assuming no updates is running through cardano-wallet

        -- ?7 => metadata_hash
        sizeOf_MetadataHash
            = maybe 0 (const (sizeOf_SmallUInt + sizeOf_Hash32)) md

    -- For metadata, we can't choose a reasonable upper bound, so it's easier to
    -- measure the serialize data since we have it anyway. When it's "empty",
    -- metadata are represented by a special "null byte" in CBOR `F6`.
    sizeOf_Metadata
        = maybe 1 (toInteger . BS.length . serialiseToCBOR) md

    -- transaction_input =
    --   [ transaction_id : $hash32
    --   , index : uint
    --   ]
    sizeOf_Input
        = sizeOf_SmallArray
        + sizeOf_Hash32
        + sizeOf_UInt

    -- transaction_output =
    --   [address, amount : coin]
    sizeOf_Output TxOut {address, tokens}
        = sizeOf_SmallArray
        + sizeOf_Address address
        + sizeOf_Coin (TokenBundle.getCoin tokens)

    sizeOf_ChangeOutput c
        = sizeOf_SmallArray
        + sizeOf_ChangeAddress
        + sizeOf_Coin c

    -- stake_registration =
    --   (0, stake_credential)
    sizeOf_StakeRegistration
        = sizeOf_SmallArray
        + sizeOf_SmallUInt
        + sizeOf_StakeCredential

    -- stake_deregistration =
    --   (1, stake_credential)
    sizeOf_StakeDeregistration
        = sizeOf_StakeRegistration

    -- stake_delegation =
    --   (2, stake_credential, pool_keyhash)
    sizeOf_StakeDelegation
        = sizeOf_SmallArray
        + sizeOf_SmallUInt
        + sizeOf_StakeCredential
        + sizeOf_Hash28

    -- stake_credential =
    --   [  0, addr_keyhash
    --   // 1, scripthash
    --   ]
    sizeOf_StakeCredential
        = sizeOf_SmallArray
        + sizeOf_SmallUInt
        + sizeOf_Hash28

    -- We carry addresses already serialized, so it's a matter of measuring.
    sizeOf_Address addr
        = 2 + toInteger (BS.length (unAddress addr))

    -- For change address, we consider the worst-case scenario based on the
    -- given wallet scheme. Byron addresses are larger.
    --
    -- NOTE: we could do slightly better if we wanted to for Byron addresses and
    -- discriminate based on the network as well since testnet addresses are
    -- larger than mainnet ones. But meh.
    sizeOf_ChangeAddress
        = case witTag of
            TxWitnessByronUTxO{} -> 85
            TxWitnessShelleyUTxO -> 59

    -- Coins can really vary so it's very punishing to always assign them the
    -- upper bound. They will typically be between 3 and 9 bytes (only 6 bytes
    -- difference, but on 20+ outputs, one starts feeling it).
    --
    -- So, for outputs, since we have the values, we can compute it accurately.
    sizeOf_Coin
        = toInteger
        . BS.length
        . CBOR.toStrictByteString
        . CBOR.encodeWord64
        . unCoin

    -- withdrawals =
    --   { * reward_account => coin }
    sizeOf_Withdrawal
        = sizeOf_Hash28
        + sizeOf_LargeUInt

    -- transaction_witness_set =
    --   { ?0 => [* vkeywitness ]
    --   , ?1 => [* multisig_script ]
    --   , ?2 => [* bootstrap_witness ]
    --   }
    sizeOf_WitnessSet
        = sizeOf_SmallMap
        + sizeOf_VKeyWitnesses
        + sizeOf_MultisigScript
        + sizeOf_BootstrapWitnesses
      where
        -- ?0 => [* vkeywitness ]
        sizeOf_VKeyWitnesses
            = (if numberOf_VkeyWitnesses > 0 then sizeOf_Array + sizeOf_SmallUInt else 0)
            + sizeOf_VKeyWitness * numberOf_VkeyWitnesses

        -- ?1 => [* multisig_script ]
        sizeOf_MultisigScript
            = 0

        -- ?2 => [* bootstrap_witness ]
        sizeOf_BootstrapWitnesses
            = (if numberOf_BootstrapWitnesses > 0 then sizeOf_Array + sizeOf_SmallUInt else 0)
            + sizeOf_BootstrapWitness * numberOf_BootstrapWitnesses

    -- vkeywitness =
    --  [ $vkey
    --  , $signature
    --  ]
    sizeOf_VKeyWitness
        = sizeOf_SmallArray
        + sizeOf_VKey
        + sizeOf_Signature

    -- bootstrap_witness =
    --  [ public_key : $vkey
    --  , signature  : $signature
    --  , chain_code : bytes .size 32
    --  , attributes : bytes
    --  ]
    sizeOf_BootstrapWitness
        = sizeOf_SmallArray
        + sizeOf_VKey
        + sizeOf_Signature
        + sizeOf_ChainCode
        + sizeOf_Attributes
      where
        sizeOf_ChainCode  = 34
        sizeOf_Attributes = 45 -- NOTE: could be smaller by ~34 for Icarus

    -- A Blake2b-224 hash, resulting in a 28-byte digest wrapped in CBOR, so
    -- with 2 bytes overhead (length <255, but length > 23)
    sizeOf_Hash28
        = 30

    -- A Blake2b-256 hash, resulting in a 32-byte digest wrapped in CBOR, so
    -- with 2 bytes overhead (length <255, but length > 23)
    sizeOf_Hash32
        = 34

    -- A 32-byte Ed25519 public key, encoded as a CBOR-bytestring so with 2
    -- bytes overhead (length < 255, but length > 23)
    sizeOf_VKey
        = 34

    -- A 64-byte Ed25519 signature, encoded as a CBOR-bytestring so with 2
    -- bytes overhead (length < 255, but length > 23)
    sizeOf_Signature
        = 66

    -- A CBOR UInt which is less than 23 in value fits on a single byte. Beyond,
    -- the first byte is used to encode the number of bytes necessary to encode
    -- the number itself, followed by the number itself.
    --
    -- When considering a 'UInt', we consider the worst case scenario only where
    -- the uint is encoded over 4 bytes, so up to 2^32 which is fine for most
    -- cases but coin values.
    sizeOf_SmallUInt = 1
    sizeOf_UInt = 5
    sizeOf_LargeUInt = 9

    -- A CBOR array with less than 23 elements, fits on a single byte, followed
    -- by each key-value pair (encoded as two concatenated CBOR elements).
    sizeOf_SmallMap = 1

    -- A CBOR array with less than 23 elements, fits on a single byte, followed
    -- by each elements. Otherwise, the length of the array is encoded first,
    -- very much like for UInt.
    --
    -- When considering an 'Array', we consider large scenarios where arrays can
    -- have up to 65536 elements.
    sizeOf_SmallArray = 1
    sizeOf_Array = 3

lookupPrivateKey
    :: (Address -> Maybe (k 'AddressK XPrv, Passphrase "encryption"))
    -> Address
    -> Either ErrMkTx (k 'AddressK XPrv, Passphrase "encryption")
lookupPrivateKey keyFrom addr =
    maybe (Left $ ErrKeyNotFoundForAddress addr) Right (keyFrom addr)

withShelleyBasedEra
    :: forall a. ()
    => AnyCardanoEra
    -> (forall era. EraConstraints era => ShelleyBasedEra era -> Either ErrMkTx a)
    -> Either ErrMkTx a
withShelleyBasedEra era fn = case era of
    AnyCardanoEra ByronEra    -> Left $ ErrInvalidEra era
    AnyCardanoEra ShelleyEra  -> fn ShelleyBasedEraShelley
    AnyCardanoEra AllegraEra  -> fn ShelleyBasedEraAllegra
    AnyCardanoEra MaryEra     -> fn ShelleyBasedEraMary

-- FIXME: Make this a Allegra or Shelley transaction depending on the era we're
-- in. However, quoting Duncan:
--
--    "Yes, you can submit Shelley format transactions in the Allegra era.The
--    proper way to do that is marking it as a Shelley tx using GenTxShelley.
--    The improper way is to submit it as a GenTxAllegra. The latter should
--    still work since the binary formats are upwards compatible."
--
-- Which suggests that we may get away with Shelley-only transactions for now?
mkUnsignedTx
    :: ShelleyBasedEra era
    -> Cardano.SlotNo
    -> CoinSelection
    -> Maybe Cardano.TxMetadata
    -> [(Cardano.StakeAddress, Cardano.Lovelace)]
    -> [Cardano.Certificate]
    -> Either ErrMkTx (Cardano.TxBody era)
mkUnsignedTx era ttl cs md wdrls certs =
    case era of
        ShelleyBasedEraShelley -> mkShelleyTx
        ShelleyBasedEraAllegra -> mkAllegraTx
        ShelleyBasedEraMary    -> Left (ErrInvalidEra (AnyCardanoEra MaryEra))
  where
    fee :: Cardano.Lovelace
    fee = toCardanoLovelace $ Coin $ feeBalance cs

    mkShelleyTx :: Either ErrMkTx (Cardano.TxBody ShelleyEra)
    mkShelleyTx = left toErrMkTx $ Cardano.makeTransactionBody $ Cardano.TxBodyContent
        { Cardano.txIns =
            toCardanoTxIn . fst <$> CS.inputs cs

        , Cardano.txOuts =
            toShelleyTxOut <$> CS.outputs cs

        , Cardano.txWithdrawals =
            Cardano.TxWithdrawals Cardano.WithdrawalsInShelleyEra wdrls

        , Cardano.txCertificates =
            Cardano.TxCertificates Cardano.CertificatesInShelleyEra certs

        , Cardano.txFee =
            Cardano.TxFeeExplicit Cardano.TxFeesExplicitInShelleyEra fee

        , Cardano.txValidityRange =
            ( Cardano.TxValidityNoLowerBound
            , Cardano.TxValidityUpperBound Cardano.ValidityUpperBoundInShelleyEra ttl
            )

        , Cardano.txMetadata =
            maybe
                Cardano.TxMetadataNone
                (Cardano.TxMetadataInEra Cardano.TxMetadataInShelleyEra)
                md

        , Cardano.txAuxScripts =
            Cardano.TxAuxScriptsNone

        , Cardano.txUpdateProposal =
            Cardano.TxUpdateProposalNone

        , Cardano.txMintValue =
            Cardano.TxMintNone
        }
      where
        toErrMkTx :: Cardano.TxBodyError ShelleyEra -> ErrMkTx
        toErrMkTx = ErrConstructedInvalidTx . T.pack . Cardano.displayError

    mkAllegraTx :: Either ErrMkTx (Cardano.TxBody AllegraEra)
    mkAllegraTx = left toErrMkTx $ Cardano.makeTransactionBody $ Cardano.TxBodyContent
        { Cardano.txIns =
            toCardanoTxIn . fst <$> CS.inputs cs

        , Cardano.txOuts =
            toAllegraTxOut <$> CS.outputs cs

        , Cardano.txWithdrawals =
            Cardano.TxWithdrawals Cardano.WithdrawalsInAllegraEra wdrls

        , Cardano.txCertificates =
            Cardano.TxCertificates Cardano.CertificatesInAllegraEra certs

        , Cardano.txFee =
            Cardano.TxFeeExplicit Cardano.TxFeesExplicitInAllegraEra fee

        , Cardano.txValidityRange =
            ( Cardano.TxValidityNoLowerBound
            , Cardano.TxValidityUpperBound Cardano.ValidityUpperBoundInAllegraEra ttl
            )

        , Cardano.txMetadata =
            maybe
                Cardano.TxMetadataNone
                (Cardano.TxMetadataInEra Cardano.TxMetadataInAllegraEra)
                md

        , Cardano.txAuxScripts =
            Cardano.TxAuxScriptsNone

        , Cardano.txUpdateProposal =
            Cardano.TxUpdateProposalNone

        , Cardano.txMintValue =
            Cardano.TxMintNone
        }
      where
        toErrMkTx :: Cardano.TxBodyError AllegraEra -> ErrMkTx
        toErrMkTx = ErrConstructedInvalidTx . T.pack . Cardano.displayError

mkWithdrawals
    :: NetworkId
    -> RewardAccount
    -> Word64
    -> [(Cardano.StakeAddress, Cardano.Lovelace)]
mkWithdrawals networkId acc amount
    | amount == 0 = mempty
    | otherwise   = [ (stakeAddress, toCardanoLovelace $ Coin amount) ]
  where
    cred = toCardanoStakeCredential acc
    stakeAddress = Cardano.makeStakeAddress networkId cred

mkShelleyWitness
    :: IsShelleyBasedEra era
    => Cardano.TxBody era
    -> (XPrv, Passphrase "encryption")
    -> Cardano.Witness era
mkShelleyWitness body key =
    Cardano.makeShelleyKeyWitness body (unencrypt key)
  where
    unencrypt (xprv, pwd) = Cardano.WitnessPaymentExtendedKey
        $ Cardano.PaymentExtendedSigningKey
        $ Crypto.HD.xPrvChangePass pwd BS.empty xprv

mkByronWitness
    :: forall era. (EraConstraints era)
    => Cardano.TxBody era
    -> Cardano.NetworkId
    -> Address
    -> (XPrv, Passphrase "encryption")
    -> Cardano.Witness era
mkByronWitness (Cardano.ShelleyTxBody era body _) nw addr encryptedKey =
    Cardano.ShelleyBootstrapWitness era $
        SL.makeBootstrapWitness txHash (unencrypt encryptedKey) addrAttr
  where
    txHash = Crypto.castHash $ Crypto.hashWith serialize' body

    unencrypt (xprv, pwd) = CC.SigningKey
        $ Crypto.HD.xPrvChangePass pwd BS.empty xprv

    addrAttr = Byron.mkAttributes $ Byron.AddrAttributes
        (toHDPayloadAddress addr)
        (Cardano.toByronNetworkMagic nw)
