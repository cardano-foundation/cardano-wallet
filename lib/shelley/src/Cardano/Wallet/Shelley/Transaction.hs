{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
    ( NetworkId, TxExtraContent (..) )
import Cardano.Binary
    ( serialize' )
import Cardano.Crypto.DSIGN
    ( DSIGNAlgorithm (..), SignedDSIGN (..) )
import Cardano.Crypto.Wallet
    ( XPub )
import Cardano.Ledger.Crypto
    ( Crypto (..) )
import Cardano.Wallet.Primitive.AddressDerivation
    ( ChimericAccount (..), Depth (..), Passphrase (..), WalletKey (..) )
import Cardano.Wallet.Primitive.AddressDerivation.Byron
    ( ByronKey )
import Cardano.Wallet.Primitive.AddressDerivation.Icarus
    ( IcarusKey )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( ShelleyKey, toChimericAccountRaw )
import Cardano.Wallet.Primitive.CoinSelection
    ( CoinSelection (..), feeBalance )
import Cardano.Wallet.Primitive.Fee
    ( Fee (..), FeePolicy (..) )
import Cardano.Wallet.Primitive.Types
    ( PoolId (..), SealedTx (..), Tx (..), TxIn (..), TxMetadata, TxOut (..) )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..) )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Cardano.Wallet.Shelley.Compatibility
    ( Shelley
    , StandardCrypto
    , sealShelleyTx
    , toCardanoLovelace
    , toCardanoStakeCredential
    , toCardanoTxIn
    , toCardanoTxOut
    , toHDPayloadAddress
    , toStakeKeyDeregCert
    , toStakeKeyRegCert
    , toStakePoolDlgCert
    )
import Cardano.Wallet.Transaction
    ( DelegationAction (..)
    , ErrDecodeSignedTx (..)
    , ErrMkTx (..)
    , ErrValidateSelection
    , TransactionLayer (..)
    )
import Control.Monad
    ( forM )
import Data.ByteString
    ( ByteString )
import Data.Maybe
    ( fromMaybe )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Quantity (..) )
import Data.Word
    ( Word16, Word64, Word8 )
import Ouroboros.Network.Block
    ( SlotNo )

import qualified Cardano.Api.Typed as Cardano
import qualified Cardano.Chain.Common as Byron
import qualified Cardano.Crypto as CC
import qualified Cardano.Crypto.Hash.Class as Crypto
import qualified Cardano.Crypto.Wallet as Crypto.HD
import qualified Cardano.Wallet.Primitive.CoinSelection as CS
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Text as T
import qualified Shelley.Spec.Ledger.Address.Bootstrap as SL
import qualified Shelley.Spec.Ledger.Keys as SL
import qualified Shelley.Spec.Ledger.LedgerState as SL
import qualified Shelley.Spec.Ledger.Tx as SL

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
    :: forall k. (TxWitnessTagFor k, WalletKey k)
    => Cardano.NetworkId
    -> TxPayload Cardano.Shelley
    -> SlotNo
    -- ^ Slot at which the transaction will expire.
    -> (XPrv, Passphrase "encryption")
    -- ^ Reward account
    -> (Address -> Maybe (k 'AddressK XPrv, Passphrase "encryption"))
    -> CoinSelection
    -> Either ErrMkTx (Tx, SealedTx)
mkTx networkId (TxPayload md certs mkExtraWits) expirySlot (rewardAcnt, pwdAcnt) keyFrom cs = do
    let wdrls = mkWithdrawals
            networkId
            (toChimericAccountRaw . toXPub $ rewardAcnt)
            (withdrawal cs)

    let unsigned = mkUnsignedTx expirySlot cs md wdrls certs

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
    return $ sealShelleyTx signed

newTransactionLayer
    :: forall k t.
        ( t ~ IO Shelley
        , TxWitnessTagFor k
        , WalletKey k
        )
    => NetworkId
    -> TransactionLayer t k
newTransactionLayer networkId = TransactionLayer
    { mkStdTx = \acc ks tip md ->
        mkTx networkId (stdTxPayload md) tip acc ks
    , initDelegationSelection = _initDelegationSelection
    , mkDelegationJoinTx = _mkDelegationJoinTx
    , mkDelegationQuitTx = _mkDelegationQuitTx
    , decodeSignedTx = _decodeSignedTx
    , minimumFee = _minimumFee @k networkId
    , estimateMaxNumberOfInputs = _estimateMaxNumberOfInputs @k networkId
    , validateSelection = const $ return ()
    , allowUnbalancedTx = True
    }
  where
    _initDelegationSelection
        :: FeePolicy
            -- Current fee policy
        -> DelegationAction
            -- What sort of action is going on
        -> CoinSelection
        -- ^ An initial selection where 'deposit' and/or 'reclaim' have been set
        -- accordingly.
    _initDelegationSelection (LinearFee _ _ (Quantity c)) = \case
        Quit{} -> mempty { reclaim = round c }
        Join{} -> mempty
        RegisterKeyAndJoin{} -> mempty { deposit = round c }

    _mkDelegationJoinTx
        :: PoolId
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
        -> Either ErrMkTx (Tx, SealedTx)
    _mkDelegationJoinTx poolId acc@(accXPrv, pwd') keyFrom ttl cs = do
        let accXPub = toXPub accXPrv
        let certs =
                if deposit cs > 0
                then mkDelegationCertificates (RegisterKeyAndJoin poolId) accXPub
                else mkDelegationCertificates (Join poolId) accXPub

        let mkWits unsigned =
                [ mkShelleyWitness unsigned (accXPrv, pwd')
                ]

        let payload = TxPayload Nothing certs mkWits
        mkTx networkId payload ttl acc keyFrom cs

    _mkDelegationQuitTx
        :: (XPrv, Passphrase "encryption")
            -- reward account
        -> (Address -> Maybe (k 'AddressK XPrv, Passphrase "encryption"))
            -- Key store
        -> SlotNo
            -- TTL slot
        -> CoinSelection
            -- A balanced coin selection where all change addresses have been
            -- assigned.
        -> Either ErrMkTx (Tx, SealedTx)
    _mkDelegationQuitTx acc@(accXPrv, pwd') keyFrom ttl cs = do
        let accXPub = toXPub accXPrv
        let certs = [toStakeKeyDeregCert accXPub]
        let mkWits unsigned =
                [ mkShelleyWitness unsigned (accXPrv, pwd')
                ]

        let payload = TxPayload Nothing certs mkWits
        mkTx networkId payload ttl acc keyFrom cs

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
    => NetworkId
    -> Quantity "byte" Word16
     -- ^ Transaction max size in bytes
    -> Maybe TxMetadata
     -- ^ Metadata associated with the transaction.
    -> Word8
    -- ^ Number of outputs in transaction
    -> Word8
_estimateMaxNumberOfInputs networkId txMaxSize md nOuts =
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
        size = computeTxSize networkId (txWitnessTagFor @k) md Nothing sel
        sel  = dummyCoinSel (fromIntegral nInps) (fromIntegral nOuts)

dummyCoinSel :: Int -> Int -> CoinSelection
dummyCoinSel nInps nOuts = mempty
    { CS.inputs = map (\ix -> (dummyTxIn ix, dummyTxOut)) [0..nInps-1]
    , CS.outputs = replicate nOuts dummyTxOut
    , CS.change = replicate nOuts (Coin 1)
    }
  where
    dummyTxIn   = TxIn (Hash $ BS.pack (1:replicate 64 0)) . fromIntegral
    dummyTxOut  = TxOut dummyAddr (Coin 1)
    dummyAddr   = Address $ BS.pack (1:replicate 64 0)

_decodeSignedTx
    :: ByteString
    -> Either ErrDecodeSignedTx (Tx, SealedTx)
_decodeSignedTx bytes = do
    case Cardano.deserialiseFromCBOR Cardano.AsShelleyTx bytes of
        Right txValid ->
            pure $ sealShelleyTx txValid
        Left decodeErr ->
            Left $ ErrDecodeSignedTxWrongPayload (T.pack $ show decodeErr)

_minimumFee
    :: forall k. TxWitnessTagFor k
    => NetworkId
    -> FeePolicy
    -> Maybe DelegationAction
    -> Maybe TxMetadata
    -> CoinSelection
    -> Fee
_minimumFee networkId policy action md cs =
    computeFee $ computeTxSize networkId (txWitnessTagFor @k) md action cs
  where
    computeFee :: Integer -> Fee
    computeFee size =
        Fee $ ceiling (a + b*fromIntegral size)
      where
        LinearFee (Quantity a) (Quantity b) _unused = policy

-- TODO: Can this function be re-written by calling @mkTx@ with dummy signing
-- functions?
computeTxSize
    :: Cardano.NetworkId
    -> TxWitnessTag
    -> Maybe Cardano.TxMetadata
    -> Maybe DelegationAction
    -> CoinSelection
    -> Integer
computeTxSize networkId witTag md action cs =
    withUnderlyingShelleyTx SL.txsize signed + outputCorrection
 where
    withUnderlyingShelleyTx
        :: (forall crypto. SL.Tx crypto -> a)
        -> Cardano.Tx Cardano.Shelley
        -> a
    withUnderlyingShelleyTx f (Cardano.ShelleyTx x) = f x

    signed = Cardano.makeSignedTransaction wits unsigned

    outputCorrection = sum
        [ toInteger (length $ change cs) * perChangeCorrection
        , toInteger (length $ inputs cs) * perInputCorrection
        ]
      where
         -- NOTE
         -- When we generate dummy output, we generate them as new Shelley addresses
         -- because their format is much easier to generate (can be a sequence of
         -- bytes, and a straightforward header). Generating dummy Byron or Icarus
         -- addresses would be nuts. So instead, we apply a small correction for each
         -- output which can either slightly increase or decrease the overall size.
         --
         -- When change are Shelley addresses, the correction is null.
         perChangeCorrection = case witTag of
            TxWitnessShelleyUTxO ->
                0
            TxWitnessByronUTxO Byron | networkId == Cardano.Mainnet ->
                maxSizeOfByronMainAddr - maxSizeOfShelleyAddr
            TxWitnessByronUTxO Byron ->
                maxSizeOfByronTestAddr - maxSizeOfShelleyAddr
            TxWitnessByronUTxO Icarus | networkId == Cardano.Mainnet ->
                maxSizeOfIcarusMainAddr - maxSizeOfShelleyAddr
            TxWitnessByronUTxO Icarus ->
                maxSizeOfIcarusTestAddr - maxSizeOfShelleyAddr
          where
            maxSizeOfShelleyAddr    = 56 + 1
            maxSizeOfByronMainAddr  = 76
            maxSizeOfByronTestAddr  = 83
            maxSizeOfIcarusMainAddr = 43
            maxSizeOfIcarusTestAddr = 50

         -- NOTE
         -- We generate dummy witnesses that are exclusively in Shelley format.
         -- In Byron (or so-called bootstrap witnesses), we need to account for
         -- some extra bytes due to the inclusion of:
         --
         -- - The chain code
         -- - Serialized address attributes (path and protocol magic)
         perInputCorrection = case witTag of
            TxWitnessShelleyUTxO ->
                0
            TxWitnessByronUTxO Byron | networkId == Cardano.Mainnet ->
                ccLen + addrPathAttrLen
            TxWitnessByronUTxO Byron ->
                ccLen + addrPathAttrLen + networkMagicLen
            TxWitnessByronUTxO Icarus | networkId == Cardano.Mainnet ->
                ccLen
            TxWitnessByronUTxO Icarus ->
                ccLen + networkMagicLen
           where
             ccLen = 34
             addrPathAttrLen = 40
             networkMagicLen = 7

    unsigned = mkUnsignedTx maxBound cs' md wdrls certs
      where
        cs' :: CoinSelection
        cs' = cs
            { CS.outputs = CS.outputs cs <> (dummyOutput <$> change cs)
            , CS.change  = []
            }

        dummyOutput :: Coin -> TxOut
        dummyOutput =
            TxOut $ Address $ BS.pack $ 0:replicate 56 0

        dummyStakeCred = toCardanoStakeCredential
            $ ChimericAccount dummyKeyHashRaw

        dummyPoolId :: Cardano.PoolId
        dummyPoolId = fromMaybe (error "dummyPoolId couldn't be constructed")
            $ Cardano.deserialiseFromRawBytes (Cardano.AsHash Cardano.AsStakePoolKey)
            dummyKeyHashRaw

        certs = case action of
            Nothing -> []
            Just RegisterKeyAndJoin{} ->
                [ Cardano.makeStakeAddressRegistrationCertificate dummyStakeCred
                , Cardano.makeStakeAddressDelegationCertificate dummyStakeCred dummyPoolId
                ]
            Just Join{} ->
                [ Cardano.makeStakeAddressDelegationCertificate dummyStakeCred dummyPoolId
                ]
            Just Quit ->
                [ Cardano.makeStakeAddressDeregistrationCertificate dummyStakeCred
                ]

    dummyKeyHashRaw = BA.convert $ BS.pack (replicate 28 0)

    wdrls = mkWithdrawals
        networkId
        (ChimericAccount dummyKeyHashRaw)
        (withdrawal cs)

    -- NOTE
    -- We do not allow certificate witnesses for Byron because we _know_ that we
    -- don't have hybrid wallets (in the context of cardano-wallet). So, any
    -- Byron UTxO would necessarily come from a Byron wallet (either Random or
    -- Icarus) and therefore, have no delegation capability and no need for
    -- certificate witnesses.
    wits = case witTag of
        TxWitnessShelleyUTxO ->
           addrWits <> certWits
        TxWitnessByronUTxO{} ->
           addrWits

    (addrWits, certWits) =
        ( mconcat
            [ map dummyWitnessUniq (fst <$> CS.inputs cs)
            , if null wdrls then mempty else [dummyWitness "0"]
            ]
        , case action of
            Nothing -> []
            Just{}  -> [dummyWitness "a"]
        )
      where
        dummyWitness :: BL.ByteString -> Cardano.Witness Cardano.Shelley
        dummyWitness chaff = Cardano.ShelleyKeyWitness $ SL.WitVKey key sig
          where
            key = SL.VKey
                $ fromMaybe (error "error creating dummy witness ver key")
                $ rawDeserialiseVerKeyDSIGN
                $ bloatChaff keyLen chaff

            sig = SignedDSIGN
                $ fromMaybe (error "error creating dummy witness sig")
                $ rawDeserialiseSigDSIGN
                $ bloatChaff sigLen chaff

        dummyWitnessUniq :: TxIn -> Cardano.Witness Cardano.Shelley
        dummyWitnessUniq (TxIn (Hash txid) ix) =
            dummyWitness chaff
          where
            chaff = L8.pack (show ix) <> BL.fromStrict txid

    sigLen = sizeSigDSIGN $ Proxy @(DSIGN StandardCrypto)

    keyLen = sizeVerKeyDSIGN $ Proxy @(DSIGN StandardCrypto)

    bloatChaff :: Word -> BL.ByteString -> ByteString
    bloatChaff n = BL.toStrict . BL.take (fromIntegral n) . BL.cycle

lookupPrivateKey
    :: (Address -> Maybe (k 'AddressK XPrv, Passphrase "encryption"))
    -> Address
    -> Either ErrMkTx (k 'AddressK XPrv, Passphrase "encryption")
lookupPrivateKey keyFrom addr =
    maybe (Left $ ErrKeyNotFoundForAddress addr) Right (keyFrom addr)

mkUnsignedTx
    :: Cardano.SlotNo
    -> CoinSelection
    -> Maybe Cardano.TxMetadata
    -> [(Cardano.StakeAddress, Cardano.Lovelace)]
    -> [Cardano.Certificate]
    -> Cardano.TxBody Cardano.Shelley
mkUnsignedTx ttl cs md wdrls certs =
        Cardano.makeShelleyTransaction
            TxExtraContent
                { txMetadata = md
                , txWithdrawals = wdrls
                , txCertificates = certs
                , txUpdateProposal = Nothing
                }
            ttl
            (toCardanoLovelace $ Coin $ feeBalance cs)
            (toCardanoTxIn . fst <$> CS.inputs cs)
            (map toCardanoTxOut $ CS.outputs cs)

mkWithdrawals
    :: NetworkId
    -> ChimericAccount
    -> Word64
    -> [(Cardano.StakeAddress, Cardano.Lovelace)]
mkWithdrawals networkId acc amount
    | amount == 0 = mempty
    | otherwise   = [ (stakeAddress, toCardanoLovelace $ Coin amount) ]
  where
    cred = toCardanoStakeCredential acc
    stakeAddress = Cardano.makeStakeAddress networkId cred

mkShelleyWitness
    :: Cardano.TxBody Cardano.Shelley
    -> (XPrv, Passphrase "encryption")
    -> Cardano.Witness Cardano.Shelley
mkShelleyWitness body key =
    Cardano.makeShelleyKeyWitness body (unencrypt key)
  where
    unencrypt (xprv, pwd) = Cardano.WitnessPaymentExtendedKey
        $ Cardano.PaymentExtendedSigningKey
        $ Crypto.HD.xPrvChangePass pwd BS.empty xprv

mkByronWitness
    :: Cardano.TxBody Cardano.Shelley
    -> Cardano.NetworkId
    -> Address
    -> (XPrv, Passphrase "encryption")
    -> Cardano.Witness Cardano.Shelley
mkByronWitness (Cardano.ShelleyTxBody body _) nw addr encryptedKey =
    Cardano.ShelleyBootstrapWitness $
        SL.makeBootstrapWitness txHash (unencrypt encryptedKey) addrAttr
  where
    txHash = Crypto.hashWith serialize' body

    unencrypt (xprv, pwd) = CC.SigningKey
        $ Crypto.HD.xPrvChangePass pwd BS.empty xprv

    addrAttr = Byron.mkAttributes $ Byron.AddrAttributes
        (toHDPayloadAddress addr)
        (Cardano.toByronNetworkMagic nw)

--------------------------------------------------------------------------------
-- Extra validations on coin selection
--

type instance ErrValidateSelection (IO Shelley) = ()
