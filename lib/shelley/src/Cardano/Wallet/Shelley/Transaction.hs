{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
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
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPrv, XPub, toXPub, xpubPublicKey )
import Cardano.Binary
    ( serialize' )
import Cardano.Crypto.DSIGN
    ( DSIGNAlgorithm (..), SignedDSIGN (..) )
import Cardano.Crypto.DSIGN.Ed25519
    ( VerKeyDSIGN (..) )
import Cardano.Wallet.Primitive.AddressDerivation
    ( ChimericAccount (..)
    , Depth (..)
    , NetworkDiscriminant (..)
    , Passphrase (..)
    , WalletKey (..)
    )
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
    ( Address (..)
    , Coin (..)
    , EpochLength (..)
    , Hash (..)
    , PoolId (..)
    , ProtocolMagic (..)
    , SealedTx (..)
    , SlotId (..)
    , Tx (..)
    , TxIn (..)
    , TxOut (..)
    )
import Cardano.Wallet.Shelley.Compatibility
    ( Shelley
    , TPraosStandardCrypto
    , fromNetworkDiscriminant
    , fromShelleyTx
    , toByronNetworkMagic
    , toCardanoLovelace
    , toCardanoStakeAddress
    , toCardanoStakeCredential
    , toCardanoTxIn
    , toCardanoTxOut
    , toHDPayloadAddress
    , toSealed
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
import Crypto.Error
    ( throwCryptoError )
import Data.ByteString
    ( ByteString )
import Data.Map.Strict
    ( Map )
import Data.Maybe
    ( fromMaybe )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Quantity (..) )
import Data.Word
    ( Word16, Word64, Word8 )
import Ouroboros.Consensus.Shelley.Protocol.Crypto
    ( Crypto (..) )
import Ouroboros.Network.Block
    ( SlotNo )
import Type.Reflection
    ( Typeable )

--import qualified Cardano.Api as Cardano
import qualified Cardano.Api.Typed as Cardano
import qualified Cardano.Chain.Common as Byron
import qualified Cardano.Crypto as Crypto
import qualified Cardano.Crypto.Hash.Class as Hash
import qualified Cardano.Crypto.Wallet as CC
import qualified Cardano.Wallet.Primitive.CoinSelection as CS
import qualified Crypto.PubKey.Ed25519 as Ed25519
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Shelley.Spec.Ledger.Address.Bootstrap as SL
import qualified Shelley.Spec.Ledger.BaseTypes as SL
import qualified Shelley.Spec.Ledger.Coin as SL
import qualified Shelley.Spec.Ledger.Credential as SL
import qualified Shelley.Spec.Ledger.Keys as SL
import qualified Shelley.Spec.Ledger.LedgerState as SL
import qualified Shelley.Spec.Ledger.Tx as SL
import qualified Shelley.Spec.Ledger.TxData as SL
import qualified Shelley.Spec.Ledger.UTxO as SL


-- | Type encapsulating what we need to know to add things -- payloads,
-- certificates -- to a transaction.
--
-- Designed to allow us to have /one/ @mkTx@ which doesn't care whether we
-- include certificates or not.
data TxPayload era = TxPayload
    { _certificates :: [Cardano.Certificate]
      -- ^ Certificates to be included in the transactions.

    , _extraWitnesses :: Cardano.TxBody era -> [Cardano.Witness era]
      -- ^ Create payload-specific witesses given the unsigned transaction body.
      --
      -- Caller has the freedom and responsibility to provide the correct
      -- witnesses for what they're trying to do.
    }

emptyTxPayload :: TxPayload c
emptyTxPayload = TxPayload mempty mempty

data TxWitnessTag
    = TxWitnessByronUTxO
    | TxWitnessShelleyUTxO
    deriving (Show, Eq)

-- | Provide a transaction witness for a given private key. The type of witness
-- is different between types of keys and, with backward-compatible support, we
-- need to support many types for one backend target.
class TxWitnessTagFor (k :: Depth -> * -> *) where
    txWitnessTagFor :: TxWitnessTag

instance TxWitnessTagFor ShelleyKey  where txWitnessTagFor = TxWitnessShelleyUTxO
instance TxWitnessTagFor IcarusKey   where txWitnessTagFor = TxWitnessByronUTxO
instance TxWitnessTagFor ByronKey    where txWitnessTagFor = TxWitnessByronUTxO

mkTx
    :: forall (n :: NetworkDiscriminant) k.
       ( Typeable n
       , TxWitnessTagFor k
       , WalletKey k
       )
    => Proxy n
    -> ProtocolMagic
    -> TxPayload Cardano.Shelley
    -> SlotNo
    -- ^ Time to Live
    -> (k 'AddressK XPrv, Passphrase "encryption")
    -- ^ Reward account
    -> (Address -> Maybe (k 'AddressK XPrv, Passphrase "encryption"))
    -> CoinSelection
    -> Either ErrMkTx (Tx, SealedTx)
mkTx proxy pm (TxPayload certs mkExtraWits) timeToLive (rewardAcnt, pwdAcnt) keyFrom cs = do
    let wdrls = mkWithdrawals
            proxy
            (toChimericAccountRaw . getRawKey . publicKey $ rewardAcnt)
            (withdrawal cs)

    let unsigned = mkUnsignedTx timeToLive cs wdrls certs

    wits <- case (txWitnessTagFor @k) of
        TxWitnessShelleyUTxO -> do
            addrWits <- forM (CS.inputs cs) $ \(_, TxOut addr _) -> do
                (k, pwd) <- lookupPrivateKey keyFrom addr
                pure $ mkShelleyWitness unsigned (getRawKey k, pwd)

            let wdrlsWits
                    | null wdrls = []
                    | otherwise =
                      [mkShelleyWitness unsigned (getRawKey rewardAcnt, pwdAcnt)]

            pure $ mkExtraWits unsigned <> addrWits <> wdrlsWits

        TxWitnessByronUTxO -> do
            bootstrapWits <- forM (CS.inputs cs) $ \(_, TxOut addr _) -> do
                (k, pwd) <- lookupPrivateKey keyFrom addr
                pure $ mkByronWitness unsigned pm addr (getRawKey k, pwd)
            pure $ bootstrapWits <> mkExtraWits unsigned

    let signed = Cardano.makeSignedTransaction wits unsigned
    let tx = fromTypedTx signed
    return (tx, undefined)
  where
    -- The Cardano.Tx GADT won't allow the Shelley crypto type param escape,
    -- so we convert directly to the concrete wallet Tx type:
    fromTypedTx :: Cardano.Tx Cardano.Shelley -> Tx
    fromTypedTx (Cardano.ShelleyTx x) =
        let (tx,_,_) = fromShelleyTx x in tx

newTransactionLayer
    :: forall (n :: NetworkDiscriminant) k t.
        ( t ~ IO Shelley
        , TxWitnessTagFor k
        , WalletKey k
        , Typeable n
        )
    => Proxy n
    -> ProtocolMagic
    -> TransactionLayer t k
newTransactionLayer proxy protocolMagic = TransactionLayer
    { mkStdTx = \acc ks tip ->
        mkTx proxy protocolMagic emptyTxPayload (defaultTTL tip) acc ks
    , initDelegationSelection = _initDelegationSelection
    , mkDelegationJoinTx = _mkDelegationJoinTx
    , mkDelegationQuitTx = _mkDelegationQuitTx
    , decodeSignedTx = _decodeSignedTx
    , minimumFee = _minimumFee @_ @k proxy protocolMagic
    , estimateMaxNumberOfInputs = _estimateMaxNumberOfInputs @_ @k proxy protocolMagic
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
        -> (k 'AddressK XPrv, Passphrase "encryption")
            -- ^ Reward account
        -> (Address -> Maybe (k 'AddressK XPrv, Passphrase "encryption"))
            -- ^ Key store
        -> SlotNo
            -- ^ Tip of the chain, for TTL
        -> CoinSelection
            -- ^ A balanced coin selection where all change addresses have been
            -- assigned.
        -> Either ErrMkTx (Tx, SealedTx)
    _mkDelegationJoinTx poolId acc@(accXPrv, pwd') keyFrom tip cs = do
        let accXPub = toXPub $ getRawKey accXPrv
        let certs =
                if deposit cs > 0 then
                    [ toStakeKeyRegCert  accXPub
                    , toStakePoolDlgCert accXPub poolId
                    ]
                else
                    [ toStakePoolDlgCert accXPub poolId ]

        let mkWits unsigned =
                [ mkShelleyWitness unsigned (getRawKey accXPrv, pwd')
                ]

        let payload = TxPayload certs mkWits
        let ttl = defaultTTL tip
        mkTx proxy protocolMagic payload ttl acc keyFrom cs

    _mkDelegationQuitTx
        :: (k 'AddressK XPrv, Passphrase "encryption")
            -- reward account
        -> (Address -> Maybe (k 'AddressK XPrv, Passphrase "encryption"))
            -- Key store
        -> SlotNo
            -- Tip of the chain, for TTL
        -> CoinSelection
            -- A balanced coin selection where all change addresses have been
            -- assigned.
        -> Either ErrMkTx (Tx, SealedTx)
    _mkDelegationQuitTx acc@(accXPrv, pwd') keyFrom tip cs = do
        let accXPub = toXPub $ getRawKey accXPrv
        let certs = [toStakeKeyDeregCert accXPub]
        let mkWits unsigned =
                [ mkShelleyWitness unsigned (getRawKey accXPrv, pwd')
                ]

        let payload = TxPayload certs mkWits
        let ttl = defaultTTL tip
        mkTx proxy protocolMagic payload ttl acc keyFrom cs

_estimateMaxNumberOfInputs
    :: forall (n :: NetworkDiscriminant) k.
       ( Typeable n
       , TxWitnessTagFor k
       )
    => Proxy n
    -> ProtocolMagic
    -> Quantity "byte" Word16
     -- ^ Transaction max size in bytes
    -> Word8
    -- ^ Number of outputs in transaction
    -> Word8
_estimateMaxNumberOfInputs proxy pm (Quantity maxSize) nOuts =
      fromIntegral $ bisect (lowerBound, upperBound)
  where
    bisect (!inf, !sup)
        | middle == inf && isTooBig sup = inf
        | middle == inf                 = sup
        | isTooBig middle               = bisect (inf, middle)
        | otherwise                     = bisect (middle, sup)
      where
        middle = inf + ((sup - inf) `div` 2)

    growingFactor = 2

    lowerBound = upperBound `div` growingFactor
    upperBound = upperBound_ 1
      where
        upperBound_ !n | isTooBig n = n
                       | otherwise  = upperBound_ (n*growingFactor)

    isTooBig nInps = size > fromIntegral maxSize
      where
        size = computeTxSize proxy pm (txWitnessTagFor @k) Nothing sel
        sel  = dummyCoinSel nInps (fromIntegral nOuts)

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
        Right (Cardano.ShelleyTx txValid) ->
            pure $ toSealed txValid
        Left decodeErr ->
            Left $ ErrDecodeSignedTxWrongPayload (T.pack $ show decodeErr)

_minimumFee
    :: forall (n :: NetworkDiscriminant) k.
       ( Typeable n
       , TxWitnessTagFor k
       )
    => Proxy (n :: NetworkDiscriminant)
    -> ProtocolMagic
    -> FeePolicy
    -> Maybe DelegationAction
    -> CoinSelection
    -> Fee
_minimumFee proxy pm policy action cs =
    computeFee $ computeTxSize proxy pm (txWitnessTagFor @k) action cs
  where
    computeFee :: Integer -> Fee
    computeFee size =
        Fee $ ceiling (a + b*fromIntegral size)
      where
        LinearFee (Quantity a) (Quantity b) _unused = policy

-- TODO: Can this function be re-written by calling @mkTx@ with dummy signing
-- functions?
computeTxSize
    :: forall (n :: NetworkDiscriminant). Typeable n
    => Proxy (n :: NetworkDiscriminant)
    -> ProtocolMagic
    -> TxWitnessTag
    -> Maybe DelegationAction
    -> CoinSelection
    -> Integer
computeTxSize proxy pm witTag action cs =
    withUnderlyingShelleyTx SL.txsize signed
 where

    withUnderlyingShelleyTx
        :: (forall crypto. SL.Tx crypto -> a)
        -> Cardano.Tx Cardano.Shelley
        -> a
    withUnderlyingShelleyTx f (Cardano.ShelleyTx x) = f x

    signed = Cardano.makeSignedTransaction wits unsigned
    unsigned = mkUnsignedTx maxBound cs' wdrls certs
      where
        cs' :: CoinSelection
        cs' = cs
            { CS.outputs = CS.outputs cs <> (dummyOutput <$> change cs)
            , CS.change  = []
            }

        dummyOutput :: Coin -> TxOut
        dummyOutput = TxOut $ Address $ BS.pack (1:replicate 56 0)

        dummyStakeCred = toCardanoStakeCredential
            $ ChimericAccount dummyKeyHashRaw

        dummyPoolId :: Cardano.PoolId
        dummyPoolId = fromMaybe (error "dummyPoolId couldn't be constructed")
            $ Cardano.deserialiseFromRawBytes (Cardano.AsHash Cardano.AsStakePoolKey)
            $ BS.pack $ replicate 32 0

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
        proxy
        (ChimericAccount dummyKeyHashRaw)
        (withdrawal cs)

    (addrWits, certWits) =
        (
            map dummyWitnessUniq (fst <$> CS.inputs cs)
            <> [dummyWitness "0" | null wdrls]
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


    -- Note that the "byron"/bootstrap witnesses are still shelley era
    -- witnesses.
    byronWits = map dummyWitnessUniq $ CS.inputs cs
      where
        dummyWitness :: BL.ByteString -> Address -> Cardano.Witness Cardano.Shelley
        dummyWitness chaff addr = error "todo"

        dummyWitnessUniq :: (TxIn, TxOut) -> Cardano.Witness Cardano.Shelley
        dummyWitnessUniq (TxIn (Hash txid) ix, TxOut addr _) =
            dummyWitness chaff addr
          where
            chaff = L8.pack (show ix) <> BL.fromStrict txid

    sigLen = sizeSigDSIGN $ Proxy @(DSIGN TPraosStandardCrypto)

    keyLen = sizeVerKeyDSIGN $ Proxy @(DSIGN TPraosStandardCrypto)

    _ccLen =  32::Int

    bloatChaff :: Word -> BL.ByteString -> ByteString
    bloatChaff n = BL.toStrict . BL.take (fromIntegral n) . BL.cycle

    -- TODO: Surely we can allow byron witnesses paying for certificates?
    -- Should be no reason to case here.
    wits = case witTag of
        TxWitnessShelleyUTxO ->
            addrWits <> certWits
        TxWitnessByronUTxO ->
           byronWits

lookupPrivateKey
    :: (Address -> Maybe (k 'AddressK XPrv, Passphrase "encryption"))
    -> Address
    -> Either ErrMkTx (k 'AddressK XPrv, Passphrase "encryption")
lookupPrivateKey keyFrom addr =
    maybe (Left $ ErrKeyNotFoundForAddress addr) Right (keyFrom addr)

mkUnsignedTx
    :: Cardano.SlotNo
    -> CoinSelection
    -> [(Cardano.StakeAddress, Cardano.Lovelace)]
    -> [Cardano.Certificate]
    -> Cardano.TxBody Cardano.Shelley
mkUnsignedTx ttl cs wdrls certs =
        Cardano.makeShelleyTransaction
            Cardano.txExtraContentEmpty
            ttl
            (toCardanoLovelace $ Coin $ feeBalance cs)
            (toCardanoTxIn . fst <$> CS.inputs cs)
            (map toCardanoTxOut $ CS.outputs cs)

mkWithdrawals
    :: forall (n :: NetworkDiscriminant). (Typeable n)
    => Proxy n
    -> ChimericAccount
    -> Word64
    -> [(Cardano.StakeAddress, Cardano.Lovelace)]
mkWithdrawals proxy acc amount
    | amount == 0 = mempty
    | otherwise =
        [ ( toCardanoStakeAddress acc -- TODO: Won't work I think. We need to
                                      -- add the networkId
          , toCardanoLovelace $ Coin amount
          )
        ]

-- NOTE: The (+7200) was selected arbitrarily when we were trying to get
-- this working on the FF testnet. Perhaps a better motivated and/or
-- configurable value would be better.
defaultTTL :: SlotNo -> SlotNo
defaultTTL = (+ 7200)

mkShelleyWitness
    :: Cardano.TxBody Cardano.Shelley
    -> (XPrv, Passphrase "encryption")
    -> Cardano.Witness Cardano.Shelley
mkShelleyWitness body xprv =
    Cardano.makeShelleyKeyWitness body (unencrypt xprv)
  where
    unencrypt (xprv, pwd) = Cardano.WitnessPaymentExtendedKey
        $ Cardano.PaymentExtendedSigningKey
        $ CC.xPrvChangePass pwd BS.empty xprv

signWith
    :: ByteString
    -> (XPrv, Passphrase "encryption")
    -> ByteString
signWith msg (prv, pass) =
    CC.unXSignature . CC.sign pass prv $ msg

unsafeMkEd25519 :: XPub -> Ed25519.PublicKey
unsafeMkEd25519 =
    throwCryptoError . Ed25519.publicKey . xpubPublicKey

mkByronWitness
    :: Cardano.TxBody Cardano.Shelley
    -> ProtocolMagic
    -> Address
    -> (XPrv, Passphrase "encryption")
    -> Cardano.Witness Cardano.Shelley
mkByronWitness body protocolMagic addr (prv, Passphrase pwd) =
    Cardano.makeShelleyBootstrapWitness networkId body signingKey
  where
    networkId = undefined
    signingKey = Cardano.ByronSigningKey
        $ Crypto.SigningKey
        $ CC.xPrvChangePass pwd BS.empty prv

--------------------------------------------------------------------------------
-- Extra validations on coin selection
--

type instance ErrValidateSelection (IO Shelley) = ()
