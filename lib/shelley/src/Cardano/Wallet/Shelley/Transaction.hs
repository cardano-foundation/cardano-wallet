{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
    , toByronNetworkMagic
    , toCardanoLovelace
    , toCardanoTxIn
    , toCardanoTxOut
    , toHDPayloadAddress
    , toSealed
    , toSlotNo
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

import qualified Cardano.Api as Cardano
import qualified Cardano.Api.Typed as CardanoTyped
import qualified Cardano.Chain.Common as Byron
import qualified Cardano.Crypto as Crypto
import qualified Cardano.Crypto.Hash.Class as Hash
import qualified Cardano.Crypto.Wallet as CC
import qualified Cardano.Wallet.Primitive.CoinSelection as CS
import qualified Crypto.PubKey.Ed25519 as Ed25519
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
data TxPayload c = TxPayload
    { _certificates :: [Cardano.Certificate]
      -- ^ Certificates to be included in the transactions.

    , _extraWitnesses :: SL.TxBody c -> SL.WitnessSet c
      -- ^ Create payload-specific witesses given the unsigned transaction body.
      --
      -- Caller has the freedom and responsibility to provide the correct
      -- witnesses for what they're trying to do.
    }

emptyTxPayload :: Crypto c => TxPayload c
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
    -> TxPayload TPraosStandardCrypto
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
            addrWits <- fmap Set.fromList $ forM (CS.inputs cs) $ \(_, TxOut addr _) -> do
                (k, pwd) <- lookupPrivateKey keyFrom addr
                pure $ mkShelleyWitness unsigned (getRawKey k, pwd)

            let wdrlsWits
                    | Map.null wdrls = Set.empty
                    | otherwise = Set.singleton $
                      mkShelleyWitness unsigned (getRawKey rewardAcnt, pwdAcnt)

            pure $ (SL.WitnessSet (addrWits <> wdrlsWits) mempty mempty)
                <> mkExtraWits unsigned

        TxWitnessByronUTxO -> do
            bootstrapWits <- fmap Set.fromList $ forM (CS.inputs cs) $ \(_, TxOut addr _) -> do
                (k, pwd) <- lookupPrivateKey keyFrom addr
                pure $ mkByronWitness unsigned pm addr (getRawKey k, pwd)
            pure $ SL.WitnessSet mempty mempty bootstrapWits
                <> mkExtraWits unsigned

    let metadata = SL.SNothing

    pure $ toSealed $ SL.Tx unsigned wits metadata

newTransactionLayer
    :: forall (n :: NetworkDiscriminant) k t.
        ( t ~ IO Shelley
        , TxWitnessTagFor k
        , WalletKey k
        , Typeable n
        )
    => Proxy n
    -> ProtocolMagic
    -> EpochLength
    -> TransactionLayer t k
newTransactionLayer proxy protocolMagic epochLength = TransactionLayer
    { mkStdTx = \acc ks tip ->
        mkTx proxy protocolMagic emptyTxPayload (defaultTTL epochLength tip) acc ks
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
        -> SlotId
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

        let mkWits unsigned = SL.WitnessSet
                (Set.singleton (mkShelleyWitness unsigned (getRawKey accXPrv, pwd')))
                mempty
                mempty

        let payload = TxPayload certs mkWits
        let ttl = defaultTTL epochLength tip
        mkTx proxy protocolMagic payload ttl acc keyFrom cs

    _mkDelegationQuitTx
        :: (k 'AddressK XPrv, Passphrase "encryption")
            -- reward account
        -> (Address -> Maybe (k 'AddressK XPrv, Passphrase "encryption"))
            -- Key store
        -> SlotId
            -- Tip of the chain, for TTL
        -> CoinSelection
            -- A balanced coin selection where all change addresses have been
            -- assigned.
        -> Either ErrMkTx (Tx, SealedTx)
    _mkDelegationQuitTx acc@(accXPrv, pwd') keyFrom tip cs = do
        let accXPub = toXPub $ getRawKey accXPrv
        let certs = [toStakeKeyDeregCert accXPub]
        let mkWits unsigned = SL.WitnessSet
                (Set.singleton (mkShelleyWitness unsigned (getRawKey accXPrv, pwd')))
                mempty
                mempty

        let payload = TxPayload certs mkWits
        let ttl = defaultTTL epochLength tip
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
    case CardanoTyped.deserialiseFromCBOR CardanoTyped.AsShelleyTx bytes of
        Right (CardanoTyped.ShelleyTx txValid) ->
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

computeTxSize
    :: forall (n :: NetworkDiscriminant). Typeable n
    => Proxy (n :: NetworkDiscriminant)
    -> ProtocolMagic
    -> TxWitnessTag
    -> Maybe DelegationAction
    -> CoinSelection
    -> Integer
computeTxSize proxy pm witTag action cs =
    SL.txsize $ SL.Tx unsigned wits metadata
 where
    metadata = SL.SNothing

    unsigned = mkUnsignedTx maxBound cs' wdrls certs
      where
        cs' :: CoinSelection
        cs' = cs
            { CS.outputs = CS.outputs cs <> (dummyOutput <$> change cs)
            , CS.change  = []
            }

        dummyOutput :: Coin -> TxOut
        dummyOutput = TxOut $ Address $ BS.pack (1:replicate 56 0)

        dummyKeyHash = SL.KeyHash . Hash.UnsafeHash $ dummyKeyHashRaw

        certs = case action of
            Nothing -> []
            Just RegisterKeyAndJoin{} ->
                [ Cardano.shelleyRegisterStakingAddress dummyKeyHash
                , Cardano.shelleyDelegateStake dummyKeyHash dummyKeyHash
                ]
            Just Join{} ->
                [ Cardano.shelleyDelegateStake dummyKeyHash dummyKeyHash
                ]
            Just Quit ->
                [ Cardano.shelleyDeregisterStakingAddress dummyKeyHash
                ]

    dummyKeyHashRaw = BS.pack (replicate 28 0)

    wdrls = mkWithdrawals
        proxy
        (ChimericAccount dummyKeyHashRaw)
        (withdrawal cs)

    (addrWits, certWits) =
        ( Set.union
            (Set.map dummyWitnessUniq $ Set.fromList (fst <$> CS.inputs cs))
            (if Map.null wdrls then Set.empty else Set.singleton (dummyWitness "0"))
        , case action of
            Nothing -> Set.empty
            Just{}  -> Set.singleton (dummyWitness "a")
        )
      where
        dummyWitness :: BL.ByteString -> SL.WitVKey TPraosStandardCrypto 'SL.Witness
        dummyWitness chaff = SL.WitVKey key sig
          where
            key = SL.VKey
                $ fromMaybe (error "error creating dummy witness ver key")
                $ rawDeserialiseVerKeyDSIGN
                $ bloatChaff keyLen chaff

            sig = SignedDSIGN
                $ fromMaybe (error "error creating dummy witness sig")
                $ rawDeserialiseSigDSIGN
                $ bloatChaff sigLen chaff

        dummyWitnessUniq :: TxIn -> SL.WitVKey TPraosStandardCrypto 'SL.Witness
        dummyWitnessUniq (TxIn (Hash txid) ix) =
            dummyWitness chaff
          where
            chaff = L8.pack (show ix) <> BL.fromStrict txid

    byronWits = Set.map dummyWitnessUniq $ Set.fromList (CS.inputs cs)
      where
        dummyWitness :: BL.ByteString -> Address -> SL.BootstrapWitness TPraosStandardCrypto
        dummyWitness chaff addr =
            SL.BootstrapWitness key sig cc padding
          where
            key = SL.VKey
                $ fromMaybe (error "error creating dummy witness ver key")
                $ rawDeserialiseVerKeyDSIGN
                $ bloatChaff keyLen chaff

            sig = SignedDSIGN
                $ fromMaybe (error "error creating dummy witness sig")
                $ rawDeserialiseSigDSIGN
                $ bloatChaff sigLen chaff

            cc = SL.ChainCode
                $ bloatChaff ccLen "0"

            padding = SL.byronVerKeyAddressPadding
                $ Byron.mkAttributes
                $ Byron.AddrAttributes
                   (toHDPayloadAddress addr)
                   (toByronNetworkMagic pm)

        dummyWitnessUniq :: (TxIn, TxOut) -> SL.BootstrapWitness TPraosStandardCrypto
        dummyWitnessUniq (TxIn (Hash txid) ix, TxOut addr _) =
            dummyWitness chaff addr
          where
            chaff = L8.pack (show ix) <> BL.fromStrict txid

    sigLen = sizeSigDSIGN $ Proxy @(DSIGN TPraosStandardCrypto)

    keyLen = sizeVerKeyDSIGN $ Proxy @(DSIGN TPraosStandardCrypto)

    ccLen =  32

    bloatChaff :: Word -> BL.ByteString -> ByteString
    bloatChaff n = BL.toStrict . BL.take (fromIntegral n) . BL.cycle

    wits = case witTag of
        TxWitnessShelleyUTxO ->
            SL.WitnessSet (Set.union addrWits certWits) mempty mempty
        TxWitnessByronUTxO ->
           SL.WitnessSet mempty mempty byronWits

lookupPrivateKey
    :: (Address -> Maybe (k 'AddressK XPrv, Passphrase "encryption"))
    -> Address
    -> Either ErrMkTx (k 'AddressK XPrv, Passphrase "encryption")
lookupPrivateKey keyFrom addr =
    maybe (Left $ ErrKeyNotFoundForAddress addr) Right (keyFrom addr)

mkUnsignedTx
    :: Cardano.SlotNo
    -> CoinSelection
    -> Map (SL.RewardAcnt TPraosStandardCrypto) SL.Coin
    -> [Cardano.Certificate]
    -> Cardano.ShelleyTxBody
mkUnsignedTx ttl cs wdrls certs =
    let
        Cardano.TxUnsignedShelley unsigned = Cardano.buildShelleyTransaction
            (toCardanoTxIn . fst <$> CS.inputs cs)
            (map toCardanoTxOut $ CS.outputs cs)
            ttl
            (toCardanoLovelace $ Coin $ feeBalance cs)
            certs
            (Cardano.WithdrawalsShelley $ SL.Wdrl wdrls)
            Nothing -- Update
            Nothing -- Metadata hash
    in
        unsigned

mkWithdrawals
    :: forall (n :: NetworkDiscriminant). (Typeable n)
    => Proxy n
    -> ChimericAccount
    -> Word64
    -> Map (SL.RewardAcnt TPraosStandardCrypto) SL.Coin
mkWithdrawals proxy (ChimericAccount keyHash) amount
    | amount == 0 = mempty
    | otherwise = Map.fromList
        [ ( SL.RewardAcnt (fromNetworkDiscriminant proxy) keyHashObj
          , SL.Coin $ fromIntegral amount
          )
        ]
  where
    keyHashObj = SL.KeyHashObj $ SL.KeyHash $ Hash.UnsafeHash keyHash

-- TODO: The SlotId-SlotNo conversion based on epoch length would not
-- work if the epoch length changed in a hard fork.

-- NOTE: The (+7200) was selected arbitrarily when we were trying to get
-- this working on the FF testnet. Perhaps a better motivated and/or
-- configurable value would be better.
defaultTTL :: SlotNo -> SlotNo
defaultTTL = (+ 7200)

mkShelleyWitness
    :: SL.TxBody TPraosStandardCrypto
    -> (XPrv, Passphrase "encryption")
    -> SL.WitVKey TPraosStandardCrypto 'SL.Witness
mkShelleyWitness body (prv, pwd) =
    SL.WitVKey key sig
  where
    sig = SignedDSIGN
        $ fromMaybe (error "error converting signatures")
        $ rawDeserialiseSigDSIGN
        $ serialize' (SL.hashTxBody body) `signWith` (prv, pwd)

    key = SL.VKey
        $ VerKeyEd25519DSIGN
        $ unsafeMkEd25519
        $ toXPub prv

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
    :: SL.TxBody TPraosStandardCrypto
    -> ProtocolMagic
    -> Address
    -> (XPrv, Passphrase "encryption")
    -> SL.BootstrapWitness TPraosStandardCrypto
mkByronWitness body protocolMagic addr (prv, Passphrase pwd) =
    SL.makeBootstrapWitness txHash signingKey addrAttr
  where
    (SL.TxId txHash) = SL.txid body
    signingKey = Crypto.SigningKey $ CC.xPrvChangePass pwd BS.empty prv
    addrAttr = Byron.mkAttributes $ Byron.AddrAttributes
        (toHDPayloadAddress addr)
        (toByronNetworkMagic protocolMagic)

--------------------------------------------------------------------------------
-- Extra validations on coin selection
--

type instance ErrValidateSelection (IO Shelley) = ()
