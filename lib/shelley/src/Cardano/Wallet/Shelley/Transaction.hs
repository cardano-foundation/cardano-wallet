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
{-# LANGUAGE TupleSections #-}
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
    , mkWitness
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
    ( Depth (..), NetworkDiscriminant (..), Passphrase, WalletKey (..) )
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
    , toCardanoLovelace
    , toCardanoTxIn
    , toCardanoTxOut
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
import Crypto.Hash.Utils
    ( blake2b256 )
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
    ( Word16, Word8 )
import Ouroboros.Consensus.Shelley.Protocol.Crypto
    ( Crypto (..) )
import Ouroboros.Network.Block
    ( SlotNo )

import qualified Cardano.Api as Cardano
import qualified Cardano.Byron.Codec.Cbor as CBOR
import qualified Cardano.Crypto.Hash.Class as Hash
import qualified Cardano.Crypto.Wallet as CC
import qualified Cardano.Wallet.Primitive.CoinSelection as CS
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Write as CBOR
import qualified Crypto.PubKey.Ed25519 as Ed25519
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Shelley.Spec.Ledger.BaseTypes as SL
import qualified Shelley.Spec.Ledger.Coin as SL
import qualified Shelley.Spec.Ledger.Keys as SL
import qualified Shelley.Spec.Ledger.LedgerState as SL
import qualified Shelley.Spec.Ledger.Tx as SL
import qualified Shelley.Spec.Ledger.TxData as SL
import qualified Shelley.Spec.Ledger.UTxO as SL

newTransactionLayer
    :: forall (n :: NetworkDiscriminant) k t.
        ( t ~ IO Shelley
        , WalletKey k
        )
    => Proxy n
    -> ProtocolMagic
    -> EpochLength
    -> TransactionLayer t k
newTransactionLayer _proxy _protocolMagic epochLength = TransactionLayer
    { mkStdTx = _mkStdTx
    , initDelegationSelection = _initDelegationSelection
    , mkDelegationJoinTx = _mkDelegationJoinTx
    , mkDelegationQuitTx = _mkDelegationQuitTx
    , decodeSignedTx = _decodeSignedTx
    , minimumFee = _minimumFee
    , estimateMaxNumberOfInputs = _estimateMaxNumberOfInputs
    , validateSelection = const $ return ()
    , allowUnbalancedTx = True
    }
  where
    _mkStdTx
        :: (k 'AddressK XPrv, Passphrase "encryption")
            -- Reward account
        -> (Address -> Maybe (k 'AddressK XPrv, Passphrase "encryption"))
            -- Key store
        -> SlotId
            -- Tip of the chain, for TTL
        -> CoinSelection
            -- A balanced coin selection where all change addresses have been
            -- assigned.
        -> Either ErrMkTx (Tx, SealedTx)
    _mkStdTx rewardAcnt keyFrom slot cs = do
        let timeToLive = defaultTTL epochLength slot
        let withdrawals = mempty -- TODO
        let unsigned = mkUnsignedTx timeToLive cs withdrawals []

        addrWits <- fmap Set.fromList $ forM (CS.inputs cs) $ \(_, TxOut addr _) -> do
            (k, pwd) <- lookupPrivateKey keyFrom addr
            pure $ mkWitness unsigned (getRawKey k, pwd)

        withdrawalsWits <- pure mempty

        let metadata = SL.SNothing

        let wits = SL.WitnessSet addrWits withdrawalsWits mempty
        pure $ toSealed $ SL.Tx unsigned wits metadata

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
            -- Pool Id to which we're planning to delegate
        -> (k 'AddressK XPrv, Passphrase "encryption")
            -- Reward account
        -> (Address -> Maybe (k 'AddressK XPrv, Passphrase "encryption"))
            -- Key store
        -> SlotId
            -- Tip of the chain, for TTL
        -> CoinSelection
            -- A balanced coin selection where all change addresses have been
            -- assigned.
        -> Either ErrMkTx (Tx, SealedTx)
    _mkDelegationJoinTx poolId (accXPrv, pwd') keyFrom slot cs = do
        let timeToLive = defaultTTL epochLength slot
        let accXPub = toXPub $ getRawKey accXPrv
        let certs =
                if deposit cs > 0 then
                    [ toStakeKeyRegCert  accXPub
                    , toStakePoolDlgCert accXPub poolId
                    ]
                else
                    [ toStakePoolDlgCert accXPub poolId ]

        let unsigned = mkUnsignedTx timeToLive cs mempty certs
        let metadata = SL.SNothing

        addrWits <- fmap Set.fromList $ forM (inputs cs) $ \(_, TxOut addr _) -> do
            (k, pwd) <- lookupPrivateKey keyFrom addr
            pure $ mkWitness unsigned (getRawKey k, pwd)
        let certWits =
                Set.singleton (mkWitness unsigned (getRawKey accXPrv, pwd'))
        let wits = SL.WitnessSet (Set.union addrWits certWits) mempty mempty

        pure $ toSealed $ SL.Tx unsigned wits metadata

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
    _mkDelegationQuitTx (accXPrv, pwd') keyFrom slot cs = do
        let timeToLive = defaultTTL epochLength slot
        let accXPub = toXPub $ getRawKey accXPrv
        let certs = [toStakeKeyDeregCert accXPub]

        let unsigned = mkUnsignedTx timeToLive cs mempty certs
        let metadata = SL.SNothing

        addrWits <- fmap Set.fromList $ forM (inputs cs) $ \(_, TxOut addr _) -> do
            (k, pwd) <- lookupPrivateKey keyFrom addr
            pure $ mkWitness unsigned (getRawKey k, pwd)
        let certWits =
                Set.singleton (mkWitness unsigned (getRawKey accXPrv, pwd'))
        let wits = SL.WitnessSet (Set.union addrWits certWits) mempty mempty

        pure $ toSealed $ SL.Tx unsigned wits metadata

_estimateMaxNumberOfInputs
    :: Quantity "byte" Word16
     -- ^ Transaction max size in bytes
    -> Word8
    -- ^ Number of outputs in transaction
    -> Word8
_estimateMaxNumberOfInputs (Quantity maxSize) nOuts =
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
        size = computeTxSize Nothing sel
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
    case Cardano.txSignedFromCBOR bytes of
        Right (Cardano.TxSignedShelley txValid) ->
            pure $ toSealed txValid
        Right (Cardano.TxSignedByron{}) ->
            case CBOR.deserialiseFromBytes CBOR.decodeSignedTx (BL.fromStrict bytes) of
                Left e ->
                    Left $ ErrDecodeSignedTxWrongPayload $ T.pack $ show e
                Right (_, ((inps, outs), _)) -> Right
                    ( W.Tx
                        { W.txId = Hash
                            $ blake2b256
                            $ CBOR.toStrictByteString
                            $ CBOR.encodeTx (inps, outs)
                        , W.resolvedInputs = (,Coin 0) <$> inps
                        , W.outputs = outs
                        }
                    , SealedTx bytes
                    )
        Left apiErr ->
            Left $ ErrDecodeSignedTxWrongPayload (Cardano.renderApiError apiErr)

_minimumFee
    :: FeePolicy
    -> Maybe DelegationAction
    -> CoinSelection
    -> Fee
_minimumFee policy action cs =
    computeFee $ computeTxSize action cs
  where
    computeFee :: Integer -> Fee
    computeFee size =
        Fee $ ceiling (a + b*fromIntegral size)
      where
        LinearFee (Quantity a) (Quantity b) _unused = policy

computeTxSize
    :: Maybe DelegationAction
    -> CoinSelection
    -> Integer
computeTxSize action cs =
    SL.txsize $ SL.Tx unsigned wits metadata
 where
    metadata = SL.SNothing

    unsigned = mkUnsignedTx maxBound cs' withdrawals certs
      where
        cs' :: CoinSelection
        cs' = cs
            { CS.outputs = CS.outputs cs <> (dummyOutput <$> change cs)
            , CS.change  = []
            }

        withdrawals :: Map (SL.RewardAcnt TPraosStandardCrypto) SL.Coin
        withdrawals = mempty

        dummyOutput :: Coin -> TxOut
        dummyOutput = TxOut $ Address $ BS.pack (1:replicate 56 0)

        dummyKeyHash = SL.KeyHash . Hash.UnsafeHash $ BS.pack (replicate 28 0)

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

    (addrWits, certWits) =
        ( Set.map dummyWitnessUniq $ Set.fromList (fst <$> CS.inputs cs)
        , case action of
            Nothing -> Set.empty
            Just{}  -> Set.singleton (dummyWitness "a")
        )
      where
        dummyWitness :: BL.ByteString -> SL.WitVKey TPraosStandardCrypto 'SL.Witness
        dummyWitness chaff = SL.WitVKey key sig
          where
            sig = SignedDSIGN
                $ fromMaybe (error "error creating dummy witness sig")
                $ rawDeserialiseSigDSIGN
                $ bloatChaff sigLen
            key = SL.VKey
                $ fromMaybe (error "error creating dummy witness ver key")
                $ rawDeserialiseVerKeyDSIGN
                $ bloatChaff keyLen
            sigLen = sizeSigDSIGN $ Proxy @(DSIGN TPraosStandardCrypto)
            keyLen = sizeVerKeyDSIGN $ Proxy @(DSIGN TPraosStandardCrypto)
            bloatChaff n = BL.toStrict $ BL.take (fromIntegral n) $ BL.cycle chaff

        dummyWitnessUniq :: TxIn -> SL.WitVKey TPraosStandardCrypto 'SL.Witness
        dummyWitnessUniq (TxIn (Hash txid) ix) = dummyWitness chaff
          where
            chaff = L8.pack (show ix) <> BL.fromStrict txid

    wits = SL.WitnessSet (Set.union addrWits certWits) mempty mempty

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
mkUnsignedTx ttl cs withdrawals certs =
    let
        Cardano.TxUnsignedShelley unsigned = Cardano.buildShelleyTransaction
            (toCardanoTxIn . fst <$> CS.inputs cs)
            (map toCardanoTxOut $ CS.outputs cs)
            ttl
            (toCardanoLovelace $ Coin $ feeBalance cs)
            certs
            (Cardano.WithdrawalsShelley $ SL.Wdrl withdrawals)
            Nothing -- Update
            Nothing -- Metadata hash
    in
        unsigned

-- TODO: The SlotId-SlotNo conversion based on epoch length would not
-- work if the epoch length changed in a hard fork.

-- NOTE: The (+7200) was selected arbitrarily when we were trying to get
-- this working on the FF testnet. Perhaps a better motivated and/or
-- configurable value would be better.
defaultTTL :: EpochLength -> SlotId -> SlotNo
defaultTTL epochLength slot =
    (toSlotNo epochLength slot) + 7200

mkWitness
    :: SL.TxBody TPraosStandardCrypto
    -> (XPrv, Passphrase "encryption")
    -> SL.WitVKey TPraosStandardCrypto 'SL.Witness
mkWitness body (prv, pwd) =
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

--------------------------------------------------------------------------------
-- Extra validations on coin selection
--

type instance ErrValidateSelection (IO Shelley) = ()
