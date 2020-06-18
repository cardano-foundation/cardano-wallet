{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
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
    , _minimumFee
    , _decodeSignedTx
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
    ( CoinSelection (..) )
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
    , WalletDelegation (..)
    , WalletDelegationStatus (..)
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
    ( ErrDecodeSignedTx (..)
    , ErrMkTx (..)
    , ErrValidateSelection
    , TransactionLayer (..)
    , WithDelegation (..)
    )
import Cardano.Wallet.Unsafe
    ( unsafeXPrv )
import Control.Monad
    ( forM )
import Crypto.Error
    ( throwCryptoError )
import Crypto.Hash.Utils
    ( blake2b256 )
import Data.ByteString
    ( ByteString )
import Data.Maybe
    ( fromMaybe )
import Data.Proxy
    ( Proxy )
import Data.Quantity
    ( Quantity (..) )
import Data.Word
    ( Word16, Word8 )
import Fmt
    ( Buildable (..) )
import Ouroboros.Network.Block
    ( SlotNo )

import qualified Cardano.Api as Cardano
import qualified Cardano.Byron.Codec.Cbor as CBOR
import qualified Cardano.Crypto.Hash.Class as Hash
import qualified Cardano.Crypto.Wallet as CC
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Write as CBOR
import qualified Crypto.PubKey.Ed25519 as Ed25519
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Shelley.Spec.Ledger.BaseTypes as SL
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
        :: (Address -> Maybe (k 'AddressK XPrv, Passphrase "encryption"))
        -> SlotId -- ^ The current slot
        -> [(TxIn, TxOut)]
        -> [TxOut]
        -> Either ErrMkTx (Tx, SealedTx)
    _mkStdTx keyFrom slot ownedIns outs = do
        let timeToLive = defaultTTL epochLength slot
        let unsigned = mkUnsignedTx timeToLive ownedIns outs []

        addrWits <- fmap Set.fromList $ forM ownedIns $ \(_, TxOut addr _) -> do
            (k, pwd) <- lookupPrivateKey keyFrom addr
            pure $ mkWitness unsigned (getRawKey k, pwd)

        let metadata = SL.SNothing

        let wits = SL.WitnessSet addrWits mempty mempty
        pure $ toSealed $ SL.Tx unsigned wits metadata

    _mkDelegationJoinTx
        :: WalletDelegation
        -> PoolId
        -> (k 'AddressK XPrv, Passphrase "encryption")
        -> (Address -> Maybe (k 'AddressK XPrv, Passphrase "encryption"))
        -> SlotId
        -> [(TxIn, TxOut)]
        -> [TxOut]
        -> Either ErrMkTx (Tx, SealedTx)
    _mkDelegationJoinTx wDeleg poolId (accXPrv, pwd') keyFrom slot ownedIns outs = do
        let timeToLive = defaultTTL epochLength slot
        let accXPub = toXPub $ getRawKey accXPrv
        let certs = case wDeleg of
                (WalletDelegation NotDelegating []) ->
                    [ toStakeKeyRegCert  accXPub
                    , toStakePoolDlgCert accXPub poolId
                    ]
                _ ->
                    [ toStakePoolDlgCert accXPub poolId ]
        let unsigned = mkUnsignedTx timeToLive ownedIns outs certs
        let metadata = SL.SNothing

        addrWits <- fmap Set.fromList $ forM ownedIns $ \(_, TxOut addr _) -> do
            (k, pwd) <- lookupPrivateKey keyFrom addr
            pure $ mkWitness unsigned (getRawKey k, pwd)
        let certWits =
                Set.singleton (mkWitness unsigned (getRawKey accXPrv, pwd'))
        let wits = SL.WitnessSet (Set.union addrWits certWits) mempty mempty

        pure $ toSealed $ SL.Tx unsigned wits metadata

    _mkDelegationQuitTx
        :: (k 'AddressK XPrv, Passphrase "encryption")
        -> (Address -> Maybe (k 'AddressK XPrv, Passphrase "encryption"))
        -> SlotId
        -> [(TxIn, TxOut)]
        -> [TxOut]
        -> Either ErrMkTx (Tx, SealedTx)
    _mkDelegationQuitTx (accXPrv, pwd') keyFrom slot ownedIns outs = do
        let timeToLive = defaultTTL epochLength slot
        let accXPub = toXPub $ getRawKey accXPrv
        let cert = [toStakeKeyDeregCert accXPub]
        let unsigned = mkUnsignedTx timeToLive ownedIns outs cert
        let metadata = SL.SNothing

        addrWits <- fmap Set.fromList $ forM ownedIns $ \(_, TxOut addr _) -> do
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
    _estimateMaxNumberOfInputs _ _ =
        -- FIXME Implement.
        100

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
    -> WithDelegation
    -> CoinSelection
    -> Fee
_minimumFee policy (WithDelegation withDelegation) (CoinSelection inps outs chngs) =
    computeFee $ SL.txsize $
        SL.Tx unsigned wits metadata
  where
    computeFee :: Integer -> Fee
    computeFee size =
        Fee $ ceiling (a + b*fromIntegral size)
      where
        LinearFee (Quantity a) (Quantity b) (Quantity _unused) = policy

    wits = SL.WitnessSet addrWits mempty mempty

    metadata = SL.SNothing

    unsigned = mkUnsignedTx maxBound inps outs'
        $ if withDelegation then [dummyRegisterCert, dummyDelegateCert] else []
      where
        outs' :: [TxOut]
        outs' = outs <> (dummyOutput <$> chngs)

        dummyOutput :: Coin -> TxOut
        dummyOutput = TxOut $ Address $ BS.pack (1:replicate 64 0)

        dummyKeyHash = SL.KeyHash . Hash.UnsafeHash $ BS.pack (1:replicate 64 0)
        dummyRegisterCert = Cardano.shelleyRegisterStakingAddress dummyKeyHash
        dummyDelegateCert = Cardano.shelleyDelegateStake dummyKeyHash dummyKeyHash

    addrWits = Set.map dummyWitness $ Set.fromList (fst <$> inps)
      where
        dummyWitness :: TxIn -> SL.WitVKey TPraosStandardCrypto
        dummyWitness = mkWitness unsigned . (,mempty) . dummyXPrv

        dummyXPrv :: TxIn -> XPrv
        dummyXPrv (TxIn (Hash txid) ix) =
            unsafeXPrv $ BS.take 128 $ mconcat $ replicate 4 $
                txid <> B8.pack (show ix)

lookupPrivateKey
    :: (Address -> Maybe (k 'AddressK XPrv, Passphrase "encryption"))
    -> Address
    -> Either ErrMkTx (k 'AddressK XPrv, Passphrase "encryption")
lookupPrivateKey keyFrom addr =
    maybe (Left $ ErrKeyNotFoundForAddress addr) Right (keyFrom addr)

mkUnsignedTx
    :: Cardano.SlotNo
    -> [(TxIn, TxOut)]
    -> [TxOut]
    -> [Cardano.Certificate]
        -- ^ TODO: This should be not be a Cardano type, but a wallet type.
    -> Cardano.ShelleyTxBody
mkUnsignedTx ttl ownedIns outs certs =
    let
        Cardano.TxUnsignedShelley unsigned = Cardano.buildShelleyTransaction
            (toCardanoTxIn . fst <$> ownedIns)
            (map toCardanoTxOut outs)
            ttl
            (realFee (snd <$> ownedIns) outs)
            certs
            (Cardano.WithdrawalsShelley $ SL.Wdrl mempty) -- Withdrawals
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

realFee :: [TxOut] -> [TxOut] -> Cardano.Lovelace
realFee inps outs = toCardanoLovelace $ Coin
    $ sum (map (getCoin . coin) inps)
    - sum (map (getCoin . coin) outs)

mkWitness
    :: SL.TxBody TPraosStandardCrypto
    -> (XPrv, Passphrase "encryption")
    -> SL.WitVKey TPraosStandardCrypto
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

-- | Transaction with 0 output amount is tried
data ErrInvalidTxOutAmount -- FIXME: = ErrInvalidTxOutAmount

instance Buildable ErrInvalidTxOutAmount where
    build _ = "Invalid coin selection: at least one output is null."

type instance ErrValidateSelection (IO Shelley) = ErrInvalidTxOutAmount
