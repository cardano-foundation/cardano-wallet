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
    ( Certificate (..)
    , ErrDecodeSignedTx (..)
    , ErrMkTx (..)
    , ErrValidateSelection
    , TransactionLayer (..)
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
import Data.List
    ( find )
import Data.Maybe
    ( fromMaybe, isJust )
import Data.Proxy
    ( Proxy )
import Data.Quantity
    ( Quantity (..) )
import Data.Word
    ( Word16, Word8 )
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
        let fee = realFee ownedIns outs
        let unsigned = mkUnsignedTx timeToLive ownedIns outs [] fee

        addrWits <- fmap Set.fromList $ forM ownedIns $ \(_, TxOut addr _) -> do
            (k, pwd) <- lookupPrivateKey keyFrom addr
            pure $ mkWitness unsigned (getRawKey k, pwd)

        let metadata = SL.SNothing

        let wits = SL.WitnessSet addrWits mempty mempty
        pure $ toSealed $ SL.Tx unsigned wits metadata

    _mkDelegationJoinTx
        :: FeePolicy
        -> WalletDelegation
        -> PoolId
        -> (k 'AddressK XPrv, Passphrase "encryption")
        -> (Address -> Maybe (k 'AddressK XPrv, Passphrase "encryption"))
        -> SlotId
        -> [(TxIn, TxOut)]
        -> [TxOut]
        -> Either ErrMkTx (Tx, SealedTx)
    _mkDelegationJoinTx policy wDeleg poolId (accXPrv, pwd') keyFrom slot ownedIns outs = do
        let timeToLive = defaultTTL epochLength slot
        let accXPub = toXPub $ getRawKey accXPrv
        let (certs, certsInfo) = case wDeleg of
                (WalletDelegation NotDelegating []) ->
                    ( [ toStakeKeyRegCert  accXPub
                      , toStakePoolDlgCert accXPub poolId
                      ]
                    , [ PoolDelegationCertificate
                      , KeyRegistrationCertificate
                      ]
                    )
                _ ->
                    ( [ toStakePoolDlgCert accXPub poolId ]
                    , [ PoolDelegationCertificate ]
                    )
        -- NOTE
        -- We treat key deposit as a _fee_ when constucting the coin selection,
        -- so that we are sure that there are enough inputs selected to cover
        -- for the deposit. However, the deposit is "implicit" when constructing
        -- a transaction and needs to be removed from the actual fee.
        --
        -- This is why here, we recalculate the fee without the "certificate
        -- fee". The missing amount is the actual deposite.
        let LinearFee a b _ = policy
        let fee = _minimumFee (LinearFee a b (Quantity 0))
                certsInfo
                (CoinSelection ownedIns outs [])
        let unsigned = mkUnsignedTx timeToLive ownedIns outs certs fee
        let metadata = SL.SNothing

        addrWits <- fmap Set.fromList $ forM ownedIns $ \(_, TxOut addr _) -> do
            (k, pwd) <- lookupPrivateKey keyFrom addr
            pure $ mkWitness unsigned (getRawKey k, pwd)
        let certWits =
                Set.singleton (mkWitness unsigned (getRawKey accXPrv, pwd'))
        let wits = SL.WitnessSet (Set.union addrWits certWits) mempty mempty

        pure $ toSealed $ SL.Tx unsigned wits metadata

    -- FIXME:
    -- When de-registering a stake key, we need to return the deposit back to
    -- the user's wallet. This is a bit tricky to handle as it would require
    -- creating a new change output (currently done in the wallet engine) which
    -- differs from the Jörmungandr's way.
    _mkDelegationQuitTx
        :: FeePolicy
        -> (k 'AddressK XPrv, Passphrase "encryption")
        -> (Address -> Maybe (k 'AddressK XPrv, Passphrase "encryption"))
        -> SlotId
        -> [(TxIn, TxOut)]
        -> [TxOut]
        -> Either ErrMkTx (Tx, SealedTx)
    _mkDelegationQuitTx _policy (accXPrv, pwd') keyFrom slot ownedIns outs = do
        let timeToLive = defaultTTL epochLength slot
        let accXPub = toXPub $ getRawKey accXPrv
        let certs = [toStakeKeyDeregCert accXPub]
        let fee = realFee ownedIns outs
        let unsigned = mkUnsignedTx timeToLive ownedIns outs certs fee
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
        size = computeTxSize [] sel
        sel  = dummyCoinSel nInps (fromIntegral nOuts)

dummyCoinSel :: Int -> Int -> CoinSelection
dummyCoinSel nInps nOuts = CoinSelection
    (map (\ix -> (dummyTxIn ix, dummyTxOut)) [0..nInps-1])
    (replicate nOuts dummyTxOut)
    (replicate nOuts (Coin 1))
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
    -> [Certificate]
    -> CoinSelection
    -> Fee
_minimumFee policy certs coinSel =
    computeFee $ computeTxSize certs coinSel
  where
    computeFee :: Integer -> Fee
    computeFee size =
        Fee $ ceiling (a + b*fromIntegral size + if needsDeposit then c else 0)
      where
        LinearFee (Quantity a) (Quantity b) (Quantity c) = policy
        needsDeposit = isJust $ find (== KeyRegistrationCertificate) certs

realFee :: [(TxIn, TxOut)] -> [TxOut] -> Fee
realFee inps outs = Fee
    $ sum (map (getCoin . coin . snd) inps)
    - sum (map (getCoin . coin) outs)

computeTxSize
    :: [Certificate]
    -> CoinSelection
    -> Integer
computeTxSize certs (CoinSelection inps outs chngs) =
    SL.txsize $ SL.Tx unsigned wits metadata
 where
    metadata = SL.SNothing

    unsigned = mkUnsignedTx maxBound inps outs' certs' (realFee inps outs')
      where
        outs' :: [TxOut]
        outs' = outs <> (dummyOutput <$> chngs)

        dummyOutput :: Coin -> TxOut
        dummyOutput = TxOut $ Address $ BS.pack (1:replicate 64 0)

        dummyKeyHash = SL.KeyHash . Hash.UnsafeHash $ BS.pack (1:replicate 64 0)

        certs' = flip map certs $ \case
            PoolDelegationCertificate ->
                Cardano.shelleyDelegateStake dummyKeyHash dummyKeyHash
            KeyRegistrationCertificate ->
                Cardano.shelleyRegisterStakingAddress dummyKeyHash
            KeyDeRegistrationCertificate ->
                Cardano.shelleyDeregisterStakingAddress dummyKeyHash

    addrWits = Set.map dummyWitness $ Set.fromList (fst <$> inps)
      where
        dummyWitness :: TxIn -> SL.WitVKey TPraosStandardCrypto 'SL.Witness
        dummyWitness = mkWitness unsigned . (,mempty) . dummyXPrv

        dummyXPrv :: TxIn -> XPrv
        dummyXPrv (TxIn (Hash txid) ix) =
            unsafeXPrv $ BS.take 128 $ mconcat $ replicate 4 $
                txid <> B8.pack (show ix)

    wits = SL.WitnessSet addrWits mempty mempty

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
    -> Fee
    -> Cardano.ShelleyTxBody
mkUnsignedTx ttl ownedIns outs certs fee =
    let
        Cardano.TxUnsignedShelley unsigned = Cardano.buildShelleyTransaction
            (toCardanoTxIn . fst <$> ownedIns)
            (map toCardanoTxOut outs)
            ttl
            (toCardanoLovelace $ Coin $ getFee fee)
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
