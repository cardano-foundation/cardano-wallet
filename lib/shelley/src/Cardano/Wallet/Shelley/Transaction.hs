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
    , realFee
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
import qualified Shelley.Spec.Ledger.Keys as SL
import qualified Shelley.Spec.Ledger.LedgerState as SL
import qualified Shelley.Spec.Ledger.Tx as SL
import qualified Shelley.Spec.Ledger.TxData as SL
import qualified Shelley.Spec.Ledger.UTxO as SL


-- | Type encapsulating what we need to know to add things -- payloads,
-- certificates -- to a transaction.
--
-- Designed to allow us to have /one/ @_mkTx@ which doesn't care whether we
-- include certificates or not.
data TxPayload c = TxPayload
    { _certificates :: [Cardano.Certificate]
      -- ^ Certificates to be included in the transactions.

    , _extraWitnesses :: SL.TxBody c -> SL.WitnessSet c
      -- ^ Create payload-specific witesses given the unsigned transaction body.

    , _fee :: Fee
      -- ^ When constructing the @TxPayload@ you are responsible to calculate
      -- the fee. This is because the payload may affect the fee.
      --
      -- TODO: Perhaps we could specify some kind of @extraCost@ instead of the
      -- absolute @fee@, but didn't seem to fit with our current @minimumFee@
      -- and @realFee@.
    }

emptyTxPayload :: Crypto c => Fee -> TxPayload c
emptyTxPayload = TxPayload mempty (const mempty)


mkTx
    :: WalletKey k
    => TxPayload TPraosStandardCrypto
    -> (Address -> Maybe (k 'AddressK XPrv, Passphrase "encryption"))
    -> SlotNo -- ^ Time to live
    -> [(TxIn, TxOut)]
    -> [TxOut]
    -> Either ErrMkTx (Tx, SealedTx)
mkTx (TxPayload certs mkExtraWits fee) keyFrom timeToLive ownedIns outs = do
    let unsigned = mkUnsignedTx timeToLive ownedIns outs certs fee

    addrWits <- fmap Set.fromList $ forM ownedIns $ \(_, TxOut addr _) -> do
        (k, pwd) <- lookupPrivateKey keyFrom addr
        pure $ mkWitness unsigned (getRawKey k, pwd)

    let metadata = SL.SNothing

    let wits = (SL.WitnessSet addrWits mempty mempty) <> mkExtraWits unsigned
    pure $ toSealed $ SL.Tx unsigned wits metadata

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
    { mkStdTx = \keyFrom slot ownedIns outs -> do
        let fee = realFee ownedIns outs
        let timeToLive = defaultTTL epochLength slot
        mkTx (emptyTxPayload fee) keyFrom timeToLive ownedIns outs
    , mkDelegationJoinTx = _mkDelegationJoinTx
    , mkDelegationQuitTx = _mkDelegationQuitTx
    , decodeSignedTx = _decodeSignedTx
    , minimumFee = _minimumFee
    , estimateMaxNumberOfInputs = _estimateMaxNumberOfInputs
    , validateSelection = const $ return ()
    , allowUnbalancedTx = True
    }
  where
    _mkDelegationJoinTx
        :: FeePolicy
            -- Latest fee policy
        -> WalletDelegation
            -- Wallet current delegation status
        -> PoolId
            -- Pool Id to which we're planning to delegate
        -> (k 'AddressK XPrv, Passphrase "encryption")
            -- Reward account
        -> (Address -> Maybe (k 'AddressK XPrv, Passphrase "encryption"))
            -- Key store
        -> SlotId
            -- Tip of the chain, for TTL
        -> [(TxIn, TxOut)]
            --  Resolved inputs
        -> [TxOut]
            -- Outputs
        -> [TxOut]
            -- Change, with assigned address
        -> Either ErrMkTx (Tx, SealedTx)
    _mkDelegationJoinTx policy dlg poolId (accXPrv, pwd') keyFrom slot inps outs chgs = do
        let accXPub = toXPub $ getRawKey accXPrv
        let timeToLive = defaultTTL epochLength slot
        let (certs, certsInfo) = case dlg of
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
        -- fee". The missing amount is the actual deposit.
        let LinearFee a b _ = policy
        let fee = _minimumFee (LinearFee a b (Quantity 0))
                certsInfo
                (CoinSelection inps (outs ++ chgs) [])

        let certWits unsigned =
                SL.WitnessSet
                    (Set.singleton (mkWitness unsigned (getRawKey accXPrv, pwd')))
                    mempty -- msig wits
                    mempty -- boot wits

        let payload = TxPayload certs certWits fee
        -- NOTE: mkDelegationJoinTx, and mkStdTx differs in their arguments
        -- in mkDelegationJoinTx, we need to add the change outputs to the other
        -- outputs.
        mkTx payload keyFrom timeToLive inps (outs ++ chgs)

    _mkDelegationQuitTx
        :: FeePolicy
            -- Latest fee policy
        -> (k 'AddressK XPrv, Passphrase "encryption")
            -- reward account
        -> (Address -> Maybe (k 'AddressK XPrv, Passphrase "encryption"))
            -- Key store
        -> SlotId
            -- Tip of the chain, for TTL
        -> [(TxIn, TxOut)]
            -- ^ Resolved inputs
        -> [TxOut]
            -- ^ Outputs
        -> [TxOut]
            -- ^ Change, with assigned address
        -> Either ErrMkTx (Tx, SealedTx)
    _mkDelegationQuitTx policy (accXPrv, pwd') keyFrom slot inps outs chgs = do
        let accXPub = toXPub $ getRawKey accXPrv
        let timeToLive = defaultTTL epochLength slot
        let certs = [toStakeKeyDeregCert accXPub]

        -- NOTE / FIXME
        -- When registering a stake key, users gave a deposit fixed by the fee
        -- policy. When de-registing a key, the deposit should be given back.
        --
        -- The deposit doesn't come from an explicit input, but rather, from an
        -- implicit balance available from the input side. So, output are
        -- allowed to create more than what's consumed as inputs. We therefore
        -- add the deposit to the first change output available, which doesn't
        -- change the fee in Shelley. Still, the real fee are computed from the
        -- differences between inputs and outputs BEFORE the deposit is added.
        --
        -- There's on "gotcha" with this method: it isn't resilient to protocol
        -- updates. So, if a key is registered at a given epoch, with a deposit
        -- X and later on, the deposit amount is changed to Y, Y /= X, then,
        -- when de-registering the stake key, there'll be a mismatch between the
        -- amount expected by the ledger, and the amount given by the wallet.
        -- Ideally, the deposit amount should be deduced from the transaction
        -- that was used to register the key! We don't have any ways at the
        -- moment to lookup such a transaction from the database and making it
        -- so would require some extensive changes that are quite risky to
        -- undergo _now_. So long as the deposit key isn't updated via protocol
        -- updates, the present solution will work fine.
        chgs' <- mapFirst (withDeposit policy) chgs
        let fee = realFee inps (outs ++ chgs)

        let certWits unsigned =
                SL.WitnessSet
                    (Set.singleton (mkWitness unsigned (getRawKey accXPrv, pwd')))
                    mempty -- msig wits
                    mempty -- boot wits

        let payload = TxPayload certs certWits fee
        mkTx payload keyFrom timeToLive inps (outs ++ chgs')
      where
        withDeposit :: FeePolicy -> TxOut -> TxOut
        withDeposit (LinearFee _ _ (Quantity deposit)) (TxOut addr (Coin c)) =
            TxOut addr (Coin (c + round deposit))

        mapFirst :: (a -> a) -> [a] -> Either ErrMkTx [a]
        mapFirst _     [] = Left ErrChangeIsEmptyForRetirement
        mapFirst fn (h:q) = Right (fn h:q)

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
        dummyOutput = TxOut $ Address $ BS.pack (1:replicate 56 0)

        dummyKeyHash = SL.KeyHash . Hash.UnsafeHash $ BS.pack (replicate 28 0)

        certs' = flip map certs $ \case
            PoolDelegationCertificate ->
                Cardano.shelleyDelegateStake dummyKeyHash dummyKeyHash
            KeyRegistrationCertificate ->
                Cardano.shelleyRegisterStakingAddress dummyKeyHash
            KeyDeRegistrationCertificate ->
                Cardano.shelleyDeregisterStakingAddress dummyKeyHash

    (addrWits, certWits) =
        ( Set.map dummyWitnessUniq $ Set.fromList (fst <$> inps)
        , if null certs
            then Set.empty
            else Set.singleton (dummyWitness "a")
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
