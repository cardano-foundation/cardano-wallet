{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2020 IOHK
-- License: Apache-2.0
--
-- Working with Byron transactions.

module Cardano.Wallet.Byron.Transaction
    ( newTransactionLayer
    , fromGenesisTxOut
    ) where

import Prelude

import Cardano.Wallet.Byron.Compatibility
    ( Byron )
import Cardano.Wallet.Byron.Transaction.Size
    ( MaxSizeOf, maxSizeOf, sizeOfSignedTx )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..)
    , NetworkDiscriminant (..)
    , Passphrase (..)
    , WalletKey (..)
    , XPrv
    )
import Cardano.Wallet.Primitive.CoinSelection
    ( CoinSelection (..) )
import Cardano.Wallet.Primitive.Types
    ( Address (..)
    , Coin (..)
    , Hash (..)
    , PoolId
    , ProtocolMagic (..)
    , SealedTx (..)
    , Tx (..)
    , TxIn (..)
    , TxOut (..)
    )
import Cardano.Wallet.Transaction
    ( ErrDecodeSignedTx (..)
    , ErrMkTx (..)
    , ErrValidateSelection
    , TransactionLayer (..)
    )
import Control.Arrow
    ( second )
import Control.Monad
    ( forM, when )
import Crypto.Hash
    ( hash )
import Crypto.Hash.Algorithms
    ( Blake2b_256 )
import Data.ByteString
    ( ByteString )
import Data.Either.Combinators
    ( maybeToRight )
import Data.Proxy
    ( Proxy )
import Data.Quantity
    ( Quantity (..) )
import Data.Word
    ( Word16, Word8 )
import Fmt
    ( Buildable (..) )
import GHC.Stack
    ( HasCallStack )

import qualified Cardano.Byron.Codec.Cbor as CBOR
import qualified Cardano.Crypto.Wallet as CC
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Write as CBOR
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T


newTransactionLayer
    :: forall (n :: NetworkDiscriminant) k t.
        ( t ~ IO Byron
        , WalletKey k
        , MaxSizeOf Address n k
        )
    => Proxy n
    -> ProtocolMagic
    -> TransactionLayer t k
newTransactionLayer _proxy protocolMagic = TransactionLayer
    { mkStdTx = _mkStdTx
    , mkDelegationJoinTx = _mkDelegationJoinTx
    , mkDelegationQuitTx = _mkDelegationQuitTx
    , decodeSignedTx = _decodeSignedTx
    , estimateSize = _estimateSize
    , estimateMaxNumberOfInputs = _estimateMaxNumberOfInputs
    , validateSelection = _validateSelection
    }
  where
    _mkStdTx
        :: (Address -> Maybe (k 'AddressK XPrv, Passphrase "encryption"))
        -> [(TxIn, TxOut)]
        -> [TxOut]
        -> Either ErrMkTx (Tx, SealedTx)
    _mkStdTx keyFrom inps outs = do
        let tx = (fst <$> inps, outs)
        let sigData = blake2b256 $ CBOR.toStrictByteString $ CBOR.encodeTx tx
        witnesses <- forM inps $ \(_, TxOut addr _) ->
            mkWitness protocolMagic sigData <$> lookupPrivateKey addr
        pure
            ( Tx (Hash sigData) (second coin <$> inps) outs
            , SealedTx $ CBOR.toStrictByteString $ CBOR.encodeSignedTx tx witnesses
            )
      where
        lookupPrivateKey
            :: Address
            -> Either ErrMkTx (k 'AddressK XPrv, Passphrase "encryption")
        lookupPrivateKey addr =
            maybeToRight (ErrKeyNotFoundForAddress addr) (keyFrom addr)

    _estimateSize
        :: CoinSelection
        -> Quantity "byte" Int
    _estimateSize (CoinSelection inps outs chngs) =
        Quantity $ sizeOfSignedTx (fst <$> inps) (outs <> map dummyOutput chngs)
      where
        dummyOutput :: Coin -> TxOut
        dummyOutput = TxOut (dummyAddress @n @k)

    _estimateMaxNumberOfInputs
        :: Quantity "byte" Word16
        -- ^ Transaction max size in bytes
        -> Word8
        -- ^ Number of outputs in transaction
        -> Word8
    _estimateMaxNumberOfInputs _ _ =
        -- TODO: Compute the actual size or, revise coin selection to not
        -- need that. Instead, we could simply compute the size of the
        -- transaction at each step and check that it remains under the
        -- allowed max size.
        maxBound

    _validateSelection
        :: CoinSelection
        -> Either (ErrValidateSelection t) ()
    _validateSelection (CoinSelection _ outs _) =
        when (any (\ (TxOut _ c) -> c == Coin 0) outs) $
            Left ErrInvalidTxOutAmount

    _decodeSignedTx
        :: ByteString
        -> Either ErrDecodeSignedTx (Tx, SealedTx)
    _decodeSignedTx bytes =
        case CBOR.deserialiseFromBytes CBOR.decodeSignedTx (BL.fromStrict bytes) of
            Left e ->
                Left $ ErrDecodeSignedTxWrongPayload $ T.pack $ show e
            Right (_, ((inps, outs), _)) -> Right
                ( Tx
                    { txId = Hash
                        $ blake2b256
                        $ CBOR.toStrictByteString
                        $ CBOR.encodeTx (inps, outs)
                    -- FIXME Do not require Tx to have resolvedInputs
                    , resolvedInputs = (,Coin 0) <$> inps
                    , outputs = outs
                    }
                , SealedTx bytes
                )

    _mkDelegationJoinTx
        :: PoolId
        -> (k 'AddressK XPrv, Passphrase "encryption") -- reward account
        -> (Address -> Maybe (k 'AddressK XPrv, Passphrase "encryption"))
        -> [(TxIn, TxOut)]
        -> [TxOut]
        -> Either ErrMkTx (Tx, SealedTx)
    _mkDelegationJoinTx =
        notImplemented "mkDelegationJoinTx"

    _mkDelegationQuitTx
        :: (k 'AddressK XPrv, Passphrase "encryption") -- reward account
        -> (Address -> Maybe (k 'AddressK XPrv, Passphrase "encryption"))
        -> [(TxIn, TxOut)]
        -> [TxOut]
        -> Either ErrMkTx (Tx, SealedTx)
    _mkDelegationQuitTx =
        notImplemented "mkDelegationQuitTx"

--------------------------------------------------------------------------------
-- Extra validations on coin selection
--

-- | Transaction with 0 output amount is tried
data ErrInvalidTxOutAmount = ErrInvalidTxOutAmount

instance Buildable ErrInvalidTxOutAmount where
    build _ = "Invalid coin selection: at least one output is null."

type instance ErrValidateSelection (IO Byron) = ErrInvalidTxOutAmount

--------------------------------------------------------------------------------
-- Internal
--

fromGenesisTxOut :: TxOut -> Tx
fromGenesisTxOut out@(TxOut (Address bytes) _) =
    Tx (Hash $ blake2b256 bytes) [] [out]

dummyAddress
    :: forall (n :: NetworkDiscriminant) k. (MaxSizeOf Address n k) => Address
dummyAddress =
    Address $ BS.replicate (maxSizeOf @Address @n @k) 0

mkWitness
    :: WalletKey k
    => ProtocolMagic
    -> ByteString
    -> (k 'AddressK XPrv, Passphrase "encryption")
    -> ByteString
mkWitness (ProtocolMagic pm) sigData (xPrv, Passphrase pwd) =
    CBOR.toStrictByteString
    $ CBOR.encodePublicKeyWitness (getRawKey $ publicKey xPrv)
    $ CC.unXSignature (CC.sign pwd (getRawKey xPrv) message)
  where
    message = mconcat
        [ "\x01"
        , CBOR.toStrictByteString (CBOR.encodeInt32 pm)
        , CBOR.toStrictByteString (CBOR.encodeBytes sigData)
        ]

blake2b256 :: ByteString -> ByteString
blake2b256 =
    BA.convert . hash @_ @Blake2b_256

notImplemented :: HasCallStack => String -> a
notImplemented what = error ("Not implemented: " <> what)
