{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
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
    , genesisBlockFromTxOuts
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPrv )
import Cardano.Wallet.Byron.Compatibility
    ( Byron )
import Cardano.Wallet.Byron.Transaction.Size
    ( MaxSizeOf, maxSizeOf, sizeOfSignedTx )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..), NetworkDiscriminant (..), Passphrase (..), WalletKey (..) )
import Cardano.Wallet.Primitive.AddressDerivation.Byron
    ( ByronKey )
import Cardano.Wallet.Primitive.CoinSelection
    ( CoinSelection (..) )
import Cardano.Wallet.Primitive.Fee
    ( Fee (..), FeePolicy (..) )
import Cardano.Wallet.Primitive.Types
    ( Address (..)
    , Block (..)
    , BlockHeader (..)
    , Coin (..)
    , GenesisParameters (..)
    , Hash (..)
    , ProtocolMagic (..)
    , SealedTx (..)
    , SlotId (..)
    , Tx (..)
    , TxIn (..)
    , TxOut (..)
    )
import Cardano.Wallet.Transaction
    ( Certificate (..)
    , ErrDecodeSignedTx (..)
    , ErrMkTx (..)
    , TransactionLayer (..)
    )
import Control.Arrow
    ( second )
import Control.Monad
    ( forM, when )
import Crypto.Hash.Utils
    ( blake2b256 )
import Data.ByteString
    ( ByteString )
import Data.Coerce
    ( coerce )
import Data.Either.Combinators
    ( maybeToRight )
import Data.Maybe
    ( isJust )
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
import qualified Cardano.Wallet.Transaction as W
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Write as CBOR
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T


newTransactionLayer
    :: forall (n :: NetworkDiscriminant) k t.
        ( t ~ IO Byron
        , WalletKey k
        , MaxSizeOf Address n ByronKey
        )
    => Proxy n
    -> ProtocolMagic
    -> TransactionLayer t k
newTransactionLayer _proxy protocolMagic = TransactionLayer
    { mkStdTx = _mkStdTx
    , mkDelegationJoinTx = _mkDelegationJoinTx
    , mkDelegationQuitTx = _mkDelegationQuitTx
    , decodeSignedTx = _decodeSignedTx
    , minimumFee = _minimumFee
    , estimateMaxNumberOfInputs = _estimateMaxNumberOfInputs
    , validateSelection = _validateSelection
    , allowUnbalancedTx = True
    }
  where
    _mkStdTx
        :: (Address -> Maybe (k 'AddressK XPrv, Passphrase "encryption"))
        -> SlotId
        -> [(TxIn, TxOut)]
        -> [TxOut]
        -> Either ErrMkTx (Tx, SealedTx)
    _mkStdTx keyFrom _slotId inps outs = do
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

    _minimumFee
        :: FeePolicy
        -> [Certificate]
        -> CoinSelection
        -> Fee
    _minimumFee policy _ (CoinSelection inps outs chngs _) =
        computeFee $ sizeOfSignedTx (fst <$> inps) (outs <> map dummyOutput chngs)
      where
        dummyOutput :: Coin -> TxOut
        dummyOutput = TxOut (dummyAddress @n)

        LinearFee (Quantity a) (Quantity b) (Quantity _unused) = policy
        computeFee size = Fee $ ceiling (a + b*fromIntegral size)

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
        -> Either ErrValidateSelection ()
    _validateSelection (CoinSelection _ outs _ rsv) = do
        when (any (\ (TxOut _ c) -> c == Coin 0) outs) $
            Left ErrInvalidTxOutAmount
        when (isJust rsv) $
            Left ErrReserveNotAllowed

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

    _mkDelegationJoinTx =
        notImplemented "mkDelegationJoinTx"

    _mkDelegationQuitTx =
        notImplemented "mkDelegationQuitTx"

--------------------------------------------------------------------------------
-- Extra validations on coin selection
--

data ErrValidateSelection
    = ErrInvalidTxOutAmount
    -- ^ Transaction with 0 output amount is tried
    | ErrReserveNotAllowed
    -- ^ Transaction has a reserve input amount, not allowed for this backend

instance Buildable ErrValidateSelection where
    build = \case
        ErrInvalidTxOutAmount ->
            "Invalid coin selection: at least one output is null."
        ErrReserveNotAllowed -> build $ T.unwords
            [ "The given coin selection was given a reserve amount and this is"
            , "not allowed for this backend / era."
            ]

type instance W.ErrValidateSelection (IO Byron) = ErrValidateSelection

--------------------------------------------------------------------------------
-- Internal
--

-- | Construct a ("fake") genesis block from genesis transaction outputs.
--
-- The genesis data on haskell nodes is not a block at all, unlike the block0 on
-- jormungandr. This function is a method to deal with the discrepancy.
genesisBlockFromTxOuts :: GenesisParameters -> [TxOut] -> Block
genesisBlockFromTxOuts gp outs = Block
    { delegations  = []
    , header = BlockHeader
        { slotId =
            SlotId 0 0
        , blockHeight =
            Quantity 0
        , headerHash =
            coerce $ getGenesisBlockHash gp
        , parentHeaderHash =
            Hash (BS.replicate 32 0)
        }
    , transactions = mkTx <$> outs
    }
  where
    mkTx out@(TxOut (Address bytes) _) =
        Tx (Hash $ blake2b256 bytes) [] [out]

dummyAddress
    :: forall (n :: NetworkDiscriminant). (MaxSizeOf Address n ByronKey)
    => Address
dummyAddress =
    Address $ BS.replicate (maxSizeOf @Address @n @ByronKey) 0

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

notImplemented :: HasCallStack => String -> a
notImplemented what = error ("Not implemented: " <> what)
