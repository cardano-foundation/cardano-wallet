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
    , SlotNo (..)
    , Tx (..)
    , TxOut (..)
    )
import Cardano.Wallet.Transaction
    ( DelegationAction
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
import qualified Cardano.Wallet.Primitive.CoinSelection as CS
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
    , initDelegationSelection = _initDelegationSelection
    , decodeSignedTx = _decodeSignedTx
    , minimumFee = _minimumFee
    , estimateMaxNumberOfInputs = _estimateMaxNumberOfInputs
    , validateSelection = _validateSelection
    , allowUnbalancedTx = True
    }
  where
    _mkStdTx
        :: (k 'AddressK XPrv, Passphrase "encryption")
            -- Reward account
        -> (Address -> Maybe (k 'AddressK XPrv, Passphrase "encryption"))
            -- Key store
        -> SlotNo
            -- Tip of the chain, for TTL
        -> CoinSelection
            -- A balanced coin selection where all change addresses have been
            -- assigned.
        -> Either ErrMkTx (Tx, SealedTx)
    _mkStdTx _rewardAcnt keyFrom _slotId cs = do
        let tx = (fst <$> CS.inputs cs, CS.outputs cs)
        let sigData = blake2b256 $ CBOR.toStrictByteString $ CBOR.encodeTx tx
        witnesses <- forM (CS.inputs cs) $ \(_, TxOut addr _) ->
            mkWitness protocolMagic sigData <$> lookupPrivateKey addr
        pure
            ( Tx (Hash sigData) (second coin <$> CS.inputs cs) (CS.outputs cs) mempty
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
        -> Maybe DelegationAction
        -> CoinSelection
        -> Fee
    _minimumFee policy _ cs =
        computeFee $ sizeOfSignedTx
            (fst <$> CS.inputs cs)
            (CS.outputs cs <> map dummyOutput (CS.change cs))
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
    _validateSelection cs = do
        when (any (\ (TxOut _ c) -> c == Coin 0) (CS.outputs cs)) $
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
                    , withdrawals = mempty
                    }
                , SealedTx bytes
                )

    _mkDelegationJoinTx =
        notImplemented "mkDelegationJoinTx"

    _mkDelegationQuitTx =
        notImplemented "mkDelegationQuitTx"

    _initDelegationSelection =
        notImplemented "initDelegationSelection"

--------------------------------------------------------------------------------
-- Extra validations on coin selection
--

data ErrValidateSelection
    = ErrInvalidTxOutAmount
    -- ^ Transaction with 0 output amount is tried

instance Buildable ErrValidateSelection where
    build = \case
        ErrInvalidTxOutAmount ->
            "Invalid coin selection: at least one output is null."

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
        { slotNo =
            SlotNo 0
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
        Tx (Hash $ blake2b256 bytes) [] [out] mempty

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
