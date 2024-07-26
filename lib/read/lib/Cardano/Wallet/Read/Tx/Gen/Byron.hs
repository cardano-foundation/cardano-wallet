{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Wallet.Read.Tx.Gen.Byron
    ( mkByronTx
    , exampleByronTx
    , mkByronAddrFromXPub
    )
where

import Prelude

import Cardano.Chain.Common
    ( mkAttributes
    )
import Cardano.Chain.UTxO
    ( ATxAux
    , Tx (UnsafeTx)
    , TxIn (..)
    , mkTxAux
    )
import Cardano.Crypto
    ( Hash
    , hashFromBytes
    )
import Cardano.Crypto.Wallet
    ( ChainCode (..)
    , XPub (..)
    )
import Cardano.Wallet.Read.Tx.Gen.Address
    ( decodeByronAddress
    )
import Cardano.Wallet.Read.Tx.Gen.TxParameters
    ( Address (..)
    , Index (..)
    , Lovelace (..)
    , TxParameters (..)
    , exampleTxParameters
    )
import Cardano.Wallet.Read.Tx.TxId
    ( TxId
    , hashFromTxId
    )
import Data.ByteString
    ( ByteString
    )
import Data.Functor
    ( (<&>)
    )
import GHC.Stack
    ( HasCallStack
    )

import qualified Cardano.Chain.Common as Byron
import qualified Cardano.Chain.UTxO as Byron
import qualified Cardano.Crypto.Signing as Byron
import qualified Cardano.Wallet.Read.Hash as Hash

mkByronTx
    :: HasCallStack
    => TxParameters
    -> ATxAux ()
mkByronTx TxParameters{txInputs, txOutputs} =
    mkTxAux tx mempty
  where
    tx :: Tx
    tx = UnsafeTx inputs outputs $ mkAttributes ()
    inputs = txInputs <&> mkByronInput
    outputs = txOutputs <&> mkByronTxOut

mkByronInput :: (Index, TxId) -> TxIn
mkByronInput (Index idx, txid) =
    TxInUtxo (unsafeHashFromTxId txid)
        $ fromIntegral idx

mkByronTxOut :: HasCallStack => (Address, Lovelace) -> Byron.TxOut
mkByronTxOut (addr@ByronAddress{}, Lovelace val) =
    Byron.TxOut (decodeByronAddress addr) (mkByronValue val)
mkByronTxOut (ShelleyAddress{}, _val) =
    error "mkByronTxOut: Shelley addresses are not supported in byron era"

mkByronValue
    :: HasCallStack
    => Integer
    -> Byron.Lovelace
mkByronValue x = case Byron.mkLovelace $ fromIntegral x of
    Left le -> error $ show le
    Right lo -> lo

mkByronAddrFromXPub :: ByteString -> Byron.Address
mkByronAddrFromXPub addr =
    Byron.makeAddress
        (Byron.VerKeyASD $ Byron.VerificationKey $ XPub addr $ ChainCode mempty)
        $ Byron.AddrAttributes Nothing Byron.NetworkMainOrStage

unsafeHashFromTxId :: TxId -> Hash a
unsafeHashFromTxId txid =
    case hashFromBytes (Hash.hashToBytes $ hashFromTxId txid) of
        Nothing -> error "hashUnsafe: failed to hash"
        Just h -> h

exampleByronTx :: ATxAux ()
exampleByronTx = mkByronTx exampleTxParameters
