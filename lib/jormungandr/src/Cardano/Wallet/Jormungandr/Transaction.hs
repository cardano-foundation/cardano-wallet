{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Wallet.Jormungandr.Transaction
    ( newTransactionLayer
    , sign
    , mkTxWitness
    , ErrExceededInpsOrOuts (..)
    ) where

import Prelude

import Cardano.Wallet.Jormungandr.Binary
    ( Message (..)
    , fragmentId
    , getMessage
    , legacyUtxoWitness
    , maxNumberOfInputs
    , maxNumberOfOutputs
    , runGetOrFail
    , signData
    , utxoWitness
    )
import Cardano.Wallet.Jormungandr.Compatibility
    ( Jormungandr )
import Cardano.Wallet.Jormungandr.Primitive.Types
    ( Tx (..) )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (AddressK), KeyToAddress, Passphrase (..), WalletKey (..), XPrv )
import Cardano.Wallet.Primitive.AddressDerivation.Random
    ( RndKey )
import Cardano.Wallet.Primitive.AddressDerivation.Sequential
    ( SeqKey )
import Cardano.Wallet.Primitive.CoinSelection
    ( CoinSelection (..) )
import Cardano.Wallet.Primitive.Types
    ( Coin, Hash (..), TxIn, TxOut (..), TxWitness (..) )
import Cardano.Wallet.Transaction
    ( ErrDecodeSignedTx (..)
    , ErrMkStdTx (..)
    , ErrValidateSelection
    , TransactionLayer (..)
    , estimateMaxNumberOfInputsBase
    )
import Control.Arrow
    ( second )
import Control.Monad
    ( forM, when )
import Data.ByteString
    ( ByteString )
import Data.Either.Combinators
    ( maybeToRight )
import Data.Quantity
    ( Quantity (..) )
import Data.Text.Class
    ( toText )
import Fmt
    ( Buildable (..) )

import qualified Cardano.Crypto.Wallet as CC
import qualified Cardano.Wallet.Jormungandr.Binary as Binary
import qualified Data.ByteString.Lazy as BL

-- | Construct a 'TransactionLayer' compatible with Shelley and 'Jörmungandr'
newTransactionLayer
    :: forall n k t.
        ( t ~ Jormungandr n
        , KeyToAddress (Jormungandr n) k
        , MkTxWitness k
        )
    => Hash "Genesis"
    -> TransactionLayer t k
newTransactionLayer block0H = TransactionLayer
    { mkStdTx = \keyFrom rnps outs -> do
        -- NOTE
        -- For signing, we need to embed a hash of the transaction data
        -- without the witnesses (since we don't yet have them!). In this sense,
        -- this is a transaction id as Byron nodes or the http-bridge
        -- defines them.
        let inps = fmap (second coin) rnps
        wits <- forM rnps $ \(_, TxOut addr _) -> do
            xprv <- maybeToRight (ErrKeyNotFoundForAddress addr) (keyFrom addr)
            pure (mkTxWitness block0H inps outs xprv)
        let tx = Tx
                { txid = fragmentId inps outs wits
                , inputs = inps
                , outputs = outs
                }
        return (tx, wits)

    , decodeSignedTx = \payload -> do
        let errInvalidPayload =
                ErrDecodeSignedTxWrongPayload "wrongly constructed binary blob"
        case runGetOrFail getMessage (BL.fromStrict payload) of
            Left _ -> Left errInvalidPayload
            Right (_,_,msg) -> case msg of
                Transaction stx -> pure stx
                _ -> Left errInvalidPayload

    -- NOTE
    -- Jörmungandr fee calculation is a linear function where the coefficient
    -- is multiplied by the total number of inputs and outputs.
    , estimateSize = \(CoinSelection inps outs chgs) ->
        Quantity $ length inps + length outs + length chgs

    , estimateMaxNumberOfInputs =
        estimateMaxNumberOfInputsBase @t @k Binary.estimateMaxNumberOfInputsParams

    , validateSelection = \(CoinSelection inps outs _) -> do
        when (length inps > maxNumberOfInputs || length outs > maxNumberOfOutputs)
            $ Left ErrExceededInpsOrOuts
    }

-- | Provide a transaction witness for a given private key. The type of witness
-- if different between types of keys and, with backward-compatible support, we
-- may have to support many types for one backend target.
class MkTxWitness (k :: Depth -> * -> *) where
    mkTxWitness
        :: Hash "Genesis"
        -> [(TxIn, Coin)]
        -> [TxOut]
        -> (k 'AddressK XPrv , Passphrase "encryption")
        -> TxWitness

instance MkTxWitness SeqKey where
    mkTxWitness (Hash block0) inps outs xprv =
        let
            payload = block0 <> getHash (signData inps outs)
        in
            utxoWitness (sign payload xprv)

instance MkTxWitness RndKey where
    mkTxWitness (Hash block0) inps outs xprv =
        let
            xpub = getRawKey $ publicKey $ fst xprv
            payload = block0 <> getHash (signData inps outs)
        in
            legacyUtxoWitness xpub (sign payload xprv)

-- | Sign some arbitrary binary data using a private key.
sign
    :: WalletKey k
    => ByteString
    -> (k 'AddressK XPrv, Passphrase "encryption")
    -> ByteString
sign bytes (key, (Passphrase pwd)) =
    CC.unXSignature $ CC.sign pwd (getRawKey key) bytes

-- | Transaction with improper number of inputs and outputs is tried
data ErrExceededInpsOrOuts = ErrExceededInpsOrOuts
    deriving (Eq, Show)

instance Buildable ErrExceededInpsOrOuts where
    build _ = build $ mconcat
        [ "I can't validate coin selection because either the number of inputs "
        , "is more than ", maxI," or the number of outputs exceeds ", maxO, "."
        ]
      where
        maxI = toText maxNumberOfInputs
        maxO = toText maxNumberOfOutputs

type instance ErrValidateSelection (Jormungandr n) = ErrExceededInpsOrOuts
