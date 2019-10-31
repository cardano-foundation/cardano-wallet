{-# LANGUAGE AllowAmbiguousTypes #-}
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
    ( Depth (AddressK), Passphrase (..), PaymentAddress, WalletKey (..), XPrv )
import Cardano.Wallet.Primitive.AddressDerivation.Byron
    ( ByronKey )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( ShelleyKey )
import Cardano.Wallet.Primitive.CoinSelection
    ( CoinSelection (..) )
import Cardano.Wallet.Primitive.Types
    ( Hash (..), TxOut (..), TxWitness (..) )
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
        ( t ~ Jormungandr
        , PaymentAddress n k
        , MkTxWitness k
        , WalletKey k
        )
    => Hash "Genesis"
    -> TransactionLayer t k
newTransactionLayer (Hash block0H) = TransactionLayer
    { mkStdTx = \keyFrom rnps outs -> do
        -- NOTE
        -- For signing, we need to embed a hash of the transaction data
        -- without the witnesses (since we don't yet have them!). In this sense,
        -- this is a transaction id as Byron nodes or the http-bridge
        -- defines them.
        let inps = fmap (second coin) rnps
        wits <- forM rnps $ \(_, TxOut addr _) -> do
            xprv <- maybeToRight (ErrKeyNotFoundForAddress addr) (keyFrom addr)
            let payload = block0H <> getHash (signData inps outs)
            pure $ mkTxWitness (fst xprv) (sign payload xprv)
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
        estimateMaxNumberOfInputsBase @t @n @k Binary.estimateMaxNumberOfInputsParams

    , validateSelection = \(CoinSelection inps outs _) -> do
        when (length inps > maxNumberOfInputs || length outs > maxNumberOfOutputs)
            $ Left ErrExceededInpsOrOuts
    }

-- | Provide a transaction witness for a given private key. The type of witness
-- is different between types of keys and, with backward-compatible support, we
-- need to support many types for one backend target.
--
-- We have tightly coupled the type of witness to the type of key for because:
--
-- - ByronKey can only be used with legacy / Byron wallets as they require a
--   special address structure (to embed the derivation path).
--
-- - ShelleyKey could theorically be used with the legacy address structure (as
-- Yoroi does) however, our implementation only associate ShelleyKey to new
-- addresses.
class MkTxWitness (k :: Depth -> * -> *) where
    mkTxWitness
        :: k 'AddressK XPrv
        -> ByteString
        -> TxWitness

instance MkTxWitness ShelleyKey where
    mkTxWitness _ = utxoWitness

instance MkTxWitness ByronKey where
    mkTxWitness xprv = legacyUtxoWitness xpub
      where
        xpub = getRawKey $ publicKey xprv

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

type instance ErrValidateSelection Jormungandr = ErrExceededInpsOrOuts
