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
    , ErrExceededInpsOrOuts (..)
    ) where

import Prelude

import Cardano.Wallet.Jormungandr.Binary
    ( fragmentId, maxNumberOfInputs, maxNumberOfOutputs, signData )
import Cardano.Wallet.Jormungandr.Compatibility
    ( Jormungandr )
import Cardano.Wallet.Jormungandr.Environment
    ( KnownNetwork )
import Cardano.Wallet.Jormungandr.Primitive.Types
    ( Tx (..) )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (AddressK), Key, Passphrase (..), XPrv, getKey )
import Cardano.Wallet.Primitive.CoinSelection
    ( CoinSelection (..) )
import Cardano.Wallet.Primitive.Types
    ( Hash (..), TxOut (..), TxWitness (..) )
import Cardano.Wallet.Transaction
    ( ErrMkStdTx (..)
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

-- | Construct a 'TransactionLayer' compatible with Shelley and 'Jörmungandr'
newTransactionLayer
    :: forall n t. (KnownNetwork n, t ~ Jormungandr n)
    => Hash "Genesis"
    -> TransactionLayer t
newTransactionLayer (Hash block0) = TransactionLayer
    { mkStdTx = \keyFrom rnps outs -> do
        -- NOTE
        --
        -- Yet, for signing, we need to embed a hash of the transaction data
        -- without the witnesses (since we don't yet have them!). In this sense,
        -- this is a transaction id as Byron nodes or the http-bridge
        -- defines them.
        let inps = fmap (second coin) rnps
        let bs = block0 <> getHash (signData inps outs)
        wits <- forM rnps $ \(_, TxOut addr _) -> sign bs
            <$> maybeToRight (ErrKeyNotFoundForAddress addr) (keyFrom addr)
        let tx = Tx
                { txid = fragmentId inps outs wits
                , inputs = inps
                , outputs = outs
                }
        return (tx, wits)

    -- NOTE
    -- Jörmungandr fee calculation is a linear function where the coefficient
    -- is multiplied by the total number of inputs and outputs.
    , estimateSize = \(CoinSelection inps outs chgs) ->
        Quantity $ length inps + length outs + length chgs

    , estimateMaxNumberOfInputs =
        estimateMaxNumberOfInputsBase @t Binary.estimateMaxNumberOfInputsParams

    , validateSelection = \(CoinSelection inps outs _) -> do
        when (length inps > maxNumberOfInputs || length outs > maxNumberOfOutputs)
            $ Left ErrExceededInpsOrOuts
    }
  where
    sign
        :: ByteString
        -> (Key 'AddressK XPrv, Passphrase "encryption")
        -> TxWitness
    sign bytes (key, (Passphrase pwd)) =
        TxWitness . CC.unXSignature $ CC.sign pwd (getKey key) bytes

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
