{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Jormungandr.Transaction
    ( newTransactionLayer
    ) where

import Prelude

import Cardano.Wallet
    ( MaxInpsOrOuts (..) )
import Cardano.Wallet.Jormungandr.Binary
    ( maxNumberOfInputs, maxNumberOfOutputs )
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
    ( Hash (..), TxOut (..), TxWitness (..), txId )
import Cardano.Wallet.Transaction
    ( ErrMkStdTx (..)
    , ErrValidateSelection (..)
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

import qualified Cardano.Crypto.Wallet as CC
import qualified Cardano.Wallet.Jormungandr.Binary as Binary

-- | Construct a 'TransactionLayer' compatible with Shelley and 'Jörmungandr'
newTransactionLayer
    :: forall n t. (KnownNetwork n, t ~ Jormungandr n)
    => Hash "Genesis"
    -> TransactionLayer t
newTransactionLayer (Hash block0) = TransactionLayer
    { mkStdTx = \keyFrom sel@(CoinSelection inps _ _) outs -> do
        let tx = Tx (fmap (second coin) inps) outs
        let bs = block0 <> getHash (txId @(Jormungandr n) tx)
        case checkSelection sel of
            Left err -> Left $ ErrInvalidTx err
            Right _ -> do
                txWitnesses <- forM inps $ \(_, TxOut addr _) -> sign bs
                    <$> maybeToRight (ErrKeyNotFoundForAddress addr) (keyFrom addr)
                return (tx, txWitnesses)

    -- NOTE: at this point 'Jörmungandr' node does not support fee calculation
    , estimateSize = \_ -> Quantity 0

    , estimateMaxNumberOfInputs =
        estimateMaxNumberOfInputsBase @t Binary.estimateMaxNumberOfInputsParams

    , validateSelection = checkSelection
    }
  where
    sign
        :: ByteString
        -> (Key 'AddressK XPrv, Passphrase "encryption")
        -> TxWitness
    sign bytes (key, (Passphrase pwd)) =
        TxWitness . CC.unXSignature $ CC.sign pwd (getKey key) bytes

    checkSelection :: CoinSelection -> Either ErrValidateSelection ()
    checkSelection (CoinSelection inps outs _) = do
        when (length inps > maxNumberOfInputs || length outs > maxNumberOfOutputs)
            $ Left $ ErrExceededInpsOrOuts
            (MaxInpsOrOuts maxNumberOfInputs maxNumberOfOutputs)
