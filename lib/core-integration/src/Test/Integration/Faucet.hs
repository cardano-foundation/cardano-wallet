{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Integration.Faucet
    ( Faucet (..)
    , nextWallet
    , nextTxBuilder
    ) where

import Prelude

import Cardano.Wallet.Primitive.Mnemonic
    ( Mnemonic )
import Cardano.Wallet.Primitive.Types
    ( Address, Coin )
import Control.Concurrent.MVar
    ( MVar, putMVar, takeMVar )
import Data.ByteString
    ( ByteString )
import Data.Functor
    ( (<$) )
import GHC.TypeLits
    ( Nat, Symbol )

-- | An opaque 'Faucet' type from which one can get a wallet with funds
data Faucet = Faucet
    { sequential :: MVar [Mnemonic 15]
    , random :: MVar [Mnemonic 12]
    , txBuilder :: MVar [(Address, Coin) -> IO ByteString]
    }

-- | Get a raw transaction builder. It constructs and sign a transaction via an
-- private key that is owned "externally". Returns a bytes string ready to be
-- sent to a node.
nextTxBuilder :: Faucet -> IO ((Address, Coin) -> IO ByteString)
nextTxBuilder (Faucet _ _ mvar) =
    takeMVar mvar >>= \case
        [] -> fail "nextTxBuilder: Awe crap! No more faucet tx builder available!"
        (h:q) -> h <$ putMVar mvar q

-- | Get the next faucet wallet. Requires the 'initFaucet' to be called in order
-- to get a hand on a 'Faucet'.
class NextWallet (scheme :: Symbol) where
    type MnemonicSize scheme :: Nat
    nextWallet :: Faucet -> IO (Mnemonic (MnemonicSize scheme))

instance NextWallet "seq" where
    type MnemonicSize "seq" = 15
    nextWallet (Faucet mvar _ _) = do
        takeMVar mvar >>= \case
            [] -> fail "nextWallet: Awe crap! No more faucet seq wallet available!"
            (h:q) -> h <$ putMVar mvar q

instance NextWallet "rnd" where
    type MnemonicSize "rnd" = 12
    nextWallet (Faucet _ mvar _) = do
        takeMVar mvar >>= \case
            [] -> fail "nextWallet: Awe crap! No more faucet rnd wallet available!"
            (h:q) -> h <$ putMVar mvar q
