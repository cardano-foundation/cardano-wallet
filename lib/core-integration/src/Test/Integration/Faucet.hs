{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Integration.Faucet
    ( Faucet (..)
    , nextWallet
    ) where

import Prelude

import Cardano.Wallet.Primitive.Mnemonic
    ( Mnemonic )
import Control.Concurrent.MVar
    ( MVar, putMVar, takeMVar )
import Data.Functor
    ( (<$) )
import GHC.TypeLits
    ( Nat, Symbol )

-- | An opaque 'Faucet' type from which one can get a wallet with funds
data Faucet = Faucet
    { sequential :: MVar [Mnemonic 15]
    , random :: MVar [Mnemonic 12]
    }

-- -- | Get the next faucet wallet. Requires the 'initFaucet' to be called in order
-- -- to get a hand on a 'Faucet'.
class NextWallet (scheme :: Symbol) where
    type MnemonicSize scheme :: Nat
    nextWallet :: Faucet -> IO (Mnemonic (MnemonicSize scheme))

instance NextWallet "seq" where
    type MnemonicSize "seq" = 15
    nextWallet (Faucet mvar _) = do
        takeMVar mvar >>= \case
            [] -> fail "nextWallet: Awe crap! No more faucet seq wallet available!"
            (h:q) -> h <$ putMVar mvar q

instance NextWallet "rnd" where
    type MnemonicSize "rnd" = 12
    nextWallet (Faucet _ mvar) = do
        takeMVar mvar >>= \case
            [] -> fail "nextWallet: Awe crap! No more faucet rnd wallet available!"
            (h:q) -> h <$ putMVar mvar q
