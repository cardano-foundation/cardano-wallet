{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

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

-- | An opaque 'Faucet' type from which one can get a wallet with funds
newtype Faucet = Faucet (MVar [Mnemonic 15])

-- | Get the next faucet wallet. Requires the 'initFaucet' to be called in order
-- to get a hand on a 'Faucet'.
nextWallet :: Faucet -> IO (Mnemonic 15)
nextWallet (Faucet mvar) = do
    takeMVar mvar >>= \case
        [] -> fail "nextWallet: Awe crap! No more faucet wallet available!"
        (h:q) -> h <$ putMVar mvar q
