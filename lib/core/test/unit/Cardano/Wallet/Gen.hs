{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Shared QuickCheck generators for wallet types.
--
-- Our convention is to let each test module define its own @Arbitrary@ orphans.
-- This module allows for code-reuse where desired, by providing generators.
module Cardano.Wallet.Gen
    ( genMnemonic
    ) where

import Prelude

import Cardano.Wallet.Primitive.Mnemonic
    ( ConsistentEntropy, EntropySize, Mnemonic, entropyToMnemonic )
import Cardano.Wallet.Unsafe
    ( unsafeMkEntropy )
import Data.Proxy
    ( Proxy (..) )
import GHC.TypeLits
    ( natVal )
import Test.QuickCheck
    ( Arbitrary (..), Gen, vectorOf )

import qualified Data.ByteString as BS

-- | Generates an arbitrary mnemonic of a size according to the type parameter.
--
-- E.g:
-- >>> arbitrary = SomeMnemonic <$> genMnemonic @12
genMnemonic
    :: forall mw ent csz.
     ( ConsistentEntropy ent mw csz
     , EntropySize mw ~ ent
     )
    => Gen (Mnemonic mw)
genMnemonic = do
        let n = fromIntegral (natVal $ Proxy @(EntropySize mw)) `div` 8
        bytes <- BS.pack <$> vectorOf n arbitrary
        let ent = unsafeMkEntropy @(EntropySize mw) bytes
        return $ entropyToMnemonic ent
