{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Primitive.Types.Gen () where

import Cardano.Wallet.Prelude

import Cardano.Wallet.Primitive.Types
    ( TokenBundleMaxSize (..) )
import Cardano.Wallet.Primitive.Types.Tx
    ( TxSize (..) )
import Test.QuickCheck
    ( Arbitrary (..), oneof )

instance Arbitrary TokenBundleMaxSize where
    arbitrary = TokenBundleMaxSize . TxSize <$>
        oneof
          -- Generate values close to the mainnet value of 4000 (and guard
          -- against underflow)
          [ fromIntegral . max 0 . (4000 +) <$> arbitrary @Int

          -- Generate more extreme values (both small and large)
          , fromIntegral <$> arbitrary @Word64
          ]
    shrink (TokenBundleMaxSize (TxSize s)) =
        map (TokenBundleMaxSize . TxSize . fromIntegral)
        . shrink @Word64 -- Safe w.r.t the generator, despite TxSize wrapping a
                         -- Natural
        $ fromIntegral s
