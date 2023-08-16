{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2023 Cardano Foundation
-- License: Apache-2.0
module Cardano.Wallet.Write.Tx.Balance.TokenBundleSize.Gen
    ( genTokenBundleMaxSize
    , shrinkTokenBundleMaxSize
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.Tx.Constraints
    ( TxSize (..) )
import Cardano.Wallet.Write.Tx.Balance.TokenBundleSize
    ( TokenBundleMaxSize (..) )
import Data.Word
    ( Word64 )
import Test.QuickCheck
    ( Arbitrary (arbitrary, shrink), Gen, oneof )

genTokenBundleMaxSize :: Gen TokenBundleMaxSize
genTokenBundleMaxSize = TokenBundleMaxSize . TxSize <$>
    oneof
        -- Generate values close to the mainnet value of 4000 (and guard
        -- against underflow)
        [ fromIntegral . max 0 . (4000 +) <$> arbitrary @Int

        -- Generate more extreme values (both small and large)
        , fromIntegral <$> arbitrary @Word64
        ]

shrinkTokenBundleMaxSize :: TokenBundleMaxSize -> [TokenBundleMaxSize]
shrinkTokenBundleMaxSize (TokenBundleMaxSize (TxSize s)) =
    map (TokenBundleMaxSize . TxSize . fromIntegral)
    . shrink @Word64 -- Safe w.r.t the generator, despite TxSize wrapping a
                     -- Natural
    $ fromIntegral s
