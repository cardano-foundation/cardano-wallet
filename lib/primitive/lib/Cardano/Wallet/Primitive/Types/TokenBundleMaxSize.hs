{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Primitive.Types.TokenBundleMaxSize
    ( TokenBundleMaxSize (..)
    )
where

import Prelude

import Cardano.Wallet.Primitive.Types.Tx.Constraints
    ( TxSize (..)
    )
import Control.DeepSeq
    ( NFData
    )
import Data.Word
    ( Word64
    )
import GHC.Generics
    ( Generic
    )
import Test.QuickCheck
    ( Arbitrary (..)
    , oneof
    )

-- | The maximum size of a serialized `TokenBundle` (`_maxValSize` in the Alonzo
-- ledger)
newtype TokenBundleMaxSize = TokenBundleMaxSize
    {unTokenBundleMaxSize :: TxSize}
    deriving (Eq, Generic, Show)

instance NFData TokenBundleMaxSize

instance Arbitrary TokenBundleMaxSize where
    arbitrary =
        TokenBundleMaxSize . TxSize
            <$> oneof
                -- Generate values close to the mainnet value of 4000 (and guard
                -- against underflow)
                [ fromIntegral . max 0 . (4000 +) <$> arbitrary @Int
                , -- Generate more extreme values (both small and large)
                  fromIntegral <$> arbitrary @Word64
                ]
    shrink (TokenBundleMaxSize (TxSize s)) =
        map (TokenBundleMaxSize . TxSize . fromIntegral)
            . shrink @Word64 -- Safe w.r.t the generator, despite TxSize wrapping a
            -- Natural
            $ fromIntegral s
