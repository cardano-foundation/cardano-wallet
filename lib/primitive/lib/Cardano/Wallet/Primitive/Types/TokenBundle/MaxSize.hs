{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Primitive.Types.TokenBundle.MaxSize where

import Prelude

import Control.DeepSeq
    ( NFData )
import Data.IntCast
    ( intCast )
import Data.Word
    ( Word64 )
import GHC.Generics
    ( Generic )
import Numeric.Natural
    ( Natural )
import Test.QuickCheck
    ( Arbitrary (..) )
import Test.QuickCheck.Gen
    ( oneof )

-- | The maximum size of a serialized `TokenBundle`
newtype TokenBundleMaxSize =
    TokenBundleMaxSize { naturalTokenBundleMaxSize :: Natural }
    deriving (Eq, Generic, Show)

instance NFData TokenBundleMaxSize

instance Arbitrary TokenBundleMaxSize where
    arbitrary = TokenBundleMaxSize <$> oneof
        -- Generate values close to the mainnet value of 4000
        -- (and guard against underflow)
        [ fromIntegral . max 0 . (4000 +) <$> arbitrary @Int

        -- Generate more extreme values (both small and large)
        , fromIntegral <$> arbitrary @Word64
        ]
    shrink (TokenBundleMaxSize s) =
        map (TokenBundleMaxSize . intCast)
        . shrink @Word64 -- Safe w.r.t the generator, despite wrapping a Natural
        $ fromIntegral s -- Safe, as max byte size of a bundle always fits Word64
