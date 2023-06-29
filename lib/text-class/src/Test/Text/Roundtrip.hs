{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Text.Roundtrip
    ( textRoundtrip
    ) where

import Prelude

import Data.Proxy
    ( Proxy
    )
import Data.Text.Class
    ( FromText (..)
    , ToText (..)
    )
import Data.Typeable
    ( Typeable
    , typeRep
    )
import Test.Hspec
    ( Spec
    , it
    )
import Test.QuickCheck
    ( Arbitrary (..)
    , property
    , (===)
    )

-- | Constructs a test to check that roundtrip textual encoding and decoding
-- is possible for values of the given type.
textRoundtrip
    :: forall a
     . (Arbitrary a, Eq a, Show a, ToText a, FromText a, Typeable a)
    => Proxy a
    -> Spec
textRoundtrip proxy = it (show (typeRep proxy))
    $ property
    $ \a -> fromText (toText @a a) === Right a
