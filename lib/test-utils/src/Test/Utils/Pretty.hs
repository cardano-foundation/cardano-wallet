-- | A convenience wrapper type for pretty-showing test values.

module Test.Utils.Pretty
    ( Pretty (..)
    , pShowBuilder
    ) where

import Prelude

import Data.Text.Class
    ( ToText (..) )
import Data.Text.Lazy.Builder
    ( Builder, fromLazyText )
import Formatting.Buildable
    ( Buildable (..) )
import Test.QuickCheck
    ( Arbitrary (..) )
import Text.Pretty.Simple
    ( pShow )

import qualified Data.Text.Lazy as TL

newtype Pretty a = Pretty { unPretty :: a }
    deriving Eq

instance Arbitrary a => Arbitrary (Pretty a) where
    arbitrary = Pretty <$> arbitrary
    shrink (Pretty a) = Pretty <$> shrink a

instance Show a => Show (Pretty a) where
    show = TL.unpack . pShow . unPretty

instance Show a => Buildable (Pretty a) where
    build = build . pShow . unPretty

instance Show a => ToText (Pretty a)

-- | Pretty-show a value as a lazy text 'Builder'. This is handy for using with
-- the "Fmt" module.
pShowBuilder :: Show a => a -> Builder
pShowBuilder = fromLazyText . pShow
