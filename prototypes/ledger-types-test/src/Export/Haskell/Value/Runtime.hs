-- | Export values of Haskell types to 'Value'.
module Export.Haskell.Value.Runtime
    ( ToValue (..)
    , V.Value (..)
    ) where

import Prelude ((.))

import qualified Value as V

import qualified Data.ByteString
import qualified Data.Map
import qualified Data.Set
import qualified Data.Text
import qualified GHC.Generics
import qualified Numeric.Natural
import qualified Prelude

{-----------------------------------------------------------------------------
    Runtime definitions
------------------------------------------------------------------------------}
-- | Class of types that can be converted to 'Value'.
--
-- Note: The 'Ord' constraint is necessary to deal with 'Set'.
class Prelude.Ord a => ToValue a where
    toValue :: a -> V.Value

z :: V.ZeroF -> V.Value
z = V.Zero

instance ToValue Prelude.Bool               where toValue = z . V.BoolV
instance ToValue Data.ByteString.ByteString where toValue = z . V.BytesV
instance ToValue Prelude.Integer            where toValue = z . V.IntegerV
instance ToValue Numeric.Natural.Natural    where toValue = z . V.NaturalV
instance ToValue Data.Text.Text             where toValue = z . V.TextV
instance ToValue ()                         where toValue _ = z V.UnitV

instance ToValue a => ToValue (Prelude.Maybe a) where
    toValue = V.One . V.OptionV . Prelude.fmap toValue

instance ToValue a => ToValue [a] where
    toValue = V.One . V.SequenceV . Prelude.fmap toValue

instance ToValue a => ToValue (Data.Set.Set a) where
    toValue = V.One . V.PowerSetV . Data.Set.map toValue

instance (ToValue a, ToValue b) => ToValue (Prelude.Either a b) where
    toValue (Prelude.Left a) = V.SumV 0 (toValue a)
    toValue (Prelude.Right b) = V.SumV 1 (toValue b)

instance (ToValue a, ToValue b) => ToValue (a,b) where
    toValue (a,b) = V.ProductV [toValue a, toValue b]

instance (ToValue a, ToValue b) => ToValue (Data.Map.Map a b) where
    toValue
        = V.Two . V.FiniteMapV
        . Data.Map.fromList
        . Prelude.map (\(k,v) -> (toValue k, toValue v))
        . Data.Map.toList
