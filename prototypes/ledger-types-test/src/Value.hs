module Value where

import Prelude

import Data.ByteString
    ( ByteString )
import Data.Text
    ( Text )
import Numeric.Natural
    ( Natural )
import Typ
    ( Typ (..), OpBinary (..), OpUnary (..) )

import qualified Data.Map as Map
import qualified Data.Set as Set

{-----------------------------------------------------------------------------
    Values corresponding to Typ
------------------------------------------------------------------------------}
type Ix = Int

data Value
    = Zero ZeroF
    | One (OneF Value)
    | Two (TwoF Value Value)
    | ProductV [Value]
        -- ^ N-ary products
    | SumV Ix Value
        -- ^ N-ary sums
    deriving (Eq, Ord, Show)

data ZeroF
    = BoolV Bool
    | BytesV ByteString
    | IntegerV Integer
    | NaturalV Natural
    | TextV Text
    | UnitV
    deriving (Eq, Ord, Show)

data OneF a
    = OptionV (Maybe a)
    | SequenceV [a]
    | PowerSetV (Set.Set a)
    deriving (Eq, Ord, Show)

data TwoF a b
    = FiniteMapV (Map.Map a b)
    deriving (Eq, Ord, Show)

{-----------------------------------------------------------------------------
    Type checking
------------------------------------------------------------------------------}
-- | Check whether a 'Value' has the given 'Typ'.
--
-- Field and constructor names are ignored.
hasTyp :: Value -> Typ -> Bool
hasTyp (Zero a) (Var b)
    = b == case a of
        BoolV{} -> "Bool"
        BytesV{} -> "Bytes"
        IntegerV{} -> "ℤ"
        NaturalV{} -> "ℕ"
        TextV{} -> "Text"
        UnitV -> "Unit"
hasTyp (One a) (Unary op typ)
    = hasTyp1 a op typ
hasTyp (ProductV [a,b]) (Binary Product ta tb)
    = (a `hasTyp` ta) && (b `hasTyp` tb)
hasTyp (ProductV as) (Record fields)
    | length as == length fields
        = and (zipWith hasTyp as $ map snd fields)
    | otherwise
        = False
hasTyp (SumV 0 a) (Binary Sum ta _)
    = a `hasTyp` ta
hasTyp (SumV 1 b) (Binary Sum _ tb)
    = b `hasTyp` tb
hasTyp (SumV ix a) (Union constructors)
    | 0 <= ix && ix < length constructors
        = a `hasTyp` (snd $ constructors !! ix)
    | otherwise
        = False
hasTyp (Two a) (Binary op typ1 typ2)
    = hasTyp2 a op typ1 typ2
hasTyp _ _
    = False

hasTyp1 :: OneF Value -> OpUnary -> Typ -> Bool
hasTyp1 (OptionV a) Option t
    = all (`hasTyp` t) a
hasTyp1 (SequenceV as) Sequence t
    = all (`hasTyp` t) as
hasTyp1 (PowerSetV as) PowerSet t
    = all (`hasTyp` t) as
hasTyp1 _ _ _
    = False

hasTyp2 :: TwoF Value Value -> OpBinary -> Typ -> Typ -> Bool
hasTyp2 (FiniteMapV m) PartialFunction ta tb
    = all (`hasTyp` ta) (Map.keys m)
    && all (`hasTyp` tb) (Map.elems m)
hasTyp2 (FiniteMapV m) FiniteSupport ta tb
    = hasTyp2 (FiniteMapV m) PartialFunction ta tb
hasTyp2 _ _ _ _
    = False
