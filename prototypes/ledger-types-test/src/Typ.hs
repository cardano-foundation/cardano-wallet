module Type
    ( TypName
    , ConstructorName
    , FieldName
    , Module (..)
    , Declarations
    , Typ (..)
    , OpUnary (..)
    , OpBinary (..)
    ) where

import Prelude

import Data.Map
    ( Map )

{-----------------------------------------------------------------------------
    Mathematical types
------------------------------------------------------------------------------}
type TypName = String
type ConstructorName = String
type FieldName = String

data Module = Module
    { moduleName :: String
    , moduleDeclarations :: Declarations
    }

type Declarations = Map TypName Typ

-- | Types
data Typ
    = Abstract
    | Var TypName
    | Unary OpUnary Typ
    | Binary OpBinary Typ Typ
    | Record [(FieldName, Typ)]
        -- ^ Cartesian product with names for components.
    | Union [(ConstructorName, Typ)]
        -- ^ Disjoint union with constructor names.
    deriving (Eq, Show)

data OpUnary
    = Option
        -- ^ An option type in type A is denoted as A?
    | Sequence
        -- ^ Given a set A, A* is the set of sequences
        -- having elements taken from A.
    | PowerSet
        -- ^ Given a set A, ℙ A is the set of all the subsets of A.
    deriving (Eq, Show)

data OpBinary
    = Sum
        -- ^ A ⊎ B  denotes the disjoint union of A and B.
    | Product
        -- ^ A × B  denotes the cartesian product of A and B.
    | PartialFunction
        -- ^ A ↦ B  denotes a partial function from A to B,
        -- which can be seen as a map (dictionary)
        -- with keys in A and values in B.
    | FiniteSupport
        -- ^ We use the notation
        -- f : A →∗ B to denote a finitely supported partial function.
    deriving (Eq, Show)
