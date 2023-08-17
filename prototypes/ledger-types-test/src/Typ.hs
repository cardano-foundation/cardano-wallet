module Type
    ( -- * Types
      TypName
    , ConstructorName
    , FieldName
    , Module (..)
    , Declarations
    , Typ (..)
    , OpUnary (..)
    , OpBinary (..)
    
    -- * Traversals
    , everywhere
    , everything
    ) where

import Prelude

import Data.Map
    ( Map )

import qualified Data.List as L

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

{-----------------------------------------------------------------------------
    Traversals

    Terminology from "Scrap your boilerplate"
    https://www.microsoft.com/en-us/research/wp-content/uploads/2003/01/hmap.pdf
------------------------------------------------------------------------------}
-- | Apply a transformation everywhere, bottom-up everywhere.
everywhere :: (Typ -> Typ) -> Typ -> Typ
everywhere f = every
 where
    every = f . recurse

    recurse a@(Abstract) =
        Abstract
    recurse a@(Var _) =
        a
    recurse (Unary op a) =
        Unary op (every a)
    recurse (Binary op a b) =
        Binary op (every a) (every b)
    recurse (Record nas) =
        Record [ (n,every a) | (n,a) <- nas ]
    recurse (Union nas) =
        Union [ (n,every a) | (n,a) <- nas ]

-- | Summarise all nodes in top-down, left-to-right.
everything :: (r -> r -> r) -> (Typ -> r) -> (Typ -> r)
everything combine f = recurse
  where
    recurse x@(Abstract) =
        f x
    recurse x@(Var _) =
        f x
    recurse x@(Unary op a) =
        f x `combine` (recurse a)
    recurse x@(Binary op a b) =
        f x `combine` (recurse a `combine` recurse b)
    recurse x@(Record nas) =
        L.foldl' combine (f x) [ recurse a | (_,a) <- nas ]
    recurse x@(Union nas) =
        L.foldl' combine (f x) [ recurse a | (_,a) <- nas ]
