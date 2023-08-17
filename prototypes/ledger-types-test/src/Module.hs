-- | Module-level functions.
module Module
    ( collectNotInScope
    , resolveVars
    , globalConstants
    ) where

import Prelude

import Data.Set
    ( Set )
import Typ
    ( Declarations, Module (..), Typ (..), TypName, everything, everywhere )

import qualified Data.Map as Map
import qualified Data.Set as Set

{-----------------------------------------------------------------------------
    Module
------------------------------------------------------------------------------}
-- | Resolve all variables in a 'Typ' that can be resolved
-- using the given declarations.
--
-- This function will loop in the case of recursive definitions.
resolveVars :: Declarations -> Typ -> Typ
resolveVars declarations = everywhere resolve
  where
    resolve (Var name) = case Map.lookup name declarations of
        Nothing -> Var name
        Just typ -> everywhere resolve typ
    resolve a = a

-- | Collect all type names that have not been defined in the 'Module'.
collectNotInScope :: Module -> Set TypName
collectNotInScope Module{moduleDeclarations=declarations} =
    rhs Set.\\ (lhs <> globalConstants)
  where
    lhs = Map.keysSet declarations
    rhs = mconcat . map collectVars $ Map.elems declarations

-- | Globally known type identifiers.
globalConstants :: Set TypName
globalConstants = Set.fromList ["ℕ","ℤ","Bool","Bytes","Text","Unit"]

-- | Collect all 'Var' in a type.
collectVars :: Typ -> Set TypName
collectVars = everything (<>) vars
  where
    vars (Var name) = Set.singleton name
    vars _ = Set.empty
