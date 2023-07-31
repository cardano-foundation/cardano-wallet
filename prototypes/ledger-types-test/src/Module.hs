-- | Module-level functions.
module Module
    ( collectNotInScope
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
collectVars = go
  where
    go Abstract = Set.empty
    go (Var name) = Set.singleton name
    go (Unary _ a) = go a
    go (Binary _ a b) = go a <> go b
    go (Record nameds) = mconcat $ map (go . snd) nameds
    go (Union nameds) = mconcat $ map (go . snd) nameds
