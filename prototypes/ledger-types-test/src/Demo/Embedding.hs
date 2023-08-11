module Demo.Embedding where

import Prelude

import Embedding
import Export.Haskell
import Export.OpenAPI
import Module
import Parser
import Typ
import Value

import qualified Data.Aeson as JS
import qualified Data.List as L
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Yaml as Yaml

{-----------------------------------------------------------------------------
    Demonstration
    of embedding of types
------------------------------------------------------------------------------}
main :: IO ()
main = do
    Just hs <- parseLedgerTypes <$> readFile "data/json/UTxO.txt"
    Just js <- parseLedgerTypes <$> readFile "data/json/UTxO_JSON.txt"

    let hsValue = forgetNames $ resolve hs "Value"
        jsValue = forgetNames $ resolve js "Value"

        zero = Zero (IntegerV 0)
        emb = second' (map1 assocR <> representMap)
            <> first' (unit0 zero)
            <> exponential

    print $ hsValue
    print $ jsValue
    print $ typecheck emb hsValue == Just jsValue

-- | Resolve the 'Typ' corresponding to a name.
-- The result will not contain 'Var'.
resolve :: Module -> TypName -> Typ
resolve m name = resolveVars declarations typ
  where
    typ = declarations Map.! name
    declarations = moduleDeclarations m

-- | Forget all field and constructor names.
forgetNames :: Typ -> Typ
forgetNames = everywhere forget
  where
    forget (Record nas@(_:_)) =
        L.foldr (Binary Product) (snd $ last nas) (map snd $ init nas)
    forget (Union  nas@(_:_)) =
        L.foldr (Binary Sum) (snd $ last nas) (map snd $ init nas)
    forget x = x
