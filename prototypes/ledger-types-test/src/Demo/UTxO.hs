module Demo.UTxO where

import Prelude

import Elaborate
import Embedding
import Export.Haskell
import Export.OpenAPI
import Module
import Parser
import Typ
import Value

import qualified Data.List as L
import qualified Data.Map as Map
import qualified Data.Yaml as Yaml

{-----------------------------------------------------------------------------
    Demonstration
    of mapping Haskell types to JSON
------------------------------------------------------------------------------}
export :: IO ()
export = do
    Just hs <- parseLedgerTypes <$> readFile "data/json/UTxO.txt"
    Just js <- parseLedgerTypes <$> readFile "data/json/UTxO_JSON.txt"

    let hsValue = forgetNames $ resolve hs "Value"
        jsValue = forgetNames $ resolve js "Value"

    putStrLn $ "Check types"
    print $
        (jsValue `elaborates`) <$> typecheck hsIntoJs hsValue

    Yaml.encodeFile "data/gen/UTxO_JSON.yaml"
        $ getOpenAPISchema
        $ schemaFromModule
        $ js

    writeFile "src/TestGen_UTxO.hs"
        $ prettyPrint
        $ haskellFromModule
        $ hs { moduleName = "TestGen_UTxO" }

load_Value_JSON :: IO Typ
load_Value_JSON = do
    Just js <- parseLedgerTypes <$> readFile "data/json/UTxO_JSON.txt"
    pure $ resolve js "Value"

hsIntoJs :: EmbeddingTyp
hsIntoJs
    = second' (map1 assocR <> representMap)
    <> first' (unit0 zero)
    <> exponential
  where
    zero = Zero (IntegerV 0)

{-----------------------------------------------------------------------------
    Helpers
------------------------------------------------------------------------------}
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