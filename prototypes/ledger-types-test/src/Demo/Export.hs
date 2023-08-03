module Demo.Export where

import Prelude

import Export.OpenAPI
import Module
import Parser
import Typ

import qualified Data.Aeson as JS
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Yaml as Yaml

{-----------------------------------------------------------------------------
    Demonstration
    of exporting types to JSON and Haskell
------------------------------------------------------------------------------}
main :: IO ()
main = do
    Just elaborated <-
        parseLedgerTypes <$> readFile "data/BabbageTxOut.txt"

    Yaml.encodeFile "data/gen/BabbageTxOut.yaml"
        $ getOpenAPISchema
        $ schemaFromModule
        $ elaborated {
            moduleDeclarations = convertToJSON (moduleDeclarations elaborated)
          }
