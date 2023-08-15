module Demo.Export where

import Prelude

import Export.Haskell
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
    Just json <-
        parseLedgerTypes <$> readFile "data/json/UTxO_JSON.txt"

    Yaml.encodeFile "data/gen/UTxO_JSON.yaml"
        $ getOpenAPISchema
        $ schemaFromModule
        $ json

    writeFile "data/gen/UTxO_JSON.hs"
        $ prettyPrint
        $ haskellFromModule
        $ json
