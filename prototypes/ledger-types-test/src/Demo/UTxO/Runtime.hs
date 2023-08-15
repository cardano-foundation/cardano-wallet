{-# LANGUAGE OverloadedStrings #-}
module Demo.UTxO.Runtime where

import Prelude

import Demo.UTxO
    ( hsIntoJs, load_Value_JSON )
import Embedding
    ( embed, to, exponential, unit0, first', unit0, second', representMap )
import Export.Haskell.Value.Runtime
    ( ToValue (..) )
import Export.OpenAPI.Value
    ( jsonFromValue )

import TestGen_UTxO

import qualified Value as V
import qualified Data.Aeson as JS
import qualified Data.Map

main :: IO ()
main = do
    putStrLn "Converting example Cardano Value to JSON"

    typ <- load_Value_JSON
    let fromHaskell = jsonFromValue typ . to (embed hsIntoJs) . toValue
    print $ fromHaskell example

example :: Value
example = Data.Map.fromList
    [ (Ada (), 1000 )
    , (Asset asset1, 42)
    , (Asset asset2, 21)
    ]

asset1 :: AssetIDNonAda
asset1 = AssetIDNonAda
    { policyId = "1337"
    , assetName = "Leetcoin"
    }

asset2 :: AssetIDNonAda
asset2 = AssetIDNonAda
    { policyId = "101"
    , assetName = "Lolcoin"
    }
