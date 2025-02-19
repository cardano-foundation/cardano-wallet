module Cardano.Wallet.Deposit.HTTP.OpenAPISpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Deposit.HTTP.Types.OpenAPI
    ( generateOpenapi3
    )
import Paths_cardano_deposit_wallet
    ( getDataDir
    , getDataFileName
    )
import System.Directory
    ( doesDirectoryExist
    , doesFileExist
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldReturn
    )
import Test.Hspec.Golden
    ( Golden (..)
    )

import qualified Data.ByteString.Lazy.Char8 as BL

spec :: Spec
spec = do
    describe "data dir" $ do
        it "should exist" $ do
            f <- getDataDir
            doesDirectoryExist f `shouldReturn` True
    describe "swagger.yaml" $ do
        it "should be generated" $ do
            f <- getDataFileName "data/swagger.json"
            doesFileExist f `shouldReturn` True
        it "contains the actual schema" $ do
            f <- getDataFileName "data/swagger.json"
            let output' = generateOpenapi3
            pure $ swaggerGolden f $ BL.unpack output'

swaggerGolden :: FilePath -> String -> Golden String
swaggerGolden goldenPath output_ =
    Golden
        { output = output_
        , encodePretty = show
        , writeToFile = writeFile
        , readFromFile = readFile
        , goldenFile = goldenPath
        , actualFile = Nothing
        , failFirstTime = False
        }
