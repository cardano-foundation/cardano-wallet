{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Utils.Roundtrip
    ( jsonRoundtripAndGolden
    , httpApiDataRoundtrip
    ) where

import Prelude

import Data.Aeson
    ( FromJSON (..)
    , ToJSON (..)
    )
import Data.Char
    ( isAlphaNum
    )
import Data.Maybe
    ( fromMaybe
    )
import Data.Proxy
    ( Proxy (..)
    )
import Data.Typeable
    ( Typeable
    , splitTyConApp
    , tyConName
    , typeRep
    )
import System.Environment
    ( lookupEnv
    )
import Test.Aeson.GenericSpecs
    ( GoldenDirectoryOption (CustomDirectoryName)
    , Settings
    , defaultSettings
    , goldenDirectoryOption
    , randomMismatchOption
    , sampleSize
    , useModuleNameAsSubDirectory
    )
import Test.Aeson.Internal.GoldenSpecs
    ( goldenSpecsWithNotePlain
    )
import Test.Aeson.Internal.RoundtripSpecs
    ( roundtripSpecs
    )
import Test.Aeson.Internal.Utils
    ( RandomMismatchOption (RandomMismatchError)
    , TypeName (..)
    , TypeNameInfo (..)
    , mkTypeNameInfo
    )
import Test.Hspec
    ( Spec
    , it
    , runIO
    , shouldBe
    )
import Test.QuickCheck
    ( Arbitrary (..)
    , property
    )
import Text.Read
    ( readMaybe
    )
import Web.HttpApiData
    ( FromHttpApiData (..)
    , ToHttpApiData (..)
    )

-- Golden tests files are generated automatically on first run. On later runs
-- we check that the format stays the same. The golden files should be tracked
-- in git.
--
-- Example:
-- >>> roundtripAndGolden $ Proxy @ Wallet
--
-- ...will compare @ToJSON@ of @Wallet@ against `Wallet.json`. It may either
-- match and succeed, or fail and write `Wallet.faulty.json` to disk with the
-- new format. Faulty golden files should /not/ be committed.
--
-- The directory `test/data/Cardano/Wallet/Api` is used.
jsonRoundtripAndGolden
    :: forall a
     . (Arbitrary a, ToJSON a, FromJSON a, Typeable a)
    => FilePath
    -> Proxy a
    -> Spec
jsonRoundtripAndGolden dir proxy = do
    roundtripSpecs proxy
    typeNameInfo <- runIO mkCompatibleTypeNameInfo
    sampleSize <- runIO getSampleSize
    goldenSpecsWithNotePlain (settings{sampleSize}) typeNameInfo Nothing
  where
    -- When generating new JSON golden files for some type, it is occasionally
    -- useful to generate a different number of samples to the default number.
    --
    -- Specifying the environment variable "GOLDEN_SAMPLE_SIZE=<n>" together
    -- with "CREATE_MISSING_GOLDEN=TRUE" will cause the newly-created golden
    -- file to have 'n' samples.
    --
    getSampleSize :: IO Int
    getSampleSize =
        fromMaybe sampleSizeDefault . (readMaybe =<<)
            <$> lookupEnv sampleSizeVariable
      where
        sampleSizeDefault = 10
        sampleSizeVariable = "GOLDEN_SAMPLE_SIZE"

    -- NOTE
    -- We use a custom 'TypeNameInfo' instead of the default one provided by
    -- @hspec-golden-aeson@ because the defaults generates names that are
    -- invalid for Windows file-system.
    mkCompatibleTypeNameInfo :: IO (TypeNameInfo a)
    mkCompatibleTypeNameInfo = do
        typeNameInfo <- mkTypeNameInfo settings proxy
        pure
            typeNameInfo
                { typeNameTypeName =
                    mkValidForWindows (typeNameTypeName typeNameInfo)
                }
      where
        mkValidForWindows :: TypeName -> TypeName
        mkValidForWindows (TypeName typeName) =
            TypeName (filter isAlphaNum typeName)

    settings :: Settings
    settings =
        defaultSettings
            { goldenDirectoryOption = CustomDirectoryName dir
            , useModuleNameAsSubDirectory = False
            , -- Note that we fail the test if the random seed does not produce the
              -- same values as those within the golden file. It's important that
              -- we do fail, because otherwise we may inadvertently fail to cover
              -- new additions to types and their associated generators.
              randomMismatchOption = RandomMismatchError
            }

-- Perform roundtrip tests for FromHttpApiData & ToHttpApiData instances
httpApiDataRoundtrip
    :: forall a
     . ( Arbitrary a
       , FromHttpApiData a
       , ToHttpApiData a
       , Typeable a
       , Eq a
       , Show a
       )
    => Proxy a
    -> Spec
httpApiDataRoundtrip proxy =
    it ("URL encoding of " <> cons (typeRep proxy)) $ property $ \(x :: a) -> do
        let bytes = toUrlPiece x
        let x' = parseUrlPiece bytes
        x' `shouldBe` Right x
  where
    cons rep =
        let
            (c, args) = splitTyConApp rep
        in
            case args of
                [] ->
                    tyConName c
                xs ->
                    "(" <> tyConName c <> " " <> unwords (cons <$> xs) <> ")"
