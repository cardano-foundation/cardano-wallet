{-# LANGUAGE ScopedTypeVariables #-}

module Test.Utils.Roundtrip
    ( jsonRoundtripAndGolden
    , httpApiDataRoundtrip
    ) where

import Prelude

import Data.Aeson
    ( FromJSON (..), ToJSON (..) )
import Data.Char
    ( isAlphaNum )
import Data.Proxy
    ( Proxy (..) )
import Data.Typeable
    ( Typeable, splitTyConApp, tyConName, typeRep )
import Test.Aeson.GenericSpecs
    ( GoldenDirectoryOption (CustomDirectoryName)
    , Settings
    , defaultSettings
    , goldenDirectoryOption
    , sampleSize
    , useModuleNameAsSubDirectory
    )
import Test.Aeson.Internal.GoldenSpecs
    ( goldenSpecsWithNotePlain )
import Test.Aeson.Internal.RoundtripSpecs
    ( roundtripSpecs )
import Test.Aeson.Internal.Utils
    ( TypeName (..), TypeNameInfo (..), mkTypeNameInfo )
import Test.Hspec
    ( Spec, it, runIO, shouldBe )
import Test.QuickCheck
    ( Arbitrary (..), property )
import Web.HttpApiData
    ( FromHttpApiData (..), ToHttpApiData (..) )

-- Golden tests files are generated automatically on first run. On later runs
-- we check that the format stays the same. The golden files should be tracked
-- in git.
--
-- Example:
-- >>> roundtripAndGolden $ Proxy @ Wallet
--
-- ...will compare @ToJSON@ of @Wallet@ against `Wallet.json`. It may either
-- match and succeed, or fail and write `Wallet.faulty.json` to disk with the
-- new format. Faulty golden files should /not/ be commited.
--
-- The directory `test/data/Cardano/Wallet/Api` is used.
jsonRoundtripAndGolden
    :: forall a. (Arbitrary a, ToJSON a, FromJSON a, Typeable a)
    => FilePath
    -> Proxy a
    -> Spec
jsonRoundtripAndGolden dir proxy = do
    roundtripSpecs proxy
    typeNameInfo <- runIO mkCompatibleTypeNameInfo
    goldenSpecsWithNotePlain settings typeNameInfo Nothing
  where
    -- NOTE
    -- We use a custom 'TypeNameInfo' instead of the default one provided by
    -- @hspec-golden-aeson@ because the defaults generates names that are
    -- invalid for Windows file-system.
    mkCompatibleTypeNameInfo :: IO (TypeNameInfo a)
    mkCompatibleTypeNameInfo = do
        typeNameInfo <- mkTypeNameInfo settings proxy
        pure $ typeNameInfo
            { typeNameTypeName =
                mkValidForWindows (typeNameTypeName typeNameInfo)
            }
      where
        mkValidForWindows :: TypeName -> TypeName
        mkValidForWindows (TypeName typeName) =
            TypeName (filter isAlphaNum typeName)

    settings :: Settings
    settings = defaultSettings
        { goldenDirectoryOption =
            CustomDirectoryName dir
        , useModuleNameAsSubDirectory =
            False
        , sampleSize = 10
        }

-- Perform roundtrip tests for FromHttpApiData & ToHttpApiData instances
httpApiDataRoundtrip
    :: forall a.
        ( Arbitrary a
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
