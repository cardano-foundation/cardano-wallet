{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  {
    flags = {};
    package = {
      specVersion = "2.2";
      identifier = { name = "cabal-cache"; version = "1.0.2.1"; };
      license = "BSD-3-Clause";
      copyright = "John Ky 2019";
      maintainer = "newhoggy@gmail.com";
      author = "John Ky";
      homepage = "https://github.com/haskell-works/cabal-cache";
      url = "";
      synopsis = "CI Assistant for Haskell projects";
      description = "CI Assistant for Haskell projects.  Implements package caching.";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [ "CHANGELOG.md" "README.md" ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."amazonka" or (errorHandler.buildDepError "amazonka"))
          (hsPkgs."amazonka-core" or (errorHandler.buildDepError "amazonka-core"))
          (hsPkgs."amazonka-s3" or (errorHandler.buildDepError "amazonka-s3"))
          (hsPkgs."antiope-core" or (errorHandler.buildDepError "antiope-core"))
          (hsPkgs."antiope-optparse-applicative" or (errorHandler.buildDepError "antiope-optparse-applicative"))
          (hsPkgs."antiope-s3" or (errorHandler.buildDepError "antiope-s3"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."conduit-extra" or (errorHandler.buildDepError "conduit-extra"))
          (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."generic-lens" or (errorHandler.buildDepError "generic-lens"))
          (hsPkgs."http-client" or (errorHandler.buildDepError "http-client"))
          (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
          (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
          (hsPkgs."process" or (errorHandler.buildDepError "process"))
          (hsPkgs."relation" or (errorHandler.buildDepError "relation"))
          (hsPkgs."resourcet" or (errorHandler.buildDepError "resourcet"))
          (hsPkgs."selective" or (errorHandler.buildDepError "selective"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          (hsPkgs."stringsearch" or (errorHandler.buildDepError "stringsearch"))
          (hsPkgs."tar" or (errorHandler.buildDepError "tar"))
          (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."topograph" or (errorHandler.buildDepError "topograph"))
          (hsPkgs."unliftio" or (errorHandler.buildDepError "unliftio"))
          (hsPkgs."zlib" or (errorHandler.buildDepError "zlib"))
          ];
        buildable = true;
        modules = [
          "Paths_cabal_cache"
          "App/Commands"
          "App/Commands/Options/Parser"
          "App/Commands/Options/Types"
          "App/Commands/SyncFromArchive"
          "App/Commands/SyncToArchive"
          "App/Commands/Version"
          "App/Static"
          "App/Static/Base"
          "App/Static/Posix"
          "App/Static/Windows"
          "HaskellWorks/CabalCache/AppError"
          "HaskellWorks/CabalCache/AWS/Env"
          "HaskellWorks/CabalCache/Concurrent/DownloadQueue"
          "HaskellWorks/CabalCache/Concurrent/Fork"
          "HaskellWorks/CabalCache/Concurrent/Type"
          "HaskellWorks/CabalCache/Core"
          "HaskellWorks/CabalCache/Data/List"
          "HaskellWorks/CabalCache/Error"
          "HaskellWorks/CabalCache/GhcPkg"
          "HaskellWorks/CabalCache/Hash"
          "HaskellWorks/CabalCache/IO/Console"
          "HaskellWorks/CabalCache/IO/Error"
          "HaskellWorks/CabalCache/IO/File"
          "HaskellWorks/CabalCache/IO/Lazy"
          "HaskellWorks/CabalCache/IO/Tar"
          "HaskellWorks/CabalCache/Location"
          "HaskellWorks/CabalCache/Metadata"
          "HaskellWorks/CabalCache/Options"
          "HaskellWorks/CabalCache/Show"
          "HaskellWorks/CabalCache/Text"
          "HaskellWorks/CabalCache/Topology"
          "HaskellWorks/CabalCache/Types"
          "HaskellWorks/CabalCache/Version"
          ];
        hsSourceDirs = [ "src" ];
        };
      exes = {
        "cabal-cache" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."cabal-cache" or (errorHandler.buildDepError "cabal-cache"))
            ];
          buildable = true;
          hsSourceDirs = [ "app" ];
          mainPath = [ "Main.hs" ];
          };
        };
      tests = {
        "cabal-cache-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."antiope-core" or (errorHandler.buildDepError "antiope-core"))
            (hsPkgs."antiope-s3" or (errorHandler.buildDepError "antiope-s3"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."generic-lens" or (errorHandler.buildDepError "generic-lens"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
            (hsPkgs."hw-hedgehog" or (errorHandler.buildDepError "hw-hedgehog"))
            (hsPkgs."hw-hspec-hedgehog" or (errorHandler.buildDepError "hw-hspec-hedgehog"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."raw-strings-qq" or (errorHandler.buildDepError "raw-strings-qq"))
            (hsPkgs."relation" or (errorHandler.buildDepError "relation"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."cabal-cache" or (errorHandler.buildDepError "cabal-cache"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.hspec-discover.components.exes.hspec-discover or (pkgs.buildPackages.hspec-discover or (errorHandler.buildToolDepError "hspec-discover:hspec-discover")))
            ];
          buildable = true;
          modules = [
            "HaskellWorks/CabalCache/AwsSpec"
            "HaskellWorks/CabalCache/LocationSpec"
            "HaskellWorks/CabalCache/QuerySpec"
            ];
          hsSourceDirs = [ "test" ];
          mainPath = [ "Spec.hs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.; }