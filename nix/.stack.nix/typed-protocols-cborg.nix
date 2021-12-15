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
      specVersion = "1.10";
      identifier = { name = "typed-protocols-cborg"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "2019-2021 Input Output (Hong Kong) Ltd.";
      maintainer = "alex@well-typed.com, duncan@well-typed.com, marcin.szamotulski@iohk.io";
      author = "Alexander Vieth, Duncan Coutts, Marcin Szamotulski";
      homepage = "";
      url = "";
      synopsis = "CBOR codecs for typed-protocols";
      description = "";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
          (hsPkgs."typed-protocols" or (errorHandler.buildDepError "typed-protocols"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/ouroboros-network";
      rev = "d613de3d872ec8b4a5da0c98afb443f322dc4dab";
      sha256 = "0lfbipfdrzay8v1pcazx0qgkda3d1j0505yig9jrml9j7991rmhl";
      }) // {
      url = "https://github.com/input-output-hk/ouroboros-network";
      rev = "d613de3d872ec8b4a5da0c98afb443f322dc4dab";
      sha256 = "0lfbipfdrzay8v1pcazx0qgkda3d1j0505yig9jrml9j7991rmhl";
      };
    postUnpack = "sourceRoot+=/typed-protocols-cborg; echo source root reset to \$sourceRoot";
    }