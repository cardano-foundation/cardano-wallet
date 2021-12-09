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
      identifier = { name = "freer-extras"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "tobias.pflug@iohk.io";
      author = "Tobias Pflug";
      homepage = "";
      url = "";
      synopsis = "Useful extensions to simple-freer";
      description = "freer-extras provides logging and monitoring functions extending simple-freer";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
          (hsPkgs."freer-simple" or (errorHandler.buildDepError "freer-simple"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
          (hsPkgs."streaming" or (errorHandler.buildDepError "streaming"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/plutus";
      rev = "1efbb276ef1a10ca6961d0fd32e6141e9798bd11";
      sha256 = "1jicyk4hr8p0xksj4048gdxndrb42jz4wsnkhc3ymxbm5v6snalf";
      }) // {
      url = "https://github.com/input-output-hk/plutus";
      rev = "1efbb276ef1a10ca6961d0fd32e6141e9798bd11";
      sha256 = "1jicyk4hr8p0xksj4048gdxndrb42jz4wsnkhc3ymxbm5v6snalf";
      };
    postUnpack = "sourceRoot+=/freer-extras; echo source root reset to \$sourceRoot";
    }