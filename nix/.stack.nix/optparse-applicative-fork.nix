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
    flags = { process = true; };
    package = {
      specVersion = "1.10";
      identifier = {
        name = "optparse-applicative-fork";
        version = "0.16.1.0";
        };
      license = "BSD-3-Clause";
      copyright = "(c) 2012-2017 Paolo Capriotti <paolo@capriotti.io>";
      maintainer = "huw.campbell@gmail.com";
      author = "Paolo Capriotti, Huw Campbell";
      homepage = "https://github.com/pcapriotti/optparse-applicative";
      url = "";
      synopsis = "Utilities and combinators for parsing command line options";
      description = "optparse-applicative is a haskell library for parsing options\non the command line, and providing a powerful applicative\ninterface for composing them.\n\noptparse-applicative takes care of reading and validating the\narguments passed to the command line, handling and reporting\nerrors, generating a usage line, a comprehensive help screen,\nand enabling context-sensitive bash, zsh, and fish completions.\n\nSee the included README for detailed instructions and examples,\nwhich is also available on github\n<https://github.com/pcapriotti/optparse-applicative>.";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = ([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."transformers-compat" or (errorHandler.buildDepError "transformers-compat"))
          (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
          ] ++ (pkgs.lib).optional (flags.process) (hsPkgs."process" or (errorHandler.buildDepError "process"))) ++ (pkgs.lib).optionals (!(compiler.isGhc && (compiler.version).ge "8")) [
          (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
          (hsPkgs."fail" or (errorHandler.buildDepError "fail"))
          ];
        buildable = true;
        };
      tests = {
        "tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."optparse-applicative-fork" or (errorHandler.buildDepError "optparse-applicative-fork"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8")) (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"));
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/optparse-applicative";
      rev = "90f5ee66e2df4e7c3b3877b4d28babb8163e96b4";
      sha256 = "13ax7vv1k885bki8a72q5f6zsk573fdkbqsra4rhf4snd6hzms6h";
      }) // {
      url = "https://github.com/input-output-hk/optparse-applicative";
      rev = "90f5ee66e2df4e7c3b3877b4d28babb8163e96b4";
      sha256 = "13ax7vv1k885bki8a72q5f6zsk573fdkbqsra4rhf4snd6hzms6h";
      };
    }