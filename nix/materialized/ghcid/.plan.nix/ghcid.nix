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
      specVersion = "1.18";
      identifier = { name = "ghcid"; version = "0.8.7"; };
      license = "BSD-3-Clause";
      copyright = "Neil Mitchell 2014-2020";
      maintainer = "Neil Mitchell <ndmitchell@gmail.com>";
      author = "Neil Mitchell <ndmitchell@gmail.com>, jpmoresmau";
      homepage = "https://github.com/ndmitchell/ghcid#readme";
      url = "";
      synopsis = "GHCi based bare bones IDE";
      description = "Either \\\"GHCi as a daemon\\\" or \\\"GHC + a bit of an IDE\\\". A very simple Haskell development tool which shows you the errors in your project and updates them whenever you save. Run @ghcid --topmost --command=ghci@, where @--topmost@ makes the window on top of all others (Windows only) and @--command@ is the command to start GHCi on your project (defaults to @ghci@ if you have a @.ghci@ file, or else to @cabal repl@).";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
      dataDir = "";
      dataFiles = [];
      extraSrcFiles = [];
      extraTmpFiles = [];
      extraDocFiles = [ "CHANGES.txt" "README.md" ];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
          (hsPkgs."process" or (errorHandler.buildDepError "process"))
          (hsPkgs."ansi-terminal" or (errorHandler.buildDepError "ansi-terminal"))
          (hsPkgs."cmdargs" or (errorHandler.buildDepError "cmdargs"))
          ];
        buildable = true;
        modules = [
          "Paths_ghcid"
          "Language/Haskell/Ghcid/Escape"
          "Language/Haskell/Ghcid/Parser"
          "Language/Haskell/Ghcid/Types"
          "Language/Haskell/Ghcid/Util"
          "Language/Haskell/Ghcid"
          ];
        hsSourceDirs = [ "src" ];
        };
      exes = {
        "ghcid" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."fsnotify" or (errorHandler.buildDepError "fsnotify"))
            (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."cmdargs" or (errorHandler.buildDepError "cmdargs"))
            (hsPkgs."ansi-terminal" or (errorHandler.buildDepError "ansi-terminal"))
            (hsPkgs."terminal-size" or (errorHandler.buildDepError "terminal-size"))
            ] ++ (if system.isWindows
            then [ (hsPkgs."Win32" or (errorHandler.buildDepError "Win32")) ]
            else [ (hsPkgs."unix" or (errorHandler.buildDepError "unix")) ]);
          buildable = true;
          modules = [
            "Language/Haskell/Ghcid/Escape"
            "Language/Haskell/Ghcid/Parser"
            "Language/Haskell/Ghcid/Terminal"
            "Language/Haskell/Ghcid/Types"
            "Language/Haskell/Ghcid/Util"
            "Language/Haskell/Ghcid"
            "Paths_ghcid"
            "Session"
            "Wait"
            ];
          hsSourceDirs = [ "src" ];
          mainPath = [ "Ghcid.hs" ] ++ [ "" ];
          };
        };
      tests = {
        "ghcid_test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."fsnotify" or (errorHandler.buildDepError "fsnotify"))
            (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
            (hsPkgs."ansi-terminal" or (errorHandler.buildDepError "ansi-terminal"))
            (hsPkgs."terminal-size" or (errorHandler.buildDepError "terminal-size"))
            (hsPkgs."cmdargs" or (errorHandler.buildDepError "cmdargs"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            ] ++ (if system.isWindows
            then [ (hsPkgs."Win32" or (errorHandler.buildDepError "Win32")) ]
            else [ (hsPkgs."unix" or (errorHandler.buildDepError "unix")) ]);
          buildable = true;
          modules = [
            "Ghcid"
            "Language/Haskell/Ghcid"
            "Language/Haskell/Ghcid/Escape"
            "Language/Haskell/Ghcid/Parser"
            "Language/Haskell/Ghcid/Terminal"
            "Language/Haskell/Ghcid/Types"
            "Language/Haskell/Ghcid/Util"
            "Paths_ghcid"
            "Session"
            "Test/API"
            "Test/Ghcid"
            "Test/Parser"
            "Test/Util"
            "Wait"
            ];
          hsSourceDirs = [ "src" ];
          mainPath = [ "Test.hs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.; }