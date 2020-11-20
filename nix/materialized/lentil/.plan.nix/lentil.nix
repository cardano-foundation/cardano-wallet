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
    flags = { developer = false; };
    package = {
      specVersion = "1.24";
      identifier = { name = "lentil"; version = "1.3.2.0"; };
      license = "GPL-3.0-only";
      copyright = "© 2015-2020 Francesco Ariis et al. (check authors.txt)";
      maintainer = "Francesco Ariis <fa-ml@ariis.it>";
      author = "Francesco Ariis <fa-ml@ariis.it> et al.\n(check authors.txt)";
      homepage = "http://www.ariis.it/static/articles/lentil/page.html";
      url = "";
      synopsis = "frugal issue tracker";
      description = "minumum effort, cohesive issue tracker based on\nubiquitous @TODO@s and @FIXME@s conventions.\nCheck homepage for manual, tutorial, binaries,\nexamples.";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
      dataDir = "";
      dataFiles = [];
      extraSrcFiles = [
        "stack.yaml"
        "test/test-files/lang-comm/clang.c"
        "test/test-files/lang-comm/haskell.hs"
        "test/test-files/lang-comm/javascript.js"
        "test/test-files/lang-comm/out.blocks"
        "test/test-files/lang-comm/pascal.pas"
        "test/test-files/lang-comm/python.py"
        "test/test-files/lang-comm/ruby.rb"
        "test/test-files/lang-comm/perl.pl"
        "test/test-files/lang-comm/shell.sh"
        "test/test-files/lang-comm/nix.nix"
        "test/test-files/lang-comm/xml.xml"
        "test/test-files/lang-comm/erlang.erl"
        "test/test-files/lang-comm/ocaml.ml"
        "test/test-files/lang-comm/rust.rs"
        "test/test-files/lang-comm/r.r"
        "test/test-files/lang-comm/standard-ml.sml"
        "test/test-files/lang-comm/rst.rst"
        "test/test-files/lang-comm/org-mode.org"
        "test/test-files/lang-comm/text.txt"
        "test/test-files/specific/contiguous.c"
        "test/test-files/specific/cont-custom.c"
        "test/test-files/specific/uppercase.C"
        "test/test-files/specific/xyz.xyz"
        "test/test-files/specific/custom-fwords.c"
        "test/test-files/test-proj/base-case/fold-a/foo1.hs"
        "test/test-files/test-proj/base-case/fold-b/foo2.hs"
        "test/test-files/test-proj/base-case/fold-c/foo3.hs"
        "test/test-files/test-proj/base-case/fold-c/sub-fold/foo4.hs"
        "test/test-files/test-proj/base-case/fold-c/sub-fold/foo5.hs"
        "test/test-files/test-proj/dot-folders/foo1.hs"
        "test/test-files/test-proj/dot-folders/.alfa/foo1.hs"
        "test/test-files/test-proj/dot-folders/_beta/foo1.hs"
        ];
      extraTmpFiles = [];
      extraDocFiles = [
        "README"
        "changes.txt"
        "doc/usr/page.rst"
        "doc/usr/test.zip"
        "issues.txt"
        "authors.txt"
        ];
      };
    components = {
      exes = {
        "lentil" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."ansi-wl-pprint" or (errorHandler.buildDepError "ansi-wl-pprint"))
            (hsPkgs."csv" or (errorHandler.buildDepError "csv"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."dlist" or (errorHandler.buildDepError "dlist"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filemanip" or (errorHandler.buildDepError "filemanip"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."megaparsec" or (errorHandler.buildDepError "megaparsec"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."natural-sort" or (errorHandler.buildDepError "natural-sort"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."regex-tdfa" or (errorHandler.buildDepError "regex-tdfa"))
            (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."terminal-progress-bar" or (errorHandler.buildDepError "terminal-progress-bar"))
            ];
          buildable = true;
          modules = [
            "Lentil/Types"
            "Lentil/Args"
            "Lentil/File"
            "Lentil/Print"
            "Lentil/Query"
            "Lentil/Export"
            "Lentil/Parse/Source"
            "Lentil/Parse/Issue"
            "Lentil/Parse/Syntaxes"
            "Lentil/Parse/Run"
            "Lentil/Helpers"
            ];
          hsSourceDirs = [ "src" ];
          mainPath = [ "Main.hs" ] ++ [ "" ];
          };
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."ansi-wl-pprint" or (errorHandler.buildDepError "ansi-wl-pprint"))
            (hsPkgs."csv" or (errorHandler.buildDepError "csv"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."dlist" or (errorHandler.buildDepError "dlist"))
            (hsPkgs."filemanip" or (errorHandler.buildDepError "filemanip"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."megaparsec" or (errorHandler.buildDepError "megaparsec"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."natural-sort" or (errorHandler.buildDepError "natural-sort"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."regex-tdfa" or (errorHandler.buildDepError "regex-tdfa"))
            (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."terminal-progress-bar" or (errorHandler.buildDepError "terminal-progress-bar"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            ];
          buildable = true;
          modules = [
            "Lentil/Types"
            "Lentil/Args"
            "Lentil/File"
            "Lentil/Print"
            "Lentil/Query"
            "Lentil/Export"
            "Lentil/Parse/Source"
            "Lentil/Parse/Issue"
            "Lentil/Parse/Syntaxes"
            "Lentil/Parse/Run"
            "Lentil/HelpersSpec"
            "Lentil/ArgsSpec"
            "Lentil/ExportSpec"
            "Lentil/FileSpec"
            "Lentil/PrintSpec"
            "Lentil/Parse/SourceSpec"
            "Lentil/Parse/IssueSpec"
            "Lentil/QuerySpec"
            "Lentil/TypeSpec"
            "Lentil/Parse/RunSpec"
            "Lentil/Helpers"
            ];
          hsSourceDirs = [ "test" "src" ];
          mainPath = [ "Tests.hs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.; }