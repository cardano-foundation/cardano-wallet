let
  buildDepError = pkg:
    builtins.throw ''
      The Haskell package set does not contain the package: ${pkg} (build dependency).
      
      If you are using Stackage, make sure that you are using a snapshot that contains the package. Otherwise you may need to update the Hackage snapshot you are using, usually by updating haskell.nix.
      '';
  sysDepError = pkg:
    builtins.throw ''
      The Nixpkgs package set does not contain the package: ${pkg} (system dependency).
      
      You may need to augment the system package mapping in haskell.nix so that it can be found.
      '';
  pkgConfDepError = pkg:
    builtins.throw ''
      The pkg-conf packages does not contain the package: ${pkg} (pkg-conf dependency).
      
      You may need to augment the pkg-conf package mapping in haskell.nix so that it can be found.
      '';
  exeDepError = pkg:
    builtins.throw ''
      The local executable components do not include the component: ${pkg} (executable dependency).
      '';
  legacyExeDepError = pkg:
    builtins.throw ''
      The Haskell package set does not contain the package: ${pkg} (executable dependency).
      
      If you are using Stackage, make sure that you are using a snapshot that contains the package. Otherwise you may need to update the Hackage snapshot you are using, usually by updating haskell.nix.
      '';
  buildToolDepError = pkg:
    builtins.throw ''
      Neither the Haskell package set or the Nixpkgs package set contain the package: ${pkg} (build tool dependency).
      
      If this is a system dependency:
      You may need to augment the system package mapping in haskell.nix so that it can be found.
      
      If this is a Haskell dependency:
      If you are using Stackage, make sure that you are using a snapshot that contains the package. Otherwise you may need to update the Hackage snapshot you are using, usually by updating haskell.nix.
      '';
in { system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "ouroboros-consensus"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "2019 Input Output (Hong Kong) Ltd.";
      maintainer = "operations@iohk.io";
      author = "IOHK Engineering Team";
      homepage = "";
      url = "";
      synopsis = "Consensus layer for the Ouroboros blockchain protocol";
      description = "";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (buildDepError "base"))
          (hsPkgs."ouroboros-network" or (buildDepError "ouroboros-network"))
          (hsPkgs."network-mux" or (buildDepError "network-mux"))
          (hsPkgs."typed-protocols" or (buildDepError "typed-protocols"))
          (hsPkgs."typed-protocols-cbor" or (buildDepError "typed-protocols-cbor"))
          (hsPkgs."io-sim-classes" or (buildDepError "io-sim-classes"))
          (hsPkgs."contra-tracer" or (buildDepError "contra-tracer"))
          (hsPkgs."bifunctors" or (buildDepError "bifunctors"))
          (hsPkgs."bimap" or (buildDepError "bimap"))
          (hsPkgs."binary" or (buildDepError "binary"))
          (hsPkgs."bytestring" or (buildDepError "bytestring"))
          (hsPkgs."cardano-binary" or (buildDepError "cardano-binary"))
          (hsPkgs."cardano-crypto-class" or (buildDepError "cardano-crypto-class"))
          (hsPkgs."cardano-crypto-wrapper" or (buildDepError "cardano-crypto-wrapper"))
          (hsPkgs."cardano-ledger" or (buildDepError "cardano-ledger"))
          (hsPkgs."cardano-prelude" or (buildDepError "cardano-prelude"))
          (hsPkgs."cborg" or (buildDepError "cborg"))
          (hsPkgs."containers" or (buildDepError "containers"))
          (hsPkgs."cryptonite" or (buildDepError "cryptonite"))
          (hsPkgs."deepseq" or (buildDepError "deepseq"))
          (hsPkgs."digest" or (buildDepError "digest"))
          (hsPkgs."directory" or (buildDepError "directory"))
          (hsPkgs."filepath" or (buildDepError "filepath"))
          (hsPkgs."fingertree" or (buildDepError "fingertree"))
          (hsPkgs."formatting" or (buildDepError "formatting"))
          (hsPkgs."hashable" or (buildDepError "hashable"))
          (hsPkgs."memory" or (buildDepError "memory"))
          (hsPkgs."mmorph" or (buildDepError "mmorph"))
          (hsPkgs."mtl" or (buildDepError "mtl"))
          (hsPkgs."network" or (buildDepError "network"))
          (hsPkgs."serialise" or (buildDepError "serialise"))
          (hsPkgs."stm" or (buildDepError "stm"))
          (hsPkgs."text" or (buildDepError "text"))
          (hsPkgs."time" or (buildDepError "time"))
          (hsPkgs."transformers" or (buildDepError "transformers"))
          (hsPkgs."vector" or (buildDepError "vector"))
          ] ++ (if system.isWindows
          then [ (hsPkgs."Win32" or (buildDepError "Win32")) ]
          else [
            (hsPkgs."unix" or (buildDepError "unix"))
            (hsPkgs."unix-bytestring" or (buildDepError "unix-bytestring"))
            ]);
        buildable = true;
        };
      exes = {
        "byron-db-converter" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."cardano-binary" or (buildDepError "cardano-binary"))
            (hsPkgs."cardano-crypto-wrapper" or (buildDepError "cardano-crypto-wrapper"))
            (hsPkgs."cardano-ledger" or (buildDepError "cardano-ledger"))
            (hsPkgs."containers" or (buildDepError "containers"))
            (hsPkgs."contra-tracer" or (buildDepError "contra-tracer"))
            (hsPkgs."directory" or (buildDepError "directory"))
            (hsPkgs."mtl" or (buildDepError "mtl"))
            (hsPkgs."optparse-applicative" or (buildDepError "optparse-applicative"))
            (hsPkgs."optparse-generic" or (buildDepError "optparse-generic"))
            (hsPkgs."ouroboros-consensus" or (buildDepError "ouroboros-consensus"))
            (hsPkgs."path" or (buildDepError "path"))
            (hsPkgs."path-io" or (buildDepError "path-io"))
            (hsPkgs."resourcet" or (buildDepError "resourcet"))
            (hsPkgs."streaming" or (buildDepError "streaming"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."time" or (buildDepError "time"))
            ];
          buildable = true;
          };
        "analyse-db" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."cardano-binary" or (buildDepError "cardano-binary"))
            (hsPkgs."cardano-crypto-wrapper" or (buildDepError "cardano-crypto-wrapper"))
            (hsPkgs."cardano-ledger" or (buildDepError "cardano-ledger"))
            (hsPkgs."mtl" or (buildDepError "mtl"))
            (hsPkgs."optparse-applicative" or (buildDepError "optparse-applicative"))
            (hsPkgs."ouroboros-consensus" or (buildDepError "ouroboros-consensus"))
            (hsPkgs."ouroboros-network" or (buildDepError "ouroboros-network"))
            ];
          buildable = true;
          };
        };
      tests = {
        "test-consensus" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."base16-bytestring" or (buildDepError "base16-bytestring"))
            (hsPkgs."binary" or (buildDepError "binary"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."cardano-binary" or (buildDepError "cardano-binary"))
            (hsPkgs."cardano-crypto-class" or (buildDepError "cardano-crypto-class"))
            (hsPkgs."cardano-crypto-wrapper" or (buildDepError "cardano-crypto-wrapper"))
            (hsPkgs."cardano-crypto-test" or (buildDepError "cardano-crypto-test"))
            (hsPkgs."cardano-ledger" or (buildDepError "cardano-ledger"))
            (hsPkgs."cardano-ledger-test" or (buildDepError "cardano-ledger-test"))
            (hsPkgs."cardano-prelude" or (buildDepError "cardano-prelude"))
            (hsPkgs."typed-protocols" or (buildDepError "typed-protocols"))
            (hsPkgs."network-mux" or (buildDepError "network-mux"))
            (hsPkgs."ouroboros-network" or (buildDepError "ouroboros-network"))
            (hsPkgs."ouroboros-consensus" or (buildDepError "ouroboros-consensus"))
            (hsPkgs."io-sim-classes" or (buildDepError "io-sim-classes"))
            (hsPkgs."io-sim" or (buildDepError "io-sim"))
            (hsPkgs."binary-search" or (buildDepError "binary-search"))
            (hsPkgs."cborg" or (buildDepError "cborg"))
            (hsPkgs."containers" or (buildDepError "containers"))
            (hsPkgs."contra-tracer" or (buildDepError "contra-tracer"))
            (hsPkgs."cryptonite" or (buildDepError "cryptonite"))
            (hsPkgs."deepseq" or (buildDepError "deepseq"))
            (hsPkgs."fgl" or (buildDepError "fgl"))
            (hsPkgs."fingertree" or (buildDepError "fingertree"))
            (hsPkgs."generics-sop" or (buildDepError "generics-sop"))
            (hsPkgs."graphviz" or (buildDepError "graphviz"))
            (hsPkgs."hedgehog-quickcheck" or (buildDepError "hedgehog-quickcheck"))
            (hsPkgs."mtl" or (buildDepError "mtl"))
            (hsPkgs."QuickCheck" or (buildDepError "QuickCheck"))
            (hsPkgs."quickcheck-state-machine" or (buildDepError "quickcheck-state-machine"))
            (hsPkgs."random" or (buildDepError "random"))
            (hsPkgs."serialise" or (buildDepError "serialise"))
            (hsPkgs."tasty" or (buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (buildDepError "tasty-quickcheck"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."time" or (buildDepError "time"))
            (hsPkgs."tree-diff" or (buildDepError "tree-diff"))
            ];
          buildable = true;
          };
        "test-storage" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."cardano-crypto-class" or (buildDepError "cardano-crypto-class"))
            (hsPkgs."cardano-ledger" or (buildDepError "cardano-ledger"))
            (hsPkgs."cardano-ledger-test" or (buildDepError "cardano-ledger-test"))
            (hsPkgs."cardano-prelude" or (buildDepError "cardano-prelude"))
            (hsPkgs."ouroboros-network" or (buildDepError "ouroboros-network"))
            (hsPkgs."ouroboros-network-testing" or (buildDepError "ouroboros-network-testing"))
            (hsPkgs."ouroboros-consensus" or (buildDepError "ouroboros-consensus"))
            (hsPkgs."io-sim-classes" or (buildDepError "io-sim-classes"))
            (hsPkgs."io-sim" or (buildDepError "io-sim"))
            (hsPkgs."base16-bytestring" or (buildDepError "base16-bytestring"))
            (hsPkgs."bifunctors" or (buildDepError "bifunctors"))
            (hsPkgs."binary" or (buildDepError "binary"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."cborg" or (buildDepError "cborg"))
            (hsPkgs."cereal" or (buildDepError "cereal"))
            (hsPkgs."containers" or (buildDepError "containers"))
            (hsPkgs."contra-tracer" or (buildDepError "contra-tracer"))
            (hsPkgs."deepseq" or (buildDepError "deepseq"))
            (hsPkgs."directory" or (buildDepError "directory"))
            (hsPkgs."fingertree" or (buildDepError "fingertree"))
            (hsPkgs."generics-sop" or (buildDepError "generics-sop"))
            (hsPkgs."hashable" or (buildDepError "hashable"))
            (hsPkgs."mtl" or (buildDepError "mtl"))
            (hsPkgs."pretty-show" or (buildDepError "pretty-show"))
            (hsPkgs."QuickCheck" or (buildDepError "QuickCheck"))
            (hsPkgs."quickcheck-state-machine" or (buildDepError "quickcheck-state-machine"))
            (hsPkgs."random" or (buildDepError "random"))
            (hsPkgs."serialise" or (buildDepError "serialise"))
            (hsPkgs."tasty" or (buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (buildDepError "tasty-quickcheck"))
            (hsPkgs."temporary" or (buildDepError "temporary"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."time" or (buildDepError "time"))
            (hsPkgs."transformers" or (buildDepError "transformers"))
            (hsPkgs."tree-diff" or (buildDepError "tree-diff"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/ouroboros-network";
      rev = "c785fe64445357b806c847fa438fc7612563b42b";
      sha256 = "015ac7fj10xg5wcgv265qdgi85gdgj14cl03lrnqsmyqxsy1pjpn";
      });
    postUnpack = "sourceRoot+=/ouroboros-consensus; echo source root reset to \$sourceRoot";
    }