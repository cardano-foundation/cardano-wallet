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
    flags = { unexpected_thunks = false; systemd = true; };
    package = {
      specVersion = "1.10";
      identifier = { name = "cardano-node"; version = "1.12.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "";
      description = "The cardano full node";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = ([
          (hsPkgs."aeson" or (buildDepError "aeson"))
          (hsPkgs."async" or (buildDepError "async"))
          (hsPkgs."base" or (buildDepError "base"))
          (hsPkgs."bytestring" or (buildDepError "bytestring"))
          (hsPkgs."deepseq" or (buildDepError "deepseq"))
          (hsPkgs."cardano-config" or (buildDepError "cardano-config"))
          (hsPkgs."cardano-ledger" or (buildDepError "cardano-ledger"))
          (hsPkgs."cardano-prelude" or (buildDepError "cardano-prelude"))
          (hsPkgs."cardano-shell" or (buildDepError "cardano-shell"))
          (hsPkgs."cardano-slotting" or (buildDepError "cardano-slotting"))
          (hsPkgs."contra-tracer" or (buildDepError "contra-tracer"))
          (hsPkgs."cborg" or (buildDepError "cborg"))
          (hsPkgs."containers" or (buildDepError "containers"))
          (hsPkgs."directory" or (buildDepError "directory"))
          (hsPkgs."filepath" or (buildDepError "filepath"))
          (hsPkgs."hostname" or (buildDepError "hostname"))
          (hsPkgs."io-sim-classes" or (buildDepError "io-sim-classes"))
          (hsPkgs."iohk-monitoring" or (buildDepError "iohk-monitoring"))
          (hsPkgs."network" or (buildDepError "network"))
          (hsPkgs."network-mux" or (buildDepError "network-mux"))
          (hsPkgs."optparse-applicative" or (buildDepError "optparse-applicative"))
          (hsPkgs."ouroboros-consensus" or (buildDepError "ouroboros-consensus"))
          (hsPkgs."ouroboros-consensus-byron" or (buildDepError "ouroboros-consensus-byron"))
          (hsPkgs."ouroboros-consensus-shelley" or (buildDepError "ouroboros-consensus-shelley"))
          (hsPkgs."ouroboros-consensus-cardano" or (buildDepError "ouroboros-consensus-cardano"))
          (hsPkgs."ouroboros-network" or (buildDepError "ouroboros-network"))
          (hsPkgs."ouroboros-network-framework" or (buildDepError "ouroboros-network-framework"))
          (hsPkgs."process" or (buildDepError "process"))
          (hsPkgs."shelley-spec-ledger" or (buildDepError "shelley-spec-ledger"))
          (hsPkgs."strict-concurrency" or (buildDepError "strict-concurrency"))
          (hsPkgs."text" or (buildDepError "text"))
          (hsPkgs."time" or (buildDepError "time"))
          (hsPkgs."tracer-transformers" or (buildDepError "tracer-transformers"))
          (hsPkgs."transformers" or (buildDepError "transformers"))
          (hsPkgs."transformers-except" or (buildDepError "transformers-except"))
          ] ++ (pkgs.lib).optional (system.isLinux && flags.systemd) (hsPkgs."systemd" or (buildDepError "systemd"))) ++ (if system.isWindows
          then [ (hsPkgs."Win32" or (buildDepError "Win32")) ]
          else [
            (hsPkgs."unix" or (buildDepError "unix"))
            (hsPkgs."brick" or (buildDepError "brick"))
            (hsPkgs."vty" or (buildDepError "vty"))
            ]);
        buildable = true;
        };
      exes = {
        "cardano-node" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."cardano-config" or (buildDepError "cardano-config"))
            (hsPkgs."cardano-node" or (buildDepError "cardano-node"))
            (hsPkgs."cardano-prelude" or (buildDepError "cardano-prelude"))
            (hsPkgs."cardano-shell" or (buildDepError "cardano-shell"))
            (hsPkgs."optparse-applicative" or (buildDepError "optparse-applicative"))
            (hsPkgs."text" or (buildDepError "text"))
            ] ++ (if system.isWindows
            then [ (hsPkgs."Win32" or (buildDepError "Win32")) ]
            else [ (hsPkgs."unix" or (buildDepError "unix")) ]);
          buildable = true;
          };
        "chairman" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."async" or (buildDepError "async"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."containers" or (buildDepError "containers"))
            (hsPkgs."cardano-config" or (buildDepError "cardano-config"))
            (hsPkgs."cardano-prelude" or (buildDepError "cardano-prelude"))
            (hsPkgs."contra-tracer" or (buildDepError "contra-tracer"))
            (hsPkgs."cardano-prelude" or (buildDepError "cardano-prelude"))
            (hsPkgs."io-sim-classes" or (buildDepError "io-sim-classes"))
            (hsPkgs."network-mux" or (buildDepError "network-mux"))
            (hsPkgs."optparse-applicative" or (buildDepError "optparse-applicative"))
            (hsPkgs."ouroboros-consensus" or (buildDepError "ouroboros-consensus"))
            (hsPkgs."ouroboros-consensus-cardano" or (buildDepError "ouroboros-consensus-cardano"))
            (hsPkgs."ouroboros-network" or (buildDepError "ouroboros-network"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."transformers-except" or (buildDepError "transformers-except"))
            (hsPkgs."typed-protocols" or (buildDepError "typed-protocols"))
            (hsPkgs."ouroboros-network-framework" or (buildDepError "ouroboros-network-framework"))
            ] ++ (if system.isWindows
            then [ (hsPkgs."Win32" or (buildDepError "Win32")) ]
            else [ (hsPkgs."unix" or (buildDepError "unix")) ]);
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/cardano-node";
      rev = "f6ef186d1f7280585a245c8565d92117133cf359";
      sha256 = "1f8n9np96as81avl9k8pb1lqlkp7anppdr3lc7qk43k69932warn";
      });
    postUnpack = "sourceRoot+=/cardano-node; echo source root reset to \$sourceRoot";
    }