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
    flags = { unexpected_thunks = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "cardano-cli"; version = "1.12.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "";
      description = "The Cardano command-line interface.";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (buildDepError "base"))
          (hsPkgs."aeson" or (buildDepError "aeson"))
          (hsPkgs."aeson-pretty" or (buildDepError "aeson-pretty"))
          (hsPkgs."bytestring" or (buildDepError "bytestring"))
          (hsPkgs."base16-bytestring" or (buildDepError "base16-bytestring"))
          (hsPkgs."base58-bytestring" or (buildDepError "base58-bytestring"))
          (hsPkgs."canonical-json" or (buildDepError "canonical-json"))
          (hsPkgs."cardano-api" or (buildDepError "cardano-api"))
          (hsPkgs."cardano-binary" or (buildDepError "cardano-binary"))
          (hsPkgs."cardano-config" or (buildDepError "cardano-config"))
          (hsPkgs."cardano-crypto" or (buildDepError "cardano-crypto"))
          (hsPkgs."cardano-crypto-class" or (buildDepError "cardano-crypto-class"))
          (hsPkgs."cardano-crypto-wrapper" or (buildDepError "cardano-crypto-wrapper"))
          (hsPkgs."cardano-ledger" or (buildDepError "cardano-ledger"))
          (hsPkgs."cardano-prelude" or (buildDepError "cardano-prelude"))
          (hsPkgs."cardano-slotting" or (buildDepError "cardano-slotting"))
          (hsPkgs."contra-tracer" or (buildDepError "contra-tracer"))
          (hsPkgs."cborg" or (buildDepError "cborg"))
          (hsPkgs."containers" or (buildDepError "containers"))
          (hsPkgs."directory" or (buildDepError "directory"))
          (hsPkgs."filepath" or (buildDepError "filepath"))
          (hsPkgs."formatting" or (buildDepError "formatting"))
          (hsPkgs."io-sim-classes" or (buildDepError "io-sim-classes"))
          (hsPkgs."iproute" or (buildDepError "iproute"))
          (hsPkgs."lens" or (buildDepError "lens"))
          (hsPkgs."memory" or (buildDepError "memory"))
          (hsPkgs."network-mux" or (buildDepError "network-mux"))
          (hsPkgs."optparse-applicative" or (buildDepError "optparse-applicative"))
          (hsPkgs."ouroboros-consensus" or (buildDepError "ouroboros-consensus"))
          (hsPkgs."ouroboros-consensus-byron" or (buildDepError "ouroboros-consensus-byron"))
          (hsPkgs."ouroboros-consensus-cardano" or (buildDepError "ouroboros-consensus-cardano"))
          (hsPkgs."ouroboros-consensus-shelley" or (buildDepError "ouroboros-consensus-shelley"))
          (hsPkgs."ouroboros-network" or (buildDepError "ouroboros-network"))
          (hsPkgs."shelley-spec-ledger" or (buildDepError "shelley-spec-ledger"))
          (hsPkgs."text" or (buildDepError "text"))
          (hsPkgs."time" or (buildDepError "time"))
          (hsPkgs."transformers" or (buildDepError "transformers"))
          (hsPkgs."transformers-except" or (buildDepError "transformers-except"))
          (hsPkgs."ouroboros-network-framework" or (buildDepError "ouroboros-network-framework"))
          (hsPkgs."utf8-string" or (buildDepError "utf8-string"))
          (hsPkgs."vector" or (buildDepError "vector"))
          ] ++ (if system.isWindows
          then [ (hsPkgs."Win32" or (buildDepError "Win32")) ]
          else [ (hsPkgs."unix" or (buildDepError "unix")) ]);
        buildable = true;
        };
      exes = {
        "cardano-cli" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."cardano-cli" or (buildDepError "cardano-cli"))
            (hsPkgs."cardano-config" or (buildDepError "cardano-config"))
            (hsPkgs."cardano-prelude" or (buildDepError "cardano-prelude"))
            (hsPkgs."optparse-applicative" or (buildDepError "optparse-applicative"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."transformers-except" or (buildDepError "transformers-except"))
            ];
          buildable = true;
          };
        };
      tests = {
        "cardano-cli-test" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."cardano-cli" or (buildDepError "cardano-cli"))
            (hsPkgs."cardano-prelude" or (buildDepError "cardano-prelude"))
            (hsPkgs."directory" or (buildDepError "directory"))
            (hsPkgs."filepath" or (buildDepError "filepath"))
            (hsPkgs."process" or (buildDepError "process"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."text-ansi" or (buildDepError "text-ansi"))
            (hsPkgs."unix" or (buildDepError "unix"))
            ];
          buildable = if system.isWindows then false else true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/cardano-node";
      rev = "f6ef186d1f7280585a245c8565d92117133cf359";
      sha256 = "1f8n9np96as81avl9k8pb1lqlkp7anppdr3lc7qk43k69932warn";
      });
    postUnpack = "sourceRoot+=/cardano-cli; echo source root reset to \$sourceRoot";
    }