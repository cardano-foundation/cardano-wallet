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
    flags = { systemd = true; };
    package = {
      specVersion = "1.10";
      identifier = { name = "cardano-config"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "";
      description = "";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (buildDepError "base"))
          (hsPkgs."attoparsec" or (buildDepError "attoparsec"))
          (hsPkgs."aeson" or (buildDepError "aeson"))
          (hsPkgs."async" or (buildDepError "async"))
          (hsPkgs."base16-bytestring" or (buildDepError "base16-bytestring"))
          (hsPkgs."bytestring" or (buildDepError "bytestring"))
          (hsPkgs."canonical-json" or (buildDepError "canonical-json"))
          (hsPkgs."cardano-binary" or (buildDepError "cardano-binary"))
          (hsPkgs."cardano-crypto-class" or (buildDepError "cardano-crypto-class"))
          (hsPkgs."cardano-crypto-wrapper" or (buildDepError "cardano-crypto-wrapper"))
          (hsPkgs."cardano-ledger" or (buildDepError "cardano-ledger"))
          (hsPkgs."cardano-prelude" or (buildDepError "cardano-prelude"))
          (hsPkgs."cardano-shell" or (buildDepError "cardano-shell"))
          (hsPkgs."cardano-slotting" or (buildDepError "cardano-slotting"))
          (hsPkgs."cborg" or (buildDepError "cborg"))
          (hsPkgs."containers" or (buildDepError "containers"))
          (hsPkgs."contra-tracer" or (buildDepError "contra-tracer"))
          (hsPkgs."directory" or (buildDepError "directory"))
          (hsPkgs."file-embed" or (buildDepError "file-embed"))
          (hsPkgs."filepath" or (buildDepError "filepath"))
          (hsPkgs."generic-monoid" or (buildDepError "generic-monoid"))
          (hsPkgs."iohk-monitoring" or (buildDepError "iohk-monitoring"))
          (hsPkgs."lobemo-backend-aggregation" or (buildDepError "lobemo-backend-aggregation"))
          (hsPkgs."lobemo-backend-ekg" or (buildDepError "lobemo-backend-ekg"))
          (hsPkgs."lobemo-backend-monitoring" or (buildDepError "lobemo-backend-monitoring"))
          (hsPkgs."lobemo-backend-trace-forwarder" or (buildDepError "lobemo-backend-trace-forwarder"))
          (hsPkgs."network" or (buildDepError "network"))
          (hsPkgs."network-mux" or (buildDepError "network-mux"))
          (hsPkgs."optparse-applicative" or (buildDepError "optparse-applicative"))
          (hsPkgs."ouroboros-consensus" or (buildDepError "ouroboros-consensus"))
          (hsPkgs."ouroboros-consensus-byron" or (buildDepError "ouroboros-consensus-byron"))
          (hsPkgs."ouroboros-consensus-cardano" or (buildDepError "ouroboros-consensus-cardano"))
          (hsPkgs."ouroboros-consensus-mock" or (buildDepError "ouroboros-consensus-mock"))
          (hsPkgs."ouroboros-consensus-shelley" or (buildDepError "ouroboros-consensus-shelley"))
          (hsPkgs."ouroboros-network" or (buildDepError "ouroboros-network"))
          (hsPkgs."ouroboros-network-framework" or (buildDepError "ouroboros-network-framework"))
          (hsPkgs."process" or (buildDepError "process"))
          (hsPkgs."iproute" or (buildDepError "iproute"))
          (hsPkgs."safe-exceptions" or (buildDepError "safe-exceptions"))
          (hsPkgs."scientific" or (buildDepError "scientific"))
          (hsPkgs."shelley-spec-ledger" or (buildDepError "shelley-spec-ledger"))
          (hsPkgs."string-conv" or (buildDepError "string-conv"))
          (hsPkgs."stm" or (buildDepError "stm"))
          (hsPkgs."template-haskell" or (buildDepError "template-haskell"))
          (hsPkgs."text" or (buildDepError "text"))
          (hsPkgs."time" or (buildDepError "time"))
          (hsPkgs."transformers" or (buildDepError "transformers"))
          (hsPkgs."transformers-except" or (buildDepError "transformers-except"))
          (hsPkgs."typed-protocols" or (buildDepError "typed-protocols"))
          (hsPkgs."yaml" or (buildDepError "yaml"))
          ] ++ (pkgs.lib).optional (system.isLinux && flags.systemd) (hsPkgs."lobemo-scribe-systemd" or (buildDepError "lobemo-scribe-systemd"));
        buildable = true;
        };
      tests = {
        "cardano-config-test" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."aeson" or (buildDepError "aeson"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."cardano-config" or (buildDepError "cardano-config"))
            (hsPkgs."cardano-crypto-class" or (buildDepError "cardano-crypto-class"))
            (hsPkgs."cardano-crypto-test" or (buildDepError "cardano-crypto-test"))
            (hsPkgs."cardano-crypto-wrapper" or (buildDepError "cardano-crypto-wrapper"))
            (hsPkgs."cardano-prelude" or (buildDepError "cardano-prelude"))
            (hsPkgs."cardano-prelude-test" or (buildDepError "cardano-prelude-test"))
            (hsPkgs."cardano-slotting" or (buildDepError "cardano-slotting"))
            (hsPkgs."containers" or (buildDepError "containers"))
            (hsPkgs."cryptonite" or (buildDepError "cryptonite"))
            (hsPkgs."iproute" or (buildDepError "iproute"))
            (hsPkgs."ouroboros-consensus" or (buildDepError "ouroboros-consensus"))
            (hsPkgs."ouroboros-consensus-shelley" or (buildDepError "ouroboros-consensus-shelley"))
            (hsPkgs."ouroboros-network" or (buildDepError "ouroboros-network"))
            (hsPkgs."shelley-spec-ledger" or (buildDepError "shelley-spec-ledger"))
            (hsPkgs."shelley-spec-ledger-test" or (buildDepError "shelley-spec-ledger-test"))
            (hsPkgs."time" or (buildDepError "time"))
            (hsPkgs."hedgehog" or (buildDepError "hedgehog"))
            (hsPkgs."hedgehog-corpus" or (buildDepError "hedgehog-corpus"))
            ];
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
    postUnpack = "sourceRoot+=/cardano-config; echo source root reset to \$sourceRoot";
    }