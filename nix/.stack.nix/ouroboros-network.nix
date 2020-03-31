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
    flags = { asserts = false; ipv6 = false; cddl = true; };
    package = {
      specVersion = "1.10";
      identifier = { name = "ouroboros-network"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "2019 Input Output (Hong Kong) Ltd.";
      maintainer = "";
      author = "Alexander Vieth, Marcin Szamotulski, Duncan Coutts";
      homepage = "";
      url = "";
      synopsis = "A networking layer for the Ouroboros blockchain protocol";
      description = "";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (buildDepError "base"))
          (hsPkgs."network-mux" or (buildDepError "network-mux"))
          (hsPkgs."typed-protocols" or (buildDepError "typed-protocols"))
          (hsPkgs."ouroboros-network-framework" or (buildDepError "ouroboros-network-framework"))
          (hsPkgs."io-sim-classes" or (buildDepError "io-sim-classes"))
          (hsPkgs."contra-tracer" or (buildDepError "contra-tracer"))
          (hsPkgs."async" or (buildDepError "async"))
          (hsPkgs."binary" or (buildDepError "binary"))
          (hsPkgs."bytestring" or (buildDepError "bytestring"))
          (hsPkgs."cardano-binary" or (buildDepError "cardano-binary"))
          (hsPkgs."cardano-prelude" or (buildDepError "cardano-prelude"))
          (hsPkgs."cardano-slotting" or (buildDepError "cardano-slotting"))
          (hsPkgs."cborg" or (buildDepError "cborg"))
          (hsPkgs."containers" or (buildDepError "containers"))
          (hsPkgs."dns" or (buildDepError "dns"))
          (hsPkgs."mtl" or (buildDepError "mtl"))
          (hsPkgs."fingertree" or (buildDepError "fingertree"))
          (hsPkgs."iproute" or (buildDepError "iproute"))
          (hsPkgs."network" or (buildDepError "network"))
          (hsPkgs."psqueues" or (buildDepError "psqueues"))
          (hsPkgs."serialise" or (buildDepError "serialise"))
          (hsPkgs."stm" or (buildDepError "stm"))
          (hsPkgs."time" or (buildDepError "time"))
          (hsPkgs."hashable" or (buildDepError "hashable"))
          (hsPkgs."text" or (buildDepError "text"))
          ];
        buildable = true;
        };
      sublibs = {
        "ouroboros-protocol-tests" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."cborg" or (buildDepError "cborg"))
            (hsPkgs."containers" or (buildDepError "containers"))
            (hsPkgs."pipes" or (buildDepError "pipes"))
            (hsPkgs."QuickCheck" or (buildDepError "QuickCheck"))
            (hsPkgs."serialise" or (buildDepError "serialise"))
            (hsPkgs."tasty" or (buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (buildDepError "tasty-quickcheck"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."cardano-prelude" or (buildDepError "cardano-prelude"))
            (hsPkgs."contra-tracer" or (buildDepError "contra-tracer"))
            (hsPkgs."io-sim" or (buildDepError "io-sim"))
            (hsPkgs."io-sim-classes" or (buildDepError "io-sim-classes"))
            (hsPkgs."typed-protocols" or (buildDepError "typed-protocols"))
            (hsPkgs."ouroboros-network-framework" or (buildDepError "ouroboros-network-framework"))
            (hsPkgs."ouroboros-network" or (buildDepError "ouroboros-network"))
            ];
          buildable = true;
          };
        };
      exes = {
        "demo-chain-sync" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."async" or (buildDepError "async"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."containers" or (buildDepError "containers"))
            (hsPkgs."contra-tracer" or (buildDepError "contra-tracer"))
            (hsPkgs."directory" or (buildDepError "directory"))
            (hsPkgs."network-mux" or (buildDepError "network-mux"))
            (hsPkgs."network" or (buildDepError "network"))
            (hsPkgs."ouroboros-network-framework" or (buildDepError "ouroboros-network-framework"))
            (hsPkgs."ouroboros-network" or (buildDepError "ouroboros-network"))
            (hsPkgs."QuickCheck" or (buildDepError "QuickCheck"))
            (hsPkgs."random" or (buildDepError "random"))
            (hsPkgs."serialise" or (buildDepError "serialise"))
            (hsPkgs."splitmix" or (buildDepError "splitmix"))
            (hsPkgs."stm" or (buildDepError "stm"))
            (hsPkgs."typed-protocols" or (buildDepError "typed-protocols"))
            ];
          buildable = true;
          };
        };
      tests = {
        "test-network" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."array" or (buildDepError "array"))
            (hsPkgs."async" or (buildDepError "async"))
            (hsPkgs."binary" or (buildDepError "binary"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."cardano-binary" or (buildDepError "cardano-binary"))
            (hsPkgs."cardano-prelude" or (buildDepError "cardano-prelude"))
            (hsPkgs."cardano-slotting" or (buildDepError "cardano-slotting"))
            (hsPkgs."cborg" or (buildDepError "cborg"))
            (hsPkgs."containers" or (buildDepError "containers"))
            (hsPkgs."contra-tracer" or (buildDepError "contra-tracer"))
            (hsPkgs."directory" or (buildDepError "directory"))
            (hsPkgs."dns" or (buildDepError "dns"))
            (hsPkgs."fingertree" or (buildDepError "fingertree"))
            (hsPkgs."hashable" or (buildDepError "hashable"))
            (hsPkgs."io-sim" or (buildDepError "io-sim"))
            (hsPkgs."io-sim-classes" or (buildDepError "io-sim-classes"))
            (hsPkgs."iproute" or (buildDepError "iproute"))
            (hsPkgs."mtl" or (buildDepError "mtl"))
            (hsPkgs."network-mux" or (buildDepError "network-mux"))
            (hsPkgs."network" or (buildDepError "network"))
            (hsPkgs."psqueues" or (buildDepError "psqueues"))
            (hsPkgs."ouroboros-network-testing" or (buildDepError "ouroboros-network-testing"))
            (hsPkgs."pipes" or (buildDepError "pipes"))
            (hsPkgs."process" or (buildDepError "process"))
            (hsPkgs."QuickCheck" or (buildDepError "QuickCheck"))
            (hsPkgs."serialise" or (buildDepError "serialise"))
            (hsPkgs."splitmix" or (buildDepError "splitmix"))
            (hsPkgs."stm" or (buildDepError "stm"))
            (hsPkgs."tasty-hunit" or (buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (buildDepError "tasty-quickcheck"))
            (hsPkgs."tasty" or (buildDepError "tasty"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."time" or (buildDepError "time"))
            (hsPkgs."typed-protocols" or (buildDepError "typed-protocols"))
            (hsPkgs."ouroboros-network-framework" or (buildDepError "ouroboros-network-framework"))
            (hsPkgs."ouroboros-network" or (buildDepError "ouroboros-network"))
            (hsPkgs."ouroboros-protocol-tests" or (buildDepError "ouroboros-protocol-tests"))
            ] ++ (pkgs.lib).optionals (system.isWindows) [
            (hsPkgs."Win32-network" or (buildDepError "Win32-network"))
            (hsPkgs."Win32" or (buildDepError "Win32"))
            ];
          buildable = true;
          };
        "test-cddl" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."cardano-binary" or (buildDepError "cardano-binary"))
            (hsPkgs."cardano-prelude" or (buildDepError "cardano-prelude"))
            (hsPkgs."cardano-slotting" or (buildDepError "cardano-slotting"))
            (hsPkgs."cborg" or (buildDepError "cborg"))
            (hsPkgs."containers" or (buildDepError "containers"))
            (hsPkgs."contra-tracer" or (buildDepError "contra-tracer"))
            (hsPkgs."fingertree" or (buildDepError "fingertree"))
            (hsPkgs."hashable" or (buildDepError "hashable"))
            (hsPkgs."io-sim" or (buildDepError "io-sim"))
            (hsPkgs."io-sim-classes" or (buildDepError "io-sim-classes"))
            (hsPkgs."network-mux" or (buildDepError "network-mux"))
            (hsPkgs."pipes" or (buildDepError "pipes"))
            (hsPkgs."process-extras" or (buildDepError "process-extras"))
            (hsPkgs."QuickCheck" or (buildDepError "QuickCheck"))
            (hsPkgs."serialise" or (buildDepError "serialise"))
            (hsPkgs."tasty" or (buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (buildDepError "tasty-quickcheck"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."typed-protocols" or (buildDepError "typed-protocols"))
            (hsPkgs."ouroboros-network-framework" or (buildDepError "ouroboros-network-framework"))
            (hsPkgs."ouroboros-network" or (buildDepError "ouroboros-network"))
            (hsPkgs."ouroboros-protocol-tests" or (buildDepError "ouroboros-protocol-tests"))
            ];
          buildable = if flags.cddl then true else false;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/ouroboros-network";
      rev = "7e89518148ebb11d9ee6b973c394a69713961de6";
      sha256 = "06mhpn3kmlk9siiki2rn3cmq4v7lz6rvxzmll1zdf5lhrh3ixks2";
      });
    postUnpack = "sourceRoot+=/ouroboros-network; echo source root reset to \$sourceRoot";
    }