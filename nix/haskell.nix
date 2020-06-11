############################################################################
# Builds Haskell packages with Haskell.nix
############################################################################
{ lib
, stdenv
, pkgs
, haskell-nix
, buildPackages
, config ? {}
# Enable profiling
, profiling ? config.haskellNix.profiling or false
# Project top-level source tree
, src
# GitHub PR number (when building on Hydra)
, pr ? null
}:

let
  haskell = pkgs.haskell-nix;
  jmPkgs = pkgs.jmPkgs;
  # commonLib = (import ./default.nix {}).commonLib;  # option a - shorter
  inherit (import ./default.nix {}) commonLib; # option b - even shorter

  # our packages
  stack-pkgs = import ./.stack.nix/default.nix;

  # Chop out a subdirectory of the source, so that the package is only
  # rebuilt when something in the subdirectory changes.
  filterSubDir = subDir:
    haskell.haskellLib.cleanSourceWith { inherit src subDir; };

  pkgSet = haskell.mkStackPkgSet {
    inherit stack-pkgs;
    modules = [
      # Add source filtering to local packages
      {
        packages.cardano-wallet-core.src = filterSubDir "lib/core";
        packages.cardano-wallet-core.components.tests.unit.keepSource = true;
        packages.cardano-wallet-core-integration.src = filterSubDir "lib/core-integration";
        packages.cardano-wallet-cli.src = filterSubDir "lib/cli";
        packages.cardano-wallet-launcher.src = filterSubDir "lib/launcher";
        packages.cardano-wallet-byron.src = filterSubDir "lib/byron";
        packages.cardano-wallet-byron.components.tests.integration.keepSource = true;
        packages.cardano-wallet-shelley.src = filterSubDir "lib/shelley";
        packages.cardano-wallet-shelley.components.tests.integration.keepSource = true;
        packages.cardano-wallet-jormungandr.src = filterSubDir "lib/jormungandr";
        packages.cardano-wallet-jormungandr.components.tests.unit.keepSource = true;
        packages.cardano-wallet-jormungandr.components.tests.jormungandr-integration.keepSource = true;
        packages.cardano-wallet-test-utils.src = filterSubDir "lib/test-utils";
        packages.text-class.src = filterSubDir "lib/text-class";
        packages.text-class.components.tests.unit.keepSource = true;
      }

      # Enable release flag (optimization and -Werror) on all local packages
      {
        packages.cardano-wallet-byron.flags.release = true;
        packages.cardano-wallet-shelley.flags.release = true;
        packages.cardano-wallet-cli.flags.release = true;
        packages.cardano-wallet-core-integration.flags.release = true;
        packages.cardano-wallet-core.flags.release = true;
        packages.cardano-wallet-jormungandr.flags.release = true;
        packages.cardano-wallet-launcher.flags.release = true;
        packages.cardano-wallet-test-utils.flags.release = true;
        packages.text-class.flags.release = true;
      }

      # Provide configuration and dependencies to cardano-wallet components
      {
        packages.cardano-wallet-byron.components.tests = {
          # Only run integration tests on non-PR jobsets. Note that
          # the master branch jobset will just re-use the cached Bors
          # staging build and test results.
          integration.doCheck = !isHydraPRJobset;

          # Running Windows integration tests under Wine is disabled
          # because ouroboros-network doesn't fully work under Wine.
          integration.testWrapper = lib.mkIf pkgs.stdenv.hostPlatform.isWindows ["echo"];

          # cardano-node socket path becomes too long otherwise
          unit.preCheck = lib.optionalString stdenv.isDarwin "export TMPDIR=/tmp";
          integration.preCheck = lib.optionalString stdenv.isDarwin "export TMPDIR=/tmp";

          # provide cardano-node command to test suites
          unit.build-tools = [ pkgs.cardano-node ];
          integration.build-tools = [ pkgs.cardano-node ];
        };
        packages.cardano-wallet-shelley.components.tests = {
          # Only run integration tests on non-PR jobsets. Note that
          # the master branch jobset will just re-use the cached Bors
          # staging build and test results.
          integration.doCheck = !isHydraPRJobset || stdenv.isDarwin; # fixme: rvl temp

          # Running Windows integration tests under Wine is disabled
          # because ouroboros-network doesn't fully work under Wine.
          integration.testWrapper = lib.mkIf pkgs.stdenv.hostPlatform.isWindows ["echo"];

          # cardano-node socket path becomes too long otherwise
          unit.preCheck = lib.optionalString stdenv.isDarwin "export TMPDIR=/tmp";
          integration.preCheck = lib.optionalString stdenv.isDarwin "export TMPDIR=/tmp";

          # provide cardano-node & cardano-cli to integration tests
          integration.build-tools = [ pkgs.cardano-node pkgs.cardano-cli ];
        };
        packages.cardano-wallet-jormungandr.components.tests = {
          # Next releases are going to be about cardano-node and we
          # aren't touching jormungandr a lot more these days.
          jormungandr-integration.doCheck = false;
          # Some tests want to write ~/.local/share/cardano-wallet
          jormungandr-integration.preCheck = "export HOME=`pwd`";
          # provide jormungandr command to test suites
          jormungandr-integration.build-tools = [
            jmPkgs.jormungandr
            jmPkgs.jormungandr-cli
          ];
          unit.build-tools = [ jmPkgs.jormungandr ];
        };
        packages.cardano-wallet-jormungandr.components.benchmarks.latency =
          pkgs.lib.optionalAttrs (!pkgs.stdenv.hostPlatform.isWindows) {
            build-tools = [ pkgs.makeWrapper ];
            postInstall = ''
              wrapProgram $out/bin/latency \
                --run "cd $src" \
                --prefix PATH : ${jmPkgs.jormungandr}/bin
            '';
          };

        # Add cardano-node to the PATH of the byron latency benchmark
        packages.cardano-wallet-byron.components.benchmarks.latency =
          pkgs.lib.optionalAttrs (!pkgs.stdenv.hostPlatform.isWindows) {
            build-tools = [ pkgs.makeWrapper ];
            postInstall = ''
              wrapProgram $out/bin/latency \
                --run "cd $src" \
                --prefix PATH : ${pkgs.cardano-node}/bin
            '';
          };

        # cardano-node will want to write logs to a subdirectory of the working directory.
        # We don't `cd $src` because of that.
        packages.cardano-wallet-byron.components.benchmarks.restore =
          pkgs.lib.optionalAttrs (!pkgs.stdenv.hostPlatform.isWindows) {
            build-tools = [ pkgs.makeWrapper ];
            postInstall = ''
              wrapProgram $out/bin/restore \
                --set CARDANO_NODE_CONFIGS ${pkgs.cardano-node.deployments} \
                --prefix PATH : ${pkgs.cardano-node}/bin
            '';
          };

        # Provide the swagger file in an environment variable because
        # it is located outside of the Cabal package source tree.
        packages.cardano-wallet-core.components.tests.unit.preBuild = ''
          export SWAGGER_YAML=${src + /specifications/api/swagger.yaml}
        '';

        # Workaround for Haskell.nix issue
        packages.cardano-wallet-jormungandr.components.all.postInstall = pkgs.lib.mkForce "";
        packages.cardano-wallet-core.components.all.preBuild = pkgs.lib.mkForce "";
        packages.cardano-wallet-byron.components.all.postInstall = pkgs.lib.mkForce "";
      }

      # Build fixes for library dependencies
      {
        # Make the /usr/bin/security tool available because it's
        # needed at runtime by the x509-system Haskell package.
        packages.x509-system.components.library.preBuild = pkgs.lib.optionalString (pkgs.stdenv.isDarwin) ''
          substituteInPlace System/X509/MacOS.hs --replace security /usr/bin/security
        '';

        # Packages we wish to ignore version bounds of.
        # This is similar to jailbreakCabal, however it
        # does not require any messing with cabal files.
        packages.katip.doExactConfig = true;

        # split data output for ekg to reduce closure size
        packages.ekg.components.library.enableSeparateDataOutput = true;
        enableLibraryProfiling = profiling;
      }

      # Musl libc fully static build
      (lib.optionalAttrs stdenv.hostPlatform.isMusl (let
        staticLibs = [ zlib openssl libffi gmp6 ];
        gmp6 = buildPackages.gmp6.override { withStatic = true; };
        zlib = buildPackages.zlib.static;
        openssl = (buildPackages.openssl.override { static = true; }).out;
        libffi = buildPackages.libffi.overrideAttrs (oldAttrs: {
          dontDisableStatic = true;
          configureFlags = (oldAttrs.configureFlags or []) ++ [
                    "--enable-static"
                    "--disable-shared"
          ];
        });

        # Module options which adds GHC flags and libraries for a fully static build
        fullyStaticOptions = {
          enableShared = false;
          enableStatic = true;
          configureFlags = map (drv: "--ghc-option=-optl=-L${drv}/lib") staticLibs;
        };
      in {
        # Apply fully static options to our Haskell executables
        packages.cardano-wallet-byron.components.benchmarks.latency = fullyStaticOptions;
        packages.cardano-wallet-byron.components.benchmarks.restore = fullyStaticOptions;
        packages.cardano-wallet-byron.components.exes.cardano-wallet-byron = fullyStaticOptions;
        packages.cardano-wallet-byron.components.tests.integration = fullyStaticOptions;
        packages.cardano-wallet-byron.components.tests.unit = fullyStaticOptions;
        packages.cardano-wallet-shelley.components.exes.cardano-wallet-shelley = fullyStaticOptions;
        packages.cardano-wallet-shelley.components.tests.integration = fullyStaticOptions;
        packages.cardano-wallet-shelley.components.tests.unit = fullyStaticOptions;
        packages.cardano-wallet-cli.components.tests.unit = fullyStaticOptions;
        packages.cardano-wallet-core.components.benchmarks.db = fullyStaticOptions;
        packages.cardano-wallet-core.components.tests.unit = fullyStaticOptions;
        packages.cardano-wallet-jormungandr.components.benchmarks.latency = fullyStaticOptions;
        packages.cardano-wallet-jormungandr.components.exes.cardano-wallet-jormungandr = fullyStaticOptions;
        packages.cardano-wallet-jormungandr.components.tests.jormungandr-integration = fullyStaticOptions;
        packages.cardano-wallet-jormungandr.components.tests.unit = fullyStaticOptions;
        packages.cardano-wallet-launcher.components.tests.unit = fullyStaticOptions;

        # systemd can't be statically linked - disable lobemo-scribe-journal
        packages.cardano-config.flags.systemd = false;

        # Haddock not working for cross builds and is not needed anyway
        doHaddock = false;
      }))

      # Allow installation of a newer version of Win32 than what is
      # included with GHC. The packages in this list are all those
      # installed with GHC, except for Win32.
      { nonReinstallablePkgs =
        [ "rts" "ghc-heap" "ghc-prim" "integer-gmp" "integer-simple" "base"
          "deepseq" "array" "ghc-boot-th" "pretty" "template-haskell"
          # ghcjs custom packages
          "ghcjs-prim" "ghcjs-th"
          "ghc-boot"
          "ghc" "array" "binary" "bytestring" "containers"
          "filepath" "ghc-boot" "ghc-compact" "ghc-prim"
          # "ghci" "haskeline"
          "hpc"
          "mtl" "parsec" "text" "transformers"
          "xhtml"
          # "stm" "terminfo"
        ];
      }
    ];
    pkg-def-extras = [
      # Workaround for https://github.com/input-output-hk/haskell.nix/issues/214
      (hackage: {
        packages = {
          "hsc2hs" = (((hackage.hsc2hs)."0.68.4").revisions).default;
        };
      })
    ];
  };

  # Hydra will pass the GitHub PR number as a string argument to release.nix.
  isHydraPRJobset = toString pr != "";

in
  pkgSet.config.hsPkgs // { _config = pkgSet.config; }
