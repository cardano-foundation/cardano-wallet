{ pkgs

# haskell.nix
, haskell

# Filtered sources of this project
, src

# Test dependencies of cardano-wallet
, cardano-sl-node
, cardano-http-bridge

# Dependencies of cardano-wallet-jormungandr
, jmPkgs ? import ./jormungandr.nix { inherit pkgs; }
, jormungandr ? jmPkgs.jormungandr
, jormungandr-cli ? jmPkgs.jormungandr-cli

# Customisations for cross-compiling
, iohk-extras ? {}
, iohk-module ? {}

}:

let
  # our packages
  stack-pkgs = import ./.stack.nix/default.nix;

  # Grab the compiler name from stack-to-nix output.
  compiler = (stack-pkgs.extras {}).compiler.nix-name;

  # Chop out a subdirectory of the source, so that the package is only
  # rebuilt when something in the subdirectory changes.
  filterSubDir = dir:  with pkgs.lib; let
      isFiltered = src ? _isLibCleanSourceWith;
      origSrc = if isFiltered then src.origSrc else src;
    in cleanSourceWith {
      inherit src;
      filter = path: type:
        type == "directory" ||
        hasPrefix (toString origSrc + toString dir) path;
    } + dir;

  pkgSet = haskell.mkStackPkgSet {
    inherit stack-pkgs;
    modules = [
      # Add source filtering to local packages
      {
        packages.cardano-wallet-core.src = filterSubDir /lib/core;
        packages.cardano-wallet-core-integration.src = filterSubDir /lib/core-integration;
        packages.cardano-wallet-cli.src = filterSubDir /lib/cli;
        packages.cardano-wallet-launcher.src = filterSubDir /lib/launcher;
        packages.cardano-wallet-http-bridge.src = filterSubDir /lib/http-bridge;
        packages.cardano-wallet-jormungandr.src = filterSubDir /lib/jormungandr;
        packages.cardano-wallet-test-utils.src = filterSubDir /lib/test-utils;
        packages.text-class.src = filterSubDir /lib/text-class;
        packages.bech32.src = filterSubDir /lib/bech32;
      }

      # Add dependencies
      {
        packages.cardano-wallet-http-bridge.components.tests = {
          integration.build-tools = [ cardano-http-bridge cardano-sl-node ];
          unit.build-tools = [ cardano-http-bridge ];
        };

        packages.cardano-wallet-jormungandr.components.tests = {
          # Some tests want to write ~/.local/share/cardano-wallet
          integration.preBuild = "export HOME=`pwd`";
          # provide jormungandr command to test suites
          integration.build-tools = [ jormungandr jormungandr-cli ];
          unit.build-tools = [ jormungandr ];
        };

        packages.cardano-wallet-http-bridge.components.exes.cardano-wallet-http-bridge = {
          build-tools = [ pkgs.makeWrapper];
          postInstall = ''
            wrapProgram $out/bin/cardano-wallet-http-bridge \
              --prefix PATH : ${cardano-http-bridge}/bin
          '';
        };

        packages.cardano-wallet-http-bridge.components.benchmarks.restore = {
          build-tools = [ pkgs.makeWrapper ];
          postInstall = ''
            makeWrapper \
              $out/cardano-wallet-*/restore \
              $out/bin/restore \
              --prefix PATH : ${cardano-http-bridge}/bin
          '';
        };

        packages.cardano-wallet-core.components.tests.unit.preBuild = ''
          export SWAGGER_YAML=${src + /specifications/api/swagger.yaml}
        '';

        # Workaround for Haskell.nix issue
        packages.cardano-wallet-jormungandr.components.all.postInstall = pkgs.lib.mkForce "";
        packages.cardano-wallet-http-bridge.components.all.postInstall = pkgs.lib.mkForce "";
        packages.cardano-wallet-core.components.all.preBuild = pkgs.lib.mkForce "";
        packages.cardano-wallet-jormungandr.components.all.preBuild = pkgs.lib.mkForce "";
      }

      # Misc. build fixes for dependencies
      {
        # Cut down iohk-monitoring deps
        packages.iohk-monitoring.flags = {
          disable-ekg = true;
          disable-examples = true;
          disable-graylog = true;
          disable-gui = true;
          disable-prometheus = true;
          disable-systemd = true;
        };

        # Katip has Win32 (>=2.3 && <2.6) constraint
        packages.katip.doExactConfig = true;
      }

      # The iohk-module will supply us with the necessary
      # cross compilation plumbing to make Template Haskell
      # work when cross compiling.
      iohk-module
    ];
    pkg-def-extras = [
      # Use the iohk-extras patched GHC for cross-compiling.
      iohk-extras.${compiler}
    ];
  };

in
  pkgSet.config.hsPkgs // { _config = pkgSet.config; }
