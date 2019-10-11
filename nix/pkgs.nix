{ pkgs

# Filtered sources of this project
, src

# Dependencies of cardano-wallet-jormungandr
, jmPkgs
}:

let
  haskell = pkgs.haskell-nix;

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

  cabalPatch = pkgs.fetchpatch {
    url = "https://patch-diff.githubusercontent.com/raw/haskell/cabal/pull/6055.diff";
    sha256 = "145g7s3z9q8d18pxgyngvixgsm6gmwh1rgkzkhacy4krqiq0qyvx";
    stripLen = 1;
  };

  pkgSet = haskell.mkStackPkgSet {
    inherit stack-pkgs;
    modules = [
      # Add source filtering to local packages
      {
        packages.cardano-wallet-core.src = filterSubDir /lib/core;
        packages.cardano-wallet-core-integration.src = filterSubDir /lib/core-integration;
        packages.cardano-wallet-cli.src = filterSubDir /lib/cli;
        packages.cardano-wallet-launcher.src = filterSubDir /lib/launcher;
        packages.cardano-wallet-jormungandr.src = filterSubDir /lib/jormungandr;
        packages.cardano-wallet-test-utils.src = filterSubDir /lib/test-utils;
        packages.text-class.src = filterSubDir /lib/text-class;
        packages.bech32.src = filterSubDir /lib/bech32;
      }

      # Add dependencies
      {
        packages.cardano-wallet-jormungandr.components.tests = {
          # Some tests want to write ~/.local/share/cardano-wallet
          integration.preBuild = "export HOME=`pwd`";
          # provide jormungandr command to test suites
          integration.build-tools = [
            jmPkgs.jormungandr
            jmPkgs.jormungandr-cli
          ];
          unit.build-tools = [ jmPkgs.jormungandr ];
        };

        packages.cardano-wallet-core.components.tests.unit.preBuild = ''
          export SWAGGER_YAML=${src + /specifications/api/swagger.yaml}
        '';

        # Workaround for Haskell.nix issue
        packages.cardano-wallet-jormungandr.components.all.postInstall = pkgs.lib.mkForce "";
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

      # Musl libc fully static build
      (with pkgs.stdenv; let
        gplWallet = true; # fixme
        staticLibs = [ zlib openssl libffi ] ++ lib.optional gplWallet gmp6;
        gmp6 = pkgs.gmp6.override { withStatic = true; };
        zlib = pkgs.zlib.static;
        openssl = (pkgs.openssl.override { static = true; }).out;
        libffi = pkgs.libffi.overrideAttrs (oldAttrs: {
          dontDisableStatic = true;
          configureFlags = (oldAttrs.configureFlags or []) ++ [
                    "--enable-static"
                    "--disable-shared"
          ];
        });
      in {
        # Use a non-GMP compiler, for software licensing reasons.
        ghc.package = lib.mkIf (hostPlatform.isMusl && !gplWallet)
            pkgs.buildPackages.haskell.compiler.integer-simple.${compiler};

        # Add GHC flags and libraries for fully static build
        packages.cardano-wallet-jormungandr.components.exes.cardano-wallet-jormungandr = {
          configureFlags =
             lib.optionals hostPlatform.isMusl ([
               "--disable-executable-dynamic"
               "--disable-shared"
               "--ghc-option=-optl=-pthread"
               "--ghc-option=-optl=-static"
             ] ++ map (drv: "--ghc-option=-optl=-L${drv}/lib") staticLibs);
        };
      })
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

in
  pkgSet.config.hsPkgs // { _config = pkgSet.config; }
