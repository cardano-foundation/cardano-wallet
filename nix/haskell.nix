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
# Git revision of sources
, gitrev ? null
}:

let
  haskell = pkgs.haskell-nix;
  jmPkgs = pkgs.jmPkgs;

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
        packages.cardano-wallet.src = filterSubDir "lib/shelley";
        packages.cardano-wallet.components.tests.integration.keepSource = true;
        packages.cardano-wallet-jormungandr.src = filterSubDir "lib/jormungandr";
        packages.cardano-wallet-jormungandr.components.tests.unit.keepSource = true;
        packages.cardano-wallet-jormungandr.components.tests.jormungandr-integration.keepSource = true;
        packages.cardano-wallet-test-utils.src = filterSubDir "lib/test-utils";
        packages.text-class.src = filterSubDir "lib/text-class";
        packages.text-class.components.tests.unit.keepSource = true;
      }

      # Enable release flag (optimization and -Werror) on all local packages
      {
        packages.cardano-wallet.flags.release = true;
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
        packages.cardano-wallet.components.tests = {
          # Only run integration tests on non-PR jobsets. Note that
          # the master branch jobset will just re-use the cached Bors
          # staging build and test results.
          integration.doCheck = !isHydraPRJobset;

          # Running Windows integration tests under Wine is disabled
          # because ouroboros-network doesn't fully work under Wine.
          integration.testWrapper = lib.mkIf pkgs.stdenv.hostPlatform.isWindows ["echo"];

          # cardano-node socket path becomes too long otherwise
          unit.preCheck = lib.optionalString stdenv.isDarwin "export TMPDIR=/tmp";
          integration.preCheck = ''
            # Variables picked up by integration tests
            export CARDANO_WALLET_TRACING_MIN_SEVERITY=info
            export CARDANO_NODE_TRACING_MIN_SEVERITY=notice

            # Integration tests will place logs here
            export TESTS_LOGDIR=$(mktemp -d)/logs

            # Causes integration tests to be re-run whenever the git revision
            # changes, even if everything else is identical.
            # Since these tests tend to fail a lot, we don't want
            # to cache false failures.
            echo "Git revision is ${toString gitrev}"
          '' + lib.optionalString stdenv.isDarwin ''
            export TMPDIR=/tmp
          '';

          integration.postCheck = ''
            # fixme: There needs to be some Haskell.nix changes to
            # permit getting build products from failed builds.
            if [ -n "$TESTS_LOGDIR" && -f $out/nix-support/failed ]; then
              logfile=$out/cardano-wallet-integration-logs.tar.gz
              ${buildPackages.gnutar}/bin/tar -C $(dirname $TESTS_LOGDIR) -czvf $logfile $TESTS_LOGDIR
              echo "file none $logfile" >> $out/nix-support/hydra-build-products
            fi
          '';

          # provide cardano-node & cardano-cli to tests
          unit.build-tools = [ pkgs.cardano-node pkgs.cardano-cli ];
          integration.build-tools = [ pkgs.cardano-node pkgs.cardano-cli ];
          unit.postInstall = libSodiumPostInstall;
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

        # Add node backend to the PATH of the latency benchmarks
        packages.cardano-wallet-jormungandr.components.benchmarks.latency = wrapBench [ jmPkgs.jormungandr ];
        packages.cardano-wallet.components.benchmarks.latency = wrapBench [ pkgs.cardano-node pkgs.cardano-cli ];

        # Add cardano-node to the PATH of the byroon restore benchmark.
        # cardano-node will want to write logs to a subdirectory of the working directory.
        # We don't `cd $src` because of that.
        packages.cardano-wallet.components.benchmarks.restore =
          lib.optionalAttrs (!stdenv.hostPlatform.isWindows) {
            build-tools = [ pkgs.makeWrapper ];
            postInstall = ''
              wrapProgram $out/bin/restore \
                --set CARDANO_NODE_CONFIGS ${pkgs.cardano-node.deployments} \
                --prefix PATH : ${pkgs.cardano-node}/bin
            '';
          };


        # Make sure that libsodium DLLs for all windows executables,
        # and add shell completions for main executables.
        packages.cardano-wallet.components.exes.cardano-wallet.postInstall = optparseCompletionPostInstall + libSodiumPostInstall;
        packages.cardano-wallet-jormungandr.components.exes.cardano-wallet-jormungandr.postInstall = optparseCompletionPostInstall + libSodiumPostInstall;
        packages.cardano-wallet-jormungandr.components.tests.unit.postInstall = libSodiumPostInstall;
        packages.cardano-wallet-core.components.tests.unit.postInstall = libSodiumPostInstall;
        packages.cardano-wallet-cli.components.tests.unit.postInstall = libSodiumPostInstall;
      }

      {
        # Add shell completions for tools.
        packages.cardano-node.components.exes.cardano-cli.postInstall = optparseCompletionPostInstall + libSodiumPostInstall;
        packages.cardano-node.components.exes.cardano-node.postInstall = optparseCompletionPostInstall + libSodiumPostInstall;
        packages.cardano-addresses-cli.components.exes.cardano-address.postInstall = optparseCompletionPostInstall;
        packages.cardano-transactions.components.exes.cardano-tx.postInstall = optparseCompletionPostInstall;
        packages.bech32.components.exes.bech32.postInstall = optparseCompletionPostInstall;
      }

      # Provide the swagger file in an environment variable for
      # tests because it is located outside of the Cabal package
      # source tree.
      (let
        swaggerYamlPreBuild = ''
          export SWAGGER_YAML=${src + /specifications/api/swagger.yaml}
        '';
      in {
        packages.cardano-wallet-core.components.tests.unit.preBuild = swaggerYamlPreBuild;
        packages.cardano-wallet-jormungandr.components.tests.unit.preBuild = swaggerYamlPreBuild;
      })

      # Build fixes for library dependencies
      {
        # Make the /usr/bin/security tool available because it's
        # needed at runtime by the x509-system Haskell package.
        packages.x509-system.components.library.preBuild = lib.optionalString (stdenv.isDarwin) ''
          substituteInPlace System/X509/MacOS.hs --replace security /usr/bin/security
        '';

        # Packages we wish to ignore version bounds of.
        # This is similar to jailbreakCabal, however it
        # does not require any messing with cabal files.
        packages.katip.doExactConfig = true;

        # split data output for ekg to reduce closure size
        packages.ekg.components.library.enableSeparateDataOutput = true;
      }

      # Enable profiling on executables if the profiling argument is set.
      (lib.optionalAttrs profiling {
        enableLibraryProfiling = true;
        packages.cardano-wallet.components.exes.cardano-wallet.enableExecutableProfiling = true;
        packages.cardano-wallet.components.benchmarks.restore.enableExecutableProfiling = true;
        packages.cardano-wallet-jormungandr.components.exes.cardano-wallet-jormungandr.enableExecutableProfiling = true;
      })

      # Musl libc fully static build
      (lib.optionalAttrs stdenv.hostPlatform.isMusl (let
        staticLibs = with pkgs; [ zlib openssl libffi gmp6 libsodium ];

        # Module options which add GHC flags and libraries for a fully static build
        fullyStaticOptions = {
          enableShared = false;
          enableStatic = true;
          configureFlags = map (drv: "--ghc-option=-optl=-L${drv}/lib") staticLibs;
        };
      in {
        # Apply fully static options to our Haskell executables
        packages.cardano-wallet.components.benchmarks.restore = fullyStaticOptions;
        packages.cardano-wallet.components.exes.cardano-wallet = fullyStaticOptions;
        packages.cardano-wallet.components.tests.integration = fullyStaticOptions;
        packages.cardano-wallet.components.tests.unit = fullyStaticOptions;
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
  };

  # Hydra will pass the GitHub PR number as a string argument to release.nix.
  isHydraPRJobset = toString pr != "";

  # Make sure that the libsodium DLL is available beside the EXEs of
  # the windows build.
  libSodiumPostInstall = lib.optionalString stdenv.hostPlatform.isWindows ''
    ln -s ${pkgs.libsodium}/bin/libsodium-23.dll $out/bin
  '';

  # This exe component postInstall script adds shell completion
  # scripts. These completion
  # scripts will be picked up automatically if the resulting
  # derivation is installed, e.g. by `nix-env -i`.
  optparseCompletionPostInstall = lib.optionalString stdenv.hostPlatform.isUnix ''
    exeName=$(ls -1 $out/bin | head -n1)  # fixme add $exeName to Haskell.nix
    bashCompDir="$out/share/bash-completion/completions"
    zshCompDir="$out/share/zsh/vendor-completions"
    fishCompDir="$out/share/fish/vendor_completions.d"
    mkdir -p "$bashCompDir" "$zshCompDir" "$fishCompDir"
    "$out/bin/$exeName" --bash-completion-script "$out/bin/$exeName" >"$bashCompDir/$exeName"
    "$out/bin/$exeName" --zsh-completion-script "$out/bin/$exeName" >"$zshCompDir/_$exeName"
    "$out/bin/$exeName" --fish-completion-script "$out/bin/$exeName" >"$fishCompDir/$exeName.fish"
  '';

  # Add component options to wrap a benchmark exe, so that it has the
  # backend executable on its path, and the source tree as its working
  # directory.
  wrapBench = progs:
    lib.optionalAttrs (!stdenv.hostPlatform.isWindows) {
      build-tools = [ pkgs.makeWrapper ];
      postInstall = ''
        wrapProgram $out/bin/* \
          --run "cd $src" \
          --prefix PATH : ${lib.makeBinPath progs}
      '';
    };

in
  pkgSet.config.hsPkgs // {
    _config = pkgSet.config;
    _roots = haskell.roots pkgSet.config.ghc;
  }
