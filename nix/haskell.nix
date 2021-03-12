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
# Enable Haskell Program Coverage for cardano-wallet libraries and test suites.
, coverage ? config.haskellNix.coverage or false
# Project top-level source tree
, src
# GitHub PR number (when building a PR jobset on Hydra)
, pr ? null
# Bors job type (when building a bors jobset on Hydra)
, borsBuild ? null
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
  filterSubDir = subDir: {
    src = haskell.haskellLib.cleanSourceWith { inherit src subDir; };
    package.isProject = true;  # fixme: Haskell.nix
  };

  pkg-set = haskell.mkStackPkgSet {
    inherit stack-pkgs;
    modules = [
      # Add source filtering to local packages
      {
        packages.cardano-wallet-cli = filterSubDir "lib/cli";
        packages.cardano-wallet-core-integration = filterSubDir "lib/core-integration";
        packages.cardano-wallet-core = filterSubDir "lib/core";
        packages.cardano-wallet-launcher = filterSubDir "lib/launcher";
        packages.cardano-numeric = filterSubDir "lib/numeric";
        packages.cardano-wallet = filterSubDir "lib/shelley";
        packages.strict-non-empty-containers = filterSubDir "lib/strict-non-empty-containers";
        packages.cardano-wallet-test-utils = filterSubDir "lib/test-utils";
        packages.text-class = filterSubDir "lib/text-class";
      }

      # Provide sources at check time to test suites which need them
      {
        packages.cardano-wallet-core.components.tests.unit.keepSource = true;
        packages.cardano-wallet.components.tests.integration.keepSource = true;
        packages.text-class.components.tests.unit.keepSource = true;
      }

      # Enable release flag (optimization and -Werror) on all local packages
      {
        packages.cardano-wallet.flags.release = true;
        packages.cardano-wallet-cli.flags.release = true;
        packages.cardano-wallet-core-integration.flags.release = true;
        packages.cardano-wallet-core.flags.release = true;
        packages.cardano-wallet-launcher.flags.release = true;
        packages.cardano-wallet-test-utils.flags.release = true;
        packages.cardano-numeric.flags.release = true;
        packages.strict-non-empty-containers.flags.release = true;
        packages.text-class.flags.release = true;
      }

      (lib.optionalAttrs coverage {
        # Enable Haskell Program Coverage for all local libraries and test suites.
        packages.cardano-wallet.components.library.doCoverage = true;
        packages.cardano-wallet-cli.components.library.doCoverage = true;
        packages.cardano-wallet-core-integration.components.library.doCoverage = true;
        packages.cardano-wallet-core.components.library.doCoverage = true;
        packages.cardano-wallet-core.components.tests.unit.doCoverage = true;
        packages.cardano-wallet-launcher.components.library.doCoverage = true;
        packages.cardano-wallet-test-utils.components.library.doCoverage = true;
        packages.text-class.components.library.doCoverage = true;
      })

      # Provide configuration and dependencies to cardano-wallet components
      ({ config, ...}: let
        cardanoNodeExes = with config.hsPkgs;
          [ cardano-node.components.exes.cardano-node
            cardano-cli.components.exes.cardano-cli ];
      in {
        packages.cardano-wallet-core.components.tests = {
          unit.preCheck = noCacheOnBorsCookie;
        };

        packages.cardano-wallet.components.tests = {
          # Only run integration tests on non-PR jobsets. Note that
          # the master branch jobset will just re-use the cached Bors
          # staging build and test results.
          integration.doCheck = !isHydraPRJobset;

          # Running Windows integration tests under Wine is disabled
          # because ouroboros-network doesn't fully work under Wine.
          integration.testWrapper = lib.mkIf pkgs.stdenv.hostPlatform.isWindows ["echo"];

          unit.preCheck = noCacheOnBorsCookie +
            lib.optionalString stdenv.isDarwin ''
              # cardano-node socket path becomes too long otherwise
              export TMPDIR=/tmp
            '';

          # Force more integration tests to run in parallel than the
          # default number of build cores.
          integration.testFlags = ["-j" "3"];

          integration.preCheck = noCacheCookie + ''
            # Variables picked up by integration tests
            export CARDANO_NODE_TRACING_MIN_SEVERITY=notice
            export TESTS_RETRY_FAILED=yes

            # Integration tests will place logs here
            export TESTS_LOGDIR=$(mktemp -d)/logs
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
          unit.build-tools = cardanoNodeExes;
          integration.build-tools = cardanoNodeExes;
          unit.postInstall = libSodiumPostInstall;
        };

        # Add node backend to the PATH of the latency benchmarks
        packages.cardano-wallet.components.benchmarks.latency = wrapBench cardanoNodeExes;

        # Add cardano-node to the PATH of the byroon restore benchmark.
        # cardano-node will want to write logs to a subdirectory of the working directory.
        # We don't `cd $src` because of that.
        packages.cardano-wallet.components.benchmarks.restore =
          lib.optionalAttrs (!stdenv.hostPlatform.isWindows) {
            build-tools = [ pkgs.makeWrapper ];
            postInstall = ''
              wrapProgram $out/bin/restore \
                --set CARDANO_NODE_CONFIGS ${pkgs.cardano-node-deployments} \
                --prefix PATH : ${lib.makeBinPath cardanoNodeExes}
            '';
          };


        packages.cardano-wallet.components.exes.local-cluster = let
          testData = src + /lib/shelley/test/data/cardano-node-shelley;
        in
          if (stdenv.hostPlatform.isWindows) then {
            postInstall = ''
              mkdir -p $out/bin/test/data
              cp -Rv ${testData} $out/bin/test/data
            '' + libSodiumPostInstall;
          } else {
            build-tools = [ pkgs.makeWrapper ];
            postInstall = ''
              wrapProgram $out/bin/local-cluster \
                --set SHELLEY_TEST_DATA ${testData} \
                --prefix PATH : ${lib.makeBinPath cardanoNodeExes}
            '';
          };

        # Make sure that libsodium DLLs for all windows executables,
        # and add shell completions for main executables.
        packages.cardano-wallet.components.exes.cardano-wallet.postInstall = optparseCompletionPostInstall + libSodiumPostInstall;
        packages.cardano-wallet-core.components.tests.unit.postInstall = libSodiumPostInstall;
        packages.cardano-wallet-cli.components.tests.unit.postInstall = libSodiumPostInstall;
      })

      ({ config, ...}: let
        setGitRevPostInstall = ''
          ${buildPackages.commonLib.haskell-nix-extra-packages.haskellBuildUtils.package}/bin/set-git-rev "${config.packages.cardano-node.src.rev}" $out/bin/* || true
        '';
      in {
        # Add shell completions for tools.
        packages.cardano-cli.components.exes.cardano-cli.postInstall = optparseCompletionPostInstall + libSodiumPostInstall + setGitRevPostInstall;
        packages.cardano-node.components.exes.cardano-node.postInstall = optparseCompletionPostInstall + libSodiumPostInstall + setGitRevPostInstall;
        packages.cardano-addresses-cli.components.exes.cardano-address.postInstall = optparseCompletionPostInstall;
        packages.cardano-transactions.components.exes.cardano-tx.postInstall = optparseCompletionPostInstall;
        packages.bech32.components.exes.bech32.postInstall = optparseCompletionPostInstall;
      })

      # Provide the git revision for cardano-addresses
      ({ config, ... }:
      {
        packages.cardano-addresses-cli.components.library.preBuild = ''
          export GITREV=${config.hsPkgs.cardano-addresses-cli.src.rev}
        '';
      })

      # Provide the swagger file in an environment variable for
      # tests because it is located outside of the Cabal package
      # source tree.
      {
        packages.cardano-wallet-core.components.tests.unit.preBuild = ''
          export SWAGGER_YAML=${src + /specifications/api/swagger.yaml}
        '';
      }

      # Build fixes for library dependencies
      {
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
        packages.cardano-wallet-launcher.components.tests.unit = fullyStaticOptions;

        # systemd can't be statically linked - disable lobemo-scribe-journal
        packages.cardano-config.flags.systemd = false;
        packages.cardano-node.flags.systemd = false;

        # Haddock not working for cross builds and is not needed anyway
        doHaddock = false;
      }))

      # Silence some warnings about "cleaning component source not
      # supported for hpack package" which appear in nix-shell
      {
        packages.cardano-addresses.cabal-generator = lib.mkForce null;
        packages.cardano-addresses-cli.cabal-generator = lib.mkForce null;
        packages.cardano-transactions.cabal-generator = lib.mkForce null;
      }

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
  isHydraBorsJobset = toString borsBuild != "";

  # Add this string to a tests preCheck to prevent test results from
  # being cached.
  #
  # It is useful to have when your tests are flaky and fail a lot --
  # we don't want to cache false failures.
  noCacheCookie = ''
    # Causes tests to be re-run whenever the git revision
    # changes, even if everything else is identical.
    echo "Git revision is ${toString gitrev}"
  '';

  # Sets the anti-cache cookie only when building a jobset for bors.
  noCacheOnBorsCookie = lib.optionalString isHydraBorsJobset noCacheCookie;

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
  haskell.addProjectAndPackageAttrs {
    inherit pkg-set;
    inherit (pkg-set.config) hsPkgs;
  }
