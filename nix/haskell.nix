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

  # When adding a new Cabal package, or removing, update this attrset.
  # It's not automatically discovered from stack-pkgs yet.
  projectPackages = {
    cardano-wallet-cli = "lib/cli";
    cardano-wallet-core-integration = "lib/core-integration";
    cardano-wallet-core = "lib/core";
    cardano-wallet-launcher = "lib/launcher";
    cardano-numeric = "lib/numeric";
    cardano-wallet = "lib/shelley";
    strict-non-empty-containers = "lib/strict-non-empty-containers";
    cardano-wallet-test-utils = "lib/test-utils";
    text-class = "lib/text-class";
  };

  pkg-set = haskell.mkStackPkgSet {
    inherit stack-pkgs;
    modules = [
      {
        packages = lib.mapAttrs (name: subDir: {
          # Add source filtering to local packages and chop out a
          # subdirectory of the source, so that the package is only
          # rebuilt when something relevant in the subdirectory changes.
          src = haskell.haskellLib.cleanSourceWith { inherit src subDir; };
          # Mark package as local non-dep in the nix-shell.
          # fixme: Haskell.nix should set it
          package.isProject = true;

          # Enable release flag (optimization and -Werror)
          flags.release = true;

          # Enable Haskell Program Coverage for all local libraries
          # and test suites.
          doCoverage = coverage;
        }) projectPackages;
      }

      # Provide configuration and dependencies to cardano-wallet components
      ({ config, ...}: let
        cardanoNodeExes = with config.hsPkgs;
          [ cardano-node.components.exes.cardano-node
            cardano-cli.components.exes.cardano-cli ];
      in {
        packages.cardano-wallet-core.components.tests = {
          unit.preCheck = noCacheOnBorsCookie;
          # Attempt to ensure visible progress in the macOS hydra job.
          #
          # An hypothesis is that #2472 is caused by heavy load and unfocused
          # resources from running the tests concurrently, risking that the slowest
          # hspec runner - and thererefore the stdout - being silent for 900s causing
          # hydra to timeout.
          #
          # Setting -j 1 should hopefully focus the resource we have in one place. It
          # should go silent less often, at the expense of the full run getting slower.
          unit.testFlags = lib.optionals pkgs.stdenv.hostPlatform.isDarwin ["-j" "1"];
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
          #
          # To alleviate TimeInterpreter race conditions on the mac builders
          # since #2755, we run slightly less in paralell on macOS.
          integration.testFlags =
            if pkgs.stdenv.hostPlatform.isDarwin
            then ["-j" "2"]
            else ["-j" "3"];

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

        # Add node backend to the PATH of the latency benchmarks, and
        # set the source tree as its working directory.
        packages.cardano-wallet.components.benchmarks.latency =
          lib.optionalAttrs (!stdenv.hostPlatform.isWindows) {
            build-tools = [ pkgs.buildPackages.makeWrapper ];
            postInstall = ''
              wrapProgram $out/bin/* \
                --run "cd $src/lib/shelley" \
                --prefix PATH : ${lib.makeBinPath cardanoNodeExes}
            '';
          };

        # Add cardano-node to the PATH of the byroon restore benchmark.
        # cardano-node will want to write logs to a subdirectory of the working directory.
        # We don't `cd $src` because of that.
        packages.cardano-wallet.components.benchmarks.restore =
          lib.optionalAttrs (!stdenv.hostPlatform.isWindows) {
            build-tools = [ pkgs.buildPackages.makeWrapper ];
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
            build-tools = [ pkgs.buildPackages.makeWrapper ];
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
        packages.cardano-wallet-launcher.components.tests.unit.postInstall = libSodiumPostInstall;
      })

      ({ config, ...}: let
        setGitRevPostInstall = ''
          ${buildPackages.iohk-nix-utils}/bin/set-git-rev "${config.packages.cardano-node.src.rev}" $out/bin/* || true
        '';
      in {
        # Add shell completions for tools.
        packages.cardano-cli.components.exes.cardano-cli.postInstall = optparseCompletionPostInstall + libSodiumPostInstall + setGitRevPostInstall;
        packages.cardano-node.components.exes.cardano-node.postInstall = optparseCompletionPostInstall + libSodiumPostInstall + setGitRevPostInstall;
        packages.cardano-addresses-cli.components.exes.cardano-address.postInstall = optparseCompletionPostInstall;
        packages.bech32.components.exes.bech32.postInstall = optparseCompletionPostInstall;
      })

      # Provide the git revision for cardano-addresses
      ({ config, ... }:
      {
        packages.cardano-addresses-cli.components.library.preBuild = ''
          export GITREV=${config.hsPkgs.cardano-addresses-cli.src.rev}
        '';
      })

      # Make git available for TH code that uses it to get the git revision
      ({ pkgs, ...}: {
        packages.cardano-config.components.library.build-tools = [
          pkgs.buildPackages.buildPackages.gitReallyMinimal
        ];
      })

      # Provide the swagger file in an environment variable for
      # tests because it is located outside of the Cabal package
      # source tree.
      {
        packages.cardano-wallet-core.components.tests.unit.preBuild = ''
          export SWAGGER_YAML=${src + /specifications/api/swagger.yaml}
        '';
      }

      ({ lib, pkgs, ... }: {
        # Use our forked libsodium from iohk-nix crypto overlay.
        packages.cardano-crypto-praos.components.library.pkgconfig = lib.mkForce [ [ pkgs.libsodium-vrf ] ];
        packages.cardano-crypto-class.components.library.pkgconfig = lib.mkForce [ [ pkgs.libsodium-vrf ] ];
      })

      # Build fixes for library dependencies
      {
        # Packages we wish to ignore version bounds of.
        # This is similar to jailbreakCabal, however it
        # does not require any messing with cabal files.
        packages.katip.doExactConfig = true;

        # split data output for ekg to reduce closure size
        packages.ekg.components.library.enableSeparateDataOutput = true;

        # Avoid this error on the windows build:
        #   Wrap.hsc:96:10: fatal error: regex.h: No such file or directory
        packages.regex-posix.flags._regex-posix-clib = stdenv.hostPlatform.isWindows;

        # Lets us put the pretty-simple tool in shell.nix.
        packages.pretty-simple.flags.buildexe = true;
      }

      # Enable profiling on executables if the profiling argument is set.
      (lib.optionalAttrs profiling {
        enableLibraryProfiling = true;
        packages.cardano-wallet.components.exes.cardano-wallet.enableExecutableProfiling = true;
        packages.cardano-wallet.components.benchmarks.restore.enableExecutableProfiling = true;
      })

      # Musl libc fully static build
      (lib.optionalAttrs stdenv.hostPlatform.isMusl (let
        staticLibs = with pkgs; [ zlib openssl libffi gmp6 libsodium-vrf ];

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
    ln -s ${pkgs.libsodium-vrf}/bin/libsodium-23.dll $out/bin
    ln -s ${pkgs.buildPackages.gcc.cc}/x86_64-w64-mingw32/lib/libstdc++-6.dll $out/bin
    ln -s ${pkgs.buildPackages.gcc.cc}/x86_64-w64-mingw32/lib/libgcc_s_seh-1.dll $out/bin
    ln -s ${pkgs.windows.mcfgthreads}/bin/mcfgthread-12.dll $out/bin
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
    "$out/bin/$exeName" --bash-completion-script "$exeName" >"$bashCompDir/$exeName"
    "$out/bin/$exeName" --zsh-completion-script "$exeName" >"$zshCompDir/_$exeName"
    "$out/bin/$exeName" --fish-completion-script "$exeName" >"$fishCompDir/$exeName.fish"
  '';

in
  haskell.addProjectAndPackageAttrs {
    inherit pkg-set;
    inherit (pkg-set.config) hsPkgs;
    roots = haskell.roots pkg-set.config.compiler.nix-name;
  }
