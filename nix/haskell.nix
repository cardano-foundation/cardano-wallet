############################################################################
# Builds Haskell packages with Haskell.nix
############################################################################
haskell-nix: haskell-nix.stackProject' [
  ({ lib, pkgs, ... }: {
    options = {
      gitrev = lib.mkOption {
        type = lib.types.str;
        description = "Git revision of sources";
      };
      profiling = lib.mkOption {
        type = lib.types.bool;
        description = "Enable profiling";
        default = false;
      };
      coverage = lib.mkOption {
        type = lib.types.bool;
        description = "Enable Haskell Program Coverage for cardano-wallet libraries and test suites.";
        default = false;
      };
      doCheck = lib.mkOption {
        type = lib.types.bool;
        description = "Enable Haskell Program Coverage for cardano-wallet libraries and test suites.";
        default = true;
      };
      doIntegrationCheck = lib.mkOption {
        type = lib.types.bool;
        description = ''Wether to run integration tests.'';
        default = false;
      };
      buildBenchmarks = lib.mkOption {
        type = lib.types.bool;
        description = ''Wether to run integration tests.'';
        default = true;
      };
      cacheTestFailures = lib.mkOption {
        type = lib.types.bool;
        description = ''If false, prevent test results from being cached'';
        default = true;
      };
    };
  })
  ({ pkgs
   , lib
   , config
   , buildProject
   , ...
   }:

    let
      inherit (pkgs) stdenv;
      inherit (haskell-nix) haskellLib;

      # Add this string to a tests preCheck to prevent test results from
      # being cached.
      #
      # It is useful to have when your tests are flaky and fail a lot --
      # we don't want to cache false failures.
      noCacheCookie = ''
        # Causes tests to be re-run whenever the git revision
        # changes, even if everything else is identical.
        echo "Git revision is ${toString config.gitrev}"
      '';

      noCacheTestFailuresCookie = lib.optionalString (!config.cacheTestFailures) noCacheCookie;

      # Make sure that the libsodium DLL is available beside the EXEs of
      # the windows build.
      libSodiumPostInstall = lib.optionalString stdenv.hostPlatform.isWindows ''
        ln -s ${pkgs.libsodium-vrf}/bin/libsodium-23.dll $out/bin
        ln -s ${pkgs.buildPackages.gcc.cc}/x86_64-w64-mingw32/lib/libstdc++-6.dll $out/bin
        ln -s ${pkgs.buildPackages.gcc.cc}/x86_64-w64-mingw32/lib/libgcc_s_seh-1.dll $out/bin
        ln -s ${pkgs.windows.mcfgthreads}/bin/mcfgthread-12.dll $out/bin
      '';

      # setGitRev is a postInstall script to stamp executables with
      # version info. It uses the "gitrev" option.
      setGitRevPostInstall = ''
        ${pkgs.buildPackages.iohk-nix-utils}/bin/set-git-rev "${config.gitrev}" $out/bin/*
      '';

      rewriteLibsPostInstall = lib.optionalString (pkgs.stdenv.hostPlatform.isDarwin) ''
        export PATH=$PATH:${lib.makeBinPath (with pkgs.buildPackages; [ iohk-nix-utils binutils nix ])}
        rewrite-libs $out/bin $out/bin/*
      '';

      stripBinariesPostInstall = lib.optionalString stdenv.hostPlatform.isMusl ''
        ${pkgs.buildPackages.binutils-unwrapped}/bin/*strip $out/bin/*
      '';

      # This exe component postInstall script adds shell completion
      # scripts. These completion
      # scripts will be picked up automatically if the resulting
      # derivation is installed, e.g. by `nix-env -i`.
      optparseCompletionPostInstall = lib.optionalString stdenv.hostPlatform.isUnix ''
        exeName=$(ls -1 $out/bin | head -n1)  # FIXME add $exeName to Haskell.nix
        bashCompDir="$out/share/bash-completion/completions"
        zshCompDir="$out/share/zsh/vendor-completions"
        fishCompDir="$out/share/fish/vendor_completions.d"
        mkdir -p "$bashCompDir" "$zshCompDir" "$fishCompDir"
        "$out/bin/$exeName" --bash-completion-script "$exeName" >"$bashCompDir/$exeName"
        "$out/bin/$exeName" --zsh-completion-script "$exeName" >"$zshCompDir/_$exeName"
        "$out/bin/$exeName" --fish-completion-script "$exeName" >"$fishCompDir/$exeName.fish"
      '';

    in
    {

      name = "cardano-wallet";

      src = haskellLib.cleanSourceWith {
        src = lib.cleanSource ../.;
        name = "cardano-wallet-src";
        filter = name: type: (pkgs.cardanoWalletLib.removeSocketFilesFilter name type)
          && (haskell-nix.haskellSourceFilter name type);
      };

      materialized = ./materialized/stack-nix;

      # nix-prefetch-git --quiet <repo-url> <rev> | jq .sha256`
      sha256map = {
        "https://github.com/input-output-hk/cardano-addresses"."4003fc09780da61bc09d85337bdd4c7664aa49ba" = "0ffavab3h56p1b1narn1w8aq4wr2affdl5xyz8cxv5ighi1sw98g";
        "https://github.com/input-output-hk/optparse-applicative"."7497a29cb998721a9068d5725d49461f2bba0e7a" = "1gvsrg925vynwgqwplgjmp53vj953qyh3wbdf34pw21c8r47w35r";
        "https://github.com/input-output-hk/cardano-base"."e8a48cf0500b03c744c7fc6f2fedb86e8bdbe055" = "0s3w796y4bgjidg5iwapdq88cq9ipy346gagbip6xlqxdvpp99xj";
        "https://github.com/input-output-hk/cardano-crypto"."07397f0e50da97eaa0575d93bee7ac4b2b2576ec" = "06sdx5ndn2g722jhpicmg96vsrys89fl81k8290b3lr6b1b0w4m3";
        "https://github.com/input-output-hk/cardano-ledger-specs"."f827a4321e42f528e25f6079f7af3eb18f10d391" = "0dmgxg7cpgz4lnscqrrk4gakw9w90dx8ljv5wr923rfp9nyzc5qf";
        "https://github.com/input-output-hk/cardano-node"."0fb43f4e3da8b225f4f86557aed90a183981a64f" = "0mkir1pg78hp7adxgb8cz6jj4izs07np23fxxnwhkvf0ql92nan7";
        "https://github.com/input-output-hk/cardano-prelude"."fd773f7a58412131512b9f694ab95653ac430852" = "02jddik1yw0222wd6q0vv10f7y8rdgrlqaiy83ph002f9kjx7mh6";
        "https://github.com/input-output-hk/cardano-sl-x509"."12925934c533b3a6e009b61ede555f8f26bac037" = "1kma25g8sl6m3pgsihja7fysmv6vjdfc0x7dyky9g5z156sh8z7i";
        "https://github.com/input-output-hk/flat"."ee59880f47ab835dbd73bea0847dab7869fc20d8" = "1lrzknw765pz2j97nvv9ip3l1mcpf2zr4n56hwlz0rk7wq7ls4cm";
        "https://github.com/input-output-hk/goblins"."cde90a2b27f79187ca8310b6549331e59595e7ba" = "17c88rbva3iw82yg9srlxjv2ia5wjb9cyqw44hik565f5v9svnyg";
        "https://github.com/input-output-hk/hedgehog-extras"."edf6945007177a638fbeb8802397f3a6f4e47c14" = "0wc7qzkc7j4ns2rz562h6qrx2f8xyq7yjcb7zidnj7f6j0pcd0i9";
        "https://github.com/input-output-hk/iohk-monitoring-framework"."808724ff8a19a33d0ed06f9ef59fbd900b08553c" = "0298dpl29gxzs9as9ha6y0w18hqwc00ipa3hzkxv7nlfrjjz8hmz";
        "https://github.com/shmish111/purescript-bridge"."6a92d7853ea514be8b70bab5e72077bf5a510596" = "13j64vv116in3c204qsl1v0ajphac9fqvsjp7x3zzfr7n7g61drb";
        "https://github.com/input-output-hk/ouroboros-network"."aa7bc087737edca29133844b14bb7cba2cd213f2" = "1rcjlj5z8igrfy07lkdrlm4xcx9a3g0jl69wvqk0vvff4hfr00ar";
        "https://github.com/input-output-hk/plutus"."edc6d4672c41de4485444122ff843bc86ff421a0" = "12dmxp11xlal8rr3371sir5q4f7gscmyl84nw6wm47mb5b28bk92";
        "https://github.com/shmish111/servant-purescript"."a76104490499aa72d40c2790d10e9383e0dbde63" = "11nxxmi5bw66va7psvrgrw7b7n85fvqgfp58yva99w3v9q3a50v9";
        "https://github.com/input-output-hk/Win32-network"."3825d3abf75f83f406c1f7161883c438dac7277d" = "19wahfv726fa3mqajpqdqhnl9ica3xmf68i254q45iyjcpj1psqx";
      };

      shell = {
        name = "cardano-wallet-shell${lib.optionalString config.profiling "-profiled"}";
        packages = ps: lib.attrValues (haskellLib.selectProjectPackages ps);
        # Prevents cabal from choosing alternate plans, so that
        # *all* dependencies are provided by Nix.
        exactDeps = true;
        # fixme: this is needed to prevent Haskell.nix double-evaluating hoogle
        tools.hoogle = {
          inherit (pkgs.haskell-build-tools.hoogle) version;
          inherit (pkgs.haskell-build-tools.hoogle.project) index-state;
          checkMaterialization = false;
          materialized = ./materialized + "/hoogle";
        };
        nativeBuildInputs = with buildProject.hsPkgs; [
          cardano-node.components.exes.cardano-node
          cardano-cli.components.exes.cardano-cli
          cardano-addresses-cli.components.exes.cardano-address
          bech32.components.exes.bech32
          pretty-simple.components.exes.pretty-simple
        ] ++ (with pkgs.buildPackages.buildPackages; [
          go-jira
          haskellPackages.ghcid
          pkgconfig
          python3Packages.openapi-spec-validator
          (ruby.withPackages (ps: [ ps.thor ]))
          sqlite-interactive
          curlFull
          jq
          yq
          nixWrapped
          cabalWrapped
        ] ++ lib.filter
          (drv: lib.isDerivation drv && drv.name != "regenerate-materialized-nix")
          (lib.attrValues haskell-build-tools));

        CARDANO_NODE_CONFIGS = pkgs.cardano-node-deployments;

        meta.platforms = lib.platforms.unix;
      };

      modules =
        let inherit (config) src coverage profiling doCheck doIntegrationCheck buildBenchmarks;
          projectPackages = lib.filterAttrs
            (_: p: p != null
              && haskellLib.isLocalPackage p.package
              && p.package.homepage == "https://github.com/input-output-hk/cardano-wallet")
            (haskell-nix.stackProject' {
              inherit (config) src name materialized sha256map;
            }).pkg-set.config.packages;

        in
        [
          {
            packages = lib.genAttrs (lib.attrNames projectPackages) (name: {
              # Mark package as local non-dep in the nix-shell.
              # fixme: Haskell.nix should set it
              package.isProject = true;

              # Enable release flag (optimization and -Werror)
              flags.release = true;

              # Enable Haskell Program Coverage for all local libraries
              # and test suites.
              doCoverage = doCheck && coverage;

              components.benchmarks = lib.genAttrs (lib.attrNames projectPackages.${name}.components.benchmarks) (_: {
                buildable = lib.mkForce buildBenchmarks;
              });

              inherit doCheck;
            });
          }

          # Provide configuration and dependencies to cardano-wallet components
          ({ config, ... }:
            let
              cardanoNodeExes = with config.hsPkgs;
                [
                  cardano-node.components.exes.cardano-node
                  cardano-cli.components.exes.cardano-cli
                ];
            in
            {
              packages.cardano-wallet-core.components.tests = {
                unit.preCheck = noCacheTestFailuresCookie;
                # Attempt to ensure visible progress in the macOS hydra job.
                #
                # An hypothesis is that #2472 is caused by heavy load and unfocused
                # resources from running the tests concurrently, risking that the slowest
                # hspec runner - and thererefore the stdout - being silent for 900s causing
                # hydra to timeout.
                #
                # Setting -j 1 should hopefully focus the resource we have in one place. It
                # should go silent less often, at the expense of the full run getting slower.
                unit.testFlags = lib.optionals pkgs.stdenv.hostPlatform.isDarwin [ "-j" "1" ];
              };

              packages.cardano-wallet.components.tests = {
                # Running Windows integration tests under Wine is disabled
                # because ouroboros-network doesn't fully work under Wine.
                integration.doCheck = doCheck && doIntegrationCheck && !pkgs.stdenv.hostPlatform.isWindows;

                unit.preCheck = noCacheTestFailuresCookie +
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
                  then [ "-j" "2" ]
                  else [ "-j" "3" ];

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
                    ${pkgs.buildPackages.gnutar}/bin/tar -C $(dirname $TESTS_LOGDIR) -czvf $logfile $TESTS_LOGDIR
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


              packages.cardano-wallet.components.exes.local-cluster =
                let
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
              packages.cardano-wallet.components.exes.cardano-wallet.postInstall = optparseCompletionPostInstall + libSodiumPostInstall + setGitRevPostInstall + rewriteLibsPostInstall + stripBinariesPostInstall;
              packages.cardano-wallet-core.components.tests.unit.postInstall = libSodiumPostInstall;
              packages.cardano-wallet-cli.components.tests.unit.postInstall = libSodiumPostInstall;
              packages.cardano-wallet-launcher.components.tests.unit.postInstall = libSodiumPostInstall;
            })

          ({ config, ... }:
            let
              setGitRevPostInstall = ''
                ${pkgs.buildPackages.iohk-nix-utils}/bin/set-git-rev "${config.packages.cardano-node.src.rev}" $out/bin/* || true
              '';
            in
            {
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
          (lib.optionalAttrs stdenv.hostPlatform.isMusl (
            let
              staticLibs = with pkgs; [ zlib openssl libffi gmp6 libsodium-vrf ];

              # Module options which add GHC flags and libraries for a fully static build
              fullyStaticOptions = {
                enableShared = false;
                enableStatic = true;
                configureFlags = map (drv: "--ghc-option=-optl=-L${drv}/lib") staticLibs;
              };
            in
            {
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
            }
          ))

          # Silence some warnings about "cleaning component source not
          # supported for hpack package" which appear in nix-shell
          {
            packages.cardano-addresses.cabal-generator = lib.mkForce null;
            packages.cardano-addresses-cli.cabal-generator = lib.mkForce null;
          }

          # Allow installation of a newer version of Win32 than what is
          # included with GHC. The packages in this list are all those
          # installed with GHC, except for Win32.
          {
            nonReinstallablePkgs =
              [
                "rts"
                "ghc-heap"
                "ghc-prim"
                "integer-gmp"
                "integer-simple"
                "base"
                "deepseq"
                "array"
                "ghc-boot-th"
                "pretty"
                "template-haskell"
                # ghcjs custom packages
                "ghcjs-prim"
                "ghcjs-th"
                "ghc-boot"
                "ghc"
                "array"
                "binary"
                "bytestring"
                "containers"
                "filepath"
                "ghc-boot"
                "ghc-compact"
                "ghc-prim"
                # "ghci" "haskeline"
                "hpc"
                "mtl"
                "parsec"
                "text"
                "transformers"
                "xhtml"
                # "stm" "terminfo"
              ];
          }
          {
            packages.cardano-wallet-core.components.library.build-tools = [ pkgs.buildPackages.buildPackages.gitMinimal ];
            packages.cardano-config.components.library.build-tools = [ pkgs.buildPackages.buildPackages.gitMinimal ];
          }
        ];
    })
]
