############################################################################
#
# Test harness for migrating state from previous versions to the
# current version.
#
# To build all migration tests:
#   nix-build nix/migration-tests.nix -o migration-tests
#
# To run all migration tests:
#   ./migration-tests/runall.sh
#
# To run migration tests for given version to the current version.
#   ./migration-tests/v2019-12-16/run.sh
#
# To build and run the migration test for just one version:
#   nix-build nix/migration-tests.nix -A v2019-12-16 -o run-migration-test-v2019-12-16
#   ./run-migration-test-v2019-12-16
#
# You can inspect the resulting bash scripts and run parts
# individually. For example, run step1 to set up a state directory for
# the old version, then try:
#   stateDir=./debug /nix/store/...-launch-migration-test-v2019-12-16.sh step1
#   stack exec migration-test -- step2 launch --state-dir=./debug ...
#
############################################################################

{ system ? builtins.currentSystem
, crossSystem ? null
, config ? {}
, pkgs ? import ./default.nix {}
}:

with pkgs.lib;

let

  # From nixpkgs 19.09 onwards we use `x86_64-w64-mingw32` instead of
  # the legacy `x86_64-pc-mingw32` that nixpkgs incorrectly used. Thus
  # to test migration paths for releases that have been build with nixpkgs
  # prior to 19.09, we'll need to rewrite the crossSystem.config.
  # This is controlled by the `needsRewrite` flags int he releases.
  rewriteCrossSystem = crossSystem: if crossSystem != null && crossSystem.config == "x86_64-w64-mingw32" then crossSystem // { config = "x86_64-pc-mingw32"; } else crossSystem;
  # List of git revisions to test against.
  # One can get sha256 for release via nix-prefetch-url, e.g. for v2020-04-28:
  # nix-prefetch-url --unpack https://github.com/input-output-hk/cardano-wallet/archive/v2020-04-28.zip
  releases = [
    { rev = "v2020-04-28";
      sha256 = "0iw5gn3d2i1f99mx293zn8l72i7lidmdlrfyzblhdvx6f358iyki";
      needsRewrite = true; }
    { rev = "v2020-04-07";
      sha256 = "10m91l6r4ghhqddk5c0a4ffjl0z3h4nmk7cjhz2b0jhxdkmz6qg5";
      needsRewrite = true;  }
    { rev = "v2020-04-01";
      sha256 = "0hjdz4bbhq7g9cc1hfxpnwypd42n3rx0ci91n8i7ahnjhv93bhhy";
      needsRewrite = true;  }
  ];

  # Download the sources for a release.
  fetchRelease = rel: pkgs.fetchFromGitHub {
    owner = "input-output-hk";
    repo = "cardano-wallet";
    inherit (rel) rev sha256;
    name = "cardano-wallet-src-${rel.rev}";
  };

  # Gets a package set for a specific release. If the argument is null
  # it returns the current working tree version.
  importRelease = rel:
    if rel == null
      then import ../default.nix { inherit system crossSystem config; }
      else let src = fetchRelease rel; in import src {
        inherit system config;
        gitrev = src.rev;
        crossSystem = if rel.needsRewrite
                      then rewriteCrossSystem crossSystem
                      else crossSystem;
      };

  # Grab the migration test from the current version.
  migrationTest = (importRelease null).haskellPackages.cardano-wallet-jormungandr.components.exes.migration-test;

  # Generate attribute name/filename for a release.
  releaseName = rel: if rel == null
    then "head"
    else builtins.replaceStrings ["."] ["-"] rel.rev;

  ############################################################################
  # Generate a directory of wallet versions under test, with bash test
  # harness scripts for running under Linux and macOS.

  mkTestsBash = let
    # Create a script that runs the migration test against the server of
    # a certain release.
    testRelease = rel: let
        targetRelease = importRelease rel;
        # Use the genesis block from the latest release only.
        # Having different genesis block (hash) across releases will basically
        # make all wallets incompatible with each others. Prior to v2020-01-20,
        # workers would simply loop ad-infinitum trying to rollback to (0, 0).
        latestRelease = importRelease null;
      in pkgs.writeScript "launch-migration-test-${releaseName rel}.sh" ''
        #!${pkgs.runtimeShell}

        export PATH=${makeBinPath [
          targetRelease.cardano-wallet-jormungandr
          targetRelease.jormungandr
          migrationTest
          pkgs.bash
          pkgs.coreutils
        ]}
        export genesisDataDir=${latestRelease.src}/lib/jormungandr/test/data/jormungandr
        export configFile=${targetRelease.src}/lib/jormungandr/test/data/jormungandr/config.yaml

        exec ${./launch-migration-test.sh} "$@"
      '' // { inherit (latestRelease) cardano-wallet-jormungandr; };

    # Create a test runner script for the given release.
    # The test scenario is quite simple at present.
    # It just starts with the given version then migrates to the current
    # version.
    mkTestRunner = rel: rec {
      name = releaseName rel;
      test = testRelease rel;
      testStep2 = testRelease null;
      allowFail = rel.allowFail or false;
      runner = pkgs.writeScript "run-${test.name}" ''
        #!${pkgs.runtimeShell}
        set -euo pipefail

        export stateDir=./state-migration-test-${name}

        rm -rf "$stateDir"

        # Setup database on server running chosen release.
        ${test} step1
        # Start up a server of the current version, and check the migration.
        ${testStep2} step2 ${optionalString allowFail
          " || echo 'This test is allowed to fail.'"}
      '';
    };

  in
    # Create a directory with migration test scripts for each release version.
    # At the top level is a script that runs all tests.
    rels: let
      tests = map mkTestRunner rels;
    in pkgs.runCommand "migration-tests" {
      # provide individual tests as attributes of this derivation
      passthru = listToAttrs (map (test: nameValuePair test.name test.runner) tests);
    } (''
      mkdir -p $out
      echo "#!${pkgs.runtimeShell}" >> $out/runall.sh
      echo "set -euo pipefail" >> $out/runall.sh
      chmod 755 $out/runall.sh
    '' + concatMapStringsSep "\n" (test: ''
        mkdir -p $out/${test.name}
        ln -s ${test.test} $out/${test.name}/migration-test
        ln -s ${test.test.cardano-wallet-jormungandr}/bin/* $out/${test.name}
        ln -s ${test.runner} $out/${test.name}/${test.runner.name}
        echo 'printf "\n\n *** Migrating from ${test.name} ***\n\n"' >> $out/runall.sh
        echo "$out/${test.name}/${test.runner.name}" >> $out/runall.sh
      '') tests);

  ############################################################################
  # Generate a folder of wallet versions under test, with batch files
  # for running the tests on Windows.

  mkTestsWindows = let
    # Create a test runner script for the given release.
    # The test scenario is quite simple at present.
    # It just starts with the given version then migrates to the current
    # version.
    mkTestRunner = rel: rec {
      name = releaseName rel;
      walletPackages = importRelease rel;
      inherit (walletPackages) cardano-wallet-jormungandr src;
      allowFail = rel.allowFail or false;
      runner = let
        stateDir = "state-migration-test-${name}";
        args = "launch --state-dir ${stateDir} --genesis-block ${name}/data/block0.bin -- --secret ${name}/data/secret.yaml --config ${name}/data/config.yaml";
      in pkgs.writeScript "run.bat" ''
        SETLOCAL

        SET PATH=%~dp0;%PATH%

        rmdir /s /q ${stateDir}

        migration-test.exe step1 ${args}
        if %errorlevel% neq 0 exit /b %errorlevel%

        migration-test.exe step2 ${args}
        if %errorlevel% neq 0 ${if allowFail then ''
          echo This test is allowed to fail.
        '' else ''
          exit /b %errorlevel%
        ''}
      '';
    };
    latest = importRelease null;
  in
    # Create a directory with migration test scripts for each release version.
    # At the top level is a script that runs all tests.
    rels: pkgs.runCommand "migration-tests" {} (''
      mkdir $out
      cp ${migrationTest}/bin/* $out
    '' + concatMapStringsSep "\n" (test: ''
      mkdir -p $out/${test.name}/data
      cp ${test.cardano-wallet-jormungandr}/bin/* $out/${test.name}
      cp ${test.runner} $out/${test.name}/${test.runner.name}
      cp ${latest.src}/lib/jormungandr/test/data/jormungandr/{block0.bin,config.yaml,secret.yaml} $out/${test.name}/data

      # append test to the run all script
      echo "${test.name}\${test.runner.name}" >> $out/runall.bat
      echo "if %errorlevel% neq 0 exit /b %errorlevel%" >> $out/runall.bat
    '') (map mkTestRunner rels));

  ############################################################################

  mkTests = if pkgs.stdenv.hostPlatform.isWindows then mkTestsWindows else mkTestsBash;

in
  if pkgs.stdenv.hostPlatform.isMusl
    then pkgs.runCommand "migration-tests-disabled" {} "touch $out"
    else mkTests releases
