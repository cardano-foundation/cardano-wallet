# Test harness for migrating databases from previous versions to the
# current version.

{ system ? builtins.currentSystem
, crossSystem ? null
, config ? {}
, pkgs ? import ./nixpkgs-haskell.nix  {}
}:

with pkgs.lib;

let
  # List of git revisions to test against.
  releases = [
    { rev = "v2019-11-18";
      sha256 = "0ijsdi8f0y49hn62dj80xipf14nb0hnisddq6b8lj10jznwgsdag";
      allowFail = true; }
    { rev = "v2019-12-09";
      sha256 = "1ld415pbbhj8hg14325w2dcwxkkanm3vx09s7cs1viy8lcl69sln";
      allowFail = true; }
    { rev = "v2019-12-13";
      sha256 = "1a2b4iflwwp824b1k9b82jw2q8pqlar6hg96nv03zv55glkgdllm";
      allowFail = true; }
    { rev = "v2019-12-16";
      sha256 = "0y5xf43lrc7gygvxh5ywkglr3d1xcc19dsskm7frl0v6m9yxzni6"; }
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
        inherit system crossSystem config;
        gitrev = src.rev;
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
    testRelease = walletPackages: rel:
      pkgs.writeScript "launch-migration-test-${releaseName rel}.sh" ''
        #!${pkgs.runtimeShell}

        export PATH=${makeBinPath [
          walletPackages.cardano-wallet-jormungandr
          walletPackages.jormungandr
          migrationTest
          pkgs.bash
          pkgs.coreutils
        ]}
        export src=${walletPackages.src}

        exec ${./launch-migration-test.sh} "$@"
      '' // { inherit (walletPackages) cardano-wallet-jormungandr; };

    # Create a test runner script for the given release.
    # The test scenario is quite simple at present.
    # It just starts with the given version then migrates to the current
    # version.
    mkTestRunner = rel: rec {
      name = releaseName rel;
      test = testRelease walletPackages rel;
      testStep2 = testRelease walletPackages null;
      walletPackages = importRelease rel;
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
    in pkgs.runCommand "migration-tests" {} (''
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
      cp ${test.src}/lib/jormungandr/test/data/jormungandr/{block0.bin,config.yaml,secret.yaml} $out/${test.name}/data

      # append test to the run all script
      echo "${test.name}\${test.runner.name}" >> $out/runall.bat
      echo "if %errorlevel% neq 0 exit /b %errorlevel%" >> $out/runall.bat
    '') (map mkTestRunner rels));

  ############################################################################

  mkTests = if pkgs.stdenv.hostPlatform.isWindows then mkTestsWindows else mkTestsBash;

in
  mkTests releases
